#include "memory-allocation.h"

RegistryEntry *newRegistryEntry(RegistryEntry::Type type) {
  RegistryEntry *newEntry;

  if (nextFreeRegistryEntry == nullptr) {
    if (tupleRegistrySize >= tupleRegistryCapacity) {
      Serial << "Stall (registry entry)!" << endl;

      fullGarbageCollection();

      if (nextFreeRegistryEntry == nullptr) {
        Serial << "No more registry entries!" << endl;
        return nullptr;
      }

      VERBOSE(Serial << "\tReusing Registry Entry: ");

      newEntry = nextFreeRegistryEntry;
      nextFreeRegistryEntry = nextFreeRegistryEntry->next;
    } else {
      VERBOSE(Serial << "\tNew Registry Entry: ");

      newEntry = tupleRegistry + tupleRegistrySize;
      ++tupleRegistrySize;
    }
  } else {
    VERBOSE(Serial << "\tReusing Registry Entry: ");

    newEntry = nextFreeRegistryEntry;
    nextFreeRegistryEntry = nextFreeRegistryEntry->next;
  }

  VERBOSE(Serial << (newEntry - tupleRegistry) << endl);

  newEntry->type = type;

  switch (garbageCollectionState) {
  case GarbageCollectionStates::scan:
    newEntry->marked = false;
    scanNext(newEntry);
    break;
  case GarbageCollectionStates::sweep:
    newEntry->marked = true;
    break;
  }

  ++tupleRegistryUsageCount;

  return newEntry;
}

Tuple *newTuple(size_t headSize, Arity n) {
  size_t newTupleSize = RegistryEntry::tupleSize(headSize, n);

  if ((nextFreeTuple + newTupleSize) >= (tuplesHeap + tuplesHeapCapacity)) {
    Serial << "Stall (tuples heap)!" << endl;

    fullGarbageCollection();

    if ((nextFreeTuple + newTupleSize) >= (tuplesHeap + tuplesHeapCapacity)) {
      Serial << "No more space for tuples!" << endl;
      return nullptr;
    }
  }

  Tuple *tuple = reinterpret_cast<Tuple *>(nextFreeTuple);
  nextFreeTuple += newTupleSize;

  VERBOSE(Serial << "\tNew Tuple at "
                 << (reinterpret_cast<uint8_t *>(tuple) - tuplesHeap) << endl);

  return tuple;
}

RegistryEntry *newRegistryEntryWithTuple(RegistryEntry::Type type,
                                         size_t headSize, Arity n) {
  RegistryEntry *entry = newRegistryEntry(type);

  if (entry == nullptr) {
    return nullptr;
  }

  partialNewRegistryEntry = entry;

  Tuple *tuple = newTuple(headSize, n);

  if (tuple == nullptr) {
    return nullptr;
  }

  partialNewRegistryEntry = nullptr;

  entry->tuple = tuple;
  tuple->entry = entry;

  return entry;
}

RegistryEntry *newVariable() {
  VERBOSE(Serial << "New Variable:" << endl);

  RegistryEntry *entry =
      newRegistryEntryWithTuple(RegistryEntry::Type::reference, ScalarSize, 0);

  if (entry == nullptr) {
    return nullptr;
  }

  entry->mutableBody<RegistryEntry *>() = entry;

  return entry;
}

RegistryEntry *newStructure(Functor f, Arity n) {
  VERBOSE(Serial << "New Structure:" << endl);

  RegistryEntry *entry = newRegistryEntryWithTuple(
      RegistryEntry::Type::structure, sizeof(Structure), n);

  if (entry == nullptr) {
    return nullptr;
  }

  Structure &structure = entry->mutableBody<Structure>();

  structure.functor = f;
  structure.arity = n;

  for (Arity i = 0; i < n; ++i) {
    structure.subterms[i] = nullptr;
  }

  currentStructure = entry;
  currentStructureSubtermIndex = 0;

  return entry;
}

RegistryEntry *newList() {
  VERBOSE(Serial << "New List:" << endl);

  RegistryEntry *entry =
      newRegistryEntryWithTuple(RegistryEntry::Type::list, 0, 2);

  if (entry == nullptr) {
    return nullptr;
  }

  List &list = entry->mutableBody<List>();

  list.subterms[0] = nullptr;
  list.subterms[1] = nullptr;

  currentStructure = entry;
  currentStructureSubtermIndex = 0;

  return entry;
}

RegistryEntry *newConstant(Constant c) {
  VERBOSE(Serial << "New Constant:" << endl);

  RegistryEntry *entry =
      newRegistryEntryWithTuple(RegistryEntry::Type::constant, ScalarSize, 0);

  if (entry == nullptr) {
    return nullptr;
  }

  entry->mutableBody<Constant>() = c;

  return entry;
}

RegistryEntry *newInteger(Integer i) {
  VERBOSE(Serial << "New Integer:" << endl);

  RegistryEntry *entry =
      newRegistryEntryWithTuple(RegistryEntry::Type::integer, ScalarSize, 0);

  if (entry == nullptr) {
    return nullptr;
  }

  entry->mutableBody<Integer>() = i;

  return entry;
}

RegistryEntry *newEnvironment(EnvironmentSize n) {
  VERBOSE(Serial << "New Environment of size " << n << ":" << endl);

  RegistryEntry *entry = newRegistryEntryWithTuple(
      RegistryEntry::Type::environment, sizeof(Environment), n);

  if (entry == nullptr) {
    return nullptr;
  }

  Environment &environment = entry->mutableBody<Environment>();

  environment.nextEnvironment = currentEnvironment;
  environment.continuePoint = continuePoint;
  environment.size = n;
  environment.capacity = n;

  for (EnvironmentSize i = 0; i < n; ++i) {
    environment.permanentVariables[i] = nullptr;
  }

  currentEnvironment = entry;

  return entry;
}

RegistryEntry *newChoicePoint(LabelIndex retryLabel) {
  VERBOSE(Serial << "New Choice Point:" << endl);

  RegistryEntry *entry = newRegistryEntryWithTuple(
      RegistryEntry::Type::choicePoint, sizeof(ChoicePoint), argumentCount);

  if (entry == nullptr) {
    return nullptr;
  }

  ChoicePoint &choicePoint = entry->mutableBody<ChoicePoint>();

  choicePoint.currentEnvironment = currentEnvironment;
  choicePoint.continuePoint = continuePoint;
  choicePoint.nextChoicePoint = currentChoicePoint;
  choicePoint.retryLabel = retryLabel;
  choicePoint.currentCutPoint = currentCutPoint;
  choicePoint.savedRegisterCount = argumentCount;

  for (ChoicePointSize i = 0; i < argumentCount; ++i) {
    choicePoint.savedRegisters[i] = registers[i];
  }

  currentChoicePoint = entry;

  return entry;
}

RegistryEntry *newTrailItem(RegistryEntry *reference) {
  VERBOSE(Serial << "New Trail Item:" << endl);

  RegistryEntry *entry = newRegistryEntryWithTuple(
      RegistryEntry::Type::trailItem, sizeof(TrailItem), 0);

  if (entry == nullptr) {
    return nullptr;
  }

  entry->mutableBody<TrailItem>().item = reference;
  entry->mutableBody<TrailItem>().nextItem = trailHead;

  trailHead = entry;

  return entry;
}

RegistryEntry *currentStructureSubterm() {
  switch (currentStructure->type) {
  case RegistryEntry::Type::structure:
    return currentStructureSubterm(currentStructure->mutableBody<Structure>());
  case RegistryEntry::Type::list:
    return currentStructureSubterm(currentStructure->mutableBody<List>());
  default:
    Serial << "Type " << currentStructure->type << " is not a structure or list"
           << endl;
    throw "Not a structure or a list";
  }
}

RegistryEntry *setCurrentStructureSubterm(RegistryEntry *value) {
  switch (currentStructure->type) {
  case RegistryEntry::Type::structure:
    currentStructureSubterm(currentStructure->mutableBody<Structure>()) = value;
    break;
  case RegistryEntry::Type::list:
    currentStructureSubterm(currentStructure->mutableBody<List>()) = value;
    break;
  default:
    Serial << "Type " << currentStructure->type << " is not a structure or list"
           << endl;
    throw "Not a structure or a list";
  }

  return value;
}

RegistryEntry *&currentStructureSubterm(Structure &s) {
  RegistryEntry *&subterm = s.subterms[currentStructureSubtermIndex];

  ++currentStructureSubtermIndex;

  return subterm;
}

RegistryEntry *&currentStructureSubterm(List &l) {
  RegistryEntry *&subterm = l.subterms[currentStructureSubtermIndex];

  ++currentStructureSubtermIndex;

  return subterm;
}

void restoreChoicePoint(LabelIndex l) {
  restoreChoicePoint(currentChoicePoint->mutableBody<ChoicePoint>(), l);
}

void restoreChoicePoint(ChoicePoint &b, LabelIndex l) {
  for (ChoicePointSize i = 0; i < b.savedRegisterCount; ++i) {
    registers[i] = b.savedRegisters[i];
  }

  currentEnvironment = b.currentEnvironment;
  continuePoint = b.continuePoint;

  b.retryLabel = l;

  VERBOSE(Serial << "Unwinding Trail" << endl);

  while (trailHead > currentChoicePoint) {
    trailHead->mutableBody<TrailItem>().item->resetToVariable();
    trailHead = trailHead->mutableBody<TrailItem>().nextItem;
  }
}
