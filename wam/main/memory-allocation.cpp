#include "memory-allocation.h"

RegistryEntry *newRegistryEntry(RegistryEntry::Type type) {
  RegistryEntry *newEntry;

  if (nextFreeRegistryEntry == nullptr) {
#ifdef VERBOSE_LOG
    Serial << "\tNew Registry Entry: ";
#endif

    newEntry = tupleRegistry + tupleRegistrySize;
    ++tupleRegistrySize;
  } else {
#ifdef VERBOSE_LOG
    Serial << "\tReusing Registry Entry: ";
#endif

    newEntry = nextFreeRegistryEntry;
    nextFreeRegistryEntry = nextFreeRegistryEntry->next;
  }

#ifdef VERBOSE_LOG
  Serial << (newEntry - tupleRegistry) << endl;
#endif

  newEntry->type = type;

  return newEntry;
}

Tuple *newTuple(RegistryEntry *entry, size_t headSize, Arity n) {
  Tuple *tuple = reinterpret_cast<Tuple *>(nextFreeTuple);
  nextFreeTuple += (sizeof(Tuple) + headSize + n * sizeof(RegistryEntry *));
  tuple->entry = entry;

#ifdef VERBOSE_LOG
  Serial << "\tNew Tuple at "
         << (reinterpret_cast<uint8_t *>(tuple) - tuplesHeap)
         << " with an entry at " << (tuple->entry - tupleRegistry) << endl;
#endif

  return tuple;
}

RegistryEntry *newVariable() {
#ifdef VERBOSE_LOG
  Serial << "New Variable:" << endl;
#endif

  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::reference);

  entry->tuple = newTuple(entry, ScalarSize);

  entry->mutableBody<RegistryEntry *>() = entry;

  return entry;
}

RegistryEntry *newStructure(Functor f, Arity n) {
#ifdef VERBOSE_LOG
  Serial << "New Structure:" << endl;
#endif

  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::structure);

  entry->tuple = newTuple(entry, sizeof(Structure), n);

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
#ifdef VERBOSE_LOG
  Serial << "New List:" << endl;
#endif

  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::list);

  entry->tuple = newTuple(entry, 0, 2);

  List &list = entry->mutableBody<List>();

  list.subterms[0] = nullptr;
  list.subterms[1] = nullptr;

  currentStructure = entry;
  currentStructureSubtermIndex = 0;

  return entry;
}

RegistryEntry *newConstant(Constant c) {
#ifdef VERBOSE_LOG
  Serial << "New Constant:" << endl;
#endif

  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::constant);

  entry->tuple = newTuple(entry, ScalarSize);

  entry->mutableBody<Constant>() = c;

  return entry;
}

RegistryEntry *newInteger(Integer i) {
#ifdef VERBOSE_LOG
  Serial << "New Integer:" << endl;
#endif

  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::integer);

  entry->tuple = newTuple(entry, ScalarSize);

  entry->mutableBody<Integer>() = i;

  return entry;
}

RegistryEntry *newEnvironment(EnvironmentSize n) {
#ifdef VERBOSE_LOG
  Serial << "New Environment of size " << n << ":" << endl;
#endif

  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::environment);

  entry->tuple = newTuple(entry, sizeof(Environment), n);

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
#ifdef VERBOSE_LOG
  Serial << "New Choice Point:" << endl;
#endif

  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::choicePoint);

  entry->tuple = newTuple(entry, sizeof(ChoicePoint), argumentCount);

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

void newTrailItem(RegistryEntry *reference) {
#ifdef VERBOSE_LOG
  Serial << "New Trail Item:" << endl;
#endif

  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::trailItem);

  entry->tuple = newTuple(entry, sizeof(TrailItem));

  entry->mutableBody<TrailItem>().item = reference;
  entry->mutableBody<TrailItem>().nextItem = trailHead;

  trailHead = entry;
}

RegistryEntry *&currentStructureSubterm() {
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

  Serial << "Unwinding Trail" << endl;

  while (trailHead > currentChoicePoint) {
    trailHead->mutableBody<TrailItem>().item->resetToVariable();
    trailHead = trailHead->mutableBody<TrailItem>().nextItem;
  }
}
