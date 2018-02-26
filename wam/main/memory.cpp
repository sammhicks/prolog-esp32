#include "memory.h"

RegistryEntry *registers[registerCount];
RegistryEntry tupleRegistry[TupleRegistryCapacity];
uint8_t tuplesHeap[TuplesHeapCapacity];

size_t tupleRegistrySize;
RegistryEntry *nextFreeRegistryEntry;
Tuple *nextFreeTuple;
RegistryEntry *trailHead;

CodeIndex continuePoint;
RegistryEntry *currentEnvironment;
RegistryEntry *currentChoicePoint;
RegistryEntry *cutChoicePoint;

RegistryEntry **structureIterator;

void resetMemory() {
  tupleRegistrySize = 0;
  nextFreeRegistryEntry = nullptr;
  nextFreeTuple = reinterpret_cast<Tuple *>(tuplesHeap);
  trailHead = nullptr;

  currentEnvironment = nullptr;
  currentChoicePoint = nullptr;
  cutChoicePoint = nullptr;
}

RegistryEntry *newRegistryEntry(RegistryEntry::Type type) {
  RegistryEntry *newEntry;

  if (nextFreeRegistryEntry == nullptr) {
    newEntry = tupleRegistry + tupleRegistrySize;
    ++tupleRegistrySize;
  } else {
    newEntry = nextFreeRegistryEntry;
    nextFreeRegistryEntry = nextFreeRegistryEntry->next;
  }

  newEntry->type = type;

  return newEntry;
}

Tuple *newTuple(RegistryEntry *entry, size_t headSize, Arity n) {
  Tuple *tuple = nextFreeTuple;
  nextFreeTuple += (sizeof(Tuple) + headSize + n * sizeof(RegistryEntry *));
  tuple->entry = entry;
  return tuple;
}

RegistryEntry *newVariable() {
  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::reference);

  entry->tuple = newTuple(entry, sizeof(Tuple::Scalar));

  entry->tuple->body->reference = entry;

  return entry;
}

RegistryEntry *newStructure(Functor f, Arity n) {
  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::structure);

  entry->tuple = newTuple(entry, sizeof(Structure), n);

  Structure &structure = entry->tuple->body->structure;

  structure.functor = f;
  structure.arity = n;

  for (Arity i = 0; i < n; ++i) {
    structure.subterms[i] = nullptr;
  }

  structureIterator = structure.subterms;

  return entry;
}

RegistryEntry *newList() {
  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::list);

  entry->tuple = newTuple(entry, 0, 2);

  List &list = entry->tuple->body->list;

  list.subterms[0] = nullptr;
  list.subterms[1] = nullptr;

  structureIterator = list.subterms;

  return entry;
}

RegistryEntry *newConstant(Constant c) {
  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::constant);

  entry->tuple = newTuple(entry, sizeof(Tuple::Scalar));

  entry->tuple->body->constant = c;

  return entry;
}

RegistryEntry *newInteger(Integer i) {
  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::integer);

  entry->tuple = newTuple(entry, sizeof(Tuple::Scalar));

  entry->tuple->body->integer = i;

  return entry;
}

RegistryEntry **newEnvironment(EnvironmentSize n) {
  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::environment);

  entry->tuple = newTuple(entry, sizeof(Environment), n);

  Environment &environment = entry->tuple->body->environment;

  environment.nextEnvironment = currentEnvironment;
  environment.continuePoint = continuePoint;
  environment.size = n;
  environment.capacity = n;

  for (EnvironmentSize i = 0; i < n; ++i) {
    environment.permanentVariables[i] = nullptr;
  }

  currentEnvironment = entry;

  return environment.permanentVariables;
}

RegistryEntry *newChoicePoint(LabelIndex retryLabel, ChoicePointSize n) {
  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::choicePoint);

  entry->tuple = newTuple(entry, sizeof(ChoicePoint), n);

  ChoicePoint &choicePoint = entry->tuple->body->choicePoint;

  choicePoint.currentEnvironment = currentEnvironment;
  choicePoint.continuePoint = continuePoint;
  choicePoint.nextChoicePoint = currentChoicePoint;
  choicePoint.retryLabel = retryLabel;
  choicePoint.savedRegisterCount = n;

  for (ChoicePointSize i = 0; i < n; ++i) {
    choicePoint.savedRegisters[i] = registers[i];
  }

  currentChoicePoint = entry;

  return entry;
}

void newTrailItem(RegistryEntry *reference) {
  RegistryEntry *entry = newRegistryEntry(RegistryEntry::Type::trailItem);

  entry->tuple = newTuple(entry, sizeof(TrailItem));

  entry->tuple->body->trailItem.item = reference;
  entry->tuple->body->trailItem.nextItem = trailHead;

  trailHead = entry;
}
