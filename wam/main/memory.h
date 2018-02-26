#pragma once

#include "value.h"

const Xn registerCount = 32;
const size_t TupleRegistryCapacity = 1024;
const size_t TuplesHeapCapacity = 4096;

extern RegistryEntry *registers[registerCount];
extern RegistryEntry tupleRegistry[TupleRegistryCapacity];
extern uint8_t tuplesHeap[TuplesHeapCapacity];

extern size_t tupleRegistrySize;
extern RegistryEntry *nextFreeRegistryEntry;
extern Tuple *nextFreeTuple;
extern RegistryEntry *trailHead;

extern CodeIndex continuePoint;
extern RegistryEntry *currentEnvironment;
extern RegistryEntry *currentChoicePoint;
extern RegistryEntry *cutChoicePoint;

extern RegistryEntry **structureIterator;

void resetMemory();

RegistryEntry *newRegistryEntry(RegistryEntry::Type type);
Tuple *newTuple(RegistryEntry *entry, size_t headSize, Arity n = 0);

RegistryEntry *newVariable();
RegistryEntry *newStructure(Functor f, Arity n);
RegistryEntry *newList();
RegistryEntry *newConstant(Constant c);
RegistryEntry *newInteger(Integer i);
RegistryEntry **newEnvironment(EnvironmentSize n);
void newChoicePoint(ChoicePointSize n);
void newTrailItem(RegistryEntry *reference);
