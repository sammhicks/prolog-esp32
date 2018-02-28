#pragma once
#pragma pack(push, 1)

#include "value.h"

const Xn registerCount = 32;
const size_t TupleRegistryCapacity = 2 << 10;
const size_t TuplesHeapCapacity = 2 << 15;

extern RegistryEntry *registers[registerCount];

extern size_t tupleRegistrySize;
extern RegistryEntry *nextFreeRegistryEntry;
extern RegistryEntry tupleRegistry[TupleRegistryCapacity];

extern uint8_t *nextFreeTuple;
extern uint8_t tuplesHeap[TuplesHeapCapacity];

extern Arity argumentCount;
extern CodeIndex continuePoint;

extern RegistryEntry *currentStructure;
extern Arity currentStructureSubtermIndex;

extern RegistryEntry *currentEnvironment;
extern RegistryEntry *currentChoicePoint;
extern RegistryEntry *currentCutPoint;
extern RegistryEntry *trailHead;

void resetMemory();

#pragma pack(pop)
