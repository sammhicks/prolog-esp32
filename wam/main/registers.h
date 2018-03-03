#pragma once
#pragma pack(push, 1)

#include "value.h"

const Xn registerCount = 32;
const size_t tupleRegistryCapacity = 2 << 10;
const size_t tuplesHeapCapacity = 2 << 15;

extern RegistryEntry *registers[registerCount];

extern size_t tupleRegistrySize;
extern RegistryEntry *nextFreeRegistryEntry;
extern RegistryEntry tupleRegistry[tupleRegistryCapacity];

extern uint8_t *nextFreeTuple;
extern uint8_t tuplesHeap[tuplesHeapCapacity];

extern Arity argumentCount;
extern CodeIndex continuePoint;

extern RegistryEntry *currentStructure;
extern Arity currentStructureSubtermIndex;

extern RegistryEntry *currentEnvironment;
extern RegistryEntry *currentChoicePoint;
extern RegistryEntry *currentCutPoint;
extern RegistryEntry *trailHead;

extern bool garbageCollectionRunning;
extern GarbageCollectionStates garbageCollectionState;
extern RegistryEntry *scanCurrentHead;
extern RegistryEntry *scanNextHead;
extern uint8_t *sweepSource;
extern uint8_t *sweepDestination;

void resetMemory();

#pragma pack(pop)
