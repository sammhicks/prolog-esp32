#pragma once
#pragma pack(push, 1)

#include "value.h"

const Xn registerCount = CONFIG_VM_REGISTER_COUNT;
const size_t tupleRegistryCapacity = 2 << CONFIG_VM_LOG_2_REGISTRY_SIZE;
const size_t tuplesHeapCapacity = 2 << CONFIG_VM_LOG_2_TUPLE_HEAP_SIZE;

extern RegistryEntry *registers[registerCount];

extern size_t tupleRegistryUsageCount;
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
