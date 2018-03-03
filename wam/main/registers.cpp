#include "registers.h"

RegistryEntry *registers[registerCount];

size_t tupleRegistrySize;
RegistryEntry *nextFreeRegistryEntry;
RegistryEntry tupleRegistry[tupleRegistryCapacity];

uint8_t *nextFreeTuple;
uint8_t tuplesHeap[tuplesHeapCapacity];

Arity argumentCount;
CodeIndex continuePoint;

RegistryEntry *currentStructure;
Arity currentStructureSubtermIndex;

RegistryEntry *currentEnvironment;
RegistryEntry *currentChoicePoint;
RegistryEntry *currentCutPoint;
RegistryEntry *trailHead;

GarbageCollectionStates garbageCollectionState;
RegistryEntry *scanCurrentHead;
RegistryEntry *scanNextHead;
uint8_t *sweepSource;
uint8_t *sweepDestination;

void resetMemory() {
  for (Xn i = 0; i < registerCount; ++i) {
    registers[i] = nullptr;
  }

  tupleRegistrySize = 0;
  nextFreeRegistryEntry = nullptr;
  nextFreeTuple = tuplesHeap;
  trailHead = nullptr;

  currentEnvironment = nullptr;
  currentChoicePoint = nullptr;
  currentCutPoint = nullptr;
}
