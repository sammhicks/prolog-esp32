#include "registers.h"

RegistryEntry *registers[registerCount];

size_t tupleRegistrySize;
RegistryEntry *nextFreeRegistryEntry;
RegistryEntry tupleRegistry[TupleRegistryCapacity];

uint8_t *nextFreeTuple;
uint8_t tuplesHeap[TuplesHeapCapacity];

Arity argumentCount;
CodeIndex continuePoint;

RegistryEntry *currentStructure;
Arity currentStructureSubtermIndex;

RegistryEntry *currentEnvironment;
RegistryEntry *currentChoicePoint;
RegistryEntry *currentCutPoint;
RegistryEntry *trailHead;

void resetMemory() {
  tupleRegistrySize = 0;
  nextFreeRegistryEntry = nullptr;
  nextFreeTuple = tuplesHeap;
  trailHead = nullptr;

  currentEnvironment = nullptr;
  currentChoicePoint = nullptr;
  currentCutPoint = nullptr;
}
