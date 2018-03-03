#include "memory-sweeping.h"

size_t liveCount;
size_t deadCount;

void initSweeping() {
#ifdef VERBOSE_LOG
  Serial << "Init Sweeping" << endl;
#endif

  garbageCollectionState = GarbageCollectionStates::sweep;

  sweepSource = tuplesHeap;
  sweepDestination = tuplesHeap;

  liveCount = 0;
  deadCount = 0;
}

bool sweepStep() {
#ifdef VERBOSE_LOG
  Serial << "Sweep" << endl;
#endif

  if (static_cast<void *>(sweepSource) >= static_cast<void *>(nextFreeTuple)) {
    nextFreeTuple = sweepDestination;

    Serial << "Livesize: " << (liveCount * 100.0) / (liveCount + deadCount)
           << "%" << endl;

    Serial << "Registry Usage: " << (liveCount * 100.0) / tupleRegistryCapacity
           << "%" << endl;

    Serial << "Tuple Heap Usage: "
           << ((nextFreeTuple - tuplesHeap) * 100.0) / tuplesHeapCapacity << "%"
           << endl;

    return true;
  }

  RegistryEntry *entry = reinterpret_cast<Tuple *>(sweepSource)->entry;
  size_t tupleSize = entry->tupleSize();

  if (entry->marked) {
#ifdef VERBOSE_LOG
    Serial << "Entry " << (entry - tupleRegistry) << " is live" << endl;
#endif

    ++liveCount;

    if (sweepSource != sweepDestination) {
      std::memcpy(sweepDestination, sweepSource, tupleSize);
      entry->tuple = reinterpret_cast<Tuple *>(sweepDestination);
    }

    sweepSource += tupleSize;
    sweepDestination += tupleSize;

    entry->marked = false;
  } else {
    Serial << "Entry " << (entry - tupleRegistry) << " is dead" << endl;

    ++deadCount;

    entry->next = nextFreeRegistryEntry;
    nextFreeRegistryEntry = entry;

    sweepSource += tupleSize;
  }

  return false;
}
