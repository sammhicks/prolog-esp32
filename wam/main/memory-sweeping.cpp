#include "memory-sweeping.h"

size_t liveCount;
size_t deadCount;
size_t previousLiveCount;

void initSweeping() {
  VERBOSE(Serial << "Init Sweeping" << endl);

  garbageCollectionState = GarbageCollectionStates::sweep;

  sweepSource = tuplesHeap;
  sweepDestination = tuplesHeap;

  liveCount = 0;
  deadCount = 0;
}

bool sweepStep() {
  VERBOSE(Serial << "Sweep" << endl);

  if (static_cast<void *>(sweepSource) >= static_cast<void *>(nextFreeTuple)) {
    // Shrink that tuple stack to fit usage
    nextFreeTuple = sweepDestination;

    LOG(Serial << "Livesize: " << (liveCount * 100.0) / (liveCount + deadCount)
               << "%" << endl);

    LOG(Serial << "Registry Usage: "
               << (liveCount * 100.0) / tupleRegistryCapacity << "%" << endl);

    LOG(Serial << "Tuple Heap Usage: "
               << ((nextFreeTuple - tuplesHeap) * 100.0) / tuplesHeapCapacity
               << "%" << endl);

    return true;
  }

  RegistryEntry *entry = reinterpret_cast<Tuple *>(sweepSource)->entry;
  size_t tupleSize = entry->tupleSize();

  if (entry->marked) {
    VERBOSE(Serial << "Entry " << (entry - tupleRegistry) << " is live"
                   << endl);

    ++liveCount;

    if (sweepSource != sweepDestination) {
      std::memcpy(sweepDestination, sweepSource, tupleSize);
      entry->tuple = reinterpret_cast<Tuple *>(sweepDestination);
    }

    sweepSource += tupleSize;
    sweepDestination += tupleSize;

    entry->marked = false;
  } else {
    LOG(Serial << "Entry " << (entry - tupleRegistry) << " is dead" << endl);

    ++deadCount;

    entry->next = nextFreeRegistryEntry;
    nextFreeRegistryEntry = entry;

    --tupleRegistryUsageCount;

    sweepSource += tupleSize;
  }

  return false;
}
