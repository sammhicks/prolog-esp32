#include "garbage-collection.h"

#include "garbage-collection.h"

bool garbageCollectionStep() {
  switch (garbageCollectionState) {
  case GarbageCollectionStates::scan:
    if (scanStep()) {
      initSweeping();
    }
    break;
  case GarbageCollectionStates::sweep:
    if (sweepStep()) {
      initScanning();

      if (deadCount == 0) {
        LOG(Serial << "pausing garbage collection" << endl);
        garbageCollectionRunning = false;
      }
      return true;
    }
    break;
  }
  return false;
}

void fullGarbageCollection() {
  while (garbageCollectionRunning) {
    garbageCollectionStep();
  }

  garbageCollectionRunning = true;

  while (garbageCollectionRunning) {
    garbageCollectionStep();
  }
}
