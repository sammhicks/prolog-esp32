#include "memory-scanning.h"

using Scanning::scanEntry;

void initScanning() {
  VERBOSE(Serial << "Init Scanning" << endl);

  garbageCollectionState = GarbageCollectionStates::scan;
  scanCurrentHead = nullptr;
  scanNextHead = nullptr;

  scanNext(partialNewRegistryEntry);
  scanNext(currentEnvironment);
  scanNext(currentChoicePoint);
  scanNext(currentCutPoint);
  scanNext(trailHead);

  for (Xn i = 0; i < registerCount; ++i) {
    scanNext(registers[i]);
  }
}

bool scanStep() {
  VERBOSE(Serial << "Scan Step: " << scanCurrentHead << endl);
  if (scanCurrentHead == nullptr) {
    if (scanNextHead == nullptr) {
      VERBOSE(Serial << "Finished scanning" << endl);
      return true;
    }

    VERBOSE(Serial << "Scanning next" << endl);

    scanCurrentHead = scanNextHead;
    scanNextHead = nullptr;
  }

  scanEntry(scanCurrentHead);
  scanCurrentHead = scanCurrentHead->next;

  VERBOSE(Serial << "New Current Head: " << scanCurrentHead << endl);

  return false;
}

void scanNext(RegistryEntry *entry) {
  VERBOSE(Serial << "Adding to next: " << entry << endl);

  if (entry == nullptr) {
    return;
  }

  if (entry->marked) {
    VERBOSE(Serial << "entry is marked" << endl);

    return;
  }

  entry->marked = true;

  VERBOSE(Serial << "Current Next: " << scanNextHead << endl);

  entry->next = scanNextHead;
  scanNextHead = entry;

  VERBOSE(Serial << "New Next: " << entry << endl);
}

namespace Scanning {
void scanEntry(RegistryEntry *entry) {
  switch (entry->type) {
  case RegistryEntry::Type::reference:
    scanNext(entry->mutableBody<RegistryEntry *>());
    break;
  case RegistryEntry::Type::structure:
    scanBody(entry->body<Structure>());
    break;
  case RegistryEntry::Type::list:
    scanBody(entry->body<List>());
    break;
  case RegistryEntry::Type::constant:
    break;
  case RegistryEntry::Type::integer:
    break;
  case RegistryEntry::Type::environment:
    scanBody(entry->body<Environment>());
    break;
  case RegistryEntry::Type::choicePoint:
    scanBody(entry->body<ChoicePoint>());
    break;
  case RegistryEntry::Type::trailItem:
    scanBody(entry->body<TrailItem>());
    break;
  }
}

void scanBody(const Structure &structure) {
  VERBOSE(Serial << "Scanning structure" << endl);

  for (Arity i = 0; i < structure.arity; ++i) {
    scanNext(structure.subterms[i]);
  }
}

void scanBody(const List &list) {
  VERBOSE(Serial << "Scanning list" << endl);

  scanNext(list.subterms[0]);
  scanNext(list.subterms[1]);
}

void scanBody(const Environment &environment) {
  VERBOSE(Serial << "Scanning Environment" << endl);

  scanNext(environment.nextEnvironment);

  for (EnvironmentSize i = 0; i < environment.size; ++i) {
    scanNext(environment.permanentVariables[i]);
  }
}

void scanBody(const ChoicePoint &choicePoint) {
  VERBOSE(Serial << "Scanning choice point" << endl);

  scanNext(choicePoint.currentEnvironment);
  scanNext(choicePoint.nextChoicePoint);
  scanNext(choicePoint.currentCutPoint);

  for (ChoicePointSize i = 0; i < choicePoint.savedRegisterCount; ++i) {
    scanNext(choicePoint.savedRegisters[i]);
  }
}

void scanBody(const TrailItem &item) {
  VERBOSE(Serial << "Scanning trail item" << endl);

  scanNext(item.item);
  scanNext(item.nextItem);
}
}; // namespace Scanning
