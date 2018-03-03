#include "memory-scanning.h"

using Scanning::scanEntry;

void initScanning() {
#ifdef VERBOSE_LOG
  Serial << "Init Scanning" << endl;
#endif

  garbageCollectionState = GarbageCollectionStates::scan;
  scanCurrentHead = nullptr;
  scanNextHead = nullptr;

  scanNext(currentEnvironment);
  scanNext(currentChoicePoint);
  scanNext(currentCutPoint);
  scanNext(trailHead);

  for (Xn i = 0; i < registerCount; ++i) {
    scanNext(registers[i]);
  }
}

bool scanStep() {
#ifdef VERBOSE_LOG
  Serial << "Scan Step: " << scanCurrentHead << endl;
#endif
  if (scanCurrentHead == nullptr) {
    if (scanNextHead == nullptr) {
#ifdef VERBOSE_LOG
      Serial << "Finished scanning" << endl;
#endif
      return true;
    }

#ifdef VERBOSE_LOG
    Serial << "Scanning next" << endl;
#endif

    scanCurrentHead = scanNextHead;
    scanNextHead = nullptr;
  }

  scanEntry(scanCurrentHead);
  scanCurrentHead = scanCurrentHead->next;

#ifdef VERBOSE_LOG
  Serial << "New Current Head: " << scanCurrentHead << endl;
#endif

  return false;
}

void scanNext(RegistryEntry *entry) {
#ifdef VERBOSE_LOG
  Serial << "Adding to next: " << entry << endl;
#endif

  if (entry == nullptr) {
    return;
  }

  if (entry->marked) {
#ifdef VERBOSE_LOG
    Serial << "entry is marked" << endl;
#endif

    return;
  }

  entry->marked = true;

#ifdef VERBOSE_LOG
  Serial << "Current Next: " << scanNextHead << endl;
#endif

  entry->next = scanNextHead;
  scanNextHead = entry;

#ifdef VERBOSE_LOG
  Serial << "New Next: " << entry << endl;
#endif
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
#ifdef VERBOSE_LOG
  Serial << "Scanning structure" << endl;
#endif

  for (Arity i = 0; i < structure.arity; ++i) {
    scanNext(structure.subterms[i]);
  }
}

void scanBody(const List &list) {
#ifdef VERBOSE_LOG
  Serial << "Scanning list" << endl;
#endif

  scanNext(list.subterms[0]);
  scanNext(list.subterms[1]);
}

void scanBody(const Environment &environment) {
#ifdef VERBOSE_LOG
  Serial << "Scanning Environment" << endl;
#endif

  scanNext(environment.nextEnvironment);

  for (EnvironmentSize i = 0; i < environment.size; ++i) {
    scanNext(environment.permanentVariables[i]);
  }
}

void scanBody(const ChoicePoint &choicePoint) {
#ifdef VERBOSE_LOG
  Serial << "Scanning choice point" << endl;
#endif

  scanNext(choicePoint.currentEnvironment);
  scanNext(choicePoint.nextChoicePoint);
  scanNext(choicePoint.currentCutPoint);

  for (ChoicePointSize i = 0; i < choicePoint.savedRegisterCount; ++i) {
    scanNext(choicePoint.savedRegisters[i]);
  }
}

void scanBody(const TrailItem &item) {
#ifdef VERBOSE_LOG
  Serial << "Scanning trail item" << endl;
#endif

  scanNext(item.item);
  scanNext(item.nextItem);
}
}; // namespace Scanning
