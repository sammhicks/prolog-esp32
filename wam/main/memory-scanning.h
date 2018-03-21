#pragma once

#include "registers.h"

void initScanning();
bool scanStep();
void scanNext(RegistryEntry *entry);

namespace Scanning {
void scanEntry(RegistryEntry *entry);

void scanBody(const Structure &structure);
void scanBody(const List &list);
void scanBody(const Environment &environment);
void scanBody(const ChoicePoint &choicePoint);
void scanBody(const TrailItem &item);
}; // namespace Scanning
