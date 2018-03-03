#pragma once

#include "memory-scanning.h"

RegistryEntry *newRegistryEntry(RegistryEntry::Type type);
Tuple *newTuple(RegistryEntry *entry, size_t headSize, Arity n = 0);

RegistryEntry *newVariable();
RegistryEntry *newStructure(Functor f, Arity n);
RegistryEntry *newList();
RegistryEntry *newConstant(Constant c);
RegistryEntry *newInteger(Integer i);
RegistryEntry *newEnvironment(EnvironmentSize n);
RegistryEntry *newChoicePoint(LabelIndex retryLabel);
void newTrailItem(RegistryEntry *reference);

RegistryEntry *&currentStructureSubterm();
RegistryEntry *&currentStructureSubterm(Structure &s);
RegistryEntry *&currentStructureSubterm(List &l);

void restoreChoicePoint(LabelIndex l = std::numeric_limits<LabelIndex>::max());
void restoreChoicePoint(ChoicePoint &b, LabelIndex l);
