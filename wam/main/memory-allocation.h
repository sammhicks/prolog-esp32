#pragma once

#include "garbage-collection.h"

RegistryEntry *newRegistryEntry(RegistryEntry::Type type);
Tuple *newTuple(size_t headSize, Arity n);

RegistryEntry *newRegistryEntryWithTuple(RegistryEntry::Type type,
                                         size_t headSize, Arity n);

RegistryEntry *newVariable();
RegistryEntry *newStructure(Functor f, Arity n);
RegistryEntry *newList();
RegistryEntry *newConstant(Constant c);
RegistryEntry *newInteger(Integer i);
RegistryEntry *newEnvironment(EnvironmentSize n);
RegistryEntry *newChoicePoint(LabelIndex retryLabel);
RegistryEntry *newTrailItem(RegistryEntry *reference);

RegistryEntry *currentStructureSubterm();
RegistryEntry *setCurrentStructureSubterm(RegistryEntry *value);
RegistryEntry *&currentStructureSubterm(Structure &s);
RegistryEntry *&currentStructureSubterm(List &l);

void restoreChoicePoint(LabelIndex l = std::numeric_limits<LabelIndex>::max());
void restoreChoicePoint(ChoicePoint &b, LabelIndex l);
