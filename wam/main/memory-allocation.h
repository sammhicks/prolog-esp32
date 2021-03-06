#pragma once

#include "garbage-collection.h"

bool spaceForRegistryEntry();
RegistryEntry *newRegistryEntry(RegistryEntry::Type type);

bool spaceForTuple(size_t headSize, Arity n);
Tuple *newTuple(size_t headSize, Arity n);

RegistryEntry *newRegistryEntryWithTuple(RegistryEntry::Type type,
                                         size_t headSize, Arity n);

RegistryEntry *newVariable();
RegistryEntry *newStructure(Functor f, Arity n);
RegistryEntry *newList();
RegistryEntry *newConstant(Constant c);
RegistryEntry *newInteger(Integer i);
RegistryEntry *newEnvironment(EnvironmentSize n);
RegistryEntry *newChoicePoint(CodeIndex retryIndex);
RegistryEntry *newTrailItem(RegistryEntry *reference);

RegistryEntry *currentStructureSubterm();
RegistryEntry *setCurrentStructureSubterm(RegistryEntry *value);
RegistryEntry *&currentStructureSubterm(Structure &s);
RegistryEntry *&currentStructureSubterm(List &l);

void restoreChoicePoint(CodeIndex p = std::numeric_limits<CodeIndex>::max());
void restoreChoicePoint(ChoicePoint &b, CodeIndex l);
