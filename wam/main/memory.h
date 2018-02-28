#pragma once

#include <limits>

#include "value.h"

extern size_t tupleRegistrySize;
extern RegistryEntry *nextFreeRegistryEntry;
extern uint8_t *nextFreeTuple;
extern RegistryEntry *trailHead;

extern CodeIndex continuePoint;
extern RegistryEntry *currentEnvironment;
extern RegistryEntry *currentChoicePoint;
extern RegistryEntry *currentCutPoint;
extern Arity argumentCount;

extern RegistryEntry *currentStructure;
extern Arity currentStructureSubtermIndex;

void resetMemory();

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

void restoreChoicePoint(LabelIndex l = std::numeric_limits<LabelIndex>::max());
void restoreChoicePoint(ChoicePoint &b, LabelIndex l);
