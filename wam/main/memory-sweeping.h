#pragma once

#include <cstring>

#include "registers.h"

extern size_t liveCount;
extern size_t deadCount;

void initSweeping();

bool sweepStep();
