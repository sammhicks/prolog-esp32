#pragma once

#include "SPIFFS.h"

#include "raw-int.h"
#include "yield.h"

const char *hashPath = "/hash";

bool checkHash(Stream &s);
void updateHash(Stream &s);
