#pragma once

#include "Client.h"
#include "SPIFFS.h"

#include "yield.h"

bool updateFile(const char *path, size_t newLength, Client &client);
