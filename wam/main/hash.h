#pragma once

#include "Client.h"
#include "SPIFFS.h"

#include "raw-io.h"
#include "update-file.h"

typedef uint8_t HashLength;

extern const char *hashPath;

bool checkHash(Client &client);
void updateHash(Client &client);
void deleteHash();
