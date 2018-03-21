#pragma once

#include "Client.h"
#include "SPIFFS.h"

#include "raw-io.h"
#include "serial-stream.h"
#include "update-file.h"
#include "verbose-log.h"

typedef uint8_t HashLength;

extern const char *hashPath;

bool checkHash(Client &client);
void updateHash(Client &client);
void deleteHash();
