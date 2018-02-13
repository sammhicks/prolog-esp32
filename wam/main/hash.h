#pragma once

#include "Client.h"
#include "SPIFFS.h"

#include "raw-int.h"
#include "update-file.h"

extern const char *hashPath;

bool checkHash(Client &client);
void updateHash(Client &client);
void deleteHash();
