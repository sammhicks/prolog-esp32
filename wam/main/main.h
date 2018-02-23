#pragma once

#include "commands.h"
#include "hash.h"
#include "machine.h"
#include "wifi-setup.h"

void setup();

void loop();

void runPing(Client &client);

void updateCode(Client &client);

void updateLabelTable(Client &client);

void readStructure(Client &client);

void readList(Client &client);
