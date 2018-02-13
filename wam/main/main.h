#pragma once

#include "commands.h"
#include "hash.h"
#include "machine.h"
#include "wifi-setup.h"

const char *codePath = "/code";
const char *labelTablePath = "/label-table";

void setup();

void loop();

void runPing(Client &client);

void updateCode(Client &client);

void updateLabelTable(Client &client);

void readRegister(Client &client);

void readMemory(Client &client);
