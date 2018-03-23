#pragma once

#include "Arduino.h"

#include "commands.h"
#include "hash.h"
#include "machine.h"
#include "wifi-setup.h"

void setup();

void loop();

void updateProgram(Client &client);

void readValue(Client &client);
