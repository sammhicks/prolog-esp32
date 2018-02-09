#pragma once

#include "raw-int.h"

enum class Command : uint8_t {
  ping = 0x00,
  checkHash = 0x10,
  updateHash,
  updateLabelTable = 0x20,
  updateProgram,
  runQuery = 0x30,
  readRegister = 0x40,
  readMemory
};

bool commandWaiting(Stream &s);

Command nextCommand(Stream &s);
