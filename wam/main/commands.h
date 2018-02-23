#pragma once

#include "raw-io.h"

enum class Command : uint8_t {
  ping = 0x00,
  checkHash = 0x10,
  updateHash = 0x20,
  updateProgram,
  updateLabelTable,
  resetMachine = 0x30,
  runQuery,
  getNextAnswer,
  readStructure = 0x40,
  readList,
};

bool commandWaiting(Stream &s);

Command nextCommand(Stream &s);