#pragma once

#include "raw-io.h"
#include "serial-stream.h"

enum class Command : uint8_t {
  ping = 0x00,
  checkHash = 0x10,
  updateHash = 0x20,
  updateProgram,
  updateLabelTable,
  resetMachine = 0x30,
  runQuery,
  getNextAnswer,
  readValue = 0x40,
};

bool commandWaiting(Stream &s);

Command nextCommand(Stream &s);

Print &operator<<(Print &os, const Command &c);
