#pragma once

#include "raw-io.h"
#include "serial-stream.h"

enum class Command : uint8_t {
  updateProgram = 0x00,
  runQuery = 0x10,
  getNextAnswer,
  readValue = 0x20,
};

bool commandWaiting(Stream &s);

Command nextCommand(Stream &s);

Print &operator<<(Print &os, const Command &c);
