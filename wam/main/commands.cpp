#include "commands.h"

bool commandWaiting(Stream &s) { return s.available() > 0; }

Command nextCommand(Stream &s) {
  return static_cast<Command>(Read::Raw::uint8(s));
}
