#include "commands.h"

bool commandWaiting(Stream &stream) {
  return stream.available() >= sizeof(Command);
}

Command nextCommand(Stream &stream) { return Raw::read<Command>(stream); }
