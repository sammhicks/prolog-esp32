#include "commands.h"

bool commandWaiting(Stream &stream) {
  return stream.available() >= sizeof(Command);
}

Command nextCommand(Stream &stream) { return Raw::read<Command>(stream); }

Print &operator<<(Print &os, const Command &c) {
  os.print(static_cast<uint8_t>(c), HEX);
  return os;
}
