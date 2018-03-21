#include "instruction.h"

Print &operator<<(Print &os, const Opcode &c) {
  os.print(static_cast<uint8_t>(c));
  return os;
}

Print &operator<<(Print &os, const DigitalPinModes &pin) {
  os.print(static_cast<uint8_t>(pin));
  return os;
}
