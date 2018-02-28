#include "serial-stream.h"

NewLine endl;

Print &operator<<(Print &os, const NewLine &) {
  os.println();
  return os;
}

Print &operator<<(Print &os, const char *str) {
  os.print(str);
  return os;
}

Print &operator<<(Print &os, const void *ptr) {
  os.print(reinterpret_cast<const int>(ptr), DEC);
  return os;
}

Print &operator<<(Print &os, uint8_t n) {
  os.print(n, HEX);
  return os;
}

Print &operator<<(Print &os, uint16_t n) {
  os.print(n, HEX);
  return os;
}

Print &operator<<(Print &os, uint32_t n) {
  os.print(n, HEX);
  return os;
}

Print &operator<<(Print &os, int16_t n) {
  os.print(n, DEC);
  return os;
}

Print &operator<<(Print &os, int32_t n) {
  os.print(n, DEC);
  return os;
}
