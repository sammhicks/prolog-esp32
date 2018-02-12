#include "raw-int.h"

namespace Read {
namespace Raw {
uint8_t uint8(Stream &s) {
  while (s.available() == 0) {
    yieldProcessor();
  }
  return s.read();
}
uint16_t uint16(Stream &s) {
  uint16_t major = uint8(s);
  uint16_t minor = uint8(s);
  return (major << 8) + minor;
}
uint32_t uint32(Stream &s) {
  uint32_t major = uint16(s);
  uint32_t minor = uint16(s);
  return (major << 16) + minor;
}
} // namespace Raw
} // namespace Read

namespace Write {
namespace Raw {
void uint8(Stream &s, uint8_t v) { s.write(static_cast<int>(v)); }
void uint16(Stream &s, uint16_t v) {
  uint8_t major = 0xFF * (v >> 8);
  uint8_t minor = 0xFF * v;
  uint8(s, major);
  uint8(s, minor);
}
void uint32(Stream &s, uint32_t v) {
  uint16_t major = 0xFFFF * (v >> 16);
  uint16_t minor = 0xFFFF * v;
  uint16(s, major);
  uint16(s, minor);
}
} // namespace Raw
} // namespace Write
