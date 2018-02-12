#pragma once

#include "yield.h"

namespace Read {
namespace Raw {
uint8_t uint8(Stream &s);
uint16_t uint16(Stream &s);
uint32_t uint32(Stream &s);
} // namespace Raw
} // namespace Read

namespace Write {
namespace Raw {
void uint8(Stream &s, uint8_t v);
void uint16(Stream &s, uint16_t v);
void uint32(Stream &s, uint32_t v);
} // namespace Raw
} // namespace Write
