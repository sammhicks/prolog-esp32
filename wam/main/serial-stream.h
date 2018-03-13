#pragma once

#include "HardwareSerial.h"

class NewLine {};

extern NewLine endl;

Print &operator<<(Print &os, const NewLine &nl);
Print &operator<<(Print &os, const char *str);
Print &operator<<(Print &os, uint8_t n);
Print &operator<<(Print &os, uint16_t n);
Print &operator<<(Print &os, uint32_t n);
Print &operator<<(Print &os, int16_t n);
Print &operator<<(Print &os, int32_t n);
Print &operator<<(Print &os, double n);
