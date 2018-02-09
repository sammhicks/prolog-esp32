#include "hash.h"

bool checkHash(Stream &s) {
  uint8_t hashSize = Read::Raw::uint8(s);

  File hashFile = SPIFFS.open(hashPath);

  if (hashFile.size() != hashSize) {
    return false;
  }

  uint8_t n = 0;
  while (n < hashSize) {
    if (s.available() > 0) {
      if (s.read() != hashFile.read()) {
        return false;
      }
      ++n;
    } else {
      yieldProcessor();
    }
  }

  return true;
}

void updateHash(Stream &s) {
  uint8_t hashSize = Read::Raw::uint8(s);

  File hashFile = SPIFFS.open(hashPath, FILE_WRITE);

  uint8_t n = 0;
  while (n < hashSize) {
    if (s.available() > 0) {
      hashFile.write(s.read());
      ++n;
    } else {
      yieldProcessor();
    }
  }
}
