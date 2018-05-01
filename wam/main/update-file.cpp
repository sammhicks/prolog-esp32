#include "update-file.h"

bool updateFile(const char *path, size_t newLength, Client &client) {
  File codeFile = SPIFFS.open(path, FILE_WRITE);

  for (size_t n = 0; n < newLength; ++n) {
    while (client.available() == 0) {
      if (!client.connected()) {
        VERBOSE(Serial << endl);
        return false;
      }
      yieldProcessor();
    }

    int nextByte = client.read();

    VERBOSE(Serial << static_cast<uint8_t>(nextByte) << " ");

    codeFile.write(nextByte);
  }

  VERBOSE(Serial << endl);

  client.write(1);

  return true;
}
