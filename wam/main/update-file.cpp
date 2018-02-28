#include "update-file.h"

bool updateFile(const char *path, size_t newLength, Client &client) {
  File codeFile = SPIFFS.open(path, FILE_WRITE);

  for (size_t n = 0; n < newLength; ++n) {
    while (client.available() == 0) {
      if (!client.connected()) {
        Serial << endl;
        return false;
      }
      yieldProcessor();
    }

    int nextByte = client.read();

#ifdef VERBOSE_LOG
    Serial << static_cast<uint8_t>(nextByte) << " ";
#endif

    codeFile.write(nextByte);
  }

  Serial << endl;

  return true;
}
