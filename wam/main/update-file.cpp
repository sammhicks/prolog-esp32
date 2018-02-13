#include "update-file.h"

bool updateFile(const char *path, size_t newLength, Client &client) {
  File codeFile = SPIFFS.open(path, FILE_WRITE);

  for (size_t n = 0; n < newLength; ++n) {
    while (client.available() == 0) {
      if (!client.connected()) {
        Serial.println();
        return false;
      }
      yieldProcessor();
    }

    int nextByte = client.read();

    Serial.print(nextByte, HEX);
    Serial.print(" ");

    codeFile.write(nextByte);
  }

  Serial.println();

  return true;
}
