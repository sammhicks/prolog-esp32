#include "hash.h"

const char *hashPath = "/hash";

bool checkHash(Client &client) {
  Serial.println("check hash:");
  uint8_t hashSize = Read::Raw::uint8(client);

  Serial.printf("hash length: %u\n", hashSize);

  File hashFile = SPIFFS.open(hashPath);

  bool hashCorrect = true;

  for (uint8_t n = 0; n < hashSize; ++n) {
    while (client.available() == 0) {
      if (!client.connected()) {
        Serial.println("client disconnected during hash check");
        return false;
      }
      yieldProcessor();
    }
    if (client.read() != hashFile.read()) {
      hashCorrect = false;
    }
  }

  client.write(hashCorrect ? 1 : 0);

  Serial.println(hashCorrect ? "hash matches" : "hash does not match");

  return hashCorrect;
}

void updateHash(Client &client) {
  Serial.println("hash update");
  uint8_t hashLength;

  while (client.available() < sizeof(hashLength)) {
    if (!client.connected()) {
      return;
    }
    yieldProcessor();
  }

  hashLength = Read::Raw::uint8(client);

  if (updateFile(hashPath, hashLength, client)) {
    client.write(1);
  } else {
    deleteHash();
    client.write(0);
  }
  Serial.println("hash update complete");
}

void deleteHash() { SPIFFS.remove(hashPath); }
