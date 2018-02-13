#include "hash.h"

const char *hashPath = "/hash";

bool checkHash(Client &client) {
  Serial.println("check hash:");
  HashLength hashlength = Raw::read<HashLength>(client);

  Serial.printf("hash length: %u\n", hashlength);

  File hashFile = SPIFFS.open(hashPath);

  bool hashCorrect = true;

  for (HashLength n = 0; n < hashlength; ++n) {
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

  while (client.available() < sizeof(HashLength)) {
    if (!client.connected()) {
      return;
    }
    yieldProcessor();
  }

  HashLength hashLength = Raw::read<HashLength>(client);

  if (updateFile(hashPath, hashLength, client)) {
    client.write(1);
  } else {
    deleteHash();
    client.write(0);
  }
  Serial.println("hash update complete");
}

void deleteHash() { SPIFFS.remove(hashPath); }
