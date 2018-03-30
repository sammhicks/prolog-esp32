#include "hash.h"

const char *hashPath = "/hash";

bool checkHash(Client &client) {
  Serial << "check hash:" << endl;
  HashLength hashlength = Raw::read<HashLength>(client);

  Serial << "hash length: " << hashlength << endl;

  File hashFile = SPIFFS.open(hashPath);

  bool hashCorrect = true;

  for (HashLength n = 0; n < hashlength; ++n) {
    while (client.available() == 0) {
      if (!client.connected()) {
        Serial << "client disconnected during hash check" << endl;
        return false;
      }
      yieldProcessor();
    }
    if (client.read() != hashFile.read()) {
      hashCorrect = false;
    }
  }

  client.write(hashCorrect ? 1 : 0);

  Serial << (hashCorrect ? "hash matches" : "hash does not match") << endl;

  return hashCorrect;
}

void updateHash(Client &client) {
  Serial << "hash update" << endl;

  while (client.available() < sizeof(HashLength)) {
    if (!client.connected()) {
      return;
    }
    yieldProcessor();
  }

  HashLength hashLength = Raw::read<HashLength>(client);

  if (updateFile(hashPath, hashLength, client)) {
  } else {
    deleteHash();
  }

  Serial << "hash update complete" << endl;
}

void deleteHash() { SPIFFS.remove(hashPath); }
