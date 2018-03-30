#include "main.h"

void setup() {
  Serial.begin(115200);

  setupWiFi();

  SPIFFS.begin(true);

  analogReadResolution(analogResolution);
}

void loop() {
  WiFiClient client = wifiServer.available();

  if (client) {
    Serial.println("Client connected");
    while (client.connected()) {
      if (commandWaiting(client)) {
        switch (Command command = nextCommand(client)) {
        case Command::updateProgram:
          updateProgram(client);
          break;
        case Command::runQuery:
          runQuery(&client);
          break;
        case Command::getNextAnswer:
          getNextAnswer(&client);
          break;
        case Command::readValue:
          readValue(client);
          break;
        default:
          Serial << "Unknown command " << command << endl;
          break;
        }
      }

      yieldProcessor();
    }

    client.stop();
    Serial << "client disconnected" << endl;
  }

  yieldProcessor();
}

void updateProgram(Client &client) {
  if (checkHash(client)) {
    return;
  }

  updateHash(client);

  Serial << "code update" << endl;
  while (client.available() < sizeof(CodeIndex)) {
    if (!client.connected()) {
      Serial << "client disconnected" << endl;
      return;
    }
    yieldProcessor();
  }

  CodeIndex codeLength = Raw::read<CodeIndex>(client);

  Serial << "code length: " << codeLength << endl;

  if (updateFile(codePath, codeLength, client)) {
    Raw::write<bool>(client, true);
  } else {
    Raw::write<bool>(client, false);
    deleteHash();
  }

  Serial << "code update complete" << endl;
}

void readValue(Client &client) {
  RegistryEntry *entry = Raw::read<RegistryEntry *>(client);

  LOG(Serial << "Reading entry: ");

#ifdef VERBOSE_LOG
  LOG(Serial << *entry << endl);
#else
  LOG(Serial << entry << endl);
#endif

  entry->deref()->sendToClient(client);
}
