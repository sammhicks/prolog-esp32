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
        case Command::ping:
          runPing(client);
          break;
        case Command::checkHash:
          checkHash(client);
          break;
        case Command::updateHash:
          updateHash(client);
          break;
        case Command::updateProgram:
          updateCode(client);
          break;
        case Command::updateLabelTable:
          updateLabelTable(client);
          break;
        case Command::resetMachine:
          resetMachine();
          Raw::write<bool>(client, true);
          break;
        case Command::runQuery:
          executeInstructions(&client);
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

void runPing(Client &client) {
  Serial << "ping: ";
  while (client.available() == 0) {
    if (!client.connected()) {
      Serial << "failed" << endl;
    }
  }
  uint8_t body = Raw::read<uint8_t>(client);

  Serial << body;

  Raw::write(client, body);
}

void updateCode(Client &client) {
  Serial << "code update" << endl;
  while (client.available() < sizeof(CodeIndex)) {
    if (!client.connected()) {
      Serial << "client disconnected" << endl;
      return;
    }
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

void updateLabelTable(Client &client) {
  Serial << "label table update" << endl;
  while (client.available() < sizeof(CodeIndex)) {
    if (!client.connected()) {
      Serial.println("client disconnected");
      return;
    }
  }

  CodeIndex labelTableLength = Raw::read<CodeIndex>(client);

  Serial << "code length: " << labelTableLength << endl;

  if (updateFile(labelTablePath, labelTableLength, client)) {
    Raw::write<bool>(client, true);
  } else {
    Raw::write<bool>(client, false);
    deleteHash();
  }

  Serial << "label table update complete" << endl;
}

void readValue(Client &client) {
  RegistryEntry *entry = Raw::read<RegistryEntry *>(client);

  Serial << "Reading entry: " << *entry << endl;

  entry->sendToClient(client);
}
