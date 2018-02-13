#include "main.h"

void setup() {
  Serial.begin(115200);

  setupWiFi();

  SPIFFS.begin(true);
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
        default:
          Serial.printf("Unknown command %x\n",
                        static_cast<unsigned int>(command));
          break;
        }
      }

      yieldProcessor();
    }

    client.stop();
    Serial.println("client disconnected");
  }

  yieldProcessor();
}

void runPing(Client &client) {
  Serial.print("ping: ");
  while (client.available() == 0) {
    if (!client.connected()) {
      Serial.println("failed");
    }
  }
  uint8_t body = Read::Raw::uint8(client);
  Serial.println(body, HEX);
  Write::Raw::uint8(client, body);
}

void updateCode(Client &client) {
  Serial.println("code update");
  while (client.available() < sizeof(CodeIndex)) {
    if (!client.connected()) {
      Serial.println("client disconnected");
      return;
    }
  }

  CodeIndex codeLength = Read::Raw::uint32(client);

  Serial.printf("code length: %x\n", codeLength);

  if (updateFile(codePath, codeLength, client)) {
    client.write(1);
  } else {
    client.write(0);
    deleteHash();
  }

  Serial.println("code update complete");
}

void updateLabelTable(Client &client) {
  Serial.println("label table update");
  while (client.available() < sizeof(CodeIndex)) {
    if (!client.connected()) {
      Serial.println("client disconnected");
      return;
    }
  }

  CodeIndex labelTableLength = Read::Raw::uint32(client);

  Serial.printf("label table length: %x\n", labelTableLength);

  if (updateFile(labelTablePath, labelTableLength, client)) {
    client.write(1);
  } else {
    client.write(0);
    deleteHash();
  }

  Serial.println("label table update complete");
}
