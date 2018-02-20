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
        case Command::readRegister:
          readRegister(client);
          break;
        case Command::readMemory:
          readMemory(client, true);
          break;
        case Command::readFunctor:
          readMemory(client, false);
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
  auto body = Raw::read<uint8_t>(client);
  Serial.println(body, HEX);
  Raw::write(client, body);
}

void updateCode(Client &client) {
  Serial.println("code update");
  while (client.available() < sizeof(CodeIndex)) {
    if (!client.connected()) {
      Serial.println("client disconnected");
      return;
    }
  }

  CodeIndex codeLength = Raw::read<CodeIndex>(client);

  Serial.printf("code length: %x\n", codeLength);

  if (updateFile(codePath, codeLength, client)) {
    Raw::write<bool>(client, true);
  } else {
    Raw::write<bool>(client, false);
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

  CodeIndex labelTableLength = Raw::read<CodeIndex>(client);

  Serial.printf("label table length: %x\n", labelTableLength);

  if (updateFile(labelTablePath, labelTableLength, client)) {
    Raw::write<bool>(client, true);
  } else {
    Raw::write<bool>(client, false);
    deleteHash();
  }

  Serial.println("label table update complete");
}

void readRegister(Client &client) {
  Serial.print("Reading Register: ");
  Xn xn = Raw::read<Xn>(client);
  Serial.println(xn, HEX);
  Value &value = reinterpret_cast<Environment *>(stack)->ys[xn];
  Raw::writeBlock<Value>(client, Ancillary::deref(value));
}

void readMemory(Client &client, bool followReference) {
  Serial.print("Reading Memory: ");
  HeapIndex hi = Raw::read<HeapIndex>(client);
  Serial.println(hi, HEX);
  Value &value = heap[hi];
  Value &valueToWrite = followReference ? Ancillary::deref(value) : value;
  Raw::writeBlock<Value>(client, valueToWrite);
}
