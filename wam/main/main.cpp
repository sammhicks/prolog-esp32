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
        case Command::ping: {
          Serial.print("ping: ");
          uint8_t body = Read::Raw::uint8(client);
          Serial.println(body, HEX);
          Write::Raw::uint8(client, body);
          break;
        }
        default:
          Serial.printf("Unknown command %x\n",
                        static_cast<unsigned int>(command));
          break;
        }
      }

      yieldProcessor();
    }

    client.stop();
    Serial.println("Client disconnected");
  }

  yieldProcessor();
}
