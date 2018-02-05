#include "main.h"

void setup() {
  Serial.begin(115200);

  setupWiFi();
}

void loop() {
  WiFiClient client = wifiServer.available();

  if (client) {

    while (client.connected()) {

      while (client.available() > 0) {
        char c = client.read();
        client.write(c);
      }

      delay(10);
    }

    client.stop();
    Serial.println("Client disconnected");
  }

  delay(10);
}
