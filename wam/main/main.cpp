#include <algorithm>

#include <Arduino.h>
#include <WiFi.h>

const char *ssid = "ESP32 WAM";
const char *password =
    "#XAbHWYZodCXLfOm6!a^e^PVi9pqRCH#BlDq#nE4SpvABE20#fBPPNmdMqi8";
const IPAddress ipAddr(192, 168, 167, 1);
const IPAddress subnet(255, 255, 255, 0);
const uint16_t serverPort = 1;

WiFiServer wifiServer(serverPort);

void WiFiEvent(WiFiEvent_t event) {
  switch (event) {
  case SYSTEM_EVENT_AP_START:
    WiFi.softAPConfig(ipAddr, ipAddr, subnet);
    break;
  case SYSTEM_EVENT_STA_GOT_IP:
    wifiServer.begin();
  default:
    break;
  }
}

void setup() {
  Serial.begin(115200);

  WiFi.onEvent(WiFiEvent);
  WiFi.softAP(ssid, password);
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
