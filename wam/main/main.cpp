#include <algorithm>

#include <Arduino.h>
#include <WiFi.h>

const char *ssid = "ESP32 WAM";
const char *password =
    "#XAbHWYZodCXLfOm6!a^e^PVi9pqRCH#BlDq#nE4SpvABE20#fBPPNmdMqi8";
const IPAddress ipAddr(192, 168, 0, 1);
const IPAddress subnet(255, 255, 255, 0);

void WiFiEvent(WiFiEvent_t event) {
  switch (event) {
  case SYSTEM_EVENT_AP_START:
    WiFi.softAPConfig(ipAddr, ipAddr, subnet);
    break;
  default:
    break;
  }
}

void setup() {
  Serial.begin(115200);

  WiFi.onEvent(WiFiEvent);
  WiFi.softAP(ssid, password);
}

void loop() { delay(10); }
