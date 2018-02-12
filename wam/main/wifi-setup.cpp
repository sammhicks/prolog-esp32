#include "wifi-setup.h"

const char *ssid = "ESP32 WAM";
const char *password =
    "#XAbHWYZodCXLfOm6!a^e^PVi9pqRCH#BlDq#nE4SpvABE20#fBPPNmdMqi8";
const IPAddress ipAddr(192, 168, 167, 1);
const IPAddress subnet(255, 255, 255, 0);
const uint16_t serverPort = 1;

WiFiServer wifiServer(serverPort);

void wifiEvent(WiFiEvent_t event) {
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

void setupWiFi() {
  WiFi.onEvent(wifiEvent);
  WiFi.softAP(ssid, password);
}
