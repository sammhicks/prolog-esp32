#include "wifi-setup.h"

const IPAddress ipAddr(192, 168, 167, 1);
const IPAddress subnet(255, 255, 255, 0);
const uint16_t serverPort = 1;

WiFiServer wifiServer(serverPort);

void wifiEvent(WiFiEvent_t event) {
  switch (event) {
  case SYSTEM_EVENT_AP_START:
    WiFi.softAPConfig(ipAddr, ipAddr, subnet);
    wifiServer.begin();
    break;
  default:
    break;
  }
}

void setupWiFi() {
  WiFi.onEvent(wifiEvent);
  WiFi.softAP(CONFIG_ESP_WIFI_SSID, CONFIG_ESP_WIFI_PASSWORD);
}
