#pragma once

#include <Arduino.h>
#include <WiFi.h>

extern WiFiServer wifiServer;

void wifiEvent(WiFiEvent_t event);

void setupWiFi();
