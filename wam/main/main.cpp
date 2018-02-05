#include <algorithm>

#include <Arduino.h>

int count;

void setup() {
  Serial.begin(115200);

  delay(1000);

  Serial.println("Hello World");

  count = 0;
}

void loop() {
  Serial.println(count, HEX);
  ++count;

  delay(500);
}
