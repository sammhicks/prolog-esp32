#pragma once

#include "Client.h"

#include <stdexcept>

#include "yield.h"

class TimeoutException : public std::exception {
public:
  TimeoutException();
  const char *what() const noexcept override;
};

namespace Raw {
template <typename T> T read(Stream &stream) {
  T result;
  if (stream.readBytes(reinterpret_cast<uint8_t *>(&result), sizeof(T)) !=
      sizeof(T)) {
    throw TimeoutException();
  }
  return result;
}

template <typename T> void write(Client &client, const T value) {
  client.write(reinterpret_cast<const uint8_t *>(&value), sizeof(T));
}

template <typename T> void writeBlock(Client &client, const T value) {
  client.write(sizeof(T));
  client.write(reinterpret_cast<const uint8_t *>(&value), sizeof(T));
}
} // namespace Raw
