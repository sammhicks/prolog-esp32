#pragma once

#include "serial-stream.h"

enum class Opcode : uint8_t {
  // Put
  putVariableXnAi = 0x00,
  putVariableYnAi = 0x01,
  putValueXnAi = 0x02,
  putValueYnAi = 0x03,
  putStructure = 0x04,
  putList = 0x05,
  putConstant = 0x06,
  putInteger = 0x07,
  putVoid = 0x08,
  // Get
  getVariableXnAi = 0x10,
  getVariableYnAi = 0x11,
  getValueXnAi = 0x12,
  getValueYnAi = 0x13,
  getStructure = 0x14,
  getList = 0x15,
  getConstant = 0x16,
  getInteger = 0x17,
  // Set
  setVariableXn = 0x20,
  setVariableYn = 0x21,
  setValueXn = 0x22,
  setValueYn = 0x23,
  setConstant = 0x26,
  setInteger = 0x27,
  setVoid = 0x28,
  // Unify
  unifyVariableXn = 0x30,
  unifyVariableYn = 0x31,
  unifyValueXn = 0x32,
  unifyValueYn = 0x33,
  unifyConstant = 0x36,
  unifyInteger = 0x37,
  unifyVoid = 0x38,
  // Control
  allocate = 0x40,
  trim = 0x41,
  deallocate = 0x42,
  call = 0x43,
  execute = 0x44,
  proceed = 0x45,
  // Choice
  tryMeElse = 0x50,
  retryMeElse = 0x51,
  trustMe = 0x52,
  neckCut = 0x53,
  getLevel = 0x54,
  cut = 0x55,
  // Arithmetic Operations
  greaterThan = 0x60,
  lessThan = 0x61,
  lessThanOrEqualTo = 0x62,
  greaterThanOrEqualTo = 0x63,
  notEqual = 0x64,
  equals = 0x65,
  is = 0x66,
  // Misc
  noOp = 0x70,
  fail = 0x71,
  unify = 0x72,
  configureDigitalPin = 0x80,
  digitalReadPin = 0x81,
  digitalWritePin = 0x82,
  pinIsAnalogInput = 0x84,
  configureChannel = 0x85,
  pinIsAnalogOutput = 0x86,
  analogReadPin = 0x87,
  analogWritePin = 0x88,
  lineSensor = 0x89,
  millis = 0x8A,
};

enum class DigitalPinModes : uint8_t {
  Input = 0x00,
  Output = 0x01,
  InputPullup = 0x02,
  InputPulldown = 0x03,
};

Print &operator<<(Print &os, const Opcode &c);
Print &operator<<(Print &os, const DigitalPinModes &pin);
