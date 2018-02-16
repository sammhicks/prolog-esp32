#pragma once

#include "value.h"

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
  // Unify
  unifyVariableXn = 0x30,
  unifyVariableYn = 0x31,
  unifyValueXn = 0x32,
  unifyValueYn = 0x33,
  unifyConstant = 0x36,
  unifyInteger = 0x37,
  // Control
  allocate = 0x40,
  deallocate = 0x41,
  call = 0x42,
  execute = 0x43,
  proceed = 0x44,
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
  succeed = 0x72,
  unify = 0x73,
};

typedef uint8_t Xn;
typedef uint8_t Yn;
typedef uint8_t Ai;
typedef uint8_t EnvironmentSize;
typedef uint16_t LabelIndex;
