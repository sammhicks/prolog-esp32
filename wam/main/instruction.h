#pragma once
#pragma pack(push, 1)

#include "value.h"

enum class Opcode : uint8_t {
  putVariableXnAi = 0x00,
  putVariableYnAi,
  putValueXnAi,
  putValueYnAi,
  putStructure,
  putList,
  putConstant,
  putInteger,
  getVariableXnAi = 0x10,
  getVariableYnAi,
  getValueXnAi,
  getValueYnAi,
  getStructure,
  getList,
  getConstant,
  getInteger,
  setVariableXn = 0x20,
  setVariableYn,
  setValueXn,
  setValueYn,
  setConstant,
  setInteger,
  unifyVariableXn = 0x30,
  unifyVariableYn,
  unifyValueXn,
  unifyValueYn,
  unifyConstant,
  unifyInteger,
  allocate = 0x40,
  deallocate,
  call,
  execute,
  proceed,
  tryMeElse = 0x48,
  retryMeElse,
  trustMe
};

typedef uint8_t Xn;
typedef uint8_t Yn;
typedef uint8_t Ai;
typedef uint8_t EnvironmentSize;
typedef uint16_t ProgramIndex;
typedef uint16_t Jump;

#pragma pack(pop)
