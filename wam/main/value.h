#pragma once
#pragma pack(push, 1)

#include <cstddef>
#include <cstdint>

#include "HardwareSerial.h"

typedef uint32_t CodeIndex;
typedef uint16_t HeapIndex;
typedef uint16_t StackIndex;
typedef uint16_t TrailIndex;
typedef uint16_t Functor;
typedef uint8_t Arity;
typedef uint16_t Constant;
typedef int16_t Integer;
typedef uint8_t VoidCount;
typedef uint16_t Level;

struct Value {
  enum class Type : uint8_t {
    reference,
    structure,
    list,
    constant,
    integer,
    level
  };

  union {
    struct {
      Functor f;
      Arity n;
    };
    struct {
      Type type;

      union {
        HeapIndex h;
        Constant c;
        Integer i;
        Level level;
      };
    };
  };

  void makeReference(HeapIndex h);
  void makeStructure(HeapIndex h);
  void makeFunctor(Functor f, Arity n);
  void makeList(HeapIndex h);
  void makeConstant(Constant c);
  void makeInteger(Integer i);
  void makeLevel(Level level);

  void dump();
};

#pragma pack(pop)
