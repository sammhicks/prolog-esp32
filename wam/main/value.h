#pragma once
#pragma pack(push, 1)

#include <cstddef>
#include <cstdint>

typedef size_t CodeIndex;
typedef uint16_t HeapIndex;
typedef uint16_t StackIndex;
typedef uint16_t Functor;
typedef uint8_t Arity;
typedef uint16_t Constant;
typedef uint16_t Integer;

struct Value {
  enum class Type : uint8_t { reference, structure, list, constant, integer };

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
      };
    };
  };

  void makeReference(HeapIndex h);
  void makeStructure(HeapIndex h);
  void makeFunctor(Functor f, Arity n);
  void makeList();
  void makeConstant(Constant c);
  void makeInteger(Integer i);
};

#pragma pack(pop)
