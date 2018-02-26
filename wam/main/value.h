#pragma once
#pragma pack(push, 1)

#include <cstddef>
#include <cstdint>
#include <iostream>

#include "raw-io.h"
#include "verbose-log.h"

typedef uint8_t Vn;
typedef Vn Xn;
typedef Vn Yn;
typedef Vn Ai;

typedef uint32_t CodeIndex;
typedef uint16_t Functor;
typedef uint8_t Arity;
typedef uint16_t Constant;
typedef int16_t Integer;
typedef Vn VoidCount;
typedef Vn EnvironmentSize;
typedef Vn ChoicePointSize;
typedef uint16_t LabelIndex;

struct Tuple;

struct RegistryEntry {
  enum class Type : uint8_t {
    reference,
    structure,
    list,
    constant,
    integer,
    environment,
    choicePoint,
    trailItem
  };

  bool marked;
  RegistryEntry *next;

  Type type;
  Tuple *tuple;

  RegistryEntry *deref();
  const RegistryEntry *deref() const;

  std::ostream &operator<<(std::ostream &os) const;

  void sendToClient(Client &client) const;
};

struct Structure {
  Functor functor;
  Arity arity;
  RegistryEntry *subterms[0];
};

struct List {
  RegistryEntry *subterms[2];
};

struct Environment {
  RegistryEntry *nextEnvironment;
  CodeIndex continuePoint;
  EnvironmentSize size;
  EnvironmentSize capacity;
  RegistryEntry *permanentVariables[0];
};

struct ChoicePoint {
  RegistryEntry *currentEnvironment;
  CodeIndex continuePoint;
  RegistryEntry *nextChoicePoint;
  LabelIndex retryLabel;
  ChoicePointSize savedRegisterCount;
  RegistryEntry *savedRegisters[0];
};

struct TrailItem {
  RegistryEntry *item;
  RegistryEntry *nextItem;
};

struct Tuple {
  RegistryEntry *entry;

  union {
    RegistryEntry *reference;
    Structure structure;
    List list;
    Constant constant;
    Integer integer;
    Environment environment;
    ChoicePoint choicePoint;
    TrailItem trailItem;
  } body[0];

  struct Scalar {
    RegistryEntry *ref;
    Constant c;
    Integer i;
  };
};

#pragma pack(pop)
