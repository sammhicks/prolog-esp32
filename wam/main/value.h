#pragma once
#pragma pack(push, 1)

#include <algorithm>
#include <cstddef>
#include <cstdint>

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

struct RegistryEntry;
struct Tuple;

extern const size_t ScalarSize;

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

  template <typename T> T &mutableBody() {
    return *reinterpret_cast<T *>(this + 1);
  }
  template <typename T> const T &body() const {
    return *reinterpret_cast<const T *>(this + 1);
  }

  RegistryEntry *deref();
  const RegistryEntry *deref() const;

  void resetToVariable();

  std::ostream &dump(std::ostream &os) const;

  void sendToClient(Client &client) const;
};

std::ostream &operator<<(std::ostream &os, const RegistryEntry &entry);

struct Tuple {
  RegistryEntry *entry;
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
  RegistryEntry *currentCutPoint;
  ChoicePointSize savedRegisterCount;
  RegistryEntry *savedRegisters[0];
};

struct TrailItem {
  RegistryEntry *item;
  RegistryEntry *nextItem;
};

#pragma pack(pop)
