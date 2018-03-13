#pragma once
#pragma pack(push, 1)

#include <algorithm>
#include <cstddef>
#include <cstdint>

#include "raw-io.h"
#include "serial-stream.h"
#include "verbose-log.h"

typedef uint8_t Vn;
typedef Vn Xn;
typedef Vn Yn;
typedef Vn Ai;

typedef uint32_t CodeIndex;
typedef uint16_t Functor;
typedef uint8_t Arity;
typedef uint16_t Constant;
typedef int32_t Integer;
typedef Vn VoidCount;
typedef Vn EnvironmentSize;
typedef Vn ChoicePointSize;
typedef uint16_t LabelIndex;

enum class GarbageCollectionStates : uint8_t {
  scan,
  sweep,
};

struct RegistryEntry;
struct Tuple;
struct Structure;
struct List;
struct Environment;
struct ChoicePoint;
struct TrailItem;

#include "registers.h"

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
    trailItem,
  };

  bool marked;
  RegistryEntry *next;

  Type type;
  Tuple *tuple;

  template <typename T> T &mutableBody() {
    return *reinterpret_cast<T *>(tuple + 1);
  }
  template <typename T> const T &body() const {
    return *reinterpret_cast<const T *>(tuple + 1);
  }

  static size_t tupleSize(size_t headSize, Arity n);
  size_t tupleSize();

  RegistryEntry *deref();
  const RegistryEntry *deref() const;

  void resetToVariable();
  void bindTo(RegistryEntry *other);
  void bindToVariable(RegistryEntry *reference);
  void bindToConstant(Constant c);
  void bindToInteger(Integer i);

  Print &dump(Print &os) const;

  void sendToClient(Client &client) const;
  static void sendToClient(Client &client, const Structure &s);
  static void sendToClient(Client &client, const List &l);
  static void sendToClient(Client &client, const Environment &e);
};

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

Print &operator<<(Print &os, const RegistryEntry *entry);
Print &operator<<(Print &os, const RegistryEntry &entry);
Print &operator<<(Print &os, const RegistryEntry::Type &t);
Print &operator<<(Print &os, const Structure &s);
Print &operator<<(Print &os, const List &l);
Print &operator<<(Print &os, const Environment &e);
Print &operator<<(Print &os, const ChoicePoint &b);
Print &operator<<(Print &os, const TrailItem &ti);

#pragma pack(pop)
