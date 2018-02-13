#pragma once
#pragma pack(push, 1)

#include "Arduino.h"
#include "FS.h"

#include "instruction.h"
#include "raw-io.h"
#include "value.h"

const Xn registerCount = 32;
const size_t heapSize = 256;
const size_t stackSize = 1024;

const CodeIndex haltIndex = 3;

enum class ExecuteModes : uint8_t { query, program };

enum class RWModes : uint8_t { read, write };

struct LabelTableEntry {
  CodeIndex entryPoint;
  Arity arity;
};

struct Environment {
  Environment *ce;
  CodeIndex cp;
  Arity n;
  Value ys[0];
};

extern ExecuteModes executeMode;
extern Stream *instructionSource;
extern File programFile;
extern File labelTableFile;

extern RWModes rwMode;
extern Arity argumentCount;
extern HeapIndex h;
extern HeapIndex s;
extern CodeIndex cp;
extern Environment *e;

extern Value registers[registerCount];
extern Value heap[heapSize];
extern uint8_t stack[stackSize];

void resetMachine();

void executeInstruction();

namespace Instructions {
void putVariableXnAi(Xn xn, Ai ai);
void putVariableYnAi(Yn yn, Ai ai);
void putValueXnAi(Xn xn, Ai ai);
void putValueYnAi(Yn yn, Ai ai);
void putStructure(Functor f, Arity n, Ai ai);
void putList(Ai ai);
void putConstant(Constant c, Ai ai);
void putInteger(Integer i, Ai ai);

void getVariableXnAi(Xn xn, Ai ai);
void getVariableYnAi(Yn yn, Ai ai);
void getValueXnAi(Xn xn, Ai ai);
void getValueYnAi(Yn yn, Ai ai);
void getStructure(Functor f, Arity n, Ai ai);
void getList(Ai ai);
void getConstant(Constant c, Ai ai);
void getInteger(Integer i, Ai ai);

void setVariableXn(Xn xn);
void setVariableYn(Yn yn);
void setValueXn(Xn xn);
void setValueYn(Yn yn);
void setConstant(Constant c);
void setInteger(Integer i);

void unifyVariableXn(Xn xn);
void unifyVariableYn(Yn yn);
void unifyValueXn(Xn xn);
void unifyValueYn(Yn yn);
void unifyConstant(Constant c);
void unifyInteger(Integer i);

void allocate(EnvironmentSize n);
void deallocate();
void call(ProgramIndex p);
void proceed();
} // namespace Instructions

namespace Ancillary {
void backtrack();
void failAndExit();
Value &deref(Value &a);
Value &deref(HeapIndex h);
void bind(Value &a1, Value &a2);
void trail(const Value &a);
void unwindTrail(const Value &a1, const Value &a2);
void tidyTrail();
bool unify(Value &a1, Value &a2);
} // namespace Ancillary

#pragma pack(pop)
