#pragma once
#pragma pack(push, 1)

#include "Arduino.h"
#include "SPIFFS.h"

#include "instruction.h"
#include "raw-io.h"
#include "value.h"

#define VERBOSE_LOG

extern const char *codePath;
extern const char *labelTablePath;

const uint8_t analogResolution = 12;

const Xn registerCount = 32;
const HeapIndex heapSize = 512;
const size_t stackSize = 2048;
const TrailIndex trailSize = 128;

enum class ExecuteModes : uint8_t { query, program };

enum class RWModes : uint8_t { read, write };

enum class Comparison : uint8_t { lessThan, equals, greaterThan };

enum class SpecialStructures : Functor { add, subtract, multiply, divide };

enum class Results : uint8_t { failure, success, choicePoints, exception };

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

struct ChoicePoint {
  Environment *ce;
  CodeIndex cp;
  ChoicePoint *b;
  LabelIndex bp;
  TrailIndex tr;
  HeapIndex h;
  Arity n;
  Value args[0];
};

extern ExecuteModes executeMode;
extern bool querySucceeded;
extern bool exceptionRaised;
extern Stream *instructionSource;
extern File *programFile;

extern RWModes rwMode;
extern Arity argumentCount;
extern HeapIndex h;
extern HeapIndex s;
extern CodeIndex cp;
extern CodeIndex haltIndex;
extern TrailIndex tr;
extern Environment *e;
extern ChoicePoint *b;
extern HeapIndex hb;

extern Value registers[registerCount];
extern Value heap[heapSize];
extern uint8_t stack[stackSize];
extern HeapIndex trail[trailSize];

void resetMachine();

void executeInstructions(Client *client);

void getNextAnswer(Client *client);

void executeProgram(Client *client);

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
void putVoid(VoidCount n, Ai ai);

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
void setVoid(VoidCount n);

void unifyVariableXn(Xn xn);
void unifyVariableYn(Yn yn);
void unifyValueXn(Xn xn);
void unifyValueYn(Yn yn);
void unifyConstant(Constant c);
void unifyInteger(Integer i);
void unifyVoid(VoidCount n);

void allocate(EnvironmentSize n);
void trim(EnvironmentSize n);
void deallocate();
void call(LabelIndex p);
void execute(LabelIndex p);
void proceed();

void tryMeElse(LabelIndex l);
void retryMeElse(LabelIndex l);
void trustMe();

void greaterThan();
void lessThan();
void lessThanOrEqualTo();
void greaterThanOrEqualTo();
void notEqual();
void equals();
void is();
void noOp();
void fail();
void succeed();
void unify();

void configureDigitalPin(DigitalPinModes pm);
void digitalReadPin();
void digitalWritePin();
void pinIsAnalogInput();
void configureChannel();
void pinIsAnalogOutput();
void analogReadPin();
void analogWritePin();
} // namespace Instructions

namespace Ancillary {
LabelTableEntry lookupLabel(LabelIndex l);
void *topOfStack();
void backtrack();
void failAndExit();
void failWithException();
Value &deref(Value &a);
Value &deref(HeapIndex h);
void bind(Value &a1, Value &a2);
void addToTrail(HeapIndex a);
void unwindTrail(TrailIndex a1, TrailIndex a2);
// void tidyTrail();
bool unify(Value &a1, Value &a2);
Comparison compare(Integer i1, Integer i2);
Comparison compare(Value &a1, Value &a2);
Integer evaluateExpression(Value &a);
Integer evaluateStructure(Value &a);
uint8_t getPin(Value &a);
uint8_t getChannel(Value &a);
void virtualPredicate(Arity n);
} // namespace Ancillary

#pragma pack(pop)
