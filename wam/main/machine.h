#pragma once
#pragma pack(push, 1)

#include <limits>

#include "instruction.h"
#include "memory-allocation.h"
#include "memory-scanning.h"
#include "memory-sweeping.h"
#include "raw-io.h"
#include "verbose-log.h"

#include "SPIFFS.h"

extern const char *codePath;
extern const char *labelTablePath;

const unsigned long yieldPeriod = 1000;

const uint8_t analogResolution = 16;

enum class MachineStates : uint8_t {
  executingQuery,
  executingProgram,
  success,
  failure,
  exception,
};

enum class RWModes : uint8_t { read, write };

enum class Comparison : uint8_t { lessThan, equals, greaterThan };

enum class SpecialStructures : Functor { add, subtract, multiply, divide };

enum class Results : uint8_t { failure, success, choicePoints, exception };

struct LabelTableEntry {
  CodeIndex entryPoint;
  Arity arity;
};

extern MachineStates machineState;
const CodeIndex haltIndex = std::numeric_limits<CodeIndex>::max();
extern Stream *instructionSource;
extern File *programFile;

extern RWModes rwMode;

void resetMachine();

void performGarbageCollection();

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
void neckCut();
void getLevel(Yn yn);
void cut(Yn yn);

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
void lineSensor();
void millisInstruction();
void delayInstruction();
} // namespace Instructions

namespace Ancillary {
RegistryEntry *nullCheck(RegistryEntry *entry);
LabelTableEntry lookupLabel(LabelIndex l);
RegistryEntry *permanentVariable(Yn yn);
RegistryEntry *setPermanentVariable(Yn yn, RegistryEntry *value);
void setInstructionCounter(CodeIndex p);
void backtrack();
void failAndExit();
void failWithException();
void bind(RegistryEntry *a1, RegistryEntry *a2);
void trail(RegistryEntry *a);
void tidyTrail(RegistryEntry *&head);
bool unify(RegistryEntry *a1, RegistryEntry *a2);
bool unify(RegistryEntry *a1, Integer i2);
Comparison compare(Integer i1, Integer i2);
Comparison compare(const RegistryEntry *a1, const RegistryEntry *a2);
Integer evaluateExpression(const RegistryEntry *a);
Integer evaluateStructure(const Structure &structure);
uint8_t getPin(const RegistryEntry *a);
uint8_t getChannel(RegistryEntry *a);
void virtualPredicate(Arity n);
void resumeGarbageCollection();
} // namespace Ancillary*/

#pragma pack(pop)
