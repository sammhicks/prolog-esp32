#include "machine.h"

const char *codePath = "/code";
const char *labelTablePath = "/label-table";

ExecuteModes executeMode;
CodeIndex haltIndex;
bool querySucceeded;
bool exceptionRaised;
Stream *instructionSource;
File *programFile;

RWModes rwMode;

template <typename T> T read() { return Raw::read<T>(*instructionSource); }

void resetMachine() {
  Serial << "Reset" << endl;
  resetMemory();
  initScanning();
}

void performGarbageCollection() {
  switch (garbageCollectionState) {
  case GarbageCollectionStates::scan:
    if (scanStep()) {
      initSweeping();
    }
    break;
  case GarbageCollectionStates::sweep:
    if (sweepStep()) {
      initScanning();

      if (deadCount == 0) {
        Serial << "pausing garbage collection" << endl;
        garbageCollectionRunning = false;
      }
    }
    break;
  }
}

void executeInstructions(Client *client) {
  executeMode = ExecuteModes::query;
  querySucceeded = true;
  exceptionRaised = false;
  instructionSource = client;

  File actualProgramFile = SPIFFS.open(codePath);
  programFile = &actualProgramFile;
  haltIndex = actualProgramFile.size();

  while (executeMode == ExecuteModes::query) {
    executeInstruction();
  }

  if (!exceptionRaised) {
    Serial << "Executing Program" << endl;
    instructionSource = programFile;

    newEnvironment(argumentCount);

    for (EnvironmentSize i = 0; i < argumentCount; ++i) {
      Ancillary::lookupPermanentVariable(i) = registers[i];
    }
  }

  executeProgram(client);
}

void getNextAnswer(Client *client) {
  File actualProgramFile = SPIFFS.open(codePath);
  programFile = &actualProgramFile;

  Ancillary::backtrack();
  executeProgram(client);
}

void executeProgram(Client *client) {
  unsigned long excessTime = 0;

  while (querySucceeded && !exceptionRaised && programFile->available() > 0) {
#ifdef VERBOSE_LOG
    Serial << "Current Point: " << programFile->position() << endl;
#endif

    unsigned long targetTime = micros() + excessTime;

    do {
      executeInstruction();
    } while (micros() < targetTime);

    excessTime = micros() - targetTime;

    yieldProcessor();

    if (garbageCollectionRunning) {
      targetTime = micros() + excessTime;

      do {
        performGarbageCollection();
      } while (micros() < targetTime);

      excessTime = micros() - targetTime;

      yieldProcessor();
    } else {
      excessTime = 0;
    }
  }

  if (exceptionRaised) {
    Serial << "Exception" << endl;
    Raw::write(*client, Results::exception);

    return;
  }

  size_t registryEntryUsage = tupleRegistrySize;
  RegistryEntry *liveIter = nextFreeRegistryEntry;

  while (liveIter != nullptr) {
    --registryEntryUsage;

    liveIter = liveIter->next;
  }

  Serial << "Registry Usage: "
         << (registryEntryUsage * 100.0) / tupleRegistryCapacity << "%" << endl;

  Serial << "Tuple Heap Usage: "
         << ((nextFreeTuple - tuplesHeap) * 100.0) / tuplesHeapCapacity << "%"
         << endl;

  if (querySucceeded) {
    if (currentChoicePoint == nullptr) {
      Serial << "Done" << endl;
      Raw::write(*client, Results::success);
    } else {
      Serial << "Choice Points" << endl;
      Raw::write(*client, Results::choicePoints);
    }

    currentEnvironment->sendToClient(*client);

    return;
  }

  Serial << "Failure!" << endl;
  Raw::write(*client, Results::failure);
}

void executeInstruction() {
  Opcode opcode = Raw::read<Opcode>(*instructionSource);

  Serial << "Executing opcode " << opcode << ": ";

  switch (opcode) {
  case Opcode::putVariableXnAi:
    Serial << "putVariableXnAi" << endl;
    Instructions::putVariableXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::putVariableYnAi:
    Serial << "putVariableYnAi" << endl;
    Instructions::putVariableYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::putValueXnAi:
    Serial << "putValueXnAi" << endl;
    Instructions::putValueXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::putValueYnAi:
    Serial << "putValueYnAi" << endl;
    Instructions::putValueYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::putStructure:
    Serial << "putStructure" << endl;
    Instructions::putStructure(read<Functor>(), read<Arity>(), read<Ai>());
    break;
  case Opcode::putList:
    Serial << "putList" << endl;
    Instructions::putList(read<Ai>());
    break;
  case Opcode::putConstant:
    Serial << "putConstant" << endl;
    Instructions::putConstant(read<Constant>(), read<Ai>());
    break;
  case Opcode::putInteger:
    Serial << "putInteger" << endl;
    Instructions::putInteger(read<Integer>(), read<Ai>());
    break;
  case Opcode::putVoid:
    Serial << "putVoid" << endl;
    Instructions::putVoid(read<VoidCount>(), read<Ai>());
    break;
  case Opcode::getVariableXnAi:
    Serial << "getVariableXnAi" << endl;
    Instructions::getVariableXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::getVariableYnAi:
    Serial << "getVariableYnAi" << endl;
    Instructions::getVariableYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::getValueXnAi:
    Serial << "getValueXnAi" << endl;
    Instructions::getValueXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::getValueYnAi:
    Serial << "getValueYnAi" << endl;
    Instructions::getValueYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::getStructure:
    Serial << "getStructure" << endl;
    Instructions::getStructure(read<Functor>(), read<Arity>(), read<Ai>());
    break;
  case Opcode::getList:
    Serial << "getList" << endl;
    Instructions::getList(read<Ai>());
    break;
  case Opcode::getConstant:
    Serial << "getConstant" << endl;
    Instructions::getConstant(read<Constant>(), read<Ai>());
    break;
  case Opcode::getInteger:
    Serial << "getInteger" << endl;
    Instructions::getInteger(read<Integer>(), read<Ai>());
    break;
  case Opcode::setVariableXn:
    Serial << "setVariableXn" << endl;
    Instructions::setVariableXn(read<Xn>());
    break;
  case Opcode::setVariableYn:
    Serial << "setVariableYn" << endl;
    Instructions::setVariableYn(read<Yn>());
    break;
  case Opcode::setValueXn:
    Serial << "setValueXn" << endl;
    Instructions::setValueXn(read<Xn>());
    break;
  case Opcode::setValueYn:
    Serial << "setValueYn" << endl;
    Instructions::setValueYn(read<Yn>());
    break;
  case Opcode::setConstant:
    Serial << "setConstant" << endl;
    Instructions::setConstant(read<Constant>());
    break;
  case Opcode::setInteger:
    Serial << "setInteger" << endl;
    Instructions::setInteger(read<Integer>());
    break;
  case Opcode::setVoid:
    Serial << "setVoid" << endl;
    Instructions::setVoid(read<VoidCount>());
    break;
  case Opcode::unifyVariableXn:
    Serial << "unifyVariableXn" << endl;
    Instructions::unifyVariableXn(read<Xn>());
    break;
  case Opcode::unifyVariableYn:
    Serial << "unifyVariableYn" << endl;
    Instructions::unifyVariableYn(read<Yn>());
    break;
  case Opcode::unifyValueXn:
    Serial << "unifyValueXn" << endl;
    Instructions::unifyValueXn(read<Xn>());
    break;
  case Opcode::unifyValueYn:
    Serial << "unifyValueYn" << endl;
    Instructions::unifyValueYn(read<Yn>());
    break;
  case Opcode::unifyConstant:
    Serial << "unifyConstant" << endl;
    Instructions::unifyConstant(read<Constant>());
    break;
  case Opcode::unifyInteger:
    Serial << "unifyInteger" << endl;
    Instructions::unifyInteger(read<Integer>());
    break;
  case Opcode::unifyVoid:
    Serial << "unifyVoid" << endl;
    Instructions::unifyVoid(read<VoidCount>());
    break;
  case Opcode::allocate:
    Serial << "allocate" << endl;
    Instructions::allocate(read<EnvironmentSize>());
    break;
  case Opcode::trim:
    Serial << "trim" << endl;
    Instructions::trim(read<EnvironmentSize>());
    break;
  case Opcode::deallocate:
    Serial << "deallocate" << endl;
    Instructions::deallocate();
    break;
  case Opcode::call:
    Serial << "call" << endl;
    Instructions::call(read<LabelIndex>());
    break;
  case Opcode::execute:
    Serial << "execute" << endl;
    Instructions::execute(read<LabelIndex>());
    break;
  case Opcode::proceed:
    Serial << "proceed" << endl;
    Instructions::proceed();
    break;
  case Opcode::tryMeElse:
    Serial << "tryMeElse" << endl;
    Instructions::tryMeElse(read<LabelIndex>());
    break;
  case Opcode::retryMeElse:
    Serial << "retryMeElse" << endl;
    Instructions::retryMeElse(read<LabelIndex>());
    break;
  case Opcode::trustMe:
    Serial << "trustMe" << endl;
    Instructions::trustMe();
    break;
  case Opcode::neckCut:
    Serial << "neckCut" << endl;
    Instructions::neckCut();
    break;
  case Opcode::getLevel:
    Serial << "getLevel" << endl;
    Instructions::getLevel(read<Yn>());
    break;
  case Opcode::cut:
    Serial << "cut" << endl;
    Instructions::cut(read<Yn>());
    break;
  case Opcode::greaterThan:
    Serial << "greaterThan" << endl;
    Instructions::greaterThan();
    break;
  case Opcode::lessThan:
    Serial << "lessThan" << endl;
    Instructions::lessThan();
    break;
  case Opcode::lessThanOrEqualTo:
    Serial << "lessThanOrEqualTo" << endl;
    Instructions::lessThanOrEqualTo();
    break;
  case Opcode::greaterThanOrEqualTo:
    Serial << "greaterThanOrEqualTo" << endl;
    Instructions::greaterThanOrEqualTo();
    break;
  case Opcode::notEqual:
    Serial << "notEqual" << endl;
    Instructions::notEqual();
    break;
  case Opcode::equals:
    Serial << "equals" << endl;
    Instructions::equals();
    break;
  case Opcode::is:
    Serial << "is" << endl;
    Instructions::is();
    break;
  case Opcode::noOp:
    Serial << "noOp" << endl;
    Instructions::noOp();
    break;
  case Opcode::fail:
    Serial << "fail" << endl;
    Instructions::fail();
    break;
  case Opcode::unify:
    Serial << "unify" << endl;
    Instructions::unify();
    break;
  case Opcode::configureDigitalPin:
    Serial << "configureDigitalPin" << endl;
    Instructions::configureDigitalPin(read<DigitalPinModes>());
    break;
  case Opcode::digitalReadPin:
    Serial << "digitalReadPin" << endl;
    Instructions::digitalReadPin();
    break;
  case Opcode::digitalWritePin:
    Serial << "digitalWritePin" << endl;
    Instructions::digitalWritePin();
    break;
  case Opcode::pinIsAnalogInput:
    Serial << "pinIsAnalogInput" << endl;
    Instructions::pinIsAnalogInput();
    break;
  case Opcode::configureChannel:
    Serial << "configureChannel" << endl;
    Instructions::configureChannel();
    break;
  case Opcode::pinIsAnalogOutput:
    Serial << "pinIsAnalogOutput" << endl;
    Instructions::pinIsAnalogOutput();
    break;
  case Opcode::analogReadPin:
    Serial << "analogReadPin" << endl;
    Instructions::analogReadPin();
    break;
  case Opcode::analogWritePin:
    Serial << "analogWritePin" << endl;
    Instructions::analogWritePin();
    break;
  default:
    Serial << "Unknown opcode \"" << opcode << "\"" << endl;
    Ancillary::failWithException();
    break;
  }

  Serial << endl;
}

namespace Instructions {
using Ancillary::backtrack;
using Ancillary::bind;
using Ancillary::compare;
using Ancillary::evaluateExpression;
using Ancillary::evaluateStructure;
using Ancillary::failAndExit;
using Ancillary::failWithException;
using Ancillary::getChannel;
using Ancillary::getPin;
using Ancillary::lookupLabel;
using Ancillary::lookupPermanentVariable;
using Ancillary::resumeGarbageCollection;
using Ancillary::tidyTrail;
using Ancillary::trail;
using Ancillary::unify;
using Ancillary::virtualPredicate;
;

void putVariableXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Xn - " << xn << endl;
  Serial << "Ai - " << ai << endl;
#endif

  registers[xn] = newVariable();
  registers[ai] = registers[xn];
}

void putVariableYnAi(Yn yn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
  Serial << "Ai - " << ai << endl;
#endif

  lookupPermanentVariable(yn) = newVariable();
  registers[ai] = lookupPermanentVariable(yn);
}

void putValueXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Xn - " << xn << endl;
  Serial << "Ai - " << ai << endl;
#endif

  registers[ai] = registers[xn];
}

void putValueYnAi(Yn yn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
  Serial << "Ai - " << ai << endl;
#endif

  registers[ai] = lookupPermanentVariable(yn);
}

void putStructure(Functor f, Arity n, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "f  - " << f << endl;
  Serial << "n  - " << n << endl;
  Serial << "Ai - " << ai << endl;
#endif

  registers[ai] = newStructure(f, n);
}

void putList(Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Ai - " << ai << endl;
#endif

  registers[ai] = newList();
}

void putConstant(Constant c, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "c  - " << c << endl;
  Serial << "Ai - " << ai << endl;
#endif

  registers[ai] = newConstant(c);
}

void putInteger(Integer i, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "i  - " << i << endl;
  Serial << "Ai - " << ai << endl;
#endif

  registers[ai] = newInteger(i);
}

void putVoid(VoidCount n, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "n  - " << n << endl;
  Serial << "Ai - " << ai << endl;
#endif

  while (n > 0) {
    registers[ai] = newVariable();
    --n;
    ++ai;
  }
}

void getVariableXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Xn - " << xn << endl;
  Serial << "Ai - " << ai << endl;
#endif

  registers[xn] = registers[ai];
}

void getVariableYnAi(Yn yn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
  Serial << "Ai - " << ai << endl;
#endif

  lookupPermanentVariable(yn) = registers[ai];
}

void getValueXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Xn - " << xn << endl;
  Serial << "Ai - " << ai << endl;
#endif

  if (!unify(registers[xn], registers[ai])) {
    backtrack();
  }
}

void getValueYnAi(Yn yn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
  Serial << "Ai - " << ai << endl;
#endif

  if (!unify(lookupPermanentVariable(yn), registers[ai])) {
    backtrack();
  }
}

void getStructure(Functor f, Arity n, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "f  - " << f << endl;
  Serial << "n  - " << n << endl;
  Serial << "Ai - " << ai << endl;
#endif

  RegistryEntry *d = registers[ai]->deref();

  switch (d->type) {
  case RegistryEntry::Type::reference:
#ifdef VERBOSE_LOG
    Serial << "Writing structure" << endl;
#endif
    bind(d, newStructure(f, n));
    rwMode = RWModes::write;
    return;
  case RegistryEntry::Type::structure:
#ifdef VERBOSE_LOG
    Serial << "Reading structure" << endl;
#endif
    if (d->body<Structure>().functor == f && d->body<Structure>().arity == n) {
      currentStructure = d;
      currentStructureSubtermIndex = 0;
      rwMode = RWModes::read;
    } else {
      backtrack();
    }
    return;
  default:
    backtrack();
    return;
  }
}

void getList(Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "Ai - " << ai << endl;
#endif

  RegistryEntry *d = registers[ai]->deref();

  switch (d->type) {
  case RegistryEntry::Type::reference:
#ifdef VERBOSE_LOG
    Serial << "Writing list" << endl;
#endif
    bind(d, newList());
    rwMode = RWModes::write;
    return;
  case RegistryEntry::Type::list:
#ifdef VERBOSE_LOG
    Serial << "Reading list" << endl;
#endif
    currentStructure = d;
    currentStructureSubtermIndex = 0;
    rwMode = RWModes::read;
    return;
  default:
    backtrack();
    return;
  }
}

void getConstant(Constant c, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "c  - " << c << endl;
  Serial << "Ai - " << ai << endl;
#endif

  RegistryEntry *d = registers[ai]->deref();

  switch (d->type) {
  case RegistryEntry::Type::reference:
#ifdef VERBOSE_LOG
    Serial << "Writing constant" << endl;
#endif
    trail(d);
    d->bindToConstant(c);
    return;
  case RegistryEntry::Type::constant:
#ifdef VERBOSE_LOG
    Serial << "Reading constant" << endl;
#endif
    if (c != d->body<Constant>()) {
      backtrack();
    }
    return;
  default:
    backtrack();
    return;
  }
}

void getInteger(Integer i, Ai ai) {
#ifdef VERBOSE_LOG
  Serial << "i  - " << i << endl;
  Serial << "Ai - " << ai << endl;
#endif

  RegistryEntry *d = registers[ai]->deref();

  switch (d->type) {
  case RegistryEntry::Type::reference:
#ifdef VERBOSE_LOG
    Serial << "Writing integer" << endl;
#endif
    trail(d);
    d->bindToInteger(i);
    return;
  case RegistryEntry::Type::integer:
#ifdef VERBOSE_LOG
    Serial << "Reading integer" << endl;
#endif
    if (i != d->body<Integer>()) {
      backtrack();
    }
    return;
  default:
    backtrack();
    return;
  }
}

void setVariableXn(Xn xn) {
#ifdef VERBOSE_LOG
  Serial << "Xn - " << xn << endl;
#endif

  currentStructureSubterm() = registers[xn] = newVariable();
}

void setVariableYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
#endif

  currentStructureSubterm() = lookupPermanentVariable(yn) = newVariable();
}

void setValueXn(Xn xn) {
#ifdef VERBOSE_LOG
  Serial << "Xn - " << xn << endl;
#endif

  currentStructureSubterm() = registers[xn];
}

void setValueYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
#endif

  currentStructureSubterm() = lookupPermanentVariable(yn);
}

void setConstant(Constant c) {
#ifdef VERBOSE_LOG
  Serial << "c  - " << c << endl;
#endif

  currentStructureSubterm() = newConstant(c);
}

void setInteger(Integer i) {
#ifdef VERBOSE_LOG
  Serial << "i - " << i << endl;
#endif

  currentStructureSubterm() = newInteger(i);
}

void setVoid(VoidCount n) {
#ifdef VERBOSE_LOG
  Serial << "n  - " << n << endl;
#endif

  while (n > 0) {
    currentStructureSubterm() = newVariable();
    --n;
  }
}

void unifyVariableXn(Xn xn) {
#ifdef VERBOSE_LOG
  Serial << "Xn - " << xn << endl;
#endif

  switch (rwMode) {
  case RWModes::read:
    registers[xn] = currentStructureSubterm();
    break;
  case RWModes::write:
    currentStructureSubterm() = registers[xn] = newVariable();
    break;
  }
}

void unifyVariableYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
#endif

  switch (rwMode) {
  case RWModes::read:
    lookupPermanentVariable(yn) = currentStructureSubterm();
    break;
  case RWModes::write:
    currentStructureSubterm() = lookupPermanentVariable(yn) = newVariable();
    break;
  }
}

void unifyValueXn(Xn xn) {
#ifdef VERBOSE_LOG
  Serial << "Xn - " << xn << endl;
#endif

  switch (rwMode) {
  case RWModes::read:
    if (!unify(registers[xn], currentStructureSubterm())) {
      backtrack();
    };
    break;
  case RWModes::write:
    currentStructureSubterm() = registers[xn];
    break;
  }
}

void unifyValueYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
#endif

  switch (rwMode) {
  case RWModes::read:
    if (!unify(lookupPermanentVariable(yn), currentStructureSubterm())) {
      backtrack();
    };
    break;
  case RWModes::write:
    currentStructureSubterm() = lookupPermanentVariable(yn);
    break;
  }
}

void unifyConstant(Constant c) {
#ifdef VERBOSE_LOG
  Serial << "c  - " << c << endl;
#endif

  switch (rwMode) {
  case RWModes::read: {
    RegistryEntry *d = currentStructureSubterm()->deref();
    switch (d->type) {
    case RegistryEntry::Type::reference:
      trail(d);
      d->bindToConstant(c);
      break;
    case RegistryEntry::Type::constant:
      if (c != d->body<Constant>()) {
        backtrack();
      }
      break;
    default:
      backtrack();
      break;
    }
  } break;
  case RWModes::write:
    currentStructureSubterm() = newConstant(c);
    break;
  }
}

void unifyInteger(Integer i) {
#ifdef VERBOSE_LOG
  Serial << "i - " << i << endl;
#endif

  switch (rwMode) {
  case RWModes::read: {
    RegistryEntry *d = currentStructureSubterm()->deref();
    switch (d->type) {
    case RegistryEntry::Type::reference:
      trail(d);
      d->bindToInteger(i);
      break;
    case RegistryEntry::Type::integer:
      if (i != d->body<Integer>()) {
        backtrack();
      }
      break;
    default:
      backtrack();
      break;
    }
  } break;
  case RWModes::write:
    currentStructureSubterm() = newInteger(i);
    break;
  }
}

void unifyVoid(VoidCount n) {
#ifdef VERBOSE_LOG
  Serial << "n  - " << n << endl;
#endif

  switch (rwMode) {
  case RWModes::read:
    currentStructureSubtermIndex += n;
    break;
  case RWModes::write:
    while (n > 0) {
      currentStructureSubterm() = newVariable();
      --n;
    }
    break;
  }
}

void allocate(EnvironmentSize n) {
#ifdef VERBOSE_LOG
  Serial << "Current environment: " << *currentEnvironment;
  Serial << "New Environment:" << newEnvironment(n);
#else
  newEnvironment(n);
#endif
}

void trim(EnvironmentSize n) {
#ifdef VERBOSE_LOG
  Serial << "Trimming " << n << " items" << endl;
  Serial << "Environment size from "
         << currentEnvironment->body<Environment>().size;
#endif

  currentEnvironment->mutableBody<Environment>().size -= n;
  resumeGarbageCollection();

#ifdef VERBOSE_LOG
  Serial << " to " << currentEnvironment->body<Environment>().size;
#endif
}

void deallocate() {
#ifdef VERBOSE_LOG
  Serial << "Deallocating environment: " << *currentEnvironment;
#endif

  continuePoint = currentEnvironment->body<Environment>().continuePoint;
  currentEnvironment = currentEnvironment->body<Environment>().nextEnvironment;
  resumeGarbageCollection();

#ifdef VERBOSE_LOG
  Serial << "New environment: " << *currentEnvironment;
#endif
}

void call(LabelIndex p) {
#ifdef VERBOSE_LOG
  Serial << "Calling label " << p << endl;
#endif

  switch (executeMode) {
  case ExecuteModes::query:
    executeMode = ExecuteModes::program;
    continuePoint = haltIndex;
    break;
  case ExecuteModes::program:
    continuePoint = programFile->position();
    break;
  }
  LabelTableEntry entry = lookupLabel(p);

#ifdef VERBOSE_LOG
  Serial << "\tcp - " << continuePoint << endl;
  Serial << "\tb  - " << currentChoicePoint << endl;
  Serial << "\tp  - " << entry.entryPoint << endl;
  Serial << "\targument count - " << entry.arity << endl;
#endif

  programFile->seek(entry.entryPoint);
  argumentCount = entry.arity;

  for (Xn i = argumentCount; i < registerCount; ++i) {
    registers[i] = nullptr;
  }

  currentCutPoint = currentChoicePoint;
}

void execute(LabelIndex p) {
#ifdef VERBOSE_LOG
  Serial << "Executing label " << p << endl;
  Serial << "\tb - " << currentChoicePoint << endl;
#endif

  LabelTableEntry entry = lookupLabel(p);

#ifdef VERBOSE_LOG
  Serial << "\tp  - " << entry.entryPoint << endl;
  Serial << "\targument count - " << entry.arity << endl;
#endif

  programFile->seek(entry.entryPoint);
  argumentCount = entry.arity;

  for (Xn i = argumentCount; i < registerCount; ++i) {
    registers[i] = nullptr;
  }

  currentCutPoint = currentChoicePoint;
}

void proceed() {
#ifdef VERBOSE_LOG
  Serial << "Proceeding to " << continuePoint << endl;
#endif
  programFile->seek(continuePoint);
}

void tryMeElse(LabelIndex l) {
#ifdef VERBOSE_LOG
  Serial << "New Choice Point: " << newChoicePoint(l);
#else
  newChoicePoint(l);
#endif
}

void retryMeElse(LabelIndex l) {
#ifdef VERBOSE_LOG
  Serial << "Current Choice Point: " << *currentChoicePoint << endl;
#endif

  restoreChoicePoint(l);

#ifdef VERBOSE_LOG
  Serial << "New Choice Point: " << *currentChoicePoint << endl;
#endif
}

void trustMe() {
#ifdef VERBOSE_LOG
  Serial << "Current Choice Point: " << *currentChoicePoint << endl;
#endif

  restoreChoicePoint();

  currentChoicePoint = currentChoicePoint->body<ChoicePoint>().nextChoicePoint;

#ifdef VERBOSE_LOG
  if (currentChoicePoint == nullptr) {
    Serial << "No more choice points" << endl;
  } else {
    Serial << "New Choice Point: " << *currentChoicePoint << endl;
  }
#endif
}

void neckCut() {
  if (currentChoicePoint > currentCutPoint) {
#ifdef VERBOSE_LOG
    Serial << "Cutting " << currentChoicePoint << " to " << currentCutPoint
           << endl;
#endif

    currentChoicePoint = currentCutPoint;

#ifdef VERBOSE_LOG
    if (currentChoicePoint == nullptr) {
      Serial << "No more choice points" << endl;
    } else {
      Serial << "New Choice Point: " << *currentChoicePoint << endl;
    }
#endif

    tidyTrail();
  }
}

void getLevel(Yn yn) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;

  if (currentCutPoint == nullptr) {
    Serial << "No cut point" << endl;
  } else {
    Serial << "Level is " << *currentCutPoint << endl;
  }
#endif

  lookupPermanentVariable(yn) = currentCutPoint;
}

void cut(Yn yn) {
#ifdef VERBOSE_LOG
  Serial << "Yn - " << yn << endl;
#endif

  RegistryEntry *cutPoint = lookupPermanentVariable(yn);

  if (cutPoint != nullptr &&
      cutPoint->type != RegistryEntry::Type::choicePoint) {
    Serial << "Value is not a choice point" << endl;
    failWithException();
    return;
  }

  if (currentChoicePoint > cutPoint) {
#ifdef VERBOSE_LOG
    Serial << "Cutting " << (currentChoicePoint - tupleRegistry) << " to "
           << (cutPoint - tupleRegistry) << endl;
#endif

    currentChoicePoint = cutPoint;
    tidyTrail();
  }
}

void greaterThan() {
  if (compare(registers[0], registers[1]) != Comparison::greaterThan) {
    backtrack();
  }
}

void lessThan() {
  if (compare(registers[0], registers[1]) != Comparison::lessThan) {
    backtrack();
  }
}

void lessThanOrEqualTo() {
  if (compare(registers[0], registers[1]) == Comparison::greaterThan) {
    backtrack();
  }
}

void greaterThanOrEqualTo() {
  if (compare(registers[0], registers[1]) == Comparison::lessThan) {
    backtrack();
  }
}

void notEqual() {
  if (compare(registers[0], registers[1]) == Comparison::equals) {
    backtrack();
  }
}

void equals() {
  if (compare(registers[0], registers[1]) != Comparison::equals) {
    backtrack();
  }
}

void is() {
  if (!unify(registers[0], evaluateExpression(registers[1]))) {
    backtrack();
  };
}

void noOp() {}

void fail() { backtrack(); }

void unify() {
  if (!unify(registers[0], registers[1])) {
    backtrack();
  }
}

void configureDigitalPin(DigitalPinModes pm) {
  virtualPredicate(1);

  uint8_t pinID = getPin(registers[0]);

  if (exceptionRaised) {
    return;
  }

  switch (pm) {
  case DigitalPinModes::Input:
    pinMode(pinID, INPUT);
    break;
  case DigitalPinModes::Output:
    pinMode(pinID, OUTPUT);
    break;
  case DigitalPinModes::InputPullup:
    pinMode(pinID, INPUT_PULLUP);
    break;
  case DigitalPinModes::InputPulldown:
    pinMode(pinID, INPUT_PULLDOWN);
    break;
  default:
    Serial << "Invalid pinmode " << pm << endl;
    failWithException();
    return;
  }
}

void digitalReadPin() {
  virtualPredicate(2);

  uint8_t pinID = getPin(registers[0]);

  if (exceptionRaised) {
    return;
  }

  if (!unify(registers[1], static_cast<Integer>(digitalRead(pinID)))) {
    backtrack();
  };
}

void digitalWritePin() {
  virtualPredicate(2);

  uint8_t pinID = getPin(registers[0]);

  Integer pinValue = evaluateExpression(registers[1]);

  if (exceptionRaised) {
    return;
  }

  switch (pinValue) {
  case 0:
    digitalWrite(pinID, LOW);
    break;
  case 1:
    digitalWrite(pinID, HIGH);
    break;
  default:
    Serial << "Invalid digital pin value \"" << pinValue << "\"" << endl;
    failWithException();
    return;
  }
}

void pinIsAnalogInput() {
  virtualPredicate(1);

  uint8_t pinID = getPin(registers[0]);

  if (exceptionRaised) {
    return;
  }

  pinMode(pinID, ANALOG);
}

void configureChannel() {
  virtualPredicate(2);

  uint8_t channelID = getChannel(registers[0]);

  Integer frequencyInt = evaluateExpression(registers[1]);

  if (exceptionRaised) {
    return;
  }

  ledcSetup(channelID, static_cast<float>(frequencyInt), analogResolution);
}

void pinIsAnalogOutput() {
  virtualPredicate(2);

  uint8_t pinID = getPin(registers[0]);

  uint8_t channelID = getChannel(registers[1]);

  if (exceptionRaised) {
    return;
  }

  pinMode(pinID, ANALOG);

  ledcAttachPin(pinID, channelID);
}

void analogReadPin() {
  virtualPredicate(2);

  uint8_t pinID = getPin(registers[0]);

  if (exceptionRaised) {
    return;
  }

  if (!unify(registers[1], static_cast<Integer>(digitalRead(pinID)))) {
    backtrack();
  };
}

void analogWritePin() {
  virtualPredicate(2);

  uint8_t channelID = getChannel(registers[0]);

  Integer pinValue = evaluateExpression(registers[1]);

  if (exceptionRaised) {
    return;
  }

  if (pinValue < 0) {
    Serial << "Analog pin write values must be positive" << endl;
    return;
  }

  ledcWrite(channelID, static_cast<uint32_t>(pinValue));
}
} // namespace Instructions

namespace Ancillary {
LabelTableEntry lookupLabel(LabelIndex p) {
  File labelTableFile = SPIFFS.open(labelTablePath);

  LabelTableEntry entry;

  labelTableFile.seek(p * sizeof(entry));

  labelTableFile.read(reinterpret_cast<uint8_t *>(&entry), sizeof(entry));

  return entry;
}

RegistryEntry *&lookupPermanentVariable(Yn yn) {
  return currentEnvironment->mutableBody<Environment>().permanentVariables[yn];
}

void backtrack() {
  if (exceptionRaised) {
    return;
  }

  Serial << endl << " ---- Backtrack! ---- " << endl << endl;

#ifdef VERBOSE_LOG
  Serial << "Choice point: " << *currentChoicePoint << endl;
#endif

  if (currentChoicePoint == nullptr) {
    failAndExit();
    return;
  }

#ifdef VERBOSE_LOG
  if (currentCutPoint == nullptr) {
    Serial << "No cut point" << endl;
  } else {
    Serial << "Current cut point: " << *currentCutPoint << endl;
  }
#endif

  if (currentChoicePoint->type != RegistryEntry::Type::choicePoint) {
    Serial << "Not a choice point!" << endl;
    failWithException();
    return;
  }

  currentCutPoint = currentChoicePoint->body<ChoicePoint>().currentCutPoint;

#ifdef VERBOSE_LOG
  if (currentCutPoint == nullptr) {
    Serial << "No cut point" << endl;
  } else {
    Serial << "New cut point: " << *currentCutPoint << endl;
  }
#endif

  programFile->seek(
      lookupLabel(currentChoicePoint->body<ChoicePoint>().retryLabel)
          .entryPoint);

  resumeGarbageCollection();
}

void failAndExit() { querySucceeded = false; }

void failWithException() { exceptionRaised = true; }

void bind(RegistryEntry *a1, RegistryEntry *a2) {
  if (a1->type == RegistryEntry::Type::reference &&
      ((a2->type != RegistryEntry::Type::reference) ||
       (a2->body<RegistryEntry *>() < a1->body<RegistryEntry *>()))) {
    trail(a1);
    a1->bindTo(a2);
  } else {
    trail(a2);
    a2->bindTo(a1);
  }
}

void trail(RegistryEntry *a) {
  if (currentChoicePoint != nullptr && a < currentChoicePoint) {
#ifdef VERBOSE_LOG
    Serial << "Adding " << (a - tupleRegistry) << " to the trail" << endl;
#endif

    newTrailItem(a);
  } else {

#ifdef VERBOSE_LOG
    Serial << "Address " << (a - tupleRegistry) << " is not conditional"
           << endl;
#endif
  }
}

void tidyTrail() { tidyTrail(trailHead); }

void tidyTrail(RegistryEntry *&head) {
  if (head == nullptr || currentChoicePoint == nullptr ||
      head < currentChoicePoint) {
    return;
  }

  if (head->body<TrailItem>().item > currentChoicePoint) {
    head = head->mutableBody<TrailItem>().nextItem;
  }

  tidyTrail(head->mutableBody<TrailItem>().nextItem);
}

bool unify(RegistryEntry *a1, RegistryEntry *a2) {
#ifdef VERBOSE_LOG
  Serial << "Unify:" << endl;
  Serial << "a1: " << *a1 << endl;
  Serial << "a2: " << *a2 << endl;
#endif

  RegistryEntry *d1 = a1->deref();
  RegistryEntry *d2 = a2->deref();

#ifdef VERBOSE_LOG
  Serial << "d1: " << *d1 << endl;
  Serial << "d2: " << *d2 << endl;
#endif

  if (d1->type == RegistryEntry::Type::reference ||
      d2->type == RegistryEntry::Type::reference) {
    bind(d1, d2);
    return true;
  }

  if (d1->type != d2->type) {
    return false;
  }

  switch (d1->type) {
  case RegistryEntry::Type::constant:
    return d1->body<Constant>() == d2->body<Constant>();
  case RegistryEntry::Type::integer:
    return d1->body<Integer>() == d2->body<Integer>();
  case RegistryEntry::Type::list:
    return unify(d1->body<List>().subterms[0], d2->body<List>().subterms[0]) &&
           unify(d1->body<List>().subterms[1], d2->body<List>().subterms[1]);
  case RegistryEntry::Type::structure: {
    Structure &s1 = d1->mutableBody<Structure>();
    Structure &s2 = d2->mutableBody<Structure>();

    if ((s1.functor != s2.functor) || (s1.arity != s2.arity)) {
      return false;
    }

    for (Arity i = 1; i <= s1.arity; ++i) {
      if (!unify(s1.subterms[i], s2.subterms[i])) {
        return false;
      }
    }
    return true;
  }
  default:
    Serial << "Error during unify. Unknown type " << d1->type << endl;
    return false;
  }
}

bool unify(RegistryEntry *a1, Integer i2) {
  RegistryEntry *d1 = a1->deref();

  switch (d1->type) {
  case RegistryEntry::Type::reference:
    trail(d1);
    d1->bindToInteger(i2);
    return true;
  case RegistryEntry::Type::integer:
    return i2 == d1->body<Integer>();
  default:
    return false;
  }
}

Comparison compare(Integer i1, Integer i2) {
  if (i1 < i2) {
    return Comparison::lessThan;
  }
  if (i1 > i2) {
    return Comparison::greaterThan;
  }
  return Comparison::equals;
}

Comparison compare(const RegistryEntry *a1, const RegistryEntry *a2) {
  return compare(evaluateExpression(a1), evaluateExpression(a2));
}

Integer evaluateExpression(const RegistryEntry *a) {
  if (exceptionRaised) {
    return 0;
  }

#ifdef VERBOSE_LOG
  Serial << "Evaluating Expression: " << *a << endl;
#endif

  const RegistryEntry *d = a->deref();

#ifdef VERBOSE_LOG
  Serial << "Derefs to: " << *d << endl;
#endif

  switch (d->type) {
  case RegistryEntry::Type::integer:
    return d->body<Integer>();
  case RegistryEntry::Type::structure:
    return evaluateStructure(d->body<Structure>());
  default:
    Serial << "Exception: invalid expression type " << d->type << endl;
    failWithException();
    return 0;
  }
}

Integer evaluateStructure(const Structure &structure) {
  if (exceptionRaised) {
    return 0;
  }

  switch (static_cast<SpecialStructures>(structure.functor)) {
  case SpecialStructures::add:
    switch (structure.arity) {
    case 1:
      return evaluateExpression(structure.subterms[0]);
    case 2:
      return evaluateExpression(structure.subterms[0]) +
             evaluateExpression(structure.subterms[1]);
    default:
      break;
    }
  case SpecialStructures::subtract:
    switch (structure.arity) {
    case 1:
      return -evaluateExpression(structure.subterms[0]);
    case 2:
      return evaluateExpression(structure.subterms[0]) -
             evaluateExpression(structure.subterms[1]);
    default:
      break;
    }
  case SpecialStructures::multiply:
    if (structure.arity == 2) {
      return evaluateExpression(structure.subterms[0]) *
             evaluateExpression(structure.subterms[1]);
    }
  case SpecialStructures::divide:
    if (structure.arity == 2) {
      Integer i1 = evaluateExpression(structure.subterms[0]);
      Integer i2 = evaluateExpression(structure.subterms[1]);
      if (i2 == 0) {
        Serial << "Exception: divide by 0" << endl;
        failWithException();
        return 0;
      }
      return i1 / i2;
    }
    break;
  default:
    break;
  }
  Serial << "Exception: not an operator: " << structure.functor << "/"
         << structure.arity;
  failWithException();
  return 0;
}

uint8_t getPin(const RegistryEntry *a) {
  Integer i = evaluateExpression(a);

  if (exceptionRaised) {
    return 0;
  }

  if (i < 0 || i >= 256) {
    Serial << "Exception: pin out of range: " << i << endl;

    failWithException();
    return 0;
  }

  return static_cast<uint8_t>(i);
}

uint8_t getChannel(RegistryEntry *a) {
  Integer channelInt = evaluateExpression(a);

  if (exceptionRaised) {
    return 0;
  }

  if (channelInt < 0 || channelInt > 15) {
    Serial << "Invalid channel number \"" << channelInt << "\"";

    failWithException();
    return 0;
  }

  return static_cast<uint8_t>(channelInt);
}

void virtualPredicate(Arity n) {
  if (executeMode == ExecuteModes::query) {
    executeMode = ExecuteModes::program;
    continuePoint = haltIndex;
    programFile->seek(haltIndex);
    argumentCount = n;
  }
}

void resumeGarbageCollection() {
  if (!garbageCollectionRunning) {
    Serial << "resuming garbage collection";

    garbageCollectionRunning = true;
  }
}
} // namespace Ancillary
