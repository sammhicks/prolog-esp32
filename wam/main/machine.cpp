#include "machine.h"

const char *codePath = "/code";
const char *labelTablePath = "/label-table";

MachineStates machineState;
Stream *instructionSource;
File *programFile;

RWModes rwMode;

template <typename T> T read() { return Raw::read<T>(*instructionSource); }

void resetMachine() {
  Serial << "Reset" << endl;
  resetMemory();
  initScanning();
}

void executeInstructions(Client *client) {
  machineState = MachineStates::executingQuery;
  instructionSource = client;

  File actualProgramFile = SPIFFS.open(codePath);
  programFile = &actualProgramFile;

  while (machineState == MachineStates::executingQuery) {
    executeInstruction();
  }

  if (machineState != MachineStates::exception) {
    LOG(Serial << "Executing Program" << endl);
    instructionSource = programFile;

    Ancillary::nullCheck(newEnvironment(argumentCount));

    if (machineState != MachineStates::exception) {
      for (EnvironmentSize i = 0; i < argumentCount; ++i) {
        Ancillary::setPermanentVariable(i, registers[i]);
      }
    }
  }

  executeProgram(client);
}

void getNextAnswer(Client *client) {
  File actualProgramFile = SPIFFS.open(codePath);
  programFile = &actualProgramFile;

  machineState = MachineStates::executingProgram;

  Ancillary::backtrack();
  executeProgram(client);
}

void executeProgram(Client *client) {
  unsigned long nextYieldTime = millis() + yieldPeriod;

  unsigned long excessTime = 0;

  while (machineState == MachineStates::executingProgram) {
    LOG(Serial << "Current Point: " << programFile->position() << endl);

    unsigned long targetTime = micros() + excessTime;

    do {
      executeInstruction();
    } while (machineState == MachineStates::executingProgram &&
             micros() < targetTime);

    excessTime = micros() - targetTime;

    if (garbageCollectionRunning) {
      double registryEntryUsage = static_cast<double>(tupleRegistryUsageCount) /
                                  static_cast<double>(tupleRegistryCapacity);
      double tupleUsage = static_cast<double>(nextFreeTuple - tuplesHeap) /
                          static_cast<double>(tuplesHeapCapacity);
      double maxUsage = std::max(registryEntryUsage, tupleUsage);
      double garbageCollectionScaling = maxUsage / (1.0 - maxUsage);

      VERBOSE(Serial << "registry usage" << registryEntryUsage << endl);
      VERBOSE(Serial << "tuple usage" << tupleUsage << endl);
      VERBOSE(Serial << "garbage collection scaling: "
                     << garbageCollectionScaling << endl);

      targetTime = micros() + excessTime * garbageCollectionScaling;

      while (garbageCollectionRunning && (micros() < targetTime)) {
        garbageCollectionStep();
      }

      if (garbageCollectionRunning) {
        excessTime = micros() - targetTime;
      } else {
        excessTime = 0;
      }
    } else {
      excessTime = 0;
    }

    if (millis() > nextYieldTime) {
      yieldProcessor();

      nextYieldTime = millis() + yieldPeriod;
    }
  }

  if (machineState == MachineStates::exception) {
    Serial << "Exception" << endl;
    Raw::write(*client, Results::exception);

    return;
  }

  fullGarbageCollection();

  Serial << "Registry Usage: "
         << (tupleRegistryUsageCount * 100.0) / tupleRegistryCapacity << "%"
         << endl;

  Serial << "Tuple Heap Usage: "
         << ((nextFreeTuple - tuplesHeap) * 100.0) / tuplesHeapCapacity << "%"
         << endl;

  if (machineState == MachineStates::success) {
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

  LOG(Serial << "Executing opcode " << opcode << ": ");

  switch (opcode) {
  case Opcode::putVariableXnAi:
    LOG(Serial << "putVariableXnAi" << endl);
    Instructions::putVariableXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::putVariableYnAi:
    LOG(Serial << "putVariableYnAi" << endl);
    Instructions::putVariableYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::putValueXnAi:
    LOG(Serial << "putValueXnAi" << endl);
    Instructions::putValueXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::putValueYnAi:
    LOG(Serial << "putValueYnAi" << endl);
    Instructions::putValueYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::putStructure:
    LOG(Serial << "putStructure" << endl);
    Instructions::putStructure(read<Functor>(), read<Arity>(), read<Ai>());
    break;
  case Opcode::putList:
    LOG(Serial << "putList" << endl);
    Instructions::putList(read<Ai>());
    break;
  case Opcode::putConstant:
    LOG(Serial << "putConstant" << endl);
    Instructions::putConstant(read<Constant>(), read<Ai>());
    break;
  case Opcode::putInteger:
    LOG(Serial << "putInteger" << endl);
    Instructions::putInteger(read<Integer>(), read<Ai>());
    break;
  case Opcode::putVoid:
    LOG(Serial << "putVoid" << endl);
    Instructions::putVoid(read<VoidCount>(), read<Ai>());
    break;
  case Opcode::getVariableXnAi:
    LOG(Serial << "getVariableXnAi" << endl);
    Instructions::getVariableXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::getVariableYnAi:
    LOG(Serial << "getVariableYnAi" << endl);
    Instructions::getVariableYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::getValueXnAi:
    LOG(Serial << "getValueXnAi" << endl);
    Instructions::getValueXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::getValueYnAi:
    LOG(Serial << "getValueYnAi" << endl);
    Instructions::getValueYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::getStructure:
    LOG(Serial << "getStructure" << endl);
    Instructions::getStructure(read<Functor>(), read<Arity>(), read<Ai>());
    break;
  case Opcode::getList:
    LOG(Serial << "getList" << endl);
    Instructions::getList(read<Ai>());
    break;
  case Opcode::getConstant:
    LOG(Serial << "getConstant" << endl);
    Instructions::getConstant(read<Constant>(), read<Ai>());
    break;
  case Opcode::getInteger:
    LOG(Serial << "getInteger" << endl);
    Instructions::getInteger(read<Integer>(), read<Ai>());
    break;
  case Opcode::setVariableXn:
    LOG(Serial << "setVariableXn" << endl);
    Instructions::setVariableXn(read<Xn>());
    break;
  case Opcode::setVariableYn:
    LOG(Serial << "setVariableYn" << endl);
    Instructions::setVariableYn(read<Yn>());
    break;
  case Opcode::setValueXn:
    LOG(Serial << "setValueXn" << endl);
    Instructions::setValueXn(read<Xn>());
    break;
  case Opcode::setValueYn:
    LOG(Serial << "setValueYn" << endl);
    Instructions::setValueYn(read<Yn>());
    break;
  case Opcode::setConstant:
    LOG(Serial << "setConstant" << endl);
    Instructions::setConstant(read<Constant>());
    break;
  case Opcode::setInteger:
    LOG(Serial << "setInteger" << endl);
    Instructions::setInteger(read<Integer>());
    break;
  case Opcode::setVoid:
    LOG(Serial << "setVoid" << endl);
    Instructions::setVoid(read<VoidCount>());
    break;
  case Opcode::unifyVariableXn:
    LOG(Serial << "unifyVariableXn" << endl);
    Instructions::unifyVariableXn(read<Xn>());
    break;
  case Opcode::unifyVariableYn:
    LOG(Serial << "unifyVariableYn" << endl);
    Instructions::unifyVariableYn(read<Yn>());
    break;
  case Opcode::unifyValueXn:
    LOG(Serial << "unifyValueXn" << endl);
    Instructions::unifyValueXn(read<Xn>());
    break;
  case Opcode::unifyValueYn:
    LOG(Serial << "unifyValueYn" << endl);
    Instructions::unifyValueYn(read<Yn>());
    break;
  case Opcode::unifyConstant:
    LOG(Serial << "unifyConstant" << endl);
    Instructions::unifyConstant(read<Constant>());
    break;
  case Opcode::unifyInteger:
    LOG(Serial << "unifyInteger" << endl);
    Instructions::unifyInteger(read<Integer>());
    break;
  case Opcode::unifyVoid:
    LOG(Serial << "unifyVoid" << endl);
    Instructions::unifyVoid(read<VoidCount>());
    break;
  case Opcode::allocate:
    LOG(Serial << "allocate" << endl);
    Instructions::allocate(read<EnvironmentSize>());
    break;
  case Opcode::trim:
    LOG(Serial << "trim" << endl);
    Instructions::trim(read<EnvironmentSize>());
    break;
  case Opcode::deallocate:
    LOG(Serial << "deallocate" << endl);
    Instructions::deallocate();
    break;
  case Opcode::call:
    LOG(Serial << "call" << endl);
    Instructions::call(read<LabelIndex>());
    break;
  case Opcode::execute:
    LOG(Serial << "execute" << endl);
    Instructions::execute(read<LabelIndex>());
    break;
  case Opcode::proceed:
    LOG(Serial << "proceed" << endl);
    Instructions::proceed();
    break;
  case Opcode::tryMeElse:
    LOG(Serial << "tryMeElse" << endl);
    Instructions::tryMeElse(read<LabelIndex>());
    break;
  case Opcode::retryMeElse:
    LOG(Serial << "retryMeElse" << endl);
    Instructions::retryMeElse(read<LabelIndex>());
    break;
  case Opcode::trustMe:
    LOG(Serial << "trustMe" << endl);
    Instructions::trustMe();
    break;
  case Opcode::neckCut:
    LOG(Serial << "neckCut" << endl);
    Instructions::neckCut();
    break;
  case Opcode::getLevel:
    LOG(Serial << "getLevel" << endl);
    Instructions::getLevel(read<Yn>());
    break;
  case Opcode::cut:
    LOG(Serial << "cut" << endl);
    Instructions::cut(read<Yn>());
    break;
  case Opcode::greaterThan:
    LOG(Serial << "greaterThan" << endl);
    Instructions::greaterThan();
    break;
  case Opcode::lessThan:
    LOG(Serial << "lessThan" << endl);
    Instructions::lessThan();
    break;
  case Opcode::lessThanOrEqualTo:
    LOG(Serial << "lessThanOrEqualTo" << endl);
    Instructions::lessThanOrEqualTo();
    break;
  case Opcode::greaterThanOrEqualTo:
    LOG(Serial << "greaterThanOrEqualTo" << endl);
    Instructions::greaterThanOrEqualTo();
    break;
  case Opcode::notEqual:
    LOG(Serial << "notEqual" << endl);
    Instructions::notEqual();
    break;
  case Opcode::equals:
    LOG(Serial << "equals" << endl);
    Instructions::equals();
    break;
  case Opcode::is:
    LOG(Serial << "is" << endl);
    Instructions::is();
    break;
  case Opcode::noOp:
    LOG(Serial << "noOp" << endl);
    Instructions::noOp();
    break;
  case Opcode::fail:
    LOG(Serial << "fail" << endl);
    Instructions::fail();
    break;
  case Opcode::unify:
    LOG(Serial << "unify" << endl);
    Instructions::unify();
    break;
  case Opcode::configureDigitalPin:
    LOG(Serial << "configureDigitalPin" << endl);
    Instructions::configureDigitalPin(read<DigitalPinModes>());
    break;
  case Opcode::digitalReadPin:
    LOG(Serial << "digitalReadPin" << endl);
    Instructions::digitalReadPin();
    break;
  case Opcode::digitalWritePin:
    LOG(Serial << "digitalWritePin" << endl);
    Instructions::digitalWritePin();
    break;
  case Opcode::pinIsAnalogInput:
    LOG(Serial << "pinIsAnalogInput" << endl);
    Instructions::pinIsAnalogInput();
    break;
  case Opcode::configureChannel:
    LOG(Serial << "configureChannel" << endl);
    Instructions::configureChannel();
    break;
  case Opcode::pinIsAnalogOutput:
    LOG(Serial << "pinIsAnalogOutput" << endl);
    Instructions::pinIsAnalogOutput();
    break;
  case Opcode::analogReadPin:
    LOG(Serial << "analogReadPin" << endl);
    Instructions::analogReadPin();
    break;
  case Opcode::analogWritePin:
    LOG(Serial << "analogWritePin" << endl);
    Instructions::analogWritePin();
    break;
  case Opcode::lineSensor:
    LOG(Serial << "lineSensor" << endl);
    Instructions::lineSensor();
    break;
  case Opcode::millis:
    LOG(Serial << "millis" << endl);
    Instructions::millisInstruction();
    break;
  case Opcode::delay:
    LOG(Serial << "delay" << endl);
    Instructions::delayInstruction();
    break;
  default:
    Serial << "Unknown opcode \"" << opcode << "\"" << endl;
    Ancillary::failWithException();
    break;
  }

  LOG(Serial << endl);
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
using Ancillary::nullCheck;
using Ancillary::permanentVariable;
using Ancillary::resumeGarbageCollection;
using Ancillary::setInstructionCounter;
using Ancillary::setPermanentVariable;
using Ancillary::tidyTrail;
using Ancillary::trail;
using Ancillary::unify;
using Ancillary::virtualPredicate;
;

void putVariableXnAi(Xn xn, Ai ai) {
  VERBOSE(Serial << "Xn - " << xn << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  nullCheck(registers[ai] = registers[xn] = newVariable());
}

void putVariableYnAi(Yn yn, Ai ai) {
  VERBOSE(Serial << "Yn - " << yn << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  nullCheck(registers[ai] = setPermanentVariable(yn, newVariable()));
}

void putValueXnAi(Xn xn, Ai ai) {
  VERBOSE(Serial << "Xn - " << xn << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  registers[ai] = registers[xn];
}

void putValueYnAi(Yn yn, Ai ai) {
  VERBOSE(Serial << "Yn - " << yn << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  registers[ai] = permanentVariable(yn);
}

void putStructure(Functor f, Arity n, Ai ai) {
  VERBOSE(Serial << "f  - " << f << endl);
  VERBOSE(Serial << "n  - " << n << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  nullCheck(registers[ai] = newStructure(f, n));
}

void putList(Ai ai) {
  VERBOSE(Serial << "Ai - " << ai << endl);

  nullCheck(registers[ai] = newList());
}

void putConstant(Constant c, Ai ai) {
  VERBOSE(Serial << "c  - " << c << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  nullCheck(registers[ai] = newConstant(c));
}

void putInteger(Integer i, Ai ai) {
  VERBOSE(Serial << "i  - " << i << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  nullCheck(registers[ai] = newInteger(i));
}

void putVoid(VoidCount n, Ai ai) {
  VERBOSE(Serial << "n  - " << n << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  while ((machineState != MachineStates::exception) && n > 0) {
    nullCheck(registers[ai] = newVariable());
    --n;
    ++ai;
  }
}

void getVariableXnAi(Xn xn, Ai ai) {
  VERBOSE(Serial << "Xn - " << xn << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  registers[xn] = registers[ai];
}

void getVariableYnAi(Yn yn, Ai ai) {
  VERBOSE(Serial << "Yn - " << yn << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  setPermanentVariable(yn, registers[ai]);
}

void getValueXnAi(Xn xn, Ai ai) {
  VERBOSE(Serial << "Xn - " << xn << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  if (!unify(registers[xn], registers[ai])) {
    backtrack();
  }
}

void getValueYnAi(Yn yn, Ai ai) {
  VERBOSE(Serial << "Yn - " << yn << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  if (!unify(permanentVariable(yn), registers[ai])) {
    backtrack();
  }
}

void getStructure(Functor f, Arity n, Ai ai) {
  VERBOSE(Serial << "f  - " << f << endl);
  VERBOSE(Serial << "n  - " << n << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  RegistryEntry *d = registers[ai]->deref();

  switch (d->type) {
  case RegistryEntry::Type::reference:
    VERBOSE(Serial << "Writing structure" << endl);
    bind(d, nullCheck(newStructure(f, n)));
    rwMode = RWModes::write;
    return;
  case RegistryEntry::Type::structure:
    VERBOSE(Serial << "Reading structure" << endl);
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
  VERBOSE(Serial << "Ai - " << ai << endl);

  RegistryEntry *d = registers[ai]->deref();

  switch (d->type) {
  case RegistryEntry::Type::reference:
    VERBOSE(Serial << "Writing list" << endl);
    bind(d, nullCheck(newList()));
    rwMode = RWModes::write;
    return;
  case RegistryEntry::Type::list:
    VERBOSE(Serial << "Reading list" << endl);
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
  VERBOSE(Serial << "c  - " << c << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  RegistryEntry *d = registers[ai]->deref();

  switch (d->type) {
  case RegistryEntry::Type::reference:
    VERBOSE(Serial << "Writing constant" << endl);
    trail(d);
    d->bindToConstant(c);
    return;
  case RegistryEntry::Type::constant:
    VERBOSE(Serial << "Reading constant" << endl);
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
  VERBOSE(Serial << "i  - " << i << endl);
  VERBOSE(Serial << "Ai - " << ai << endl);

  RegistryEntry *d = registers[ai]->deref();

  switch (d->type) {
  case RegistryEntry::Type::reference:
    VERBOSE(Serial << "Writing integer" << endl);
    trail(d);
    d->bindToInteger(i);
    return;
  case RegistryEntry::Type::integer:
    VERBOSE(Serial << "Reading integer" << endl);
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
  VERBOSE(Serial << "Xn - " << xn << endl);

  nullCheck(setCurrentStructureSubterm(registers[xn] = newVariable()));
}

void setVariableYn(Yn yn) {
  VERBOSE(Serial << "Yn - " << yn << endl);

  nullCheck(
      setCurrentStructureSubterm(setPermanentVariable(yn, newVariable())));
}

void setValueXn(Xn xn) {
  VERBOSE(Serial << "Xn - " << xn << endl);

  setCurrentStructureSubterm(registers[xn]);
}

void setValueYn(Yn yn) {
  VERBOSE(Serial << "Yn - " << yn << endl);

  setCurrentStructureSubterm(permanentVariable(yn));
}

void setConstant(Constant c) {
  VERBOSE(Serial << "c  - " << c << endl);

  nullCheck(setCurrentStructureSubterm(newConstant(c)));
}

void setInteger(Integer i) {
  VERBOSE(Serial << "i - " << i << endl);

  nullCheck(setCurrentStructureSubterm(newInteger(i)));
}

void setVoid(VoidCount n) {
  VERBOSE(Serial << "n  - " << n << endl);

  while ((machineState != MachineStates::exception) && n > 0) {
    nullCheck(setCurrentStructureSubterm(newVariable()));
    --n;
  }
}

void unifyVariableXn(Xn xn) {
  VERBOSE(Serial << "Xn - " << xn << endl);

  switch (rwMode) {
  case RWModes::read:
    registers[xn] = currentStructureSubterm();
    break;
  case RWModes::write:
    nullCheck(setCurrentStructureSubterm(registers[xn] = newVariable()));
    break;
  }
}

void unifyVariableYn(Yn yn) {
  VERBOSE(Serial << "Yn - " << yn << endl);

  switch (rwMode) {
  case RWModes::read:
    setPermanentVariable(yn, currentStructureSubterm());
    break;
  case RWModes::write:
    nullCheck(
        setCurrentStructureSubterm(setPermanentVariable(yn, newVariable())));
    break;
  }
}

void unifyValueXn(Xn xn) {
  VERBOSE(Serial << "Xn - " << xn << endl);

  switch (rwMode) {
  case RWModes::read:
    if (!unify(registers[xn], currentStructureSubterm())) {
      backtrack();
    };
    break;
  case RWModes::write:
    setCurrentStructureSubterm(registers[xn]);
    break;
  }
}

void unifyValueYn(Yn yn) {
  VERBOSE(Serial << "Yn - " << yn << endl);

  switch (rwMode) {
  case RWModes::read:
    if (!unify(permanentVariable(yn), currentStructureSubterm())) {
      backtrack();
    };
    break;
  case RWModes::write:
    setCurrentStructureSubterm(permanentVariable(yn));
    break;
  }
}

void unifyConstant(Constant c) {
  VERBOSE(Serial << "c  - " << c << endl);

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
    nullCheck(setCurrentStructureSubterm(newConstant(c)));
    break;
  }
}

void unifyInteger(Integer i) {
  VERBOSE(Serial << "i - " << i << endl);

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
    nullCheck(setCurrentStructureSubterm(newInteger(i)));
    break;
  }
}

void unifyVoid(VoidCount n) {
  VERBOSE(Serial << "n  - " << n << endl);

  switch (rwMode) {
  case RWModes::read:
    currentStructureSubtermIndex += n;
    break;
  case RWModes::write:
    while ((machineState != MachineStates::exception) && n > 0) {
      nullCheck(setCurrentStructureSubterm(newVariable()));
      --n;
    }
    break;
  }
}

void allocate(EnvironmentSize n) {
  VERBOSE(Serial << "Current environment: " << *currentEnvironment);

  nullCheck(newEnvironment(n));

  VERBOSE(Serial << "New Environment:" << currentEnvironment);
}

void trim(EnvironmentSize n) {
  VERBOSE(Serial << "Trimming " << n << " items" << endl);
  VERBOSE(Serial << "Environment size from "
                 << currentEnvironment->body<Environment>().size);

  currentEnvironment->mutableBody<Environment>().size -= n;
  resumeGarbageCollection();

  VERBOSE(Serial << " to " << currentEnvironment->body<Environment>().size);
}

void deallocate() {
  VERBOSE(Serial << "Deallocating environment: " << *currentEnvironment);

  continuePoint = currentEnvironment->body<Environment>().continuePoint;
  currentEnvironment = currentEnvironment->body<Environment>().nextEnvironment;
  resumeGarbageCollection();

  VERBOSE(Serial << "New environment: " << *currentEnvironment);
}

void call(LabelIndex p) {
  VERBOSE(Serial << "Calling label " << p << endl);

  switch (machineState) {
  case MachineStates::executingQuery:
    machineState = MachineStates::executingProgram;
    continuePoint = haltIndex;
    break;
  case MachineStates::executingProgram:
    continuePoint = programFile->position();
    break;
  default:
    Serial << "Invalid call state" << endl;
    return;
  }

  LabelTableEntry entry = lookupLabel(p);

  VERBOSE(Serial << "\tcp - " << continuePoint << endl);
  VERBOSE(Serial << "\tb  - " << currentChoicePoint << endl);
  VERBOSE(Serial << "\tp  - " << entry.entryPoint << endl);
  VERBOSE(Serial << "\targument count - " << entry.arity << endl);

  setInstructionCounter(entry.entryPoint);
  argumentCount = entry.arity;

  for (Xn i = argumentCount; i < registerCount; ++i) {
    registers[i] = nullptr;
  }

  currentCutPoint = currentChoicePoint;
}

void execute(LabelIndex p) {
  VERBOSE(Serial << "Executing label " << p << endl);
  VERBOSE(Serial << "\tb - " << currentChoicePoint << endl);

  LabelTableEntry entry = lookupLabel(p);

  VERBOSE(Serial << "\tp  - " << entry.entryPoint << endl);
  VERBOSE(Serial << "\targument count - " << entry.arity << endl);

  setInstructionCounter(entry.entryPoint);
  argumentCount = entry.arity;

  for (Xn i = argumentCount; i < registerCount; ++i) {
    registers[i] = nullptr;
  }

  currentCutPoint = currentChoicePoint;
}

void proceed() {
  VERBOSE(Serial << "Proceeding to " << continuePoint << endl);
  setInstructionCounter(continuePoint);
}

void tryMeElse(LabelIndex l) {
  nullCheck(newChoicePoint(l));

  VERBOSE(Serial << "New Choice Point: " << currentChoicePoint);
}

void retryMeElse(LabelIndex l) {
  VERBOSE(Serial << "Current Choice Point: " << *currentChoicePoint << endl);

  restoreChoicePoint(l);

  VERBOSE(Serial << "New Choice Point: " << *currentChoicePoint << endl);
}

void trustMe() {
  VERBOSE(Serial << "Current Choice Point: " << *currentChoicePoint << endl);

  restoreChoicePoint();

  currentChoicePoint = currentChoicePoint->body<ChoicePoint>().nextChoicePoint;

  if (currentChoicePoint == nullptr) {
    VERBOSE(Serial << "No more choice points" << endl);
  } else {
    VERBOSE(Serial << "New Choice Point: " << *currentChoicePoint << endl);
  }
}

void neckCut() {
  if (currentChoicePoint == nullptr) {
    LOG(Serial << "No choice point to cut!" << endl);
    return;
  }

  if ((currentCutPoint == nullptr) ||
      (currentChoicePoint->tuple > currentCutPoint->tuple)) {
    VERBOSE(Serial << "Cutting " << currentChoicePoint << " to "
                   << currentCutPoint << endl);

    currentChoicePoint = currentCutPoint;

    if (currentChoicePoint == nullptr) {
      VERBOSE(Serial << "No more choice points" << endl);
    } else {
      VERBOSE(Serial << "New Choice Point: " << *currentChoicePoint << endl);
    }

    tidyTrail(trailHead);
  }
}

void getLevel(Yn yn) {
  VERBOSE(Serial << "Yn - " << yn << endl);

  if (currentCutPoint == nullptr) {
    VERBOSE(Serial << "No cut point" << endl);
  } else {
    VERBOSE(Serial << "Level is " << *currentCutPoint << endl);
  }

  setPermanentVariable(yn, currentCutPoint);
}

void cut(Yn yn) {
  VERBOSE(Serial << "Yn - " << yn << endl);

  RegistryEntry *cutPoint = permanentVariable(yn);

  if (cutPoint != nullptr &&
      cutPoint->type != RegistryEntry::Type::choicePoint) {
    Serial << "Value is not a choice point" << endl;
    failWithException();
    return;
  }

  if (currentChoicePoint == nullptr) {
    LOG(Serial << "No choice point to cut!" << endl);
    return;
  }

  if ((cutPoint == nullptr) || (currentChoicePoint->tuple > cutPoint->tuple)) {
    VERBOSE(Serial << "Cutting " << currentChoicePoint << " to " << cutPoint
                   << endl);

    currentChoicePoint = cutPoint;
    tidyTrail(trailHead);
  }
}

void greaterThan() {
  virtualPredicate(2);

  if (compare(registers[0], registers[1]) != Comparison::greaterThan) {
    backtrack();
  }
}

void lessThan() {
  virtualPredicate(2);

  if (compare(registers[0], registers[1]) != Comparison::lessThan) {
    backtrack();
  }
}

void lessThanOrEqualTo() {
  virtualPredicate(2);

  if (compare(registers[0], registers[1]) == Comparison::greaterThan) {
    backtrack();
  }
}

void greaterThanOrEqualTo() {
  virtualPredicate(2);

  if (compare(registers[0], registers[1]) == Comparison::lessThan) {
    backtrack();
  }
}

void notEqual() {
  virtualPredicate(2);

  if (compare(registers[0], registers[1]) == Comparison::equals) {
    backtrack();
  }
}

void equals() {
  virtualPredicate(2);

  if (compare(registers[0], registers[1]) != Comparison::equals) {
    backtrack();
  }
}

void is() {
  virtualPredicate(2);

  if (!unify(registers[0], evaluateExpression(registers[1]))) {
    backtrack();
  };
}

void noOp() {}

void fail() { backtrack(); }

void unify() {
  virtualPredicate(2);

  if (!unify(registers[0], registers[1])) {
    backtrack();
  }
}

void configureDigitalPin(DigitalPinModes pm) {
  virtualPredicate(1);

  uint8_t pinID = getPin(registers[0]);

  if (machineState == MachineStates::exception) {
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

  if (machineState == MachineStates::exception) {
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

  if (machineState == MachineStates::exception) {
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

  if (machineState == MachineStates::exception) {
    return;
  }

  pinMode(pinID, ANALOG);
}

void configureChannel() {
  virtualPredicate(2);

  uint8_t channelID = getChannel(registers[0]);

  Integer frequencyInt = evaluateExpression(registers[1]);

  LOG(Serial << "Channel " << channelID << " is at " << frequencyInt << "Hz"
             << endl);

  if (machineState == MachineStates::exception) {
    return;
  }

  ledcSetup(channelID, static_cast<double>(frequencyInt), analogResolution);
}

void pinIsAnalogOutput() {
  virtualPredicate(2);

  uint8_t pinID = getPin(registers[0]);

  uint8_t channelID = getChannel(registers[1]);

  if (machineState == MachineStates::exception) {
    return;
  }

  LOG(Serial << "Pin " << pinID << " is attached to channel " << channelID
             << "endl");

  pinMode(pinID, OUTPUT);

  ledcAttachPin(pinID, channelID);
}

void analogReadPin() {
  virtualPredicate(2);

  uint8_t pinID = getPin(registers[0]);

  if (machineState == MachineStates::exception) {
    return;
  }

  if (!unify(registers[1], static_cast<Integer>(analogRead(pinID)))) {
    backtrack();
  };
}

void analogWritePin() {
  virtualPredicate(2);

  uint8_t channelID = getChannel(registers[0]);

  Integer pinValue = evaluateExpression(registers[1]);

  if (machineState == MachineStates::exception) {
    return;
  }

  LOG(Serial << "Writing " << pinValue << " to channel " << channelID);

  if (pinValue < 0) {
    Serial << "Analog pin write values must be positive" << endl;
    return;
  }

  ledcWrite(channelID, static_cast<uint32_t>(pinValue));
  yieldProcessor();
}

const Ai lineSensorCount = 6;

void lineSensor() {
  virtualPredicate(2 * lineSensorCount);

  uint8_t pins[lineSensorCount];

  for (Ai i = 0; i < lineSensorCount; ++i) {
    pins[i] = getPin(registers[i]);

    if (machineState == MachineStates::exception) {
      return;
    }
  }

  bool triggered[lineSensorCount];

  LOG(Serial << "Reading line sensor pins:");

  for (Ai i = 0; i < lineSensorCount; ++i) {
    pinMode(pins[i], OUTPUT);
    digitalWrite(pins[i], HIGH);
    triggered[i] = false;

    LOG(Serial << "\t" << pins[i]);
  }

  LOG(Serial << endl);

  delay(10);

  Ai pinsRemaining = lineSensorCount;
  Integer timings[lineSensorCount];

  for (Ai i = 0; i < lineSensorCount; ++i) {
    pinMode(pins[i], INPUT);
  }

  unsigned long startTime = micros();

  while (pinsRemaining > 0) {
    unsigned long currentTime = micros() - startTime;
    for (Ai i = 0; i < lineSensorCount; ++i) {
      if (!triggered[i] && digitalRead(pins[i]) == LOW) {
        timings[i] = currentTime;
        triggered[i] = true;
        --pinsRemaining;
      }
    }
  }

  LOG(Serial << "Timings:                 ");
  for (Ai i = 0; i < lineSensorCount; ++i) {
    LOG(Serial << "\t" << timings[i]);
  }
  LOG(Serial << endl);

  for (Ai i = 0; i < lineSensorCount; ++i) {
    if (!unify(registers[i + lineSensorCount], timings[i])) {
      backtrack();
      return;
    }
  }
}

void millisInstruction() {
  virtualPredicate(1);

  Integer t = millis();

  LOG(Serial << "time: " << t << endl);

  if (!unify(registers[0], t)) {
    backtrack();
  }
}

void delayInstruction() {
  virtualPredicate(1);

  delay(evaluateExpression(registers[0]));
}

} // namespace Instructions

namespace Ancillary {
RegistryEntry *nullCheck(RegistryEntry *entry) {
  if (entry == nullptr) {
    failWithException();
  }

  return entry;
}

LabelTableEntry lookupLabel(LabelIndex p) {
  File labelTableFile = SPIFFS.open(labelTablePath);

  LabelTableEntry entry;

  labelTableFile.seek(p * sizeof(entry));

  labelTableFile.read(reinterpret_cast<uint8_t *>(&entry), sizeof(entry));

  return entry;
}

RegistryEntry *permanentVariable(Yn yn) {
  return currentEnvironment->mutableBody<Environment>().permanentVariables[yn];
}

RegistryEntry *setPermanentVariable(Yn yn, RegistryEntry *value) {
  currentEnvironment->mutableBody<Environment>().permanentVariables[yn] = value;
  return value;
}

void setInstructionCounter(CodeIndex p) {
  if (p == haltIndex) {
    machineState = MachineStates::success;
  } else {
    programFile->seek(p);
  }
}

void backtrack() {
  if (machineState == MachineStates::exception) {
    return;
  }

  LOG(Serial << endl << " ---- Backtrack! ---- " << endl << endl);

  if (currentChoicePoint == nullptr) {
    failAndExit();
    return;
  }

  VERBOSE(Serial << "Choice point: " << *currentChoicePoint << endl);

  if (currentCutPoint == nullptr) {
    VERBOSE(Serial << "No cut point" << endl);
  } else {
    VERBOSE(Serial << "Current cut point: " << *currentCutPoint << endl);
  }

  if (currentChoicePoint->type != RegistryEntry::Type::choicePoint) {
    Serial << "Not a choice point!" << endl;
    failWithException();
    return;
  }

  currentCutPoint = currentChoicePoint->body<ChoicePoint>().currentCutPoint;

  if (currentCutPoint == nullptr) {
    VERBOSE(Serial << "No cut point" << endl);
  } else {
    VERBOSE(Serial << "New cut point: " << *currentCutPoint << endl);
  }

  setInstructionCounter(
      lookupLabel(currentChoicePoint->body<ChoicePoint>().retryLabel)
          .entryPoint);

  resumeGarbageCollection();
}

void failAndExit() { machineState = MachineStates::failure; }

void failWithException() { machineState = MachineStates::exception; }

void bind(RegistryEntry *a1, RegistryEntry *a2) {
  if (machineState == MachineStates::exception) {
    return;
  }

  if (a1->type == RegistryEntry::Type::reference &&
      ((a2->type != RegistryEntry::Type::reference) ||
       (a2->body<RegistryEntry *>()->tuple <
        a1->body<RegistryEntry *>()->tuple))) {
    trail(a1);
    a1->bindTo(a2);
  } else {
    trail(a2);
    a2->bindTo(a1);
  }
}

void trail(RegistryEntry *a) {
  if (currentChoicePoint != nullptr && (a->tuple < currentChoicePoint->tuple)) {
    VERBOSE(Serial << "Adding " << a << " to the trail" << endl);

    nullCheck(newTrailItem(a));
  } else {
    VERBOSE(Serial << "Address " << a << " is not conditional" << endl);
  }
}

void tidyTrail(RegistryEntry *&head) {
  // If we're at the end of the trail
  if (head == nullptr) {
    return;
  }

  // If we're past the choice point
  if ((currentChoicePoint != nullptr) &&
      (head->tuple < currentChoicePoint->tuple)) {
    return;
  }

  VERBOSE(Serial << "Tidying " << *head << endl);

  if (currentChoicePoint == nullptr) {
    VERBOSE(Serial << "No choice point");
  } else {
    VERBOSE(Serial << "Current choice point: " << *currentChoicePoint);
  }

  if ((currentChoicePoint == nullptr) ||
      (head->body<TrailItem>().item->tuple > currentChoicePoint->tuple)) {
    head = head->mutableBody<TrailItem>().nextItem;
    tidyTrail(head);
  } else {
    tidyTrail(head->mutableBody<TrailItem>().nextItem);
  }
}

bool unify(RegistryEntry *a1, RegistryEntry *a2) {
  VERBOSE(Serial << "Unify:" << endl);
  VERBOSE(Serial << "a1: " << *a1 << endl);
  VERBOSE(Serial << "a2: " << *a2 << endl);

  RegistryEntry *d1 = a1->deref();
  RegistryEntry *d2 = a2->deref();

  VERBOSE(Serial << "d1: " << *d1 << endl);
  VERBOSE(Serial << "d2: " << *d2 << endl);

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
  if (machineState == MachineStates::exception) {
    return 0;
  }

  VERBOSE(Serial << "Evaluating Expression: " << *a << endl);

  const RegistryEntry *d = a->deref();

  VERBOSE(Serial << "Derefs to: " << *d << endl);

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
  if (machineState == MachineStates::exception) {
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

  if (machineState == MachineStates::exception) {
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

  if (machineState == MachineStates::exception) {
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
  if (machineState == MachineStates::executingQuery) {
    machineState = MachineStates::success;
    argumentCount = n;
  }
}

void resumeGarbageCollection() {
  if (!garbageCollectionRunning) {
    LOG(Serial << "resuming garbage collection" << endl);

    garbageCollectionRunning = true;
  }
}
} // namespace Ancillary
