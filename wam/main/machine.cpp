#include "machine.h"

const char *codePath = "/code";
const char *labelTablePath = "/label-table";

ExecuteModes executeMode;
bool querySucceeded;
bool exceptionRaised;
Stream *instructionSource;
File *programFile;

RWModes rwMode;
Arity argumentCount;

template <typename T> T read() { return Raw::read<T>(*instructionSource); }

void resetMachine() {
  cout << "Reset" << endl;
  resetMemory();
}

void executeInstructions(Client *client) {
  executeMode = ExecuteModes::query;
  querySucceeded = true;
  exceptionRaised = false;
  instructionSource = client;

  File actualProgramFile = SPIFFS.open(codePath);
  programFile = &actualProgramFile;

  while (executeMode == ExecuteModes::query) {
    executeInstruction();
  }

  if (!exceptionRaised) {
    cout << "Executing Program" << endl;
    instructionSource = programFile;

    RegistryEntry **permanentVariables = newEnvironment(argumentCount);

    for (EnvironmentSize i = 0; i < argumentCount; ++i) {
      permanentVariables[i] = registers[i];
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
  while (querySucceeded && !exceptionRaised && programFile->available() > 0) {
    executeInstruction();
  }

  if (exceptionRaised) {
    cout << "Exception" << endl;
    Raw::write(*client, Results::exception);

    return;
  }

  if (querySucceeded) {
    if (currentChoicePoint == nullptr) {
      cout << "Done" << endl;
      Raw::write(*client, Results::success);
    } else {
      cout << "Choice Points" << endl;
      Raw::write(*client, Results::choicePoints);
    }

    currentEnvironment->sendToClient(*client);

    return;
  }

  cout << "Failure!" << endl;
  Raw::write(*client, Results::failure);
}

void executeInstruction() {
  Opcode opcode = Raw::read<Opcode>(*instructionSource);

  cout << "Executing opcode " << std::hex << static_cast<size_t>(opcode)
       << ": ";

  switch (opcode) {
  case Opcode::putVariableXnAi:
    cout << "putVariableXnAi" << endl;
    Instructions::putVariableXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::putVariableYnAi:
    cout << "putVariableYnAi" << endl;
    Instructions::putVariableYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::putValueXnAi:
    cout << "putValueXnAi" << endl;
    Instructions::putValueXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::putValueYnAi:
    cout << "putValueYnAi" << endl;
    Instructions::putValueYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::putStructure:
    cout << "putStructure" << endl;
    Instructions::putStructure(read<Functor>(), read<Arity>(), read<Ai>());
    break;
  case Opcode::putList:
    cout << "putList" << endl;
    Instructions::putList(read<Ai>());
    break;
  case Opcode::putConstant:
    cout << "putConstant" << endl;
    Instructions::putConstant(read<Constant>(), read<Ai>());
    break;
  case Opcode::putInteger:
    cout << "putInteger" << endl;
    Instructions::putInteger(read<Integer>(), read<Ai>());
    break;
  case Opcode::putVoid:
    cout << "putVoid" << endl;
    Instructions::putVoid(read<VoidCount>(), read<Ai>());
    break;
  case Opcode::getVariableXnAi:
    cout << "getVariableXnAi" << endl;
    Instructions::getVariableXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::getVariableYnAi:
    cout << "getVariableYnAi" << endl;
    Instructions::getVariableYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::getValueXnAi:
    cout << "getValueXnAi" << endl;
    Instructions::getValueXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::getValueYnAi:
    cout << "getValueYnAi" << endl;
    Instructions::getValueYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::getStructure:
    cout << "getStructure" << endl;
    Instructions::getStructure(read<Functor>(), read<Arity>(), read<Ai>());
    break;
  case Opcode::getList:
    cout << "getList" << endl;
    Instructions::getList(read<Ai>());
    break;
  case Opcode::getConstant:
    cout << "getConstant" << endl;
    Instructions::getConstant(read<Constant>(), read<Ai>());
    break;
  case Opcode::getInteger:
    cout << "getInteger" << endl;
    Instructions::getInteger(read<Integer>(), read<Ai>());
    break;
  case Opcode::setVariableXn:
    cout << "setVariableXn" << endl;
    Instructions::setVariableXn(read<Xn>());
    break;
  case Opcode::setVariableYn:
    cout << "setVariableYn" << endl;
    Instructions::setVariableYn(read<Yn>());
    break;
  case Opcode::setValueXn:
    cout << "setValueXn" << endl;
    Instructions::setValueXn(read<Xn>());
    break;
  case Opcode::setValueYn:
    cout << "setValueYn" << endl;
    Instructions::setValueYn(read<Yn>());
    break;
  case Opcode::setConstant:
    cout << "setConstant" << endl;
    Instructions::setConstant(read<Constant>());
    break;
  case Opcode::setInteger:
    cout << "setInteger" << endl;
    Instructions::setInteger(read<Integer>());
    break;
  case Opcode::setVoid:
    cout << "setVoid" << endl;
    Instructions::setVoid(read<VoidCount>());
    break;
  case Opcode::unifyVariableXn:
    cout << "unifyVariableXn" << endl;
    Instructions::unifyVariableXn(read<Xn>());
    break;
  case Opcode::unifyVariableYn:
    cout << "unifyVariableYn" << endl;
    Instructions::unifyVariableYn(read<Yn>());
    break;
  case Opcode::unifyValueXn:
    cout << "unifyValueXn" << endl;
    Instructions::unifyValueXn(read<Xn>());
    break;
  case Opcode::unifyValueYn:
    cout << "unifyValueYn" << endl;
    Instructions::unifyValueYn(read<Yn>());
    break;
  case Opcode::unifyConstant:
    cout << "unifyConstant" << endl;
    Instructions::unifyConstant(read<Constant>());
    break;
  case Opcode::unifyInteger:
    cout << "unifyInteger" << endl;
    Instructions::unifyInteger(read<Integer>());
    break;
  case Opcode::unifyVoid:
    cout << "unifyVoid" << endl;
    Instructions::unifyVoid(read<VoidCount>());
    break;
  case Opcode::allocate:
    cout << "allocate" << endl;
    Instructions::allocate(read<EnvironmentSize>());
    break;
  case Opcode::trim:
    cout << "trim" << endl;
    Instructions::trim(read<EnvironmentSize>());
    break;
  case Opcode::deallocate:
    cout << "deallocate" << endl;
    Instructions::deallocate();
    break;
  case Opcode::call:
    cout << "call" << endl;
    Instructions::call(read<LabelIndex>());
    break;
  case Opcode::execute:
    cout << "execute" << endl;
    Instructions::execute(read<LabelIndex>());
    break;
  case Opcode::proceed:
    cout << "proceed" << endl;
    Instructions::proceed();
    break;
  case Opcode::tryMeElse:
    cout << "tryMeElse" << endl;
    Instructions::tryMeElse(read<LabelIndex>());
    break;
  case Opcode::retryMeElse:
    cout << "retryMeElse" << endl;
    Instructions::retryMeElse(read<LabelIndex>());
    break;
  case Opcode::trustMe:
    cout << "trustMe" << endl;
    Instructions::trustMe();
    break;
  case Opcode::neckCut:
    cout << "neckCut" << endl;
    Instructions::neckCut();
    break;
  case Opcode::getLevel:
    cout << "getLevel" << endl;
    Instructions::getLevel(read<Yn>());
    break;
  case Opcode::cut:
    cout << "cut" << endl;
    Instructions::cut(read<Yn>());
    break;
  case Opcode::greaterThan:
    cout << "greaterThan" << endl;
    Instructions::greaterThan();
    break;
  case Opcode::lessThan:
    cout << "lessThan" << endl;
    Instructions::lessThan();
    break;
  case Opcode::lessThanOrEqualTo:
    cout << "lessThanOrEqualTo" << endl;
    Instructions::lessThanOrEqualTo();
    break;
  case Opcode::greaterThanOrEqualTo:
    cout << "greaterThanOrEqualTo" << endl;
    Instructions::greaterThanOrEqualTo();
    break;
  case Opcode::notEqual:
    cout << "notEqual" << endl;
    Instructions::notEqual();
    break;
  case Opcode::equals:
    cout << "equals" << endl;
    Instructions::equals();
    break;
  case Opcode::is:
    cout << "is" << endl;
    Instructions::is();
    break;
  case Opcode::noOp:
    cout << "noOp" << endl;
    Instructions::noOp();
    break;
  case Opcode::fail:
    cout << "fail" << endl;
    Instructions::fail();
    break;
  case Opcode::unify:
    cout << "unify" << endl;
    Instructions::unify();
    break;
  case Opcode::configureDigitalPin:
    cout << "configureDigitalPin" << endl;
    Instructions::configureDigitalPin(read<DigitalPinModes>());
    break;
  case Opcode::digitalReadPin:
    cout << "digitalReadPin" << endl;
    Instructions::digitalReadPin();
    break;
  case Opcode::digitalWritePin:
    cout << "digitalWritePin" << endl;
    Instructions::digitalWritePin();
    break;
  case Opcode::pinIsAnalogInput:
    cout << "pinIsAnalogInput" << endl;
    Instructions::pinIsAnalogInput();
    break;
  case Opcode::configureChannel:
    cout << "configureChannel" << endl;
    Instructions::configureChannel();
    break;
  case Opcode::pinIsAnalogOutput:
    cout << "pinIsAnalogOutput" << endl;
    Instructions::pinIsAnalogOutput();
    break;
  case Opcode::analogReadPin:
    cout << "analogReadPin" << endl;
    Instructions::analogReadPin();
    break;
  case Opcode::analogWritePin:
    cout << "analogWritePin" << endl;
    Instructions::analogWritePin();
    break;
  default:
    cout << "Unknown opcode \"" << std::hex << static_cast<size_t>(opcode)
         << "\"" << endl;
    Ancillary::failWithException();
    break;
  }

  cout << endl;
}

namespace Instructions {
using Ancillary::backtrack;
using Ancillary::bind;
using Ancillary::compare;
using Ancillary::evaluateExpression;
using Ancillary::failWithException;
using Ancillary::getChannel;
using Ancillary::getPin;
using Ancillary::lookupLabel;
using Ancillary::tidyTrail;
using Ancillary::unify;
using Ancillary::unwindTrail;
using Ancillary::virtualPredicate;

/*
void putVariableXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
  Serial.printf("Ai - %u\n", ai);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h].makeReference(h);
  registers[xn] = heap[h];
  registers[ai] = heap[h];
  ++h;
}

void putVariableYnAi(Yn yn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
  Serial.printf("Ai - %u\n", ai);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h].makeReference(h);
  e->ys[yn] = heap[h];
  registers[ai] = heap[h];
  ++h;
}

void putValueXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
  Serial.printf("Ai - %u\n", ai);
#endif

  registers[ai] = registers[xn];
}

void putValueYnAi(Yn yn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
  Serial.printf("Ai - %u\n", ai);
#endif

  registers[ai] = e->ys[yn];
}

void putStructure(Functor f, Arity n, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("f  - %u\n", f);
  Serial.printf("n  - %u\n", n);
  Serial.printf("Ai - %u\n", ai);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h].makeFunctor(f, n);
  registers[ai].makeStructure(h);
  ++h;
}

void putList(Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Ai - %u\n", ai);
  Serial.printf("h  - %x\n", h);
#endif

  registers[ai].makeList(h);
}

void putConstant(Constant c, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("c  - %u\n", c);
  Serial.printf("Ai - %u\n", ai);
#endif

  registers[ai].makeConstant(c);
}

void putInteger(Integer i, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("i  - %i\n", i);
  Serial.printf("Ai - %u\n", ai);
#endif

  registers[ai].makeInteger(i);
}

void putVoid(VoidCount n, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("n  - %u\n", n);
  Serial.printf("Ai - %u\n", ai);
#endif

  while (n > 0) {
    heap[h].makeReference(h);
    registers[ai] = heap[h];
    ++h;
    --n;
    ++ai;
  }
}

void getVariableXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
  Serial.printf("Ai - %u\n", ai);
#endif

  registers[xn] = registers[ai];
}

void getVariableYnAi(Yn yn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
  Serial.printf("Ai - %u\n", ai);
#endif

  e->ys[yn] = registers[ai];
}

void getValueXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
  Serial.printf("Ai - %u\n", ai);
#endif

  if (!unify(registers[xn], registers[ai])) {
    backtrack();
  }
}

void getValueYnAi(Yn yn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
  Serial.printf("Ai - %u\n", ai);
#endif

  if (!unify(e->ys[yn], registers[ai])) {
    backtrack();
  }
}

void getStructure(Functor f, Arity n, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("f  - %u\n", f);
  Serial.printf("n  - %u\n", n);
  Serial.printf("Ai - %u\n", ai);
#endif

  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
#ifdef VERBOSE_LOG
    cout << "Writing structure" << endl;
#endif
    heap[h].makeStructure(h + 1);
    heap[h + 1].makeFunctor(f, n);
    bind(derefAi, heap[h]);
    h = h + 2;
    rwMode = RWModes::write;
    return;
  case Value::Type::structure:
#ifdef VERBOSE_LOG
    cout << "Reading structure" << endl;
#endif
    if (heap[derefAi.h].f == f && heap[derefAi.h].n == n) {
      s = derefAi.h + 1;
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
  Serial.printf("Ai - %u\n", ai);
#endif

  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
#ifdef VERBOSE_LOG
    cout << "Writing list" << endl;
#endif
    heap[h].makeList(h + 1);
    bind(derefAi, heap[h]);
    ++h;
    rwMode = RWModes::write;
    return;
  case Value::Type::list:
#ifdef VERBOSE_LOG
    cout << "Reading list" << endl;
#endif
    s = derefAi.h;
    rwMode = RWModes::read;
    return;
  default:
    backtrack();
    return;
  }
}

void getConstant(Constant c, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("c  - %u\n", c);
  Serial.printf("Ai - %u\n", ai);
#endif

  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
#ifdef VERBOSE_LOG
    cout << "Writing constant" << endl;
#endif
    addToTrail(derefAi.h);
    derefAi.makeConstant(c);
    return;
  case Value::Type::constant:
#ifdef VERBOSE_LOG
    cout << "Reading constant" << endl;
#endif
    if (c != derefAi.c) {
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
  Serial.printf("i  - %i\n", i);
  Serial.printf("Ai - %u\n", ai);
#endif

  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
#ifdef VERBOSE_LOG
    cout << "Writing integer" << endl;
#endif
    addToTrail(derefAi.h);
    derefAi.makeInteger(i);
    return;
  case Value::Type::integer:
#ifdef VERBOSE_LOG
    cout << "Reading integer" << endl;
#endif
    if (i != derefAi.i) {
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
  Serial.printf("Xn - %u\n", xn);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h].makeReference(h);
  registers[xn] = heap[h];
  ++h;
}

void setVariableYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h].makeReference(h);
  e->ys[yn] = heap[h];
  ++h;
}

void setValueXn(Xn xn) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h] = registers[xn];
  ++h;
}

void setValueYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h] = e->ys[yn];
  ++h;
}

void setConstant(Constant c) {
#ifdef VERBOSE_LOG
  Serial.printf("c - %u\n", c);
  Serial.printf("h - %x\n", h);
#endif

  heap[h].makeConstant(c);
  ++h;
}

void setInteger(Integer i) {
#ifdef VERBOSE_LOG
  Serial.printf("i - %i\n", i);
  Serial.printf("h - %x\n", h);
#endif

  heap[h].makeInteger(i);
  ++h;
}

void setVoid(VoidCount n) {
#ifdef VERBOSE_LOG
  Serial.printf("n - %u\n", n);
#endif

  while (n > 0) {
    heap[h].makeReference(h);
    ++h;
    --n;
  }
}

void unifyVariableXn(Xn xn) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
#endif

  switch (rwMode) {
  case RWModes::read:
    registers[xn] = heap[s];
    break;
  case RWModes::write:
#ifdef VERBOSE_LOG
    Serial.printf("h - %x\n", h);
#endif

    heap[h].makeReference(h);
    registers[xn] = heap[h];
    ++h;
    break;
  }
  s = s + 1;
}

void unifyVariableYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
#endif

  switch (rwMode) {
  case RWModes::read:
    e->ys[yn] = heap[s];
    break;
  case RWModes::write:
#ifdef VERBOSE_LOG
    Serial.printf("h - %x\n", h);
#endif

    heap[h].makeReference(h);
    e->ys[yn] = heap[h];
    ++h;
    break;
  }
  s = s + 1;
}

void unifyValueXn(Xn xn) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
#endif

  switch (rwMode) {
  case RWModes::read:
    if (!unify(registers[xn], heap[s])) {
      backtrack();
    };
    break;
  case RWModes::write:
#ifdef VERBOSE_LOG
    Serial.printf("h - %x\n", h);
#endif

    heap[h] = registers[xn];
    ++h;
    break;
  }
  s = s + 1;
}

void unifyValueYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
#endif

  switch (rwMode) {
  case RWModes::read:
    if (!unify(e->ys[yn], heap[s])) {
      backtrack();
    };
    break;
  case RWModes::write:
#ifdef VERBOSE_LOG
    Serial.printf("h - %x\n", h);
#endif

    heap[h] = e->ys[yn];
    ++h;
    break;
  }
  s = s + 1;
}

void unifyConstant(Constant c) {
#ifdef VERBOSE_LOG
  Serial.printf("c - %u\n", c);
#endif

  switch (rwMode) {
  case RWModes::read: {
    Value &derefS = deref(heap[s]);
    switch (derefS.type) {
    case Value::Type::reference:
      addToTrail(derefS.h);
      derefS.makeConstant(c);
      break;
    case Value::Type::constant:
      if (c != derefS.c) {
        backtrack();
      }
      break;
    default:
      backtrack();
      break;
    }
  } break;
  case RWModes::write:
#ifdef VERBOSE_LOG
    Serial.printf("h - %x\n", h);
#endif

    heap[h].makeConstant(c);
    ++h;
    break;
  }
  s = s + 1;
}

void unifyInteger(Integer i) {
#ifdef VERBOSE_LOG
  Serial.printf("i - %i\n", i);
#endif

  switch (rwMode) {
  case RWModes::read: {
    Value &derefS = deref(heap[s]);
    switch (derefS.type) {
    case Value::Type::reference:
      addToTrail(derefS.h);
      derefS.makeInteger(i);
      break;
    case Value::Type::integer:
      if (i != derefS.i) {
        backtrack();
      }
      break;
    default:
      backtrack();
      break;
    }
  } break;
  case RWModes::write:
#ifdef VERBOSE_LOG
    Serial.printf("h - %x\n", h);
#endif

    heap[h].makeInteger(i);
    ++h;
    break;
  }
  s = s + 1;
}

void unifyVoid(VoidCount n) {
#ifdef VERBOSE_LOG
  Serial.printf("n - %u\n", n);
#endif

  switch (rwMode) {
  case RWModes::read:
    s = s + n;
    break;
  case RWModes::write:
    while (n > 0) {
      heap[h].makeReference(h);
      ++h;
      --n;
    }
    break;
  }
}

void allocate(EnvironmentSize n) {
  Environment *newEnvironment = reinterpret_cast<Environment *>(topOfStack());

#ifdef VERBOSE_LOG
  Serial.printf("Allocating a new environment at %x:\n",
                reinterpret_cast<uint8_t *>(newEnvironment) - stack);
  Serial.printf("\tce - %x\n", reinterpret_cast<uint8_t *>(e) - stack);
  Serial.printf("\tcp - %u\n", cp);
  Serial.printf("\tn - %u\n", n);
  Serial.printf("\ttop of stack - %x\n",
                reinterpret_cast<uint8_t *>(newEnvironment->ys + n) - stack);
#endif

  newEnvironment->ce = e;
  newEnvironment->cp = cp;
  newEnvironment->n = n;

  e = newEnvironment;
}

void trim(EnvironmentSize n) {
#ifdef VERBOSE_LOG
  Serial.printf("Trimming %u items\n", n);
  Serial.printf("Environment size from %u\n", e->n);
#endif

  e->n -= n;

#ifdef VERBOSE_LOG
  Serial.printf(" to %u\n", e->n);
#endif
}

void deallocate() {
#ifdef VERBOSE_LOG
  Serial.printf("Deallocating environment %x to %x:\n",
                reinterpret_cast<uint8_t *>(e) - stack,
                reinterpret_cast<uint8_t *>(e->ce) - stack);
  Serial.printf("\tcp - %u\n", e->cp);
#endif
  cp = e->cp;
  e = e->ce;
}

void call(LabelIndex p) {
#ifdef VERBOSE_LOG
  Serial.printf("Calling label %u:\n", p);
#endif
  switch (executeMode) {
  case ExecuteModes::query:
    executeMode = ExecuteModes::program;
    cp = haltIndex;
    break;
  case ExecuteModes::program:
    cp = programFile->position();
    break;
  }
  LabelTableEntry entry = lookupLabel(p);

#ifdef VERBOSE_LOG
  Serial.printf("\tcp - %u\n", cp);
  Serial.printf("\tb - %u\n", reinterpret_cast<uint8_t *>(b) - stack);
  Serial.printf("\tp - %u\n", entry.entryPoint);
  Serial.printf("\targument count - %u\n", entry.arity);
#endif

  programFile->seek(entry.entryPoint);
  argumentCount = entry.arity;
  b0 = b;
}

void execute(LabelIndex p) {
#ifdef VERBOSE_LOG
  Serial.printf("Executing label %u:\n", p);
  Serial.printf("\tb - %u\n", reinterpret_cast<uint8_t *>(b) - stack);
#endif

  LabelTableEntry entry = lookupLabel(p);

#ifdef VERBOSE_LOG
  Serial.printf("\tp - %u\n", entry.entryPoint);
  Serial.printf("\targument count - %u\n", entry.arity);
#endif

  programFile->seek(entry.entryPoint);
  argumentCount = entry.arity;
  b0 = b;
}

void proceed() {
#ifdef VERBOSE_LOG
  Serial.printf("Proceeding to %u\n", cp);
#endif
  programFile->seek(cp);
}

void tryMeElse(LabelIndex l) {
  ChoicePoint *newChoicePoint = reinterpret_cast<ChoicePoint *>(topOfStack());

#ifdef VERBOSE_LOG
  Serial.printf("Allocating a new choice point at %x:\n",
                reinterpret_cast<uint8_t *>(newChoicePoint) - stack);
  Serial.printf("\tce - %x\n", reinterpret_cast<uint8_t *>(e) - stack);
  Serial.printf("\tcp - %u\n", cp);
  Serial.printf("\tb  - %x\n", reinterpret_cast<uint8_t *>(b) - stack);
  Serial.printf("\tbp - %u\n", l);
  Serial.printf("\ttr - %u\n", tr);
  Serial.printf("\th  - %u\n", h);
  Serial.printf("\tb0 - %u\n", reinterpret_cast<uint8_t *>(b0) - stack);
  Serial.printf("\tn  - %u\n", argumentCount);
  Serial.printf(
      "\ttop of stack - %x\n",
      reinterpret_cast<uint8_t *>(newChoicePoint->args + argumentCount) -
          stack);
#endif

  newChoicePoint->ce = e;
  newChoicePoint->cp = cp;
  newChoicePoint->b = b;
  newChoicePoint->bp = l;
  newChoicePoint->tr = tr;
  newChoicePoint->h = h;
  newChoicePoint->b0 = b0;
  newChoicePoint->n = argumentCount;
  for (Arity n = 0; n < argumentCount; ++n) {
    newChoicePoint->args[n] = registers[n];
  }

  b = newChoicePoint;
  hb = h;
}

void retryMeElse(LabelIndex l) {
#ifdef VERBOSE_LOG
  Serial.printf("\te - %x\n", reinterpret_cast<uint8_t *>(b->ce) - stack);
  Serial.printf("\tce - %u\n", b->cp);
  Serial.printf("\tbp - %u\n", l);
  Serial.printf("\tn - %u\n", b->n);
#endif

  for (Arity n = 0; n < b->n; ++n) {
    registers[n] = b->args[n];
  }

  e = b->ce;
  cp = b->cp;
  b->bp = l;
  unwindTrail(b->tr, tr);
  tr = b->tr;
#ifdef VERBOSE_LOG
  Serial.printf("\t h - %u -> %u\n", h, b->h);
#endif
  h = b->h;
  hb = h;
}

void trustMe() {
#ifdef VERBOSE_LOG
  Serial.printf("\te - %x\n", reinterpret_cast<uint8_t *>(b->ce) - stack);
  Serial.printf("\tcp - %u\n", b->cp);
  Serial.printf("\tn - %u\n", b->n);
#endif

  for (Arity n = 0; n < b->n; ++n) {
    registers[n] = b->args[n];
  }

  e = b->ce;
  cp = b->cp;
  unwindTrail(b->tr, tr);
  tr = b->tr;

#ifdef VERBOSE_LOG
  Serial.printf("\t h - %u -> %u\n", h, b->h);
#endif
  h = b->h;

#ifdef VERBOSE_LOG
  Serial.printf("Moving from choice point %x to %x\n",
                reinterpret_cast<uint8_t *>(b) - stack,
                reinterpret_cast<uint8_t *>(b->b) - stack);
#endif

  b = b->b;
  hb = h;
}

void neckCut() {
  if (b > b0) {
#ifdef VERBOSE_LOG
    Serial.printf("Cutting %u to %u\n", reinterpret_cast<uint8_t *>(b) - stack,
                  reinterpret_cast<uint8_t *>(b0) - stack);
#endif

    b = b0;
    tidyTrail();
  }
}

void getLevel(Yn yn) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
#endif

  Level level = reinterpret_cast<uint8_t *>(b0) - stack;

#ifdef VERBOSE_LOG
  Serial.printf("Level is %u\n", level);
#endif

  e->ys[yn].makeLevel(level);
}

void cut(Yn yn) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
#endif

  if (e->ys[yn].type != Value::Type::level) {
    cout << "Value is not a level" << endl;
    failWithException();
    return;
  }

  ChoicePoint *newChoicePoint =
      reinterpret_cast<ChoicePoint *>(stack + e->ys[yn].level);

  if (b > newChoicePoint) {
    b = newChoicePoint;
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
  Value v;
  v.makeInteger(evaluateExpression(registers[1]));
  if (!unify(registers[0], v)) {
    backtrack();
  }
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
    Serial.printf("Invalid pinmode %u\n", static_cast<uint8_t>(pm));
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

  Value pinValue;

  pinValue.makeInteger(static_cast<Integer>(digitalRead(pinID)));

  if (!unify(registers[1], pinValue)) {
    backtrack();
  }
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
    Serial.printf("Invalid digital pin value \"%i\"\n",
                  static_cast<int>(pinValue));

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

  Value pinValue;

  pinValue.makeInteger(static_cast<Integer>(analogRead(pinID)));

  if (!unify(registers[1], pinValue)) {
    backtrack();
  }
}

void analogWritePin() {
  virtualPredicate(2);

  uint8_t channelID = getChannel(registers[0]);

  Integer pinValue = evaluateExpression(registers[1]);

  if (exceptionRaised) {
    return;
  }

  if (pinValue < 0) {
    cout << "Analog pin write values must be positive" << endl;
  }

  ledcWrite(channelID, static_cast<uint32_t>(pinValue));
}
// */
} // namespace Instructions

namespace Ancillary {
LabelTableEntry lookupLabel(LabelIndex p) {
  File labelTableFile = SPIFFS.open(labelTablePath);

  LabelTableEntry entry;

  labelTableFile.seek(p * sizeof(entry));

  labelTableFile.read(reinterpret_cast<uint8_t *>(&entry), sizeof(entry));

  return entry;
}

void backtrack() {
  if (exceptionRaised) {
    return;
  }

  cout << endl << " ---- Backtrack! ---- " << endl << endl;

#ifdef VERBOSE_LOG
  cout << "Choice point: " << *currentChoicePoint << endl;
#endif

  if (currentChoicePoint == nullptr) {
    failAndExit();
    return;
  }

#ifdef VERBOSE_LOG
  cout << "Current cut point: " << *currentCutPoint << endl;
#endif

  if (currentCutPoint->type != RegistryEntry::Type::choicePoint) {
    cout << "Not a choice point!" << endl;
    failWithException();
    return;
  }

  currentCutPoint = currentChoicePoint->body<ChoicePoint>().currentCutPoint;

#ifdef VERBOSE_LOG
  cout << "New cut choice point: " << *currentCutPoint << endl;
#endif

  programFile->seek(
      lookupLabel(currentChoicePoint->body<ChoicePoint>().retryLabel)
          .entryPoint);
}

void failAndExit() { querySucceeded = false; }

void failWithException() { exceptionRaised = true; }

void bind(RegistryEntry *a1, RegistryEntry *a2) {
  if (a1 == a2) {
    return;
  }

  if (a1->type == RegistryEntry::Type::reference &&
      a2->type == RegistryEntry::Type::reference &&
      a1->body<RegistryEntry *>() == a2->body<RegistryEntry *>()) {
    return;
  }

  if (a1->type == RegistryEntry::Type::reference &&
      ((a2->type != RegistryEntry::Type::reference) ||
       (a2->body<RegistryEntry *>() < a1->body<RegistryEntry *>()))) {
    trail(a1);
    a1->mutableBody<RegistryEntry *>() = a2;
  } else {
    trail(a2);
    a2->mutableBody<RegistryEntry *>() = a1;
  }
}

void trail(RegistryEntry *a) {
  if (currentChoicePoint != nullptr && a < currentChoicePoint) {
#ifdef VERBOSE_LOG
    cout << "Adding " << std::hex << a << " to the trail" << endl;
#endif
    newTrailItem(a);
  } else {
#ifndef VERBOSE_LOG
    cout << "Address " << std::hex << a << " is not conditional" << endl;
#endif
  }
}

void unwindTrail() {
#ifdef VERBOSE_LOG
  cout << "Unwinding trail" << endl;
#endif

  if (currentChoicePoint != nullptr) {
    while (trailHead > currentChoicePoint) {
      trailHead->mutableBody<TrailItem>().item->resetToVariable();
      trailHead = trailHead->mutableBody<TrailItem>().nextItem;
    }
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
  cout << "Unify:" << endl;
  cout << "a1: " << *a1 << endl;
  cout << "a2: " << *a2 << endl;
#endif

  RegistryEntry *d1 = a1->deref();
  RegistryEntry *d2 = a2->deref();

#ifdef VERBOSE_LOG
  cout << "d1: " << *d1 << endl;
  cout << "d2: " << *d2 << endl;
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
    cout << "Error during unify. Unknown type " << std::hex
         << static_cast<size_t>(d1->type) << endl;
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
  cout << "Evaluating Expression: " << std::hex << *a << endl;
#endif

  const RegistryEntry *d = a->deref();

#ifdef VERBOSE_LOG
  cout << "Derefs to: " << *d << endl;
#endif

  switch (d->type) {
  case RegistryEntry::Type::integer:
    return d->body<Integer>();
  case RegistryEntry::Type::structure:
    return evaluateStructure(d->body<Structure>());
  default:
    cout << "Exception: invalid expression type " << std::hex
         << static_cast<size_t>(d->type) << endl;
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
        cout << "Exception: divide by 0" << endl;
        failWithException();
        return 0;
      }
      return i1 / i2;
    }
    break;
  default:
    break;
  }
  cout << "Exception: not an operator: " << structure.functor << "/"
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
    cout << "Exception: pin out of range: " << std::dec << i << std::endl;

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
    cout << "Invalid channel number \"" << channelInt << "\"";

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
} // namespace Ancillary
