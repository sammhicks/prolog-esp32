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
HeapIndex h;
HeapIndex s;
CodeIndex cp;
CodeIndex haltIndex;
TrailIndex tr;
Environment *e;
ChoicePoint *b;

Value registers[registerCount];
Value heap[heapSize];
uint8_t stack[stackSize];
HeapIndex trail[trailSize];

template <typename T> T read() { return Raw::read<T>(*instructionSource); }

void resetMachine() {
  Serial.println("Reset");
  h = 0;
  tr = 0;

  e = reinterpret_cast<Environment *>(stack);
  e->ce = nullptr;
  e->cp = 0;
  e->n = 0;

  b = reinterpret_cast<ChoicePoint *>(stack);
  b->ce = nullptr;
  b->cp = 0;
  b->b = nullptr;
  b->bp = 0;
  b->tr = 0;
  b->n = 0;
}

void executeInstructions(Client *client) {
  executeMode = ExecuteModes::query;
  querySucceeded = true;
  exceptionRaised = false;
  instructionSource = client;

  File actualProgramFile = SPIFFS.open(codePath);
  programFile = &actualProgramFile;

  haltIndex = programFile->size();

  while (executeMode == ExecuteModes::query) {
    executeInstruction();
  }

  if (!exceptionRaised) {
    Serial.println("Executing Program");
    instructionSource = programFile;

    e->n = argumentCount;
    for (Arity n = 0; n < argumentCount; ++n) {
      e->ys[n] = registers[n];
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
    Serial.println("Exception");
    Raw::write(*client, Results::exception);

    return;
  }

  if (querySucceeded) {
    if (static_cast<void *>(b) == static_cast<void *>(stack)) {
      Serial.println("Done");
      Raw::write(*client, Results::success);
    } else {
      Serial.println("Choice Points");
      Raw::write(*client, Results::choicePoints);
    }

    Environment *queryEnvironment = reinterpret_cast<Environment *>(stack);

    Raw::write(*client, queryEnvironment->n);
    for (Arity n = 0; n < queryEnvironment->n; ++n) {
      Raw::writeBlock(*client, Ancillary::deref(queryEnvironment->ys[n]));
    }

    return;
  }

  Serial.println("Failure!");
  Raw::write(*client, Results::failure);
}

void executeInstruction() {
  Serial.println();
  Opcode opcode = Raw::read<Opcode>(*instructionSource);
  Serial.print("Executing opcode ");
  Serial.println(static_cast<int>(opcode), HEX);

  switch (opcode) {
  case Opcode::putVariableXnAi:
    Serial.println("putVariableXnAi");
    Instructions::putVariableXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::putVariableYnAi:
    Serial.println("putVariableYnAi");
    Instructions::putVariableYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::putValueXnAi:
    Serial.println("putValueXnAi");
    Instructions::putValueXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::putValueYnAi:
    Serial.println("putValueYnAi");
    Instructions::putValueYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::putStructure:
    Serial.println("putStructure");
    Instructions::putStructure(read<Functor>(), read<Arity>(), read<Ai>());
    break;
  case Opcode::putList:
    Serial.println("putList");
    Instructions::putList(read<Ai>());
    break;
  case Opcode::putConstant:
    Serial.println("putConstant");
    Instructions::putConstant(read<Constant>(), read<Ai>());
    break;
  case Opcode::putInteger:
    Serial.println("putInteger");
    Instructions::putInteger(read<Integer>(), read<Ai>());
    break;
  case Opcode::getVariableXnAi:
    Serial.println("getVariableXnAi");
    Instructions::getVariableXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::getVariableYnAi:
    Serial.println("getVariableYnAi");
    Instructions::getVariableYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::getValueXnAi:
    Serial.println("getValueXnAi");
    Instructions::getValueXnAi(read<Xn>(), read<Ai>());
    break;
  case Opcode::getValueYnAi:
    Serial.println("getValueYnAi");
    Instructions::getValueYnAi(read<Yn>(), read<Ai>());
    break;
  case Opcode::getStructure:
    Serial.println("getStructure");
    Instructions::getStructure(read<Functor>(), read<Arity>(), read<Ai>());
    break;
  case Opcode::getList:
    Serial.println("getList");
    Instructions::getList(read<Ai>());
    break;
  case Opcode::getConstant:
    Serial.println("getConstant");
    Instructions::getConstant(read<Constant>(), read<Ai>());
    break;
  case Opcode::getInteger:
    Serial.println("getInteger");
    Instructions::getInteger(read<Integer>(), read<Ai>());
    break;
  case Opcode::setVariableXn:
    Serial.println("setVariableXn");
    Instructions::setVariableXn(read<Xn>());
    break;
  case Opcode::setVariableYn:
    Serial.println("setVariableYn");
    Instructions::setVariableYn(read<Yn>());
    break;
  case Opcode::setValueXn:
    Serial.println("setValueXn");
    Instructions::setValueXn(read<Xn>());
    break;
  case Opcode::setValueYn:
    Serial.println("setValueYn");
    Instructions::setValueYn(read<Yn>());
    break;
  case Opcode::setConstant:
    Serial.println("setConstant");
    Instructions::setConstant(read<Constant>());
    break;
  case Opcode::setInteger:
    Serial.println("setInteger");
    Instructions::setInteger(read<Integer>());
    break;
  case Opcode::unifyVariableXn:
    Serial.println("unifyVariableXn");
    Instructions::unifyVariableXn(read<Xn>());
    break;
  case Opcode::unifyVariableYn:
    Serial.println("unifyVariableYn");
    Instructions::unifyVariableYn(read<Yn>());
    break;
  case Opcode::unifyValueXn:
    Serial.println("unifyValueXn");
    Instructions::unifyValueXn(read<Xn>());
    break;
  case Opcode::unifyValueYn:
    Serial.println("unifyValueYn");
    Instructions::unifyValueYn(read<Yn>());
    break;
  case Opcode::unifyConstant:
    Serial.println("unifyConstant");
    Instructions::unifyConstant(read<Constant>());
    break;
  case Opcode::unifyInteger:
    Serial.println("unifyInteger");
    Instructions::unifyInteger(read<Integer>());
    break;
  case Opcode::allocate:
    Serial.println("allocate");
    Instructions::allocate(read<EnvironmentSize>());
    break;
  case Opcode::deallocate:
    Serial.println("deallocate");
    Instructions::deallocate();
    break;
  case Opcode::call:
    Serial.println("call");
    Instructions::call(read<LabelIndex>());
    break;
  case Opcode::proceed:
    Serial.println("proceed");
    Instructions::proceed();
    break;
  case Opcode::tryMeElse:
    Serial.println("tryMeElse");
    Instructions::tryMeElse(read<LabelIndex>());
    break;
  case Opcode::retryMeElse:
    Serial.println("retryMeElse");
    Instructions::retryMeElse(read<LabelIndex>());
    break;
  case Opcode::trustMe:
    Serial.println("trustMe");
    Instructions::trustMe();
    break;
  case Opcode::greaterThan:
    Serial.println("greaterThan");
    Instructions::greaterThan();
    break;
  case Opcode::lessThan:
    Serial.println("lessThan");
    Instructions::lessThan();
    break;
  case Opcode::lessThanOrEqualTo:
    Serial.println("lessThanOrEqualTo");
    Instructions::lessThanOrEqualTo();
    break;
  case Opcode::greaterThanOrEqualTo:
    Serial.println("greaterThanOrEqualTo");
    Instructions::greaterThanOrEqualTo();
    break;
  case Opcode::notEqual:
    Serial.println("notEqual");
    Instructions::notEqual();
    break;
  case Opcode::equals:
    Serial.println("equals");
    Instructions::equals();
    break;
  case Opcode::is:
    Serial.println("is");
    Instructions::is();
    break;
  case Opcode::noOp:
    Serial.println("noOp");
    Instructions::noOp();
    break;
  case Opcode::fail:
    Serial.println("fail");
    Instructions::fail();
    break;
  case Opcode::unify:
    Serial.println("unify");
    Instructions::unify();
    break;
  case Opcode::configureDigitalPin:
    Serial.println("configureDigitalPin");
    Instructions::configureDigitalPin(read<DigitalPinModes>());
    break;
  case Opcode::digitalReadPin:
    Serial.println("digitalReadPin");
    Instructions::digitalReadPin();
    break;
  case Opcode::digitalWritePin:
    Serial.println("digitalWritePin");
    Instructions::digitalWritePin();
    break;
  case Opcode::pinIsAnalogInput:
    Serial.println("pinIsAnalogInput");
    Instructions::pinIsAnalogInput();
    break;
  case Opcode::configureChannel:
    Serial.println("configureChannel");
    Instructions::configureChannel();
    break;
  case Opcode::pinIsAnalogOutput:
    Serial.println("pinIsAnalogOutput");
    Instructions::pinIsAnalogOutput();
    break;
  case Opcode::analogReadPin:
    Serial.println("analogReadPin");
    Instructions::analogReadPin();
    break;
  case Opcode::analogWritePin:
    Serial.println("analogWritePin");
    Instructions::analogWritePin();
    break;
  default:
    Serial.print("Unknown opcode ");
    Serial.println(static_cast<int>(opcode), HEX);
    Ancillary::backtrack();
    break;
  }
}

namespace Instructions {
using Ancillary::addToTrail;
using Ancillary::backtrack;
using Ancillary::bind;
using Ancillary::compare;
using Ancillary::deref;
using Ancillary::evaluateExpression;
using Ancillary::failWithException;
using Ancillary::getChannel;
using Ancillary::getPin;
using Ancillary::lookupLabel;
using Ancillary::topOfStack;
using Ancillary::unify;
using Ancillary::unwindTrail;
using Ancillary::virtualPredicate;

void putVariableXnAi(Xn xn, Ai ai) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
  Serial.printf("Ai - %u\n", ai);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h].makeReference(h);
  registers[xn] = heap[h];
  registers[ai] = heap[h];
  h = h + 1;
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
  h = h + 1;
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
  h = h + 1;
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
    Serial.println("Writing structure");
#endif
    heap[h].makeStructure(h + 1);
    heap[h + 1].makeFunctor(f, n);
    bind(derefAi, heap[h]);
    h = h + 2;
    rwMode = RWModes::write;
    return;
  case Value::Type::structure:
#ifdef VERBOSE_LOG
    Serial.println("Reading structure");
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
    Serial.println("Writing list");
#endif
    heap[h].makeList(h + 1);
    bind(derefAi, heap[h]);
    h = h + 1;
    rwMode = RWModes::write;
    return;
  case Value::Type::list:
#ifdef VERBOSE_LOG
    Serial.println("Reading list");
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
    Serial.println("Writing constant");
#endif
    addToTrail(derefAi.h);
    derefAi.makeConstant(c);
    return;
  case Value::Type::constant:
#ifdef VERBOSE_LOG
    Serial.println("Reading constant");
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
    Serial.println("Writing integer");
#endif
    addToTrail(derefAi.h);
    derefAi.makeInteger(i);
    return;
  case Value::Type::integer:
#ifdef VERBOSE_LOG
    Serial.println("Reading integer");
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
  h = h + 1;
}

void setVariableYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h].makeReference(h);
  e->ys[yn] = heap[h];
  h = h + 1;
}

void setValueXn(Xn xn) {
#ifdef VERBOSE_LOG
  Serial.printf("Xn - %u\n", xn);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h] = registers[xn];
  h = h + 1;
}

void setValueYn(Yn yn) {
#ifdef VERBOSE_LOG
  Serial.printf("Yn - %u\n", yn);
  Serial.printf("h  - %x\n", h);
#endif

  heap[h] = e->ys[yn];
  h = h + 1;
}

void setConstant(Constant c) {
#ifdef VERBOSE_LOG
  Serial.printf("c - %u\n", c);
  Serial.printf("h - %x\n", h);
#endif

  heap[h].makeConstant(c);
  h = h + 1;
}

void setInteger(Integer i) {
#ifdef VERBOSE_LOG
  Serial.printf("i - %i\n", i);
  Serial.printf("h - %x\n", h);
#endif

  heap[h].makeInteger(i);
  h = h + 1;
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
    h = h + 1;
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
    h = h + 1;
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
    h = h + 1;
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
    h = h + 1;
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
    h = h + 1;
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
    h = h + 1;
    break;
  }
  s = s + 1;
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

void deallocate() {
#ifdef VERBOSE_LOG
  Serial.printf("Deallocating environment %x to %x:\n",
                reinterpret_cast<uint8_t *>(e) - stack,
                reinterpret_cast<uint8_t *>(e->ce) - stack);
  Serial.printf("\tcp - %u\n", e->cp);
#endif
  programFile->seek(e->cp);
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
#ifdef VERBOSE_LOG
    Serial.printf("\tcp - %u\n", programFile->position());
#endif
    cp = programFile->position();
    break;
  }
  LabelTableEntry entry = lookupLabel(p);

#ifdef VERBOSE_LOG
  Serial.printf("\tp - %u\n", entry.entryPoint);
  Serial.printf("\targument count - %u\n", entry.arity);
#endif

  programFile->seek(entry.entryPoint);
  argumentCount = entry.arity;
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
  Serial.printf("\tb - %x\n", reinterpret_cast<uint8_t *>(b) - stack);
  Serial.printf("\tbp - %u\n", l);
  Serial.printf("\ttr - %u\n", tr);
  Serial.printf("\tn - %u\n", argumentCount);
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
  newChoicePoint->n = argumentCount;
  for (Arity n = 0; n < argumentCount; ++n) {
    newChoicePoint->args[n] = registers[n];
  }

  b = newChoicePoint;
}

void retryMeElse(LabelIndex l) {
#ifdef VERBOSE_LOG
  Serial.printf("\te - %x", reinterpret_cast<uint8_t *>(b->ce) - stack);
  Serial.printf("\tce - %u", b->cp);
  Serial.printf("\tbp - %u", l);
  Serial.printf("\tn - %u", b->n);
#endif

  for (Arity n = 0; n < b->n; ++n) {
    registers[n] = b->args[n];
  }

  e = b->ce;
  cp = b->cp;
  b->bp = l;
  unwindTrail(b->tr, tr);
  tr = b->tr;
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
  Serial.printf("Moving from choice point %x to %x\n",
                reinterpret_cast<uint8_t *>(b) - stack,
                reinterpret_cast<uint8_t *>(b->b) - stack);
#endif

  b = b->b;
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
    Serial.println("Analog pin write values must be positive");
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

void *topOfStack() {
#ifdef VERBOSE_LOG
  Serial.printf("e - %x\n", reinterpret_cast<uint8_t *>(e) - stack);
  Serial.printf("b - %x\n", reinterpret_cast<uint8_t *>(b) - stack);
#endif

  if (static_cast<void *>(e) >= static_cast<void *>(b)) {
#ifdef VERBOSE_LOG
    Serial.println("e higher");
#endif
    return e->ys + e->n;
  } else {
#ifdef VERBOSE_LOG
    Serial.println("b higher");
#endif
    return b->args + b->n;
  }
}

void backtrack() {
  if (exceptionRaised) {
    return;
  }

  Serial.println("\n ---- Backtrack! ---- \n");

#ifdef VERBOSE_LOG
  Serial.printf("Choice point: %x", reinterpret_cast<uint8_t *>(b) - stack);
#endif

  if (static_cast<void *>(b) == static_cast<void *>(stack)) {
    failAndExit();
    return;
  }

  // b0 = b;
  programFile->seek(lookupLabel(b->bp).entryPoint);
}

void failAndExit() { querySucceeded = false; }

void failWithException() { exceptionRaised = true; }

Value &deref(Value &a) {
#ifdef VERBOSE_LOG
  Serial.print("Dereferencing value:");
  a.dump();
#endif
  if (a.type == Value::Type::reference) {
    return deref(a.h);
  } else {
    return a;
  }
}

Value &deref(HeapIndex derefH) {
#ifdef VERBOSE_LOG
  Serial.printf("Heap index %u:", static_cast<uint16_t>(derefH));
  heap[derefH].dump();
#endif
  if (heap[derefH].type == Value::Type::reference && heap[derefH].h != derefH) {
    return deref(heap[derefH].h);
  } else {
    return heap[derefH];
  }
}

void bind(Value &a1, Value &a2) {
  if (a1.type == Value::Type::reference && a2.type == Value::Type::reference &&
      a1.h == a2.h) {
    return;
  }

  if (a1.type == Value::Type::reference &&
      ((a2.type != Value::Type::reference) || (a2.h < a1.h))) {
    addToTrail(a1.h);
    a1 = a2;
  } else {
    addToTrail(a2.h);
    a2 = a1;
  }
}

void addToTrail(HeapIndex a) {
#ifdef VERBOSE_LOG
  Serial.printf("Adding %u to the trail\n", a);
#endif

  trail[tr] = a;
  tr = tr + 1;
}

void unwindTrail(TrailIndex a1, TrailIndex a2) {
#ifdef VERBOSE_LOG
  Serial.printf("Unwinding from %u to %u\n", a1, a2);
#endif
  for (TrailIndex i = a1; i < a2; ++i) {
#ifndef VERBOSE_LOG
    Serial.printf("Resetting address %u\n", trail[i]);
#endif
    heap[trail[i]].makeReference(trail[i]);
  }
}

// void tidyTrail();

bool unify(Value &a1, Value &a2) {
#ifdef VERBOSE_LOG
  Serial.println("Unify:");
  Serial.print("a1: ");
  a1.dump();
  Serial.print("a2: ");
  a2.dump();
#endif

  Value &d1 = deref(a1);
  Value &d2 = deref(a2);

#ifdef VERBOSE_LOG
  Serial.print("d1: ");
  d1.dump();
  Serial.print("d2: ");
  d2.dump();
#endif

  if (d1.type == Value::Type::reference || d2.type == Value::Type::reference) {
    bind(d1, d2);
    return true;
  }

  if (d1.type != d2.type) {
    return false;
  }

  switch (d1.type) {
  case Value::Type::constant:
    return d1.c == d2.c;
  case Value::Type::integer:
    return d1.i == d2.i;
  case Value::Type::list:
    return unify(heap[d1.h], heap[d2.h]) &&
           unify(heap[d1.h + 1], heap[d2.h + 1]);
  case Value::Type::structure: {
    Value &h1 = heap[d1.h];
    Value &h2 = heap[d2.h];

    if (h1.f != h2.f) {
      return false;
    }

    for (Arity n = 1; n <= h1.n; ++n) {
      if (!unify(heap[d1.h + n], heap[d2.h + n])) {
        return false;
      }
    }
    return true;
  }
  default:
    Serial.printf("Error during unify. Unknown type %u\n",
                  static_cast<uint8_t>(d1.type));
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

Comparison compare(Value &e1, Value &e2) {
  return compare(evaluateExpression(e1), evaluateExpression(e2));
}

Integer evaluateExpression(Value &a) {
  if (exceptionRaised) {
    return 0;
  }

#ifdef VERBOSE_LOG
  Serial.print("Evaluating Expression: ");
  a.dump();
#endif

  Value &derefA = deref(a);

#ifdef VERBOSE_LOG
  Serial.print("Derefs to: ");
  derefA.dump();
#endif

  switch (derefA.type) {
  case Value::Type::integer:
    return derefA.i;
  case Value::Type::structure:
    return evaluateStructure(derefA);
  default:
    Serial.printf("Exception: invalid expression type %u\n",
                  static_cast<uint8_t>(derefA.type));
    failWithException();
    return 0;
  }
}

Integer evaluateStructure(Value &a) {
  if (exceptionRaised) {
    return 0;
  }

  switch (static_cast<SpecialStructures>(heap[a.h].f)) {
  case SpecialStructures::add:
    switch (heap[a.h].n) {
    case 1:
      return evaluateExpression(heap[a.h + 1]);
    case 2:
      return evaluateExpression(heap[a.h + 1]) +
             evaluateExpression(heap[a.h + 2]);
    default:
      break;
    }
  case SpecialStructures::subtract:
    switch (heap[a.h].n) {
    case 1:
      return -evaluateExpression(heap[a.h + 1]);
    case 2:
      return evaluateExpression(heap[a.h + 1]) -
             evaluateExpression(heap[a.h + 2]);
    default:
      break;
    }
  case SpecialStructures::multiply:
    if (heap[a.h].n == 2) {
      return evaluateExpression(heap[a.h + 1]) *
             evaluateExpression(heap[a.h + 2]);
    }
  case SpecialStructures::divide:
    if (heap[a.h].n == 2) {
      Integer i1 = evaluateExpression(heap[a.h + 1]);
      Integer i2 = evaluateExpression(heap[a.h + 2]);
      if (i2 == 0) {
        Serial.println("Exception: divide by 0");
        failWithException();
        return 0;
      }
      return i1 / i2;
    }
    break;
  default:
    break;
  }
  Serial.printf("Exception: not an operator: %u/%u\n",
                static_cast<uint16_t>(heap[a.h].f),
                static_cast<uint8_t>(heap[a.h].n));
  failWithException();
  return 0;
}

uint8_t getPin(Value &a) {
  Integer i = evaluateExpression(a);

  if (exceptionRaised) {
    return 0;
  }

  if (i < 0 || i >= 256) {
    Serial.printf("Exception: pin out of range: %i\n", i);

    failWithException();
    return 0;
  }

  return static_cast<uint8_t>(i);
}

uint8_t getChannel(Value &a) {
  Integer channelInt = evaluateExpression(a);

  if (exceptionRaised) {
    return 0;
  }

  if (channelInt < 0 || channelInt > 15) {
    Serial.printf("Invalid channel number \"%i\"",
                  static_cast<int>(channelInt));

    failWithException();
    return 0;
  }

  return static_cast<uint8_t>(channelInt);
}

void virtualPredicate(Arity n) {
  if (executeMode == ExecuteModes::query) {
    executeMode = ExecuteModes::program;
    cp = haltIndex;
    programFile->seek(haltIndex);
    argumentCount = n;
  }
}

} // namespace Ancillary
