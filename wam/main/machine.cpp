#include "machine.h"

const char *codePath = "/code";
const char *labelTablePath = "/label-table";

ExecuteModes executeMode;
bool querySucceeded;
Stream *instructionSource;
File *programFile;

RWModes rwMode;
Arity argumentCount;
HeapIndex h;
HeapIndex hb;
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
  b->h = 0;
  b->n = 0;
}

void executeInstructions(Client *client) {
  executeMode = ExecuteModes::query;
  querySucceeded = true;
  instructionSource = client;

  File actualProgramFile = SPIFFS.open(codePath);
  programFile = &actualProgramFile;

  haltIndex = programFile->size();

  while (executeMode == ExecuteModes::query) {
    executeInstruction();
  }

  Serial.println("Executing Program");
  instructionSource = programFile;

  e->n = argumentCount;
  for (Arity n = 0; n < argumentCount; ++n) {
    e->ys[n] = registers[n];
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
  while (querySucceeded && programFile->available() > 0) {
    executeInstruction();
  }

  if (querySucceeded) {
    for (Arity n = 0; n < e->n; ++n) {
      registers[n] = e->ys[n];
    }
    if (static_cast<void *>(b) == static_cast<void *>(stack)) {
      Serial.println("Done");
      Raw::write(*client, Results::success);
    } else {
      Serial.println("Choice Points");
      Raw::write(*client, Results::choicePoints);
    }
  } else {
    Serial.println("Failure!");
    Raw::write(*client, Results::failure);
  }
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
using Ancillary::deref;
using Ancillary::lookupLabel;
using Ancillary::topOfStack;
using Ancillary::unify;
using Ancillary::unwindTrail;

void putVariableXnAi(Xn xn, Ai ai) {
  heap[h].makeReference(h);
  registers[xn] = heap[h];
  registers[ai] = heap[h];
  h = h + 1;
}

void putVariableYnAi(Yn yn, Ai ai) {
  heap[h].makeReference(h);
  e->ys[yn] = heap[h];
  registers[ai] = heap[h];
  h = h + 1;
}

void putValueXnAi(Xn xn, Ai ai) { registers[ai] = registers[xn]; }

void putValueYnAi(Yn yn, Ai ai) { registers[ai] = e->ys[yn]; }

void putStructure(Functor f, Arity n, Ai ai) {
  heap[h].makeFunctor(f, n);
  registers[ai].makeStructure(h);
  h = h + 1;
}

void putList(Ai ai) { registers[ai].makeList(h); }

void putConstant(Constant c, Ai ai) { registers[ai].makeConstant(c); }

void putInteger(Integer i, Ai ai) { registers[ai].makeInteger(i); }

void getVariableXnAi(Xn xn, Ai ai) { registers[xn] = registers[ai]; }

void getVariableYnAi(Yn yn, Ai ai) { e->ys[yn] = registers[ai]; }

void getValueXnAi(Xn xn, Ai ai) {
  if (!unify(registers[xn], registers[ai])) {
    backtrack();
  }
}

void getValueYnAi(Yn yn, Ai ai) {
  if (!unify(e->ys[yn], registers[ai])) {
    backtrack();
  }
}

void getStructure(Functor f, Arity n, Ai ai) {
  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
    heap[h].makeStructure(h + 1);
    heap[h + 1].makeFunctor(f, n);
    bind(derefAi, heap[h]);
    h = h + 2;
    rwMode = RWModes::write;
    return;
  case Value::Type::structure:
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
  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
    heap[h].makeList(h + 1);
    bind(derefAi, heap[h]);
    h = h + 1;
    rwMode = RWModes::write;
    return;
  case Value::Type::list:
    s = derefAi.h;
    rwMode = RWModes::read;
    return;
  default:
    backtrack();
    return;
  }
}

void getConstant(Constant c, Ai ai) {
  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
    addToTrail(derefAi.h);
    derefAi.makeConstant(c);
    return;
  case Value::Type::constant:
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
  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
    addToTrail(derefAi.h);
    derefAi.makeInteger(i);
    return;
  case Value::Type::integer:
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
  heap[h].makeReference(h);
  registers[xn] = heap[h];
  h = h + 1;
}

void setVariableYn(Yn yn) {
  heap[h].makeReference(h);
  e->ys[yn] = heap[h];
  h = h + 1;
}

void setValueXn(Xn xn) {
  heap[h] = registers[xn];
  h = h + 1;
}

void setValueYn(Yn yn) {
  heap[h] = e->ys[yn];
  h = h + 1;
}

void setConstant(Constant c) {
  heap[h].makeConstant(c);
  h = h + 1;
}

void setInteger(Integer i) {
  heap[h].makeInteger(i);
  h = h + 1;
}

void unifyVariableXn(Xn xn) {
  switch (rwMode) {
  case RWModes::read:
    registers[xn] = heap[s];
    break;
  case RWModes::write:
    heap[h].makeReference(h);
    registers[xn] = heap[h];
    h = h + 1;
    break;
  }
  s = s + 1;
}

void unifyVariableYn(Yn yn) {
  switch (rwMode) {
  case RWModes::read:
    e->ys[yn] = heap[s];
    break;
  case RWModes::write:
    heap[h].makeReference(h);
    e->ys[yn] = heap[h];
    h = h + 1;
    break;
  }
  s = s + 1;
}

void unifyValueXn(Xn xn) {
  switch (rwMode) {
  case RWModes::read:
    if (!unify(registers[xn], heap[s])) {
      backtrack();
    };
    break;
  case RWModes::write:
    heap[h] = registers[xn];
    h = h + 1;
    break;
  }
  s = s + 1;
}

void unifyValueYn(Yn yn) {
  switch (rwMode) {
  case RWModes::read:
    if (!unify(e->ys[yn], heap[s])) {
      backtrack();
    };
    break;
  case RWModes::write:
    heap[h] = e->ys[yn];
    h = h + 1;
    break;
  }
  s = s + 1;
}

void unifyConstant(Constant c) {
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
    heap[h].makeConstant(c);
    h = h + 1;
    break;
  }
}

void unifyInteger(Integer i) {
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
    heap[h].makeInteger(i);
    h = h + 1;
    break;
  }
}

void allocate(EnvironmentSize n) {
  Serial.printf("Allocating %u values\n", static_cast<uint8_t>(n));
  Environment *newEnvironment = reinterpret_cast<Environment *>(topOfStack());

  newEnvironment->ce = e;
  newEnvironment->cp = cp;
  newEnvironment->n = n;

  e = newEnvironment;
}

void deallocate() {
  programFile->seek(e->cp);
  e = e->ce;
}

void call(LabelIndex p) {
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

  programFile->seek(entry.entryPoint);
  argumentCount = entry.arity;
}

void proceed() { programFile->seek(cp); }

void tryMeElse(LabelIndex l) {
  ChoicePoint *newChoicePoint = reinterpret_cast<ChoicePoint *>(topOfStack());

  newChoicePoint->ce = e;
  newChoicePoint->cp = cp;
  newChoicePoint->b = b;
  newChoicePoint->bp = l;
  newChoicePoint->tr = tr;
  newChoicePoint->h = h;
  newChoicePoint->n = argumentCount;
  for (Arity n = 0; n < argumentCount; ++n) {
    newChoicePoint->args[n] = registers[n];
  }

  b = newChoicePoint;
  hb = h;
}

void retryMeElse(LabelIndex l) {
  for (Arity n = 0; n < b->n; ++n) {
    registers[n] = b->args[n];
  }

  e = b->ce;
  cp = b->cp;
  b->bp = l;
  unwindTrail(b->tr, tr);
  tr = b->tr;
  h = b->h;
  hb = h;
}

void trustMe() {
  for (Arity n = 0; n < b->n; ++n) {
    registers[n] = b->args[n];
  }

  e = b->ce;
  cp = b->cp;
  unwindTrail(b->tr, tr);
  tr = b->tr;
  h = b->h;
  b = b->b;
  hb = h;
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
  if (static_cast<void *>(e) >= static_cast<void *>(b)) {
    // Serial.println("e higher");
    return e->ys + e->n;
  } else {
    // Serial.println("b higher");
    return b->args + b->n;
  }
}

void backtrack() {
  Serial.println("Backtrack!");
  if (static_cast<void *>(b) == static_cast<void *>(stack)) {
    failAndExit();
    return;
  }

  // b0 = b;
  programFile->seek(lookupLabel(b->bp).entryPoint);
}

void failAndExit() { querySucceeded = false; }

Value &deref(Value &a) {
  // Serial.print("Dereferencing value:");
  // a.dump();
  if (a.type == Value::Type::reference) {
    return deref(a.h);
  } else {
    return a;
  }
}

Value &deref(HeapIndex derefH) {
  // Serial.printf("Heap index %u:", static_cast<uint16_t>(derefH));
  // heap[derefH].dump();
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
  // Serial.printf("Adding %u to the trail\n", static_cast<uint8_t>(a));
  if (a < hb) {
    trail[tr] = a;
    tr = tr + 1;
  }
}

void unwindTrail(TrailIndex a1, TrailIndex a2) {
  // Serial.printf("Unwinding from %u to %u\n", static_cast<uint8_t>(a1),
  //               static_cast<uint8_t>(a2));
  for (TrailIndex i = a1; i < a2; ++i) {
    // Serial.printf("Resetting address %u\n", static_cast<uint16_t>(trail[i]));
    heap[trail[i]].makeReference(trail[i]);
  }
}

// void tidyTrail();

bool unify(Value &a1, Value &a2) {
  // Serial.println("Unify:");
  // Serial.print("a1: ");
  // a1.dump();
  // Serial.print("a2: ");
  // a2.dump();

  Value &d1 = deref(a1);
  Value &d2 = deref(a2);

  // Serial.print("d1: ");
  // d1.dump();
  // Serial.print("d2: ");
  // d2.dump();

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
} // namespace Ancillary
