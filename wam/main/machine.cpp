#include "machine.h"

const char *codePath = "/code";
const char *labelTablePath = "/label-table";

ExecuteModes executeMode;
Stream *instructionSource;
File *programFile;

RWModes rwMode;
Arity argumentCount;
HeapIndex h;
HeapIndex s;
CodeIndex cp;
CodeIndex haltIndex;
Environment *e;

Value registers[registerCount];
Value heap[heapSize];
uint8_t stack[stackSize];

template <typename T> T read() { return Raw::read<T>(*instructionSource); }

void resetMachine() {
  Serial.println("Reset");
  h = 0;
  e = reinterpret_cast<Environment *>(stack);
  e->ce = nullptr;
  e->cp = 0;
  e->n = 0;
}

void executeInstructions(Client *client) {
  executeMode = ExecuteModes::query;
  instructionSource = client;

  File actualProgramFile = SPIFFS.open(codePath);
  programFile = &actualProgramFile;

  haltIndex = programFile->size();

  while (executeMode == ExecuteModes::query) {
    executeInstruction();
  }

  Serial.println("Executing Program");
  instructionSource = programFile;

  while (programFile->available() > 0) {
    executeInstruction();
  }

  Serial.println("Done");
  Raw::write(*client, Results::success);
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
  /*case Opcode::putVariableYnAi:Serial.println("putVariableYnAi");
    Instructions::putVariableYnAi(read<Yn>(), read<Ai>());
    break;*/
  case Opcode::putValueXnAi:
    Serial.println("putValueXnAi");
    Instructions::putValueXnAi(read<Xn>(), read<Ai>());
    break;
  /*case Opcode::putValueYnAi:Serial.println("putValueYnAi");
    Instructions::putValueYnAi(read<Xn>(), read<Ai>());
    break;*/
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
  /*case Opcode::getVariableYnAi:Serial.println("getVariableYnAi");
    Instructions::getVariableYnAi(read<Yn>(), read<Ai>());
    break;*/
  case Opcode::getValueXnAi:
    Serial.println("getValueXnAi");
    Instructions::getValueXnAi(read<Xn>(), read<Ai>());
    break;
  /*case Opcode::getValueYnAi:Serial.println("getValueYnAi");
    Instructions::getValueYnAi(read<Xn>(), read<Ai>());
    break;*/
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
  /*case Opcode::setVariableYn:Serial.println("setVariableYn");
    Instructions::setVariableYn(read<Yn>());
    break;*/
  case Opcode::setValueXn:
    Serial.println("setValueXn");
    Instructions::setValueXn(read<Xn>());
    break;
  /*case Opcode::setValueYn:Serial.println("setValueYn");
    Instructions::setValueYn(read<Yn>());
    break;*/
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
  /*case Opcode::unifyVariableYn:Serial.println("unifyVariableYn");
    Instructions::unifyVariableYn(read<Yn>());
    break;*/
  case Opcode::unifyValueXn:
    Serial.println("unifyValueXn");
    Instructions::unifyValueXn(read<Xn>());
    break;
  /*case Opcode::unifyValueYn:Serial.println("unifyValueYn");
    Instructions::unifyValueYn(read<Yn>());
    break;*/
  case Opcode::unifyConstant:
    Serial.println("unifyConstant");
    Instructions::unifyConstant(read<Constant>());
    break;
  case Opcode::unifyInteger:
    Serial.println("unifyInteger");
    Instructions::unifyInteger(read<Integer>());
    break;
  /*case Opcode::allocate:Serial.println("allocate");
    Instructions::allocate(Read::environmentSize());
    break;
  case Opcode::deallocate:Serial.println("deallocate");
    Instructions::deallocate();
    break;*/
  case Opcode::call:
    Serial.println("call");
    Instructions::call(read<ProgramIndex>());
    break;
  case Opcode::proceed:
    Serial.println("proceed");
    Instructions::proceed();
    break;
  default:
    Serial.print("Unknown opcode ");
    Serial.println(static_cast<int>(opcode), HEX);
    break;
  }
}

namespace Instructions {
using Ancillary::backtrack;
using Ancillary::bind;
using Ancillary::deref;
using Ancillary::trail;
using Ancillary::unify;

void putVariableXnAi(Xn xn, Ai ai) {
  heap[h].makeReference(h);
  registers[xn] = heap[h];
  registers[ai] = heap[h];
  h = h + 1;
}

// void putVariableYnAi(Yn yn, Ai ai) {}

void putValueXnAi(Xn xn, Ai ai) { registers[ai] = registers[xn]; }

// void putValueYnAi(Yn yn, Ai ai) {}

void putStructure(Functor f, Arity n, Ai ai) {
  heap[h].makeFunctor(f, n);
  registers[ai].makeStructure(h);
  h = h + 1;
}

void putList(Ai ai) { registers[ai].makeList(h); }

void putConstant(Constant c, Ai ai) { registers[ai].makeConstant(c); }

void putInteger(Integer i, Ai ai) { registers[ai].makeInteger(i); }

void getVariableXnAi(Xn xn, Ai ai) { registers[xn] = registers[ai]; }

// void getVariableYnAi(Yn yn, Ai ai) {}

void getValueXnAi(Xn xn, Ai ai) { unify(registers[xn], registers[ai]); }

// void getValueYnAi(Yn yn, Ai ai) {}

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
  Serial.printf("Constant: %u\n", c);
  Serial.printf("Ai: %u\n", ai);
  Value &derefAi = deref(registers[ai]);

  switch (derefAi.type) {
  case Value::Type::reference:
    Serial.printf("Reference to %u\n", derefAi.h);
    trail(derefAi);
    derefAi.makeConstant(c);
    return;
  case Value::Type::constant:
    Serial.printf("Constant %u\n", derefAi.c);
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
    trail(derefAi);
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

// void setVariableYn(Yn yn) {}

void setValueXn(Xn xn) {
  heap[h] = registers[xn];
  h = h + 1;
}

// void setValueYn(Yn yn) {}

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

// void unifyVariableYn(Yn yn) {}

void unifyValueXn(Xn xn) {
  switch (rwMode) {
  case RWModes::read:
    if (unify(registers[xn], heap[s])) {
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

// void unifyValueYn(Yn yn) {}

void unifyConstant(Constant c) {
  switch (rwMode) {
  case RWModes::read: {
    Value &derefS = deref(heap[s]);
    switch (derefS.type) {
    case Value::Type::reference:
      trail(derefS);
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
      trail(derefS);
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
  Environment *newEnvironment = reinterpret_cast<Environment *>(e->ys + e->n);

  newEnvironment->ce = e;
  newEnvironment->cp = cp;
  newEnvironment->n = n;

  e = newEnvironment;
}

void deallocate() {
  programFile->seek(e->cp);
  e = e->ce;
}

void call(ProgramIndex p) {
  switch (executeMode) {
  case ExecuteModes::query:
    executeMode = ExecuteModes::program;
    cp = haltIndex;
    break;
  case ExecuteModes::program:
    cp = programFile->position();
    break;
  }

  Serial.printf("Program index: %u\n", p);

  File labelTableFile = SPIFFS.open(labelTablePath);

  LabelTableEntry entry;

  labelTableFile.seek(p * sizeof(entry));

  labelTableFile.read(reinterpret_cast<uint8_t *>(&entry), sizeof(entry));

  Serial.printf("Entry Point: %u\n", entry.entryPoint);
  Serial.printf("Arity: %u\n", entry.arity);

  programFile->seek(entry.entryPoint);
  argumentCount = entry.arity;
}

void proceed() { programFile->seek(cp); }

} // namespace Instructions

namespace Ancillary {
void backtrack() { failAndExit(); }

void failAndExit() {
  Serial.println("Failed!");
  while (true) {
    delay(10);
  }
}

Value &deref(Value &a) {
  if (a.type == Value::Type::reference) {
    return deref(a.h);
  } else {
    return a;
  }
}

Value &deref(HeapIndex derefH) {
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
      ((a2.type != Value::Type::reference) || a2.h < a1.h)) {
    a1 = a2;
    trail(a1);
  } else {
    a2 = a1;
    trail(a2);
  }
}

void trail(const Value &a) {}

// void unwindTrail(const Value &a1, const Value &a2);

// void tidyTrail();

bool unify(Value &a1, Value &a2) {
  Value &d1 = deref(a1);
  Value &d2 = deref(a2);

  if (d1.type == Value::Type::reference || d1.type == Value::Type::reference) {
    bind(d1, d2);
    return false;
  }

  if (d1.type != d2.type) {
    return true;
  }

  switch (d1.type) {
  case Value::Type::constant:
    return d1.c != d2.c;
  case Value::Type::integer:
    return d1.i != d2.i;
  case Value::Type::list:
    return unify(heap[d1.h], heap[d2.h]) ||
           unify(heap[d1.h + 1], heap[d2.h + 1]);
  case Value::Type::structure: {
    Value &h1 = heap[d1.h];
    Value &h2 = heap[d2.h];

    if (h1.f != h2.f) {
      return true;
    }

    for (Arity n = 1; n <= h1.n; ++n) {
      if (unify(heap[d1.h + n], heap[d2.h + n])) {
        return true;
      }
    }
    return false;
  }
  default:
    return false;
  }
}
} // namespace Ancillary
