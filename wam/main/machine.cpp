#include "machine.h"

ExecuteModes executeMode;
Stream *instructionSource;
File programFile;
File labelTableFile;

RWModes rwMode;
Arity argumentCount;
HeapIndex h;
HeapIndex s;
CodeIndex cp;
Environment *e;

Value registers[registerCount];
Value heap[heapSize];
uint8_t stack[stackSize];

namespace Read {
Opcode opcode() { return static_cast<Opcode>(Raw::uint8(*instructionSource)); }
Xn xn() { return static_cast<Xn>(Raw::uint8(*instructionSource)); }
Yn yn() { return static_cast<Yn>(Raw::uint8(*instructionSource)); }
Ai ai() { return static_cast<Ai>(Raw::uint8(*instructionSource)); }
Functor f() { return static_cast<Functor>(Raw::uint16(*instructionSource)); }
Arity n() { return static_cast<Arity>(Raw::uint8(*instructionSource)); }
Constant c() { return static_cast<Constant>(Raw::uint16(*instructionSource)); }
Integer i() { return static_cast<Integer>(Raw::uint16(*instructionSource)); }
EnvironmentSize environmentSize() {
  return static_cast<EnvironmentSize>(Raw::uint8(*instructionSource));
}
ProgramIndex programIndex() {
  return static_cast<ProgramIndex>(Raw::uint16(*instructionSource));
}
Jump jump() { return static_cast<Jump>(Raw::uint16(*instructionSource)); }
} // namespace Read

void executeInstruction() {
  Opcode opcode = Read::opcode();
  Serial.print("Executing opcode ");
  Serial.println(static_cast<int>(opcode), HEX);

  switch (opcode) {
  case Opcode::putVariableXnAi:
    Instructions::putVariableXnAi(Read::xn(), Read::ai());
    break;
  /*case Opcode::putVariableYnAi:
    Instructions::putVariableYnAi(Read::yn(), Read::ai());
    break;*/
  case Opcode::putValueXnAi:
    Instructions::putValueXnAi(Read::xn(), Read::ai());
    break;
  /*case Opcode::putValueYnAi:
    Instructions::putValueYnAi(Read::xn(), Read::ai());
    break;*/
  case Opcode::putStructure:
    Instructions::putStructure(Read::f(), Read::n(), Read::ai());
    break;
  case Opcode::putList:
    Instructions::putList(Read::ai());
    break;
  case Opcode::putConstant:
    Instructions::putConstant(Read::c(), Read::ai());
    break;
  case Opcode::putInteger:
    Instructions::putInteger(Read::i(), Read::ai());
    break;
  case Opcode::getVariableXnAi:
    Instructions::getVariableXnAi(Read::xn(), Read::ai());
    break;
  /*case Opcode::getVariableYnAi:
    Instructions::getVariableYnAi(Read::yn(), Read::ai());
    break;*/
  case Opcode::getValueXnAi:
    Instructions::getValueXnAi(Read::xn(), Read::ai());
    break;
  /*case Opcode::getValueYnAi:
    Instructions::getValueYnAi(Read::xn(), Read::ai());
    break;*/
  case Opcode::getStructure:
    Instructions::getStructure(Read::f(), Read::n(), Read::ai());
    break;
  case Opcode::getList:
    Instructions::getList(Read::ai());
    break;
  case Opcode::getConstant:
    Instructions::getConstant(Read::c(), Read::ai());
    break;
  case Opcode::getInteger:
    Instructions::getInteger(Read::i(), Read::ai());
    break;
  case Opcode::setVariableXn:
    Instructions::setVariableXn(Read::xn());
    break;
  /*case Opcode::setVariableYn:
    Instructions::setVariableYn(Read::yn());
    break;*/
  case Opcode::setValueXn:
    Instructions::setValueXn(Read::xn());
    break;
  /*case Opcode::setValueYn:
    Instructions::setValueYn(Read::yn());
    break;*/
  case Opcode::setConstant:
    Instructions::setConstant(Read::c());
    break;
  case Opcode::setInteger:
    Instructions::setInteger(Read::i());
    break;
  case Opcode::unifyVariableXn:
    Instructions::unifyVariableXn(Read::xn());
    break;
  /*case Opcode::unifyVariableYn:
    Instructions::unifyVariableYn(Read::yn());
    break;*/
  case Opcode::unifyValueXn:
    Instructions::unifyValueXn(Read::xn());
    break;
  /*case Opcode::unifyValueYn:
    Instructions::unifyValueYn(Read::yn());
    break;*/
  case Opcode::unifyConstant:
    Instructions::unifyConstant(Read::c());
    break;
  case Opcode::unifyInteger:
    Instructions::unifyInteger(Read::i());
    break;
  /*case Opcode::allocate:
    Instructions::allocate(Read::environmentSize());
    break;
  case Opcode::deallocate:
    Instructions::deallocate();
    break;*/
  case Opcode::call:
    Instructions::call(Read::programIndex());
    break;
  case Opcode::proceed:
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

void putConstant(Constant c, Ai ai) {}

void putInteger(Integer i, Ai ai) {}

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

void getConstant(Constant c, Ai ai) {}

void getInteger(Integer i, Ai ai) {}

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

void setStructure(Functor f, Arity n) {}

void setConstant(Constant c) {}

void setInteger(Integer i) {}

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
    return;
  case RWModes::write:
    heap[h] = registers[xn];
    h = h + 1;
    break;
  }
  s = s + 1;
}

// void unifyValueYn(Yn yn) {}

void unifyStructure(Functor f, Arity n) {}

void unifyConstant(Constant c) {}

void unifyInteger(Integer i) {}

void allocate(EnvironmentSize n) {
  Environment *newEnvironment = reinterpret_cast<Environment *>(e->ys + e->n);

  newEnvironment->ce = e;
  newEnvironment->cp = cp;
  newEnvironment->n = n;

  e = newEnvironment;
}

void deallocate() {
  programFile.seek(e->cp);
  e = e->ce;
}

void call(ProgramIndex p) {
  switch (executeMode) {
  case ExecuteModes::query:
    executeMode = ExecuteModes::program;
    cp = haltIndex;
    break;
  case ExecuteModes::program:
    cp = programFile.position();
    break;
  }

  LabelTableEntry entry;

  labelTableFile.seek(p * sizeof(entry));

  labelTableFile.read(reinterpret_cast<uint8_t *>(&entry), sizeof(entry));

  programFile.seek(entry.entryPoint);
  argumentCount = entry.arity;
}

void proceed() { programFile.seek(cp); }

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

Value &deref(HeapIndex h) {
  if (heap[h].type == Value::Type::reference && heap[h].h != h) {
    return deref(heap[h].h);
  } else {
    return heap[h];
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
