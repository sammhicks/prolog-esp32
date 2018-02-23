#include "value.h"

void Value::makeReference(HeapIndex newH) {
  type = Type::reference;
  h = newH;
}

void Value::makeStructure(HeapIndex newH) {
  type = Type::structure;
  h = newH;
}

void Value::makeFunctor(Functor newF, Arity newN) {
  f = newF;
  n = newN;
}

void Value::makeList(HeapIndex newH) {
  type = Type::list;
  h = newH;
};

void Value::makeConstant(Constant newC) {
  type = Type::constant;
  c = newC;
}

void Value::makeInteger(Integer newI) {
  type = Type::integer;
  i = newI;
}

void Value::makeLevel(Level newLevel) {
  type = Type::level;
  level = newLevel;
}

void Value::dump() {
  switch (type) {
  case Value::Type::reference:
    Serial.printf("reference to %x\n", h);
    break;
  case Value::Type::structure:
    Serial.printf("structure starting at %x\n", h);
    break;
  case Value::Type::list:
    Serial.printf("list starting at %x\n", h);
    break;
  case Value::Type::constant:
    Serial.printf("constant %u\n", c);
    break;
  case Value::Type::integer:
    Serial.printf("integer %i\n", i);
    break;
  default:
    Serial.printf("Unknown type %u\n", static_cast<uint8_t>(type));
  }
}
