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

void Value::makeList() { type = Type::list; };

void Value::makeConstant(Constant newC) {
  type = Type::constant;
  c = newC;
}

void Value::makeInteger(Integer newI) {
  type = Type::integer;
  i = newI;
}
