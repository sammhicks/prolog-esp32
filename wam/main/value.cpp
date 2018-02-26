#include "value.h"

RegistryEntry *RegistryEntry::deref() {
  if (type == Type::reference && tuple->body->reference != this) {
    return tuple->body->reference->deref();
  }

  return this;
}

const RegistryEntry *RegistryEntry::deref() const {
  if (type == Type::reference && tuple->body->reference != this) {
    return tuple->body->reference->deref();
  }

  return this;
}

std::ostream &RegistryEntry::operator<<(std::ostream &os) const {
  os << "\tRegistry Index: " << std::hex << this;
  os << "\tTuple Index: " << std::hex << tuple;

  switch (type) {
  case RegistryEntry::Type::reference:
    os << "\tReference: " << std::hex << tuple->body->reference;
    break;
  case RegistryEntry::Type::structure:
    os << "\tStructure: " << std::dec << tuple->body->structure.functor << "/"
       << std::dec << tuple->body->structure.arity;
    break;
  case RegistryEntry::Type::list:
    os << "\tList";
    break;
  case RegistryEntry::Type::constant:
    os << "\tConstant: " << std::dec << tuple->body->constant;
    break;
  case RegistryEntry::Type::integer:
    os << "\tInteger: " << std::dec << tuple->body->integer;
    break;
  case RegistryEntry::Type::environment:
    os << "\tEnvironment: " << std::dec << tuple->body->environment.size << "/"
       << std::dec << tuple->body->environment.capacity;
    break;
  case RegistryEntry::Type::choicePoint:
    os << "\tChoice Point";
    break;
  case RegistryEntry::Type::trailItem:
    break;
  }

  return os;
}

void RegistryEntry::sendToClient(Client &client) const {}
