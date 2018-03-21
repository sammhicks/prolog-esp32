#include "value.h"

const size_t ScalarSize = std::max(sizeof(RegistryEntry *),
                                   std::max(sizeof(Constant), sizeof(Integer)));

size_t RegistryEntry::tupleSize(size_t headSize, Arity n) {
  return sizeof(Tuple) + headSize + n * sizeof(RegistryEntry *);
}

size_t RegistryEntry::tupleSize() {
  switch (type) {
  case RegistryEntry::Type::reference:
    return tupleSize(0, 1);
  case RegistryEntry::Type::structure:
    return tupleSize(sizeof(Structure), body<Structure>().arity);
  case RegistryEntry::Type::list:
    return tupleSize(0, 2);
  case RegistryEntry::Type::constant:
    return tupleSize(ScalarSize, 0);
  case RegistryEntry::Type::integer:
    return tupleSize(ScalarSize, 0);
  case RegistryEntry::Type::environment:
    return tupleSize(sizeof(Environment), body<Environment>().capacity);
  case RegistryEntry::Type::choicePoint:
    return tupleSize(sizeof(ChoicePoint),
                     body<ChoicePoint>().savedRegisterCount);
  case RegistryEntry::Type::trailItem:
    return tupleSize(0, 2);
  default:
    return 0;
  }
}

RegistryEntry *RegistryEntry::deref() {
  VERBOSE(Serial << "Dereferencing " << *this);

  if (type == Type::reference && mutableBody<RegistryEntry *>() != this) {
    return mutableBody<RegistryEntry *>()->deref();
  }

  return this;
}

const RegistryEntry *RegistryEntry::deref() const {
  VERBOSE(Serial << "Dereferencing " << *this);

  if (type == Type::reference && body<const RegistryEntry *>() != this) {
    return body<const RegistryEntry *>()->deref();
  }

  return this;
}

void RegistryEntry::resetToVariable() {
  VERBOSE(Serial << "Resetting " << this << " to variable" << endl);

  type = Type::reference;

  mutableBody<RegistryEntry *>() = this;
}

void RegistryEntry::bindTo(RegistryEntry *other) {
  switch (other->type) {
  case Type::reference:
    bindToVariable(other->mutableBody<RegistryEntry *>());
    break;
  case Type::structure:
  case Type::list:
    bindToVariable(other);
    break;
  case Type::constant:
    bindToConstant(other->body<Constant>());
    break;
  case Type::integer:
    bindToInteger(other->body<Integer>());
    break;
  default:
    Serial << "Cannot bind to type \"" << other->type << "\"" << endl;
    break;
  }
}

void RegistryEntry::bindToVariable(RegistryEntry *reference) {
  mutableBody<RegistryEntry *>() = reference;
}

void RegistryEntry::bindToConstant(Constant c) {
  type = Type::constant;

  mutableBody<Constant>() = c;
}

void RegistryEntry::bindToInteger(Integer i) {
  type = Type::integer;

  mutableBody<Integer>() = i;
}

Print &operator<<(Print &os, const RegistryEntry *entry) {
  if (entry == nullptr) {
    return os << "NULL";
  }

  return os << (entry - tupleRegistry);
}

Print &operator<<(Print &os, const RegistryEntry &entry) {
  return entry.dump(os);
}

Print &RegistryEntry::dump(Print &os) const {
  os << "Registry Entry:" << endl;
  os << "\tRegistry Index: " << (this - tupleRegistry) << endl;
  os << "\tTuple Index: " << (reinterpret_cast<uint8_t *>(tuple) - tuplesHeap)
     << endl;
  os << "\tTuple:" << endl;

  switch (type) {
  case Type::reference:
    os << "\t\tReference: " << body<RegistryEntry *>() << endl;
    break;
  case Type::structure:
    os << "\t\tStructure: " << body<Structure>() << endl;
    break;
  case Type::list:
    os << "\t\tList" << body<List>() << endl;
    break;
  case Type::constant:
    os << "\t\tConstant: " << body<Constant>() << endl;
    break;
  case Type::integer:
    os << "\t\tInteger: " << body<Integer>() << endl;
    break;
  case Type::environment:
    os << "\t\tEnvironment:" << endl;
    os << body<Environment>() << endl;
    break;
  case Type::choicePoint:
    os << "\t\tChoice Point:" << endl;
    os << body<ChoicePoint>() << endl;
    break;
  case Type::trailItem:
    os << "\t\tTrail Item: " << endl;
    os << "\t\t\tCurrent:  " << body<TrailItem>().item << endl;
    os << "\t\t\tNext:     " << body<TrailItem>().nextItem << endl;
    break;
  }

  return os;
}

Print &operator<<(Print &os, const RegistryEntry::Type &t) { return os; }

Print &operator<<(Print &os, const Structure &s) {
  os << s.functor << "/" << s.arity << " (";

  for (Arity i = 0; i < s.arity; ++i) {
    os << s.subterms[i] << ", ";
  }

  return os << ")";
}

Print &operator<<(Print &os, const List &l) {
  return os << "[" << l.subterms[0] << ", " << l.subterms[1] << "]" << endl;
}

Print &operator<<(Print &os, const Environment &e) {
  os << "\t\tNext Environment: " << e.nextEnvironment << endl;
  os << "\t\tContinue Point: " << e.continuePoint << endl;
  os << "\t\tSize: " << e.size << endl;
  os << "\t\tCapacity: " << e.capacity << endl;
  return os;
}

Print &operator<<(Print &os, const ChoicePoint &b) {
  os << "\t\t\tCurrent Environment: " << b.currentEnvironment << endl;
  os << "\t\t\tContinue Point: " << b.continuePoint << endl;
  os << "\t\t\tNext Choice Point: " << b.nextChoicePoint << endl;
  os << "\t\t\tRetry Label: " << b.retryLabel << endl;
  os << "\t\t\tCurrent Cut Point: " << b.currentCutPoint << endl;
  return os;
}

Print &operator<<(Print &os, const TrailItem &ti) { return os; }

void RegistryEntry::sendToClient(Client &client) const {
  Raw::write(client, type);

  switch (type) {
  case Type::reference:
    Raw::write(client, body<RegistryEntry *>());
    break;
  case Type::structure:
    sendToClient(client, body<Structure>());
    break;
  case Type::list:
    sendToClient(client, body<List>());
    break;
  case Type::constant:
    Raw::write(client, body<Constant>());
    break;
  case Type::integer:
    Raw::write(client, body<Integer>());
    break;
  case Type::environment:
    sendToClient(client, body<Environment>());
    break;
  case Type::choicePoint:
    Serial << "Cannot send choice point to client" << endl;
    break;
  case Type::trailItem:
    Serial << "Cannot send trail item to client" << endl;
    break;
  default:
    Serial << "Unknown value type \"" << type << "\"" << endl;
    break;
  }
}

void RegistryEntry::sendToClient(Client &client, const Structure &s) {
  Raw::write(client, s.functor);
  Raw::write(client, s.arity);
  for (Arity i = 0; i < s.arity; ++i) {
    Raw::write(client, s.subterms[i]);
  }
}

void RegistryEntry::sendToClient(Client &client, const List &l) {
  Raw::write(client, l.subterms[0]);
  Raw::write(client, l.subterms[1]);
}

void RegistryEntry::sendToClient(Client &client, const Environment &e) {
  Raw::write(client, e.size);
  for (EnvironmentSize i = 0; i < e.size; ++i) {
    Raw::write(client, e.permanentVariables[i]);
  }
}
