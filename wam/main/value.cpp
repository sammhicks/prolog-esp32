#include "value.h"

const size_t ScalarSize = std::max(sizeof(RegistryEntry *),
                                   std::max(sizeof(Constant), sizeof(Integer)));

RegistryEntry *RegistryEntry::deref() {
  if (type == Type::reference && mutableBody<RegistryEntry *>() != this) {
    return mutableBody<RegistryEntry *>()->deref();
  }

  return this;
}

const RegistryEntry *RegistryEntry::deref() const {
  if (type == Type::reference && body<const RegistryEntry *>() != this) {
    return body<const RegistryEntry *>()->deref();
  }

  return this;
}

void RegistryEntry::resetToVariable() {
#ifdef VERBOSE_LOG
  cout << "Resetting " << std::hex << this << " to variable" << endl;
#endif

  type = Type::reference;

  mutableBody<RegistryEntry *>() = this;
}

std::ostream &operator<<(std::ostream &os, const RegistryEntry &entry) {
  return entry.dump(os);
}

std::ostream &RegistryEntry::dump(std::ostream &os) const {
  os << "\tRegistry Index: " << std::hex << this;
  os << "\tTuple Index: " << std::hex << tuple;

  switch (type) {
  case Type::reference:
    os << "\tReference: " << std::hex << body<RegistryEntry *>();
    break;
  case Type::structure:
    os << "\tStructure: " << std::dec << body<Structure>().functor << "/"
       << std::dec << body<Structure>().arity;
    break;
  case Type::list:
    os << "\tList";
    break;
  case Type::constant:
    os << "\tConstant: " << std::dec << body<Constant>();
    break;
  case Type::integer:
    os << "\tInteger: " << std::dec << body<Integer>();
    break;
  case Type::environment:
    os << "\tEnvironment: " << std::dec << body<Environment>().size << "/"
       << std::dec << body<Environment>().capacity;
    break;
  case Type::choicePoint:
    os << "\tChoice Point";
    break;
  case Type::trailItem:
    os << "\tTrail Item: Current: " << std::hex << body<TrailItem>().item
       << "\tNext: " << std::hex << body<TrailItem>().nextItem;
    break;
  }

  return os;
}

void RegistryEntry::sendToClient(Client &client) const {
  Raw::write(client, type);

  switch (type) {
  case Type::reference:
    Raw::write(client, body<RegistryEntry *>());
    break;
  case Type::structure:
    Raw::write(client, body<Structure>().functor);
    Raw::write(client, body<Structure>().arity);
    for (Arity i = 0; i < body<Structure>().arity; ++i) {
      Raw::write(client, body<Structure>().subterms[i]);
    }
    break;
  case Type::list:
    Raw::write(client, body<List>().subterms[0]);
    Raw::write(client, body<List>().subterms[1]);
    break;
  case Type::constant:
    Raw::write(client, body<Constant>());
    break;
  case Type::integer:
    Raw::write(client, body<Integer>());
    break;
  case Type::environment:
    Raw::write(client, body<Environment>().size);
    for (EnvironmentSize i = 0; i < body<Environment>().size; ++i) {
      body<Environment>().permanentVariables[i]->deref()->sendToClient(client);
      break;
    }
  case Type::choicePoint:
    std::cout << "Cannot send choice point to client" << std::endl;
    break;
  case Type::trailItem:
    std::cout << "Cannot send trail item to client" << std::endl;
    break;
  default:
    std::cout << "Unknown value type \"" << std::hex
              << static_cast<size_t>(type) << "\"" << std::endl;
    break;
  }
}
