/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2008-2011  Georg Sauthoff
         email: gsauthof@techfak.uni-bielefeld.de or gsauthof@sdf.lonestar.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

}}} */


#include <cassert>
#include <list>
#include <utility>
#include <string>

#include "type.hh"
#include "log.hh"
#include "signature.hh"
#include "table.hh"
#include "printer.hh"

namespace Type {

// Sets the type reference of the first parameter to
// the type the second parameter points to, under the
// condition, that both types are compatible. If the
// first type instance is NULL, it is overwritten with
// the second instance without checking both types.
bool set_if_compatible(Base * &a, Base *b, const Loc &l1, const Loc &l2) {
  if (!a && !b) {
    return true;
  }
  if (!a) {
    a = b;
    return true;
  }
  bool r = a->is_eq(*b);
  if (!r) {
    std::ostringstream o1;
    o1 << "Type " << *a << " is not compatible with";
    Log::instance()->error(l1, o1.str());
    std::ostringstream o2;
    o2 << "data type " << *b << '.';
    Log::instance()->error_continue(l2, o2.str());
  }
  return r;
}

}  // namespace Type


Type::Tuple::Tuple(Base *a, Base *b) : Base(TUPLE) {
  Tuple_Pair *l = new Tuple_Pair();
  l->first = new Name(a);
  l->second = new std::string("first");
  Tuple_Pair *r = new Tuple_Pair();
  r->first = new Name(b);
  r->second = new std::string("second");

  members[*l->second] = l->first;
  names.push_back(l->second);
  list.push_back(l);

  members[*r->second] = r->first;
  names.push_back(r->second);
  list.push_back(r);
}


void Type::Tuple::init(Tuple_List *l) {
  for (Tuple_List::iterator i = l->begin(); i != l->end(); ++i) {
    Tuple_Pair *p = *i;
    if (members[*(p->second)]) {
      Log::instance()->error(p->first->location,
                             "Component name " + *(p->second) +
                             " already used");
      Log::instance()->error(members[*(p->second)]->location, "here.");
    } else {
      members[*(p->second)] = p->first;
      names.push_back(p->second);
      list.push_back(p);
    }
  }
}


void Type::add_predefined(hashtable<std::string, Base*> &table) {
  ::Type::Base *t = new ::Type::Int();
  std::string s = "int";
  table[s] = t;

  t = new ::Type::Integer();
  s = "integer";
  table[s] = t;

  t = new ::Type::Float();
  s = "float";
  table[s] = t;
  s = "double";
  table[s] = t;

  t = new ::Type::Single();
  s = "single";
  table[s] = t;

  t = new ::Type::String();
  s = "string";
  table[s] = t;

  t = new ::Type::Char();
  s = "char";
  table[s] = t;

  t = new ::Type::Bool();
  s = "bool";
  table[s] = t;

  t = new ::Type::Void();
  s = "void";
  table[s] = t;

  t = new ::Type::Alphabet();
  s = "alphabet";
  table[s] = t;

  // FIXME use seq type information from ast
  t = new ::Type::Subseq();
  s = "Subsequence";
  table[s] = t;

  t = new ::Type::Shape();
  s = "shape";
  table[s] = t;

  t = new ::Type::Rational();
  s = "rational";
  table[s] = t;

  t = new ::Type::BigInt();
  s = "bigint";
  table[s] = t;
}



/*
bool Type::TypeDef::is(Type::Type t)
{
  return rhs->is(t);
}
*/


bool Type::List::is_eq(const Base & base) const {
  const List *l = dynamic_cast<const List*> (base.const_simple());
  if (!l) {
    return false;
  }
  return of->is_eq(*l->of);
}


bool Type::Tuple::is_eq(const Base & base) const {
  const Tuple *t = dynamic_cast<const Tuple*>(base.const_simple());
  if (!t) {
    return false;
  }
  if (list.size() != t->list.size()) {
    return false;
  }
  Tuple_List::const_iterator i = list.begin();
  for (Tuple_List::const_iterator j = t->list.begin();
       i != list.end() && j!= t->list.end(); ++i, ++j) {
    if (*(*i)->second != *(*j)->second) {
      return false;
    }
    if (!(*i)->first->is_eq(*(*j)->first)) {
      return false;
    }
  }
  return true;
}


bool Type::TupleDef::is_eq(const Base & base) const {
  const TupleDef *t = dynamic_cast<const TupleDef*>(base.const_simple());
  if (!t) {
    return false;
  }
  if (list.size() != t->list.size()) {
    return false;
  }
  Tuple_List::const_iterator i = list.begin();
  for (Tuple_List::const_iterator j = t->list.begin();
       i != list.end() && j!= t->list.end(); ++i, ++j) {
    if (*(*i)->second != *(*j)->second) {
      return false;
    }
    if (!(*i)->first->is_eq(*(*j)->first)) {
      return false;
    }
  }
  return true;
}


bool Type::Name::is_eq(Name & n) const {
  return lhs->const_simple()->is_eq(*n.lhs->const_simple());
}


bool Type::Signature::is_eq(const Base & base) const {
  Base *t = signature->var_lookup(this);
  if (t) {
    return t->is_eq(base);
  }

  const ::Type::Signature *s = dynamic_cast<const ::Type::Signature*>(
    base.const_simple());
  if (!s) {
    return false;
  }
  return *_name == *s->_name;
}


bool Type::Alphabet::is_eq(const Base &base) const {
  assert(signature);
  Base *t = signature->var_lookup(this);
  if (t) {
    return t->is_eq(base);
  }
  if (temp) {
    return temp->is_eq(base);
  }
  return base.const_simple()->is(ALPHABET);
}


bool Type::Range::is_eq(const Base & base) const {
  const Range * t = dynamic_cast<const Range*>(base.const_simple());
  if (!t) {
    return false;
  }
  return element_type->is_eq(*t->element_type);
}


Type::Base * Type::Def::simple() {
  return rhs->simple();
}


Type::Base * Type::Usage::simple() {
  return base->simple();
}


Type::Base * Type::Signature::simple() {
  Base *t = signature->var_lookup(this);
  if (t) {
    return t;
  }
  return this;
}


Type::Base *Type::Alphabet::simple() {
  assert(signature);
  Base *t = signature->var_lookup(this);
  if (t) {
    return t->simple();
  }
  if (temp) {
    return temp->simple();
  }
  return this;
}


const Type::Base * Type::Def::const_simple() const {
  return rhs->const_simple();
}


const Type::Base * Type::Usage::const_simple() const {
  return base->const_simple();
}


const Type::Base * Type::Signature::const_simple() const {
  Base *t = signature->var_lookup(this);
  if (t) {
    return t;
  }
  return this;
}


const Type::Base *Type::Alphabet::const_simple() const {
  assert(signature);
  Base *t = signature->var_lookup(this);
  if (t) {
    return t->const_simple();
  }
  if (temp) {
    return temp->const_simple();
  }
  return this;
}


bool Type::Def::is_eq(const Base & base) const {
  return rhs->is_eq(*base.const_simple());
}


bool Type::Usage::is_eq(const Base & b) const {
  return base->is_eq(*b.const_simple());
}


bool Type::External::is_eq(const Base & base) const {
  const External *l = dynamic_cast<const External*>(base.const_simple());
  if (!l)
    return false;
  return *name == *l->name;
}


std::ostream & Type::List::put(std::ostream &s) const {
  if (of) {
    s << '[' << *of << ']';
  } else {
    s << '[' << "NULL" << ']';
  }
  if (hash_decl_) {
    s << "hdecl" << hash_decl_;
  }
  return s;
}


std::ostream & Type::Tuple::put(std::ostream &s) const {
  s << '(';
  for (Tuple_List::const_iterator i = list.begin(); i != list.end(); ++i) {
    s << *(*i)->first << ' ' << *(*i)->second << ", ";
  }
  s << ')';
  return s;
}


std::ostream & Type::Name::put(std::ostream &s) const {
  s << *lhs;
  return s;
}


std::ostream & Type::Alphabet::put(std::ostream &s) const {
  assert(signature);
  Base *t = signature->var_lookup(this);
  if (t) {
    s << *t;
    return s;
  }
  if (temp) {
    s << *temp;
    return s;
  }
  s << "alphabet";
  return s;
}


std::ostream & Type::Def::put(std::ostream &s) const {
  s << *name << "{ " << *rhs << '}';
  return s;
}


std::ostream & Type::Usage::put(std::ostream &s) const {
  s << *base;
  return s;
}


std::ostream & Type::Choice::put(std::ostream &s) const {
  s << "choice " << *rest;
  return s;
}


std::ostream & Type::Void::put(std::ostream &s) const {
  s << "void";
  return s;
}


std::ostream & Type::RealVoid::put(std::ostream &s) const {
  s << "void";
  return s;
}


std::ostream & Type::Int::put(std::ostream &s) const {
  s << "int";
  return s;
}


std::ostream & Type::Integer::put(std::ostream &s) const {
  s << "integer";
  return s;
}


std::ostream & Type::Size::put(std::ostream &s) const {
  s << "size";
  return s;
}


std::ostream & Type::Float::put(std::ostream &s) const {
  s << "float";
  return s;
}


std::ostream & Type::Single::put(std::ostream &s) const {
  s << "single";
  return s;
}


std::ostream & Type::String::put(std::ostream &s) const {
  s << "string";
  return s;
}


std::ostream & Type::Char::put(std::ostream &s) const {
  s << "char";
  return s;
}


std::ostream & Type::Bool::put(std::ostream &s) const {
  s << "bool";
  return s;
}


std::ostream & Type::Signature::put(std::ostream &s) const {
  Base *t = signature->var_lookup(this);
  if (t) {
    s << *t;
    return s;
  }
  s << "Sig " << *_name;
  return s;
}


std::ostream & Type::Range::put(std::ostream &s) const {
  s << "Range of " << *element_type;
  return s;
}


std::ostream & Type::Seq::put(std::ostream &s) const {
  s << "[Input-Sequence-Type of " << *element_type << ']';
  return s;
}


std::ostream & Type::Table::put(std::ostream &s) const {
  s << "[Table of " << *element_type << ", " << *table << "]";
  return s;
}


Type::Base *Type::List::left() {
  assert(of->simple()->is(TUPLE));
  Tuple *tuple = dynamic_cast<Tuple*> (of->simple());
  assert(tuple);
  assert(tuple->list.size() == 2);
  std::pair<Name*, std::string*> *pair = tuple->list.front();
  return pair->first->lhs;
}


Type::Base *Type::Tuple::left() {
  assert(list.size() == 2);
  std::pair<Name*, std::string*> *pair = list.front();
  return pair->first->lhs;
}


Type::Base *Type::List::right() {
  assert(of->simple()->is(TUPLE));
  Tuple *tuple = dynamic_cast<Tuple*>(of->simple());
  assert(tuple);
  assert(tuple->list.size() == 2);
  std::list<std::pair<Name*, std::string*>*>::iterator i = tuple->list.begin();
  ++i;
  assert(i != tuple->list.end());
  std::pair<Name*, std::string*> *pair = *i;
  assert(pair);
  return pair->first->lhs;
}


Type::Base *Type::Tuple::right() {
  assert(list.size() == 2);
  std::list<std::pair<Name*, std::string*>*>::iterator i = list.begin();
  ++i;
  assert(i != list.end());
  std::pair<Name*, std::string*> *pair = *i;
  assert(pair);
  return pair->first->lhs;
}


Type::Base *Type::List::component() {
  /*assert(of->simple()->is(Type::TUPLE));
  Type::Tuple *tuple = dynamic_cast<Type::Tuple*>(of->simple());
  assert(tuple);
  return tuple; */
  return of->simple();
}


Type::Base *Type::Tuple::component() {
  return simple();
}


Type::Base *Type::Referencable::deref() {
  return base;
}


void Type::Seq::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::List::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Tuple::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::TupleDef::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Signature::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Alphabet::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Def::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Choice::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Void::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::RealVoid::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Int::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Integer::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Size::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Float::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Single::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::String::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Char::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Bool::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Usage::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Range::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Table::print(Printer::Base &s) const {
  s.print(*this);
}


std::string Type::List::push_str() const {
  switch (push_type_) {
    case NORMAL : return std::string();
    case MIN : return std::string("min");
    case MAX : return std::string("max");
    case SUM : return std::string("sum");
    case MIN_OTHER : return std::string("min_other");
    case MAX_OTHER : return std::string("max_other");
    case MIN_SUBOPT: return std::string("min_subopt");
    case MAX_SUBOPT: return std::string("max_subopt");

    case HASH : std::abort(); return std::string();
  }
  std::abort();
  return std::string();
}


std::ostream & Type::Subseq::put(std::ostream &s) const {
  if (seq) {
    s << "<Input-Sub-Sequence-Type of " << *seq << '>';
  } else {
    s << "<Input-Sub-Sequence-Type>";
  }
  return s;
}


void Type::Subseq::print(Printer::Base &s) const {
  s.print(*this);
}


std::ostream & Type::Generic::put(std::ostream &s) const {
  assert(!name.empty());
  s << name;
  return s;
}


void Type::Shape::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::Rational::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::BigInt::print(Printer::Base &s) const {
  s.print(*this);
}


std::ostream & Type::Referencable::put(std::ostream &s) const {
  assert(base);
  s << *base << " & ";
  return s;
}


void Type::Referencable::print(Printer::Base &s) const {
  s.print(*this);
}


std::ostream & Type::External::put(std::ostream &s) const {
  s << *name;
  return s;
}


void Type::External::print(Printer::Base &s) const {
  s.print(*this);
}


void Type::List::set_push_type(Push_Type x) {
  // assert(push_type_ == NORMAL || push_type_ == x);
  push_type_ = x;
}


void Type::List::set_hash_decl(Statement::Hash_Decl *h) {
  assert(h);
  hash_decl_ = h;
}
