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


#include "fn_decl.hh"
#include <utility>
#include <string>
#include <list>

#include "log.hh"

#include "yieldsize.hh"


Fn_Decl::~Fn_Decl() {}


Fn_Decl::Fn_Decl(Type::Base *r, std::string *n, const Loc &l)
  : in_use_(false), choice_fn(false), return_type(0), name(n), location(l) {
  init(r);
}

Fn_Decl::Fn_Decl(Type::Base *r, std::string *n)
  : in_use_(false), choice_fn(false), return_type(0), name(n) {
  init(r);
}


void Fn_Decl::init(Type::Base *r) {
  if (r->is(Type::CHOICE)) {
    return_type = dynamic_cast<Type::Choice*>(r)->rest;
    choice_fn = true;
    delete r;
  } else {
    return_type = r;
    choice_fn = false;
  }
}


hashtable<std::string, Fn_Decl*> Fn_Decl::builtins;


void Fn_Decl::init_table() {
  Type::Base *r = new Type::Char();
  std::string *s = new std::string("CHAR");
  Loc l;
  Fn_Decl *f = new Fn_Decl(r, s, l);
  f->types.push_back(r);

  Yield::Size ys(Yield::Poly(1), Yield::Poly(1));
  f->set_yield_size(ys);

  builtins[*s] = f;

  r = new Type::Float();
  s = new std::string("CONST_FLOAT");
  f = new Fn_Decl(r, s, l);
  f->types.push_back(r);
  ys = Yield::Size(Yield::Poly(0), Yield::Poly(0));
  f->set_yield_size(ys);
  builtins[*s] = f;

  r = new Type::Int();
  s = new std::string("CONST_INT");
  f = new Fn_Decl(r, s, l);
  f->types.push_back(r);
  ys = Yield::Size(Yield::Poly(0), Yield::Poly(0));
  f->set_yield_size(ys);
  builtins[*s] = f;

  r = new Type::Char();
  s = new std::string("CONST_CHAR");
  f = new Fn_Decl(r, s, l);
  f->types.push_back(r);
  ys = Yield::Size(Yield::Poly(0), Yield::Poly(0));
  f->set_yield_size(ys);
  builtins[*s] = f;

  r = new Type::Rational();
  s = new std::string("CONST_RATIO");
  f = new Fn_Decl(r, s, l);
  f->types.push_back(r);
  ys = Yield::Size(Yield::Poly(0), Yield::Poly(0));
  f->set_yield_size(ys);
  builtins[*s] = f;

  /*
  r = new Type::String();
  s = new std::string("CONST_STRING");
  f = new Fn_Decl(r, s, l);
  f->types.push_back(r);
  ys = Yield::Size(0, 0);
  f->set_yield_size(ys);
  builtins[*s] = f;
  */

  r = new Type::External("Rope");
  s = new std::string("CONST_ROPE");
  f = new Fn_Decl(r, s, l);
  f->types.push_back(new Type::String());
  ys = Yield::Size(Yield::Poly(0), Yield::Poly(0));
  f->set_yield_size(ys);
  builtins[*s] = f;

  r = new Type::External("Rope");
  s = new std::string("ROPE");
  f = new Fn_Decl(r, s, l);
  f->types.push_back(r);
  /* yield size will be later determined according to constant terminal
     arguments like ROPE("stefan") */
  ys = Yield::Size(Yield::Poly(1), Yield::Poly(1));
  f->set_yield_size(ys);
  builtins[*s] = f;
}


// FIXME no deep replacement - e.g. alphabet in list of list ...
void Fn_Decl::replace(Type::Base *a, Type::Base *b) {
  if (return_type->is_eq(*a)) {
    return_type = b;
  }
  for (std::list<Type::Base*>::iterator i = types.begin();
       i != types.end(); ++i) {
    if ((*i)->is_eq(*a)) {
      *i = b;
    }
  }
  std::list<Type::Base*> types;
}


#include "type/multi.hh"


void Fn_Decl::replace_types(
  const std::pair<std::string*, Type::Base*> &alph,
  const std::pair<std::string*, Type::Base*> &answer) {
  // FIXME for multiple answer types ...
  if (choice_fn) {
    assert(types.size() == 1);
    return_type = new Type::List(answer.second);
    types.clear();
    types.push_back(new Type::List(answer.second));
    return;
  }
  for (std::list<Type::Base*>::iterator i = types.begin();
       i != types.end(); ++i) {
    Type::Base *t = *i;
    if (t->simple()->is(Type::ALPHABET)) {
      *i = alph.second;
    } else if (t->simple()->is(Type::SIGNATURE)) {
      *i = answer.second;
    } else if (t->simple()->is(Type::MULTI)) {
      Type::Multi *m = dynamic_cast<Type::Multi*>(t->simple());
      assert(m);
      std::list<Type::Base*> types;
      for (std::list<Type::Base*>::const_iterator j = m->types().begin();
           j != m->types().end(); ++j) {
        Type::Base *t = *j;
        if (t->simple()->is(Type::ALPHABET)) {
          types.push_back(alph.second);
        } else if (t->simple()->is(Type::SIGNATURE)) {
          types.push_back(answer.second);
        } else {
          types.push_back(*j);
        }
      }
      *i = new Type::Multi(types);
    }
  }
  if (return_type->simple()->is(Type::SIGNATURE)) {
    return_type = answer.second;
  }
}

std::ostream &operator<<(std::ostream &s, const Fn_Decl &f) {
  s << *f.return_type << ' ' << *f.name << '(';
  for (std::list<Type::Base*>::const_iterator i = f.types.begin();
       i != f.types.end(); ++i) {
    s << **i << ", ";
  }
  s << ");";
  return s;
}


void Fn_Decl::set_types(std::list<Type::Base*> *l) {
  types = *l;
}


void Fn_Decl::add_fn_decl(hashtable<std::string, Fn_Decl *> *h, Fn_Decl *f) {
  if (h->find(*f->name) != h->end()) {
    Log::instance()->error(
      f->location, "Operator name " + *f->name + " redefined");
    Log::instance()->error(h->operator[](*f->name)->location, "here.");
  } else {
    h->operator[](*f->name) = f;
  }
}

bool Fn_Decl::operator==(const Fn_Decl &d) const {
  bool r = true;
  if (choice_fn != d.choice_fn) {
    Log::instance()->error(location, "Choice modifier does not match");
    Log::instance()->error(d.location, "between these two.");
    r = false;
  }
  if (!return_type->is_eq(*d.return_type)) {
    std::ostringstream o1;
    o1 << "Return type " << *return_type << " does not match";
    Log::instance()->error(return_type->location, o1.str());
    std::ostringstream o2;
    o2 << "return type " << *d.return_type << ".";
    Log::instance()->error(d.return_type->location, o2.str());
    r = false;
  }
  if (types.size() != d.types.size()) {
    Log::instance()->error(location, "Number of types does not");
    Log::instance()->error(d.location, "match.");
    return false;
  }
  std::list<Type::Base*>::const_iterator i = types.begin();
  for (std::list<Type::Base*>::const_iterator j = d.types.begin();
       i != types.end() && j != d.types.end(); ++i, ++j) {
    if (!(*i)->is_eq(**j)) {
      std::ostringstream o1;
      o1 << "Type " << **i << " does not match";
      Log::instance()->error((*i)->location, o1.str());
      std::ostringstream o2;
      o2 << "datatype " << **j << ".";
      Log::instance()->error((*j)->location, o2.str());
      r = false;
    }
  }
  return r;
}


bool Fn_Decl::types_equal(const Fn_Decl &d) {
  return types.size() == d.types.size();
}


void Fn_Decl::set_nttypes(std::list<Type::Base*> *l) {
  if (!l) {
    return;
  }
  nttypes_ = *l;
}
