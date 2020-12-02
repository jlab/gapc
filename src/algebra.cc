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

#include "algebra.hh"
#include "log.hh"

#include "type.hh"
#include "fn_decl.hh"
#include "signature.hh"
#include "fn_def.hh"

#include "cc.hh"

#include "product.hh"

#include "arg.hh"

#include "printer.hh"

#include "mode.hh"

#include <iostream>




// create a joined algebra
Algebra::Algebra(Algebra &a, Algebra &b) :
  Signature_Base(), default_choice_fn_mode(0), signature(NULL),
  signature_name(NULL) {
  // join all params of both algebras
  // alphabet only needs to be added once,
  // all others are tupled for same string name
  for (hashtable<std::string, Type::Base*>::iterator i = a.params.begin();
       i != a.params.end(); ++i) {
    if (i->first == "alphabet") {
      params[i->first] = i->second;
      continue;
    }
    hashtable<std::string, Type::Base*>::iterator j = b.params.find(i->first);
    assert(j != b.params.end());
    params[i->first] = new Type::Tuple(i->second, j->second);
  }


  // join all functions as tuples on same string name
  for (hashtable<std::string, Fn_Def*>::iterator i = a.fns.begin();
       i != a.fns.end(); ++i) {
    hashtable<std::string, Fn_Def*>::iterator j = b.fns.find(i->first);
    if (j == b.fns.end()) {
      continue;
    }

    fns[i->first] = new Fn_Def(*i->second, *j->second);
  }

  // find choice function in functions
  init_choice_fns();
  // join names
  name = new std::string(*a.name + "_" + *b.name);

  // if signature is not the same we have a problem
  // but what would be the consequence of two equal signatures??
  assert(a.signature == b.signature);
  signature = a.signature;
  signature_name = a.signature_name;
}

// get function with name s
Fn_Decl* Algebra::decl(const std::string &s) {
  hashtable<std::string, Fn_Def*>::iterator i = fns.find(s);
  if (i == fns.end())
    return NULL;
  return i->second;
}

void Algebra::set_params(hashtable<std::string, Type::Base*> *p) {
  std::cout << "\n called set_params of class algebra \n";
  params = *p;
}

// add the signature in p to map of signature h, if it is not
// already contained in h
void Algebra::add_sig_var(hashtable<std::string, Type::Base*> &h,
    std::pair<std::string*, Type::Base*> &p, const Loc &l) {
  std::cout << "\n called add_sig_var of class algebra \n";
  hashtable<std::string, Type::Base*>::iterator i = h.find(*p.first);
  if (i != h.end()) {
    Log::instance()->error(l, "Type assignment to " + i->first + " redefined");
    Log::instance()->error(i->second->location, "here.");
  } else {
    h[*p.first] = p.second;
  }
}


bool Algebra::check_signature(Signature &s) {
  std::cout << "\n called check_signature of class algebra \n";
  signature = &s;
  s.set_algebra(this);
  bool r = true;

  // loop over all declarations
  for (hashtable<std::string, Fn_Decl*>::iterator i = s.decls.begin();
       i != s.decls.end(); ++i) {
    // get corresponding function by name
    hashtable<std::string, Fn_Def*>::iterator j = fns.find(i->first);
    if (j == fns.end()) {  // does such a function even exist?
      Log::instance()->error(i->second->location, "Signature function " +
          i->first + " not defined in Algebra " + *name);
      Log::instance()->error(location, "(here).");
      r = false;
      continue;
    }

    // test types and number of parameters
    bool b = *i->second == *j->second;
    r = r && b;
    b = j->second->check_ntparas(*i->second);
    r = r && b;
  }
  s.reset_algebra();
  return r;
}


// loop through all functions and filter out choice functions
void Algebra::init_choice_fns() {
  std::cout << "\n called init_choice_fns of class algebra \n";
  // FIXME
  // assert(choice_fns.empty());
  // see Signature::setDecls ...
  for (hashtable<std::string, Fn_Def*>::iterator i = fns.begin();
       i != fns.end(); ++i) {
    if (i->second->is_Choice_Fn()) {
      if (default_choice_fn_mode && i->second->choice_mode() == ::Mode::NONE) {
         i->second->choice_mode().set(*default_choice_fn_mode);
      }
      choice_fns[i->first] = i->second;
    }
  }
}

void Algebra::set_fns(const hashtable<std::string, Fn_Def*> &h) {
  std::cout << "\n called set_fns of class algebra \n";
  if (fns.empty()) {
    fns = h;
  } else {
    for (hashtable<std::string, Fn_Def*>::const_iterator i =
        h.begin(); i != h.end(); ++i) {
      fns[i->first] = i->second;
    }
  }
  init_choice_fns();
}

// test if all arguments are set through the given parameters
bool Algebra::check_params(Signature &s) {
  std::cout << "\n called check_params of class algebra \n";
  bool r = true;
  // loop over all arguments
  for (hashtable<std::string, Arg*>::iterator i = s.args.begin();
       i != s.args.end(); ++i) {
    // if no parameter exists for this needed argument, return false
    hashtable<std::string, Type::Base*>::iterator j = params.find(i->first);
    if (j == params.end()) {
      Log::instance()->error(location, "Signature argument " + i->first
          + " not defined in Algebra " + *name + ".");
      Log::instance()->error(i->second->location, "(declared here)");
      r = false;
    }
  }
  return r;
}

// write current state to outputstrean
std::ostream &Algebra::put(std::ostream &s) const {
  std::cout << "\n called put of class algebra \n";
  s << "Algebra " << *name << ":" << std::endl;
  s << std::endl;
  for (hashtable<std::string, Type::Base*>::const_iterator i =
      params.begin(); i != params.end(); ++i) {
    s << i->first << " = " << *i->second << std::endl;
  }
  s << std::endl;
  for (hashtable<std::string, Fn_Def*>::const_iterator i =
      fns.begin(); i != fns.end(); ++i) {
    s << *i->second << std::endl;
  }
  s << std::endl;
  return s;
}

// loop through all functions in the declaration and call annotate on them
void Algebra::annotate_terminal_arguments(Signature &s) {
  std::cout << "\n called annotate_terminal_arguments of class algebra \n";
  for (hashtable<std::string, Fn_Decl*>::iterator i = s.decls.begin();
       i != s.decls.end(); ++i) {
    hashtable<std::string, Fn_Def*>::iterator j = fns.find(i->first);
    assert(j != fns.end());
    j->second->annotate_terminal_arguments(*i->second);
  }
}

// call codegen on all function pairs
void Algebra::codegen(Product::Two &product) {
  std::cout << "\n called codegen of class algebra \n";
  Algebra *a = product.left()->algebra();
  Algebra *b = product.right()->algebra();
  for (hashtable<std::string, Fn_Def*>::iterator i = fns.begin();
       i != fns.end(); ++i) {
    hashtable<std::string, Fn_Def*>::iterator x = a->fns.find(i->first);
    assert(x != a->fns.end());
    hashtable<std::string, Fn_Def*>::iterator y = b->fns.find(i->first);
    assert(y != b->fns.end());
    i->second->codegen(*x->second, *y->second, product);
  }
}

// call codegen on all functions
// PRETTY is handled differently
void Algebra::codegen() {
  std::cout << "\n called codegen of class algebra \n";
  for (hashtable<std::string, Fn_Def*>::iterator i = choice_fns.begin();
       i != choice_fns.end(); ++i) {
    if (i->second->choice_mode() == Mode::PRETTY)
      continue;
    i->second->codegen();
  }
}

// call install choice on all functions
void Algebra::install_choice_filter(Filter &filter) {
  std::cout << "\n called install_choice_filter of class algebra \n";
  for (hashtable<std::string, Fn_Def*>::iterator i = choice_fns.begin();
       i != choice_fns.end(); ++i) {
    i->second->install_choice_filter(filter);
  }
}

// add suffix s to all function names
void Algebra::init_fn_suffix(const std::string &s) {
  std::cout << "\n called init_fn_suffix of class algebra \n";
  for (hashtable<std::string, Fn_Def*>::iterator i = fns.begin();
       i != fns.end(); ++i) {
    i->second->init_fn_suffix(s);
  }
}

void Algebra::print_code(Printer::Base &s) {
  std::cout << "\n called print_code of class algebra \n";
  s << endl;

  assert(signature);
  for (hashtable<std::string, Fn_Def*>::iterator i = fns.begin();
       i != fns.end(); ++i) {
    if (i->second->in_use() || signature->decl(i->first) )
      continue;
    s << *i->second;
  }

  for (hashtable<std::string, Fn_Def*>::iterator i = fns.begin();
       i != fns.end(); ++i) {
    if (!i->second->in_use())
      continue;
    s << *i->second;
  }
  s << endl;
}


void Algebra::derive_role() {
  std::cout << "\n called derive_role of class algebra \n";
  for (hashtable<std::string, Fn_Def*>::iterator i = choice_fns.begin();
       i != choice_fns.end(); ++i) {
    Fn_Def *fn = i->second;
    ::Mode m = fn->choice_mode();
    ::Mode tmp = fn->derive_role();
    if (m.is(::Mode::NONE)) {
      if (tmp == m) {
        Log::instance()->error(fn->location,
            "Could not autodetect role of choice function " + *fn->name
            + ". Perhaps you forgot list(). Or use an explicit qualifier"
            " like scoring.");
      }
      fn->set_mode(tmp);
    }

    if (m.is(::Mode::NONE)) {
      if (Log::instance()->is_debug()) {
        std::cerr << "Set autodetected m to: "
          << fn->choice_mode() << std::endl;
      }
    } else {
        if (!m.is(::Mode::NONE) && tmp != m) {
          std::ostringstream o;
          o << "Declared role of algebra choice function "
            << *fn->name << " (in algebra " << *name
            << ") does not match autodetected role " << tmp << '.';
          Log::instance()->warning(fn->location, o.str());
        }
    }
  }
}

void Algebra::set_default_choice_fn_mode(std::string *s) {
  std::cout << "\n called set_default_choice_fn_mode of class algebra \n";
  default_choice_fn_mode = s;
}

Type::Base *Algebra::answer_type() {
  std::cout << "\n called answer_type of class algebra \n";
  Type::Base *ret = 0;
  for (hashtable<std::string, Fn_Def*>::iterator i = choice_fns.begin();
      i != choice_fns.end(); ++i) {
    Fn_Def *fn = i->second;
    Type::Base *t = fn->return_type;
    if (t->simple()->is(Type::LIST))
      t = t->component();
    if (ret) {
      assert(t->is_eq(*ret));
    }
    ret = t;
  }
  return ret;
}

bool Algebra::is_compatible(Mode::Type t) {
  std::cout << "\n called is_compatible of class algebra \n";
  bool r = true;
  for (hashtable<std::string, Fn_Def*>::iterator i = choice_fns.begin();
       i != choice_fns.end(); ++i) {
    Fn_Def *fn = i->second;
    r = r && (fn->choice_mode() == t);
  }
  return r;
}


Fn_Def *Algebra::fn_def(const std::string &name) {
  std::cout << "\n called fn_def of class algebra \n";
  hashtable<std::string, Fn_Def*>::iterator i = fns.find(name);
  assert(i != fns.end());
  return i->second;
}


Fn_Def *Algebra::choice_fn(Fn_Def *f) {
  std::cout << "\n called choice_fn of class algebra \n";
  hashtable<std::string, Fn_Def*>::iterator i = choice_fns.find(*f->name);
  assert(i != choice_fns.end());
  return i->second;
}

void Algebra::add_choice_specialisations(Product::Two &product) {
  std::cout << "\n called add_choice_specialisations of class algebra \n";
  Algebra *a = product.left()->algebra();
  Algebra *b = product.right()->algebra();
  for (hashtable<std::string, Fn_Def*>::iterator i=choice_fns.begin();
       i != choice_fns.end(); ++i) {
    hashtable<std::string, Fn_Def*>::iterator x = a->fns.find(i->first);
    assert(x != a->fns.end());
    hashtable<std::string, Fn_Def*>::iterator y = b->fns.find(i->first);
    assert(y != b->fns.end());

    i->second->add_choice_specialization(*x->second, *y->second, product);
  }
}


Algebra *Algebra::copy() const {
  std::cout << "\n called copy of class algebra \n";
  Algebra *o = new Algebra(*this);
  if (signature_name)
    o->signature_name = new std::string(*signature_name);
  o->fns.clear();
  o->choice_fns.clear();
  for (hashtable<std::string, Fn_Def*>::const_iterator i = fns.begin();
       i != fns.end(); ++i) {
    Fn_Def *f = i->second->copy();
    o->fns[i->first] = f;
    hashtable<std::string, Fn_Def*>::const_iterator j = choice_fns.find(
      i->first);
    if (j != choice_fns.end())
      o->choice_fns[i->first] = f;
  }
  return o;
}
