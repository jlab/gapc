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

#include <list>
#include <string>
#include "instance.hh"
#include "product.hh"
#include "fn_def.hh"
#include "var_acc.hh"

#include "log.hh"

Instance::Instance(std::string *n, Product::Base *p, const Loc &l)
  : name_(n), product(p), location(l), grammar_(0) {
}


Instance::Instance(std::string *n, Product::Base *p, Grammar *g)
  : name_(n), product(p), grammar_(g) {
}


Instance::Instance(Algebra *a, Algebra *b) {
  Product::Single *x = new Product::Single(a);
  Product::Single *y = new Product::Single(b);
  Product::Times *times = new Product::Times(x, y);
  product = times;
  [[maybe_unused]] bool r = product->init();
  assert(r);
}

Instance::Instance(Product::Base *a, Algebra *b) {
  Product::Base *x = a;
  Product::Single *y = new Product::Single(b);
  Product::Times *times = new Product::Times(x, y);
  product = times;
  [[maybe_unused]] bool r = product->init();
  assert(r);
}

bool Instance::init(Instance *instance) {
  if (instance && instance != this) {
    return true;
  }
  bool r = product->init();

  for (hashtable<std::string, Fn_Def*>::iterator i =
       product->algebra()->choice_fns.begin();
       i != product->algebra()->choice_fns.end(); ++i) {
    Fn_Def *f = i->second;
    if (f->choice_mode() != Mode::PRETTY) {
      continue;
    }
    Log::instance()->verboseMessage(location,
      "This algebra product results in choice fns of type PRETTY. I.e. you may"
      " get an exponential number of answers.");
  }
  return r;
}


std::ostream &Instance::put(std::ostream &s) const {
  s << "Instance: " << *name_ << std::endl;
  s << *product->algebra();
  return s;
}


void Instance::eliminate_lists() {
  product->eliminate_lists();
}

void Instance::codegen() {
  product = product->optimize_shuffle_products();
  product->init_fn_suffix("");
  product->codegen();
}


void Instance::print_code(Printer::Base &s) {
  product->print_code(s);
}


std::string *Instance::lookup(const std::string &n) {
  hashtable<std::string, Fn_Def*> fns;
  hashtable<std::string, Fn_Def*>::iterator i = product->algebra()->fns.find(n);
  if (i == product->algebra()->fns.end()) {
    return 0;
  }
  const std::string &s = i->second->target_name();
  if (s == "") {
    return 0;
  }
  return new std::string(s);
}


#include "statement/hash_decl.hh"
#include "statement.hh"


Statement::Hash_Decl *Instance::generate_hash_decl(
  const Fn_Def &fn, bool kbest) {
  Statement::Hash_Decl *ret = new Statement::Hash_Decl();
  ret->set_suffix(*fn.name);
  ret->set_answer_type(fn.return_type->component());

  product->init_vacc(new Var_Acc::Plain(new std::string("src")),
                     new Var_Acc::Plain(new std::string("dst")));

  std::list<Statement::Base*> code;
  std::list<Statement::Var_Decl*> filters;
  std::list<Statement::Base*> finalize_code;
  std::list<Statement::Base*> init_code;

  std::list<Statement::Base*> equal_score_code;
  std::list<Statement::Base*> compare_code;
  product->generate_hash_decl(fn, code, filters, finalize_code, init_code,
                              equal_score_code, compare_code);
  if (init_code.empty()) {
    init_code.push_back(new Statement::Return(new std::string("src")));
  } else {
    init_code.push_back(new Statement::Return(new std::string("dst")));
  }
  ret->set_code(code);
  ret->set_filters(filters);
  ret->set_finalize_code(finalize_code);
  ret->set_init_code(init_code);

  ret->set_equal_score_code(equal_score_code);
  ret->set_compare_code(compare_code);
  ret->set_kbest(kbest);

  return ret;
}


bool Instance::replace_classified_product() {
  bool r = false;
  product = product->replace_classified(r);
  return r;
}


void Instance::check_alphabets() {
  std::list<Type::Base*> list;
  for (Product::iterator i = Product::begin(product);
       i != Product::end(); ++i) {
    Product::Base *p = *i;
    if (!(*i)->is(Product::SINGLE)) {
      continue;
    }
    hashtable<std::string, Type::Base*>::iterator j =
      p->algebra()->params.find("alphabet");
    assert(j != p->algebra()->params.end());
    Type::Base *type = j->second;
    bool found = false;
    for (std::list<Type::Base*>::iterator k = list.begin();
         k != list.end(); ++k) {
      if ((*k)->is_eq(*type)) {
        found = true;
        break;
      }
    }
    if (!found) {
      list.push_back(type);
    }
  }
  assert(!list.empty());
  if (list.size() > 1) {
    throw LogError(location,
      "Alphabet types of algebras in the product are not compatible ");
  }
}

bool Instance::uses_tikz() {
  for (Product::iterator i = Product::begin(product);
       i != Product::end(); ++i) {
    if ((*i)->is(Product::SINGLE)) {
      if ((*i)->algebra()->get_auto_role() == Algebra::TIKZ) {
        return true;
      }
    }
  }
  return false;
}
