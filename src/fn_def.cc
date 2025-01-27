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
#include <cmath>
#include <sstream>
#include <iostream>
#include <vector>
#include <utility>
#include <list>
#include <string>

#include "fn_def.hh"

#include "statement.hh"
#include "expr.hh"
#include "const.hh"

#include "log.hh"

#include "product.hh"

#include "para_decl.hh"

#include "type/multi.hh"

#include "expr/fn_call.hh"

#include "operator.hh"


// join two Function definitions into one
Fn_Def::Fn_Def(Fn_Def &a, Fn_Def &b)
  :  adaptor(NULL), comparator(NULL), sorter(NULL),
    choice_fn_type_(Expr::Fn_Call::NONE) {
  gen_type = a.gen_type;
  comperator_suffix = b.comperator_suffix;
  sorter_suffix = b.sorter_suffix;
  nullary_sort_ob = b.nullary_sort_ob;

  // assert(a.in_use() == b.in_use());
  set_in_use(a.in_use() || b.in_use());
  choice_fn = a.choice_fn;

  // set return type (LIST, Single for Left and Right choice function)
  // new return type is a tuple of both choice functions
  // names are the same for both choice functions
  if (choice_fn) {
    Type::Base *x = 0;
    if (a.return_type->is(Type::LIST))
      x = dynamic_cast<Type::List*>(a.return_type->simple())->of->simple();
    else
      x = a.return_type->simple();

    Type::Base *y = 0;
    if (b.return_type->is(Type::LIST))
      y = dynamic_cast<Type::List*>(b.return_type->simple())->of->simple();
    else
      y = b.return_type->simple();

    Type::List *l = new Type::List(new Type::Tuple(x, y));
    return_type = l;
    types.push_back(l);
    names = a.names;
    name = a.name;

    assert(!names.empty());
    paras.push_back(new Para_Decl::Simple(l, names.front()));
    return;
  }

  return_type = new Type::Tuple(a.return_type, b.return_type);

  // iterate over both parameter list at the same time
  std::list<Para_Decl::Base*>::iterator j = b.paras.begin();
  for (std::list<Para_Decl::Base*>::iterator i = a.paras.begin();
       i != a.paras.end(); ++i, ++j) {
    // try if parameter is a single track (Simple) parameter
    Para_Decl::Simple *p = dynamic_cast<Para_Decl::Simple*>(*i);
    if (p) {
      Para_Decl::Simple *u = dynamic_cast<Para_Decl::Simple*>(*j);
      assert(u);

      assert(p->type()->is_terminal() == u->type()->is_terminal());


      if (p->type()->is_terminal()) {
        // type is a terminal and terminals are the same
        assert(p->type()->is_eq(*u->type()));
        types.push_back(p->type());
        std::string *n = new std::string("p_" + *p->name());
        names.push_back(n);
        paras.push_back(new Para_Decl::Simple(p->type(), n));
      } else  {
        // type is single track variable (non-terminal)
        Type::Base *t = new Type::Tuple(p->type(), u->type());
        types.push_back(t);
        std::string *n = new std::string("p_" + *p->name());
        names.push_back(n);
        paras.push_back(new Para_Decl::Simple(t, n));
      }
    } else {
      // parameter has to be a multi track parameter
      Para_Decl::Multi *p = dynamic_cast<Para_Decl::Multi*>(*i);
      assert(p);
      Para_Decl::Multi *u = dynamic_cast<Para_Decl::Multi*>(*j);
      assert(u);

      std::list<Para_Decl::Simple*> s;

      std::list<Type::Base*> l;

      // loop over both parameter tracks at the same time and add content
      // of each track
      std::list<Para_Decl::Simple*>::const_iterator b = u->list().begin();
      for (std::list<Para_Decl::Simple*>::const_iterator a = p->list().begin();
           a != p->list().end(); ++a, ++b)
        if ((*a)->type()->is_terminal()) {
          // a,b are terminal and terminals are the same
          assert((*a)->type()->is_eq(*(*b)->type()));
          l.push_back((*a)->type());
          s.push_back(
            new Para_Decl::Simple((*a)->type(),
            new std::string("p_" + *(*a)->name())));
        } else {
          // a,b are variables (non-terminal)
          Type::Base *t = new Type::Tuple((*a)->type(), (*b)->type());
          l.push_back(t);
          s.push_back(
            new Para_Decl::Simple(t, new std::string("p_" + *(*a)->name())));
        }

      Type::Base *t = new Type::Multi(l);
      types.push_back(t);
      std::string *n = new std::string("p_MULTI");
      names.push_back(n);
      paras.push_back(new Para_Decl::Multi(s));
    }
  }

  name = a.name;

  assert(a.ntparas_.size() == b.ntparas_.size());
  assert(a.nttypes_.size() == b.nttypes_.size());
  ntparas_ = a.ntparas_;
  nttypes_ = a.nttypes_;
}

// create definition from declaration
Fn_Def::Fn_Def(const Fn_Decl &other)
  : Fn_Decl() {
  return_type = other.return_type;
  types = other.types;
  name = other.name;
  choice_fn = other.is_Choice_Fn();
  adaptor = NULL;
  comparator = NULL;
  sorter = NULL;
  nullary_sort_ob = NULL;

  gen_type = STANDARD;
  comperator_suffix = new std::string("_comperator");
  sorter_suffix = new std::string("_sorter");

  // creates names as simple index numerations with a prefix
  std::string pref("param_");
  for (unsigned int i = 0; i < types.size(); i++) {
    std::ostringstream o;
    o << pref << i;
    names.push_back(new std::string(o.str()));
  }

  // loop over names and types simultaneously
  std::list<std::string*>::iterator j = names.begin();
  for (std::list<Type::Base*>::iterator i = types.begin();
       i != types.end(); ++i, ++j) {
    if ((*i)->is(Type::MULTI)) {  // is this a multi track?
      Type::Multi *m = dynamic_cast<Type::Multi*>(*i);
      std::list<Para_Decl::Simple*> l;
      unsigned track = 0;

      // loop over types while keeping a track counter
      for (std::list<Type::Base*>::const_iterator k = m->types().begin();
           k != m->types().end(); ++k, ++track) {
        std::ostringstream o;
        o << **j << "_" << track;
        l.push_back(new Para_Decl::Simple(*k, new std::string(o.str())));
      }
      // add parameter as a multitrack of all encountered types
      paras.push_back(new Para_Decl::Multi(l));
    } else {  // single track
      paras.push_back(new Para_Decl::Simple(*i, *j));
    }
  }

  // copy non-terminal types over
  nttypes_ = other.nttypes();

  // create non-terminal parameters from types (named by indexing them)
  unsigned a = 0;
  for (std::list<Type::Base*>::iterator i = nttypes_.begin();
       i != nttypes_.end(); ++i, ++a) {
    std::ostringstream o;
    o << "ntp_" << a;
    ntparas_.push_back(new Para_Decl::Simple(*i, new std::string(o.str())));
  }
}


void Fn_Def::set_paras(const std::list<Para_Decl::Base*> &l) {
  // add parameters to list
  paras.insert(paras.end(), l.begin(), l.end());

  // loop over new parameters and add their types and names to the lists
  for (std::list<Para_Decl::Base*>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    Para_Decl::Simple *p = dynamic_cast<Para_Decl::Simple*>(*i);

    if (!p) {
      Para_Decl::Multi *m = dynamic_cast<Para_Decl::Multi*>(*i);
      assert(m);

      types.push_back(m->type());
      names.push_back(new std::string("MULTI"));

      continue;
    }
    types.push_back(p->type());
    names.push_back(p->name());
    hashtable<std::string, Type::Base*>::iterator j =
      parameters.find(*p->name());
    if (j != parameters.end())
      Log::instance()->error(p->location(),
          "Parameter redefined in definition of function"
          + *name + ".");
    else
      parameters[*p->name()] = p->type();
  }
}

// copy over terminal status from given function to current function
void Fn_Def::annotate_terminal_arguments(Fn_Decl &d) {
  assert(types_equal(d));

  // loop over all types of the given function together with types of the
  // current function
  std::list<Type::Base*>::iterator j = types.begin();
  for (std::list<Type::Base*>::iterator i = d.types.begin();
       i != d.types.end() && j != types.end(); ++i, ++j) {
    // new type is terminal, set current terminal
    if ((*i)->is_terminal())
      (*j)->set_terminal();

    if ((*i)->is(Type::MULTI)) {
      Type::Multi *a = dynamic_cast<Type::Multi*>(*i);
      assert(a);
      Type::Multi *b = dynamic_cast<Type::Multi*>(*j);
      assert(b);
      std::list<Type::Base*>::const_iterator l = b->types().begin();
      for (std::list<Type::Base*>::const_iterator k = a->types().begin();
          k != a->types().end(); ++k, ++l)
        if ((*k)->is_terminal()) {
          (*l)->set_terminal();
        }
    }
  }
}

// add new parameter to all lists
void Fn_Def::add_para(Type::Base *type, std::string *n) {
  names.push_back(n);
  types.push_back(type);
  hashtable<std::string, Type::Base*>::iterator i = parameters.find(*n);
  if (i != parameters.end())
    std::cerr << "Parameter already there: " << *n << '\n';

  assert(i == parameters.end());
  parameters[*n] = type;
  paras.push_back(new Para_Decl::Simple(type, n));
}

void Fn_Def::add_paras(const std::list<Statement::Var_Decl*> &l) {
  for (std::list<Statement::Var_Decl*>::const_iterator i = l.begin();
       i != l.end();
       ++i)
    add_para((*i)->type, (*i)->name);
}

#include "symbol.hh"
#include "expr/vacc.hh"
#include "var_acc.hh"

// add a nonterminal
void Fn_Def::add_para(Symbol::NT &nt) {
  Type::Base *t = new Type::Size();

  const std::vector<Table> &tables = nt.tables();
  std::vector<Expr::Base*> &left = nt.left_indices;
  std::vector<Expr::Base*> &right = nt.right_indices;

  std::vector<Expr::Base*>::iterator j = left.begin();
  std::vector<Expr::Base*>::iterator k = right.begin();
  for (std::vector<Table>::const_iterator i = tables.begin();
       i != tables.end(); ++i, ++j, ++k) {
    // only add the
    if (!(*i).delete_left_index())
      add_para(t, (*j)->vacc()->name());
    if (!(*i).delete_right_index())
      add_para(t, (*k)->vacc()->name());
  }

  set_paras(nt.ntargs());
}

void Fn_Def::set_statements(const std::list<Statement::Base*> &l) {
  stmts = l;
}

void Fn_Def::init_var_decl(Para_Decl::Simple *a, Para_Decl::Simple *b,
    Para_Decl::Simple *c,
    const std::string &o1, const std::string &o2) {
    Statement::Var_Decl *v =
      new Statement::Var_Decl(a->type(), new std::string(o1));
    Statement::Var_Decl *w =
      new Statement::Var_Decl(b->type(), new std::string(o2));
    Type::Base *type = c->type();
    if (type->is_terminal()) {
      v->set_rhs(new Expr::Vacc(c->name()));
      w->set_rhs(new Expr::Vacc(c->name()));
    } else {
      Type::Tuple *t = dynamic_cast<Type::Tuple*>(type->simple());
      assert(t);
      assert(t->list.size() == 2);
      std::list<std::pair<Type::Name*, std::string*>*>::iterator list =
        t->list.begin();
      std::pair<Type::Name*, std::string*> *lname = *list;
      ++list;
      std::pair<Type::Name*, std::string*> *rname = *list;
      v->set_rhs(new Expr::Vacc(c->name(), lname->second));
      w->set_rhs(new Expr::Vacc(c->name(), rname->second));
    }
    v_list.push_back(v);
    w_list.push_back(w);
}

void Fn_Def::init_var_decls(Fn_Def &a, Fn_Def &b) {
  assert(a.paras.size() == b.paras.size());
  assert(a.paras.size() == paras.size());
  unsigned int z = 0;
  std::list<Para_Decl::Base*>::iterator x = paras.begin();
  std::list<Para_Decl::Base*>::iterator j = b.paras.begin();
  for (std::list<Para_Decl::Base*>::iterator i = a.paras.begin();
      i != a.paras.end(); ++i, ++j, ++x, ++z) {
    Para_Decl::Simple *p = dynamic_cast<Para_Decl::Simple*>(*i);
    if (p) {
      Para_Decl::Simple *u = dynamic_cast<Para_Decl::Simple*>(*j);
      assert(u);
      Para_Decl::Simple *v = dynamic_cast<Para_Decl::Simple*>(*x);
      assert(v);

      std::ostringstream o1;
      o1 << "l_" << z;
      std::ostringstream o2;
      o2 << "r_" << z;
      init_var_decl(p, u, v, o1.str(), o2.str());
    } else {
      Para_Decl::Multi *p = dynamic_cast<Para_Decl::Multi*>(*i);
      assert(p);
      Para_Decl::Multi *u = dynamic_cast<Para_Decl::Multi*>(*j);
      assert(u);
      Para_Decl::Multi *v = dynamic_cast<Para_Decl::Multi*>(*x);
      assert(v);

      unsigned y = 0;
      std::list<Para_Decl::Simple*>::const_iterator m = v->list().begin();
      std::list<Para_Decl::Simple*>::const_iterator l = u->list().begin();
      for (std::list<Para_Decl::Simple*>::const_iterator k = p->list().begin();
          k != p->list().end(); ++k, ++l, ++m, ++y) {
        std::ostringstream o1;
        o1 << "l_" << z << "_" << y;
        std::ostringstream o2;
        o2 << "r_" << z << "_" << y;
        init_var_decl(*k, *l, *m, o1.str(), o2.str());
      }
    }
  }
}

void Fn_Def::codegen() {
  adaptor = new Fn_Def(*this);
  adaptor->stmts.clear();
  init_range_iterator();
}

void Fn_Def::codegen(Fn_Def &a, Fn_Def &b, Product::Two &product) {
  assert(stmts.empty());
  if (choice_fn) {
    codegen_choice(a, b, product);
    return;
  }

  init_var_decls(a, b);
  stmts.insert(stmts.end(), v_list.begin(), v_list.end());
  stmts.insert(stmts.end(), w_list.begin(), w_list.end());
  Expr::Fn_Call *left_fn = new Expr::Fn_Call(new std::string(a.target_name()));
  Expr::Fn_Call *right_fn = new Expr::Fn_Call(new std::string(b.target_name()));
  std::list<Statement::Var_Decl*>::iterator j = w_list.begin();
  for (std::list<Statement::Var_Decl*>::iterator i = v_list.begin();
      i != v_list.end() && j != w_list.end(); ++i, ++j) {
    left_fn->add_arg(**i);
    right_fn->add_arg(**j);
  }

  left_fn->add(a.ntparas_);
  right_fn->add(a.ntparas_);

  Statement::Var_Decl *ret_left =
    new Statement::Var_Decl(a.return_type,
        new std::string("ret_left"), left_fn);
  stmts.push_back(ret_left);
  Statement::Var_Decl *ret_right =
    new Statement::Var_Decl(b.return_type,
        new std::string("ret_right"), right_fn);
  stmts.push_back(ret_right);
  Statement::Var_Decl *ret = new Statement::Var_Decl(return_type,
      new std::string("ret"));
  stmts.push_back(ret);
  Statement::Var_Assign *left_ass =
    new Statement::Var_Assign(ret->left(), *ret_left);
  stmts.push_back(left_ass);
  Statement::Var_Assign *right_ass =
    new Statement::Var_Assign(ret->right(), *ret_right);
  stmts.push_back(right_ass);
  Statement::Return *r = new Statement::Return(*ret);
  stmts.push_back(r);
}

void Fn_Def::init_fn_suffix(const std::string &s) {
  target_name_ = *name + s;
}

void Fn_Def::init_range_iterator(Fn_Def &a, Fn_Def &b, Product::Two &product) {
    adaptor = new Fn_Def(*this);
    adaptor->stmts.clear();

    adaptor->comparator = NULL;
    adaptor->sorter = NULL;

    if (product.is_sorted_choice() && gen_type != CHOICE_SPECIALIZATION) {
        Statement::Var_Decl *input_list = new Statement::Var_Decl(
            types.front(), names.front(), new Expr::Vacc(names.front()));

        if (gen_type == STANDARD) {
            // the sorter, using sorter struct
            Statement::Sorter *s = new Statement::Sorter(sorter, input_list);
            adaptor->stmts.push_back(s);
        } else {
            std::string *name = new std::string(*nullary_sort_ob);
            name->append(*sorter_suffix);

            Statement::Sorter *s = new Statement::Sorter(name, input_list);
            adaptor->stmts.push_back(s);
        }
    }

    init_range_iterator();
}

void Fn_Def::init_range_iterator() {
  adaptor->name = name;
  adaptor->names = names;
  adaptor->types = types;
  Type::Base *t = 0;
  if (types.front()->is(Type::LIST))
    t = types.front()->component();
  else
    t = types.front();
  Type::Range *range = new Type::Range(t);
  types.clear();
  types.push_back(range);
  Expr::Fn_Call *get_range = new Expr::Fn_Call(Expr::Fn_Call::GET_RANGE);
  get_range->add_arg(names.front());
  Statement::Var_Decl *v = new Statement::Var_Decl(range,
      "range", get_range);

  adaptor->stmts.push_back(v);

  Expr::Fn_Call *fn = new Expr::Fn_Call(&target_name_);
  fn->add_arg(*v);
  Statement::Return *ret = new Statement::Return(fn);
  adaptor->stmts.push_back(ret);
}

// generates struct for comparing at a specific dimension
void Fn_Def::init_comparator_adaptor() {
    std::string *name = new std::string(target_name_);
    name->append(*comperator_suffix);

    comparator = new Operator(new Type::Int(), name);

    comparator->add_para(return_type->component(), new std::string("e1"));
    comparator->add_para(return_type->component(), new std::string("e2"));
    comparator->add_para(new Type::Int(), new std::string("d"));
}

// generates struct for comparing all dims at once
void Fn_Def::init_sorter_adaptor() {
    std::string *name = new std::string(target_name_);
    name->append(*sorter_suffix);

    sorter = new Operator(new Type::Bool(), name);

    sorter->add_para(return_type->component(), new std::string("c1"));
    sorter->add_para(return_type->component(), new std::string("c2"));
}


bool Fn_Def::is_pareto_instance(Product::Base &product) {
    switch (product.type()) {
    case Product::SINGLE:
        return false;
    case Product::PARETO:
        return true;
    default:
        bool left = is_pareto_instance(*product.left());
        bool right = is_pareto_instance(*product.right());
        return left || right;
      break;
    }

    return false;
}

int Fn_Def::codegen_compare(Product::Base &product) {
  // create the adaptor
  init_comparator_adaptor();

  // the comparator always needs to get the parameters to variables
  Statement::Var_Decl *c1 = new Statement::Var_Decl(
    return_type->component(), "c1", new Expr::Vacc(new std::string("e1")));
  comparator->stmts.push_back(c1);

  Statement::Var_Decl *c2 = new Statement::Var_Decl(
    return_type->component(), "c2", new Expr::Vacc(new std::string("e2")));
  comparator->stmts.push_back(c2);

  Statement::Var_Decl *dim = new Statement::Var_Decl(
    new Type::Int(), "dim", new Expr::Vacc(new std::string("d")));

  comparator->stmts.push_back(dim);

  int dimension;
  if ((product.sort_product &&  is_pareto_instance(*product.sort_product)) ||
    is_pareto_instance(product)) {
    Product::Two prod = codegen_pareto_move_to_first_all_dim(
      c1, c2, &comparator->stmts, product);
    dimension = codegen_pareto_comparator_all_dim(
      c1, c2, dim, *comparator, prod);
  } else {
    dimension = 1;
    Log::instance()->error("Non-Pareto Compare not implemented yet!");
  }

  std::ostringstream D_str;
  D_str << dimension;

  Statement::Var_Decl *dimInt = new Statement::Var_Decl(
  new Type::Int(), "dim", new Expr::Vacc(new std::string(D_str.str())));

  comparator->add_const_value(dimInt);

  return dimension;
}

void Fn_Def::codegen_sorter(Product::Base &product) {
  // create the adaptor
  init_sorter_adaptor();

  if ((product.sort_product &&  is_pareto_instance(*product.sort_product)) ||
      is_pareto_instance(product)) {
     codegen_multi_sort(product, &sorter->stmts);
  } else {
     Log::instance()->error("Non-Pareto Compare not implemented yet!");
  }
}


void Fn_Def::add_simple_choice_fn_adaptor() {
  assert(adaptor);
  types = adaptor->types;
  adaptor = 0;
  stmts.push_front(new Statement::Return(names.front()));
}

void Fn_Def::codegen_sorting_nullary(Product::Two &product) {
    std::string *name = new std::string(*nullary_sort_ob);
    name->append(*sorter_suffix);

    Statement::Var_Decl *input_list = new Statement::Var_Decl(
        types.front(), names.front() /*"input_list"*/,
        new Expr::Vacc(names.front()));

    // the sorter, using sorter struct
    Statement::Sorter *s = new Statement::Sorter(name, input_list);
    stmts.push_back(s);

    codegen_nop(product);
}


void Fn_Def::codegen_choice(Fn_Def &a, Fn_Def &b, Product::Two &product) {
  // for specialized ADP or Sorted Pareto, generate a comparator on the
  // original choice function (only one is needed to original is chosen for
  // naming, no other reasons)
  if (product.get_sorted_choice() != Product::NONE && gen_type == STANDARD) {
     switch (product.get_sorted_choice()) {
         case Product::STANDARD:
         case Product::MULTI:
         case Product::SORTER:
         case Product::NULLARY_SORTER:
              codegen_sorter(product);
              break;
         case Product::COMPERATOR:
         case Product::NULLARY_COMPERATOR:
              codegen_compare(product);
             break;
         case Product::COMPERATOR_SORTER:
         case Product::NULLARY_COMPERATOR_SORTER:
              codegen_sorter(product);
              codegen_compare(product);
              break;
         default:
             break;
     }
  }

  if (gen_type == NULLARY &&
      (product.get_adp_specialization() == ADP_Mode::SORTED_STEP ||
       product.get_adp_specialization() == ADP_Mode::SORTED_BLOCK)) {
      if (product.get_sorted_choice() != Product::NONE) {
          codegen_sorting_nullary(product);
      } else {
          codegen_nop(product);
      }
      return;
  }


  if (gen_type == STANDARD &&
      (product.get_adp_specialization() == ADP_Mode::PARETO_EAGER_STEP ||
       product.get_adp_specialization() == ADP_Mode::PARETO_EAGER_BLOCK)) {
      codegen_nop(product);
      return;
  }

  if (!product.is(Product::NOP))
    init_range_iterator(a, b, product);

  Product::Pareto* p;
  switch (product.type()) {
    case Product::TIMES :
      codegen_times(a, b, product);
      break;
    case Product::NOP :
      codegen_nop(product);
      break;
    case Product::CARTESIAN :
      codegen_cartesian(a, b, product);
      break;
    case Product::TAKEONE :
      codegen_takeone(a, b, product);
      break;
    case Product::PARETO:
      p = dynamic_cast<Product::Pareto*>(&product);
      switch (p->get_pareto_type()) {
          case Product::Pareto::NoSort:
              if (p->get_multi_dim()) {
                 codegen_pareto_multi_nosort(a, b, product);
              } else {
                 codegen_pareto_nosort(a, b, product);
              }
              break;
          case Product::Pareto::Sort:
              if (p->get_multi_dim()) {
                  codegen_pareto_multi_lex(a, b, product);
              } else {
                  codegen_pareto_lex(a, b, product);
              }

              break;
          case Product::Pareto::ISort:
              codegen_pareto_isort(a, b, product);
              break;
          case Product::Pareto::NoSortDomOpt:
              codegen_compare(product);
              codegen_pareto_domination_nosort(a, b, product);
              break;
          case Product::Pareto::MultiDimOpt:
              int dim = codegen_compare(product);
              codegen_pareto_multi_yukish(a, b, product, p->get_cutoff(), dim);
              break;
      }
      break;
    default:
      assert(false);
  }
}

void Fn_Def::get_pareto_dimensions(
  Product::Base &product, std::list<Statement::Base*> &base,
  int *i, int *D, Statement::Var_Decl *last_decl, std::string prefix,
  std::list<std::pair<Product::Base*, bool> > &products,
  std::list<Statement::Var_Decl*> &decls, int float_acc) {
    if (product.left()->type() == Product::PARETO) {
        std::ostringstream temp_str;
        temp_str << prefix << "_temp_" << (*i+1);
        Statement::Var_Decl *t_vardecl = new Statement::Var_Decl(
          last_decl->type->left() , temp_str.str(),
          new Expr::Vacc(last_decl->left()));
        base.push_back(t_vardecl);
        (*i)++;
        get_pareto_dimensions(
          *product.left(), base, i, D, t_vardecl,
          prefix, products, decls, float_acc);
    } else {
        std::ostringstream temp_str;
        temp_str << prefix << "_dim_" << (*D+1);

        Statement::Var_Decl *t_vardecl;

        // switch to make float roundings
        if (last_decl->type->left()->is(Type::FLOAT) && float_acc != 0) {
            Expr::Fn_Call *round = new Expr::Fn_Call(
              Expr::Fn_Call::ROUND_TO_DIGIT);

            std::ostringstream offset;
            offset << static_cast<int>(std::pow(10, float_acc));

            round->add_arg( new std::string(offset.str()) );
            round->add_arg( new Expr::Vacc(last_decl->left()));

            t_vardecl = new Statement::Var_Decl(
              last_decl->type->left(), temp_str.str(), round);
        } else {
            t_vardecl = new Statement::Var_Decl(
              last_decl->type->left(), temp_str.str(),
              new Expr::Vacc(last_decl->left()));
        }

        (*D)++;

        base.push_back(t_vardecl);
        decls.push_back(t_vardecl);
        products.push_back(std::make_pair(&product, true));
    }
    if (product.right()->type() == Product::PARETO) {
        std::ostringstream temp_str;
        temp_str << prefix << "_temp_" << (*i+1);
        Statement::Var_Decl *t_vardecl = new Statement::Var_Decl(
          last_decl->type->right() , temp_str.str(),
          new Expr::Vacc(last_decl->right()));
        base.push_back(t_vardecl);
        (*i)++;
        get_pareto_dimensions(
          *product.right(), base, i, D, t_vardecl, prefix,
          products, decls, float_acc);
    } else {
        std::ostringstream temp_str;
        temp_str << prefix << "_dim_" << (*D+1);

        Statement::Var_Decl *t_vardecl;
        if (last_decl->type->right()->is(Type::FLOAT) && float_acc != 0) {
            Expr::Fn_Call *round = new Expr::Fn_Call(
              Expr::Fn_Call::ROUND_TO_DIGIT);

            std::ostringstream offset;
            offset << static_cast<int>(std::pow(10, float_acc));

            round->add_arg( new std::string(offset.str()) );
            round->add_arg( new Expr::Vacc(last_decl->right()));

            t_vardecl = new Statement::Var_Decl(
              last_decl->type->right(), temp_str.str(), round);
        } else {
            t_vardecl = new Statement::Var_Decl(
              last_decl->type->right(), temp_str.str(),
              new Expr::Vacc(last_decl->right()));
        }

        base.push_back(t_vardecl);
        (*D)++;

        decls.push_back(t_vardecl);
        products.push_back(std::make_pair(&product, false));
    }
}



bool Fn_Def::get_sort_grab_list(std::list<bool> &o, Product::Base &product) {
    switch (product.type()) {
    case Product::SINGLE:
        return false;
    case Product::PARETO:
        if (!o.empty()) {
          Log::instance()->error(
            "Two pareto products found. No global sorting can be generated.");
        }
        o.push_back(true);
        return true;
    default:
        bool left = get_sort_grab_list(o, *product.left());
        bool right = get_sort_grab_list(o, *product.right());
        if (left) {
            o.push_front(true);
            return true;
        } else if (right) {
            o.push_front(false);
            return true;
        }
      break;
    }

  return false;
}

#include "statement/fn_call.hh"

void Fn_Def::codegen_multi_sort(
  Product::Base &product, std::list<Statement::Base*> *stmts) {
  // list elements
  Statement::Var_Decl *c1 = new Statement::Var_Decl(
    return_type->component(), "c1");
  Statement::Var_Decl *c2 = new Statement::Var_Decl(
    return_type->component(), "c2");

  Product::Two prod = codegen_pareto_move_to_first_all_dim(
    c1, c2 , stmts, product);

  int i = 0;
  int D = 0;
  std::list<std::pair<Product::Base*, bool> > c_1_products;
  std::list<Statement::Var_Decl*> c_1_decl;
  get_pareto_dimensions(
    prod, *stmts, &i, &D, c1, std::string("c1"), c_1_products,
    c_1_decl, product.get_float_accuracy() );

  i = 0;
  D = 0;
  std::list<std::pair<Product::Base*, bool> > c_2_products;
  std::list<Statement::Var_Decl*> c_2_decl;
  get_pareto_dimensions(
    prod, *stmts, &i, &D, c2, std::string("c2"), c_2_products,
    c_2_decl, product.get_float_accuracy() );

  std::list<Statement::Base*> *cur_stmts = stmts;

  int dim = 1;
  std::list<Statement::Var_Decl*>::iterator it_c1 = c_1_decl.begin();
  std::list<Statement::Var_Decl*>::iterator it_c2 = c_2_decl.begin();
  for (std::list<std::pair<Product::Base*, bool> >::iterator it =
      c_1_products.begin();
      it != c_1_products.end(); ++it, ++it_c1, ++it_c2, ++dim) {
    Statement::Var_Decl *u = *it_c1;
    Statement::Var_Decl *x = *it_c2;

    std::pair<Product::Base*, bool> pairt = *it;
    Product::Two prod = *dynamic_cast<Product::Two*>(pairt.first);
    bool left = pairt.second;

    std::list<Statement::Base*> *insert = cur_stmts;
    if (dim < D) {
        Statement::If *if_case_equal = new Statement::If(
          new Expr::Eq( new Expr::Vacc(*u) , new Expr::Vacc(*x)));
        cur_stmts->push_back(if_case_equal);
        cur_stmts = &if_case_equal->then;
        insert = &if_case_equal->els;
    }

    Type::Base *type;
    if (left) {
       type = u->type;
    } else {
       type = x->type;
    }

    Statement::Var_Decl *answer = new Statement::Var_Decl(type, "answer");
    insert->push_back(answer);
    if ((left && prod.left_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM) ||
        (!left && prod.right_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM)) {
      Statement::If *if_case_min = new Statement::If(new Expr::Less(
        new Expr::Vacc(*u) , new Expr::Vacc(*x)));
      insert->push_back(if_case_min);
      Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
      if_case_min->then.push_back(la);
      Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
      if_case_min->els.push_back(la2);
    } else if ((left &&
                prod.left_choice_fn_type(*name) == Expr::Fn_Call::MAXIMUM) ||
               (!left && prod.right_choice_fn_type(*name) ==
                Expr::Fn_Call::MAXIMUM)) {
      Statement::If *if_case_min = new Statement::If(
        new Expr::Greater( new Expr::Vacc(*u) , new Expr::Vacc(*x)));
      insert->push_back(if_case_min);
      Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
      if_case_min->then.push_back(la);
      Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
      if_case_min->els.push_back(la2);
    } else {
      Statement::Var_Decl *candidates = new Statement::Var_Decl(
        new Type::List(type), "candidates");
      insert->push_back(candidates);
      insert->push_back(new Statement::Fn_Call(
        Statement::Fn_Call::EMPTY, *candidates));

      Statement::Fn_Call *push_backu = new Statement::Fn_Call(
        Statement::Fn_Call::PUSH_BACK);
      push_backu->add_arg(*candidates);
      push_backu->add_arg(*u);
      insert->push_back(push_backu);

      Statement::Fn_Call *push_backx = new Statement::Fn_Call(
        Statement::Fn_Call::PUSH_BACK);
      push_backx->add_arg(*candidates);
      push_backx->add_arg(*x);
      insert->push_back(push_backx);

      Fn_Def c;
      if (left) {
          c = *prod.left_choice_function(*name);
      } else {
          c = *prod.right_choice_function(*name);
      }

      Expr::Fn_Call *h = new Expr::Fn_Call(new std::string(c.target_name()));
      h->add_arg(*candidates);

      if (prod.left_mode(*name).number == Mode::ONE) {
          Statement::Var_Assign* la = new Statement::Var_Assign(*answer, h);
          insert->push_back(la);
      } else {
         Statement::Var_Decl *answer_list = new Statement::Var_Decl(
           c.return_type, "answer_list", h);

         Expr::Fn_Call *first = new Expr::Fn_Call(Expr::Fn_Call::GET_FRONT);
         first->add_arg(*answer_list);

         Statement::Var_Assign* la = new Statement::Var_Assign(*answer, first);
         insert->push_back(answer_list);
         insert->push_back(la);
      }
    }

    Statement::Return *sort_ret = new Statement::Return(
      new Expr::And(new Expr::Eq(new Expr::Vacc(*u) , new Expr::Vacc(*answer)),
      new Expr::Not_Eq(new Expr::Vacc(*x) , new Expr::Vacc(*answer))));
    insert->push_back(sort_ret);
  }
}


void Fn_Def::codegen_times(Fn_Def &a, Fn_Def &b, Product::Two &product) {
  Statement::Var_Decl *answers = new Statement::Var_Decl(
    return_type, "answers");
  stmts.push_back(answers);
  stmts.push_back(new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *answers));

  Expr::Fn_Call *splice_left = new Expr::Fn_Call(Expr::Fn_Call::SPLICE_LEFT);
  splice_left->add_arg(names.front());

  Statement::Var_Decl *left = new Statement::Var_Decl(
    new Type::Range(
      return_type->left(), return_type, Type::Range::LEFT),
    "left", splice_left);
  stmts.push_back(left);

  Expr::Fn_Call *h_left = new Expr::Fn_Call(new std::string(a.target_name()));
  h_left->add_arg(*left);

  Statement::Var_Decl *left_answers = new Statement::Var_Decl(
      a.return_type, "left_answers", h_left);
  stmts.push_back(left_answers);

  Expr::Fn_Call *isEmpty = new Expr::Fn_Call(Expr::Fn_Call::IS_EMPTY);
  isEmpty->add_arg(*left_answers);
  Statement::If *if_empty =
    new Statement::If(isEmpty);
  stmts.push_back(if_empty);
  Statement::Var_Decl *temp = new Statement::Var_Decl(return_type, "temp");
  if_empty->then.push_back(temp);
  if_empty->then.push_back(
      new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *temp));
  if_empty->then.push_back(new Statement::Fn_Call(Statement::Fn_Call::ERASE,
        *left_answers));
  if_empty->then.push_back(new Statement::Return(*temp));

  Statement::Var_Decl *elem = new Statement::Var_Decl(
      return_type->left(), "elem");

  std::list<Statement::Base*> *loop_body = &stmts;
  if (product.left_mode(*name).number == Mode::ONE) {
    stmts.push_back(elem);
    elem->rhs = new Expr::Vacc(*left_answers);
  } else {
    Statement::Foreach *loop1 = new Statement::Foreach(elem, left_answers);
    loop1->set_itr(true);
    stmts.push_back(loop1);
    loop_body = &loop1->statements;
  }

  if (b.choice_mode() == Mode::PRETTY) {
    times_cg_without_rhs_choice(a, b, product, answers, loop_body, elem);
  } else {
    times_cg_with_rhs_choice(a, b, product, answers, loop_body, elem);
  }

  Statement::Return *ret = new Statement::Return(*answers);
  stmts.push_back(ret);
}

void Fn_Def::times_cg_with_rhs_choice(
  Fn_Def &a, Fn_Def &b, Product::Two &product,
  Statement::Var_Decl *answers, std::list<Statement::Base*> *loop_body,
  Statement::Var_Decl *elem) {
  Statement::Var_Decl *right_candidates = new Statement::Var_Decl(
      new Type::List(return_type->right()), "right_candidates");
  loop_body->push_back(right_candidates);
  loop_body->push_back(
      new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *right_candidates));

  Statement::Var_Decl *input_list = new Statement::Var_Decl(
      types.front(), names.front() /*"input_list"*/,
      new Expr::Vacc(names.front()));
  // loop_body->push_back(input_list);
  Statement::Var_Decl *tupel = new Statement::Var_Decl(return_type->component(),
      "tupel");
  // loop_body->push_back(tupel);

  Statement::Foreach *loop2 = new Statement::Foreach(tupel, input_list);
  loop2->set_itr(true);
  loop_body->push_back(loop2);
  std::list<Statement::Base*> *loop_body2 = &loop2->statements;

  Statement::If *if_eq = new Statement::If(new Expr::Eq(tupel->left(), elem));
  loop_body2->push_back(if_eq);

  Statement::Fn_Call *push_back =
    new Statement::Fn_Call(Statement::Fn_Call::PUSH_BACK);
  push_back->add_arg(*right_candidates);
  push_back->add_arg(tupel->right());
  if_eq->then.push_back(push_back);

  Expr::Fn_Call *h_right = new Expr::Fn_Call(new std::string(b.target_name()));
  h_right->add_arg(*right_candidates);

  Statement::Var_Decl *right_answers = new Statement::Var_Decl(
      b.return_type, "right_answers", h_right);
  loop_body->push_back(right_answers);

  Statement::Var_Decl *right_elem = new Statement::Var_Decl(
      return_type->right(), "right_elem");

  std::list<Statement::Base*> *loop_body3 = loop_body;
  if (product.right_mode(*name).number == Mode::ONE) {
    loop_body->push_back(right_elem);
    Statement::Var_Assign *t =
      new Statement::Var_Assign(*right_elem, *right_answers);
    loop_body3->push_back(t);
  } else {
    Statement::Foreach *loop3 =
      new Statement::Foreach(right_elem, right_answers);
    loop3->set_itr(true);
    loop_body3->push_back(loop3);
    loop_body3 = &loop3->statements;
  }

  Statement::Fn_Call *pb =
    new Statement::Fn_Call(Statement::Fn_Call::PUSH_BACK);

  Statement::Var_Decl *temp_elem = new Statement::Var_Decl(
      return_type->component(), "temp_elem");
  loop_body3->push_back(temp_elem);
  Statement::Var_Assign *l_ass = new Statement::Var_Assign(temp_elem->left(),
      *elem);
  loop_body3->push_back(l_ass);
  Statement::Var_Assign *r_ass = new Statement::Var_Assign(temp_elem->right(),
      *right_elem);
  loop_body3->push_back(r_ass);
  pb->add_arg(*answers);
  pb->add_arg(*temp_elem);

  if (mode_.number == Mode::ONE) {
    Statement::Var_Assign *t = new Statement::Var_Assign(*answers, *temp_elem);
    loop_body3->push_back(t);
  } else {
    loop_body3->push_back(pb);
  }
}

void Fn_Def::times_cg_without_rhs_choice(
  Fn_Def &a, Fn_Def &b, Product::Two &product,
  Statement::Var_Decl *answers, std::list<Statement::Base*> *loop_body,
  Statement::Var_Decl *elem) {
  Statement::Var_Decl *input_list = new Statement::Var_Decl(
      types.front(), names.front(), new Expr::Vacc(names.front()));
  Statement::Var_Decl *tupel =
    new Statement::Var_Decl(return_type->component(), "tupel");

  Statement::Foreach *loop2 = new Statement::Foreach(tupel, input_list);
  loop2->set_itr(true);
  loop_body->push_back(loop2);
  std::list<Statement::Base*> *loop_body2 = &loop2->statements;

  Statement::If *if_eq = new Statement::If(new Expr::Eq(tupel->left(), elem));
  loop_body2->push_back(if_eq);

  switch (product.type()) {
    case Product::TIMES: {
      Statement::Fn_Call *push_back =
        new Statement::Fn_Call(Statement::Fn_Call::PUSH_BACK);
      push_back->add_arg(*answers);
      push_back->add_arg(*tupel);
      if_eq->then.push_back(push_back);
      if (product.no_coopt_class())
        if_eq->then.push_back(new Statement::Break());
      }
      break;
    case Product::TAKEONE: {
      Statement::Return *ret = new Statement::Return(*tupel);
      if_eq->then.push_back(ret);
      Statement::Fn_Call *check = new Statement::Fn_Call(
          Statement::Fn_Call::ASSERT);
      check->add_arg(new Expr::Const(0));
      loop_body->push_back(check);
      }
      break;
    default:
      assert(false);
  }
}

void Fn_Def::codegen_pareto_nosort(
  Fn_Def &a, Fn_Def &b, Product::Two &product) {
  // create  a variable to put all answers in
  assert(stmts.empty());
  Statement::Var_Decl *answers = new Statement::Var_Decl(
      return_type, "answers");
  stmts.push_back(answers);
  stmts.push_back(new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *answers));

  // first main loop, loop over raw answers
  Statement::Var_Decl *input_list = new Statement::Var_Decl(
      types.front(), names.front(), new Expr::Vacc(names.front()));

  Statement::Var_Decl *tupel = new Statement::Var_Decl(
    return_type->component(), "tupel");

  Statement::Foreach *loop = new Statement::Foreach(tupel, input_list);
  loop->set_itr(true);
  stmts.push_back(loop);
  std::list<Statement::Base*> *loop_body = &loop->statements;

  // set the first variables, tuple to insert into the answer list
  Statement::Var_Decl *u, *v;
  if (return_type->left()->is(Type::FLOAT) &&
      product.get_float_accuracy() != 0) {
      Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
      std::ostringstream offset;
      offset << static_cast<int>(std::pow(10, product.get_float_accuracy()));

      round->add_arg(new std::string(offset.str()));
      round->add_arg(new Expr::Vacc(tupel->left()));

      u = new Statement::Var_Decl(return_type->left(), "u",  round);
  } else {
      u = new Statement::Var_Decl(
        return_type->left(), "u",  new Expr::Vacc(tupel->left()));
  }
  if (return_type->right()->is(Type::FLOAT) &&
      product.get_float_accuracy() != 0) {
      Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
      std::ostringstream offset;
      offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

      round->add_arg( new std::string(offset.str()) );
      round->add_arg( new Expr::Vacc(tupel->right()) );

      v = new Statement::Var_Decl(return_type->right(), "v",  round);
  } else {
      v = new Statement::Var_Decl(
        return_type->right(), "v",  new Expr::Vacc(tupel->right()));
  }

  loop_body->push_back(u);
  loop_body->push_back(v);

  // create a boolean variable
  Statement::Var_Decl *add = new Statement::Var_Decl(
    new Type::Bool() , "add", new Expr::Const(new Const::Bool(true)));
  loop_body->push_back(add);


  Statement::Var_Decl *answer = new Statement::Var_Decl(return_type, "answer");
  Statement::Var_Decl *answers_list = new Statement::Var_Decl(
      return_type, "answers", new Expr::Vacc(new std::string("answer")));

  Statement::Foreach *loop2 = new Statement::Foreach(answer, answers_list);
  loop2->set_itr(true);
  loop2->set_iteration(false);
  loop_body->push_back(loop2);
  std::list<Statement::Base*> *loop_body2 = &loop2->statements;

  Statement::Var_Decl *tmp = new Statement::Var_Decl(
    return_type->component(), "tmp",  new Expr::Vacc(*answer));
  Statement::Var_Decl *x, *y;
  if (return_type->left()->is(Type::FLOAT) &&
      product.get_float_accuracy() != 0) {
      Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
      std::ostringstream offset;
      offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

      round->add_arg( new std::string(offset.str()) );
      round->add_arg( new Expr::Vacc(tmp->left()) );

      x = new Statement::Var_Decl(return_type->left(), "x",  round);
  } else {
      x = new Statement::Var_Decl(
        return_type->left(), "x",  new Expr::Vacc(tmp->left()));
  }
  if (return_type->right()->is(Type::FLOAT) &&
      product.get_float_accuracy() != 0) {
      Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
      std::ostringstream offset;
      offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

      round->add_arg( new std::string(offset.str()) );
      round->add_arg( new Expr::Vacc(tmp->right()) );

      y = new Statement::Var_Decl(return_type->right(), "y",  round);
  } else {
      y = new Statement::Var_Decl(
        return_type->right(), "y",  new Expr::Vacc(tmp->right()));
  }

  loop_body2->push_back(tmp);
  loop_body2->push_back(x);
  loop_body2->push_back(y);


  Statement::Var_Decl *left_answer = new Statement::Var_Decl(
    return_type->left(), "left_answer");
  loop_body2->push_back(left_answer);
  if (product.left_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Less(
      new Expr::Vacc(*u) , new Expr::Vacc(*x)));
    loop_body2->push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*left_answer, *u);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*left_answer, *x);
    if_case_min->els.push_back(la2);
  } else if (product.left_choice_fn_type(*name) == Expr::Fn_Call::MAXIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Greater(
      new Expr::Vacc(*u) , new Expr::Vacc(*x)));
    loop_body2->push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*left_answer, *u);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*left_answer, *x);
    if_case_min->els.push_back(la2);
  } else {
    Statement::Var_Decl *left_candidates = new Statement::Var_Decl(
      new Type::List(return_type->left()), "left_candidates");
    loop_body2->push_back(left_candidates);
    loop_body2->push_back(new Statement::Fn_Call(
      Statement::Fn_Call::EMPTY, *left_candidates));

    Statement::Fn_Call *push_backu = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backu->add_arg(*left_candidates);
    push_backu->add_arg(*u);
    loop_body2->push_back(push_backu);

    Statement::Fn_Call *push_backx = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backx->add_arg(*left_candidates);
    push_backx->add_arg(*x);
    loop_body2->push_back(push_backx);

    Expr::Fn_Call *h_left = new Expr::Fn_Call(new std::string(a.target_name()));
    h_left->add_arg(*left_candidates);

    if (product.left_mode(*name).number == Mode::ONE) {
      Statement::Var_Assign* la = new Statement::Var_Assign(
        *left_answer, h_left);
      loop_body2->push_back(la);
    } else {
     Statement::Var_Decl *left_answer_list = new Statement::Var_Decl(
       a.return_type, "left_answer_list", h_left);

     Expr::Fn_Call *left_first = new Expr::Fn_Call(Expr::Fn_Call::GET_FRONT);
     left_first->add_arg(*left_answer_list);

     Statement::Var_Assign* la = new Statement::Var_Assign(
       *left_answer, left_first);
     loop_body2->push_back(left_answer_list);
     loop_body2->push_back(la);
    }
  }


  Statement::Var_Decl *right_answer = new Statement::Var_Decl(
    return_type->right(), "right_answer");
  loop_body2->push_back(right_answer);
  if (product.right_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Less(
      new Expr::Vacc(*v) , new Expr::Vacc(*y)));
    loop_body2->push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*right_answer, *v);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*right_answer, *y);
    if_case_min->els.push_back(la2);

  } else if (product.right_choice_fn_type(*name) == Expr::Fn_Call::MAXIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Greater(
      new Expr::Vacc(*v) , new Expr::Vacc(*y)));
    loop_body2->push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*right_answer, *v);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*right_answer, *y);
    if_case_min->els.push_back(la2);

  } else {
    Statement::Var_Decl *right_candidates = new Statement::Var_Decl(
      new Type::List(return_type->right()), "right_candidates");
    loop_body2->push_back(right_candidates);
    loop_body2->push_back(new Statement::Fn_Call(
      Statement::Fn_Call::EMPTY, *right_candidates));

    Statement::Fn_Call *push_backv = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backv->add_arg(*right_candidates);
    push_backv->add_arg(*v);
    loop_body2->push_back(push_backv);

    Statement::Fn_Call *push_backy = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backy->add_arg(*right_candidates);
    push_backy->add_arg(*y);
    loop_body2->push_back(push_backy);

    Expr::Fn_Call *h_right = new Expr::Fn_Call(
      new std::string(b.target_name()));
    h_right->add_arg(*right_candidates);

    if (product.right_mode(*name).number == Mode::ONE) {
      Statement::Var_Assign* la = new Statement::Var_Assign(
        *right_answer, h_right);
      loop_body2->push_back(la);
    } else {
       Statement::Var_Decl *right_answer_list = new Statement::Var_Decl(
         b.return_type, "right_answer_list", h_right);

       Expr::Fn_Call *right_first = new Expr::Fn_Call(Expr::Fn_Call::GET_FRONT);
       right_first->add_arg(*right_answer_list);

       Statement::Var_Assign* la = new Statement::Var_Assign(
         *right_answer, right_first);
       loop_body2->push_back(right_answer_list);
       loop_body2->push_back(la);
    }
  }

  Expr::Eq *eq_1_1 = new Expr::Eq( new Expr::Vacc(*u), new Expr::Vacc(
    *left_answer));
  Expr::Eq *eq_1_2 = new Expr::Eq( new Expr::Vacc(*v), new Expr::Vacc(
    *right_answer));

  Statement::If *if_case1 = new Statement::If(new Expr::And(eq_1_1, eq_1_2));
  loop_body2->push_back(if_case1);

  Expr::Eq *eq_2_1 = new Expr::Eq( new Expr::Vacc(*x), new Expr::Vacc(
    *left_answer));
  Expr::Eq *eq_2_2 = new Expr::Eq( new Expr::Vacc(*y), new Expr::Vacc(
    *right_answer));

  Statement::If *if_case2 = new Statement::If(new Expr::And(eq_2_1, eq_2_2));
  if_case1->els.push_back(if_case2);

  Expr::Fn_Call *erase = new Expr::Fn_Call(Expr::Fn_Call::ERASE_ELEMENT);
  erase->add_arg(*answers);
  erase->add_arg(new std::string(*answer->name));

  Statement::Var_Assign *newAnswer = new Statement::Var_Assign(*answer, erase);
  if_case1->then.push_back(newAnswer);

  Statement::Var_Assign *add_false = new Statement::Var_Assign(
    *add, new Expr::Const(new Const::Bool(false)));
  if_case2->then.push_back(add_false);

  Statement::Break *ansbreak = new Statement::Break();
  if_case2->then.push_back(ansbreak);

  Statement::Increase *increase = new Statement::Increase(answer->name);
  if_case1->els.push_back(increase);

  Statement::If *if_add = new Statement::If(new Expr::Eq(
    new Expr::Vacc(*add) , new Expr::Const(new Const::Bool(true))));

  loop_body->push_back(if_add);

  Statement::Fn_Call *pb = new Statement::Fn_Call(
    Statement::Fn_Call::PUSH_BACK);

  Statement::Var_Decl *temp_elem = new Statement::Var_Decl(
    return_type->component(), "temp_elem");
  if_add->then.push_back(temp_elem);

  Statement::Var_Assign *l_ass = new Statement::Var_Assign(
    temp_elem->left(), new Expr::Vacc(tupel->left()));
  if_add->then.push_back(l_ass);
  Statement::Var_Assign *r_ass = new Statement::Var_Assign(
    temp_elem->right(), new Expr::Vacc(tupel->right()));
  if_add->then.push_back(r_ass);
  pb->add_arg(*answers);
  pb->add_arg(*temp_elem);

  if_add->then.push_back(pb);

  Statement::Return *ret = new Statement::Return(*answers);
  stmts.push_back(ret);
}


void Fn_Def::codegen_pareto_multi_nosort(
  Fn_Def &a, Fn_Def &b, Product::Two &product) {
  // input list
  Statement::Var_Decl *input_list = new Statement::Var_Decl(
  types.front(), names.front(), new Expr::Vacc(names.front()));

  // create  a variable to put all answers in
  assert(stmts.empty());
  Statement::Var_Decl *answers = new Statement::Var_Decl(
      return_type, "answers");
  stmts.push_back(answers);
  stmts.push_back(new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *answers));

  // main loop
  Statement::Var_Decl *tupel = new Statement::Var_Decl(
    return_type->component(), "tupel");

  Statement::Foreach *loop = new Statement::Foreach(tupel, input_list);
  loop->set_itr(true);
  stmts.push_back(loop);
  std::list<Statement::Base*> *loop_body = &loop->statements;

  Statement::Var_Decl *c1 = new Statement::Var_Decl(
    return_type->component(), "c1", new Expr::Vacc(*tupel));
  loop_body->push_back(c1);

  // test if list is empty
  Expr::Fn_Call *isEmpty = new Expr::Fn_Call(Expr::Fn_Call::IS_EMPTY);
  isEmpty->add_arg(*answers);
  Statement::If *if_empty = new Statement::If(isEmpty);
  loop_body->push_back(if_empty);

  Statement::Fn_Call *pb = new Statement::Fn_Call(
    Statement::Fn_Call::PUSH_BACK);
  Statement::Var_Decl *temp_elem = new Statement::Var_Decl(
    return_type->component(), "temp_elem", new Expr::Vacc(*c1));
  if_empty->then.push_back(temp_elem);

  pb->add_arg(*answers);
  pb->add_arg(*temp_elem);

  if_empty->then.push_back(pb);

  if_empty->then.push_back(new Statement::Continue());

  // create a boolean variable if the new element is added
  Statement::Var_Decl *add = new Statement::Var_Decl(
    new Type::Bool() , "add", new Expr::Const(new Const::Bool(true)));
  loop_body->push_back(add);


  Statement::Var_Decl *ans_it = new Statement::Var_Decl(return_type, "ans_it");
  Statement::Var_Decl *answers_list = new Statement::Var_Decl(
      return_type, "answers", new Expr::Vacc(new std::string("answer")));

  Statement::Foreach *loop2 = new Statement::Foreach(ans_it, answers_list);
  loop2->set_itr(true);
  loop2->set_iteration(false);
  loop_body->push_back(loop2);

  std::list<Statement::Base*> *loop_body2 = &loop2->statements;

  // get c2 as last element of answer list
  Statement::Var_Decl *c2 = new Statement::Var_Decl(
    return_type->component(), "c2",  new Expr::Vacc(*ans_it));
  loop_body2->push_back(c2);

  // create access for all dimensions
  int i = 0;
  int D = 0;
  std::list<std::pair<Product::Base*, bool> > c_1_products;
  std::list<Statement::Var_Decl*> c_1_decl;
  get_pareto_dimensions(
    product, *loop_body2, &i, &D, c1, std::string("c1"), c_1_products,
    c_1_decl, product.get_float_accuracy());

  i = 0;
  D = 0;
  std::list<std::pair<Product::Base*, bool> > c_2_products;
  std::list<Statement::Var_Decl*> c_2_decl;
  get_pareto_dimensions(
    product, *loop_body2, &i, &D, c2, std::string("c2"), c_2_products,
    c_2_decl, product.get_float_accuracy());

  // storage to keep track of what to add where in loop
  std::list<Statement::Base*> *cur_stmts = loop_body2;

  // test element for domination
  // loop over all dimensions
  int dim = 1;
  std::list<Statement::Var_Decl*>::iterator it_c1 = c_1_decl.begin();
  std::list<Statement::Var_Decl*>::iterator it_c2 = c_2_decl.begin();
  std::list<std::pair<Product::Base*, bool> >::iterator it =
    c_1_products.begin();
  for (; it != c_1_products.end(); ++it, ++it_c1, ++it_c2, ++dim) {
    Statement::Var_Decl *u = *it_c1;
    Statement::Var_Decl *x = *it_c2;

    std::pair<Product::Base*, bool> pairt = *it;
    Product::Two prod = *dynamic_cast<Product::Two*>(pairt.first);
    bool left = pairt.second;

    Type::Base *type;
    if (left) {
       type = u->type;
    } else {
       type = x->type;
    }


    // apply choice function
    Statement::Var_Decl *answer = new Statement::Var_Decl(type, "answer");
    cur_stmts->push_back(answer);
    if ((left && prod.left_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM)
        || (!left && prod.right_choice_fn_type(*name) ==
            Expr::Fn_Call::MINIMUM)) {
      Statement::If *if_case_min = new Statement::If(new Expr::Less(
        new Expr::Vacc(*u) , new Expr::Vacc(*x)));
      cur_stmts->push_back(if_case_min);
      Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
      if_case_min->then.push_back(la);
      Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
      if_case_min->els.push_back(la2);

    } else if ((left &&
               prod.left_choice_fn_type(*name) == Expr::Fn_Call::MAXIMUM) ||
               (!left && prod.right_choice_fn_type(*name) ==
                Expr::Fn_Call::MAXIMUM)) {
      Statement::If *if_case_min = new Statement::If(new Expr::Greater(
        new Expr::Vacc(*u) , new Expr::Vacc(*x)));
      cur_stmts->push_back(if_case_min);
      Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
      if_case_min->then.push_back(la);
      Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
      if_case_min->els.push_back(la2);
    } else {
      Statement::Var_Decl *candidates = new Statement::Var_Decl(
        new Type::List(type), "candidates");
      cur_stmts->push_back(candidates);
      cur_stmts->push_back(new Statement::Fn_Call(
        Statement::Fn_Call::EMPTY, *candidates));

      Statement::Fn_Call *push_backu = new Statement::Fn_Call(
        Statement::Fn_Call::PUSH_BACK);
      push_backu->add_arg(*candidates);
      push_backu->add_arg(*u);
      cur_stmts->push_back(push_backu);

      Statement::Fn_Call *push_backx = new Statement::Fn_Call(
        Statement::Fn_Call::PUSH_BACK);
      push_backx->add_arg(*candidates);
      push_backx->add_arg(*x);
      cur_stmts->push_back(push_backx);

      Fn_Def c;
      if (left) {
          c = *prod.left_choice_function(*name);
      } else {
          c = *prod.right_choice_function(*name);
      }

      Expr::Fn_Call *h = new Expr::Fn_Call(new std::string(c.target_name()));
      h->add_arg(*candidates);

      if (prod.left_mode(*name).number == Mode::ONE) {
          Statement::Var_Assign* la = new Statement::Var_Assign(*answer, h);
          cur_stmts->push_back(la);
      } else {
         Statement::Var_Decl *answer_list = new Statement::Var_Decl(
           c.return_type, "answer_list", h);

         Expr::Fn_Call *first = new Expr::Fn_Call(Expr::Fn_Call::GET_FRONT);
         first->add_arg(*answer_list);

         Statement::Var_Assign* la = new Statement::Var_Assign(*answer, first);
         cur_stmts->push_back(answer_list);
         cur_stmts->push_back(la);
      }
    }

     // test if add
    Statement::If *if_case_add = new Statement::If(new Expr::Eq(
      new Expr::Vacc(*x) , new Expr::Vacc(*answer)));
    cur_stmts->push_back(if_case_add);
    cur_stmts = &if_case_add->then;
  }

  Statement::Var_Assign *add_false = new Statement::Var_Assign(*add,
    new Expr::Const(new Const::Bool(false)));
  cur_stmts->push_back(add_false);
  cur_stmts->push_back(new Statement::Break());
  // add element to answer

  cur_stmts = loop_body2;
  // now the same spiel again to test for deletion
  dim = 1;
  it_c1 = c_1_decl.begin();
  it_c2 = c_2_decl.begin();
  it = c_1_products.begin();
  for (; it != c_1_products.end(); ++it, ++it_c1, ++it_c2, ++dim) {
        Statement::Var_Decl *u = *it_c1;
        Statement::Var_Decl *x = *it_c2;

        std::pair<Product::Base*, bool> pairt = *it;
        Product::Two prod = *dynamic_cast<Product::Two*>(pairt.first);
        bool left = pairt.second;

        Type::Base *type;
        if (left) {
           type = u->type;
        } else {
           type = x->type;
        }

        // apply choice function
        Statement::Var_Decl *answer = new Statement::Var_Decl(type, "answer");
        if (dim > 1) {
            cur_stmts->push_back(answer);
        }
        if ((left && prod.left_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM)
                || (!left && prod.right_choice_fn_type(*name) ==
                Expr::Fn_Call::MINIMUM)) {
          Statement::If *if_case_min = new Statement::If(new Expr::Less(
            new Expr::Vacc(*u) , new Expr::Vacc(*x)));
          cur_stmts->push_back(if_case_min);
          Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
          if_case_min->then.push_back(la);
          Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
          if_case_min->els.push_back(la2);
        } else if ((left &&
                   prod.left_choice_fn_type(*name) == Expr::Fn_Call::MAXIMUM) ||
                   (!left && prod.right_choice_fn_type(*name) ==
                   Expr::Fn_Call::MAXIMUM)) {
          Statement::If *if_case_min = new Statement::If(new Expr::Greater(
            new Expr::Vacc(*u) , new Expr::Vacc(*x)));
          cur_stmts->push_back(if_case_min);
          Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
          if_case_min->then.push_back(la);
          Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
          if_case_min->els.push_back(la2);

        } else {
          Statement::Var_Decl *candidates = new Statement::Var_Decl(
            new Type::List(type), "candidates");
          cur_stmts->push_back(candidates);
          cur_stmts->push_back(new Statement::Fn_Call(
            Statement::Fn_Call::EMPTY, *candidates));

          Statement::Fn_Call *push_backu = new Statement::Fn_Call(
            Statement::Fn_Call::PUSH_BACK);
          push_backu->add_arg(*candidates);
          push_backu->add_arg(*u);
          cur_stmts->push_back(push_backu);

          Statement::Fn_Call *push_backx = new Statement::Fn_Call(
            Statement::Fn_Call::PUSH_BACK);
          push_backx->add_arg(*candidates);
          push_backx->add_arg(*x);
          cur_stmts->push_back(push_backx);

          Fn_Def c;
          if (left) {
              c = *prod.left_choice_function(*name);
          } else {
              c = *prod.right_choice_function(*name);
          }

          Expr::Fn_Call *h = new Expr::Fn_Call(
            new std::string(c.target_name()));
          h->add_arg(*candidates);

          if (prod.left_mode(*name).number == Mode::ONE) {
                Statement::Var_Assign* la = new Statement::Var_Assign(
                  *answer, h);
                cur_stmts->push_back(la);
          } else {
               Statement::Var_Decl *answer_list = new Statement::Var_Decl(
                 c.return_type, "answer_list", h);

               Expr::Fn_Call *first = new Expr::Fn_Call(
                 Expr::Fn_Call::GET_FRONT);
               first->add_arg(*answer_list);

               Statement::Var_Assign* la = new Statement::Var_Assign(
                 *answer, first);
               cur_stmts->push_back(answer_list);
               cur_stmts->push_back(la);
          }
        }

         // test if add
        Statement::If *if_case_add = new Statement::If(new Expr::Eq(
          new Expr::Vacc(*u) , new Expr::Vacc(*answer)));
        cur_stmts->push_back(if_case_add);
        cur_stmts = &if_case_add->then;
  }

  Expr::Fn_Call *erase = new Expr::Fn_Call(Expr::Fn_Call::ERASE_ELEMENT);
  erase->add_arg(*answers);
  erase->add_arg(new std::string(*ans_it->name));
  Statement::Var_Assign *newAnswer = new Statement::Var_Assign(*ans_it, erase);
  cur_stmts->push_back(newAnswer);
  cur_stmts->push_back(new Statement::Continue());

  Statement::Increase *increase = new Statement::Increase(ans_it->name);
  loop_body2->push_back(increase);

  Statement::If *if_add = new Statement::If(new Expr::Eq(new Expr::Vacc(*add),
    new Expr::Const(new Const::Bool(true))));
  loop_body->push_back(if_add);
  Statement::Fn_Call *pb2 = new Statement::Fn_Call(
    Statement::Fn_Call::PUSH_BACK);
  Statement::Var_Decl *temp_elem2 = new Statement::Var_Decl(
    return_type->component(), "temp_elem", new Expr::Vacc(*c1));
  if_add->then.push_back(temp_elem2);

  pb2->add_arg(*answers);
  pb2->add_arg(*temp_elem2);
  if_add->then.push_back(pb2);

  Statement::Return *ret = new Statement::Return(*answers);
  stmts.push_back(ret);
}

void Fn_Def::codegen_pareto_isort(Fn_Def &a, Fn_Def &b, Product::Two &product) {
  // create  a variable to put all answers in
  assert(stmts.empty());
  Statement::Var_Decl *answers = new Statement::Var_Decl(
      return_type, "answers");
  stmts.push_back(answers);
  stmts.push_back(new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *answers));

  // first main loop, loop over raw answers
  Statement::Var_Decl *input_list = new Statement::Var_Decl(
      types.front(), names.front(), new Expr::Vacc(names.front()));

  Statement::Var_Decl *tupel = new Statement::Var_Decl(
    return_type->component(), "tupel");

  Statement::Foreach *loop = new Statement::Foreach(tupel, input_list);
  loop->set_itr(true);
  stmts.push_back(loop);
  std::list<Statement::Base*> *loop_body = &loop->statements;

  // set the first variables, tuple to insert into the answer list
  Statement::Var_Decl *u, *v;
    if (return_type->left()->is(Type::FLOAT) &&
        product.get_float_accuracy() != 0) {
        Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
        std::ostringstream offset;
        offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

        round->add_arg(new std::string(offset.str()) );
        round->add_arg(new Expr::Vacc(tupel->left()) );

        u = new Statement::Var_Decl(return_type->left(), "u",  round);
    } else {
        u = new Statement::Var_Decl(return_type->left(), "u",  new Expr::Vacc(
          tupel->left()));
    }
    if (return_type->right()->is(Type::FLOAT) &&
        product.get_float_accuracy() != 0) {
        Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
        std::ostringstream offset;
        offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

        round->add_arg( new std::string(offset.str()) );
        round->add_arg( new Expr::Vacc(tupel->right()) );

        v = new Statement::Var_Decl(return_type->right(), "v",  round);
    } else {
        v = new Statement::Var_Decl(return_type->right(), "v",  new Expr::Vacc(
          tupel->right()));
    }

  loop_body->push_back(u);
  loop_body->push_back(v);

  // create a boolean variable if the new element is added
  Statement::Var_Decl *add = new Statement::Var_Decl(
    new Type::Bool() , "add", new Expr::Const(new Const::Bool(true)));
  loop_body->push_back(add);

  // create a boolean variable set for removing following elements
  Statement::Var_Decl *erase = new Statement::Var_Decl(
    new Type::Bool() , "erase", new Expr::Const(new Const::Bool(false)));
  loop_body->push_back(erase);

  Statement::Var_Decl *answer = new Statement::Var_Decl(return_type, "answer");
  Statement::Var_Decl *answers_list = new Statement::Var_Decl(
      return_type, "answers", new Expr::Vacc(new std::string("answer")));

  Statement::Foreach *loop2 = new Statement::Foreach(answer, answers_list);
  loop2->set_itr(true);
  loop2->set_iteration(false);
  loop_body->push_back(loop2);
  std::list<Statement::Base*> *loop_body2 = &loop2->statements;

  Statement::Var_Decl *tmp = new Statement::Var_Decl(
    return_type->component(), "tmp",  new Expr::Vacc(*answer));
  Statement::Var_Decl *x, *y;
  if (return_type->left()->is(Type::FLOAT) &&
      product.get_float_accuracy() != 0) {
      Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
      std::ostringstream offset;
      offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

      round->add_arg( new std::string(offset.str()) );
      round->add_arg( new Expr::Vacc(tmp->left()) );

      x = new Statement::Var_Decl(return_type->left(), "x",  round);
  } else {
      x = new Statement::Var_Decl(
        return_type->left(), "x", new Expr::Vacc(tmp->left()));
  }
  if (return_type->right()->is(Type::FLOAT) &&
      product.get_float_accuracy() != 0) {
      Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
      std::ostringstream offset;
      offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

      round->add_arg( new std::string(offset.str()) );
      round->add_arg( new Expr::Vacc(tmp->right()) );

      y = new Statement::Var_Decl(return_type->right(), "y",  round);
  } else {
      y = new Statement::Var_Decl(
        return_type->right(), "y",  new Expr::Vacc(tmp->right()));
  }
  loop_body2->push_back(tmp);
  loop_body2->push_back(x);
  loop_body2->push_back(y);


  // get right answer with optimization
  Statement::Var_Decl *right_answer = new Statement::Var_Decl(
    return_type->right(), "right_answer");
  loop_body2->push_back(right_answer);
  if (product.right_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM) {
    Statement::If *if_case_min = new Statement::If(
      new Expr::Less(new Expr::Vacc(*v) , new Expr::Vacc(*y)));
    loop_body2->push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*right_answer, *v);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*right_answer, *y);
    if_case_min->els.push_back(la2);

  } else if (product.right_choice_fn_type(*name) == Expr::Fn_Call::MAXIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Greater(
      new Expr::Vacc(*v) , new Expr::Vacc(*y)));
    loop_body2->push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*right_answer, *v);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*right_answer, *y);
    if_case_min->els.push_back(la2);

  } else {
    Statement::Var_Decl *right_candidates = new Statement::Var_Decl(
      new Type::List(return_type->right()), "right_candidates");
    loop_body2->push_back(right_candidates);
    loop_body2->push_back(new Statement::Fn_Call(
      Statement::Fn_Call::EMPTY, *right_candidates));

    Statement::Fn_Call *push_backv = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backv->add_arg(*right_candidates);
    push_backv->add_arg(*v);
    loop_body2->push_back(push_backv);

    Statement::Fn_Call *push_backy = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backy->add_arg(*right_candidates);
    push_backy->add_arg(*y);
    loop_body2->push_back(push_backy);

    Expr::Fn_Call *h_right = new Expr::Fn_Call(
      new std::string(b.target_name()));
    h_right->add_arg(*right_candidates);

    if (product.right_mode(*name).number == Mode::ONE) {
          Statement::Var_Assign* la = new Statement::Var_Assign(
            *right_answer, h_right);
          loop_body2->push_back(la);
    } else {
         Statement::Var_Decl *right_answer_list = new Statement::Var_Decl(
           b.return_type, "right_answer_list", h_right);

         Expr::Fn_Call *right_first = new Expr::Fn_Call(
           Expr::Fn_Call::GET_FRONT);
         right_first->add_arg(*right_answer_list);

         Statement::Var_Assign* la = new Statement::Var_Assign(
           *right_answer, right_first);
         loop_body2->push_back(right_answer_list);
         loop_body2->push_back(la);
    }
  }


  Statement::If *if_erase = new Statement::If(new Expr::Eq(new Expr::Vacc(
    *erase) , new Expr::Const(new Const::Bool(false))));
  loop_body2->push_back(if_erase);

  // not erasing

  // left answer only needed in case of not erasing
  Statement::Var_Decl *left_answer = new Statement::Var_Decl(
    return_type->left(), "left_answer");
  if_erase->then.push_back(left_answer);
  if (product.left_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Less(
      new Expr::Vacc(*u) , new Expr::Vacc(*x)));
    if_erase->then.push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*left_answer, *u);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*left_answer, *x);
    if_case_min->els.push_back(la2);

  } else if (product.left_choice_fn_type(*name) == Expr::Fn_Call::MAXIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Greater(
      new Expr::Vacc(*u) , new Expr::Vacc(*x)));
    if_erase->then.push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*left_answer, *u);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*left_answer, *x);
    if_case_min->els.push_back(la2);
  } else {
    Statement::Var_Decl *left_candidates = new Statement::Var_Decl(
      new Type::List(return_type->left()), "left_candidates");
    if_erase->then.push_back(left_candidates);
    if_erase->then.push_back(new Statement::Fn_Call(
      Statement::Fn_Call::EMPTY, *left_candidates));

    Statement::Fn_Call *push_backu = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backu->add_arg(*left_candidates);
    push_backu->add_arg(*u);
    if_erase->then.push_back(push_backu);

    Statement::Fn_Call *push_backx = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backx->add_arg(*left_candidates);
    push_backx->add_arg(*x);
    if_erase->then.push_back(push_backx);

    Expr::Fn_Call *h_left = new Expr::Fn_Call(new std::string(a.target_name()));
    h_left->add_arg(*left_candidates);

    if (product.left_mode(*name).number == Mode::ONE) {
          Statement::Var_Assign* la = new Statement::Var_Assign(
            *left_answer, h_left);
          if_erase->then.push_back(la);
    } else {
         Statement::Var_Decl *left_answer_list = new Statement::Var_Decl(
           a.return_type, "left_answer_list", h_left);

         Expr::Fn_Call *left_first = new Expr::Fn_Call(
           Expr::Fn_Call::GET_FRONT);
         left_first->add_arg(*left_answer_list);

         Statement::Var_Assign* la = new Statement::Var_Assign(
           *left_answer, left_first);
         if_erase->then.push_back(left_answer_list);
         if_erase->then.push_back(la);
    }
  }

  // not erase, two cases
  // case 1.1
  Expr::Eq *eq_1_1_1 = new Expr::Eq( new Expr::Vacc(*u) , new Expr::Vacc(*x));
  Expr::Eq *eq_1_1_2 = new Expr::Eq( new Expr::Vacc(*y) , new Expr::Vacc(
    *right_answer));
  Expr::And *eq_1_1 = new Expr::And(eq_1_1_1, eq_1_1_2);

  // case 1.2
  Expr::Eq *eq_1_2_1 = new Expr::Eq( new Expr::Vacc(*x) , new Expr::Vacc(
    *left_answer));
  Expr::Not_Eq *eq_1_2_2 = new Expr::Not_Eq(
    new Expr::Vacc(*u) , new Expr::Vacc(*left_answer));
  Expr::Eq *eq_1_2_3 = new Expr::Eq(
    new Expr::Vacc(*y) , new Expr::Vacc(*right_answer));
  Expr::And *eq_1_2 = new Expr::And(
    new Expr::And(eq_1_2_1,  eq_1_2_2), eq_1_2_3);

  // full 1
  Expr::Or *eq_1 = new Expr::Or(eq_1_1, eq_1_2);

  // case 2.1
  Expr::Eq *eq_2_1_1 = new Expr::Eq(
    new Expr::Vacc(*u) , new Expr::Vacc(*left_answer));
  Expr::Not_Eq *eq_2_1_2 = new Expr::Not_Eq(
    new Expr::Vacc(*x) , new Expr::Vacc(*left_answer));
  Expr::And *eq_2_1 = new Expr::And(eq_2_1_1, eq_2_1_2);

  // case 2.2
  Expr::Eq *eq_2_2_1 = new Expr::Eq(
    new Expr::Vacc(*u) , new Expr::Vacc(*x));
  Expr::Eq *eq_2_2_2 = new Expr::Eq(
    new Expr::Vacc(*v) , new Expr::Vacc(*right_answer));
  Expr::Not_Eq *eq_2_2_3 = new Expr::Not_Eq(
    new Expr::Vacc(*y) , new Expr::Vacc(*right_answer));
  Expr::And *eq_2_2 = new Expr::And(
    new Expr::And(eq_2_2_2,  eq_2_2_3), eq_2_2_1);

  // full 2
  Expr::Or *eq_2 = new Expr::Or(eq_2_1, eq_2_2);

  // now create the ifs
  // if 1
  Statement::If *if_case1 = new Statement::If(eq_1);
  if_erase->then.push_back(if_case1);

  // if 1 content
  Statement::Var_Assign *add_false = new Statement::Var_Assign(
    *add, new Expr::Const(new Const::Bool(false)));
  if_case1->then.push_back(add_false);

  Statement::Break *ansbreak = new Statement::Break();
  if_case1->then.push_back(ansbreak);

  // else if 2
  Statement::If *if_case2 = new Statement::If(eq_2);
  if_case1->els.push_back(if_case2);

  // else if 2 content
  Statement::Var_Assign *add_false2 = new Statement::Var_Assign(
    *add, new Expr::Const(new Const::Bool(false)));
  if_case2->then.push_back(add_false2);
  Statement::Var_Assign *erase_true = new Statement::Var_Assign(
    *erase, new Expr::Const(new Const::Bool(true)));
  if_case2->then.push_back(erase_true);

  Statement::Var_Decl *temp_elem = new Statement::Var_Decl(
    return_type->component(), "temp_elem");
  if_case2->then.push_back(temp_elem);

  Statement::Var_Assign *l_ass = new Statement::Var_Assign(
    temp_elem->left(), new Expr::Vacc(tupel->left()));
  if_case2->then.push_back(l_ass);
  Statement::Var_Assign *r_ass = new Statement::Var_Assign(
    temp_elem->right(), new Expr::Vacc(tupel->right()));
  if_case2->then.push_back(r_ass);

  Expr::Fn_Call *insert = new Expr::Fn_Call(Expr::Fn_Call::INSERT_ELEMENT);
  insert->add_arg(*answers);
  insert->add_arg(new std::string(*answer->name));
  insert->add_arg(*temp_elem);

  Statement::Var_Assign *newAnswer = new Statement::Var_Assign(*answer, insert);
  if_case2->then.push_back(newAnswer);

  Statement::Increase *increase = new Statement::Increase(answer->name);
  if_erase->then.push_back(increase);

  // erasing

  // break condition
  Expr::Eq *eq_erase_1 = new Expr::Eq( new Expr::Vacc(*y) , new Expr::Vacc(
    *right_answer));
  Expr::Not_Eq *eq_erase_2 = new Expr::Not_Eq(
    new Expr::Vacc(*v) , new Expr::Vacc(*right_answer));
  Statement::If *if_erase_break = new Statement::If(
    new Expr::And(eq_erase_1, eq_erase_2));
  if_erase->els.push_back(if_erase_break);

  // content break condition
  Statement::Break *erasebreak = new Statement::Break();
  if_erase_break->then.push_back(erasebreak);

  // else erase
  Expr::Fn_Call *erase_f = new Expr::Fn_Call(Expr::Fn_Call::ERASE_ELEMENT);
  erase_f->add_arg(*answers);
  erase_f->add_arg(new std::string(*answer->name));

  Statement::Var_Assign *newAnswer2 = new Statement::Var_Assign(
    *answer, erase_f);
  if_erase_break->els.push_back(newAnswer2);


  // base condition for empty list
  Statement::If *if_add = new Statement::If(new Expr::Eq(new Expr::Vacc(*add),
    new Expr::Const(new Const::Bool(true))));
  loop_body->push_back(if_add);

  Statement::Fn_Call *pb = new Statement::Fn_Call(
    Statement::Fn_Call::PUSH_BACK);

  Statement::Var_Decl *temp_elem2 = new Statement::Var_Decl(
    return_type->component(), "temp_elem");
  if_add->then.push_back(temp_elem2);

  Statement::Var_Assign *l_ass2 = new Statement::Var_Assign(
    temp_elem->left(), new Expr::Vacc(tupel->left()));
  if_add->then.push_back(l_ass2);
  Statement::Var_Assign *r_ass2 = new Statement::Var_Assign(
    temp_elem->right(), new Expr::Vacc(tupel->right()));
  if_add->then.push_back(r_ass2);
  pb->add_arg(*answers);
  pb->add_arg(*temp_elem2);

  if_add->then.push_back(pb);

  // return condition
  Statement::Return *ret = new Statement::Return(*answers);
  stmts.push_back(ret);
}

void Fn_Def::codegen_pareto_multi_lex(Fn_Def &a, Fn_Def &b,
  Product::Two &product) {
  // input list
  Statement::Var_Decl *input_list = new Statement::Var_Decl(
  types.front(), names.front(), new Expr::Vacc(names.front()));

  // create  a variable to put all answers in
  assert(stmts.empty());
  Statement::Var_Decl *answers = new Statement::Var_Decl(
      return_type, "answers");
  stmts.push_back(answers);
  stmts.push_back(new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *answers));

  // main loop
  Statement::Var_Decl *tupel = new Statement::Var_Decl(
    return_type->component(), "tupel");

  Statement::Foreach *loop = new Statement::Foreach(tupel, input_list);
  loop->set_itr(true);
  stmts.push_back(loop);
  std::list<Statement::Base*> *loop_body = &loop->statements;

  Statement::Var_Decl *c1 = new Statement::Var_Decl(
    return_type->component(), "c1", new Expr::Vacc(*tupel));
  loop_body->push_back(c1);

  // test if list is empty
  Expr::Fn_Call *isEmpty = new Expr::Fn_Call(Expr::Fn_Call::IS_EMPTY);
  isEmpty->add_arg(*answers);
  Statement::If *if_empty = new Statement::If(isEmpty);
  loop_body->push_back(if_empty);

  Statement::Fn_Call *pb = new Statement::Fn_Call(
    Statement::Fn_Call::PUSH_BACK);
  Statement::Var_Decl *temp_elem = new Statement::Var_Decl(
    return_type->component(), "temp_elem", new Expr::Vacc(*c1));
  if_empty->then.push_back(temp_elem);

  pb->add_arg(*answers);
  pb->add_arg(*temp_elem);

  if_empty->then.push_back(pb);

  if_empty->then.push_back(new Statement::Continue());

  // create a boolean variable if the new element is added
  Statement::Var_Decl *add = new Statement::Var_Decl(
    new Type::Bool() , "add", new Expr::Const(new Const::Bool(true)));
  loop_body->push_back(add);


  Statement::Var_Decl *ans_it = new Statement::Var_Decl(return_type, "ans_it");
  Statement::Var_Decl *answers_list = new Statement::Var_Decl(
      return_type, "answers", new Expr::Vacc(new std::string("answer")));

  Statement::Foreach *loop2 = new Statement::Foreach(ans_it, answers_list);
  loop2->set_itr(true);
  loop_body->push_back(loop2);

  std::list<Statement::Base*> *loop_body2 = &loop2->statements;

  // get c2 as last element of answer list
  Statement::Var_Decl *c2 = new Statement::Var_Decl(
    return_type->component(), "c2",  new Expr::Vacc(*ans_it));
  loop_body2->push_back(c2);

  // create access for all dimensions
  int i = 0;
  int D = 0;
  std::list<std::pair<Product::Base*, bool> > c_1_products;
  std::list<Statement::Var_Decl*> c_1_decl;
  get_pareto_dimensions(
    product, *loop_body2, &i, &D, c1, std::string("c1"), c_1_products,
    c_1_decl, product.get_float_accuracy());

  i = 0;
  D = 0;
  std::list<std::pair<Product::Base*, bool> > c_2_products;
  std::list<Statement::Var_Decl*> c_2_decl;
  get_pareto_dimensions(
    product, *loop_body2, &i, &D, c2, std::string("c2"), c_2_products,
    c_2_decl, product.get_float_accuracy());

  // storage to keep track of what to add where in loop
  std::list<Statement::Base*> *cur_stmts = loop_body2;

  // loop over all dimensions, starting at dimension 2!
  int dim = 2;
  std::list<Statement::Var_Decl*>::iterator it_c1 = c_1_decl.begin();
  std::list<Statement::Var_Decl*>::iterator it_c2 = c_2_decl.begin();
  std::list<std::pair<Product::Base*, bool> >::iterator it =
    c_1_products.begin();
  it++;
  it_c1++;
  it_c2++;
  for (; it != c_1_products.end(); ++it, ++it_c1, ++it_c2, ++dim) {
    Statement::Var_Decl *u = *it_c1;
    Statement::Var_Decl *x = *it_c2;

    std::pair<Product::Base*, bool> pairt = *it;
    Product::Two prod = *dynamic_cast<Product::Two*>(pairt.first);
    bool left = pairt.second;

    Type::Base *type;
    if (left) {
       type = u->type;
    } else {
       type = x->type;
    }

    // apply choice function
    Statement::Var_Decl *answer = new Statement::Var_Decl(type, "answer");
    cur_stmts->push_back(answer);
    if ((left && prod.left_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM) ||
        (!left && prod.right_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM)) {
      Statement::If *if_case_min = new Statement::If(new Expr::Less(
        new Expr::Vacc(*u) , new Expr::Vacc(*x)));
      cur_stmts->push_back(if_case_min);
      Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
      if_case_min->then.push_back(la);
      Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
      if_case_min->els.push_back(la2);
    } else if ((left && prod.left_choice_fn_type(*name) ==
               Expr::Fn_Call::MAXIMUM)
               || (!left && prod.right_choice_fn_type(*name) ==
               Expr::Fn_Call::MAXIMUM)) {
      Statement::If *if_case_min = new Statement::If(new Expr::Greater(
        new Expr::Vacc(*u) , new Expr::Vacc(*x)));
      cur_stmts->push_back(if_case_min);
      Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
      if_case_min->then.push_back(la);
      Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
      if_case_min->els.push_back(la2);
    } else {
      Statement::Var_Decl *candidates = new Statement::Var_Decl(
        new Type::List(type), "candidates");
      cur_stmts->push_back(candidates);
      cur_stmts->push_back(new Statement::Fn_Call(
        Statement::Fn_Call::EMPTY, *candidates));

      Statement::Fn_Call *push_backu = new Statement::Fn_Call(
        Statement::Fn_Call::PUSH_BACK);
      push_backu->add_arg(*candidates);
      push_backu->add_arg(*u);
      cur_stmts->push_back(push_backu);

      Statement::Fn_Call *push_backx = new Statement::Fn_Call(
        Statement::Fn_Call::PUSH_BACK);
      push_backx->add_arg(*candidates);
      push_backx->add_arg(*x);
      cur_stmts->push_back(push_backx);

      Fn_Def c;
      if (left) {
          c = *prod.left_choice_function(*name);
      } else {
          c = *prod.right_choice_function(*name);
      }

      Expr::Fn_Call *h = new Expr::Fn_Call(new std::string(c.target_name()));
      h->add_arg(*candidates);

      if (prod.left_mode(*name).number == Mode::ONE) {
            Statement::Var_Assign* la = new Statement::Var_Assign(*answer, h);
            cur_stmts->push_back(la);
      } else {
         Statement::Var_Decl *answer_list = new Statement::Var_Decl(
           c.return_type, "answer_list", h);

         Expr::Fn_Call *first = new Expr::Fn_Call(Expr::Fn_Call::GET_FRONT);
         first->add_arg(*answer_list);

         Statement::Var_Assign* la = new Statement::Var_Assign(*answer, first);
         cur_stmts->push_back(answer_list);
         cur_stmts->push_back(la);
      }
    }

     // test if add
    Statement::If *if_case_add = new Statement::If(new Expr::Eq(
      new Expr::Vacc(*x) , new Expr::Vacc(*answer)));
    cur_stmts->push_back(if_case_add);
    cur_stmts = &if_case_add->then;
  }

  Statement::Var_Assign *add_false = new Statement::Var_Assign(
    *add, new Expr::Const(new Const::Bool(false)));
  cur_stmts->push_back(add_false);
  cur_stmts->push_back(new Statement::Break());
  // add element to answer

  Statement::If *if_add = new Statement::If(new Expr::Eq(
    new Expr::Vacc(*add) , new Expr::Const(new Const::Bool(true))));
  loop_body->push_back(if_add);
  Statement::Fn_Call *pb2 = new Statement::Fn_Call(
    Statement::Fn_Call::PUSH_BACK);
  Statement::Var_Decl *temp_elem2 = new Statement::Var_Decl(
    return_type->component(), "temp_elem", new Expr::Vacc(*c1));
  if_add->then.push_back(temp_elem2);

  pb2->add_arg(*answers);
  pb2->add_arg(*temp_elem2);
  if_add->then.push_back(pb2);

  Statement::Return *ret = new Statement::Return(*answers);
  stmts.push_back(ret);
}


Product::Two Fn_Def::codegen_pareto_move_to_first_all_dim(
  Statement::Var_Decl * & c1, Statement::Var_Decl * & c2,
  std::list<Statement::Base*> *stmts, Product::Base &product) {
    // first find the beginning pareto entry

    // true : left
    // false : right
    std::list<bool> grabList;

    Type::Base* type = return_type;

    Product::Two *prod_t;

    if (product.sort_product) {
        get_sort_grab_list(grabList, *product.sort_product);

        Product::Two *p = dynamic_cast<Product::Two*>(product.sort_product);
        prod_t = p;
    } else {
        get_sort_grab_list(grabList, product);

        Product::Two *p = dynamic_cast<Product::Two*>(&product);

        prod_t = p;
    }

    Product::Two prod = *prod_t;

    if (grabList.empty() && product.type() != Product::PARETO) {
       Log::instance()->error(
         "No pareto product found to sort by. Remove option -P 1 or -P 3 "
         "respectively.");
       assert(0);
    }

    int i = 0;

    // don't move into the first pareto, but stop AT it
    std::list<bool>::iterator preEnd = grabList.end();
    preEnd--;

    for (std::list<bool>::iterator it1=grabList.begin();
        it1 != preEnd; ++it1, ++i) {
       bool op = *it1;
       std::ostringstream oc1;
       oc1 << "c1_" << i;
       std::ostringstream oc2;
       oc2 << "c2_" << i;

       std::list<bool>::iterator next = it1;
       next++;

       if (op) {
           type = type->left();
           if (next != grabList.end()) {
             Product::Two *p = dynamic_cast<Product::Two*>(prod.left());
             prod = *p;
           }
           c1 = new Statement::Var_Decl(type, oc1.str(),
            new Expr::Vacc(c1->left()));
           c2 = new Statement::Var_Decl(type, oc2.str(),
            new Expr::Vacc(c2->left()));
       } else {
           type = type->right();
           if (next != grabList.end()) {
             Product::Two *p = dynamic_cast<Product::Two*>(prod.right());
             prod = *p;
           }
           c1 = new Statement::Var_Decl(type, oc1.str(),
            new Expr::Vacc(c1->right()));
           c2 = new Statement::Var_Decl(type, oc2.str(),
            new Expr::Vacc(c2->right()));
       }

       stmts->push_back(c1);
       stmts->push_back(c2);
    }

    return prod;
}


int Fn_Def::codegen_pareto_comparator_all_dim(
  Statement::Var_Decl *c1, Statement::Var_Decl *c2, Statement::Var_Decl *dim,
  Operator &comp, Product::Base &product) {
    // create access for all dimensions
  int i = 0;
  int D = 0;
  std::list<std::pair<Product::Base*, bool> > c_1_products;
  std::list<Statement::Var_Decl*> c_1_decl;
  get_pareto_dimensions(
    product, comp.stmts, &i, &D, c1, std::string("c1"), c_1_products,
    c_1_decl, product.get_float_accuracy());

  i = 0;
  D = 0;
  std::list<std::pair<Product::Base*, bool> > c_2_products;
  std::list<Statement::Var_Decl*> c_2_decl;
  get_pareto_dimensions(
    product, comp.stmts, &i, &D, c2, std::string("c2"), c_2_products,
    c_2_decl, product.get_float_accuracy());

  std::list<Statement::Base*> *cur_stmts = &comp.stmts;

  // loop over all dimensions
  int d = 1;
  std::list<Statement::Var_Decl*>::iterator it_c1 = c_1_decl.begin();
  std::list<Statement::Var_Decl*>::iterator it_c2 = c_2_decl.begin();
  std::list<std::pair<Product::Base*, bool> >::iterator it =
    c_1_products.begin();

  Statement::Switch *sw = new Statement::Switch(new Expr::Vacc(*dim));

  comp.stmts.push_back(sw);

  for (; it != c_1_products.end(); ++it, ++it_c1, ++it_c2, ++d) {
        std::ostringstream D_str;
        D_str << d;

        cur_stmts = sw->add_case(new std::string(D_str.str()));

        Statement::Var_Decl *u = *it_c1;
        Statement::Var_Decl *x = *it_c2;

        std::pair<Product::Base*, bool> pairt = *it;
        Product::Two prod = *dynamic_cast<Product::Two*>(pairt.first);
        bool left = pairt.second;

        Type::Base *type;
        if (left) {
           type = u->type;
        } else {
           type = x->type;
        }

        // apply choice function
        Statement::Var_Decl *answer = new Statement::Var_Decl(type, "answer");
        cur_stmts->push_back(answer);
        if ((left && prod.left_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM)
            || (!left && prod.right_choice_fn_type(*name) ==
            Expr::Fn_Call::MINIMUM)) {
          Statement::If *if_case_min = new Statement::If(new Expr::Less(
            new Expr::Vacc(*u) , new Expr::Vacc(*x)));
          cur_stmts->push_back(if_case_min);
          Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
          if_case_min->then.push_back(la);
          Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
          if_case_min->els.push_back(la2);

        } else if ((left && prod.left_choice_fn_type(*name) ==
                   Expr::Fn_Call::MAXIMUM)
                   ||  (!left && prod.right_choice_fn_type(*name) ==
                   Expr::Fn_Call::MAXIMUM)) {
          Statement::If *if_case_min = new Statement::If(new Expr::Greater(
            new Expr::Vacc(*u) , new Expr::Vacc(*x)));
          cur_stmts->push_back(if_case_min);
          Statement::Var_Assign* la = new Statement::Var_Assign(*answer, *u);
          if_case_min->then.push_back(la);
          Statement::Var_Assign* la2 = new Statement::Var_Assign(*answer, *x);
          if_case_min->els.push_back(la2);

        } else {
          Statement::Var_Decl *candidates = new Statement::Var_Decl(
            new Type::List(type), "candidates");
          cur_stmts->push_back(candidates);
          cur_stmts->push_back(new Statement::Fn_Call(
            Statement::Fn_Call::EMPTY, *candidates));

          Statement::Fn_Call *push_backu = new Statement::Fn_Call(
            Statement::Fn_Call::PUSH_BACK);
          push_backu->add_arg(*candidates);
          push_backu->add_arg(*u);
          cur_stmts->push_back(push_backu);

          Statement::Fn_Call *push_backx = new Statement::Fn_Call(
            Statement::Fn_Call::PUSH_BACK);
          push_backx->add_arg(*candidates);
          push_backx->add_arg(*x);
          cur_stmts->push_back(push_backx);

          Fn_Def c;
          if (left) {
              c = *prod.left_choice_function(*name);
          } else {
              c = *prod.right_choice_function(*name);
          }

          Expr::Fn_Call *h = new Expr::Fn_Call(new std::string(
            c.target_name()));
          h->add_arg(*candidates);

          if (prod.left_mode(*name).number == Mode::ONE) {
                Statement::Var_Assign* la = new Statement::Var_Assign(
                  *answer, h);
                cur_stmts->push_back(la);
          } else {
               Statement::Var_Decl *answer_list = new Statement::Var_Decl(
                 c.return_type, "answer_list", h);

               Expr::Fn_Call *first = new Expr::Fn_Call(
                 Expr::Fn_Call::GET_FRONT);
               first->add_arg(*answer_list);

               Statement::Var_Assign* la = new Statement::Var_Assign(
                 *answer, first);
               cur_stmts->push_back(answer_list);
               cur_stmts->push_back(la);
          }
        }


        // test strict lesser
        Statement::If *if_strict_greater = new Statement::If(
          new Expr::And(new Expr::Eq(new Expr::Vacc(*u) ,
          new Expr::Vacc(*answer)), new Expr::Not_Eq(new Expr::Vacc(*x) ,
          new Expr::Vacc(*answer))));
        cur_stmts->push_back(if_strict_greater);
        Statement::Return *ret_greater = new Statement::Return(
          new Expr::Const(new Const::Int(1)));
        if_strict_greater->then.push_back(ret_greater);

        Statement::If *if_equal = new Statement::If(new Expr::Eq(
          new Expr::Vacc(*u) , new Expr::Vacc(*x)));
        Statement::Return *ret_equal = new Statement::Return(
          new Expr::Const(new Const::Int(0)));
        Statement::Return *ret_lesser = new Statement::Return(
          new Expr::Const(new Const::Int(-1)));
        if_strict_greater->els.push_back(if_equal);

        if_equal->then.push_back(ret_equal);
        if_equal->els.push_back(ret_lesser);
  }

  Statement::Return *ret_cond = new Statement::Return(new Expr::Const(
    new Const::Int(-1)));
  comp.stmts.push_back(ret_cond);

  return D;
}


// generates the comparator element needed for advanced multi-dim pareto
void Fn_Def::codegen_pareto_multi_yukish(Fn_Def &a, Fn_Def &b,
  Product::Two &product, int cutoff, int D) {
    // real implementation is in rtlib, this just calls
    // the function passing the comparator

    std::ostringstream D_str;
    D_str << D;

    std::ostringstream cutoff_str;
    cutoff_str << cutoff;

    // create  a variable to put all answers in
    assert(stmts.empty());
    Statement::Var_Decl *answers = new Statement::Var_Decl(
        return_type, "answers");
    stmts.push_back(answers);
    stmts.push_back(new Statement::Fn_Call(
      Statement::Fn_Call::EMPTY, *answers));

    std::string* first = new std::string(*names.front());
    first->append(".first");
    std::string* second = new std::string(*names.front());
    second->append(".second");

    Statement::Fn_Call *pareto = new Statement::Fn_Call(
      Statement::Fn_Call::PARETO_YUKISH);
    pareto->add_arg(*answers);
    pareto->add_arg(first);
    pareto->add_arg(second);
    pareto->add_arg(comparator->object);
    pareto->add_arg(new std::string(D_str.str()));
    pareto->add_arg(new std::string(cutoff_str.str()));

    stmts.push_back(pareto);

    Statement::Return *ret = new Statement::Return(*answers);
    stmts.push_back(ret);
}

// generates the comparator element needed for domination optimized nosort
void Fn_Def::codegen_pareto_domination_nosort(Fn_Def &a, Fn_Def &b,
  Product::Two &product) {
    // create  a variable to put all answers in
    assert(stmts.empty());
    Statement::Var_Decl *answers = new Statement::Var_Decl(
        return_type, "answers");
    stmts.push_back(answers);
    stmts.push_back(new Statement::Fn_Call(
      Statement::Fn_Call::EMPTY, *answers));

    std::string* first = new std::string(*names.front());
    first->append(".first");
    std::string* second = new std::string(*names.front());
    second->append(".second");

    Statement::Fn_Call *pareto = new Statement::Fn_Call(
      Statement::Fn_Call::PARETO_DOMINATION_SORT);
    pareto->add_arg(*answers);
    pareto->add_arg(first);
    pareto->add_arg(second);
    pareto->add_arg(comparator->object);

    stmts.push_back(pareto);

    Statement::Return *ret = new Statement::Return(*answers);
    stmts.push_back(ret);
}


void Fn_Def::codegen_pareto_lex(Fn_Def &a, Fn_Def &b, Product::Two &product) {
  // input list
  Statement::Var_Decl *input_list = new Statement::Var_Decl(
  types.front(), names.front(), new Expr::Vacc(names.front()));

  // create  a variable to put all answers in
  assert(stmts.empty());
  Statement::Var_Decl *answers = new Statement::Var_Decl(
      return_type, "answers");
  stmts.push_back(answers);
  stmts.push_back(new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *answers));

  // main loop
  Statement::Var_Decl *tupel = new Statement::Var_Decl(
    return_type->component(), "tupel");

  Statement::Foreach *loop = new Statement::Foreach(tupel, input_list);
  loop->set_itr(true);
  stmts.push_back(loop);
  std::list<Statement::Base*> *loop_body = &loop->statements;

  // test if list is empty
  Expr::Fn_Call *isEmpty = new Expr::Fn_Call(Expr::Fn_Call::IS_EMPTY);
  isEmpty->add_arg(*answers);
  Statement::If *if_empty = new Statement::If(isEmpty);
  loop_body->push_back(if_empty);

  Statement::Fn_Call *pb = new Statement::Fn_Call(
    Statement::Fn_Call::PUSH_BACK);

  Statement::Var_Decl *temp_elem = new Statement::Var_Decl(
    return_type->component(), "temp_elem");
  if_empty->then.push_back(temp_elem);

  Statement::Var_Assign *l_ass = new Statement::Var_Assign(
    temp_elem->left(), new Expr::Vacc(tupel->left()));
  if_empty->then.push_back(l_ass);
  Statement::Var_Assign *r_ass = new Statement::Var_Assign(
    temp_elem->right(), new Expr::Vacc(tupel->right()));
  if_empty->then.push_back(r_ass);
  pb->add_arg(*answers);
  pb->add_arg(*temp_elem);

  if_empty->then.push_back(pb);

  if_empty->then.push_back(new Statement::Continue());

  // raw comp is easy
  Statement::Var_Decl *v;
  if (return_type->right()->is(Type::FLOAT) &&
      product.get_float_accuracy() != 0) {
      Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
      std::ostringstream offset;
      offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

      round->add_arg( new std::string(offset.str()) );
      round->add_arg( new Expr::Vacc(tupel->right()) );

      v = new Statement::Var_Decl(return_type->right(), "v",  round);
  } else {
      v = new Statement::Var_Decl(return_type->right(), "v",  new Expr::Vacc(
        tupel->right()));
  }
  loop_body->push_back(v);

  // get last element of the list
  Expr::Fn_Call *last_answer = new Expr::Fn_Call(Expr::Fn_Call::GET_BACK);
  last_answer->add_arg(*answers);

  Statement::Var_Decl *tmp = new Statement::Var_Decl(
    return_type->component(), "tmp",  last_answer);
  Statement::Var_Decl *y;
  if (return_type->right()->is(Type::FLOAT) &&
      product.get_float_accuracy() != 0) {
        Expr::Fn_Call *round = new Expr::Fn_Call(Expr::Fn_Call::ROUND_TO_DIGIT);
        std::ostringstream offset;
        offset  << static_cast<int>(std::pow(10, product.get_float_accuracy()));

        round->add_arg( new std::string(offset.str()) );
        round->add_arg( new Expr::Vacc(tmp->right()) );

        y = new Statement::Var_Decl(return_type->right(), "y",  round);

    } else {
        y = new Statement::Var_Decl(
          return_type->right(), "y",  new Expr::Vacc(tmp->right()));
    }

  loop_body->push_back(tmp);
  loop_body->push_back(y);

  // apply right ordering
  Statement::Var_Decl *right_answer = new Statement::Var_Decl(
    return_type->right(), "right_answer");
  loop_body->push_back(right_answer);
  if (product.right_choice_fn_type(*name) == Expr::Fn_Call::MINIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Less(
      new Expr::Vacc(*v) , new Expr::Vacc(*y)));
    loop_body->push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*right_answer, *v);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*right_answer, *y);
    if_case_min->els.push_back(la2);

  } else if (product.right_choice_fn_type(*name) == Expr::Fn_Call::MAXIMUM) {
    Statement::If *if_case_min = new Statement::If(new Expr::Greater(
      new Expr::Vacc(*v) , new Expr::Vacc(*y)));
    loop_body->push_back(if_case_min);
    Statement::Var_Assign* la = new Statement::Var_Assign(*right_answer, *v);
    if_case_min->then.push_back(la);
    Statement::Var_Assign* la2 = new Statement::Var_Assign(*right_answer, *y);
    if_case_min->els.push_back(la2);

  } else {
    Statement::Var_Decl *right_candidates = new Statement::Var_Decl(
      new Type::List(return_type->right()), "right_candidates");
    loop_body->push_back(right_candidates);
    loop_body->push_back(new Statement::Fn_Call(
      Statement::Fn_Call::EMPTY, *right_candidates));

    Statement::Fn_Call *push_backv = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backv->add_arg(*right_candidates);
    push_backv->add_arg(*v);
    loop_body->push_back(push_backv);

    Statement::Fn_Call *push_backy = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    push_backy->add_arg(*right_candidates);
    push_backy->add_arg(*y);
    loop_body->push_back(push_backy);

    Expr::Fn_Call *h_right = new Expr::Fn_Call(
      new std::string(b.target_name()));
    h_right->add_arg(*right_candidates);

    if (product.right_mode(*name).number == Mode::ONE) {
          Statement::Var_Assign* la = new Statement::Var_Assign(
            *right_answer, h_right);
          loop_body->push_back(la);
    } else {
         Statement::Var_Decl *right_answer_list = new Statement::Var_Decl(
           b.return_type, "right_answer_list", h_right);

         Expr::Fn_Call *right_first = new Expr::Fn_Call(
           Expr::Fn_Call::GET_FRONT);
         right_first->add_arg(*right_answer_list);

         Statement::Var_Assign* la = new Statement::Var_Assign(
           *right_answer, right_first);
         loop_body->push_back(right_answer_list);
         loop_body->push_back(la);
    }
  }

  // test if add
  Expr::Eq *eq_1 = new Expr::Eq( new Expr::Vacc(*v) , new Expr::Vacc(
    *right_answer));
  Expr::Not_Eq *eq_2 = new Expr::Not_Eq( new Expr::Vacc(*y) , new Expr::Vacc(
    *right_answer));

  Statement::If *if_case_add = new Statement::If(new Expr::And(eq_1, eq_2));
  loop_body->push_back(if_case_add);

  Statement::Fn_Call *pb2 = new Statement::Fn_Call(
    Statement::Fn_Call::PUSH_BACK);

  Statement::Var_Decl *temp_elem2 = new Statement::Var_Decl(
    return_type->component(), "temp_elem");
  if_case_add->then.push_back(temp_elem2);

  Statement::Var_Assign *l_ass2 = new Statement::Var_Assign(
    temp_elem->left(), new Expr::Vacc(tupel->left()));
  if_case_add->then.push_back(l_ass2);
  Statement::Var_Assign *r_ass2 = new Statement::Var_Assign(
    temp_elem->right(), new Expr::Vacc(tupel->right()));
  if_case_add->then.push_back(r_ass2);
  pb2->add_arg(*answers);
  pb2->add_arg(*temp_elem2);

  if_case_add->then.push_back(pb2);

  Statement::Return *ret = new Statement::Return(*answers);
  stmts.push_back(ret);
}

void Fn_Def::codegen_takeone(Fn_Def &a, Fn_Def &b, Product::Two &product) {
  assert(product.left_mode(*name).number == Mode::ONE);
  codegen_times(a, b, product);
}

void Fn_Def::codegen_nop(Product::Two &product) {
  Statement::Return *ret =
    new Statement::Return(new Expr::Vacc(names.front()));
  stmts.push_back(ret);
}

void Fn_Def::codegen_cartesian(Fn_Def &a, Fn_Def &b, Product::Two &product) {
  // FIXME answers is no list?
  Statement::Var_Decl *answers = new Statement::Var_Decl(
      return_type, "answers");
  stmts.push_back(answers);

  Expr::Fn_Call *splice_left = new Expr::Fn_Call(Expr::Fn_Call::SPLICE_LEFT);
  splice_left->add_arg(names.front());
  Statement::Var_Decl *left = new Statement::Var_Decl(
      new Type::Range(return_type->left(), return_type, Type::Range::LEFT),
      "left", splice_left);
  stmts.push_back(left);

  Expr::Fn_Call *splice_right = new Expr::Fn_Call(Expr::Fn_Call::SPLICE_RIGHT);
  splice_right->add_arg(names.front());
  Statement::Var_Decl *right = new Statement::Var_Decl(
      new Type::Range(return_type->right(), return_type, Type::Range::RIGHT),
      "right", splice_right);
  stmts.push_back(right);

  Expr::Fn_Call *h_l = new Expr::Fn_Call(new std::string(a.target_name()));
  h_l->add_arg(*left);
  Statement::Var_Assign *l = new Statement::Var_Assign(
    new Var_Acc::Comp(*answers, 0), h_l);
  stmts.push_back(l);

  Expr::Fn_Call *h_r = new Expr::Fn_Call(new std::string(b.target_name()));
  h_r->add_arg(*right);
  Statement::Var_Assign *r = new Statement::Var_Assign(
    new Var_Acc::Comp(*answers, 1), h_r);
  stmts.push_back(r);

  Statement::Return *ret = new Statement::Return(*answers);
  stmts.push_back(ret);
}

void Fn_Def::remove_return_list() {
  if (stmts.empty())
    return;
  Statement::Base *s = stmts.back();
  if (!s->is(Statement::RETURN)) {
      Log::instance()->warning(location,
          "Last statement is not return - cannot eliminate list.");
      return;
  }
  Statement::Return *ret = dynamic_cast<Statement::Return*>(s);
  assert(ret);
  Expr::Base *l = ret->expr;
  if (!l->is(Expr::FN_CALL)) {
      Log::instance()->warning(location,
          "Return does not call a function - cannot eliminate list.");
      return;
  }
  Expr::Fn_Call *list = dynamic_cast<Expr::Fn_Call*>(l);
  assert(list);
  if (list->builtin != Expr::Fn_Call::LIST) {
      Log::instance()->warning(location,
          "Return does not call the list function - cannot eliminate list.");
      return;
  }
  ret->expr = list->exprs.front();
  delete list;
}


// set the kind of function (predefined types, modes, yield type)
Mode Fn_Def::derive_role() const {
  Mode r;
  r.set(Yield::Poly(Yield::UP));
  if (!stmts.back()->is(Statement::RETURN))
    return r;
  Statement::Return *ret = dynamic_cast<Statement::Return*>(stmts.back());
  assert(ret);
  if (!ret->expr->is(Expr::FN_CALL)) {
    if (ret->expr->is(Expr::VACC)) {
      Expr::Vacc *vacc = dynamic_cast<Expr::Vacc*>(ret->expr);
      assert(vacc);
      if (*vacc->name() == *names.front())
        r.set(Mode::PRETTY);
    }
    return r;
  }
  Expr::Fn_Call *fn = dynamic_cast<Expr::Fn_Call*>(ret->expr);
  assert(fn);
  if (fn->builtin == Expr::Fn_Call::UNIQUE) {
    r.set(Mode::CLASSIFY);
    return r;
  }
  if (fn->builtin != Expr::Fn_Call::LIST)
    return r;
  if (!fn->exprs.front()->is(Expr::FN_CALL))
    return r;
  fn = dynamic_cast<Expr::Fn_Call*>(fn->exprs.front());
  assert(fn);
  switch (fn->builtin) {
    case Expr::Fn_Call::SUM :
    case Expr::Fn_Call::EXPSUM :
    case Expr::Fn_Call::EXP2SUM :
    case Expr::Fn_Call::BITSUM :
      r.set(Mode::SYNOPTIC);
      break;
    case Expr::Fn_Call::MINIMUM :
    case Expr::Fn_Call::MAXIMUM :
      r.set(Mode::SCORING);
      break;
    default:
      break;
  }
  return r;
}

// find type of the choice function
Expr::Fn_Call::Builtin Fn_Def::choice_fn_type() const {
  if (!choice_fn) {
    assert(choice_fn_type_ != Expr::Fn_Call::NONE);
    return choice_fn_type_;
  }
  // assert(!stmts.empty());
  if (stmts.empty())
    return Expr::Fn_Call::NONE;
  if (!stmts.back()->is(Statement::RETURN))
    return Expr::Fn_Call::NONE;
  Statement::Return *ret = dynamic_cast<Statement::Return*>(stmts.back());
  assert(ret);
  if (!ret->expr->is(Expr::FN_CALL)) {
    if (ret->expr->is(Expr::VACC)) {
      [[maybe_unused]] Expr::Vacc *vacc = dynamic_cast<Expr::Vacc*>(ret->expr);
      assert(vacc);
      // FIXME
      // if (*vacc->name() == *names.front())
      //  ;
    }
    return Expr::Fn_Call::NONE;
  }
  Expr::Fn_Call *fn = dynamic_cast<Expr::Fn_Call*>(ret->expr);
  assert(fn);
  if (fn->builtin == Expr::Fn_Call::UNIQUE) {
    return fn->builtin;
  }
  if (fn->builtin != Expr::Fn_Call::LIST)
    return fn->builtin;
  if (!fn->exprs.front()->is(Expr::FN_CALL))
    return Expr::Fn_Call::NONE;
  fn = dynamic_cast<Expr::Fn_Call*>(fn->exprs.front());
  assert(fn);
  return fn->builtin;
}

void Fn_Def::set_mode(std::string *s) {
  if (!s)
    return;
  [[maybe_unused]] bool b = mode_.set(*s);
  assert(b);
}

void Fn_Def::reduce_return_type() {
  ::Type::Base *t = return_type;
  ::Type::List *l = dynamic_cast< ::Type::List*>(t->simple());
  assert(l);
  t = l->of;
  return_type = t;
  remove_return_list();
}


void Fn_Def::install_choice_filter(Filter &filter) {
  assert(choice_fn);
  Fn_Def *fn = adaptor;
  // ok, if Product::Nop, i.e. if product shuffle opt was applied
  // assert(fn);
  if (!fn) {
    fn = this;
  } else {
    /* FIXME allows filter with choice fns - remove this code
    Statement::Fn_Call *a = new Statement::Fn_Call(Statement::Fn_Call::ASSERT);
    // FIXME add bool support to Const::
    a->add_arg(new Expr::Const(0));
    stmts.push_front(a);
    */
  }
  if (!fn->types.front()->is(Type::LIST)) {
    Log::instance()->error("Can't filter non-list answer type.");
    return;
  }
  std::string *orig = new std::string(*fn->names.front() + "_orig");
  Statement::Var_Decl *cont = new Statement::Var_Decl(fn->types.front(), orig);

  Expr::Fn_Call *filter_fn = new Expr::Fn_Call(filter);
  filter_fn->add_arg(*cont);
  Statement::Var_Decl *v = new Statement::Var_Decl(
    fn->types.front(), fn->names.front(), filter_fn);
  fn->names.erase(fn->names.begin());

  Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(fn->paras.front());
  assert(s);
  s->replace(orig);

  fn->names.push_back(orig);
  fn->stmts.push_front(v);
}


void Fn_Def::optimize_classify() {
  adaptor = 0;
  std::list<Statement::Base*> s;
  s.push_back(stmts.front());

  Statement::Var_Decl *answers = dynamic_cast<Statement::Var_Decl*> (
    stmts.front());
  assert(answers);

  Statement::Fn_Call *app = new Statement::Fn_Call(
    Statement::Fn_Call::APPEND_FILTER);
  app->add_arg(*answers);
  app->add_arg(new Expr::Vacc(names.front()));
  s.push_back(app);

  Statement::Fn_Call *fin = new Statement::Fn_Call(
    Statement::Fn_Call::FINALIZE);
  fin->add_arg(*answers);
  s.push_back(fin);

  s.push_back(stmts.back());

  stmts = s;
}


void Fn_Def::add_choice_specialization(
  Fn_Def &a, Fn_Def &b, Product::Two &product) {
  Fn_Def *x = 0;
  Fn_Def *y = 0;
  if (gen_type == STANDARD &&
           (product.get_adp_specialization() == ADP_Mode::PARETO_EAGER_STEP ||
           product.get_adp_specialization() == ADP_Mode::PARETO_EAGER_BLOCK)) {
        // base case is NOP, but we need real codegen
        x = new Fn_Def();
        x->name = name;
        x->names = names;
        x->types = types;
        x->return_type = return_type;
        x->target_name_ = target_name_;
        x->gen_type = CHOICE_SPECIALIZATION;
        x->paras = paras;
        x->choice_fn = true;

        x->codegen_choice(a, b, product);

//        x->paras.clear();
//        x->add_para(x->types.front(), new std::string("i"));

        y = x->adaptor;

        if (adaptor) {
            adaptor->adaptor = x;
        } else {
            adaptor = x;
        }

  } else {
    assert(!adaptor || !adaptor->adaptor);

    bool is_list_opt = false;

    x = new Fn_Def(*this);

    x->comparator = NULL;
    x->sorter = NULL;


    if (adaptor)
      y = new Fn_Def(*adaptor);
    else
      is_list_opt = true;
    x->adaptor = y;
    if (y)
      adaptor->adaptor = x;
    else
      adaptor = x;

    if (is_list_opt) {
      Type::Base *t = x->types.front()->component()->left();

      if (x->types.front()->is(Type::LIST) &&
          product.get_adp_specialization() != ADP_Mode::STANDARD) {
          t = new Type::List(t);
      }

      assert(t);
      x->add_para(new Type::Referencable(t), new std::string("left"));
      return;
    }
  }

  // sorting for adaptors taking forwards tabulated data
  for (Statement::iterator i = Statement::begin(y->stmts);
       i != Statement::end(); ++i) {
    Statement::Base *s = *i;
    if (s->is(Statement::SORTER)) {
        Statement::Sorter *so = dynamic_cast<Statement::Sorter*>(s);

        *i = new Statement::Sorter(so->op, so->list);
        (*i)->disable();
    }
  }

  for (Statement::iterator i = Statement::begin(x->stmts);
       i != Statement::end(); ++i) {
    Statement::Base *s = *i;
    if (s->is(Statement::VAR_DECL)) {
      Statement::Var_Decl *v = dynamic_cast<Statement::Var_Decl*>(s);
      if (*v->name == "left") {
        v = v->clone();
        v->disable();
        *i = v;

        ++i;
        Statement::Var_Decl *w = dynamic_cast<Statement::Var_Decl*>(*i);
        assert(w);
        w = w->clone();
        x->add_para(new Type::Referencable(w->type), w->name);

        y->add_para(new Type::Referencable(w->type), w->name);

        Statement::Return *r = dynamic_cast<Statement::Return*>
          (y->stmts.back());
        assert(r);
        Expr::Fn_Call *f = dynamic_cast<Expr::Fn_Call*>(r->expr);
        assert(f);
        f = f->clone();
        f->add_arg(*w);

        Statement::Return *ret = new Statement::Return(f);
        y->stmts.pop_back();
        y->stmts.push_back(ret);

        w->disable();
        *i = w;
        break;
      }
    }
  }
}

void Fn_Def::replace_types(std::pair<std::string*, Type::Base*> &alph,
      std::pair<std::string*, Type::Base*> &answer) {
  Fn_Decl::replace_types(alph, answer);
  assert(paras.size() == types.size());
  std::list<Para_Decl::Base*>::iterator j = paras.begin();
  for (std::list<Type::Base*>::iterator i = types.begin(); i != types.end();
      ++i, ++j)
    (*j)->replace(*i);
}

Fn_Def *Fn_Def::copy_head(Type::Base *t, std::string *s) {
  Fn_Def *f = new Fn_Def(t, s);
  f->types = types;
  f->names = names;
  f->paras = paras;
  return f;
}

void Fn_Def::set_ntparas(std::list<Para_Decl::Base*> *l) {
  if (!l)
    return;
  ntparas_ = *l;
  for (std::list<Para_Decl::Base*>::iterator i = ntparas_.begin();
       i != ntparas_.end(); ++i) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    assert(s);
    nttypes_.push_back(s->type());
  }
}

bool Fn_Def::check_ntparas(const Fn_Decl &d) {
  if (ntparas_.size() != d.nttypes().size()) {
    Log::instance()->error(location, "Number of nt parameters does not");
    Log::instance()->error(d.location, "match.");
    return false;
  }
  bool r = true;
  std::list<Type::Base*>::const_iterator j = d.nttypes().begin();
  for (std::list<Para_Decl::Base*>::iterator i = ntparas_.begin();
       i != ntparas_.end();
       ++i, ++j) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    if (!s) {
      Log::instance()->error((*i)->location(), "No simple parameter.");
      r = false;
      continue;
    }
    if (!s->type()->is_eq(**j)) {
      Log::instance()->error(s->location(), "Types does not");
      Log::instance()->error((*j)->location, "match.");
      r = false;
    }
  }
  return r;
}

Fn_Def *Fn_Def::copy() const {
  Fn_Def *o = new Fn_Def(*this);
  o->name = new std::string(*name);
  o->stmts.clear();
  for (std::list<Statement::Base*>::const_iterator i = stmts.begin();
       i != stmts.end(); ++i)
    o->stmts.push_back((*i)->copy());
  o->names.clear();
  for (std::list<std::string*>::const_iterator i = names.begin();
       i != names.end(); ++i)
    o->names.push_back(new std::string(**i));
  o->paras.clear();
  for (std::list<Para_Decl::Base*>::const_iterator i = paras.begin();
       i != paras.end(); ++i)
    o->paras.push_back((*i)->copy());
  return o;
}
