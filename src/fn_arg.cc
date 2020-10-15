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

#include <algorithm>

#include "fn_arg.hh"

#include "visitor.hh"

#include "const.hh"

#include "statement.hh"

#include "expr.hh"

#include <iostream>

Fn_Arg::Base::Base(Type t, const Loc &l)
  : type(t), productive(false), datatype(NULL), terminal_type(false),
  location(l) {
  }

Fn_Arg::Base::~Base() {}


Fn_Arg::Const::Const(::Const::Base *e, const Loc &l)
  : Base(CONST, l), expr_(e) {
  std::cout << "\n called Const of class fn_arg \n";
  productive = true;
  terminal_type = true;
  list_size_ = 1;

  init_multi_ys();
}

Fn_Arg::Base *Fn_Arg::Alt::clone() {
  std::cout << "\n called clone of class fn_arg \n";
  Alt *f = new Alt(*this);
  f->alt = alt->clone();
  return f;
}

Fn_Arg::Base *Fn_Arg::Const::clone() {
  std::cout << "\n called clone of class fn_arg \n";
  Const *f = new Const(*this);
  return f;
}

bool Fn_Arg::Base::init_links(Grammar &grammar) {
  std::cout << "\n called init_links of class fn_arg \n";
  return true; 
}

bool Fn_Arg::Alt::init_links(Grammar &g) {
  std::cout << "\n called init_links of class fn_arg \n";
  bool b = alt->init_links(g);
  return b;
}

bool Fn_Arg::Alt::init_productive() {
  std::cout << "\n called init_productive of class fn_arg \n";
  bool r = alt->init_productive();
  productive = alt->is_productive();
  return r;
}

bool Fn_Arg::Const::init_productive() {
  std::cout << "\n called init_productive of class fn_arg \n";
  return false;
}

size_t Fn_Arg::Alt::width() {
  std::cout << "\n called width of class fn_arg \n";
  return alt->width();
}

size_t Fn_Arg::Const::width() {
  std::cout << "\n called width of class fn_arg \n";
  if (expr_->yield_size().high() == Yield::UP)
    return 1;
  else
    return 0;
}


void Fn_Arg::Alt::print_link(std::ostream &s) {
  std::cout << "\n called print_link of class fn_arg \n";
  alt->print_link(s);
}

void Fn_Arg::Const::print_link(std::ostream &s) {
}

bool Fn_Arg::Alt::is(::Alt::Type t) {
  std::cout << "\n called is of class fn_arg \n";
  return alt->is(t);
}

bool Fn_Arg::Const::is(::Alt::Type t) {
  std::cout << "\n called is of class fn_arg \n";
  return false;
}

Runtime::Poly Fn_Arg::Alt::runtime(
  std::list<Symbol::NT*> &active_list, Runtime::Poly accum_rt) {
  std::cout << "\n called runtime of class fn_arg \n";
  return alt->runtime(active_list, accum_rt);
}

Runtime::Poly Fn_Arg::Const::runtime(
  std::list<Symbol::NT*> &active_list, Runtime::Poly accum_rt) {
  std::cout << "\n called runtime of class fn_arg \n";
  return 1;
  // just const, does not change the module test
  // return calls;
}

Runtime::Poly Fn_Arg::Alt::init_in_out() {
  std::cout << "\n called init_in_out of class fn_arg \n";
  return alt->init_in_out();
}

Runtime::Poly Fn_Arg::Const::init_in_out() {
  std::cout << "\n called init_in_out of class fn_arg \n";
  // before: return calls
  // since this is just a const call, it make more sense to just return 1
  // (does not change the module test results, anyways)
  return 1;
}

bool Fn_Arg::Alt::set_data_type(::Type::Base *t, const Loc &l) {
  std::cout << "\n called set_data_type of class fn_arg \n";
  bool b = ::Type::set_if_compatible(datatype, t, location, l);
  return b;
}

bool Fn_Arg::Const::set_data_type(::Type::Base *t, const Loc &l) {
  std::cout << "\n called set_data_type of class fn_arg \n";
  bool b = ::Type::set_if_compatible(datatype, t, location, l);
  bool r = expr_->set_data_type(t, l);
  return b && r;
}

#include "type/multi.hh"
#include "symbol.hh"
#include "fn_decl.hh"

::Type::Status Fn_Arg::Alt::infer_missing_types() {
  std::cout << "\n called infer_missing_types of class fn_arg \n";
  ::Type::Status r = alt->infer_missing_types();

  assert(datatype);
  if (!terminal_type && alt->terminal_type) {
    terminal_type = true;
    datatype->set_terminal();
    r = ::Type::RUNNING;
  }

  ::Type::Base *t = alt->data_type();



  if (t) {
    t = t->simple();
    if (t->is(::Type::LIST))
      t = dynamic_cast< ::Type::List*>(t)->of;


    if (t->is(::Type::MULTI)) {
      ::Type::Multi *a = dynamic_cast< ::Type::Multi*>(t);
      ::Type::Multi *b = dynamic_cast< ::Type::Multi*>(datatype);
      if (!a || !b) {
        Log::instance()->error(location, "#tracks of types does not match");
        return ::Type::ERROR;
      }

      std::list< ::Type::Base*>::const_iterator j = b->types().begin();
      for (std::list< ::Type::Base*>::const_iterator i = a->types().begin();
          i != a->types().end(); ++i, ++j) {
        if ((*i)->is_terminal()) {
          (*j)->set_terminal();
        }
      }
    }

    bool b = false;
    if (alt->is(::Alt::LINK)) {
      ::Alt::Link *l = dynamic_cast< ::Alt::Link*>(alt);
      if (l->nt->is(Symbol::NONTERMINAL)) {
        Symbol::NT *nt = dynamic_cast<Symbol::NT*>(l->nt);
        if (nt->has_eval_fn()) {
          b = set_data_type(t, nt->eval_decl->location);
        } else {
          b = set_data_type(t, nt->location);
        }
      } else {
        b = set_data_type(t, alt->location);
      }
    } else {
      b = set_data_type(t, alt->location);
    }
    if (!b)
      return ::Type::ERROR;
    return r;
  } else {
    return std::max(r, ::Type::RUNNING);
  }
}

::Type::Status Fn_Arg::Const::infer_missing_types() {
  std::cout << "\n called print_type of class fn_arg \n";
  return ::Type::READY;
}

void Fn_Arg::Alt::print_type(std::ostream &s) {
  std::cout << "\n called infer_missing_types of class fn_arg \n";
  if (datatype)
    s << *datatype;
  else
    s << "NULL";
  s << "< ";
  alt->print_type(s);
  s << " >";
}

void Fn_Arg::Const::print_type(std::ostream &s) {
  std::cout << "\n called print_type of class fn_arg \n";
  if (datatype)
    s << *datatype;
  else
    s << "NULL";
  s << "-< ";
  expr_->print_type(s);
  s << " >-";
}

bool Fn_Arg::Alt::returns_list() {
  std::cout << "\n called returns_list of class fn_arg \n";
  if (alt->is(::Alt::MULTI)) {
    if (alt->data_type()->simple()->is(::Type::LIST))
      return true;
    std::list< ::Type::Base*> types;
    ::Alt::Multi *m = dynamic_cast< ::Alt::Multi*>(alt);
    m->types(types);
    for (std::list< ::Type::Base*>::iterator i = types.begin();
        i != types.end(); ++i)
      if ((*i)->simple()->is(::Type::LIST))
        return true;
    return false;
  } else {
    return alt->data_type()->simple()->is(::Type::LIST);
  }

  assert(!var_decls().empty());
  for (std::vector<Statement::Var_Decl*>::const_iterator i =
       var_decls().begin(); i != var_decls().end(); ++i) {
    if (*i) {
      return true;
    }
  }
  return false;
}

bool Fn_Arg::Const::returns_list() {
  std::cout << "\n called returns_list of class fn_arg \n";
  return expr_->data_type()->simple()->is(::Type::LIST);
}

::Type::Base *Fn_Arg::Alt::ext_data_type() {
  std::cout << "\n called ext_data_type of class fn_arg \n";
  return alt->data_type()->simple();
}

::Type::Base *Fn_Arg::Const::ext_data_type() {
  std::cout << "\n called ext_data_type of class fn_arg \n";
  return expr_->data_type()->simple();
}

void Fn_Arg::Base::reset_types() {
  std::cout << "\n called reset_types of class fn_arg \n";
  datatype = NULL;
}

const Yield::Poly& Fn_Arg::Alt::list_size() const {
  std::cout << "\n called list_size of class fn_arg \n";
  return alt->list_size();
}

const Yield::Poly& Fn_Arg::Const::list_size() const {
  std::cout << "\n called list_size of class fn_arg \n";
  return list_size_;
}


void Fn_Arg::Alt::traverse(Visitor &v) {
  std::cout << "\n called traverse of class fn_arg \n";
  v.visit(*dynamic_cast<Base*>(this));
  v.visit(*this);
  alt->traverse(v);
}

void Fn_Arg::Const::traverse(Visitor &v) {
  std::cout << "\n called traverse of class fn_arg \n";
  v.visit(*dynamic_cast<Base*>(this));
  v.visit(*this);
}

void Fn_Arg::Base::set_tracks(size_t t) {
  std::cout << "\n called set_tracks of class fn_arg \n";
  left_indices.resize(t);
  right_indices.resize(t);
}

void Fn_Arg::Base::init_indices(Expr::Base *left, Expr::Base *right,
    unsigned int &k, size_t track) {
  std::cout << "\n called init_indices of class fn_arg \n";
  left_indices[track] = left;
  right_indices[track] = right;
}

void Fn_Arg::Alt::init_indices(Expr::Base *left, Expr::Base *right,
    unsigned int &k, size_t track) {
  std::cout << "\n called init_indices of class fn_arg \n";
  Base::init_indices(left, right, k, track);
  alt->init_indices(left, right, k, track);
}

void Fn_Arg::Const::init_indices(Expr::Base *left, Expr::Base *right,
    unsigned int &k, size_t track) {
  std::cout << "\n called init_indices of class fn_arg \n";
  Base::init_indices(left, right, k, track);
}

void Fn_Arg::Base::init_ret_decl(unsigned int i, const std::string &prefix) {
  std::cout << "\n called init_ret_decl of class fn_arg \n";
  ret_decls_.clear();
  var_decls_.clear();
  std::ostringstream a;
  a << prefix << "a_" << i;
  Statement::Var_Decl *ret_decl = new Statement::Var_Decl(ext_data_type(),
      new std::string(a.str()));
  ret_decls_.push_back(ret_decl);
  if (returns_list()) {
    std::ostringstream x;
    x << prefix << "x_" << i;
    Statement::Var_Decl *var_decl =  new Statement::Var_Decl(data_type(),
        new std::string(x.str()));
    var_decls_.push_back(var_decl);
  } else {
    var_decls_.push_back(0);
  }
}

void Fn_Arg::Alt::init_ret_decl(unsigned int i, const std::string &prefix) {
  std::cout << "\n called init_ret_decl of class fn_arg \n";
  if (!alt->is(::Alt::MULTI)) {
    Base::init_ret_decl(i, prefix);
    return;
  }
  ret_decls_.clear();
  var_decls_.clear();
  ::Alt::Multi *m = dynamic_cast< ::Alt::Multi*>(alt);
  assert(m);
  ::Type::Multi *mtype = dynamic_cast< ::Type::Multi*>(data_type());
  assert(mtype);
  std::list< ::Type::Base*> ext_types;
  m->types(ext_types);
  size_t t = 0;
  assert(mtype->types().size() == ext_types.size());
  std::list< ::Type::Base*>::iterator b = ext_types.begin();
  for (std::list< ::Type::Base*>::const_iterator a = mtype->types().begin();
      a != mtype->types().end(); ++a, ++b, ++t) {
    std::ostringstream u;
    u << prefix << "a_" << i << "_" << t;
    Statement::Var_Decl *ret_decl =
      new Statement::Var_Decl((*b)->simple(), new std::string(u.str()));
    ret_decls_.push_back(ret_decl);
    if (!(*b)->simple()->is(::Type::LIST)) {
      var_decls_.push_back(0);
      continue;
    }
    std::ostringstream v;
    v << prefix << "x_" << i << "_" << t;
    Statement::Var_Decl *var_decl =
      new Statement::Var_Decl((*a)->simple(), new std::string(v.str()));
    var_decls_.push_back(var_decl);
  }
}

void Fn_Arg::Alt::codegen(AST &ast) {
  std::cout << "\n called codegen of class fn_arg \n";
  alt->codegen(ast);
  // statements_ = alt->statements;
  assert(!ret_decls_.empty());
  if (ret_decls_.size() == 1) {
    ret_decls_.front()->rhs = new Expr::Vacc(*alt->ret_decl);
    return;
  }

  ::Alt::Multi *m = dynamic_cast< ::Alt::Multi*>(alt);
  assert(m);
  assert(ret_decls_.size() == m->ret_decls().size());
  std::list<Statement::Var_Decl*>::const_iterator j = m->ret_decls().begin();
  for (std::vector<Statement::Var_Decl*>::iterator i =
      ret_decls_.begin(); i != ret_decls_.end(); ++i, ++j)
    (*i)->rhs = new Expr::Vacc(**j);

  // statements_.push_back(ret_decl);
}

void Fn_Arg::Const::codegen(AST &ast) {
  std::cout << "\n called codegen of class fn_arg \n";
  assert(ret_decls_.size() == 1);
  ret_decls_.front()->rhs = new Expr::Const(expr_);
}

std::list<Statement::Base*> &Fn_Arg::Alt::statements() {
  std::cout << "\n called statements of class fn_arg \n";
  return alt->statements;
}

std::list<Statement::Base*> &Fn_Arg::Const::statements() {
  std::cout << "\n called statements of class fn_arg \n";
  return statements_;
}

void Fn_Arg::Base::print_dot_edge(std::ostream &out, Symbol::NT &nt) {
  std::cout << "\n called print_dot_edge of class fn_arg \n";
}

void Fn_Arg::Alt::print_dot_edge(std::ostream &out, Symbol::NT &nt) {
  std::cout << "\n called print_dot_edge of class fn_arg \n";
  alt->print_dot_edge(out, nt);
}

void Fn_Arg::Const::print(std::ostream &s) {
  std::cout << "\n called print of class fn_arg \n";
  s << *expr_;
}

void Fn_Arg::Alt::print(std::ostream &s) {
  std::cout << "\n called print of class fn_arg \n";
  alt->print(s);
}


void Fn_Arg::Const::init_multi_ys() {
  std::cout << "\n called init_multi_ys of class fn_arg \n";
  m_ys.set_tracks(1);
  m_ys(0) = expr_->yield_size();
}
const Yield::Multi &Fn_Arg::Alt::multi_ys() const {
  std::cout << "\n called init_multi_ys of class fn_arg \n";
  return alt->multi_ys();
}
void Fn_Arg::Alt::init_multi_ys() {
  std::cout << "\n called init_multi_ys of class fn_arg \n";
  alt->init_multi_ys();
}

Statement::Var_Decl *Fn_Arg::Base::ret_decl() {
  std::cout << "\n called ret_decl of class fn_arg \n";
  assert(ret_decls_.size() == 1);
  return ret_decls_.front();
}

Statement::Var_Decl *Fn_Arg::Base::var_decl() {
  std::cout << "\n called var_decl of class fn_arg \n";
  assert(var_decls_.size() == 1);
  return var_decls_.front();
}

bool Fn_Arg::Alt::choice_set() {
  std::cout << "\n called choice_set of class fn_arg \n";
  return alt->choice_set();
}

bool Fn_Arg::Const::choice_set() {
  std::cout << "\n called choice_set of class fn_arg \n";
  return false;
}
