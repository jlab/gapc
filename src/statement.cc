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
#include <cstdlib>
#include <algorithm>
#include <list>
#include <utility>
#include <string>

#include "statement.hh"

#include "var_acc.hh"

#include "expr.hh"
#include "printer.hh"

#include "cc.hh"

#include "type.hh"

#include "operator.hh"

Statement::Sorter::Sorter(Operator *op, Var_Decl *l) :
  Block_Base(SORTER), list(l) {
    this->op = op->object;
}

Statement::Var_Decl::Var_Decl(::Type::Base *t, Expr::Base *e, Expr::Base *f)
  : Base(VAR_DECL), type(t), rhs(f) {
  Expr::Vacc *v = dynamic_cast<Expr::Vacc*>(e);
  assert(v);
  name = v->name();
  assert(name);
}


Statement::Var_Decl::Var_Decl(const Var_Decl &v)
  : Base(VAR_DECL), name(NULL), rhs(NULL) {
  type = v.type;
}


Statement::Var_Decl::Var_Decl(::Type::Base *t, std::string *n)
  : Base(VAR_DECL), type(t), name(n), rhs(NULL) {
}


Statement::Var_Decl::Var_Decl(::Type::Base *t, std::string *n, Expr::Base *e)
  : Base(VAR_DECL), type(t), name(n), rhs(e) {
}


Statement::Var_Decl *Statement::Var_Decl::clone() const {
  Var_Decl *ret = new Var_Decl(*this);
  ret->disabled_ = disabled_;
  ret->use_as_itr = use_as_itr;
  ret->name = name;
  ret->rhs = rhs;
  return ret;
}


void Statement::Var_Decl::print(Printer::Base &p) const {
  p.print(*this);
}


void Statement::Return::print(Printer::Base &p) const {
  p.print(*this);
}


void Statement::Break::print(Printer::Base &p) const {
  p.print(*this);
}

void Statement::Decrease::print(Printer::Base &p) const {
  p.print(*this);
}

void Statement::Increase::print(Printer::Base &p) const {
  p.print(*this);
}

void Statement::Continue::print(Printer::Base &p) const {
  p.print(*this);
}


void Statement::If::print(Printer::Base &p) const {
  p.print(*this);
}


void Statement::Switch::print(Printer::Base &p) const {
  p.print(*this);
}

void Statement::For::print(Printer::Base &p) const {
  p.print(*this);
}


void Statement::Foreach::print(Printer::Base &p) const {
  p.print(*this);
}

void Statement::Sorter::print(Printer::Base &p) const {
  p.print(*this);
}


void Statement::Var_Assign::print(Printer::Base &p) const {
  p.print(*this);
}


void Statement::Block::print(Printer::Base &p) const {
  p.print(*this);
}

void Statement::CustomCode::print(Printer::Base &p) const {
  p.print(*this);
}

Statement::Var_Assign::Var_Assign(Var_Decl &a)
  : Base(VAR_ASSIGN), op_(Expr::EQ), rhs(NULL) {
  acc = new Var_Acc::Plain(a);
}


Statement::Var_Assign::Var_Assign(Var_Decl &a, Var_Decl &b)
  : Base(VAR_ASSIGN), op_(Expr::EQ) {
  acc = new Var_Acc::Plain(a);
  rhs = new Expr::Vacc(b);
}


Statement::Var_Assign::Var_Assign(Var_Decl &a, Expr::Base *b)
  : Base(VAR_ASSIGN), op_(Expr::EQ), rhs(b) {
  acc = new Var_Acc::Plain(a);
}


Statement::Var_Assign::Var_Assign(Var_Acc::Base *a, Expr::Base *b, const Loc &l)
  : Base(VAR_ASSIGN, l), op_(Expr::EQ), acc(a), rhs(b) {
}


Statement::Var_Assign::Var_Assign(Var_Acc::Base *a, Expr::Base *b)
  : Base(VAR_ASSIGN), op_(Expr::EQ), acc(a), rhs(b) {
}


Statement::Var_Assign::Var_Assign(Var_Acc::Base *a, Var_Decl &v)
  : Base(VAR_ASSIGN), op_(Expr::EQ), acc(a) {
  rhs = new Expr::Vacc(v);
}


Var_Acc::Base *Statement::Var_Decl::left() {
  ::Type::Tuple *t = dynamic_cast< ::Type::Tuple*>(type->simple());
  assert(t);
  assert(t->list.size() == 2);
  std::list<std::pair< ::Type::Name*, std::string*>*>::iterator i =
    t->list.begin();
  Var_Acc::Comp *ret = new Var_Acc::Comp(new Var_Acc::Plain(name),
                                         (*i)->second);
  if (use_as_itr) {
    ret->set_itr(true);
  }
  return ret;
}


Var_Acc::Base *Statement::Var_Decl::right() {
  ::Type::Tuple *t = dynamic_cast< ::Type::Tuple*>(type->simple());
  assert(t);
  assert(t->list.size() == 2);
  std::list<std::pair< ::Type::Name*, std::string*>*>::iterator i =
    t->list.begin();
  ++i;
  Var_Acc::Comp *ret = new Var_Acc::Comp(new Var_Acc::Plain(name),
                                         (*i)->second);
  if (use_as_itr) {
    ret->set_itr(true);
  }
  return ret;
}


Statement::Return::Return(Var_Decl &vdecl)
  : Base(RETURN) {
  expr = new Expr::Vacc(vdecl);
}


Statement::Return::Return(std::string *n)
  : Base(RETURN) {
  expr = new Expr::Vacc(n);
}


void Statement::If::push(std::list<Base*> &l, Base* stmt) {
  if (stmt->is(BLOCK)) {
    Block *b = dynamic_cast<Block*>(stmt);
    assert(b);
    l.insert(l.end(), b->statements.begin(), b->statements.end());
  } else {
    l.push_back(stmt);
  }
}


Statement::Var_Decl *Statement::Var_Decl::var_decl() {
  return this;
}


void Statement::Var_Decl::replace(Var_Decl &decl, Expr::Base *expr) {
  if (!rhs) {
    return;
  }
  if (*rhs == decl) {
    rhs = expr;
  }
}


void Statement::Foreach::replace(Var_Decl &decl, Expr::Base *expr) {
  if (*container == decl) {
    Expr::Vacc *vacc = expr->vacc();
    assert(vacc->var_decl()->type->is(::Type::LIST));
    container = vacc->var_decl();
    assert(container);
  }
}


#include "expr/fn_call.hh"


void Statement::If::replace(Var_Decl &decl, Expr::Base *expr) {
  for (Expr::iterator i = Expr::begin(cond); i != Expr::end(); ++i) {
    if (!(*i)->is(Expr::FN_CALL)) {
      continue;
    }
    Expr::Fn_Call *f = (*i)->fn_call();
    f->replace(decl, expr);
  }
}


bool Statement::Var_Decl::operator==(const Var_Decl &other) const {
  return this == &other;
}


void Statement::Iterator::fwd() {
  while (true) {
    if (i == j) {
      if (stack.empty()) {
        end = true;
        break;
      }
      stuple p(stack.top());
      stack.pop();
      i = boost::get<0>(p);
      j = boost::get<1>(p);
      list = boost::get<2>(p);
      if (i == j) {
        continue;
      }
    }
    break;
  }
}


Statement::Iterator &Statement::Iterator::operator++() {
  if ((*i)->is(Statement::BLOCK) ||
      (*i)->is(Statement::FOREACH) ||
      (*i)->is(Statement::FOR)) {
    Statement::Base *t = *i;
    std::list<Statement::Base*> *l =  t->stmts();
    ++i;
    stack.push(boost::make_tuple(i, j, list));
    i = l->begin();
    j = l->end();
    list = l;
    fwd();
  } else if ((*i)->is(Statement::IF)) {
    Statement::Base *t = *i;
    Statement::If *s = dynamic_cast<Statement::If*>(t);
    assert(s);
    ++i;
    stack.push(boost::make_tuple(i, j, list));
    stack.push(boost::make_tuple(s->els.begin(), s->els.end(), &s->els));
    i = s->then.begin();
    j = s->then.end();
    list = &s->then;
    assert(!list->empty());
  } else {
    ++i;
    fwd();
  }
  return *this;
}


std::list<Statement::Base*> *Statement::Switch::add_case(std::string *n) {
  std::string *name = new std::string(*n);
  std::list<Base*> cont;
  std::pair<std::string, std::list<Base*> > newCase = std::make_pair(
    *name,  cont);
  cases.push_back(newCase);
  return &cases.back().second;
}


Statement::Foreach::Foreach(Var_Decl *i, Var_Decl *l)
  : Block_Base(FOREACH), elem(i), container(l), iteration(true) {
  assert(elem);
  assert(container);
}


void Statement::Foreach::set_itr(bool x) {
  // elem = elem->clone();
  elem->set_itr(x);
}

void Statement::Foreach::set_iteration(bool b) {
    iteration = b;
}


void Statement::Var_Assign::set_op(Expr::Type t) {
  op_ = t;
}


std::string Statement::Var_Assign::op_str() const {
  switch (op_) {
    case Expr::EQ:
      return std::string("=");
      break;
    case Expr::PLUS:
      return std::string("+=");
      break;
    default:
      assert(0);
      std::abort();
  }
}


Statement::Base *Statement::Return::copy() const {
  Return *o = new Return(*this);
  if (expr) {
    o->expr = expr->copy();
  }
  return o;
}

Statement::Base *Statement::Break::copy() const {
  Break *o = new Break(*this);
  return o;
}


Statement::Base *Statement::Continue::copy() const {
  Continue *o = new Continue(*this);
  return o;
}

Statement::Base *Statement::Decrease::copy() const {
  Decrease *o = new Decrease(*this);
  return o;
}

Statement::Base *Statement::Increase::copy() const {
  Increase *o = new Increase(*this);
  return o;
}

Statement::Base *Statement::If::copy() const {
  If *o = new If(*this);
  if (cond) {
    o->cond = cond->copy();
  }
  o->then.clear();
  o->els.clear();
  for (std::list<Base*>::const_iterator i=then.begin(); i != then.end(); ++i) {
    o->then.push_back((*i)->copy());
  }
  for (std::list<Base*>::const_iterator i=els.begin(); i != els.end(); ++i) {
    o->els.push_back((*i)->copy());
  }
  return o;
}


Statement::Base *Statement::Var_Decl::copy() const {
  Var_Decl *o = new Var_Decl(*this);
  if (rhs) {
    o->rhs = rhs->copy();
  }
  if (name) {
    o->name = new std::string(*name);
  }
  return o;
}


Statement::Base *Statement::For::copy() const {
  For *o = new For(*this);
  Block_Base::copy(*o);
  if (cond) {
    o->cond = cond->copy();
  }
  if (inc) {
    o->inc = inc->copy();
  }
  return o;
}


Statement::Base *Statement::Var_Assign::copy() const {
  Var_Assign *o = new Var_Assign(*this);
  if (rhs) {
    o->rhs = rhs->copy();
  }
  return o;
}


Statement::Base *Statement::Block::copy() const {
  Block *o = new Block(*this);
  Block_Base::copy(*o);
  return o;
}


Statement::Base *Statement::CustomCode::copy() const {
  CustomCode *o = new CustomCode(*this);
  o->line_of_code = std::string(line_of_code);
  return o;
}
