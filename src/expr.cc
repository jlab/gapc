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
#include <string>
#include "expr.hh"
#include "yieldsize.hh"
#include "const.hh"

#include "statement.hh"


Expr::Const::Const(const Yield::Poly &p) : Base(CONST) {
  assert(p != Yield::Poly(Yield::UP));
  base = new ::Const::Size(p.konst());
}

Expr::Const::Const(const Yield::Size &ys)
  : Base(CONST) {
  assert(ys.low() != Yield::Poly(Yield::UP));
  assert(ys.high() != Yield::Poly(Yield::UP));
  assert(ys.low().konst() <= ys.high().konst());
  base = new ::Const::Size(ys.high().konst() - ys.low().konst() + 1);
}

Expr::Const::Const(int p) : Base(CONST) {
  base = new ::Const::Size(p);
}

Expr::Const::Const(double d) : Base(CONST) {
  base = new ::Const::Float(d);
}

Expr::Const::Const(const std::string &s) : Base(CONST) {
  base = new ::Const::String(s);
}

Expr::Const::Const(char c) : Base(CONST) {
  base = new ::Const::Char(c);
}

Expr::Greater::Greater(Base *l, const Yield::Poly &p) : Two(GREATER, l, NULL) {
  Expr::Const *c = new Expr::Const(p);
  right_ = c;
}

Expr::Less::Less(Base *l, const Yield::Poly &p) : Two(LESS, l, NULL) {
  Expr::Const *c = new Expr::Const(p);
  right_ = c;
}

Expr::Greater_Eq::Greater_Eq(Base *l, const Yield::Poly &p)
: Two(GREATER_EQ, l, NULL) {
  set_pretty_op(">=");
  Expr::Const *c = new Expr::Const(p);
  right_ = c;
}

Expr::Less_Eq::Less_Eq(Base *l, const Yield::Poly &p) : Base(LESS_EQ), lhs(l) {
  Expr::Const *c = new Expr::Const(p);
  rhs = c;
}


void Expr::Comp::put(std::ostream &s) const {
  s << " ( " << *expr << " ) ";
}

void Expr::Plus::put(std::ostream &s) const {
  s << '(' << *left_ << " + " << *right_ << ')';
}

void Expr::Minus::put(std::ostream &s) const {
  s << '(' << *left_ << " - " << *right_ << ')';
}

void Expr::Const::put(std::ostream &s) const {
  s << *base;
}
void Expr::Const::put_noquote(std::ostream &s) const {
  ::Const::String *basestring = dynamic_cast<::Const::String*>(this->base);
  // SMJ 2022-05-06: I've only seen quotes printing out for Strings
  // However, for plotGrammar *.dot creation, " must be properly escaped,
  // which is handled in alt.cc and requires an un-quoted version here
  assert(basestring);
  basestring->put_noquote(s);
}

void Expr::Less_Eq::put(std::ostream &s) const {
  s << '(' << *lhs << " <= " << *rhs << ')';
}

void Expr::Less::put(std::ostream &s) const {
  s << '(' << *left_ << " < " << *right_ << ')';
}


void Expr::Greater::put(std::ostream &s) const {
  s << '(' << *left_ << " > " << *right_ << ')';
}

void Expr::And::put(std::ostream &s) const {
  s << '(' << *left_ << " && " << *right_ << ')';
}

void Expr::Max::put(std::ostream &s) const {
  s << "max(" << *left << " ," << *right << ')';
}

void Expr::Cond::put(std::ostream &s) const {
  s << "((" << *cond << ") ? (" << *then << ") : (" << *els << "))";
}

void Expr::Not::put(std::ostream &s) const {
  s << '!' << *base;
}

Expr::Eq::Eq(Var_Acc::Base *vacc, Statement::Var_Decl *v)
        : Two(EQ) {
  set_pretty_op("==");
  left_ = new Expr::Vacc(vacc);
  right_ = new Expr::Vacc(*v);
}


Expr::Base *Expr::Base::plus(Base *b) {
  assert(b);
  Expr::Plus *r = new Expr::Plus(this, b);
  return r;
}

Expr::Base *Expr::Base::plus(const Yield::Poly &p) {
  assert(p != Yield::Poly(Yield::UP));
  if (p == 0)
    return this;
  Expr::Const *c = new Expr::Const(p);
  Expr::Base *r = plus(c);
  return r;
}

Expr::Base *Expr::Base::minus(Base *b) {
  assert(b);
  assert(this);
  Expr::Minus *r = new Expr::Minus(this, b);
  return r;
}

Expr::Base *Expr::Base::minus(const Yield::Poly &p) {
  assert(p != Yield::Poly(Yield::UP));
  if (p == 0)
    return this;
  Expr::Const *c = new Expr::Const(p);
  Expr::Base *r = minus(c);
  return r;
}

bool operator==(Expr::Base &expr, const Statement::Var_Decl &decl) {
  if (!expr.is(Expr::VACC))
    return false;
  Expr::Vacc *v = expr.vacc();
  Statement::Var_Decl *d = v->var_decl();
  // assert(d);
  if (!d)
    return false;
  return *d == decl;
}

Expr::Base *Expr::Comp::copy() const {
  Comp *o = new Comp(*this);
  o->expr = expr->copy();
  return o;
}

Expr::Base *Expr::Const::copy() const {
  Const *o = new Const(*this);
  return o;
}

Expr::Base *Expr::Less_Eq::copy() const {
  Less_Eq *o = new Less_Eq(*this);
  o->lhs = lhs->copy();
  o->rhs = rhs->copy();
  return o;
}

Expr::Base *Expr::Max::copy() const {
  Max *o = new Max(*this);
  o->left = left->copy();
  o->right = right->copy();
  return o;
}

Expr::Base *Expr::Cond::copy() const {
  Cond *o = new Cond(*this);
  o->cond = cond->copy();
  o->then = then->copy();
  o->els = els->copy();
  return o;
}

Expr::Base *Expr::Not::copy() const {
  Not *o = new Not(*this);
  o->base = base->copy();
  return o;
}


#define EXPRTWOCP(A) Expr::Base *Expr::A::copy() const { \
  A *o = new A(*this); Two::copy(*o); return o; }

EXPRTWOCP(Plus)
EXPRTWOCP(Minus)
EXPRTWOCP(Times)
EXPRTWOCP(Div)
EXPRTWOCP(Less)
EXPRTWOCP(Greater)
EXPRTWOCP(Greater_Eq)
EXPRTWOCP(Eq)
EXPRTWOCP(Not_Eq)
EXPRTWOCP(And)
EXPRTWOCP(Or)

#include "expr/mod.hh"
EXPRTWOCP(Mod)

#undef EXPRTWOCP
