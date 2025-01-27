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


#include "vacc.hh"

#include <string>
#include "../statement.hh"
#include "../var_acc.hh"


Expr::Vacc::Vacc(std::string *n)
  : Base(VACC) {
  Var_Acc::Plain *p = new Var_Acc::Plain (n);
  var_acc = p;
}


Expr::Vacc::Vacc(std::string* a, std::string *b)
  : Base(VACC) {
  Var_Acc::Plain *p = new Var_Acc::Plain (a);
  Var_Acc::Comp *c = new Var_Acc::Comp (p, b);
  var_acc = c;
}


Expr::Vacc::Vacc(Statement::Var_Decl &vdecl)
  : Base(VACC) {
  Var_Acc::Plain *p = new Var_Acc::Plain (vdecl);
  var_acc = p;
  if (vdecl.is_itr()) {
    p->set_itr(true);
  }
}


std::string * Expr::Vacc::name() {
  Var_Acc::Plain *p = dynamic_cast<Var_Acc::Plain*> (var_acc);
  assert(p);
  assert(p->name);
  return p->name;
}


void Expr::Vacc::put(std::ostream &s) const {
  s << *var_acc;
}


Statement::Var_Decl *Expr::Vacc::var_decl() {
  if (!var_acc->is(Var_Acc::PLAIN)) {
    return NULL;
  }
  Var_Acc::Plain *v = dynamic_cast<Var_Acc::Plain*> (var_acc);
  assert(v);
  return v->vdecl;
}


Expr::Vacc *Expr::Vacc::vacc() {
  return this;
}


Expr::Base *Expr::Vacc::copy() const {
  Vacc *o = new Vacc (*this);
  return o;
}
