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
#include <utility>
#include <string>
#include "var_acc.hh"

#include "statement.hh"
#include "type.hh"

#include "expr.hh"

Var_Acc::Plain::Plain(Statement::Var_Decl &a) : Base(PLAIN), name(NULL) {
  vdecl = &a;
}

Var_Acc::Base::~Base() {}

void Var_Acc::Base::put(std::ostream &s) const {
}

void Var_Acc::Plain::put(std::ostream &s) const {
  if (itr_access)
    s << "(*";
  if (name)
    s << *name;
  else
    s << *vdecl->name;
  if (itr_access)
    s << ')';
}

void Var_Acc::Comp::put(std::ostream &s) const {
  if (itr_access)
    s << "(*" << *lhs << ")." << *rhs;
  else
    s << *lhs << '.' << *rhs;
}

void Var_Acc::Array::put(std::ostream &s) const {
  s << *lhs << '[' << *expr << ']';
}

Var_Acc::Comp::Comp(const Statement::Var_Decl &vdecl, int n) : Base(COMP) {
  Plain *p = new Var_Acc::Plain(vdecl.name);
  assert(vdecl.type->is(::Type::TUPLE));
  ::Type::Tuple *t = dynamic_cast< ::Type::Tuple*>(vdecl.type);
  assert(t);
  std::list<std::pair< ::Type::Name*, std::string*>*>::iterator j =
    t->list.begin();
  assert(j != t->list.end());
  for (int i = 0; i < n; ++i) {
    ++j;
    assert(j != t->list.end());
  }
  lhs = p;
  rhs = (*j)->second;
}
