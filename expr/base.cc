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

#include "base.hh"

#include <cstdlib>

Expr::Base::~Base() {}


Statement::Var_Decl *Expr::Base::var_decl()
{
  return NULL;
}

Expr::Vacc *Expr::Base::vacc()
{
  std::abort();
  return 0;
}

Expr::Fn_Call *Expr::Base::fn_call()
{
  std::abort();
  return 0;
}

void Expr::Base::put(std::ostream &s) const
{
}


void Expr::Two::put(std::ostream &s) const
{
  assert(op != "");
  s << '(' << *left_ << ' ' << op << ' ' << *right_ << ')';
}

Expr::Base *Expr::Base::copy() const
{
  assert(42 == 0);
  return 0;
}

void Expr::Two::copy(Expr::Two &o) const
{
  assert(left_);
  assert(right_);
  o.left_ = left_->copy();
  o.right_ = right_->copy();
}
