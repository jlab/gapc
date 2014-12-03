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

#include "hash_decl.hh"

#include "../printer.hh"

Statement::Hash_Decl::Hash_Decl()
  : Statement::Base(HASH_DECL), answer_type_(0)
{
}

void Statement::Hash_Decl::print(Printer::Base &p) const
{
  p.print(*this);
}

void Statement::Hash_Decl::set_suffix(const std::string &n)
{
  name_ = "hash_" + n;
}

#include "../statement.hh"
#include "../expr.hh"
#include "../const.hh"

void Statement::Hash_Decl::set_kbest(bool b)
{
  kbest_ = b;
  if (kbest_) {
    cutoff_code_.push_back(new Statement::Return(new Expr::Const(new Const::Bool(true))));
    k_code_.push_back(new Statement::Return(new Expr::Vacc(new std::string("k_"))));
  } else {
    cutoff_code_.push_back(new Statement::Return(new Expr::Const(new Const::Bool(false))));
    k_code_.push_back(new Statement::Return(new Expr::Const(0)));
  }
  if (equal_score_code_.empty())
    equal_score_code_.push_back(new Statement::Return(new Expr::Const(new Const::Bool(false))));
  if (compare_code_.empty())
    compare_code_.push_back(new Statement::Return(new Expr::Const(new Const::Bool(false))));
}

std::string Statement::Hash_Decl::ext_name() const
{
  return class_name_ + "_insp_" + name_;
}
std::string Statement::Hash_Decl::name() const
{
  return class_name_ + "_" + name_;
}
