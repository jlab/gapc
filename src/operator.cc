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

#include "operator.hh"
#include <string>
#include "para_decl.hh"
#include "statement.hh"


// add new parameter to all lists
void Operator::add_para(Type::Base *type, std::string *n) {
  // no security checks like for functions
  // because there is no user contact with this function

  paras.push_back(new Para_Decl::Simple(type, n));
}


void Operator::add_const_value(Statement::Var_Decl *v) {
    // no sanity checks because only internal use
    // but, v MUST have a constant RHS

    const_values.push_back(v);
}
