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


#ifndef SRC_CC_HH_
#define SRC_CC_HH_

#include <list>
#include "printer.hh"

namespace Printer {

class CC : public Base {
 public:
  CC() : Base() {}
  explicit CC(std::ostream &o) : Base(o) {}
  void print(const Statement::For &stmt);
  void print(const Statement::Var_Decl &stmt);
  void print(const Statement::If &stmt);
  void print(const Statement::Return &stmt);
  void print(const Statement::Foreach &stmt);
  void print(const Statement::Var_Assign &stmt);
  void print(const Statement::Fn_Call &stmt);
  void print(const Statement::Block &stmt);
  
  void print(const Fn_Def &fn_def);


  void print(const std::list<Statement::Base*> &stmts);

  void print(const Expr::Base &expr);
  void print(const Type::Base &);
  void print(const Var_Acc::Base &);
};

}  // namespace Printer

#endif  // SRC_CC_HH_
