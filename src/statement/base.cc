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

#include <cstdlib>

#include "base.hh"

#include "../cc.hh"

#include "../fn_def.hh"

Statement::Base::~Base() {}

std::ostream &operator<<(std::ostream &s, const Statement::Base &b) {
  Printer::CC cc(s);
  b.print(cc);
  return s;
}

/* this function shall only be used for debugging purposes, it can print a
 * list of statements to a stream */
std::ostream &operator<<(std::ostream &s,
                         const std::list<Statement::Base*> &stmts) {
  Printer::CC cc(s);
  if (stmts.size() == 0) {
    s << "empty statement list!\n";
  }
  unsigned int counter = 1;
  for (std::list<Statement::Base*>::const_iterator b = stmts.begin();
       b != stmts.end(); ++b, ++counter) {
    s << "=== start statement " << counter << "/" << stmts.size() << " ====\n";
    (*b)->print(cc);
    s << "\n--- end statement " << counter << "/" << stmts.size() << " ----\n";
  }
  return s;
}

namespace Statement {

iterator begin(std::list<Statement::Base*> &l) {
  return Iterator(l);
}

iterator begin(Fn_Def &fn) {
  return Iterator(fn.stmts);
}

iterator end() {
  return Iterator();
}

}  // namespace Statement


std::list<Statement::Base*> *Statement::Base::stmts() {
  std::abort();
  return 0;
}

Statement::Var_Decl *Statement::Base::var_decl() {
  std::abort();
  return 0;
}

void Statement::Base::replace(Var_Decl &decl, Expr::Base *expr) {
}

Statement::Base *Statement::Base::copy() const {
  assert(23 == 0);
  return 0;
}
