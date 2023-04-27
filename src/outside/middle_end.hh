/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2011-2023  Stefan Janssen
         email: stefan.m.janssen@gmail.com or stefan.janssen@computational.bio.uni-giessen.de

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

#ifndef SRC_OUTSIDE_MIDDLE_END_HH_
#define SRC_OUTSIDE_MIDDLE_END_HH_


#include "../symbol.hh"
#include "../expr.hh"
#include "../fn_arg.hh"
#include "../alt.hh"
#include "../statement.hh"
#include "../visitor.hh"

class Parser {
 public:
  Yield::Size yield_size;
  std::vector<Expr::Base*> &left_indices;
  std::vector<Expr::Base*> &right_indices;
  std::list<Statement::For*> &simple_loops;

  Parser(Yield::Size ys, std::vector<Expr::Base*> &left_indices, std::vector<Expr::Base*> &right_indices, std::list<Statement::For*> &simple_loops) : yield_size(ys), left_indices(left_indices), right_indices(right_indices), simple_loops(simple_loops) {
  }
};

#include "../alt.hh"

void outside_init_indices(Alt::Base *alt, Expr::Vacc *left, Expr::Vacc *right, unsigned int &k, size_t track);

struct GetOutsideLink : public Visitor {
  Alt::Link *outside_link = nullptr;

  void visit(Alt::Link &a) {
    if (a.nt->is_partof_outside() || a.is_outside_inside_transition()) {
      this->outside_link = &a;
    }
  }
};

#endif /* SRC_OUTSIDE_MIDDLE_END_HH_ */
