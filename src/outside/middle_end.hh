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

#include <vector>
#include <list>
#include <utility>
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

  Parser(Yield::Size ys,
         std::vector<Expr::Base*> &left_indices,
         std::vector<Expr::Base*> &right_indices,
         std::list<Statement::For*> &simple_loops) :
           yield_size(ys),
           left_indices(left_indices),
           right_indices(right_indices),
           simple_loops(simple_loops) {
  }
};

void outside_init_indices(
    Alt::Base *alt,  // the top level alternative
    Expr::Vacc *left, Expr::Vacc *right,  // indices of lhs NT
    unsigned int &k, size_t track,
    // left/right borders of user input
    Expr::Vacc *left_most, Expr::Vacc *right_most);

Yield::Size sum_ys(std::vector<Parser*> parser,
    std::vector<Parser*>::iterator itr_start,
    std::vector<Parser*>::iterator itr_end,
    size_t track);

struct GetOutsideLink : public Visitor {
  Alt::Link *outside_link = nullptr;
  Fn_Arg::Alt *outside_fn_arg = nullptr;

  void visit(Alt::Link &a) {
    if (a.nt->is_partof_outside() || a.is_outside_inside_transition()) {
      this->outside_link = &a;
    }
  }
  void visit_end(Fn_Arg::Alt &f) {
    if (outside_link && !outside_fn_arg) {
      this->outside_fn_arg = &f;
    }
  }
};

#endif /* SRC_OUTSIDE_MIDDLE_END_HH_ */
