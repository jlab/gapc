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

#include "middle_end.hh"

Expr::Base *next_index_var(unsigned &k, size_t track,
  Expr::Base *next_var, Expr::Base *last_var, Expr::Base *right,
  const Yield::Size &ys, const Yield::Size &lhs, const Yield::Size &rhs) {
  if (ys.low() != ys.high()) {
    if (rhs.low() == rhs.high()) {
      return right;
    } else {
      std::ostringstream o;
      o << "t_" << track << "_k_" << k;
      k++;
      Expr::Vacc *ivar = new Expr::Vacc(new std::string(o.str()));

      assert(lhs.low() == lhs.high());
      std::pair<Expr::Base*, Expr::Base*> index(0, 0);

      Yield::Size lhs_ys(lhs);
      lhs_ys += ys;

      if (rhs.high() == Yield::UP) {
        index.first = last_var->plus(lhs_ys.low());
      } else {
        // e.g. second maxsize filter in grammar/forloops5
        Expr::Cond *ce = new Expr::Cond(
          new Expr::Greater_Eq(right->minus(
            last_var->plus(lhs_ys.low())), rhs.high()),
          right->minus(rhs.high()), last_var->plus(lhs_ys.low()));
        index.first = ce;
      }

      index.second = right->minus(rhs.low());

      Expr::Base *cond = new Expr::Less_Eq(ivar, index.second);
      // e.g. first maxsize filter in grammar/forloops5
      if (lhs_ys.high() < Yield::UP) {
        cond = new Expr::And(
          cond, new Expr::Less_Eq (ivar, last_var->plus(lhs_ys.high())));
      }

      Statement::Var_Decl *loopvariable = new Statement::Var_Decl(
        new ::Type::Size(), ivar, index.first);
      // flag this variable as being an iterator e.g. in for-loops,
      // such that it won't have a trailing indent for code generation
      loopvariable->set_itr(true);
      Statement::For *f = new Statement::For (loopvariable, cond);
      //loops.push_back(f);
      return ivar;
    }
  } else {
    return next_var;
  }
}
