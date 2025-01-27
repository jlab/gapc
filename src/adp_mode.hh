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

#ifndef SRC_ADP_MODE_HH_
#define  SRC_ADP_MODE_HH_

namespace ADP_Mode {
enum Adp_Specialization { STANDARD, SORTED_STEP, SORTED_BLOCK,
PARETO_EAGER_STEP, PARETO_EAGER_BLOCK };

enum Adp_Join { EMPTY, SORTER, COMPERATOR, SORTER_COMPERATOR};

extern bool is_step(Adp_Specialization s);

extern bool is_coopt_param(Adp_Specialization s);

enum Rtlib_Header { NONE, PARETO_NOSORT_STEP, PARETO_NOSORT_BLOCK,
PARETO_SORT_STEP, PARETO_SORT_BLOCK, PARETO_YUK_STEP, PARETO_YUK_BLOCK,
SORT_BLOCK, SORT_STEP};
}  // namespace ADP_Mode

#endif  // SRC_ADP_MODE_HH_
