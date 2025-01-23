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


#ifndef SRC_EXPR_FWD_HH_
#define SRC_EXPR_FWD_HH_

namespace Expr {
enum Type {PLUS, MINUS, TIMES, DIV, COMP, FN_CALL, VACC, CONST,
LESS_EQ, LESS, GREATER, GREATER_EQ, EQ, NOT_EQ,
AND, OR, NOT,
MAX, COND,
NEW, THIS, MOD };

class Base;
class Vacc;
class Fn_Call;

class New;

}  // namespace Expr

#endif  // SRC_EXPR_FWD_HH_
