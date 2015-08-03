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


#ifndef STATEMENT_FWD_HH
#define STATEMENT_FWD_HH

namespace Statement {
  class Base;
  class Block;
  class Fn_Call;
  class For;
  class While;
  class Foreach;
  class Sorter;
  class If;
  class Switch;
  class Return;
  class Var_Assign;
  class Var_Decl;
  class Backtrace_Decl;
  class Backtrace_NT_Decl;
  class Iterator;
  class Hash_Decl;
  typedef Iterator iterator;

  class Break;
  class Continue;
  class Increase;
  class Decrease;

  class Marker_Decl;

  class Table_Decl;
}

#endif
