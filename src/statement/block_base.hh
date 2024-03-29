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

#ifndef SRC_STATEMENT_BLOCK_BASE_HH_
#define SRC_STATEMENT_BLOCK_BASE_HH_

#include <list>
#include <algorithm>
#include "base.hh"

namespace Statement {
class Block_Base : public Base {
 public:
    std::list<Base*> statements;
    explicit Block_Base(Type t) : Base(t) {}
    Block_Base(Type t, const Loc &l) : Base(t, l) {}

    std::list<Base*> *stmts();

    void push_back(Base *s) { statements.push_back(s); }

    void copy(Block_Base &o) const;
};

}  // namespace Statement


#endif  // SRC_STATEMENT_BLOCK_BASE_HH_
