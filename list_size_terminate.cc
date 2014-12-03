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


#include "list_size_terminate.hh"

#include "yieldsize.hh"
#include "symbol.hh"
#include "alt.hh"

// example, where this is needed: affinelocsim.gap

void List_Size_Terminate::visit(Symbol::NT &n)
{
  if (n.list_size() == 0) {
    Yield::Poly p(Yield::UP);
    n.set_list_size(p);
  }
}


void List_Size_Terminate::visit(Alt::Base &a)
{
  if (a.list_size() == 0) {
    Yield::Poly p(Yield::UP);
    a.set_list_size(p);
  }
}


