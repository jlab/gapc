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


#ifndef ADP_HH
#define ADP_HH

#include "empty.hh" 
#include "algebra.hh" 
#include "erase.hh" 
#include "list.hh" 
#include "sequence.hh"
#include "string.hh"
#include "table.hh" 
#include "terminal.hh"

#include "filter.hh"

#include "range.hh"

#include "output.hh"

#include "push_back.hh"

#include "shape.hh"

#include "bigint.hh"

#include "backtrack.hh"

#include "bench.hh"

#include "rope.hh"

// needed for uint64_t (Integer ...)
#include <boost/cstdint.hpp>

#include <algorithm>

using std::max;
using std::min;

typedef int nosuchtype;

#endif
