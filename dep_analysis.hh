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

#include "hashtable.hh"
#include "symbol_fwd.hh"

#include <list>
#include <vector>
#include <string>

class Dep_Analysis {
  private:
    const hashtable<std::string, Symbol::Base*> &symbols;

    std::list<Symbol::NT*> ordering;

    std::vector<Symbol::NT*> int_map;

    hashtable<Symbol::NT*, size_t> nt_map;

  public:
    Dep_Analysis(const hashtable<std::string, Symbol::Base*> &s);

    void sort();

    typedef std::list<Symbol::NT*>::iterator iterator;

    iterator begin() { return ordering.begin(); }
    iterator end() { return ordering.end(); }

    const std::list<Symbol::NT*> &result() const
    {
      assert(!ordering.empty());
      return ordering;
    }
};
