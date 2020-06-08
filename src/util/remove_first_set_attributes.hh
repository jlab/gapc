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


#ifndef SRC_UTIL_REMOVE_FIRST_SET_ATTRIBUTES_HH_
#define SRC_UTIL_REMOVE_FIRST_SET_ATTRIBUTES_HH_


#include "../cfg/cfg.hh"


namespace Util {


  class RemoveFirstSetAttributes {

    public:

      RemoveFirstSetAttributes();
      ~RemoveFirstSetAttributes();

      // Removes all attributes from the given grammar.
      void removeFromGrammar (CFG::CFG* grammar);

    private:

      void removeFromProduction (CFG::GrammarProduction* production);
      void removeFromBase (CFG::Base* b);


  };


}


#endif  // ifndef SRC_UTIL_REMOVE_FIRST_SET_ATTRIBUTES_HH_
