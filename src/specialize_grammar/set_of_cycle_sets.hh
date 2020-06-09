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

#ifndef SRC_SPECIALIZE_GRAMMAR_SET_OF_CYCLE_SETS_HH_
#define SRC_SPECIALIZE_GRAMMAR_SET_OF_CYCLE_SETS_HH_


#include <set>

#include "../cfg/cfg.hh"
#include "../util/cycle_set.hh"


namespace Util {


  class SetOfCycleSets {

    private:

      // This is the set of sets.
      std::set<CycleSet*> sets;


    public:

      SetOfCycleSets();
      SetOfCycleSets (std::set<CycleSet*> sets);
      SetOfCycleSets (SetOfCycleSets& s);
      ~SetOfCycleSets();

      // Adds a cycle-set to this set-set.
      void addCycleSet (CycleSet* cycleSet);
      // Returns TRUE if the parameter is already stored in
      // this set-set.
      bool containsCycleSet (CycleSet* cycleSet);
      // Returns TRUE if the non-terminal is stored in any
      // of the cycle-sets held by this set-set.
      bool containsElement (CFG::NonTerminal* nonTerminal);
      // Returns TRUE if this set is empty;
      bool isEmpty();
      // Returns true, if the source non-terminal is dominated
      // by the destination non-terminal (the destination comes
      // before the source in the cycle-order).
      bool isBackReference (
        CFG::NonTerminal* source, CFG::NonTerminal* destination);

      // Returns a string representation of the set-set.
      std::string toString();


  };


}


#endif  // SRC_SPECIALIZE_GRAMMAR_SET_OF_CYCLE_SETS_HH_
