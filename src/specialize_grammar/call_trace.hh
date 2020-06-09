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

#ifndef SRC_SPECIALIZE_GRAMMAR_CALL_TRACE_HH_
#define SRC_SPECIALIZE_GRAMMAR_CALL_TRACE_HH_


#include <map>
#include <vector>

#include "../util/annotate_cycles.hh"
#include "../util/naming_path.hh"
#include "set_of_cycle_sets.hh"


namespace Util {


  class CallTrace {

    private:

      typedef std::pair<std::string, Util::SetOfCycleSets*> PairedElementType;

      std::vector<PairedElementType> callTrace;
      std::map<std::string, PairedElementType> searchPool;


    public:

      CallTrace();
      CallTrace(CallTrace& t);
      ~CallTrace();

      // Pushes a non-terminal name onto the call-stack.
      void push(CFG::NonTerminal* nt);
      // Pushes a non-terminal name onto a call-stack with
      // the cycle set that is currently processed.
      void push(CFG::NonTerminal* nt, Util::SetOfCycleSets* cycleSets);

      // Returns TRUE if the call-stack is empty.
      bool isEmpty();
      // Returns TRUE if this call TRACE contains at any
      // position the given key.
      bool contains(CFG::NonTerminal* nt);

      // Returns the CycleSet which is associated with the
      // name '*nt'.
      std::pair<std::string, Util::SetOfCycleSets*>
        searchForCycleSetContainingNonTerminal(CFG::NonTerminal* nt);

      // Returns the top element of the call-stack.
      Util::SetOfCycleSets* pop();

      // Reads the top element from the stack and returns it.
      // The stack contents is not changed by this method.
      std::pair<std::string, Util::SetOfCycleSets*> peek();

      // Returns a string representation of this trace/
      std::string toString();

      // Returns the naming path which reflects the call stack
      // structure.
      NamingPath* getNamingPath(CFG::NonTerminal* nonTerminal);


  };


}  // namespace Util


#endif  // SRC_SPECIALIZE_GRAMMAR_CALL_TRACE_HH_
