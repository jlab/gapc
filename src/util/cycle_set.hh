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

#ifndef SRC_UTIL_CYCLE_SET_HH_
#define SRC_UTIL_CYCLE_SET_HH_


#include <list>
#include <set>

#include "../cfg/cfg.hh"


namespace Util {


  class CycleSet {

    private:

      // Stores all non-terminal names that belong
      // to a cycle.
      std::set<std::string> set;

      // Stores all elements that are in the set of elements
      // in a list, which constitutes an order. The order is
      // establiched first, when the main entry point is set,
      // otherwise the order is unspecified.
      std::list<std::string> orderedElements;

      // The main entry point onto the cycle.
      CFG::NonTerminal* mainEntryPoint;


    public:

      CycleSet();
      ~CycleSet();

      // Sets the main entry point for this cycle.
      void setMainEntryPoint (CFG::NonTerminal* mainEntryPoint);
      // Returns TRUE, if the given non-terminal is the last
      // element of the cycle.
      bool isLastElementInCycle (CFG::NonTerminal* nonTerminal);
      // Returns true, if the source non-terminal is dominated
      // by the destination non-terminal (the destination comes
      // before the source in the cycle-order).
      bool isBackReference (CFG::NonTerminal* source, CFG::NonTerminal* destination);

      // Adds a non-terminal to the set.
      void addElement (CFG::NonTerminal* nonTerminal);
      // Addls all elements of the list into the set in that
      // order in which they appear in the list.
      void addElements (std::list<CFG::NonTerminal*> elements);
      // Checks if the non-terminal is already present in
      // this set.
      bool containsElement (CFG::NonTerminal* nonTerminal);
      // Returns TRUE if this instance does not contain any
      // elements.
      bool isEmpty();

      // Calculates the intersection of the current instance
      // and the set which is passed as parameter. The new set
      // which is returned contains only those elements which
      // are in this instance as well as in the set given as
      // parameter.
      CycleSet intersect (CycleSet cycleSet);
      // Calculates the difference between the current
      // instance and the set given as parameter and
      // returns a new set which contains those elements
      // which were in this instance, but not in the
      // set which was passed as parameter.
      CycleSet difference (CycleSet cycleSet);

      // Determines whether two cycle-sets are equal.
      bool operator== (CycleSet& set);

      // Returns a string representation of the set.
      std::string toString();

      // The iterator which can be used to walk through all
      // elements in an ordered fashion.
      typedef std::list<std::string>::iterator iterator;

      iterator begin();
      iterator end();


  };


}


#endif  // ifndef SRC_UTIL_CYCLE_SET_HH_
