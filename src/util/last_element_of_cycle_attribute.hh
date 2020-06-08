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

#ifndef __LAST_ELEMENT_OF_CYCLE_ATTRIBUTE_HH__
#define __LAST_ELEMENT_OF_CYCLE_ATTRIBUTE_HH__


#include <set>

#include "attribute.hh"
#include "cycle_set.hh"


namespace Util {


  // This attribute is used to mark GrammarProductions with
  // those cycles-sets which end in this production.
  class LastCycleElementAttribute : public Attribute {

    private:

      // Stores all CycleSets which have their last element
      // in cycle order in that GrammarProduction which will
      // be attributed with this LastCycleElement instance.
      std::set<CycleSet*> cycleSets;


    public:

      LastCycleElementAttribute();
      LastCycleElementAttribute (LastCycleElementAttribute& a);
      ~LastCycleElementAttribute();

      // Adds a CycleSet to this attribute.
      void addCycleSet (CycleSet* cycleSet);
      // Returns the set of CycleSets held by this attribute.
      std::set<CycleSet*> getCycleSets();

      typedef std::set<CycleSet*>::iterator iterator;
      iterator begin();
      iterator end();

      virtual Util::Attribute* clone();

  };


}


#endif  // ifndef __LAST_ELEMENT_OF_CYCLE_ATTRIBUTE_HH__
