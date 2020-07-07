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

#ifndef SRC_UTIL_CYCLE_ATTRIBUTE_HH_
#define SRC_UTIL_CYCLE_ATTRIBUTE_HH_


#include <set>

#include "attribute.hh"
#include "cycle_set.hh"


namespace Util {
// Used to annotate a non-terminal in a grammar with cycle-information.
class CycleAttribute : public Attribute {
 private:
    // Stores the set of non-terminals that belong to
    // the cycle.
    std::set<CycleSet*> cycleSets;

 public:
    CycleAttribute(std::set<CycleSet*> cycleSet);
    CycleAttribute(CycleAttribute& a);
    ~CycleAttribute();

    std::set<CycleSet*> getCycleSets();

    virtual Attribute* clone();

    // Returns true, if the current cycle-mark-attribute
    // already contains the given set.
    bool containsCycleSet(CycleSet* set);

 private:
    // Adds all elements of the std::set to this attribute,
    // provided they are not already inserted. Dumplicate
    // sets will be removed by this provedure.
    void addCycleSets(std::set<CycleSet*> sets);
};
}  // namespace Util


#endif  // SRC_UTIL_CYCLE_ATTRIBUTE_HH_
