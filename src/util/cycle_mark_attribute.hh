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

#ifndef SRC_UTIL_CYCLE_MARK_ATTRIBUTE_HH_
#define SRC_UTIL_CYCLE_MARK_ATTRIBUTE_HH_


#include <set>
#include "attribute.hh"
#include "cycle_set.hh"


namespace Util {


// A marker attribute which is used to mark a non-terminal
// as being part of a cycle. It contains information about
// the cycle it is part of as a pointer to a CycleSet.
class CycleMarkAttribute : public Attribute {
 private:
    // The cycle-set the marked non-terminal is part of.
    std::set<CycleSet*> cycleSets;

 public:
    CycleMarkAttribute();
    CycleMarkAttribute(CycleMarkAttribute& a);
    ~CycleMarkAttribute();

    // Adds the given set to the set of cycle sets.
    void addCycleSet(CycleSet* set);
    // Returns true, if the current cycle-mark-attribute
    // already contains the given set.
    bool containsCycleSet(CycleSet* set);

    // Returns the set of cycle sets this attribute holds.
    std::set<CycleSet*> getCycleSets();

    virtual Attribute* clone();
};
}  // namespace Util


#endif  // SRC_UTIL_CYCLE_MARK_ATTRIBUTE_HH_
