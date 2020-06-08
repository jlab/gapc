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

#ifndef SRC_UTIL_CYCLE_SET_UTILS_HH_
#define SRC_UTIL_CYCLE_SET_UTILS_HH_


#include "attribute.hh"
#include "annotate_the_set_first.hh"
#include "cycle_mark_attribute.hh"
#include "../cfg/cfg.hh"


namespace Util {


  // This class provides utility mehtods which can be used anywhere in
  // this project. All methods are static. Preconditions of each method
  // are stated in the method-comment, please see below for more details.
  class CycleSetUtils {

    public:

      // Returns TRUE if the element if annotated with a Util::FirstSetAttribute
      // which contains epsilon as an element.
      // This method returns FALSE, if the given node is not annotated with
      // such an attribute!
      static bool elementIsNullable (CFG::Base* b) {
        Attribute* attribute = b->getAttribute ("Util::FirstSetAttribute");
        FirstSetAttribute* firstSetAttribute = (FirstSetAttribute*)attribute;

        if (firstSetAttribute == NULL) {
          return false;
        }
        else {
          CFG::Epsilon* epsilon = new CFG::Epsilon();
          bool result = firstSetAttribute->getFirstSet().containsElement (epsilon);
          delete (epsilon);
          return result;
        }
      }


      // Returns TRUE if the element if annotated with a Util::FirstSetAttribute
      // which contains nothing but epsilon as an element.
      // This method returns FALSE, if the given node is not annotated with
      // such an attribute!
      static bool elementIsNullableOnly (CFG::Base* b) {
        Attribute* attribute = b->getAttribute ("Util::FirstSetAttribute");
        FirstSetAttribute* firstSetAttribute = (FirstSetAttribute*)attribute;

        if (firstSetAttribute == NULL) {
          return false;
        }
        else {
          CFG::Epsilon* epsilon = new CFG::Epsilon();
          FirstSet firstSet = firstSetAttribute->getFirstSet();
          bool result = firstSet.containsElement (epsilon);
          delete (epsilon);
          return result && firstSet.size() == 1;
        }
      }


      // Returns TRUE if the element if annotated with a Util::FirstSetAttribute
      // which contains more then one element, excluding epsilon.
      // This method returns FALSE, if the given node is not annotated with
      // such an attribute!
      static bool elementIsProductive (CFG::Base* b) {
        Attribute* attribute = b->getAttribute ("Util::FirstSetAttribute");
        FirstSetAttribute* firstSetAttribute = (FirstSetAttribute*)attribute;

        if (firstSetAttribute == NULL) {
          return false;
        }
        else {
          CFG::Epsilon* epsilon = new CFG::Epsilon();
          FirstSet firstSet = firstSetAttribute->getFirstSet();
          delete (epsilon);
          int numberOfElementsWithoutEpsilon = firstSet.size() - (firstSet.containsElement (epsilon) ? 1 : 0);
          return numberOfElementsWithoutEpsilon > 0;
        }
      }


      // This method returns TRUE if the given non-terminal is
      // part of a cycle. If this is not a non-terminal instance
      // or not part of a cycle, FALSE is returned.
      static bool elementIsPartOfCycle (CFG::Base* b) {
        if (b->getType() != CFG::NONTERMINAL) {
          return false;
        }

        Attribute* attribute = b->getAttribute ("Util::CycleMarkAttribute");
        CycleMarkAttribute* cycleMarkAttribute = (CycleMarkAttribute*)attribute;

        if (cycleMarkAttribute == NULL) {
          return false;
        }
        else {
          return true;
        }
      }


  };


}


#endif  // ifndef SRC_UTIL_CYCLE_SET_UTILS_HH_
