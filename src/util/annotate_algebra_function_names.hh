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

#ifndef SRC_UTIL_ANNOTATE_ALGEBRA_FUNCTION_NAMES_HH_
#define SRC_UTIL_ANNOTATE_ALGEBRA_FUNCTION_NAMES_HH_


#include "../cfg/cfg.hh"


#include "algebra_function_name_attribute.hh"


namespace Util {
// Forward declaration of the attribute, becuase we need it
// in the interface definition of the annotator class.
class AlgebraFunctionNameAttribute;


// Simply annotates each alternative of a production with a
// new algebra function name which is also unique.
// This algorithm relies on the fact, that each grammar which
// is given as input to 'annotateGrammar()' consists of
// grammar productions which are basically a list of alternative
// context free terms, where those terms do not contain alternatives
// itself, but only sequences or basic grammar parts (e.g.
// non-terminals, terminals or epsilons).
class AlgebraFunctionNameAnnotator {
 public:
    AlgebraFunctionNameAnnotator();
    ~AlgebraFunctionNameAnnotator();
    void annotateGrammar(CFG::CFG* grammar);

 private:
    void annotateProduction(CFG::Base* production);
    AlgebraFunctionNameAttribute* createNextAttribute();
};
}  // namespace Util


#endif  // SRC_UTIL_ANNOTATE_ALGEBRA_FUNCTION_NAMES_HH_
