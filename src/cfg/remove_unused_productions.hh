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

#ifndef SRC_CFG_REMOVE_UNUSED_PRODUCTIONS_HH_
#define SRC_CFG_REMOVE_UNUSED_PRODUCTIONS_HH_


#include <set>
#include <string>

#include "cfg.hh"


namespace CFG {


class UnusedProductionsRemover {
 private:
    // The source grammar
    CFG* oldGrammar;
    CFG* newGrammar;

 public:
    UnusedProductionsRemover();
    ~UnusedProductionsRemover();

    // Removes all unused productions from this grammar,
    // and returns a new grammar, which contains only
    // those grammar productions which are reachable from
    // the axiom.
    CFG* removeUnusedProductions(CFG* grammar);

 private:
    void removeFromProductions();
    void removeFromProduction(
      GrammarProduction* production,
      std::set<std::string>* visitedNonTerminals);
    void removeFromBase(Base* b, std::set<std::string>* visitedNonTerminals);
};


}  // namespace CFG


#endif  // SRC_CFG_REMOVE_UNUSED_PRODUCTIONS_HH_
