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

#ifndef SRC_SPECIALIZE_GRAMMAR_ADD_SPECIAL_AXIOM_TO_CFG_HH_
#define SRC_SPECIALIZE_GRAMMAR_ADD_SPECIAL_AXIOM_TO_CFG_HH_


#include "../cfg/cfg.hh"


namespace SpecializeGrammar {


  class AddSpecialAxiomToCFG {


    public:

      AddSpecialAxiomToCFG();
      ~AddSpecialAxiomToCFG();

      // Adds two new rules to the grammar and annotated both
      // productions with attributes. the first production
      // defining the axiom, will be annotated as designated
      // axiom-production. The second production will be
      // annotated with a choice-function-attribute which
      // hints the gap-code generator which choice function
      // to use for the production.
      void addSpecialAxiom(CFG::CFG* grammar);


  };


}


#endif  // ifndef SRC_SPECIALIZE_GRAMMAR_ADD_SPECIAL_AXIOM_TO_CFG_HH_
