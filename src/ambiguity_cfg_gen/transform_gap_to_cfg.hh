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

#ifndef SRC_AMBIGUITY_CFG_GEN_TRANSFORM_GAP_TO_CFG_HH_
#define SRC_AMBIGUITY_CFG_GEN_TRANSFORM_GAP_TO_CFG_HH_


#include <list>
#include <stack>
#include <string>
#include "../hashtable.hh"

#include "../algebra.hh"
#include "../symbol.hh"
#include "../alt.hh"
#include "../fn_arg.hh"
#include "../fn_def.hh"

#include "../cfg/cfg.hh"


namespace AmbiguityCFG {


class GAPToCFGTransformer {
 private:
    // Holds a stack of non-terminals for which a grammar
    // rule needs to be produced
    std::stack<Symbol::NT*> nonTerminalsToGenerate;

    // Stores a pointer to the name of the currently processed
    // GAP grammar rule. This instance field is used to avoid
    // a great deal of parameter passing, since the name of the
    // rule is extracted at the beginning of a deeper traversal
    // of the grammar graph, where each method needed to pass
    // that value along.
    std::string* currentlyProcessedGrammarRuleName;

    // Stores the grammar that is produced by this generator
    CFG::CFG* grammar;

 public:
    GAPToCFGTransformer();
    ~GAPToCFGTransformer();

    // This is the main entry point for the algorithm, though
    // some other methods can be used as well, e.g. when a function
    // argument needs to be converted into a CFG graph structure.
    CFG::CFG* generateCFG(Symbol::NT *axiom);


  // private:

    void generateProductions();
    void generateRule(Symbol::NT *nonTerminal);

    CFG::Base* generateFragment(Symbol::Base *b);
    CFG::Base* generateFragment(Symbol::NT *nonTerminal);
    CFG::Base* generateFragment(Symbol::Terminal *terminal);

    CFG::Base* generateFragment(Alt::Base *alt);
    CFG::Base* generateFragment(Alt::Simple *alt);
    CFG::Base* generateFragment(Alt::Block *alt);
    CFG::Base* generateFragment(Alt::Link *alt);
    CFG::Base* generateFragment(Alt::Multi *alt);

    CFG::Base* generateFragment(Fn_Arg::Base* arg);
    CFG::Base* generateFragment(Fn_Arg::Alt* arg);
    CFG::Base* generateFragment(Fn_Arg::Const* arg);

 private:
    CFG::Base* generateFunctionDefFragment(
      std::string* functionName, std::list<Fn_Arg::Base*> &args);
};


}  // namespace AmbiguityCFG


#endif  // SRC_AMBIGUITY_CFG_GEN_TRANSFORM_GAP_TO_CFG_HH_
