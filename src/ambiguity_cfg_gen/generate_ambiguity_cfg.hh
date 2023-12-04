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

#ifndef SRC_AMBIGUITY_CFG_GEN_GENERATE_AMBIGUITY_CFG_HH_
#define SRC_AMBIGUITY_CFG_GEN_GENERATE_AMBIGUITY_CFG_HH_

#include <string>
#include <list>
#include <stack>
#include "../hashtable.hh"

#include "../algebra.hh"
#include "../symbol.hh"
#include "../alt.hh"
#include "../fn_arg.hh"
#include "../fn_def.hh"

#include "../cfg/cfg.hh"
#include "grammar_vm.hh"
#include "grammar_vm_function_compiler.hh"


namespace AmbiguityCFG {


class GenerateAmbiguityCFG {
 private:
    // stores a pointer to the canonical string algebra
    // that produces the candidates our CFG will produce.
    Algebra *algebra;

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

    // The grammarVM that computes CFG algebra outcomes.
    GrammarVM grammarVM;

    // The compiler which transforms algebra function definitions
    // into grammar VM code.
    // GrammarVMFunctionCompiler* grammarVMCompiler;

 public:
    GenerateAmbiguityCFG();
    ~GenerateAmbiguityCFG();

    // This is the starting point of the algorithm. It is provided
    // a canonical string algebra and an axiom.
    // Please note that the grammar is not explicitely given, since
    // the axiom provides access to all other gapc grammar rules
    // because a gapc grammar is stored as a graph, with links connecting
    // all non-terminals with its gapc grammar rule definition.
    CFG::CFG* generateCFG(Algebra *canonical_algebra, Symbol::NT *axiom);

 private:
    // Checks the whole algebra if it meats the requirements of
    // a pretty print algebra suitable for ambiguity CFG generation.
    void checkAlgebra(Algebra* alg);
    // Checks the choice function if it is of type PRETTY and issues
    // a warning if not.
    void checkChoiceFuntion(Algebra* alg);
    // Checks if the return type of the algebra is String.
    void checkReturnType(Algebra* alg);

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

    CFG::Base* generateFunctionDefFragment(
      std::string* functionName, Fn_Def *algebra_function,
      std::list<Fn_Arg::Base*> &args);
};

}  // namespace AmbiguityCFG


#endif  // SRC_AMBIGUITY_CFG_GEN_GENERATE_AMBIGUITY_CFG_HH_
