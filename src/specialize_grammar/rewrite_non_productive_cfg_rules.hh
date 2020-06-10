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

#ifndef SRC_SPECIALIZE_GRAMMAR_REWRITE_NON_PRODUCTIVE_CFG_RULES_HH_
#define SRC_SPECIALIZE_GRAMMAR_REWRITE_NON_PRODUCTIVE_CFG_RULES_HH_


#include <list>

#include "../cfg/cfg.hh"
#include "../util/attribute.hh"


namespace SpecializeGrammar {
// Rewrites a given grammar into an equivalent grammar which
// consists only of rules with sequences, that are either
// productive (except the cycling non-terminals) or non-terminals
// that can only accept epsilon (except the cycling non-terminals).
//
// Please note that as a prerequisite the cycle-annotator and
// the FIRST-set-annotator must have been run on the CFG.
class RewriteNonProductiveCFGRules {
 private:
    CFG::CFG* oldGrammar;
    CFG::CFG* newGrammar;

 public:
    RewriteNonProductiveCFGRules();
    ~RewriteNonProductiveCFGRules();

    // Rewrites the grammar.
    CFG::CFG* rewriteGrammar(CFG::CFG* grammar);

 private:
    void rewriteProductions();
    void rewriteProduction(CFG::GrammarProduction* production);

    //
    CFG::GrammarProduction* rewriteProductionWithoutEpsilon(
      CFG::GrammarProduction* production);
    //
    CFG::Base* rewriteBaseWithoutEpsilon(CFG::Base* b);
    //
    CFG::GrammarProduction* rewriteProductionWithEpsilon(
      CFG::GrammarProduction* production);
    //
    CFG::Base* rewriteBaseWithEpsilon(CFG::Base* b);

    // Copies all attributes that are annotated to the
    // source instance to the destination instance.
    void copyAttributes(CFG::Base* source, CFG::Base* destination);
};


// This is simply a marker attribute which marks any
// element of the CFG graph. A node which is marked
// with this attribute can only derive epsilon, nothing
// else.
class EpsilonOnlyAttribute : public Util::Attribute {
 public:
    EpsilonOnlyAttribute();
    EpsilonOnlyAttribute(EpsilonOnlyAttribute& a);
    ~EpsilonOnlyAttribute();

    virtual Util::Attribute* clone();
};


}  // namespace SpecializeGrammar


#endif  // SRC_SPECIALIZE_GRAMMAR_REWRITE_NON_PRODUCTIVE_CFG_RULES_HH_
