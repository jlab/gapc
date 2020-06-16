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

#include <string>
#include "add_special_axiom_to_cfg.hh"
#include "choice_function_application_attribute.hh"
#include "designated_axiom_attribute.hh"


SpecializeGrammar::AddSpecialAxiomToCFG::AddSpecialAxiomToCFG() {
}


SpecializeGrammar::AddSpecialAxiomToCFG::~AddSpecialAxiomToCFG() {
}


void SpecializeGrammar::AddSpecialAxiomToCFG::addSpecialAxiom(
  CFG::CFG* grammar) {
  // All we need are two more non-terminals, which do not
  // collide in their name-spaces with the existing rule names.
  // We do not make much of an effort here, just using some
  // pretty unusual names.
  CFG::NonTerminal* newAxiom = new CFG::NonTerminal(new std::string("rule00"));
  CFG::NonTerminal* oldAxiom = (CFG::NonTerminal*)grammar->getAxiom()->clone();
  oldAxiom->clearAttributes();
  CFG::NonTerminal* intermediateNonTerminal = new CFG::NonTerminal(
    new std::string("rule01"));

  // Create an intermediate grammar-rule, and add an attribute
  // to the rule hinting the gap-code generator of the specialize
  // grammar generator to create a choice function application
  // for this grammar-production.
  CFG::GrammarProduction* intermediateProduction = new CFG::GrammarProduction(
    intermediateNonTerminal);
  CFG::ProductionAlternative* newIntermediateRHS =
    new CFG::ProductionAlternative();
  newIntermediateRHS->addAlternative(oldAxiom);
  intermediateProduction->rhs = newIntermediateRHS;
  intermediateProduction->setAttribute(
    new ChoiceFunctionApplicationAttribute(new std::string("h")));
  grammar->addProduction(intermediateProduction);

  // Create a new axiom-rule, and annotate an attribute which
  // gives a hint that this is the designated axiom grammar-rule.
  CFG::GrammarProduction* newAxiomProduction = new CFG::GrammarProduction(
    newAxiom);
  CFG::ProductionAlternative* newAxiomRHS = new CFG::ProductionAlternative();
  CFG::NonTerminal* intermediateNonTerminalRHS =
    (CFG::NonTerminal*)intermediateNonTerminal->clone();
  intermediateNonTerminalRHS->setAttribute(
    new DesignatedAxiomAttribute(oldAxiom->getName()));
  newAxiomRHS->addAlternative(intermediateNonTerminalRHS);
  newAxiomProduction->rhs = newAxiomRHS;
  grammar->addProduction(newAxiomProduction);

  grammar->setAxiom(newAxiom);
}
