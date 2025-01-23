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

#include "remove_unused_productions.hh"

#include <set>
#include <string>
#include <iostream>


CFG::UnusedProductionsRemover::UnusedProductionsRemover() {
}


CFG::UnusedProductionsRemover::~UnusedProductionsRemover() {
}


CFG::CFG* CFG::UnusedProductionsRemover::removeUnusedProductions(
  CFG* grammar) {
  this->oldGrammar = grammar;
  this->newGrammar = new CFG();

  // Copy all reachable non-terminals into the new grammar.
  removeFromProductions();

  // Set the axiom of the new grammar.
  this->newGrammar->setAxiom(this->oldGrammar->getAxiom());

  return this->newGrammar;
}


void CFG::UnusedProductionsRemover::removeFromProductions() {
  GrammarProduction* production = this->oldGrammar->getProduction(
    this->oldGrammar->getAxiom());
  std::set<std::string>* visitedNonTerminals = new std::set<std::string>();
  removeFromProduction(production, visitedNonTerminals);
  delete(visitedNonTerminals);
}


void CFG::UnusedProductionsRemover::removeFromProduction(
  GrammarProduction* production, std::set<std::string>* visitedNonTerminals) {
  visitedNonTerminals->insert(*production->lhs->getName());
  this->newGrammar->addProduction(production);
  removeFromBase(production->rhs, visitedNonTerminals);
}


void CFG::UnusedProductionsRemover::removeFromBase(
  Base* b, std::set<std::string>* visitedNonTerminals) {
  switch (b->getType()) {
    case BASE_WRAPPER: {
      BaseWrapper* wrapper = dynamic_cast<BaseWrapper*> (b);

      removeFromBase(wrapper->getWrappedBase(), visitedNonTerminals);

      break;
    }
    case NONTERMINAL: {
      NonTerminal* nonTerminal = dynamic_cast<NonTerminal*> (b);

      if (visitedNonTerminals->find(*nonTerminal->getName()) ==
          visitedNonTerminals->end()) {
        GrammarProduction* production = this->oldGrammar->getProduction(
          nonTerminal);
        removeFromProduction(production, visitedNonTerminals);
      }

      break;
    }
    case PRODUCTION_SEQUENCE: {
      ProductionSequence* sequence = dynamic_cast<ProductionSequence*> (b);

      for (ProductionSequence::iterator i = sequence->begin();
           i != sequence->end(); i++) {
        removeFromBase(*i, visitedNonTerminals);
      }

      break;
    }
    case PRODUCTION_ALTERNATIVE: {
      ProductionAlternative* alternative =
        dynamic_cast<ProductionAlternative*> (b);

      for (ProductionAlternative::iterator i = alternative->begin();
           i != alternative->end(); i++) {
        removeFromBase(*i, visitedNonTerminals);
      }

      break;
    }
    default: {
    }
  }
}
