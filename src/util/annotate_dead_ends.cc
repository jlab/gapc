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
#include <set>
#include "annotate_dead_ends.hh"


#include "../log.hh"
#include "annotate_the_set_first.hh"


Util::AnnotateDeadEnds::AnnotateDeadEnds() {
}


Util::AnnotateDeadEnds::~AnnotateDeadEnds() {
}


void Util::AnnotateDeadEnds::annotateGrammar(CFG::CFG* grammar) {
  this->grammar = grammar;
  std::set<std::string>* visitedNonTerminals = new std::set<std::string>();

  // Start with the production of the axiom, and traverse the
  // whole graph recursively.
  annotateProduction(this->grammar->getAxiom(), visitedNonTerminals);
}


bool Util::AnnotateDeadEnds::annotateProduction(
  CFG::NonTerminal* nonTerminal, std::set<std::string>* visitedNonTerminals) {
  CFG::GrammarProduction* production = this->grammar->getProduction(
    nonTerminal);
  // Before we start, we need to mark this non-terminal as processed,
  // otherwise we will have an infinite loop here.
  visitedNonTerminals->insert(*production->lhs->getName());
  // Now just process the right-hand-side.
  bool result = annotateBase(production->rhs, visitedNonTerminals);
  // If the result is TRUE, this means that the right-hand-side
  // of this production is completely a dead end, and thus we
  // annotate this production with the same attribute.
  if (result) {
    production->setAttribute(new DeadEndAttribute());
  }

  return result;
}


bool Util::AnnotateDeadEnds::annotateBase(
  CFG::Base* b, std::set<std::string>* visitedNonTerminals) {
  switch (b->getType()) {
    case CFG::EPSILON: {
      b->setAttribute(new DeadEndAttribute());
      return true;
    }
    case CFG::TERMINAL: {
      b->setAttribute(new DeadEndAttribute());
      return true;
    }
    case CFG::BASE_WRAPPER: {
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);

      bool result = annotateBase(
        wrapper->getWrappedBase(), visitedNonTerminals);
      if (result) {
        wrapper->setAttribute(new DeadEndAttribute());
      }

      return result;
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (b);

      // A non-terminal is a dead-end only if the production it
      // references also is a dead end. If this production
      if (visitedNonTerminals->find(*nonTerminal->getName())
          == visitedNonTerminals->end()) {
        if (annotateProduction(nonTerminal, visitedNonTerminals)) {
          b->setAttribute(new DeadEndAttribute());
          return true;
        }
      }

      return false;
    }
    case CFG::REGULAR_EXPRESSION: {
      b->setAttribute (new DeadEndAttribute());
      return true;
    }
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* sequence =
        dynamic_cast<CFG::ProductionSequence*> (b);

      bool overallResult = true;
      for (CFG::ProductionSequence::iterator i = sequence->begin();
           i != sequence->end(); i++) {
        overallResult &= annotateBase(*i, visitedNonTerminals);
      }

      if (overallResult) {
        sequence->setAttribute(new DeadEndAttribute());
      }

      return overallResult;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      CFG::ProductionAlternative* alternative =
        dynamic_cast<CFG::ProductionAlternative*> (b);

      bool overallResult = true;
      for (CFG::ProductionAlternative::iterator i = alternative->begin();
           i != alternative->end(); i++) {
        overallResult &= annotateBase(*i, visitedNonTerminals);
      }

      if (overallResult) {
        alternative->setAttribute(new DeadEndAttribute());
      }

      return overallResult;
    }
    default: {
      throw LogError(
        "gap-00701: Unhandled CFG node type in annotation method.");
    }
  }
  return false;
}


bool Util::AnnotateDeadEnds::elementIsNullable(CFG::Base* b) {
  Attribute* attribute = b->getAttribute("Util::FirstSetAttribute");
  FirstSetAttribute* firstSetAttribute =
    reinterpret_cast<FirstSetAttribute*>(attribute);

  if (firstSetAttribute != NULL) {
    FirstSet firstSet = firstSetAttribute->getFirstSet();
    CFG::Epsilon* epsilon = new CFG::Epsilon();
    bool result = firstSet.containsElement(epsilon);
    delete (epsilon);
    return result;
  }
  return false;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


Util::DeadEndAttribute::DeadEndAttribute()
  : Attribute("Util::DeadEndAttribute") {
}


Util::DeadEndAttribute::DeadEndAttribute(DeadEndAttribute& a)
  : Attribute("Util::DeadEndAttribute") {
  // Nothing to initialize here!
}


Util::DeadEndAttribute::~DeadEndAttribute() {
}


Util::Attribute* Util::DeadEndAttribute::clone() {
  return new DeadEndAttribute (*this);
}
