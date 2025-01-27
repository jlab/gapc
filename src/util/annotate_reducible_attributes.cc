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

#include <list>
#include <set>

#include "annotate_reducible_attributes.hh"

#include "../log.hh"
#include "cycle_mark_attribute.hh"


Util::ReducibleAttributeAnnotator::ReducibleAttributeAnnotator() {
}


Util::ReducibleAttributeAnnotator::~ReducibleAttributeAnnotator() {
}


void Util::ReducibleAttributeAnnotator::annotateGrammar(CFG::CFG* grammar) {
  // Store the current grammar first, we need this pointer
  // at a deeper level, and dont want to pass it as a
  // parameter through all method calls.
  this->grammar = grammar;

  // For each grammar production we start our annotation process.
  std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
  for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin();
       i != productions.end(); i++) {
    annotateProduction(*i);
  }
}


void Util::ReducibleAttributeAnnotator::annotateProduction(
  CFG::GrammarProduction* production) {
  annotateElement(production->rhs);
}


bool Util::ReducibleAttributeAnnotator::annotateElement(CFG::Base* b) {
  switch (b->getType()) {
    case CFG::BASE_WRAPPER: {
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);

      bool result = annotateElement(wrapper->getWrappedBase());
      if (result) {
        wrapper->setAttribute (new ReducibleElementAttribute());
      }

      return result;
    }
    case CFG::EPSILON:
    case CFG::REGULAR_EXPRESSION:
    case CFG::TERMINAL: {
      // Do nothing, unless we want also the opposite being annotated:
      // which means we put NotReducibleAttribute to each CFG node.
      break;
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (b);

      // Find out if there are any cycle annotations for this non-terminal.
      std::set<Util::CycleSet*> cycleSets = getCycleMarkSets(nonTerminal);

      // If there is at least one cycle which contains this non-terminal,
      // this element might be dropped due to cycle-breaks.
      if (cycleSets.size() > 0) {
        nonTerminal->setAttribute (new ReducibleElementAttribute());
        // TRUE: this element is reducible.
        return true;
      }

      break;
    }
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* seq = dynamic_cast<CFG::ProductionSequence*> (b);
      // Devide all elements into two groups:
      //  a) all non-terminals which are nullable
      //  b) all non-terminals which FIRST set does
      //     not contain epsilon.
      // It is important, that we only check non-terminals,
      // no other kind of elements. This algorithm relies
      // on the fact, that each sequence of elements may
      // not contain an alternative at any deeper nested
      // level of the grammar. This assumption leads to the
      // simplification that there are only three different
      // types of elements which may occur in the sequence
      // (epsilon, terminal and non-terminal).
      bool sequenceContainsInvalidElements = false;
      // just needed for the check, whether epsilon is in the set FIRST
      CFG::Epsilon epsilon;
      std::list<CFG::Base*> nullableElements;
      std::list<CFG::Base*> notNullableElements;
      for (int i = 0; i < seq->getSize(); i++) {
        CFG::Base* elem = seq->elementAt(i);

        if (!elem->is(CFG::NONTERMINAL)) {
          sequenceContainsInvalidElements = true;
          break;
        }

        // The element as a non-terminal, at this position
        // we can be sure about it.
        CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (elem);

        Util::FirstSet firstSet = getFirstSet(nonTerminal);
        if (firstSet.containsElement(&epsilon)) {
          nullableElements.push_back(elem);
        } else {
          notNullableElements.push_back(elem);
        }
      }

      // Do not process any further if the sequence contained
      // elements other than non-terminals.
      if (sequenceContainsInvalidElements) {
        // std::cout << "Sequence contains invalid elements." << std::endl;
        break;  // exit from the switch-statement
      }

      // After dividing all elements of the sequence into
      // those who may produce epsilon, and those who will
      // never produce epsilon, we distinguish two main
      // cases:
      //  1) exactly one element of the sequence is not nullable
      //  2) all elements of the sequence are nullable, thus no
      //     elemet is not nullable.
      if (notNullableElements.size() == 1) {
        // std::cout << "not nullable elements size is 1" << std::endl;
        bool elementIsReducible = annotateElement(notNullableElements.front());
        if (elementIsReducible) {
          seq->setAttribute (new ReducibleElementAttribute());
        }
        return elementIsReducible;
      } else if (notNullableElements.size() == 0) {
        // std::cout << "not nullable elements size is 0" << std::endl;
        // usually TRUE, this prevents that an empty list is also marked as
        // reducible.
        bool someElementIsReducible = nullableElements.size() > 0;
        for (std::list<CFG::Base*>::iterator i = nullableElements.begin();
             i != nullableElements.end(); i++) {
          someElementIsReducible &= annotateElement(*i);
        }
        // If at least one element was reducible, the whole sequence is
        // also reducible.
        if (someElementIsReducible) {
          seq->setAttribute (new ReducibleElementAttribute());
        }
        return someElementIsReducible;
      }
      break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      CFG::ProductionAlternative* productionAlternative =
        dynamic_cast<CFG::ProductionAlternative*> (b);
      bool allElementsAreReducible = true;
      for (CFG::ProductionAlternative::iterator i =
           productionAlternative->begin();
           i != productionAlternative->end(); i++) {
        allElementsAreReducible &= annotateElement(*i);
      }
      // If all elements were reducible, this alternative is also reducible.
      if (allElementsAreReducible) {
        productionAlternative->setAttribute (new ReducibleElementAttribute());
      }
      return allElementsAreReducible;
    }
    default: {
      throw LogError(
        "gap-00611: Unhandled CFG node type when annotating reducible "
        "attribute to grammar elements.");
    }
  }

  // The default: the element is NOT reducible:
  return false;
}


std::set<Util::CycleSet*> Util::ReducibleAttributeAnnotator::getCycleMarkSets(
  CFG::Base* b) {
  Util::Attribute* attribute = b->getAttribute("Util::CycleMarkAttribute");
  Util::CycleMarkAttribute* cycleMarkAttribute =
    (Util::CycleMarkAttribute*)attribute;

  if (cycleMarkAttribute != NULL) {
    return cycleMarkAttribute->getCycleSets();
  } else {
    return std::set<Util::CycleSet*>();
  }
}


Util::FirstSet Util::ReducibleAttributeAnnotator::getFirstSet(
    CFG::NonTerminal* nt) {
  CFG::GrammarProduction* production = this->grammar->getProduction(nt);
  Util::FirstSetAttribute* attr =
    (Util::FirstSetAttribute*)production->lhs->getAttribute(
      "Util::FirstSetAttribute");
  if (attr == NULL) {
    return FirstSet();
  }
  return attr->getFirstSet();
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


Util::ReducibleElementAttribute::ReducibleElementAttribute()
  : Attribute("Util::ReducibleElementAttribute") {
}


Util::ReducibleElementAttribute::ReducibleElementAttribute(
  ReducibleElementAttribute& a)
  : Attribute(a) {
}


Util::ReducibleElementAttribute::~ReducibleElementAttribute() {
}


Util::Attribute* Util::ReducibleElementAttribute::clone() {
  return new ReducibleElementAttribute (*this);
}
