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

#include <cassert>
#include <list>
#include <iostream>
#include <utility>
#include <set>

#include "annotate_cycles.hh"
#include "annotate_the_set_first.hh"
#include "cycle_attribute.hh"
#include "cycle_mark_attribute.hh"
#include "cycle_set.hh"
#include "last_element_of_cycle_attribute.hh"


Util::AnnotateCycles::AnnotateCycles() {
}


Util::AnnotateCycles::~AnnotateCycles() {
}


void Util::AnnotateCycles::annotateGrammar(CFG::CFG* grammar) {
  this->grammar = grammar;
  std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
  for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin();
       i != productions.end(); ++i) {
    CycleSet* visitedNonTerminals = new CycleSet();
    std::list<CFG::NonTerminal*> callStack;
    std::set<std::pair<CycleSet*, std::list<CFG::NonTerminal*>* > > cycles =
      searchForCycles((*i)->lhs, *i, visitedNonTerminals, callStack);

    std::set<CycleSet*> strippedSetSet;
    // The result is a set of pairs, where its first component
    // is the cycle-set we are interested in, and the second
    // component is a list of non-terminals which need to be
    // attributed.
    for (std::set<std::pair<CycleSet*, std::list<CFG::NonTerminal*>* > >
         ::iterator j = cycles.begin(); j != cycles.end(); ++j) {
      CycleSet* cycleSet = (*j).first;
      std::list<CFG::NonTerminal*>* affectedNonTerminals = (*j).second;
      // For each affected non-terminal add an attribute to the
      // non-terminal node itself.
      for (std::list<CFG::NonTerminal*>::iterator k =
           affectedNonTerminals->begin();
           k != affectedNonTerminals->end(); k++) {
        addCycleMarkAttribute(*k, cycleSet);
        // Check if the current affected non-terminal is
        // the last terminal in the cycle. If that is the
        // case, annotate the grammar-production with an
        // attribute.
        if (cycleSet->isLastElementInCycle(*k)) {
          addLastCycleElementAttribute(*i, cycleSet);
        }
      }
      strippedSetSet.insert(cycleSet);
    }

    // Annotate the production and its left-hand-side
    // with a cycle-attribute.
    (*i)->setAttribute(new CycleAttribute (strippedSetSet));
    (*i)->rhs->setAttribute(new CycleAttribute (strippedSetSet));

    delete visitedNonTerminals;
  }
}


std::set<std::pair<Util::CycleSet*, std::list<CFG::NonTerminal*>* > >
  Util::AnnotateCycles::searchForCycles(
    CFG::NonTerminal* nt, CFG::GrammarProduction* production,
    Util::CycleSet* visitedNonTerminals,
    std::list<CFG::NonTerminal*> callStack) {
  // This method is a proxy for each non-terminal application.
  // It is by this the best point for injecting symbols into
  // any kind of sets: First of all, the set of visited
  // non-terminals is extended by the lhs symbol of the grammar
  // production. Second, each result cycle-set must also
  // contain this lhs, because this is a point in code it
  // must have passed.
  CFG::NonTerminal* nonTerminal = production->lhs;
  visitedNonTerminals->addElement(nonTerminal);
  std::set<std::pair<CycleSet*, std::list<CFG::NonTerminal*>* > > results =
    searchForCycles(nt, visitedNonTerminals, production->rhs, callStack);
  return results;
}


std::set<std::pair<Util::CycleSet*, std::list<CFG::NonTerminal*>* > >
  Util::AnnotateCycles::searchForCycles(
    CFG::NonTerminal* nt, Util::CycleSet* visitedNonTerminals,
    CFG::Base* fragment, std::list<CFG::NonTerminal*> callStack) {
  std::set<std::pair<Util::CycleSet*, std::list<CFG::NonTerminal*>* > > result;
  switch (fragment->getType()) {
    case CFG::EPSILON:
    case CFG::TERMINAL:
    case CFG::REGULAR_EXPRESSION: {
      return result;
    }
    case CFG::BASE_WRAPPER: {
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (fragment);
      return searchForCycles(
        nt, visitedNonTerminals, wrapper->getWrappedBase(), callStack);
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*>(fragment);
      if (visitedNonTerminals->containsElement(nonTerminal)) {
        // ...create a new cycle-set, but only if this is
        // a cycle that connects the start non-terminal with
        // itself. While we search for such a cycle for a
        // certain non-terminal, we may encounter many other
        // cycles which point back to any non-terminal we
        // already processed in our search, but not exactly
        // the non-terminal we started with in the first place.
        // This is true for any non-terminals which use non-terminals
        // that are itself cyclic, but whose cycle does not
        // include the non-terminal we started with.
        if (*nt->getName() == *nonTerminal->getName()) {
          callStack.push_back(nonTerminal);
          CycleSet* newCycleSet = new CycleSet();
          newCycleSet->addElements(callStack);
          newCycleSet->setMainEntryPoint(nt);
          std::list<CFG::NonTerminal*>* newAffectedNonTerminals =
            new std::list<CFG::NonTerminal*> (callStack);
          std::pair<Util::CycleSet*, std::list<CFG::NonTerminal*>* > newElem(
            newCycleSet, newAffectedNonTerminals);
          result.insert(newElem);
        }
        return result;
      } else {
        // ...or just follow the 'link'.
        CFG::GrammarProduction* production = this->grammar->getProduction(
          nonTerminal);
        callStack.push_back(nonTerminal);
        return searchForCycles(nt, production, visitedNonTerminals, callStack);
      }
    }
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* seq = dynamic_cast<CFG::ProductionSequence*>(
        fragment);
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

        // We can only detect cycles in non-terminal sequences. All
        // other things are not cycle-able, because we assume that
        // those other things are only terminals, which prevent cycles
        // by mere presence in a sequence.
        if (!elem->is(CFG::NONTERMINAL) && !isWrappedNonTerminal(elem)) {
          sequenceContainsInvalidElements = true;
          break;
        }

        // The element is either a non-terminal or a wrapped
        // non-terminal, this is for sure. We need the non-terminal
        // so we just unwrap the wrapper, if needed.
        CFG::NonTerminal* nonTerminal;
        if (isWrappedNonTerminal(elem)) {
          CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (elem);
          CFG::NonTerminal* nt = dynamic_cast<CFG::NonTerminal*>(
            wrapper->getWrappedBase());
          assert(nt != NULL);
          nonTerminal = nt;
        } else {
          nonTerminal = dynamic_cast<CFG::NonTerminal*> (elem);
        }

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
        break;  // exit from the switch-statement
      }

      // After dividing all elements of the sequence into
      // those who may produce epsilon, and those who will
      // never produce epsilon, we distinguish two main
      // cases:
      //  1) exactly one element of the sequence is not nullable
      //  2) all elements of the sequence are nullable.
      if (notNullableElements.size() == 1) {
        return searchForCycles(
          nt, visitedNonTerminals, notNullableElements.front(), callStack);
      } else if (notNullableElements.size() == 0) {
        for (std::list<CFG::Base*>::iterator i = nullableElements.begin();
             i != nullableElements.end(); i++) {
          std::set<std::pair<CycleSet*, std::list<CFG::NonTerminal*>* > >
            subResult = searchForCycles(
              nt, visitedNonTerminals, *i, callStack);
          result.insert(subResult.begin(), subResult.end());
        }
        return result;
      }
      break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      // Merge each result of each alternative into
      // one final result.
      CFG::ProductionAlternative* alternativeProduction =
        dynamic_cast<CFG::ProductionAlternative*> (fragment);
      for (CFG::ProductionAlternative::iterator i =
           alternativeProduction->begin();
           i != alternativeProduction->end(); ++i) {
        std::set<std::pair<CycleSet*, std::list<CFG::NonTerminal*>* > >
          subResult = searchForCycles(nt, visitedNonTerminals, *i, callStack);
        result.insert(subResult.begin(), subResult.end());
      }
      return result;
    }
    default: {
      break;
    }
  }
  return result;
}


Util::FirstSet Util::AnnotateCycles::getFirstSet(CFG::NonTerminal* nt) {
  CFG::GrammarProduction* production = grammar->getProduction(nt);
  Util::FirstSetAttribute* attr = (
    Util::FirstSetAttribute*)production->lhs->getAttribute(
      "Util::FirstSetAttribute");
  if (attr == NULL) {
    return FirstSet();
  }
  return attr->getFirstSet();
}


void Util::AnnotateCycles::addCycleMarkAttribute(
  Util::Attributable* attributableInstance, Util::CycleSet* cycleSet) {
  Util::CycleMarkAttribute* cycleMarkAttribute = NULL;
  if (!attributableInstance->containsAttribute("Util::CycleMarkAttribute")) {
    cycleMarkAttribute = new CycleMarkAttribute();
    attributableInstance->setAttribute(cycleMarkAttribute);
  } else {
    Util::Attribute* attribute = attributableInstance->getAttribute(
      "Util::CycleMarkAttribute");
    cycleMarkAttribute = (Util::CycleMarkAttribute*)attribute;
    // there must be a cycle mark attribute, we just checked it above!
    assert(cycleMarkAttribute != NULL);
  }
  // Only if the attribute does not contain this cycle set already.
  if (!cycleMarkAttribute->containsCycleSet(cycleSet)) {
    cycleMarkAttribute->addCycleSet(cycleSet);
  }
}


void Util::AnnotateCycles::addLastCycleElementAttribute(
  CFG::GrammarProduction* production, Util::CycleSet* cycleSet) {
  Util::LastCycleElementAttribute* lastCycleElementAttribute = NULL;
  if (!production->containsAttribute("Util::LastCycleElementAttribute")) {
    lastCycleElementAttribute = new Util::LastCycleElementAttribute();
    production->setAttribute(lastCycleElementAttribute);
  } else {
    Util::Attribute* attribute = production->getAttribute(
      "Util::LastCycleElementAttribute");
    lastCycleElementAttribute = (Util::LastCycleElementAttribute*)attribute;
    assert(lastCycleElementAttribute != NULL);
  }
  lastCycleElementAttribute->addCycleSet(cycleSet);
}


bool Util::AnnotateCycles::isWrappedNonTerminal(CFG::Base* b) {
  if (b->is(CFG::BASE_WRAPPER)) {
    CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
    if (wrapper->getWrappedBase()->is(CFG::NONTERMINAL)) {
      return true;
    }
  }
  return false;
}
