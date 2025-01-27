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
#include <iostream>
#include <string>

#include "annotate_the_set_first.hh"

#include "../log.hh"


Util::AnnotateTheSetFirst::AnnotateTheSetFirst() {
}


Util::AnnotateTheSetFirst::~AnnotateTheSetFirst() {
}


void Util::AnnotateTheSetFirst::annotateGrammar(CFG::CFG* grammar) {
  // The list of all productions the grammar has.
  std::list<CFG::GrammarProduction*> productions = grammar->getProductions();

  // A flag that is set TRUE, when something has changed
  // in the current state of any FIRST-set. Initially we
  // set this value TRUE to get the algorithm started.
  bool somethingChanged = true;
  while (somethingChanged) {
    // A flag that is set FALSE when something has changed
    // in any FIRST-set.
    bool nothingChanged = true;
    for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin();
         i != productions.end(); ++i) {
      CFG::NonTerminal* nt = (*i)->lhs;
      // If the FIRST-set map does not contain any set
      // for the non-terminal, we create a new set.
      if (firstSetMap.find(*nt->getName()) == firstSetMap.end()) {
        this->firstSetMap[*nt->getName()] = new FirstSet();
      }
      // Get the FIRST-set for the non-terminal name.
      FirstSet* firstSet = this->firstSetMap[*nt->getName()];

      // For each alternative, get the first terminal symbols
      // they create.
      for (CFG::ProductionAlternative::iterator j = (*i)->rhs->begin();
           j != (*i)->rhs->end(); j++) {
        nothingChanged &= !extractFirstTerminals(firstSet, *j);
      }
    }
    somethingChanged = !nothingChanged;
  }

  // Now that all FIRST-sets are computed for each non-terminal,
  // we process the whole grammar a second time, and annotate
  // each CFG node.
  annotateProductions(grammar);
}


bool Util::AnnotateTheSetFirst::extractFirstTerminals(
  Util::FirstSet* firstSet, CFG::Base* fragment) {
  std::string terminalString;
  switch (fragment->getType()) {
    case CFG::BASE_WRAPPER: {
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (fragment);
      return extractFirstTerminals(firstSet, wrapper->getWrappedBase());
    }
    case CFG::EPSILON: {
      CFG::Epsilon* e = dynamic_cast<CFG::Epsilon*> (fragment);
      bool anythingChanged = !firstSet->containsElement(e);
      firstSet->addElement(e);
      return anythingChanged;
    }
    case CFG::TERMINAL: {
      CFG::Terminal* t = dynamic_cast<CFG::Terminal*> (fragment);
      bool anythingChanged = !firstSet->containsElement(t);
      firstSet->addElement(t);
      return anythingChanged;
    }
    case CFG::REGULAR_EXPRESSION: {
      CFG::RegularExpression* regExpr =
        dynamic_cast<CFG::RegularExpression*> (fragment);
      bool anythingChanged = !firstSet->containsElement(regExpr);
      firstSet->addElement(regExpr);
      return anythingChanged;
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nt = dynamic_cast<CFG::NonTerminal*> (fragment);
      // If the FIRST-set map does not contain any set
      // for the non-terminal, we create a new set.
      if (firstSetMap.find(*nt->getName()) == firstSetMap.end()) {
        this->firstSetMap[*nt->getName()] = new FirstSet();
      }
      // Get the FIRST-set for the non-terminal name.
      FirstSet* ntSet = this->firstSetMap[*nt->getName()];

      // Merge the set of the non-terminal with the set which
      // was passed as a parameter to this method.
      bool anythingChanged = !(ntSet->difference(
        firstSet->intersect(*ntSet)).isEmpty());
      firstSet->addElements(*ntSet);

      return anythingChanged;
    }
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* seq =
        dynamic_cast<CFG::ProductionSequence*> (fragment);
      bool anythingChanged = extractFirstTerminalsFromSequence(firstSet, seq);
      return anythingChanged;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      CFG::ProductionAlternative* alt =
        dynamic_cast<CFG::ProductionAlternative*> (fragment);
      bool anythingChanged = false;
      for (CFG::ProductionAlternative::iterator i = alt->begin();
           i != alt->end(); i++) {
        anythingChanged |= extractFirstTerminals(firstSet, *i);
      }
      return anythingChanged;
    }
    default: {
      throw LogError("gap-00734: Internal: unhandled CFG node type.");
    }
  }

  return false;
}


bool Util::AnnotateTheSetFirst::extractFirstTerminalsFromSequence(
  Util::FirstSet* firstSet, CFG::ProductionSequence* seq) {
  // We need an instance of epsilon inside of the loop,
  // so we create just one here, and delete it at the end
  // of this method.
  CFG::Epsilon* epsilon = new CFG::Epsilon();
  FirstSet* workingSet = new FirstSet();
  bool anythingChanged = false;
  bool allWorkingSetsContainedEpsilon = true;
  for (int i = 0; i < seq->getSize(); i++) {
    CFG::Base* fragment = seq->elementAt(i);
    workingSet->clear();
    extractFirstTerminals(workingSet, fragment);
    // Save the information if epsilon was present in
    // the working-set, and remove it from the set
    bool workingSetContainedEpsilon = workingSet->containsElement(epsilon);
    workingSet->removeElement(epsilon);
    anythingChanged |= !workingSet->difference(
      firstSet->intersect(*workingSet)).isEmpty();
    firstSet->addElements(*workingSet);
    // If the FIRST-set of the current element does not
    // contain epsilon, we go no further.
    // if (!workingSet->containsElement (epsilon)) {
    if (!workingSetContainedEpsilon) {
      allWorkingSetsContainedEpsilon = false;
      break;
    }
    // Otherwise we check the next element
  }
  // Now if all elements contained epsilon in their
  // intermediate results, we also add epsilon to the
  // overall result:
  if (allWorkingSetsContainedEpsilon) {
    firstSet->addElement(epsilon);
  }
  delete epsilon;
  delete workingSet;
  return anythingChanged;
}


void Util::AnnotateTheSetFirst::annotateProductions(CFG::CFG* grammar) {
  std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
  for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin();
       i != productions.end(); i++) {
    annotateProduction(*i);
  }
}


void Util::AnnotateTheSetFirst::annotateProduction(
  CFG::GrammarProduction* production) {
  CFG::NonTerminal* nt = production->lhs;
  // Each non-terminal should have by now a FIRST-set stored
  // for its name. If there is no set associacted to the name,
  // there is an internal error!
  if (firstSetMap.find(*nt->getName()) == firstSetMap.end()) {
    // TODO(who?): logError!
  }
  // Get the FIRST-set for the non-terminal name.
  FirstSet* firstSet = firstSetMap[*nt->getName()];
  // Now annotate the non-terminal.
  annotateFirstSet(nt, firstSet);

  // At the end, annotate all sub-nodes.
  annotateBase(production->rhs);
}


Util::FirstSet Util::AnnotateTheSetFirst::annotateBase(CFG::Base* b) {
  FirstSet result;

  switch (b->getType()) {
    case CFG::BASE_WRAPPER: {
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
      result.addElements(annotateBase(wrapper->getWrappedBase()));
      break;
    }
    case CFG::EPSILON: {
      CFG::Epsilon* e = dynamic_cast<CFG::Epsilon*> (b);
      result.addElement(e);
      break;
    }
    case CFG::TERMINAL: {
      CFG::Terminal* t = dynamic_cast<CFG::Terminal*> (b);
      result.addElement(t);
      break;
    }
    case CFG::REGULAR_EXPRESSION: {
      CFG::RegularExpression* r = dynamic_cast<CFG::RegularExpression*> (b);
      result.addElement(r);
      break;
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nt = dynamic_cast<CFG::NonTerminal*> (b);
      if (firstSetMap.find(*nt->getName()) == firstSetMap.end()) {
        throw LogError(
          "gap-00640: Internal: no FIRST-set for non-terminal" +
          *nt->getName());
      }
      // Get the FIRST-set for the non-terminal name.
      FirstSet* firstSet = firstSetMap[*nt->getName()];
      result.addElements(*firstSet);
      break;
    }
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* sequence =
        dynamic_cast<CFG::ProductionSequence*> (b);

      // We use the method which extracts the FIRST-set
      // from a sequence of elements, because FIRST of a
      // sequence is FIRST of its first element, plus FIRST
      // of its second element, of the first element's FIRST
      // contained eplilon. If the second element's FIRST
      // set contained epsilon, we go and add all elements
      // from the third set, and so on.
      // extractFirstTerminalsFromSequence (&result, sequence);
      CFG::Epsilon* epsilon = new CFG::Epsilon();
      FirstSet workingSet;
      bool allWorkingSetsContainedEpsilon = true;
      for (CFG::ProductionSequence::iterator i = sequence->begin();
           i != sequence->end(); i++) {
        workingSet.clear();
        workingSet = annotateBase(*i);

        bool workingSetContainedEpsilon = workingSet.containsElement(epsilon);
        workingSet.removeElement(epsilon);
        // anythingChanged |= !workingSet.difference (result.intersect
        // (workingSet)).isEmpty();
        result.addElements(workingSet);
        // If the FIRST-set of the current element does not
        // contain epsilon, we go no further.
        if (!workingSetContainedEpsilon) {
          allWorkingSetsContainedEpsilon = false;
          break;
        }
      }
      // The overall result contains epsilon only if all
      // elements contained epsilon.
      if (allWorkingSetsContainedEpsilon) {
        result.addElement(epsilon);
      }
      // Displose this helper instance.
      delete epsilon;

      break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      CFG::ProductionAlternative* alternative =
        dynamic_cast<CFG::ProductionAlternative*> (b);

      for (CFG::ProductionAlternative::iterator i = alternative->begin();
           i != alternative->end(); i++) {
        result.addElements(annotateBase(*i));
      }

      break;
    }
    default: {
      throw LogError("gap-733: Internal: unhandled CFG node type");
    }
  }

  // Annotate the CFG node.
  annotateFirstSet(b, &result);

  // Also return the result for all outer levels of annotation.
  return result;
}


void Util::AnnotateTheSetFirst::annotateFirstSet(
  Util::Attributable* a, Util::FirstSet* set) {
  a->setAttribute(new FirstSetAttribute (*set));
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


Util::FirstSet::FirstSet() {
  this->containsInfinity = false;
}


Util::FirstSet::~FirstSet() {
}


void Util::FirstSet::addElement(CFG::Epsilon* elem) {
  this->set.insert("");
}


void Util::FirstSet::addElement(CFG::Terminal* elem) {
  this->set.insert(*elem->getValue());
}


void Util::FirstSet::addElement(CFG::RegularExpression* elem) {
  this->set.insert(*elem->getExpression());
}


void Util::FirstSet::addINF() {
  this->containsInfinity = true;
}


void Util::FirstSet::addElements(Util::FirstSet set) {
  for (std::set<std::string>::iterator i = set.set.begin();
       i != set.set.end(); ++i) {
    this->set.insert(*i);
  }
}


void Util::FirstSet::removeElement(CFG::Epsilon* elem) {
  this->set.erase("");
}


void Util::FirstSet::removeElement(CFG::Terminal* elem) {
  this->set.erase(*elem->getValue());
}


void Util::FirstSet::removeElement(CFG::RegularExpression* elem) {
  this->set.erase(*elem->getExpression());
}


void Util::FirstSet::removeINF() {
  this->containsInfinity = false;
}


bool Util::FirstSet::containsElement(CFG::Epsilon* elem) {
  return this->set.find("") != this->set.end();
}


bool Util::FirstSet::containsElement(CFG::Terminal* elem) {
  return this->set.find(*elem->getValue()) != this->set.end();
}


bool Util::FirstSet::containsElement(CFG::RegularExpression* elem) {
  return this->set.find(*elem->getExpression()) != this->set.end();
}


bool Util::FirstSet::containsINF() {
  return this->containsInfinity;
}


void Util::FirstSet::clear() {
  this->containsInfinity = false;
  this->set.clear();
}


Util::FirstSet Util::FirstSet::intersect(Util::FirstSet firstSet) {
  FirstSet newSet;
  newSet.containsInfinity = this->containsInfinity && firstSet.containsInfinity;
  for (std::set<std::string>::iterator i = this->set.begin();
       i != this->set.end(); ++i) {
    if (firstSet.set.find(*i) != firstSet.set.end()) {
      newSet.set.insert(*i);
    }
  }
  return newSet;
}


Util::FirstSet Util::FirstSet::difference(Util::FirstSet firstSet) {
  FirstSet result;
  result.containsInfinity = this->containsInfinity & !firstSet.containsInfinity;
  for (std::set<std::string>::iterator i = this->set.begin();
       i != this->set.end(); ++i) {
    if (firstSet.set.find(*i) == firstSet.set.end()) {
      result.set.insert(*i);
    }
  }
  return result;
}


bool Util::FirstSet::isEmpty() {
  return this->set.empty() && !this->containsInfinity;
}


unsigned int Util::FirstSet::size() {
  return this->set.size();
}


std::string Util::FirstSet::toString() {
  std::string res;
  bool firstElement = true;

  if (this->containsInfinity) {
    res += "INF";
    firstElement = false;
  }

  for (std::set<std::string>::iterator i = this->set.begin();
       i != this->set.end(); ++i) {
    if (!firstElement) {
      res += ", ";
    }
    res += "\"" + *i + "\"";
    firstElement = false;
  }

  return "{" + res + "}";
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


Util::FirstSetAttribute::FirstSetAttribute(Util::FirstSet firstSet)
  : Util::Attribute("Util::FirstSetAttribute") {
  this->firstSet = firstSet;
}


Util::FirstSetAttribute::FirstSetAttribute(FirstSetAttribute& a)
  : Attribute(a) {
  this->firstSet = firstSet;
}


Util::FirstSetAttribute::~FirstSetAttribute() {
}


Util::FirstSet Util::FirstSetAttribute::getFirstSet() {
  return this->firstSet;
}


Util::Attribute* Util::FirstSetAttribute::clone() {
  FirstSetAttribute* copy = new FirstSetAttribute (this->firstSet);
  return copy;
}
