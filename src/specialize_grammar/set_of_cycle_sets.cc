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

#include "set_of_cycle_sets.hh"

#include <cassert>


Util::SetOfCycleSets::SetOfCycleSets() {
}


Util::SetOfCycleSets::SetOfCycleSets(std::set<CycleSet*> sets) {
  for (std::set<CycleSet*>::iterator i = sets.begin(); i != sets.end(); i++) {
    addCycleSet(*i);
  }
}


Util::SetOfCycleSets::SetOfCycleSets(SetOfCycleSets& s)
  : sets(s.sets) {
}


Util::SetOfCycleSets::~SetOfCycleSets() {
}


void Util::SetOfCycleSets::addCycleSet(CycleSet* cycleSet) {
  this->sets.insert(cycleSet);
}


bool Util::SetOfCycleSets::containsCycleSet(CycleSet* cycleSet) {
  for (std::set<CycleSet*>::iterator i = this->sets.begin();
       i != this->sets.end(); i++) {
    if (*(*i) == *cycleSet) {
      return true;
    }
  }
  return false;
}


bool Util::SetOfCycleSets::containsElement(CFG::NonTerminal* nonTerminal) {
  for (std::set<CycleSet*>::iterator i = this->sets.begin();
       i != this->sets.end(); i++) {
    if ((*i)->containsElement(nonTerminal)) {
      return true;
    }
  }
  return false;
}


bool Util::SetOfCycleSets::isEmpty() {
  return this->sets.empty();
}


bool Util::SetOfCycleSets::isBackReference(
  CFG::NonTerminal* source, CFG::NonTerminal* destination) {
  bool allSetsAreBackReferences = true;
  bool someSetIsBackReference = false;

  // This algorithm calculates both information: the source
  // and destination non-terminals are no back-reference in
  // no cycle-set at all, and both non-terminals are back
  // references in all cycle sets.
  // First we check if both, source and destination are
  // contained in this set-set.
  if (!containsElement(source) || !containsElement(destination)) {
    // should this be an error message?
    return false;
  }

  for (std::set<Util::CycleSet*>::iterator i = this->sets.begin();
       i != this->sets.end(); i++) {
    bool singleResult = (*i)->isBackReference(source, destination);
    allSetsAreBackReferences &= singleResult;
    someSetIsBackReference |= singleResult;
  }

  // Both information should be complementary, that is only
  // one flag may be TRUE at a time.
  assert(allSetsAreBackReferences ^ !someSetIsBackReference);

  // At least one set needs to be a back reference in order to
  // establish a real back reference.
  return someSetIsBackReference;
}


std::string Util::SetOfCycleSets::toString() {
  std::string result;

  bool firstLoopRun = true;
  for (std::set<Util::CycleSet*>::iterator i = this->sets.begin();
       i != this->sets.end(); i++) {
    if (!firstLoopRun) {
      result += ", ";
    }
    result += (*i)->toString();
    firstLoopRun = false;
  }

  return "<" + result + ">";
}
