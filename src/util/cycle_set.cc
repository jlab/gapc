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
#include <list>
#include "cycle_set.hh"


Util::CycleSet::CycleSet() {
}


Util::CycleSet::~CycleSet() {
}


void Util::CycleSet::setMainEntryPoint(CFG::NonTerminal* mainEntryPoint) {
  this->mainEntryPoint = mainEntryPoint;
  // reorder the element ordering, starting with the main
  // entry point.
  std::list<std::string> newOrder;
  std::list<std::string> fetchList;
  bool fetchElements = true;
  for (std::list<std::string>::iterator i = this->orderedElements.begin();
       i != this->orderedElements.end(); i++) {
    std::string element = *i;
    if (element == *mainEntryPoint->getName()) {
      fetchElements = false;
    }
    if (fetchElements) {
      fetchList.push_back(element);
    } else {
      newOrder.push_back(element);
    }
  }
  //
  for (std::list<std::string>::iterator i = fetchList.begin();
       i != fetchList.end(); i++) {
    newOrder.push_back(*i);
  }
  this->orderedElements = newOrder;
}


bool Util::CycleSet::isLastElementInCycle(CFG::NonTerminal* nonTerminal) {
  if (this->orderedElements.size() > 0 &&
      this->orderedElements.back() == *nonTerminal->getName()) {
    return true;
  }
  return false;
}


bool Util::CycleSet::isBackReference(
  CFG::NonTerminal* source, CFG::NonTerminal* destination) {
  if (!containsElement(source) || !containsElement(destination)) {
    return false;
  }
  int sourcePos = -1;
  int destinationPos = -1;
  int pos = 0;
  for (std::list<std::string>::iterator i = this->orderedElements.begin();
       i != this->orderedElements.end(); i++, pos++) {
    if ((*i) == *source->getName()) {
      sourcePos = pos;
    }
    if ((*i) == *destination->getName()) {
      destinationPos = pos;
    }
  }
  return sourcePos >= destinationPos;
}


void Util::CycleSet::addElement(CFG::NonTerminal* nonTerminal) {
  std::string name = *nonTerminal->getName();
  this->set.insert(name);
  this->orderedElements.push_back(name);
}


void Util::CycleSet::addElements(std::list<CFG::NonTerminal*> elements) {
  for (std::list<CFG::NonTerminal*>::iterator i = elements.begin();
       i != elements.end(); i++) {
    addElement(*i);
  }
}


bool Util::CycleSet::containsElement(CFG::NonTerminal* nonTerminal) {
  return this->set.find(*nonTerminal->getName()) != this->set.end();
}


bool Util::CycleSet::isEmpty() {
  return this->set.empty();
}


Util::CycleSet Util::CycleSet::intersect(Util::CycleSet cycleSet) {
  CycleSet result;
  for (std::list<std::string>::iterator i = this->orderedElements.begin();
       i != this->orderedElements.end(); i++) {
    if (cycleSet.set.find(*i) != cycleSet.set.end()) {
      result.set.insert(*i);
      result.orderedElements.push_back(*i);
    }
  }
  if (this->mainEntryPoint == cycleSet.mainEntryPoint) {
    result.mainEntryPoint = this->mainEntryPoint;
  }
  return result;
}


Util::CycleSet Util::CycleSet::difference(Util::CycleSet cycleSet) {
  CycleSet result;
  for (std::list<std::string>::iterator i = this->orderedElements.begin();
       i != this->orderedElements.end(); ++i) {
    if (cycleSet.set.find(*i) == cycleSet.set.end()) {
      result.set.insert(*i);
      result.orderedElements.push_back(*i);
    }
  }
  if (this->mainEntryPoint == cycleSet.mainEntryPoint) {
    result.mainEntryPoint = NULL;
  } else {
    result.mainEntryPoint = this->mainEntryPoint;
  }
  return result;
}


bool Util::CycleSet::operator== (CycleSet& set) {
  if (set.set.size() != this->set.size()) {
    return false;
  }
  for (std::set<std::string>::iterator i = this->set.begin();
       i != this->set.end(); i++) {
    if (set.set.find(*i) == set.set.end()) {
      return false;
    }
  }
  return true;
}


std::string Util::CycleSet::toString() {
  std::string result;
  bool firstElement = true;
  for (std::list<std::string>::iterator i = this->orderedElements.begin();
       i != this->orderedElements.end(); ++i) {
    if (!firstElement) {
      result += ", ";
    }
    result += *i;
    firstElement = false;
  }
  return "{" + result + "}";
}


Util::CycleSet::iterator Util::CycleSet::begin() {
  return this->orderedElements.begin();
}


Util::CycleSet::iterator Util::CycleSet::end() {
  return this->orderedElements.end();
}
