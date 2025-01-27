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

#include <utility>
#include <string>
#include <map>
#include "call_trace.hh"


Util::CallTrace::CallTrace() {
}


Util::CallTrace::CallTrace(CallTrace& t)
  : callTrace(t.callTrace), searchPool(t.searchPool) {
}


Util::CallTrace::~CallTrace() {
}


void Util::CallTrace::push(CFG::NonTerminal* nt) {
  this->push(nt, NULL);
}


void Util::CallTrace::push(
  CFG::NonTerminal* nt, Util::SetOfCycleSets* cycleSets) {
  this->callTrace.push_back(PairedElementType(
    *nt->getName(), cycleSets));
  this->searchPool[*nt->getName()] = PairedElementType(
    *nt->getName(), cycleSets);
}


bool Util::CallTrace::isEmpty() {
  return this->callTrace.empty();
}


bool Util::CallTrace::contains(CFG::NonTerminal* nt) {
  return this->searchPool.find(*nt->getName()) != this->searchPool.end();
}


std::pair<std::string, Util::SetOfCycleSets*> Util::CallTrace::
  searchForCycleSetContainingNonTerminal(CFG::NonTerminal* nt) {
  for (std::map<std::string, PairedElementType>::iterator i =
       this->searchPool.begin(); i != this->searchPool.end(); i++) {
    if ((*i).second.second != NULL &&
        (*i).second.second->containsElement(nt)) {
      return (*i).second;
    }
  }
  return std::pair<std::string, Util::SetOfCycleSets*>("", NULL);
}


Util::SetOfCycleSets* Util::CallTrace::pop() {
  PairedElementType elem = this->callTrace.back();
  this->callTrace.pop_back();
  this->searchPool.erase(elem.first);
  return elem.second;
}


std::pair<std::string, Util::SetOfCycleSets*> Util::CallTrace::peek() {
  return this->callTrace.back();
}


std::string Util::CallTrace::toString() {
  std::string result;

  bool firstLoopRun = true;
  for (std::vector<PairedElementType>::iterator i =
       this->callTrace.begin(); i != this->callTrace.end(); i++) {
    PairedElementType element = *i;
    if (!firstLoopRun) {
      result += ", ";
    }
    std::string cycleSetAsString = "NLL";
    if (element.second != NULL) {
      cycleSetAsString = element.second->toString();
    }
    result += "(" + element.first + " | " + cycleSetAsString + ")";
    firstLoopRun = false;
  }

  // Make it look nice:
  return "{" + result + "}";
}


Util::NamingPath* Util::CallTrace::getNamingPath(
  CFG::NonTerminal* nonTerminal) {
  NamingPath* result = new NamingPath();

  // Now copy the contents back onto the stack.
  for (std::vector<PairedElementType>::iterator
       i = this->callTrace.begin(); i != this->callTrace.end(); i++) {
    PairedElementType element = *i;
    std::string* elementName = new std::string(element.first);
    NamingPath* nextResult = result->createSubPath(elementName);
    delete(result);
    result = nextResult;
    if ((*i).first == *nonTerminal->getName()) {
      break;
    }
  }

  return result;
}
