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

#include "cycle_attribute.hh"
#include <set>


Util::CycleAttribute::CycleAttribute(std::set<CycleSet*> cycleSets)
  : Attribute("Util::CycleAttribute") {
  this->addCycleSets(cycleSets);
}


Util::CycleAttribute::CycleAttribute(CycleAttribute& a)
  : Attribute(a) {
  this->addCycleSets(a.cycleSets);
}


Util::CycleAttribute::~CycleAttribute() {
}


std::set<Util::CycleSet*> Util::CycleAttribute::getCycleSets() {
  return this->cycleSets;
}


Util::Attribute* Util::CycleAttribute::clone() {
  CycleAttribute* copy = new CycleAttribute (this->cycleSets);
  return copy;
}


bool Util::CycleAttribute::containsCycleSet(CycleSet* set) {
  for (std::set<CycleSet*>::iterator i = this->cycleSets.begin();
       i != this->cycleSets.end(); i++) {
    if (*(*i) == *set) {
      return true;
    }
  }
  return false;
}


void Util::CycleAttribute::addCycleSets(std::set<CycleSet*> sets) {
  for (std::set<CycleSet*>::iterator i = sets.begin(); i != sets.end(); i++) {
    if (!this->containsCycleSet(*i)) {
      this->cycleSets.insert(*i);
    }
  }
}
