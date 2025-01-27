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

#include "last_element_of_cycle_attribute.hh"
#include <set>


Util::LastCycleElementAttribute::LastCycleElementAttribute()
  : Attribute("Util::LastCycleElementAttribute") {
}


Util::LastCycleElementAttribute::LastCycleElementAttribute(
  LastCycleElementAttribute& a)
  : Attribute(a) {
  for (iterator i = a.begin(); i != a.end(); i++) {
    this->cycleSets.insert(*i);
  }
}


Util::LastCycleElementAttribute::~LastCycleElementAttribute() {
}


void Util::LastCycleElementAttribute::addCycleSet(CycleSet* cycleSet) {
  this->cycleSets.insert(cycleSet);
}


std::set<Util::CycleSet*> Util::LastCycleElementAttribute::getCycleSets() {
  return this->cycleSets;
}


Util::LastCycleElementAttribute::iterator
  Util::LastCycleElementAttribute::begin() {
  return this->cycleSets.begin();
}


Util::LastCycleElementAttribute::iterator
  Util::LastCycleElementAttribute::end() {
  return this->cycleSets.end();
}


Util::Attribute* Util::LastCycleElementAttribute::clone() {
  return new LastCycleElementAttribute (*this);
}
