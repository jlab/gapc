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

#include <set>
#include "cycle_mark_attribute.hh"


Util::CycleMarkAttribute::CycleMarkAttribute()
  : Attribute("Util::CycleMarkAttribute") {
}


Util::CycleMarkAttribute::CycleMarkAttribute(CycleMarkAttribute& a)
  : Attribute(a), cycleSets(a.cycleSets) {
}


Util::CycleMarkAttribute::~CycleMarkAttribute() {
}


void Util::CycleMarkAttribute::addCycleSet(CycleSet* set) {
  this->cycleSets.insert(set);
}


bool Util::CycleMarkAttribute::containsCycleSet(CycleSet* set) {
  for (std::set<CycleSet*>::iterator i = this->cycleSets.begin();
       i != this->cycleSets.end(); i++) {
    if (*(*i) == *set) {
      return true;
    }
  }
  return false;
}


std::set<Util::CycleSet*> Util::CycleMarkAttribute::getCycleSets() {
  return this->cycleSets;
}


Util::Attribute* Util::CycleMarkAttribute::clone() {
  return new CycleMarkAttribute(*this);
}
