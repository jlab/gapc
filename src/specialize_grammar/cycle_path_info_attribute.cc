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
#include <list>
#include "cycle_path_info_attribute.hh"


Util::CyclePathInfoAttribute::CyclePathInfoAttribute()
  : Attribute("Util::CyclePathInfoAttribute") {
}


Util::CyclePathInfoAttribute::CyclePathInfoAttribute(CyclePathInfoAttribute& a)
  : Attribute(a) {
}


Util::CyclePathInfoAttribute::~CyclePathInfoAttribute() {
}


void Util::CyclePathInfoAttribute::addElement(
  std::string nonTerminalName, CFG::Base* fragment) {
  this->elements.push_back(std::pair<std::string, CFG::Base*>(
    nonTerminalName, fragment));
}


void Util::CyclePathInfoAttribute::addElements(
  std::list< std::pair<std::string, CFG::Base*> >* elems,
  unsigned int startPos) {
  unsigned int pos = 0;
  for (std::list< std::pair<std::string, CFG::Base*> >::iterator i =
       elems->begin(); i != elems->end(); i++, pos++) {
    if (pos >= startPos) {
      this->elements.push_back(std::pair<std::string, CFG::Base*>(
        (*i).first, (*i).second));
    }
  }
}


Util::CyclePathInfoAttribute::iterator Util::CyclePathInfoAttribute::begin() {
  return this->elements.begin();
}


Util::CyclePathInfoAttribute::iterator Util::CyclePathInfoAttribute::end() {
  return this->elements.end();
}


Util::Attribute* Util::CyclePathInfoAttribute::clone() {
  return new CyclePathInfoAttribute(*this);
}
