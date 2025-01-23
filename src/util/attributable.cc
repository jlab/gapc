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

#include "attributable.hh"
#include <map>
#include <string>


Util::Attributable::Attributable() {
}


Util::Attributable::Attributable(Attributable& a) {
  for (std::map<std::string, Util::Attribute*>::iterator
       i = a.attributeMap.begin(); i != a.attributeMap.end(); i++) {
    this->attributeMap[(*i).first] = (*i).second;
  }
}


Util::Attributable::~Attributable() {
}


void Util::Attributable::setAttribute(Util::Attribute* attr) {
  if (attr != NULL) {
    this->setAttribute(attr->getAttributeID(), attr);
  }
}


void Util::Attributable::setAttribute(std::string key, Util::Attribute* attr) {
  this->attributeMap[key] = attr;
}


Util::Attribute* Util::Attributable::getAttribute(std::string key) {
  return this->attributeMap[key];
}


bool Util::Attributable::containsAttribute(std::string key) {
  return this->attributeMap.find (key) != this->attributeMap.end();
}


bool Util::Attributable::removeAttribute(std::string key) {
  if (containsAttribute(key)) {
    this->attributeMap.erase(key);
    return true;
  }
  return false;
}


void Util::Attributable::clearAttributes() {
  this->attributeMap.clear();
}


Util::Attributable::iterator Util::Attributable::begin() {
  return this->attributeMap.begin();
}


Util::Attributable::iterator Util::Attributable::end() {
  return this->attributeMap.end();
}
