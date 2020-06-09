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
#include <iostream>

#include "naming_domain.hh"

#include "boost/format.hpp"


unsigned int Util::NamingDomain::nextNameNumber = 0;


Util::NamingDomain::NamingDomain()
  : parentDomain(NULL) {
}


Util::NamingDomain::NamingDomain(Util::NamingDomain* parentDomain)
  : parentDomain(parentDomain) {
}


Util::NamingDomain::~NamingDomain() {
}


bool Util::NamingDomain::containsName(std::string* name) {
  return containsName(*name);
}


bool Util::NamingDomain::containsName(std::string name) {
  if (this->aliasMap.find(name) != this->aliasMap.end()) {
    return true;
  } else if (this->parentDomain != NULL) {
    return this->parentDomain->containsName(name);
  }
  return false;
}


std::string* Util::NamingDomain::getAlias(std::string* name) {
  return getAlias(*name);
}


std::string* Util::NamingDomain::getAlias(std::string name) {
  if (this->aliasMap.find(name) != this->aliasMap.end()) {
    return this->aliasMap[name];
  } else if (this->containsName(name)) {
    // The alias must be defined in the parent domain,
    // because it is not in the local alias map, but
    // it is found in the whole nested map, which also
    // inplies that the parent domain is not NULL!
    assert(this->parentDomain != NULL);
    return this->parentDomain->getAlias(name);
  } else {
    // create a new alias for the name:
    std::string* newAliasName = new std::string(
      "rule" + str(boost::format("%i") % nextNameNumber++));
    this->aliasMap[name] = newAliasName;
    return newAliasName;
  }
}
