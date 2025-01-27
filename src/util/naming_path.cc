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

#include "naming_path.hh"
#include <string>


// The separator character for the string representation
// of a naming path.
const char Util::NamingPath::separatorChar[] = "/";


Util::NamingPath::NamingPath()
  : prefix(NULL), suffix(new std::string ("")) {
}


Util::NamingPath::NamingPath(std::string* name)
  : prefix(NULL), suffix(name) {
}


Util::NamingPath::NamingPath(NamingPath& p) {
  if (p.prefix != NULL) {
    this->prefix = new NamingPath(*p.prefix);
  } else {
    this->prefix = NULL;
  }
  this->suffix = new std::string(*p.suffix);
}


Util::NamingPath::~NamingPath() {
}


Util::NamingPath* Util::NamingPath::createSubPath(std::string* newName) {
  NamingPath* result = new NamingPath();
  result->prefix = new NamingPath(*this);
  result->suffix = newName;
  return result;
}


std::string Util::NamingPath::toString() {
  std::string result = "";

  if (this->prefix != NULL) {
    result = this->prefix->toString() + separatorChar;
  } else {
    result = separatorChar;
  }

  result += *this->suffix;

  return result;
}
