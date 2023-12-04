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

#ifndef SRC_UTIL_NAMING_DOMAIN_HH_
#define SRC_UTIL_NAMING_DOMAIN_HH_


#include <map>
#include <string>

#include "naming_path.hh"


namespace Util {
class NamingDomain {
 private:
    // The parent domain of this naming domain.
    NamingDomain* parentDomain;

    // Stores the mapping of all aliasses on any naming-path.
    std::map<std::string, std::string*> aliasMap;
    // Each alias name consists of the string "rule" and a
    // unique number (this one). Each naming-domain has its
    // own numbering cycle, because this variable is not a
    // static variable.
    static unsigned int nextNameNumber;

 public:
    NamingDomain();
    explicit NamingDomain(NamingDomain* d);
    ~NamingDomain();

    // Returns TRUE if the domain contains the name in
    // the given naming-path.
    bool containsName(std::string* name);
    bool containsName(std::string name);

    // Returns an alias for the given name. If this name
    // had no alias before, a new alias will be created.
    std::string* getAlias(std::string* name);
    std::string* getAlias(std::string name);
};
}  // namespace Util


#endif  // SRC_UTIL_NAMING_DOMAIN_HH_
