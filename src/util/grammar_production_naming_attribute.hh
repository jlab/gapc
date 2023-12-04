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

#ifndef SRC_UTIL_GRAMMAR_PRODUCTION_NAMING_ATTRIBUTE_HH_
#define SRC_UTIL_GRAMMAR_PRODUCTION_NAMING_ATTRIBUTE_HH_


#include <string>
#include "attribute.hh"


namespace Util {
// This attribute is used to annotate a non-terminal
// with its original grammar production name.
class GrammarProductionNamingAttribute : public Attribute {
 private:
    // Stores the original name.
    std::string* originalName;

 public:
    explicit GrammarProductionNamingAttribute(std::string* originalName);
    GrammarProductionNamingAttribute(GrammarProductionNamingAttribute& a);
    ~GrammarProductionNamingAttribute();

    // Returns the original name of the instance annotated
    // by this attribute.
    std::string* getOriginalName();

    // Creates a deep copy of this instance.
    virtual Util::Attribute* clone();
};


}  // namespace Util


#endif  // SRC_UTIL_GRAMMAR_PRODUCTION_NAMING_ATTRIBUTE_HH_
