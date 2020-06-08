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

#ifndef SRC_UTIL_ATTRIBUTE_HH_
#define SRC_UTIL_ATTRIBUTE_HH_


#include <string>


namespace Util {


  class Attribute {

    private:

      // The internal name a subclass of the Attribute-class
      // was used to register it as an attribute.
      std::string kindOfName;


    public:

      Attribute (std::string kindOfName);
      Attribute (Attribute& a);
      virtual ~Attribute();

      // Returns TRUE of the name equals the kind-name
      // of this attribute.
      bool isKindOf (std::string kindOfName);

      // Returns the ID of this attribute (this is the same
      // value as the method 'isKindOf' expects and compares.
      std::string getAttributeID();

      //Virtual clone function which creates a copy of the instance.
      virtual Attribute* clone() = 0;


  };


}


#endif  // ifndef SRC_UTIL_ATTRIBUTE_HH_
