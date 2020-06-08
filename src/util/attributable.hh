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

#ifndef __ATTRIBUTABLE_HH__
#define __ATTRIBUTABLE_HH__


#include <map>
#include <string>

#include "attribute.hh"


namespace Util {


  /*
   * Stores and retrieves attributes for anything you like to put
   * a lable on. Note that there is a one to one correspondence
   * between an attribute name, and the stored attribute. That means
   * there may not be more than one attribute stored for a given
   * name. If there is already an attribute stored for a given
   * name, while the user tries to set an other value, the old
   * value is overwritten.
   */
  class Attributable {

    private:

      std::map<std::string, Util::Attribute*> attributeMap;


    public:

      Attributable();
      Attributable (Attributable& a);
      ~Attributable();


      // Stores the attribute under its attribute-ID.
      void setAttribute (Util::Attribute* attr);
      // Forces this attributable instance to store the attribute
      // under an other name than the attribute-ID.
      void setAttribute (std::string key, Util::Attribute* attr);
      Util::Attribute* getAttribute (std::string key);
      bool containsAttribute (std::string key);

      // Removes the attribute for the given key from this instance.
      // If this instance held no such attribute, the method returns
      // FALSE, otherwise TRUE is returned.
      bool removeAttribute (std::string key);
      // Removes all attributes from this instance.
      void clearAttributes();

      typedef std::map<std::string, Util::Attribute*>::iterator iterator;
      iterator begin();
      iterator end();


  };


}


#endif  // ifndef __ATTRIBUTABLE_HH__
