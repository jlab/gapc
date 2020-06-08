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

#ifndef SRC_UTIL_NAMING_PATH_HH_
#define SRC_UTIL_NAMING_PATH_HH_


#include <string>


namespace Util {


  class NamingPath {

    private:

      // The character that is used as a separator when
      // the pretty print is created.
      static std::string separatorChar;

      // The prefix of this naming-path, or NULL if this
      // is the root of the path.
      NamingPath* prefix;
      // the name of this path element, or "" if this is
      // the root of the path.
      std::string* suffix;


    public:

      // Creates an empty naming path.
      NamingPath();
      // Creates a naming-path with the a first suffix
      // as given by the parameter 'name'.
      NamingPath (std::string* name);
      // Copy constructor, creates a deep copy of this instance.
      NamingPath (NamingPath& p);
      ~NamingPath();

      // Returns a new naming path with this naming path
      // as prefix, and the 'newName' as suffix.
      NamingPath* createSubPath (std::string* newName);

      // Returns a string representation of this naming-path,
      // which is a forward-slash separated ('/') list of all its names.
      std::string toString();


  };


}


#endif  // ifndef SRC_UTIL_NAMING_PATH_HH_
