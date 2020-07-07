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

#ifndef SRC_SPECIALIZE_GRAMMAR_CYCLE_PATH_INFO_ATTRIBUTE_HH_
#define SRC_SPECIALIZE_GRAMMAR_CYCLE_PATH_INFO_ATTRIBUTE_HH_

#include <string>
#include <utility>
#include <list>
#include <set>

#include "../cfg/cfg.hh"
#include "../util/attribute.hh"


namespace Util {


class CyclePathInfoAttribute : public Attribute {
 private:
    std::list< std::pair<std::string, CFG::Base*> > elements;

 public:
    CyclePathInfoAttribute();
    CyclePathInfoAttribute(CyclePathInfoAttribute& a);
    virtual ~CyclePathInfoAttribute();

    void addElement(std::string nonTerminalName, CFG::Base* fragment);
    void addElements(
      std::list< std::pair<std::string, CFG::Base*> >* elems,
      unsigned int startPos);

    typedef std::list< std::pair<std::string, CFG::Base*> >
      ::iterator iterator;
    iterator begin();
    iterator end();

    virtual Attribute* clone();
};


}  // namespace Util


#endif  // SRC_SPECIALIZE_GRAMMAR_CYCLE_PATH_INFO_ATTRIBUTE_HH_
