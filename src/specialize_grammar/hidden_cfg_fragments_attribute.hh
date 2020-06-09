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

#ifndef SRC_SPECIALIZE_GRAMMAR_HIDDEN_CFG_FRAGMENTS_ATTRIBUTE_HH_
#define SRC_SPECIALIZE_GRAMMAR_HIDDEN_CFG_FRAGMENTS_ATTRIBUTE_HH_


#include <list>
#include <set>

#include "../cfg/cfg.hh"
#include "../util/attribute.hh"


namespace SpecializeGrammar {


  class HiddenCFGFragmentsAttribute : public Util::Attribute {

    private:

      // The list of all hidden CFG fragments.
      std::list<CFG::Base*> hiddenFragments;


    public:

      HiddenCFGFragmentsAttribute();
      HiddenCFGFragmentsAttribute(HiddenCFGFragmentsAttribute& a);
      virtual ~HiddenCFGFragmentsAttribute();

      void addHiddenFragment(CFG::Base* b);
      void addHiddenFragments(std::set<CFG::Base*>* fragments);

      typedef std::list<CFG::Base*>::iterator iterator;
      iterator begin();
      iterator end();

      virtual Util::Attribute* clone();


  };


}  // namespace SpecializeGrammar


#endif  // SRC_SPECIALIZE_GRAMMAR_HIDDEN_CFG_FRAGMENTS_ATTRIBUTE_HH_
