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

#include "hidden_cfg_fragments_attribute.hh"
#include <set>


SpecializeGrammar::HiddenCFGFragmentsAttribute::HiddenCFGFragmentsAttribute()
  : Attribute("SpecializeGrammar::HiddenCFGFragmentsAttribute") {
}


SpecializeGrammar::HiddenCFGFragmentsAttribute::HiddenCFGFragmentsAttribute(
  HiddenCFGFragmentsAttribute& a)
  : Attribute(a), hiddenFragments(a.hiddenFragments) {
}


SpecializeGrammar::HiddenCFGFragmentsAttribute::~HiddenCFGFragmentsAttribute() {
}


void SpecializeGrammar::HiddenCFGFragmentsAttribute::addHiddenFragment(
  CFG::Base* b) {
  this->hiddenFragments.push_back(b);
}


void SpecializeGrammar::HiddenCFGFragmentsAttribute::addHiddenFragments(
  std::set<CFG::Base*>* fragments) {
  for (std::set<CFG::Base*>::iterator i = fragments->begin();
       i != fragments->end(); i++) {
    addHiddenFragment(*i);
  }
}


SpecializeGrammar::HiddenCFGFragmentsAttribute::iterator
  SpecializeGrammar::HiddenCFGFragmentsAttribute::begin() {
  return this->hiddenFragments.begin();
}


SpecializeGrammar::HiddenCFGFragmentsAttribute::iterator
  SpecializeGrammar::HiddenCFGFragmentsAttribute::end() {
  return this->hiddenFragments.end();
}


Util::Attribute* SpecializeGrammar::HiddenCFGFragmentsAttribute::clone() {
  return new HiddenCFGFragmentsAttribute(*this);
}
