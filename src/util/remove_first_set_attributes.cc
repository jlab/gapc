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

#include <list>
#include "remove_first_set_attributes.hh"


Util::RemoveFirstSetAttributes::RemoveFirstSetAttributes() {
}


Util::RemoveFirstSetAttributes::~RemoveFirstSetAttributes() {
}


void Util::RemoveFirstSetAttributes::removeFromGrammar(CFG::CFG* grammar) {
  std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
  for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin();
       i != productions.end(); i++) {
    removeFromProduction(*i);
  }
}


void Util::RemoveFirstSetAttributes::removeFromProduction(
  CFG::GrammarProduction* production) {
  production->removeAttribute("Util::FirstSetAttribute");
  production->lhs->removeAttribute("Util::FirstSetAttribute");
  removeFromBase(production->rhs);
}


void Util::RemoveFirstSetAttributes::removeFromBase(CFG::Base* b) {
  switch (b->getType()) {
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* sequence =
        dynamic_cast<CFG::ProductionSequence*> (b);

      for (CFG::ProductionSequence::iterator i = sequence->begin();
           i != sequence->end(); i++) {
        removeFromBase(*i);
      }

      break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      CFG::ProductionAlternative* alternative =
        dynamic_cast<CFG::ProductionAlternative*> (b);

      for (CFG::ProductionAlternative::iterator i = alternative->begin();
           i != alternative->end(); i++) {
        removeFromBase(*i);
      }

      break;
    }
    default: {
    }
  }
  b->removeAttribute("Util::FirstSetAttribute");
}
