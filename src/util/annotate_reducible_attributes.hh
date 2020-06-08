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

#ifndef SRC_UTIL_ANNOTATE_REDUCIBLE_ATTRIBUTES_HH_
#define SRC_UTIL_ANNOTATE_REDUCIBLE_ATTRIBUTES_HH_


#include <set>

#include "../cfg/cfg.hh"
#include "../util/annotate_cycles.hh"
#include "../util/annotate_the_set_first.hh"


namespace Util {


  // Annotates a CFG graph with the ReducibleElementAttribute,
  // which marks an element of the graph as collapsible, which
  // means the element is part of a cycle and may be omitted
  // in the cause of cycle removal.
  class ReducibleAttributeAnnotator {

    private:

      // Holds a pointer to the currently processed grammar,
      // because some deep down sub-routines require information
      // from this structure.
      CFG::CFG* grammar;


    public:

      ReducibleAttributeAnnotator();
      ~ReducibleAttributeAnnotator();

      void annotateGrammar (CFG::CFG* grammar);


    private:

      void annotateProduction (CFG::GrammarProduction* production);
      bool annotateElement (CFG::Base* b);

      std::set<Util::CycleSet*> getCycleMarkSets (CFG::Base* b);
      Util::FirstSet getFirstSet (CFG::NonTerminal* nt);


  };


  // This is a marker atribute, which marks CFG nodes as reducible,
  // which means that the node is part of a cycle, and may possibly
  // collapse (vanish).
  class ReducibleElementAttribute : public Attribute {

    public:

      ReducibleElementAttribute();
      ReducibleElementAttribute (ReducibleElementAttribute& a);
      ~ReducibleElementAttribute();

      virtual Util::Attribute* clone();


  };


}


#endif  // ifndef SRC_UTIL_ANNOTATE_REDUCIBLE_ATTRIBUTES_HH_
