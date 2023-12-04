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

#ifndef SRC_UTIL_ANNOTATE_DEAD_ENDS_HH_
#define SRC_UTIL_ANNOTATE_DEAD_ENDS_HH_

#include <string>
#include <set>

#include "../cfg/cfg.hh"
#include "attribute.hh"


namespace Util {


// Annotates each CFG node with a DeadEndAttribute if the node
// is a dead end in the grammar graph, which means it does not
// participate in any cycle.
// This algorithm needs the set FIRST, and assumes that the
// corresponding attribute has already been annotated.
class AnnotateDeadEnds {
 private:
    // The grammar which this algorithm traverses. This
    // pointer is set when 'annotateGrammar' is called.
    CFG::CFG* grammar;

 public:
    AnnotateDeadEnds();
    ~AnnotateDeadEnds();

    void annotateGrammar(CFG::CFG* grammar);

 private:
    bool annotateProduction(
      CFG::NonTerminal* nonTerminal,
      std::set<std::string>* visitedNonTerminals);
    bool annotateBase(CFG::Base* b, std::set<std::string>* visitedNonTerminals);

    // Returns TRUE if the CFG node can be the empty word (epsilon).
    bool elementIsNullable(CFG::Base* b);
};


// This is a marker attribute, which marks a CFG node
// as a dead end, which means it is not involved in
// any cycle whatsoever. Productions marked by this
// attribute may be treated in the cycle reduction
// algorithm as "constants" which can be accessed
// by their original name, and may be simply cloned.
class DeadEndAttribute : public Util::Attribute {
 public:
    DeadEndAttribute();
    DeadEndAttribute(DeadEndAttribute& a);
    ~DeadEndAttribute();

    virtual Attribute* clone();
};

}  // namespace Util


#endif  // SRC_UTIL_ANNOTATE_DEAD_ENDS_HH_
