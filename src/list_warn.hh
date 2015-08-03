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


#ifndef LIST_WARN_HH
#define LIST_WARN_HH

#include "visitor.hh"

class Instance;
class Fn_Def;

class List_Warn : public Visitor {
  private:
    Instance *instance;
    bool skip;

  public:
    List_Warn() : instance(0), skip(false) {}
    List_Warn(Instance *i) : instance(i), skip(false) {}
    void visit(Symbol::NT &n);
    void visit_begin(Alt::Block &a);

};

#endif
