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


#ifndef OPT_CHOICE_VISITOR_HH
#define OPT_CHOICE_VISITOR_HH

#include "visitor.hh"

#include "type.hh"

class Fn_Def;

#include "symbol_fwd.hh"

class Opt_Choice_Visitor : public Visitor {
  private:
    Fn_Def *choice_fn;
    bool in_choice_fn;
    Type::List::Push_Type push_type;
    Statement::Hash_Decl *hash_decl;

  public:
    Opt_Choice_Visitor(Fn_Def *f, Type::List::Push_Type t)
      : choice_fn(f), in_choice_fn(false), push_type(t), hash_decl(0) {}
    Opt_Choice_Visitor(Fn_Def *f, Type::List::Push_Type t,
        Statement::Hash_Decl *h)
      : choice_fn(f), in_choice_fn(false), push_type(t), hash_decl(h) {}

    void visit(Alt::Base &b);
    void visit(Symbol::NT &n);
    void visit_end(Symbol::NT &n);

};

#endif
