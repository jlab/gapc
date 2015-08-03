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

#include "opt_choice_visitor.hh"
#include "symbol.hh"

#include "fn_def.hh"

void Opt_Choice_Visitor::visit(Alt::Base &b)
{
  //return;
  if (in_choice_fn)
    if (hash_decl)
      b.optimize_choice(push_type, hash_decl);
    else
      b.optimize_choice(push_type);
}

void Opt_Choice_Visitor::visit(Symbol::NT &n)
{
  if (n.eval_fn && *n.eval_fn == *choice_fn->name) {
    // Product::optimize_shuffle_products
    // is able to invalidate this assertion
    //Fn_Def *f = dynamic_cast<Fn_Def*>(n.eval_decl);
    //Fn_Decl *fn = dynamic_cast<Fn_Decl*>(choice_fn);
    //assert(fn == n.eval_decl);
  
    in_choice_fn = true;
    if (hash_decl)
      n.optimize_choice(push_type, hash_decl);
    else
      n.optimize_choice(push_type);
  }
}

void Opt_Choice_Visitor::visit_end(Symbol::NT &n)
{
  in_choice_fn = false;
}

