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


#include "list_warn.hh"

#include "symbol.hh"
#include "alt.hh"
#include "yieldsize.hh"
#include "log.hh"

#include "instance.hh"
#include "product.hh"
#include "algebra.hh"

#include "fn_def.hh"

void List_Warn::visit(Symbol::NT &n)
{
  if (skip)
    return;
  if (instance) {
    bool x = true;
    for (hashtable<std::string, Fn_Def*>::iterator i
        = instance->product->algebra()->choice_fns.begin();
        i != instance->product->algebra()->choice_fns.end(); ++i) {
      Fn_Def *f = i->second;
      x = x && (f->choice_mode() == Mode::PRETTY);
    }
    if (x) {
      skip = true;
      return;
    }
  }

  if (!n.eval_decl && n.list_size() == Yield::UP)
    Log::instance()->warning(n.location, "Nonterminal " + *n.name
        + " has an answer list >= n but no choice function is applied.");
}

void List_Warn::visit_begin(Alt::Block &a)
{
  if (skip)
    return;
  if (a.alts.size() > 1 && a.list_size() == Yield::UP) {
    Log::instance()->warning(a.location, "Block has an answer list >= n, "
        "consider moving this block into an extra NT and"
        "applying choice function to it.");
  }
}
