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


#include "inline_nts.hh"

#include "fn_arg.hh"
#include "alt.hh"
#include "symbol.hh"
#include "grammar.hh"

Inline_Nts::Inline_Nts(Grammar *g) : grammar(g)
{
}

void Inline_Nts::visit_end(Symbol::NT &n)
{
  n.inline_nts(grammar);
}

void Inline_Nts::visit(Fn_Arg::Alt &f)
{
  if (!f.is(Alt::LINK))
    return;
  Alt::Link *l = dynamic_cast<Alt::Link*>(f.alt);
  if (!l->nt->is(Symbol::NONTERMINAL))
    return;
  Symbol::NT *nt = dynamic_cast<Symbol::NT*>(l->nt);
  if (nt->is_inlineable()) {
    Alt::Link *t = l;
    f.alt = nt->alts.front();
    delete t;
    grammar->remove(nt);
    delete nt;
  }
}

