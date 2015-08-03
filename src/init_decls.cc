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


#include "init_decls.hh"

#include "alt.hh"
#include "fn_arg.hh"

#include "log.hh"

#include "statement.hh"

#include "symbol.hh"

#include <iostream>

void Init_Decls::visit(Symbol::NT &s)
{
  ret = 0;
  arg = 0;
  if (Log::instance()->is_debug())
    std::cerr << std::endl << *s.name << std::endl;
}

void Init_Decls::visit(Alt::Base &a)
{
  a.init_ret_decl(ret, prefix);
  if (a.ret_decl) {
    if (Log::instance()->is_debug())
      std::cerr << *a.ret_decl << std::endl;
    ++ret;
  }
}

void Init_Decls::visit(Fn_Arg::Base &f)
{
  f.init_ret_decl(arg, prefix);
  if (Log::instance()->is_debug()) {
    for (std::vector<Statement::Var_Decl*>::const_iterator i = f.ret_decls().begin();
        i != f.ret_decls().end(); ++i)
      if (*i)
        std::cerr << **i << ' ';
    std::cerr << '\n';
    for (std::vector<Statement::Var_Decl*>::const_iterator i = f.var_decls().begin();
        i != f.var_decls().end(); ++i)
      if (*i)
        std::cerr << **i << ' ';
    std::cerr << '\n';
  }
  ++arg;
}

