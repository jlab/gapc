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

#include "char_visitor.hh"

#include "alt.hh"
#include "fn_arg.hh"
#include "const.hh"
#include "type.hh"


void Char_Visitor::visit_begin(Alt::Simple &a)
{
  if (a.is_terminal()) {
    if (a.args.size() == 1 && *a.name == "CHAR")
      active = true;
  }
}

void Char_Visitor::visit_end(Alt::Simple &a)
{
  active = false;
}

void Char_Visitor::visit(Fn_Arg::Const &f)
{
  if (!active)
    return;
  Type::Base *type = f.expr().data_type();
  bool found = false;
  for (std::list<Type::Base*>::iterator i = list_.begin(); i!=list_.end(); ++i)
    if ((*i)->is_eq(*type)) {
      found = true;
      break;
    }
  if (!found)
    list_.push_back(type);
}
