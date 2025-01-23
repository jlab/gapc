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

#include <string>
#include <list>
#include "table_decl.hh"

#include "../printer.hh"

#include "../type.hh"

namespace Statement {

Table_Decl::Table_Decl(
Symbol::NT &nt,
::Type::Base *t, std::string *n,
bool c,
Fn_Def *fn_is_tab,
Fn_Def *fn_tab,
Fn_Def *fn_get_tab,
Fn_Def *fn_size,
const std::list<Statement::Var_Decl*> &ns)
: Base(TABLE_DECL),
nt_(nt),
type_(t),
pos_type_(0),
name_(n), cyk_(c),
fn_is_tab_(fn_is_tab),
fn_untab_(0),
fn_tab_(fn_tab),
fn_get_tab_(fn_get_tab),
fn_size_(fn_size),
ns_(ns) {
  // FIXME?
  pos_type_ = new ::Type::Size();
}

void Table_Decl::print(Printer::Base &p) const {
  p.print(*this);
}


}  // namespace Statement
