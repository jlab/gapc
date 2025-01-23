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

#include <vector>
#include <list>
#include <string>
#include "fn_call.hh"

#include "../printer.hh"
#include "../expr.hh"
#include "../statement/table_decl.hh"
#include "../table.hh"
#include "../symbol.hh"


void Statement::Fn_Call::print(Printer::Base &p) const {
  p.print(*this);
}


const char * Statement::Fn_Call::map_builtin_to_string[] = {
  "NONE",
  "push_back",
  "erase",
  "clear",
  "set",  // "tabulate",
  "empty",
  "append",
  "append",
  "pareto_yukish",
  "assert",
  "set_value",
  "hash_filter",
  "append_filter",
  "update_filter",
  "mark",
  "finalize",
  "INNER",
  "mark_position",
  "join_marked",
  "pareto_domination_sort"
};


Statement::Fn_Call::Fn_Call(
  std::string *n, std::list<Expr::Base*> *l, const Loc &loc)
  : Base(FN_CALL, loc), builtin(NONE), name_(n), args(*l) {
}

std::string Statement::Fn_Call::name() const {
  if (name_)
    return *name_;
  else
    return std::string(map_builtin_to_string[builtin]);
}

void Statement::Fn_Call::add_arg(Expr::Vacc *vdecl) {
  args.push_back(vdecl);
}

void Statement::Fn_Call::add_arg(Var_Decl &vdecl) {
  args.push_back(new Expr::Vacc(vdecl));
}


void Statement::Fn_Call::add_arg(Table_Decl &vdecl) {
  args.push_back(new Expr::Vacc(new std::string(vdecl.name())));
}

void Statement::Fn_Call::add(Table_Decl &v) {
  is_obj = Bool(true);

  add_arg(new std::string(v.name()));

  const std::vector<Table> &tables = v.nt().tables();
  const std::vector<Expr::Base*> &left = v.nt().left_indices;
  const std::vector<Expr::Base*> &right = v.nt().right_indices;

  assert(left.size() == tables.size());
  assert(right.size() == tables.size());

  std::vector<Expr::Base*>::const_iterator j = left.begin();
  std::vector<Expr::Base*>::const_iterator k = right.begin();
  for (std::vector<Table>::const_iterator i = tables.begin(); i != tables.end();
       ++i, ++j, ++k) {
    if (!(*i).delete_left_index())
      add_arg(*j);
    if (!(*i).delete_right_index())
      add_arg(*k);
  }
}

void Statement::Fn_Call::add_arg(Var_Acc::Base *vacc) {
  args.push_back(new Expr::Vacc(vacc));
}

void Statement::Fn_Call::add_arg(std::string *n) {
  args.push_back(new Expr::Vacc(n));
}

void Statement::Fn_Call::replace(Var_Decl &decl, Expr::Base *expr) {
  for (std::list<Expr::Base*>::iterator i = args.begin();
       i != args.end(); ++i)
    if (**i == decl)
      *i = expr;
}

Statement::Base *Statement::Fn_Call::copy() const {
  Fn_Call *o = new Fn_Call(*this);
  o->args.clear();
  for (std::list<Expr::Base*>::const_iterator i = args.begin();
       i != args.end(); ++i)
    o->args.push_back((*i)->copy());
  return o;
}
