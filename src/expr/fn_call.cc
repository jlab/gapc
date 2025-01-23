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

#include "fn_call.hh"

#include <iostream>
#include <map>
#include <vector>
#include <string>
#include <list>
#include "../filter.hh"

#include "../fn_decl.hh"
#include "../fn_def.hh"

Expr::Fn_Call::Fn_Call(const Fn_Decl &f)
  : Base(FN_CALL), name(f.name), builtin(NONE), type_param(NULL) {
  const Fn_Def *x = dynamic_cast<const Fn_Def*>(&f);
  if (x) {
    if (x->target_name() != "")
      name = new std::string(x->target_name());
  }
}

Expr::Fn_Call::Fn_Call(const Fn_Def &f)
  : Base(FN_CALL), name(f.name), builtin(NONE), type_param(NULL) {
  for (std::list<std::string*>::const_iterator i = f.names.begin();
       i != f.names.end(); ++i) {
    Expr::Vacc *e = new Expr::Vacc(*i);
    exprs.push_back(e);
  }
}

Expr::Fn_Call::Fn_Call(std::string *n, const Loc &l)
  : Base(FN_CALL, l), name(NULL), builtin(NONE), type_param(NULL) {
  std::map<std::string, Builtin>::iterator i = map_string_to_builtin.find(*n);
  if (i == map_string_to_builtin.end())
    name = n;
  else
    builtin = i->second;
}

Expr::Fn_Call::Fn_Call(const Filter &f)
  : Base(FN_CALL), name(f.name), builtin(NONE), type_param(0) {
  for (std::list<Base*>::const_iterator i = f.args.begin();
       i != f.args.end(); ++i)
    add_arg(*i);
}

#include "../var_acc.hh"
#include "../statement.hh"
#include "../type/multi.hh"

void Expr::Fn_Call::put_arg(std::ostream &s, Expr::Base *e) const {
  if (!e) {
    std::cerr << "Fn contains 0 arg: " << *name << '\n';
    assert(0);
  }

  if (e->is(Expr::VACC)) {
    Expr::Vacc *x = dynamic_cast<Expr::Vacc*>(e);
    if (x->var_acc->is(Var_Acc::PLAIN)) {
      Var_Acc::Plain *v = dynamic_cast<Var_Acc::Plain*>(x->var_acc);
      if (v->vdecl) {
        if (v->vdecl->type->is(::Type::MULTI)
            || (v->vdecl->type->simple()->is(::Type::LIST) &&
                v->vdecl->type->component()->is(::Type::MULTI))) {
          ::Type::Base *tbase = v->vdecl->type;
          if (v->vdecl->type->simple()->is(::Type::LIST))
            tbase = v->vdecl->type->component();

          ::Type::Multi *t = dynamic_cast< ::Type::Multi*>(tbase);
          std::list< ::Type::Base*>::const_iterator i = t->types().begin();
          s << *v->vdecl->name << "_0";
          ++i;
          size_t j = 1;
          for (; i != t->types().end(); ++i, ++j)
            s << ", " << *v->vdecl->name << "_" << j;
          return;
        }
      }
    }
  }

  s << *e;
}

void Expr::Fn_Call::put(std::ostream &s) const {
  std::list<Base*>::const_iterator i = exprs.begin();
  if (is_obj) {
    assert(i != exprs.end());
    s << **i << '.';
    ++i;
  }

  if (name)
    s << *name;
  else
    s << map_builtin_to_string[builtin];
  if (type_param)
    s << '<' << *type_param << '>';

  s << '(';
  if (i != exprs.end()) {
    put_arg(s, *i);
    ++i;
  }
  for (; i != exprs.end(); ++i) {
    s << ", ";
    put_arg(s, *i);
  }
  s << ')';
}

const char * Expr::Fn_Call::map_builtin_to_string[] = {
  "NONE",
  "is_not_empty",
  "splice_left",
  "get_range",
  "is_tabulated",
  "get",  // "get_tabulated",
  "isEmpty",
  "get_front",
  "get_back",
  "erase_element",
  "insert_element",
  "round_to_digit",
  "minimum",
  "maximum",
  "sum",
  "unique",
  "list",
  "evaluate",
  "execute_backtrack",
  "splice_right",
  "execute_backtrack_one",
  "execute_backtrack_k",
  "is_marked",
  "dummy_bt",
  "expsum",
  "exp",
  "log",
  "execute_backtrace_k_one",
  "exp2",
  "log2",
  "exp2sum",
  "bitsum",
  "pow",
  0
};

std::map<std::string, Expr::Fn_Call::Builtin>
  Expr::Fn_Call::map_string_to_builtin;

void Expr::Fn_Call::init_builtins() {
  int i = NOT_EMPTY;
  while (map_builtin_to_string[i]) {
    map_string_to_builtin[map_builtin_to_string[i]] = Builtin(i);
    ++i;
  }
}


void Expr::Fn_Call::add_arg(Statement::Var_Decl &v) {
  Expr::Vacc *e = new Expr::Vacc(v);
  exprs.push_back(e);
}

void Expr::Fn_Call::add(const std::vector<Statement::Var_Decl*> &l) {
  for (std::vector<Statement::Var_Decl*>::const_iterator i = l.begin();
       i != l.end(); ++i)
    add_arg(**i);
}

#include "../statement/table_decl.hh"

void Expr::Fn_Call::add_arg(const Statement::Table_Decl &v) {
  Expr::Vacc *e = new Expr::Vacc(new std::string(v.name()));
  exprs.push_back(e);
}

#include "../symbol.hh"

void Expr::Fn_Call::add(const Statement::Table_Decl &v) {
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

void Expr::Fn_Call::add(const std::vector<Expr::Base*> &l,
    const std::vector<Expr::Base*> &r) {
  assert(l.size() == r.size());
  std::vector<Expr::Base*>::const_iterator j = r.begin();
  for (std::vector<Expr::Base*>::const_iterator i = l.begin(); i != l.end();
      ++i, ++j) {
    add_arg(*i);
    add_arg(*j);
  }
}

void Expr::Fn_Call::add_arg(std::string *n) {
  Expr::Vacc *e = new Expr::Vacc(n);
  exprs.push_back(e);
}

void Expr::Fn_Call::add_arg(Expr::Base *e) {
  assert(e);
  exprs.push_back(e);
}

void Expr::Fn_Call::add_arg(Var_Acc::Base *e) {
  exprs.push_back(new Expr::Vacc(e));
}

Expr::Fn_Call::Fn_Call(std::string *n, std::list<Statement::Var_Decl*> &l)
  : Base(FN_CALL), name(n), builtin(NONE), type_param(0) {
  for (std::list<Statement::Var_Decl*>::iterator i = l.begin(); i != l.end();
       ++i)
    exprs.push_back(new Expr::Vacc(**i));
}

void Expr::Fn_Call::replace(Statement::Var_Decl &decl, Expr::Base *expr) {
  for (std::list<Base*>::iterator i = exprs.begin(); i != exprs.end();
       ++i) {
    if (**i == decl)
      *i = expr;
  }
}

Expr::Fn_Call *Expr::Fn_Call::fn_call() {
  return this;
}

Expr::Fn_Call *Expr::Fn_Call::clone() {
  return new Fn_Call(*this);
}

void Expr::Fn_Call::add(const std::list<Para_Decl::Base*> &l) {
  for (std::list<Para_Decl::Base*>::const_iterator i = l.begin(); i != l.end();
       ++i) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    assert(s);
    add_arg(s->name());
  }
}

Expr::Base *Expr::Fn_Call::copy() const {
  Fn_Call *o = new Fn_Call(*this);
  o->exprs.clear();
  for (std::list<Base*>::const_iterator i = exprs.begin();
       i != exprs.end(); ++i)
    o->exprs.push_back((*i)->copy());
  return o;
}
