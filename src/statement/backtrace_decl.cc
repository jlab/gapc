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
#include "backtrace_decl.hh"

#include "../printer.hh"
#include "../fn_def.hh"

#include "../type/backtrace.hh"
#include "../statement.hh"

const std::list<Para_Decl::Base*> &Statement::Backtrace_Decl::ntparas() const {
  return fn.ntparas();
}

Statement::Backtrace_Decl::Backtrace_Decl(const Fn_Decl &a, const Fn_Def &b)
  : Base(BACKTRACE_DECL), fn_sig(a), fn(b),
        eval_code_(0),
        algebra_code_(0),
        bt_last(0),
        answer(0),
        elist_type(0),
        original_name(*b.name),
        derive_bt_score_(false),
        score_type_(0) {
}

void Statement::Backtrace_Decl::print(Printer::Base &p) const {
  p.print(*this);
}

void Statement::Backtrace_NT_Decl::print(Printer::Base &p) const {
  p.print(*this);
}

void Statement::Backtrace_Decl::codegen() {
  name_ = "Backtrace_" + *fn.name;
  codegen_init();
  codegen_eval();
  codegen_algebra_fn();
}

#include "../para_decl.hh"

void Statement::Backtrace_Decl::add_arg(
  Para_Decl::Simple *p, const ::Type::Base *i) {
  arg_types.push_back(p->type());

  ::Type::Base *t = 0;
  if (i->const_simple()->is(::Type::SIGNATURE)) {
    // t = new Type::Backtrace_List();
    t = new ::Type::Backtrace();
  } else {
    t = p->type();
  }
  assert(t);
  std::string *n = new std::string("arg_" + *p->name());
  Statement::Var_Decl *v = new Statement::Var_Decl(t, n);
  args.push_back(v);
}

#include "../type/multi.hh"

void Statement::Backtrace_Decl::codegen_init() {
  std::list< ::Type::Base*>::const_iterator i = fn_sig.types.begin();
  for (std::list<Para_Decl::Base*>::const_iterator x = fn.paras.begin();
       x != fn.paras.end(); ++x,  ++i) {
    Para_Decl::Simple *p = dynamic_cast<Para_Decl::Simple*>(*x);
    if (p) {
      add_arg(p, *i);
    } else {
      Para_Decl::Multi *p = dynamic_cast<Para_Decl::Multi*>(*x);
      assert(p);
      ::Type::Multi *m = dynamic_cast< ::Type::Multi*>(*i);
      assert(m);
      assert(m->types().size() == p->list().size());
      std::list< ::Type::Base*>::const_iterator b = m->types().begin();
      for (std::list<Para_Decl::Simple*>::const_iterator a = p->list().begin();
          a != p->list().end(); ++a, ++b) {
        add_arg(*a, *b);
      }
    }
  }
}


void Statement::Backtrace_Decl::codegen_eval() {
  assert(stmts.empty());
  eval_outer();
  eval_inner();
  eval_end();
}

#include "../expr/new.hh"

void Statement::Backtrace_Decl::eval_outer() {
  ::Type::Backtrace *bt_type = new ::Type::Backtrace();
  elist_type = new ::Type::Eval_List();
  elist_type->of = fn.return_type;
  answer = new Statement::Var_Decl(elist_type, "answer",
      new Expr::New(elist_type));
  stmts.push_back(answer);
  std::list< ::Type::Base*>::const_iterator j = arg_types.begin();
  for (std::list<Statement::Var_Decl*>::iterator i = args.begin();
       i != args.end(); ++i, ++j) {
    // if (!(*i)->type->is(::Type::BACKTRACE_LIST)) {
    if (!(*i)->type->is(::Type::BACKTRACE)) {
      paras_.push_back(*i);
      eval_paras.push_back(*i);
      continue;
    }
    ::Type::Base *elem_type = *j;
    Statement::Var_Decl *a = *i;
    bt_paras.push_back(a);

    Statement::Var_Decl *bt =
      new Statement::Var_Decl(bt_type, *a->name + "_bt");

    Statement::Foreach *bt_loop = new Statement::Foreach(bt, a);
    bt_loops.push_back(bt_loop);

    Expr::Fn_Call *e =
        new Expr::Fn_Call(Expr::Fn_Call::EVALUATE);
    e->add_arg(*bt);
    Statement::Var_Decl *eval_list =
      new Statement::Var_Decl(elist_type, *a->name + "_elist", e);
    elist_stmts.push_back(eval_list);
    elist_paras.push_back(eval_list);

    Statement::Var_Decl *eval = new Statement::Var_Decl(elem_type,
        *a->name + "_elem");
    // paras_.push_back(eval);
    eval_paras.push_back(eval);
    paras_.push_back(a);

    Statement::Foreach *eval_loop = new Statement::Foreach(eval, eval_list);
    eval_loops.push_back(eval_loop);
  }
  bt_last = 0;
  if (!bt_loops.empty()) {
    stmts.push_back(bt_loops.front());
    bt_last = Statement::nest_for_loops(bt_loops.begin(), bt_loops.end());
    bt_last->statements.insert(bt_last->statements.end(),
        elist_stmts.begin(), elist_stmts.end());
  } else {
    stmts.insert(stmts.end(), elist_stmts.begin(), elist_stmts.end());
  }
}

#include "fn_call.hh"

void Statement::Backtrace_Decl::eval_inner() {
  Expr::Fn_Call *e = new Expr::Fn_Call(fn.name, eval_paras);
  e->add(fn.ntparas());
  Statement::Var_Decl *t = new Statement::Var_Decl(fn.return_type, "ret", e);
  inner_stmts.push_back(t);
  Statement::Fn_Call *push_back =
    new Statement::Fn_Call(Statement::Fn_Call::PUSH_BACK);
  push_back->add_arg(*answer);
  push_back->add_arg(*t);
  inner_stmts.push_back(push_back);

  if (!eval_loops.empty()) {
    bt_last->statements.push_back(eval_loops.front());
    Statement::Foreach *last =
      Statement::nest_for_loops(eval_loops.begin(), eval_loops.end());
    last->statements.insert(last->statements.end(), inner_stmts.begin(),
        inner_stmts.end());

    for (std::list<Statement::Var_Decl*>::iterator i = elist_paras.begin();
         i != elist_paras.end(); ++i) {
      Statement::Fn_Call *erase =
        new Statement::Fn_Call(Statement::Fn_Call::ERASE);
      erase->add_arg(**i);
      bt_last->statements.push_back(erase);
    }

    for (std::list<Statement::Var_Decl*>::iterator i = bt_paras.begin();
         i != bt_paras.end(); ++i) {
      // FIXME in cpp destructor for these args
      Statement::Fn_Call *erase =
        new Statement::Fn_Call(Statement::Fn_Call::ERASE);
      erase->add_arg(**i);
      stmts.push_back(erase);
    }
  } else {
    stmts.insert(stmts.end(), inner_stmts.begin(), inner_stmts.end());
  }
}

void Statement::Backtrace_Decl::eval_end() {
  Statement::Return *ret = new Statement::Return(*answer);
  stmts.push_back(ret);
  eval_code_ = new Fn_Def(elist_type, new std::string("eval"));
  eval_code_->stmts = stmts;
}

void Statement::Backtrace_Decl::codegen_algebra_fn() {
  algebra_code_ = &fn;
}

const ::Type::Base & Statement::Backtrace_Decl::score_type() const {
  return *score_type_;
}

#include "../symbol.hh"
#include "../table.hh"

void Statement::Backtrace_NT_Decl::init(Symbol::NT &n) {
  size_t t = 0;
  for (std::vector<Table>::const_iterator i = n.tables().begin();
      i != n.tables().end(); ++i, ++t) {
    if (!(*i).delete_left_index()) {
      std::ostringstream o;
      o << "t_" << t << "_i";
      track_args_.push_back(o.str());
    }
    if (!(*i).delete_right_index()) {
      std::ostringstream o;
      o << "t_" << t << "_j";
      track_args_.push_back(o.str());
    }
  }
  ntparas_ = n.ntargs();
}

Statement::Backtrace_NT_Decl::Backtrace_NT_Decl(Symbol::NT &n)
  : Base(BACKTRACE_NT_DECL), name_(*n.name), score_type_(0) {
  init(n);
}

Statement::Backtrace_NT_Decl::Backtrace_NT_Decl(Symbol::NT &n, ::Type::Base *s)
  : Base(BACKTRACE_NT_DECL), name_(*n.name), score_type_(s) {
  init(n);
}
