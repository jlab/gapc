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

#include "kbacktrack.hh"

#include "algebra.hh"
#include "instance.hh"
#include "product.hh"
#include "statement/backtrace_decl.hh"
#include "statement.hh"
#include "statement/fn_call.hh"
#include "var_acc.hh"
#include "fn_def.hh"
#include "signature.hh"
#include "symbol.hh"
#include "ast.hh"
#include "expr/new.hh"

#include "type/backtrace.hh"


#include "printer.hh"


void KBacktrack::gen_instance(Algebra *score) {
  gen_instance(score, Product::NONE);
}

void KBacktrack::gen_instance(Algebra *score, Product::Sort_Type sort) {
  Instance *i = new Instance(score, algebra);
  if (sort != Product::NONE) {
    i->product->set_sorted_choice(sort);
  }
  i->product = i->product->optimize_shuffle_products();
  i->product->init_fn_suffix("_bt");
  Product::Two *t = dynamic_cast<Product::Two*>(i->product);
  assert(t);
  t->left()->init_fn_suffix("");
  instance = i;
}

void KBacktrack::gen_instance(Algebra *score, Product::Base *base,
  Product::Sort_Type sort) {
  gen_instance(score,  sort);

  instance->product->set_sort_product((new Instance(base, algebra))->product);
}

void KBacktrack::apply_filter(Filter *f) {
  assert(instance);
  instance->product->set_filter(f);
}


void KBacktrack::gen_backtrack(AST &ast) {
  hashtable<std::string, Type::Base*> score_return_types;
  const std::list<Symbol::NT*> &nts = ast.grammar()->nts();
  for (std::list<Symbol::NT*>::const_iterator i = nts.begin();
       i != nts.end(); ++i) {
    Fn_Def *fn = (*i)->code();
    assert(fn);
    assert(fn->name);
    score_return_types[*fn->name] = fn->return_type;
  }

  Code::Mode m = ast.code_mode();
  m.set_kscoring(true);
  ast.set_code_mode(m);
  [[maybe_unused]] bool r = ast.check_instances(instance);
  assert(r);
  r = ast.insert_instance(instance);
  assert(r);
  remove_unused();

  // ast.instance_grammar_eliminate_lists(instance);
  Product::Two *t = dynamic_cast<Product::Two*>(instance->product);
  assert(t);
  t->right()->eliminate_lists();
  ast.grammar()->eliminate_lists();

  ast.grammar()->init_list_sizes();
  // ast.warn_missing_choice_fns(instance);
  ast.grammar()->init_indices();
  ast.grammar()->init_decls();

  ast.set_backtrace();
  ast.codegen();

  // FIXME
  Type::Backtrace *bt_type = new Type::Backtrace(pos_type, value_type);
  for (std::list<Symbol::NT*>::const_iterator i = nts.begin();
       i != nts.end(); ++i) {
    Fn_Def *fn = (*i)->code();

    hashtable<std::string, Type::Base*>::iterator j =
      score_return_types.find(*fn->name);
    assert(j != score_return_types.end());

    gen_nt_proxy_fn(fn, j->second);

    for (std::list<Statement::Base*>::reverse_iterator j = fn->stmts.rbegin();
         j != fn->stmts.rend(); ++j) {
      Statement::Base *s = *j;
      if (s->is(Statement::VAR_DECL)) {
        Statement::Var_Decl *v = dynamic_cast<Statement::Var_Decl*>(s);
        assert(v->type->is(Type::BACKTRACE_LIST));
        v->type = bt_type;
        break;
      }
    }
    fn->return_type = bt_type;
    fn->set_target_name("bt_" + *fn->name);
  }
}

void KBacktrack::gen_instance_code(AST &ast) {
  instance->product->right_most()->codegen();

  instance->product->algebra()
    ->codegen(*dynamic_cast<Product::Two*>(instance->product));
  ast.optimize_choice(*instance);
  instance->product->install_choice_filter();

  instance->product->algebra()->add_choice_specialisations(
    *dynamic_cast<Product::Two*>(instance->product));
}


void KBacktrack::gen_nt_proxy_fn(Fn_Def *fn, Type::Base *score_return_type) {
  Type::Base *t = fn->return_type->deref();
  Type::Base *u = score_return_type->deref();
  bool is_list = t->is(Type::LIST);

  if (!is_list && !t->is(Type::TUPLE)) {
    fn->disable();
    Fn_Def *f = fn->copy_head(t, new std::string("bt_proxy_" + *fn->name));
    Expr::Fn_Call *x = new Expr::Fn_Call(fn->name);
    x->add_arg(f->names.front());
    x->add_arg(f->names.back());
    f->stmts.push_back(new Statement::Return(x));
    proxy_fns.push_back(f);
    return;
  }

  Fn_Def *f = fn->copy_head(t, new std::string("bt_proxy_" + *fn->name));

  Statement::Var_Decl *list = new Statement::Var_Decl(t, "l");
  f->stmts.push_back(list);

  Statement::Var_Decl *ret  = new Statement::Var_Decl(u, "ret");
  Statement::Var_Decl *elem = 0;
  Type::Base *bt_type = 0;
  if (is_list) {
    Type::Base *left = t->component()->left();
    elem = new Statement::Var_Decl(left, "elem");
    bt_type = t->component()->right();
  } else {
    bt_type = t->right();
    elem = new Statement::Var_Decl(t->component()->left(), "elem");
  }
  assert(bt_type->is(Type::BACKTRACE));
  Statement::Var_Decl *bt_decl = new Statement::Var_Decl(bt_type,
    "bt");
  f->stmts.push_back(ret);

  ret->rhs = new Expr::Fn_Call(*fn);

  Statement::Return *r = new Statement::Return(*list);
  Expr::Fn_Call *isEmpty =
    new Expr::Fn_Call(Expr::Fn_Call::IS_EMPTY);
  isEmpty->add_arg(*ret);
  Statement::If *if_empty = new Statement::If(isEmpty);
  if (!is_list) {
    Statement::Fn_Call *empty =
      new Statement::Fn_Call(Statement::Fn_Call::EMPTY);
    empty->add_arg(*list);
    if_empty->then.push_back(empty);
  }
  if_empty->then.push_back(r);
  f->stmts.push_back(if_empty);

  Type::Backtrace *back_end =
    new Type::Backtrace(fn->name, pos_type, value_type,
        t->component()->left());
  Expr::New *bt = new Expr::New(back_end);

  Expr::New *bt_frontend = new Expr::New(new Type::Backtrace(back_end));

  Type::Backtrace *back_end_ptr = back_end->clone();
  back_end_ptr->set_body_context();
  bt_decl->type = back_end_ptr;

  bt->add_arg(new Expr::This());
  bt->add_args(f->names.begin(), f->names.end());
  bt_decl->rhs = bt;
  f->stmts.push_back(bt_decl);

  bt_frontend->add_arg(new Expr::Vacc(*bt_decl));

  std::list<Statement::Base*> *body = 0;
  if (u->is(Type::LIST)) {
    Statement::Foreach *foreach = new Statement::Foreach(elem, ret);
    f->stmts.push_back(foreach);
    body = foreach->stmts();
  } else {
    body = &f->stmts;
  }

  Statement::Var_Decl *tupel = 0;
  if (is_list) {
    tupel = new Statement::Var_Decl(t->component(), "tupel");
    body->push_back(tupel);
  } else {
    tupel = list;
  }
  Statement::Var_Assign *ass1 = 0;
  if (u->is(Type::LIST))
    ass1 = new Statement::Var_Assign(new Var_Acc::Comp(*tupel, 0), *elem);
  else
    ass1 = new Statement::Var_Assign(new Var_Acc::Comp(*tupel, 0), *ret);
  body->push_back(ass1);
  Statement::Var_Assign *ass2 = new Statement::Var_Assign(
      new Var_Acc::Comp(*tupel, 1), bt_frontend);
  body->push_back(ass2);

  Statement::Fn_Call *set_value = new Statement::Fn_Call(
      Statement::Fn_Call::SET_VALUE);
  set_value->add_arg(*tupel);
  body->push_back(set_value);

  if (is_list) {
    Statement::Fn_Call *push_back = new Statement::Fn_Call(
        Statement::Fn_Call::PUSH_BACK);
    push_back->add_arg(*list);
    push_back->add_arg(*tupel);
    body->push_back(push_back);
  }

  f->stmts.push_back(r);

  proxy_fns.push_back(f);
}


void KBacktrack::print_header(Printer::Base &pp, AST &ast) {
  for (std::list<Statement::Backtrace_Decl*>::iterator i = bt_decls.begin();
       i != bt_decls.end(); ++i)
    pp << **i;
  for (std::list<Statement::Backtrace_NT_Decl*>::iterator i =
       bt_nt_decls.begin(); i != bt_nt_decls.end(); ++i)
    pp << **i;

  pp.begin_fwd_decls();
  for (std::list<Fn_Def*>::iterator i = proxy_fns.begin();
       i != proxy_fns.end(); ++i)
    pp << **i;
  ast.print_code(pp);

  instance->product->right_most()->print_code(pp);
  instance->product->algebra()->print_code(pp);
  pp.end_fwd_decls();
}

void KBacktrack::print_body(Printer::Base &pp, AST &ast) {
  for (std::list<Fn_Def*>::iterator i = proxy_fns.begin();
       i != proxy_fns.end(); ++i)
    pp << **i;
  ast.print_code(pp);

  instance->product->right_most()->print_code(pp);
  instance->product->algebra()->print_code(pp);
}

void KBacktrack::gen_nt_decls(const std::list<Symbol::NT*> &nts) {
  for (std::list<Symbol::NT*>::const_iterator i = nts.begin();
       i != nts.end(); ++i) {
    Type::Base *t = (*i)->data_type();
    bt_nt_decls.push_back(new Statement::Backtrace_NT_Decl(*(*i), t));
  }

  for (std::list<Statement::Backtrace_Decl*>::iterator i = bt_decls.begin();
       i != bt_decls.end(); ++i)
    (*i)->set_derive_bt_score();
}
