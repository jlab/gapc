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
#include "backtrack.hh"

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


void Backtrack::gen_instance(Algebra *score) {
     gen_instance(score, Product::NONE);
}

void Backtrack::gen_instance(Algebra *score, Product::Sort_Type sort) {
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

void Backtrack::gen_instance(
  Algebra *score, Product::Base *base, Product::Sort_Type sort) {
  gen_instance(score,  sort);

  instance->product->set_sort_product((new Instance(base, algebra))->product);
}

void Backtrack::apply_filter(Filter *f) {
  assert(instance);
  instance->product->set_filter(f);
}


void Backtrack::gen_backtrack(AST &ast) {
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

  const std::list<Symbol::NT*> l = ast.grammar()->nts();
  // Type::Backtrace_List *bt_type = new Type::Backtrace_List();
  Type::Backtrace *bt_type = new Type::Backtrace(pos_type, value_type);
  for (std::list<Symbol::NT*>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    Fn_Def *fn = (*i)->code();

    gen_nt_proxy_fn(fn);

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


void Backtrack::gen_instance_code(AST &ast) {
  instance->product->right_most()->codegen();

  instance->product->algebra()
    ->codegen(*dynamic_cast<Product::Two*>(instance->product));
  ast.optimize_choice(*instance);
  instance->product->install_choice_filter();

  instance->product->algebra()->add_choice_specialisations(
    *dynamic_cast<Product::Two*>(instance->product));
}


void Backtrack::gen_nt_proxy_fn(Fn_Def *fn) {
  Type::Base *t = fn->return_type->deref();
  bool is_list = t->is(Type::LIST);

  if (!is_list && !t->is(Type::TUPLE)) {
    fn->disable();
    Fn_Def *f = fn->copy_head(t, new std::string(
      "bt_proxy_" + *fn->name));
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

  Statement::Var_Decl *ret = 0;
  if (is_list) {
    Type::List *l = dynamic_cast<Type::List*>(t);
    assert(l->of->is(Type::TUPLE));
    Type::Tuple *tuple = dynamic_cast<Type::Tuple*>(l->of);
    ret = new Statement::Var_Decl(tuple, "ret");
  } else {
    ret = new Statement::Var_Decl(t, "ret");
  }
  f->stmts.push_back(ret);

  Expr::Fn_Call *nt_fn = new Expr::Fn_Call(*fn);
  Statement::Var_Assign *score =
    new Statement::Var_Assign(new Var_Acc::Comp(*ret, 0), nt_fn);
  f->stmts.push_back(score);

  Expr::New *bt =
    new Expr::New(new Type::Backtrace(fn->name, pos_type, value_type));
  bt->add_arg(new Expr::This());
  bt->add_args(f->names.begin(), f->names.end());
  Statement::Var_Assign *track =
    new Statement::Var_Assign(new Var_Acc::Comp(*ret, 1), bt);

  Expr::Fn_Call *empty = new Expr::Fn_Call(Expr::Fn_Call::IS_EMPTY);
  empty->add_arg(new Var_Acc::Comp(*ret, 0));
  Statement::Fn_Call *set_empty = new Statement::Fn_Call(
    Statement::Fn_Call::EMPTY);
  set_empty->add_arg(new Var_Acc::Comp(*ret, 1));
  Statement::If *if_empty = new Statement::If(empty, set_empty, track);

  f->stmts.push_back(if_empty);

  if (is_list) {
    Statement::Fn_Call *push =
      new Statement::Fn_Call(Statement::Fn_Call::PUSH_BACK);
    push->add_arg(*list);
    push->add_arg(*ret);
    if_empty->els.push_back(push);

    f->stmts.push_back(new Statement::Return(*list));
  } else {
    f->stmts.push_back(new Statement::Return(*ret));
  }

  proxy_fns.push_back(f);
}


void Backtrack::print_header(Printer::Base &pp, AST &ast) {
  for (std::list<Statement::Backtrace_Decl*>::iterator i =
       bt_decls.begin();
       i != bt_decls.end(); ++i) {
    pp << **i;
  }
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

void Backtrack::print_body(Printer::Base &pp, AST &ast) {
  for (std::list<Fn_Def*>::iterator i = proxy_fns.begin();
       i != proxy_fns.end(); ++i)
    pp << **i;
  ast.print_code(pp);

  instance->product->right_most()->print_code(pp);
  instance->product->algebra()->print_code(pp);
}
