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

#include "subopt.hh"

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

#include "codegen.hh"


#include "printer.hh"

void Subopt::gen_instance(Algebra *score) {
    gen_instance(score, Product::NONE);
}

void Subopt::gen_instance(Algebra *score, Product::Sort_Type sort)
{
  score_algebra = score;
  Instance *i = new Instance(score, algebra);
  if(sort != Product::NONE) {
    i->product->set_sorted_choice(sort);
  }
  i->product->init_fn_suffix("_bt");
  Product::Two *t = dynamic_cast<Product::Two*>(i->product);
  assert(t);
  t->left()->init_fn_suffix("");
  instance = i;
}

void Subopt::gen_instance(Algebra *score, Product::Base *base, Product::Sort_Type sort) {
     
  gen_instance(score,  sort);  
    
  instance->product->set_sort_product((new Instance(base, algebra))->product);
    
}

void Subopt::adjust_list_types(Fn_Def *fn, Fn_Def *fn_type)
{
  ::Type::List::Push_Type push_type = ::Type::List::NORMAL;
  switch (fn_type->choice_fn_type()) {
    case Expr::Fn_Call::MINIMUM :
      push_type = ::Type::List::MIN_SUBOPT;
      break;
    case Expr::Fn_Call::MAXIMUM:
      push_type = ::Type::List::MAX_SUBOPT;
      break;
    default:
      assert(false);
      break;
  }
  for (Statement::iterator i = Statement::begin(fn->stmts);
       i != Statement::end(); ++i) {
    Statement::Base *s = *i;
    if (s->is(Statement::VAR_DECL)) {
      Statement::Var_Decl *decl = s->var_decl();
      if (*decl->name == "answers") {
        assert(decl->type->is(::Type::LIST));
        ::Type::List *l =
          new Type::List(*dynamic_cast< ::Type::List*>(decl->type));
        l->set_push_type(push_type);
        decl->type = l;
      } else {
        if (decl->type->is(::Type::LIST)) {
          ::Type::List *l =
            new ::Type::List(*dynamic_cast< ::Type::List*>(decl->type));
          l->set_push_type(::Type::List::NORMAL);
        }
      }
    }
  }
}

void Subopt::add_subopt_fn_args(Fn_Def *fn)
{
  for (Statement::iterator i = Statement::begin(fn->stmts);
       i != Statement::end(); ++i) {
    Statement::Base *s = *i;
    if (s->is(Statement::FN_CALL)) {
      Statement::Fn_Call *f = dynamic_cast<Statement::Fn_Call*>(s);
      if (f->builtin == Statement::Fn_Call::PUSH_BACK
          || f->builtin == Statement::Fn_Call::APPEND) {
        if (f->args.size() == 2) {
          Statement::Var_Decl *v = f->args.front()->var_decl();
          if (v && v->type->is(::Type::LIST)) {
            ::Type::List *l = dynamic_cast< ::Type::List*>(v->type);
            if (l->push_type() == ::Type::List::MAX_SUBOPT
                || l->push_type() == ::Type::List::MIN_SUBOPT) {
              f->args.push_back(new Expr::Vacc(new std::string("score")));
              f->args.push_back(new Expr::Vacc(new std::string("delta")));
            }
          }
        }
      }
    }
  }
}

void Subopt::gen_backtrack(AST &ast)
{
  bool r = ast.check_instances(instance);
  assert(r);
  r = ast.insert_instance(instance);
  assert(r);
  remove_unused();

  //ast.instance_grammar_eliminate_lists(instance);
  Product::Two *t = dynamic_cast<Product::Two*>(instance->product);
  assert(t);
  t->right()->eliminate_lists();
  ast.grammar()->eliminate_lists();

  ast.grammar()->init_list_sizes();
  //ast.warn_missing_choice_fns(instance);
  ast.grammar()->init_indices();
  ast.grammar()->init_decls("sub_");

  ast.set_code_mode(Code::Mode(Code::Mode::UNGER, Code::Mode::SUBOPT));
  ast.codegen();

  const std::list<Symbol::NT*> l = ast.grammar()->nts();
  for (std::list<Symbol::NT*>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    Fn_Def *fn = (*i)->code();
    fn->set_target_name("bt_" + *fn->name);
    if ((*i)->eval_fn) {
      assert(score_algebra);
      hashtable<std::string, Fn_Def*>::iterator j =
        score_algebra->choice_fns.find(*(*i)->eval_fn);
      assert(j != score_algebra->choice_fns.end());
      adjust_list_types(fn, j->second);
      add_subopt_fn_args(fn);
    }
  }

}

void Subopt::gen_instance_code(AST &ast)
{
  instance->product->right_most()->codegen();
  //ast.optimize_choice(*instance);

  instance->product->algebra()
    ->codegen(*dynamic_cast<Product::Two*>(instance->product));
  for (hashtable<std::string, Fn_Def*>::iterator i =
       instance->product->algebra()->choice_fns.begin();
       i != instance->product->algebra()->choice_fns.end();
       ++i) {
    instance->product->algebra()->fns.erase(i->first);
    instance->product->algebra()->choice_fns.erase(i);
  }
}




void Subopt::print_header(Printer::Base &pp, AST &ast)
{
  pp.print_zero_decls(*ast.grammar());

  for (std::list<Symbol::NT*>::const_iterator i = ast.grammar()->nts().begin();
       i != ast.grammar()->nts().end(); ++i)
    pp << *(*i)->table_decl << endl;
  pp << endl;
  for (std::list<Statement::Backtrace_Decl*>::iterator i = bt_decls.begin();
       i != bt_decls.end(); ++i)
    pp << **i;
  for (std::list<Statement::Backtrace_NT_Decl*>::iterator i =
       bt_nt_decls.begin(); i != bt_nt_decls.end(); ++i)
    pp << **i;

  pp.begin_fwd_decls();
  ast.print_code(pp);

  instance->product->right_most()->print_code(pp);
  instance->product->algebra()->print_code(pp);
  pp.end_fwd_decls();
}

void Subopt::print_body(Printer::Base &pp, AST &ast)
{
  ast.print_code(pp);

  instance->product->right_most()->print_code(pp);
  instance->product->algebra()->print_code(pp);
}

