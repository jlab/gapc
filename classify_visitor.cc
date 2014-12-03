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

#include "classify_visitor.hh"


#include "symbol.hh"
#include "statement.hh"
#include "statement/fn_call.hh"
#include "fn_def.hh"

#include <iostream>


void Classify_Visitor::visit(Symbol::NT &n)
{
  if (!n.eval_decl) {
    if (!n.return_decl().type->simple()->is(Type::LIST))
      return;

    Fn_Def *fn = n.code_list().back();
    std::list<Statement::Base*> &l = fn->stmts;
    for (std::list<Statement::Base*>::iterator i = l.begin(); i != l.end(); ++i) {
      if ((*i)->is(Statement::RETURN)) {
        Statement::Fn_Call *finalize = new Statement::Fn_Call(
            Statement::Fn_Call::FINALIZE);
        finalize->add_arg(new Expr::Vacc(n.return_decl()));
        l.insert(i, finalize);
      }
      if ((*i)->is(Statement::FN_CALL)) {
        Statement::Fn_Call *f = dynamic_cast<Statement::Fn_Call*>(*i);
        if (f->builtin == Statement::Fn_Call::TABULATE) {
          Statement::Fn_Call *finalize = new Statement::Fn_Call(
              Statement::Fn_Call::FINALIZE);
          finalize->add_arg(new Expr::Vacc(n.return_decl()));
          l.insert(i, finalize);
          break;
        }
      }
    }
    return;
  }

  std::list<Fn_Def*> &list = n.code_list();
  for (std::list<Fn_Def*>::iterator a = list.begin(); a != list.end(); ++a) {
    //Fn_Def *fn = dynamic_cast<Fn_Def*>(n.code());
    Fn_Def *fn = *a;

    for (Statement::iterator i = Statement::begin(*fn); i != Statement::end();
        ++i) {
      Statement::Base *x = *i;
      if (x->is(Statement::VAR_DECL)) {
        Statement::Var_Decl *v = dynamic_cast<Statement::Var_Decl*>(x);
        if (*v->name == "eval") {
          v->disable();


          Statement::Fn_Call *filter = new Statement::Fn_Call(
              Statement::Fn_Call::HASH_FILTER);
          filter->add_arg(new Expr::Vacc(n.return_decl()));
          *i = filter;

          ++i;
          // (*i)->disable();
          Statement::Fn_Call *finalize = new Statement::Fn_Call(
              Statement::Fn_Call::FINALIZE);
          finalize->add_arg(new Expr::Vacc(n.return_decl()));
          *i = finalize;

          ++i;
          x = *i;
          if (x->is(Statement::FN_CALL)) {
            Statement::Fn_Call *f = dynamic_cast<Statement::Fn_Call*>(x);
            if (f->builtin == Statement::Fn_Call::TABULATE) {
              std::list<Expr::Base*>::reverse_iterator j = f->args.rbegin();
              *j = new Expr::Vacc(n.return_decl());
            }
          } else if (x->is(Statement::RETURN)) {
            Statement::Return *r = dynamic_cast<Statement::Return*>(x);
            r->expr = new Expr::Vacc(n.return_decl());
          }
        }
      }
    }

  }
}


