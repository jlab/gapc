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
#include "backtrack_base.hh"
#include "type.hh"
#include "product.hh"
#include "statement.hh"
#include "statement/backtrace_decl.hh"
#include "fn_def.hh"
#include "signature.hh"
#include "symbol.hh"

#include "instance.hh"


Backtrack_Base::Backtrack_Base()
  : score_algebra(0), algebra(0), instance(0), value_type(0), pos_type(0) {
  pos_type = new Type::Size();
}

void Backtrack_Base::remove_unused() {
  assert(instance);
  Algebra *a = instance->product->algebra();
  for (std::list<Statement::Backtrace_Decl*>::iterator i = bt_decls.begin();
       i != bt_decls.end(); ) {
    Statement::Backtrace_Decl *x = *i;
    Fn_Decl *d = a->decl(x->original_name);
    assert(d);
    if (!d->in_use())
      i = bt_decls.erase(i);
    else
      ++i;
  }
}

void Backtrack_Base::gen_backtraces(Product::Base *bt_product,
    const Algebra &score) {
  bt_product->init_fn_suffix("");
  bt_product->codegen();

  Algebra &algebra = *bt_product->algebra();
  value_type = algebra.answer_type();
  for (hashtable<std::string, Fn_Def*>::iterator i = algebra.fns.begin();
      i != algebra.fns.end(); ++i) {
    Fn_Def *fn = i->second;
    if (fn->is_Choice_Fn())
      continue;

    assert(algebra.signature);
    hashtable<std::string, Fn_Decl*>::iterator j =
      algebra.signature->decls.find(*fn->name);
    assert(j != algebra.signature->decls.end());
    Fn_Decl *decl = j->second;

    Statement::Backtrace_Decl *bt_decl =
      new Statement::Backtrace_Decl(*decl, *fn);

    hashtable<std::string, Fn_Def*>::const_iterator k = score.fns.find(
      *fn->name);
    assert(k != score.fns.end());
    bt_decl->set_score_type(k->second->return_type);

    std::list<Fn_Def*> l;
    bt_product->collect_fns(l, *fn->name);
    l.erase(l.begin());
    bt_decl->add_fns(l);

    bt_decl->codegen();
    bt_decls.push_back(bt_decl);
  }
}


void Backtrack_Base::gen_nt_decls(const std::list<Symbol::NT*> &nts) {
  for (std::list<Symbol::NT*>::const_iterator i = nts.begin();
       i != nts.end(); ++i) {
    bt_nt_decls.push_back(new Statement::Backtrace_NT_Decl(*(*i)));
  }
}


void Backtrack_Base::gen_algebra(Signature &signature, Type::Base *alph) {
  Algebra *a = signature.generate_backtrace
    (new std::string("bt_algebra"), value_type, pos_type, alph, Algebra::NONE);
  algebra = a;
}
