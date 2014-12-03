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

#ifndef STATEMENT_BACKTRACE_DECL_HH
#define STATEMENT_BACKTRACE_DECL_HH


#include "base.hh"
#include "../type_fwd.hh"
#include "../symbol_fwd.hh"
#include "../para_decl_fwd.hh"

class Fn_Decl;
class Fn_Def;

namespace Statement {

  class Backtrace_Decl : public Base {
    private:
      const Fn_Decl &fn_sig;
      const Fn_Def &fn;
      std::list<Fn_Def *> fns;

      std::list<Statement::Var_Decl*> args;
      std::list< ::Type::Base*> arg_types;
      std::string name_;

      Fn_Def *eval_code_;
      const Fn_Def *algebra_code_;



      Statement::Foreach *bt_last;
      Statement::Var_Decl *answer;
      ::Type::Eval_List *elist_type;
      std::list<Statement::Base*> stmts;
      std::list<Statement::Foreach*> bt_loops;
      std::list<Statement::Foreach*> eval_loops;
      std::list<Statement::Base*> inner_stmts;
      std::list<Statement::Base*> elist_stmts;
      std::list<Statement::Var_Decl*> paras_;
      std::list<Statement::Var_Decl*> eval_paras;
      std::list<Statement::Var_Decl*> bt_paras;
      std::list<Statement::Var_Decl*> elist_paras;



      void add_arg(Para_Decl::Simple *p, const ::Type::Base *i);
      void codegen_init();

      void eval_outer();
      void eval_inner();
      void eval_end();
      void codegen_eval();

      void codegen_algebra_fn();
    public:
      std::string original_name;
    private:
      bool derive_bt_score_;
      ::Type::Base *score_type_;
    public:
      Backtrace_Decl(const Fn_Decl &a, const Fn_Def &b);

      void print(Printer::Base &p) const;

      void codegen();

      const Fn_Def & eval_code() const { return *eval_code_; }
      const std::list<Fn_Def*> &algebra_code_deps() const { return fns; }
      const Fn_Def & algebra_code() const { return *algebra_code_; }
      const std::string &name() const { return name_; }

      const std::list<Statement::Var_Decl*> & paras() const { return paras_; }

      void add_fns(const std::list<Fn_Def*> &l)
      {
        fns = l;
      }

      void set_derive_bt_score() { derive_bt_score_ = true; }
      bool derive_bt_score() const { return derive_bt_score_; }
      const ::Type::Base &score_type() const;
      void set_score_type(::Type::Base *t) { score_type_ = t; }

    public:
      const std::list<Para_Decl::Base*> &ntparas() const;
  };

  class Backtrace_NT_Decl : public Base {
    private:
      std::string name_;
      ::Type::Base *score_type_;

      std::list<std::string> track_args_;

      std::list<Para_Decl::Base*> ntparas_;

      void init(Symbol::NT &n);
    public:
      Backtrace_NT_Decl(Symbol::NT &n);
      Backtrace_NT_Decl(Symbol::NT  &n, ::Type::Base *s);

      void print(Printer::Base &p) const;
      //void codegen();
      
      const std::string &name() const { return name_; }
      ::Type::Base* score_type() const { return score_type_; }

      const std::list<std::string> &track_args() const { return track_args_; }

      const std::list<Para_Decl::Base*> &ntparas() const { return ntparas_; }
  };

}

#endif
