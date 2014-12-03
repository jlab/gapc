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

#ifndef HASH_DECL_HH
#define HASH_DECL_HH

#include "base.hh"
#include "../bool.hh"
#include "../type_fwd.hh"

namespace Statement {

  class Hash_Decl : public Base {
    private:
      ::Type::Base *answer_type_;
      std::list<Statement::Base*> code_;
      std::list<Statement::Var_Decl*> filters_;
      std::list<Statement::Base*> finalize_code_;
      std::list<Statement::Base*> init_code_;

      std::list<Statement::Base*> k_code_;
      std::list<Statement::Base*> cutoff_code_;
      std::list<Statement::Base*> equal_score_code_;
      std::list<Statement::Base*> compare_code_;
      Bool kbest_;

      std::string name_;
      std::string class_name_;
    public:
      Hash_Decl();

      void print(Printer::Base &p) const;

      void set_answer_type(::Type::Base *t) { answer_type_ = t; }
      const ::Type::Base &answer_type() const
      { assert(answer_type_); return *answer_type_; }

      void set_code(const std::list<Statement::Base*> &l) { code_ = l; }
      const std::list<Statement::Base*> &code() const { return code_; }

      void set_filters(const std::list<Statement::Var_Decl*> &l)
      { filters_ = l; }
      const std::list<Statement::Var_Decl*> &filters() const
      { return filters_; }

      void set_finalize_code(const std::list<Statement::Base*> &l)
      { finalize_code_ = l; }
      const std::list<Statement::Base*> &finalize_code() const
      { return finalize_code_; }

      void set_init_code(const std::list<Statement::Base*> &l)
      { init_code_ = l; }
      const std::list<Statement::Base*> &init_code() const
      { return init_code_; }

      void set_suffix(const std::string &n);
      std::string name() const;
      std::string ext_name() const;
      void set_class_name(const std::string &n) { class_name_ = n; }

      void set_kbest(bool b);
      bool kbest() const { return kbest_; }
      const std::list<Statement::Base*> &k_code() const { return k_code_; }
      const std::list<Statement::Base*> &cutoff_code() const { return cutoff_code_; }
      const std::list<Statement::Base*> &equal_score_code() const { return equal_score_code_; }
      const std::list<Statement::Base*> &compare_code() const { return compare_code_; }
      void set_equal_score_code(const std::list<Statement::Base*> &l) { equal_score_code_ = l; }
      void set_compare_code(const std::list<Statement::Base*> &l) { compare_code_ = l; }
  };

}


#endif
