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

#ifndef TABLE_DECL_HH
#define TABLE_DECL_HH

#include "base.hh"

#include <list>

#include "../type_fwd.hh"
#include "../symbol_fwd.hh"

class Fn_Def;

namespace Statement {

  class Table_Decl : public Base {
    private:

      Symbol::NT &nt_;

      ::Type::Base *type_;
      ::Type::Base *pos_type_;
      std::string *name_;
      bool cyk_;

      Fn_Def *fn_is_tab_;
      Fn_Def *fn_untab_;
      Fn_Def *fn_tab_;
      Fn_Def *fn_get_tab_;
      Fn_Def *fn_size_;

      std::list<Statement::Var_Decl*> ns_;
    public:
      Table_Decl(
          Symbol::NT &nt,
          ::Type::Base *t, std::string *n, 
          bool c,
          Fn_Def *fn_is_tab,
          Fn_Def *fn_tab,
          Fn_Def *fn_get_tab,
          Fn_Def *fn_size,
          const std::list<Statement::Var_Decl*> &ns
          );

      void print(Printer::Base &p) const;

      const std::string &name() const { assert(name_); return *name_; }
      const ::Type::Base &datatype() const { assert(type_); return *type_; }
      const ::Type::Base &pos_type() const { assert(pos_type_); return *pos_type_; }
      bool cyk() const { return cyk_; }
      const std::list<Statement::Var_Decl*> &ns() const { return ns_; }

      const Fn_Def &fn_is_tab() const { return *fn_is_tab_; }
      const Fn_Def &fn_untab() const { assert(fn_untab_); return *fn_untab_; }
      void set_fn_untab(Fn_Def *d) { fn_untab_=d; }
      const Fn_Def &fn_tab() const { return *fn_tab_; }
      const Fn_Def &fn_get_tab() const { return *fn_get_tab_; }
      const Fn_Def &fn_size() const { return *fn_size_; }

      const Symbol::NT &nt() const { return nt_; }

  };

}


#endif
