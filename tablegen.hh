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

#ifndef TABLEGEN_HH
#define TABLEGEN_HH


#include "table.hh"

#include <vector>
#include <list>

#include "type_fwd.hh"
#include "expr_fwd.hh"
#include "statement_fwd.hh"
#include "symbol_fwd.hh"

class Fn_Def;

class Tablegen {
  public:
    typedef std::vector<Table>::const_iterator itr;
    typedef std::vector<size_t>::const_iterator titr;

  private:
    // FIXME
  public:
    std::list<Statement::Base*> code;
    std::list<Statement::Base*> window_code;
    std::list<Statement::Var_Decl*> paras;
    std::list<Statement::Var_Decl*> ns;

  private:

    ::Type::Base *type;
    // FIXME for unit testing ...
  public:
    Expr::Base *size;
    Expr::Base *window_size;
    Expr::Base *off;
  private:
    Statement::Base *ret_zero;
    Expr::Base *cond;
    ::Type::Base *dtype;

    bool cyk_;
    bool window_mode_;

    void head(Expr::Base *&i, Expr::Base *&j, Expr::Base *&n,
      const Table &table, size_t track);
    void offset_const(titr track, itr first, const itr &end,
      Expr::Base *dim, Expr::Base *access);
    void offset_left_lin(titr track, itr first, const itr &end,
      Expr::Base *dim, Expr::Base *access);
    void offset_right_lin(titr track, itr first, const itr &end,
      Expr::Base *dim, Expr::Base *access);
    void offset_quad(titr track, itr first, const itr &end,
      Expr::Base *dim, Expr::Base *access);
    void offset(titr track, itr first, const itr &end,
      Expr::Base *dim, Expr::Base *access);

    Fn_Def *gen_is_tab();
    Fn_Def *gen_untab();
    Fn_Def *gen_tab();
    Fn_Def *gen_get_tab();
    Fn_Def *gen_size();

  public:
    Tablegen();

    void set_window_mode(bool b) { window_mode_ = b; }

    void offset(size_t track_pos, itr first, const itr &end);

    Statement::Table_Decl *create(Symbol::NT &nt,
      std::string *name, bool cyk);

};



#endif
