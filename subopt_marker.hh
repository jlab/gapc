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

#ifndef SUBOPT_MARKER_HH
#define SUBOPT_MARKER_HH


#include "backtrack_base.hh"
#include "printer_fwd.hh"

#include "statement_fwd.hh"
#include "symbol_fwd.hh"

class Algebra;
class AST;
class Fn_Def;

class Subopt_Marker : public Backtrack_Base {
  private:
    std::list<Statement::Marker_Decl*> marker_decls;


    void adjust_list_types(Fn_Def *fn, Fn_Def *fn_type);
    void add_subopt_fn_args(Fn_Def *fn, const Symbol::NT &nt);
  public:
    void gen_instance(Algebra *score);
    void gen_instance(Algebra *score, Product::Sort_Type sort);
    void gen_instance(Algebra *score ,Product::Base *base, Product::Sort_Type sort);
    void gen_backtrack(AST &ast);
    void gen_instance_code(AST &ast);

    //void gen_algebra(Signature &signature);

    void print_header(Printer::Base &pp, AST &ast);
    void print_body(Printer::Base &pp, AST &ast);
};

#endif
