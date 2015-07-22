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

#ifndef BACKTRACK_BASE_HH
#define BACKTRACK_BASE_HH

#include <list>

#include "symbol_fwd.hh"
#include "type_fwd.hh"
#include "product.hh"
#include "printer_fwd.hh"

class AST;
class Signature;
class Instance;
class Algebra;
namespace Statement {
  class Backtrace_Decl;
  class Backtrace_NT_Decl;
}
class Filter;

class Backtrack_Base {
  private:
  protected:
    Algebra *score_algebra;
    Algebra *algebra;
    Instance *instance;
    Type::Base *value_type;
    Type::Base *pos_type;

    std::list<Statement::Backtrace_Decl*> bt_decls;
    std::list<Statement::Backtrace_NT_Decl*> bt_nt_decls;

    void remove_unused();
  public:
    Backtrack_Base();
    virtual ~Backtrack_Base() {};

    void gen_backtraces(Product::Base *algebra, const Algebra &score);
    virtual void gen_nt_decls(const std::list<Symbol::NT*> &nts);
    virtual void gen_algebra(Signature &signature, Type::Base *alph);

    Algebra* get_gen_algebra() {
        return algebra;
    }

    Instance *get_instance() {
        return instance;
    }
    
    virtual void gen_instance(Algebra *score) = 0;
    virtual void gen_instance(Algebra *score, Product::Sort_Type sort) = 0;
    
    virtual void gen_instance(Algebra *score, Product::Base *base, Product::Sort_Type sort) = 0;
    
    virtual void apply_filter(Filter *f) {}
    virtual void gen_backtrack(AST &ast) = 0;
    virtual void gen_instance_code(AST &ast) = 0;


    virtual void print_header(Printer::Base &pp, AST &ast) = 0;
    virtual void print_body(Printer::Base &pp, AST &ast) = 0;
};

#endif
