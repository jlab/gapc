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


#ifndef SRC_CPP_HH_
#define SRC_CPP_HH_

#include <vector>
#include <string>
#include <list>

#include "operator.hh"

#include "printer.hh"
#include "codegen.hh"

#include "table.hh"
#include "symbol_fwd.hh"
#include "para_decl_fwd.hh"
#include "product.hh"

class Grammar;
class AST;

namespace Printer {
class Cpp : public Base {
 private:
    const AST *ast;
    // FIXME probably remove this
    bool pure_list_type;
    bool in_fn_head;
    bool pointer_as_itr;

    Type::Range *choice_range;


    void lines_start_mark(const std::list<Statement::Base*> &stmts);
    void lines_end_mark(const std::list<Statement::Base*> &stmts);

    void includes();
    void print_type_defs(const AST &ast);

 public:
    void print_zero_decls(const Grammar &grammar);

 private:
    void print_filter_decls(const AST &ast);
    void print_filter_init(const AST &ast);

    void print_table_decls(const Grammar &grammar);
    void print_seq_init(const AST &ast);
    void print_table_init(const AST &ast);
    void print_zero_init(const Grammar &grammar);
    void print_most_decl(const Symbol::NT &nt);
    void print_most_init(const AST &ast);
    void print_init_fn(const AST &ast);

    void print_window_inc_fn(const AST &ast);

 private:
    void print_run_fn(const AST &ast);
    void print_stats_fn(const AST &ast);

    /* generate code to print statements prior to result list, useful e.g. to
     * include statements for LaTeX documents when generating TikZ candidates */
    void print_document_header(const AST &ast);
    /* generate code to print one line of the root node of a tikz candidate
     * tree for an algebra value, e.g. let product be "(mfe * pfunc) * tikz"
     * two lines for mfe and pfunc shall be listed in the root node */
    void print_tikz_singleAlgebraValue(
        Product::Base *product, std::string candidate);
    /* generate code to print the LaTeX tikZ generated string that represents
     * the candidate tree. Return false if product does not contain tikz auto
     * generated algebra */
    bool print_tikz_value(Product::Base *product, std::string candidate);
    void print_tikz_candidate(const AST &ast, std::string candidate,
                                              std::string value);
    void print_value_pp(const AST &ast);

    void print(const std::list<Type::Base*> &types,
               const std::list<std::string*> &names);

    void print_id();

    void print_hash_decls(const AST &ast);

    void print_buddy_decls(const AST &ast);
    void print_buddy_init(const AST &ast);
    void print_marker_init(const AST &ast);
    void print_marker_clear(const AST &ast);

 public:
    /* generate code to print statements after reporing the result list,
     * useful e.g. to include statements for LaTeX documents when generating
     * TikZ candidates */
    void print_document_footer(const AST &ast);

    bool in_class;
    std::string class_name;
    Cpp()
      : Base(), ast(0), pure_list_type(false), in_fn_head(false),
      pointer_as_itr(false),
      choice_range(NULL),
      in_class(false)
    {}
    Cpp(const AST &ast_, std::ostream &o)
      : Base(o), ast(&ast_), pure_list_type(false), in_fn_head(false),
      pointer_as_itr(false),
      choice_range(NULL),
      in_class(false)
    {}
    void print(const Statement::For &stmt);
    void print(const Statement::While &stmt);
    void print(const Statement::Var_Decl &stmt);
    void print(const Statement::If &stmt);
    void print(const Statement::Switch &stmt);
    void print(const Statement::Return &stmt);
    void print(const Statement::Break &stmt);
    void print(const Statement::Continue &stmt);
    void print(const Statement::Increase &stmt);
    void print(const Statement::Decrease &stmt);
    void print(const Statement::Foreach &stmt);
    void print(const Statement::Sorter &stmt);
    void print(const Statement::Var_Assign &stmt);
    void print(const Statement::Fn_Call &stmt);
    void print(const Statement::Block &stmt);
    void print(const Statement::CustomCode &stmt);
    void print(const Statement::Backtrace_Decl &stmt);
    void print(const Statement::Backtrace_NT_Decl &stmt);
    void print(const Statement::Hash_Decl &stmt);
    void print(const Statement::Marker_Decl &stmt);

    void print(const Statement::SYCL_Accessor_Decl &stmt);
    void print(const Statement::SYCL_Submit_Kernel &stmt);
    void print(const Statement::SYCL_Buffer_Decl &stmt);
    void print(const Statement::SYCL_Host_Accessor_Decl &stmt);

    void print(const Fn_Def &fn_def);
    void print(const Operator &op);

    void print(const std::list<Statement::Base*> &stmts);

    void print(const Expr::Base &expr);
    void print(const Expr::New &expr);
    void print(const Var_Acc::Base &);


    void print(const Type::List &expr);
    void print(const Type::Tuple &expr);
    void print(const Type::TupleDef &expr);
    void print(const Type::Signature &expr);
    void print(const Type::Alphabet &expr);
    void print(const Type::Def &expr);
    void print(const Type::Choice &expr);
    void print(const Type::Void &expr);
    void print(const Type::RealVoid &expr);
    void print(const Type::Int &expr);
    void print(const Type::Integer &expr);
    void print(const Type::Size &expr);
    void print(const Type::Float &expr);
    void print(const Type::Single &expr);
    void print(const Type::String &expr);
    void print(const Type::Char &expr);
    void print(const Type::Bool &expr);
    void print(const Type::Usage &expr);
    void print(const Type::Range &expr);
    void print(const Type::Seq &expr);

    void print(const Type::Table &expr);
    void print(const Statement::Table_Decl &t);


    void print(const Type::Subseq &expr);
    void print(const Type::Shape &expr);
    void print(const Type::Referencable &expr);
    void print(const Type::Rational &expr);
    void print(const Type::BigInt &expr);
    void print(const Type::External &expr);
    void print(const Type::Eval_List &expr);
    void print(const Type::Backtrace &expr);
    void print(const Type::Backtrace_List &expr);

    void print(const Type::Multi &expr);

    void header(const AST &ast);
    void header_footer(const AST &ast);
    void footer(const AST &ast);
    void close_class();
    void typedefs(Code::Gen &code);

    void prelude(const Options &opts, const AST &ast);
    void imports(const AST &ast);

    void global_constants(const AST &ast);

    void makefile(const Options &opts);

 private:
    bool print_axiom_args(const AST &ast);
    void print_subopt_fn(const AST &ast);
    void print_backtrack_fn(const AST &ast);
    void print_backtrack_pp(const AST &ast);
    void print_kbacktrack_pp(const AST &ast);

 public:
    void backtrack_footer(const AST &ast);

 private:
    void print(const std::list<Statement::Var_Decl*> &l);
    void print_paras(const std::list<Statement::Var_Decl*> &l, char c);
    void print_names(const std::list<Statement::Var_Decl*> &l, char c);
    void print_eqs(const std::list<Statement::Var_Decl*> &l, char c);

    std::vector<Statement::Var_Decl*> ns;
    std::vector<std::string> inps;

    void set_tracks(const AST &ast);


    void print(Para_Decl::Base *p);
    void print(const std::list<Para_Decl::Base*> &paras);

    void print_arg(Expr::Base *e);

    void print_subseq_typedef(const AST &ast);

    void print_window_inc(const Symbol::NT &nt);
};

}  // namespace Printer

#endif  // SRC_CPP_HH_
