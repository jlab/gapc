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


#ifndef SRC_PRINTER_HH_
#define SRC_PRINTER_HH_

#include <list>
#include <ostream>
#include <iostream>
#include <string>

#include "statement_fwd.hh"
#include "expr_fwd.hh"
#include "var_acc_fwd.hh"
#include "type_fwd.hh"
#include "statement.hh"


class Grammar;

class Fn_Def;
class AST;
class Options;
class Operator;


namespace Printer {
class Base;
}

inline Printer::Base &endl(Printer::Base &p);


namespace Printer {

class Base {
 private:
  unsigned int ind_count;
  std::string indent_string;
  void set_indent_string();

  template<class T> friend inline Base &operator<<(Base &p, const T &c);
  friend inline Base &operator<<(Base &p, std::ostream& (*fn)(std::ostream&) );
  friend Base &operator<<(Base &p, const std::string &c);
  friend Base &operator<<(Base &p, const char *c);
  friend inline Printer::Base &::endl(Printer::Base &p);
  std::ostream &out;

 protected:
  size_t line_number;
  std::string in_name;
  std::string out_name;

  std::string &indent();
  void inc_indent();
  void dec_indent();

  std::ostream &external_out() { return out; }
  Base &stream;
  bool fwd_decls;

  std::string id_string;
  std::string gapc_call_string, gapc_version_string;

 public:
  Base() : ind_count(0), out(std::cerr), line_number(0), stream(*this),
    fwd_decls(false) {}
  explicit Base(std::ostream &o) : ind_count(0), out(o), line_number(0),
  stream(*this),
    fwd_decls(false) {}

  virtual ~Base();

  void set_files(const std::string &i, const std::string &o) {
    in_name = i;
    out_name = o;
  }

  virtual void print(const Fn_Def &fn_def);
  virtual void print(const Operator &op);

  virtual void print(const Statement::Base &stmt);
  virtual void print(const Statement::For &stmt);
  virtual void print(const Statement::While &stmt);
  virtual void print(const Statement::Var_Decl &stmt);
  virtual void print(const Statement::If &stmt);
  virtual void print(const Statement::Switch &stmt);
  virtual void print(const Statement::Return &stmt);
  virtual void print(const Statement::Break &stmt);
  virtual void print(const Statement::Increase &stmt);
  virtual void print(const Statement::Decrease &stmt);
  virtual void print(const Statement::Continue &stmt);
  virtual void print(const Statement::Foreach &stmt);
  virtual void print(const Statement::Sorter &stmt);
  virtual void print(const Statement::Var_Assign &stmt);
  virtual void print(const Statement::Fn_Call &stmt);
  virtual void print(const Statement::Block &stmt);
  virtual void print(const Statement::CustomCode &stmt);
  virtual void print(const Statement::Backtrace_Decl &stmt);
  virtual void print(const Statement::Backtrace_NT_Decl &stmt);
  virtual void print(const Statement::Hash_Decl &stmt);
  virtual void print(const Statement::Marker_Decl &stmt);
  virtual void print(const Statement::Table_Decl &stmt);


  virtual void print(const Statement::SYCL_Buffer_Decl &stmt);
  virtual void print(const Statement::SYCL_Submit_Kernel &stmt);
  virtual void print(const Statement::SYCL_Accessor_Decl &stmt);
  virtual void print(const Statement::SYCL_Host_Accessor_Decl &stmt);


  virtual void print(const Expr::Base &);
  virtual void print(const Type::Base &);
  virtual void print(const Var_Acc::Base &);

  virtual void print(const std::list<Statement::Base*> &stmts);

  virtual void print(const Type::List &expr);
  virtual void print(const Type::Tuple &expr);
  virtual void print(const Type::TupleDef &expr);
  virtual void print(const Type::Signature &expr);
  virtual void print(const Type::Alphabet &expr);
  virtual void print(const Type::Def &expr);
  virtual void print(const Type::Choice &expr);
  virtual void print(const Type::Void &expr);
  virtual void print(const Type::RealVoid &expr);
  virtual void print(const Type::Int &expr);
  virtual void print(const Type::Integer &expr);
  virtual void print(const Type::Size &expr);
  virtual void print(const Type::Float &expr);
  virtual void print(const Type::Single &expr);
  virtual void print(const Type::String &expr);
  virtual void print(const Type::Char &expr);
  virtual void print(const Type::Bool &expr);
  virtual void print(const Type::Usage &expr);
  virtual void print(const Type::Range &expr);
  virtual void print(const Type::Seq &expr);
  virtual void print(const Type::Table &expr);
  virtual void print(const Type::Subseq &expr);
  virtual void print(const Type::Shape &expr);
  virtual void print(const Type::Referencable &expr);
  virtual void print(const Type::BigInt &expr);
  virtual void print(const Type::Rational &expr);
  virtual void print(const Type::External &expr);

  virtual void print(const Type::Eval_List &expr);
  virtual void print(const Type::Backtrace &expr);
  virtual void print(const Type::Backtrace_List &expr);

  virtual void print(const Type::Multi  &expr);


  virtual void header(const AST &ast);
  virtual void header_footer(const AST &ast);
  virtual void footer(const AST &ast);

  virtual void prelude(const Options &opts);

  virtual void imports(const AST &ast);

  virtual void begin_fwd_decls();
  virtual void end_fwd_decls();

  virtual void backtrack_footer(const AST &ast);

  virtual void set_argv(char **argv, int argc);


  virtual void print_zero_decls(const Grammar &grammar);
};  // class Base


inline Base &operator<<(Base &p, Base& (*fn)(Base&)) {
  return fn(p);
}


inline Base &operator<<(Base &p, std::ostream& (*fn)(std::ostream&)) {
  fn(p.out);
  p.line_number++;
  return p;
}


Base &operator<<(Base &p, const Fn_Def &b);
Base &operator<<(Base &p, const Operator &b);

Base &operator<<(Base &p, const Statement::Base &b);
Base &operator<<(Base &p, const Statement::Block &b);
Base &operator<<(Base &p, const Statement::Fn_Call &b);
Base &operator<<(Base &p, const Statement::For &b);
Base &operator<<(Base &p, const Statement::While &b);
Base &operator<<(Base &p, const Statement::Foreach &b);
Base &operator<<(Base &p, const Statement::Sorter &b);
Base &operator<<(Base &p, const Statement::If &b);
Base &operator<<(Base &p, const Statement::Switch &b);
Base &operator<<(Base &p, const Statement::Return &b);
Base &operator<<(Base &p, const Statement::Break &b);
Base &operator<<(Base &p, const Statement::Increase &b);
Base &operator<<(Base &p, const Statement::Decrease &b);
Base &operator<<(Base &p, const Statement::Continue &b);
Base &operator<<(Base &p, const Statement::Var_Assign &b);
Base &operator<<(Base &p, const Statement::Var_Decl &b);
Base &operator<<(Base &p, const Statement::Backtrace_Decl &b);
Base &operator<<(Base &p, const Statement::Backtrace_NT_Decl &b);
Base &operator<<(Base &p, const Statement::Hash_Decl &b);
Base &operator<<(Base &p, const Statement::Marker_Decl &b);
Base &operator<<(Base &p, const Statement::Table_Decl &b);

Base &operator<<(Base &p, const std::list<Statement::Base*> &stmts);

Base &operator<<(Base &p, const Expr::Base &b);

Base &operator<<(Base &p, const Var_Acc::Base &b);

template<typename T> inline Base &operator<<(Base &p, const T &c) {
  p.out << c;
  return p;
}

Base &operator<<(Base &p, const std::string &c);
Base &operator<<(Base &p, const char *c);

Base &operator<<(Base &p, const Type::Base &b);

Base &operator<<(Base &p, const Type::List &t);
Base &operator<<(Base &p, const Type::Tuple &t);
Base &operator<<(Base &p, const Type::TupleDef &t);
Base &operator<<(Base &p, const Type::Signature &t);
Base &operator<<(Base &p, const Type::Alphabet &t);
Base &operator<<(Base &p, const Type::Def &t);
Base &operator<<(Base &p, const Type::Choice &t);
Base &operator<<(Base &p, const Type::Void &t);
Base &operator<<(Base &p, const Type::Int &t);
Base &operator<<(Base &p, const Type::Integer &t);
Base &operator<<(Base &p, const Type::Size &t);
Base &operator<<(Base &p, const Type::Float &t);
Base &operator<<(Base &p, const Type::Single &t);
Base &operator<<(Base &p, const Type::String &t);
Base &operator<<(Base &p, const Type::Char &t);
Base &operator<<(Base &p, const Type::Bool &t);
Base &operator<<(Base &p, const Type::Usage &t);
Base &operator<<(Base &p, const Type::Range &t);
Base &operator<<(Base &p, const Type::Seq &t);
Base &operator<<(Base &p, const Type::Table &t);
Base &operator<<(Base &p, const Type::Eval_List &t);
Base &operator<<(Base &p, const Type::Backtrace_List &t);

Base &operator<<(Base &p, const Type::Multi &t);

}  // namespace Printer


inline Printer::Base &endl(Printer::Base &p) {
  p.line_number++;
  p.out << '\n';
  return p;
}


#endif  // SRC_PRINTER_HH_
