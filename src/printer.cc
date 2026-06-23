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


#include <cassert>
#include <cstring>
#include <algorithm>
#include <list>
#include <string>

#include "printer.hh"
#include "statement.hh"
#include "statement/backtrace_decl.hh"
#include "expr.hh"
#include "var_acc.hh"
#include "type.hh"
#include "type/backtrace.hh"
#include "fn_def.hh"
#include "ast.hh"
#include "options.hh"


Printer::Base::~Base() {}


void Printer::Base::print(const Fn_Def &fn_def) {}
void Printer::Base::print(const Operator &op) {}


void Printer::Base::print(const Statement::Base &stmt) {
  if (stmt.is_disabled())
    return;
  stmt.print(*this);
}


void Printer::Base::print(const Statement::For &stmt) {}
void Printer::Base::print(const Statement::While &stmt) {}
void Printer::Base::print(const Statement::Var_Decl &stmt) {}
void Printer::Base::print(const Statement::If &stmt) {}
void Printer::Base::print(const Statement::Switch &stmt) {}
void Printer::Base::print(const Statement::Return &stmt) {}
void Printer::Base::print(const Statement::Break &stmt) {}
void Printer::Base::print(const Statement::Increase &stmt) {}
void Printer::Base::print(const Statement::Decrease &stmt) {}
void Printer::Base::print(const Statement::Continue &stmt) {}
void Printer::Base::print(const Statement::Foreach &stmt) {}
void Printer::Base::print(const Statement::Sorter &stmt) {}
void Printer::Base::print(const Statement::Var_Assign &stmt) {}
void Printer::Base::print(const Statement::Fn_Call &stmt) {}
void Printer::Base::print(const Statement::Block &stmt) {}
void Printer::Base::print(const Statement::CustomCode &stmt) {}
void Printer::Base::print(const Statement::Backtrace_Decl &stmt) {}
void Printer::Base::print(const Statement::Backtrace_NT_Decl &stmt) {}
void Printer::Base::print(const Statement::Hash_Decl &stmt) {}
void Printer::Base::print(const Statement::Marker_Decl &stmt) {}
void Printer::Base::print(const Statement::Table_Decl &stmt) {}
void Printer::Base::print(const std::list<Statement::Base*> &stmts) {}


void Printer::Base::print(const Statement::SYCL_Buffer_Decl &stmt) {}
void Printer::Base::print(const Statement::SYCL_Submit_Kernel &stmt) {}
void Printer::Base::print(const Statement::SYCL_Accessor_Decl &stmt) {}
void Printer::Base::print(const Statement::SYCL_Host_Accessor_Decl &stmt) {}


void Printer::Base::print(const Type::List &t) {}
void Printer::Base::print(const Type::Tuple &t) {}
void Printer::Base::print(const Type::TupleDef &t) {}
void Printer::Base::print(const Type::Signature &t) {}
void Printer::Base::print(const Type::Alphabet &t) {}
void Printer::Base::print(const Type::Def &t) {}
void Printer::Base::print(const Type::Choice &t) {}
void Printer::Base::print(const Type::Void &t) {}
void Printer::Base::print(const Type::RealVoid &t) {}
void Printer::Base::print(const Type::Int &t) {}
void Printer::Base::print(const Type::Integer &t) {}
void Printer::Base::print(const Type::Size &t) {}
void Printer::Base::print(const Type::Float &t) {}
void Printer::Base::print(const Type::Single &t) {}
void Printer::Base::print(const Type::String &t) {}
void Printer::Base::print(const Type::Char &t) {}
void Printer::Base::print(const Type::Bool &t) {}
void Printer::Base::print(const Type::Usage &t) {}
void Printer::Base::print(const Type::Range &t) {}
void Printer::Base::print(const Type::Seq &t) {}
void Printer::Base::print(const Type::Table &t) {}
void Printer::Base::print(const Type::Subseq &t) {}
void Printer::Base::print(const Type::Shape &t) {}
void Printer::Base::print(const Type::Referencable &t) {}
void Printer::Base::print(const Type::Rational &t) {}
void Printer::Base::print(const Type::BigInt &t) {}
void Printer::Base::print(const Type::External &t) {}


void Printer::Base::print(const Type::Eval_List &t) {}
void Printer::Base::print(const Type::Backtrace &t) {}
void Printer::Base::print(const Type::Backtrace_List &t) {}


void Printer::Base::print(const Type::Multi &t) {}


void Printer::Base::header(const AST &ast) {}
void Printer::Base::header_footer(const AST &ast) {}
void Printer::Base::footer(const AST &ast) {}
void Printer::Base::prelude(const Options &opts) {}
void Printer::Base::imports(const AST &ast) {}


void Printer::Base::backtrack_footer(const AST &ast) {}


void Printer::Base::print(const Expr::Base &expr) {
  // FIXME
  assert(false);
  // expr.print(*this);
}


void Printer::Base::print(const Type::Base &t) {
  t.print(*this);
}


void Printer::Base::print(const Var_Acc::Base &va) {
  // FIXME
  assert(false);
  // va.print(*this);
}


namespace Printer {

Base &operator<<(Base &p, const std::string &c) {
  size_t x = std::count(c.begin(), c.end(), '\n');
  p.line_number += x;
  p.out << c;
  return p;
}

Base &operator<<(Base &p, const char *c) {
  if (!c)
    return p;
  size_t x = std::count(c, c + std::strlen(c), '\n');
  p.line_number += x;
  p.out << c;
  return p;
}


Base &operator<<(Base &p, const Fn_Def &b) {
  p.print(b);
  return p;
}
Base &operator<<(Base &p, const Operator &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Base &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Var_Decl &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Var_Assign &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::For &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::While &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Foreach &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Sorter &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Block &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Break &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Decrease &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Increase &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Continue &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Return &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Fn_Call &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::If &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Switch &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Backtrace_Decl &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Backtrace_NT_Decl &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Hash_Decl &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Marker_Decl &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Statement::Table_Decl &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const std::list<Statement::Base*> &stmts) {
  p.print(stmts);
  return p;
}

Base &operator<<(Base &p, const Expr::Base &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Var_Acc::Base &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Type::Base &b) {
  p.print(b);
  return p;
}

Base &operator<<(Base &p, const Type::List &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Tuple &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::TupleDef &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Signature &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Alphabet &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Def &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Choice &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Void &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Int &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Integer &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Size &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Float &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Single &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::String &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Char &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Bool &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Usage &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Range &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Seq &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Table &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Eval_List &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Backtrace_List &t) {
  p.print(t);
  return p;
}

Base &operator<<(Base &p, const Type::Multi &t) {
  p.print(t);
  return p;
}

}  // namespace Printer


std::string &Printer::Base::indent() {
  return indent_string;
}


void Printer::Base::set_indent_string() {
  indent_string = std::string(ind_count*2, ' ');
}


void Printer::Base::inc_indent() {
  ++ind_count;
  set_indent_string();
}


void Printer::Base::dec_indent() {
  assert(ind_count > 0);
  --ind_count;
  set_indent_string();
}


void Printer::Base::begin_fwd_decls() {
  fwd_decls = true;
}

void Printer::Base::end_fwd_decls() {
  fwd_decls = false;
}


#include "version.hh"


void Printer::Base::set_argv(char **argv, int argc) {
  std::ostringstream o, gapc_call;
  o  << "A dynamic programming evaluator generated by GAP-C.\n\n"
    << "  GAP-C version:\n    " << gapc::version_id << "\n\n"
    << "  GAP-C call:\n    ";
  for (int i = 0; i < argc; ++i) {
    gapc_call << argv[i];
    if (i+1 < argc) {
      gapc_call << ' ';
    }
  }

  gapc_version_string = gapc::version_id;
  gapc_call_string = gapc_call.str();
  o << gapc_call_string;
  o << "\n\n";
  id_string = o.str();
}


void Printer::Base::print_zero_decls(const Grammar &grammar) {
}
