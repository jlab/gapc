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
#include "cc.hh"

#include "statement.hh"
#include "statement/fn_call.hh"
#include "expr.hh"
#include "type.hh"

#include "fn_def.hh"

#include "var_acc.hh"

void Printer::CC::print(const std::list<Statement::Base*> &stmts) {
  stream << indent() << '{' << endl;
  inc_indent();
  for (std::list<Statement::Base*>::const_iterator i = stmts.begin();
       i != stmts.end(); ++i)
    stream << **i << endl;
  dec_indent();
  stream << indent() << '}' << endl;
}

void Printer::CC::print(const Statement::For &stmt) {
  stream << indent() << "for(";
  stream << *stmt.var_decl;
  stream << ' '
    << *stmt.cond << "; ";
  if (!stmt.inc)
    stream << "++" << *stmt.var_decl->name << ")";
  stream << endl;
  stream << stmt.statements;
}

void Printer::CC::print(const Statement::Var_Decl &stmt) {
  assert(stmt.type);
  assert(stmt.name);
  stream << indent() << *stmt.type << ' ' << *stmt.name;
  if (stmt.rhs)
    stream << " = " << *stmt.rhs;
  stream << ';';
}

void Printer::CC::print(const Statement::If &stmt) {
  stream << indent() << "if (" << *stmt.cond << ")" << endl;
  if (stmt.then.size() == 1)
    inc_indent();
  stream << stmt.then;
  if (stmt.then.size() == 1)
    dec_indent();
  if (stmt.els.empty())
    return;
  stream << endl << indent() << "else" << std::endl;
  if (stmt.els.size() == 1)
    inc_indent();
  stream << stmt.els;
  if (stmt.els.size() == 1)
    dec_indent();
}

void Printer::CC::print(const Statement::Return &stmt) {
  stream << indent() << "return " << *stmt.expr << ';';
}

void Printer::CC::print(const Statement::Foreach &stmt) {
  stream << indent()
    << "foreach " << *stmt.elem->name << " in " << *stmt.container->name;
  print(stmt.statements);
}

void Printer::CC::print(const Statement::Var_Assign &stmt) {
  stream << indent();
  stream << *stmt.acc;
  stream << " = " << *stmt.rhs << ";";
}

void Printer::CC::print(const Statement::Fn_Call &stmt) {
  stream << indent() << stmt.name() << "( ";
  std::list<Expr::Base*>::const_iterator i = stmt.args.begin();
  if (i != stmt.args.end()) {
    stream << **i;
    for (++i; i != stmt.args.end(); ++i)
      stream << ", " << **i;
  }
  stream << ");";
}

void Printer::CC::print(const Statement::Block &stmt) {
  stream << indent() << "{" << endl;
  inc_indent();
  for (std::list<Statement::Base*>::const_iterator i = stmt.statements.begin();
      i != stmt.statements.end(); ++i)
    stream << **i << endl;
  dec_indent();
  stream << indent() << "}" << endl;
}

void Printer::CC::print(const Fn_Def &fn_def) {
  if (fn_def.adaptor)
    stream << *fn_def.adaptor;
  stream << indent() << *fn_def.return_type << ' ';
  if (fn_def.target_name().empty()) {
    stream << *fn_def.name;
  } else {
    stream << fn_def.target_name();
  }
  stream << '(';
  std::list<std::string*>::const_iterator j = fn_def.names.begin();
  std::list<Type::Base*>::const_iterator i = fn_def.types.begin();
  if (i != fn_def.types.end() && j != fn_def.names.end())
    stream << **i << ' ' << **j;
  ++i; ++j;
  for (; i != fn_def.types.end() && j != fn_def.names.end(); ++i, ++j) {
    stream << " ," << **i << ' ' << **j;
  }
  stream << ')' << endl;
  stream << indent() << '{' << endl;
  inc_indent();
  for (std::list<Statement::Base*>::const_iterator s = fn_def.stmts.begin();
      s != fn_def.stmts.end(); ++s)
    stream << **s << endl;
  dec_indent();
  stream << indent() << '}' << endl;
}

void Printer::CC::print(const Expr::Base &expr) {
  // Default pretty print
  external_out() << expr;
  // if needed use dispatch code like for statement
  // which is called by base class
}

void Printer::CC::print(const Type::Base &b) {
  // Default pretty print
  external_out() << b;
  // if needed use dispatch code like for statement
  // which is called by base class
}


void Printer::CC::print(const Var_Acc::Base &b) {
  // Default pretty print
  external_out() << b;
  // if needed use dispatch code like for statement
  // which is called by base class
}
