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

    * File:   operator_struct.hh
    * Author: gatter
    *
    * Created on June 22, 2015, 12:21 PM

}}} */

#ifndef SRC_OPERATOR_HH_
#define  SRC_OPERATOR_HH_

#include <list>
#include <string>
#include "printer_fwd.hh"
#include "type_fwd.hh"
#include "para_decl_fwd.hh"
#include "statement_fwd.hh"

class Operator {
  friend class Printer::Cpp;

 private:
  // The return type of the operator function
  Type::Base* return_type;
  // name of the operator container
  std::string* name;

 public:
  // name of the created object container
  std::string* object;

 private:
  // The list of parameter declarations for the operator function.
  std::list<Para_Decl::Base*> paras;

  // add a variable definition of a constant static value in container
  std::list<Statement::Var_Decl*> const_values;

 public:
  // The list of statements of the operator function
  std::list<Statement::Base*> stmts;


  Operator(Type::Base *r, std::string *ob) :
      return_type(r),  object(ob) {
      name = new std::string(*ob);
      name->append("_container");
  }

  void add_para(Type::Base *type, std::string *n);

  void add_const_value(Statement::Var_Decl *v);
};


#endif  // SRC_OPERATOR_HH_
