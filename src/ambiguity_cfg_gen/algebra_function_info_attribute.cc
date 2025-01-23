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
#include "algebra_function_info_attribute.hh"


Util::AlgebraFunctionInfoAttribute::AlgebraFunctionInfoAttribute()
  : Attribute("Util::AlgebraFunctionInfoAttribute"), algebraFunctionName(NULL),
  grammarRuleName(NULL), algebraFunctionDefinition(NULL) {
}


Util::AlgebraFunctionInfoAttribute::AlgebraFunctionInfoAttribute(
  AlgebraFunctionInfoAttribute& a)
  :   Attribute(a),
    algebraFunctionName(new std::string(*a.algebraFunctionName)),
    grammarRuleName(new std::string(*a.grammarRuleName)),
    algebraFunctionDefinition(a.algebraFunctionDefinition),
    algebraFunctionArgs(a.algebraFunctionArgs) {
}


Util::AlgebraFunctionInfoAttribute::~AlgebraFunctionInfoAttribute() {
}


void Util::AlgebraFunctionInfoAttribute::setAlgebraFunctionName(
  std::string* name) {
  this->algebraFunctionName = name;
}


std::string* Util::AlgebraFunctionInfoAttribute::getAlgebraFunctionName() {
  return this->algebraFunctionName;
}


void Util::AlgebraFunctionInfoAttribute::setGrammarRuleName(std::string* name) {
  this->grammarRuleName = name;
}


std::string* Util::AlgebraFunctionInfoAttribute::getGrammarRuleName() {
  return this->grammarRuleName;
}


void Util::AlgebraFunctionInfoAttribute::setAlgebraFunctionDefinition(
  Fn_Def* functionDefinition) {
  this->algebraFunctionDefinition = functionDefinition;
}


Fn_Def* Util::AlgebraFunctionInfoAttribute::getAlgebraFunctionDefinition() {
  return this->algebraFunctionDefinition;
}


void Util::AlgebraFunctionInfoAttribute::setAlgebraFunctionArguments(
  std::list<Fn_Arg::Base*> args) {
  this->algebraFunctionArgs = args;
}


std::list<Fn_Arg::Base*> Util::AlgebraFunctionInfoAttribute::
  getAlgebraFunctionArguments() {
  return this->algebraFunctionArgs;
}


Util::Attribute* Util::AlgebraFunctionInfoAttribute::clone() {
  return new AlgebraFunctionInfoAttribute(*this);
}
