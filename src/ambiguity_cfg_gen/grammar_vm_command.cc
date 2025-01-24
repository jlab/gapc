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

#include "grammar_vm_command.hh"
#include <string>
#include <list>


AmbiguityCFG::GrammarVMCommand::GrammarVMCommand(
  AmbiguityCFG::GrammarVMCommandType type, Loc& location)
  : type(type), location(location) {
}


AmbiguityCFG::GrammarVMCommand::~GrammarVMCommand() {
}


bool AmbiguityCFG::GrammarVMCommand::is(
  AmbiguityCFG::GrammarVMCommandType type) {
  return (this->type = type);
}


AmbiguityCFG::GrammarVMCommandType AmbiguityCFG::GrammarVMCommand::getType() {
  return this->type;
}

Loc AmbiguityCFG::GrammarVMCommand::getLocation() {
  return this->location;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::LoadIntCommand::LoadIntCommand(Loc& location, int value)
  : GrammarVMCommand(LOAD_INT, location), value(value) {
}


AmbiguityCFG::LoadIntCommand::~LoadIntCommand() {
}


int AmbiguityCFG::LoadIntCommand::getValue() {
  return this->value;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::LoadThisCommand::LoadThisCommand(Loc& location)
  : GrammarVMCommand(LOAD_THIS, location) {
}


AmbiguityCFG::LoadThisCommand::~LoadThisCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////



AmbiguityCFG::LoadVarCommand::LoadVarCommand(
  Loc& location, std::string* variableName)
  : GrammarVMCommand(LOAD_VAR, location), variableName(variableName) {
}


AmbiguityCFG::LoadVarCommand::~LoadVarCommand() {
}


std::string* AmbiguityCFG::LoadVarCommand::getVariableName() {
  return this->variableName;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::StoreVarCommand::StoreVarCommand(
  Loc& location, std::string* variableName)
  : GrammarVMCommand(STORE_VAR, location), variableName(variableName) {
}


AmbiguityCFG::StoreVarCommand::~StoreVarCommand() {
}


std::string* AmbiguityCFG::StoreVarCommand::getVariableName() {
  return this->variableName;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::CreateVarCommand::CreateVarCommand(
  Loc& location, std::string* variableName)
  : GrammarVMCommand(CREATE_VAR, location), variableName(variableName) {
}


AmbiguityCFG::CreateVarCommand::~CreateVarCommand() {
}


std::string* AmbiguityCFG::CreateVarCommand::getVariableName() {
  return this->variableName;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::LoadContextCommand::LoadContextCommand(Loc& location)
  : GrammarVMCommand(LOAD_CONTEXT, location) {
}


AmbiguityCFG::LoadContextCommand::~LoadContextCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::StoreContextCommand::StoreContextCommand(Loc& location)
  : GrammarVMCommand(STORE_CONTEXT, location) {
}


AmbiguityCFG::StoreContextCommand::~StoreContextCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::WrapContextCommand::WrapContextCommand(Loc& location)
  : GrammarVMCommand(WRAP_CONTEXT, location) {
}


AmbiguityCFG::WrapContextCommand::~WrapContextCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::MergeContextsCommand::MergeContextsCommand(Loc& location)
  : GrammarVMCommand(MERGE_CONTEXTS, location) {
}


AmbiguityCFG::MergeContextsCommand::~MergeContextsCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::ClearContextCommand::ClearContextCommand(Loc& location)
  : GrammarVMCommand(CLEAR_CONTEXT, location) {
}


AmbiguityCFG::ClearContextCommand::~ClearContextCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::ClearLocalContextCommand::ClearLocalContextCommand(Loc& location)
  : GrammarVMCommand(CLEAR_LOCAL_CONTEXT, location) {
}


AmbiguityCFG::ClearLocalContextCommand::~ClearLocalContextCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::CreateEpsilonCommand::CreateEpsilonCommand(Loc& location)
  : GrammarVMCommand(CREATE_EPSILON, location) {
}


AmbiguityCFG::CreateEpsilonCommand::~CreateEpsilonCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::CreateTerminalCommand::CreateTerminalCommand(
  Loc& location, std::string* terminal)
  : GrammarVMCommand(CREATE_TERMINAL, location), terminal(terminal) {
}


AmbiguityCFG::CreateTerminalCommand::~CreateTerminalCommand() {
}


std::string* AmbiguityCFG::CreateTerminalCommand::getTerminalString() {
  return this->terminal;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::CreateNonTerminalCommand::CreateNonTerminalCommand(
  Loc& location, std::string* name)
  : GrammarVMCommand(CREATE_NONTERMINAL, location), name(name) {
}


AmbiguityCFG::CreateNonTerminalCommand::~CreateNonTerminalCommand() {
}


std::string* AmbiguityCFG::CreateNonTerminalCommand::getNonTerminalName() {
  return this->name;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::CreateProductionFragmentCommand::CreateProductionFragmentCommand(
  Loc& location, CFG::Base* b)
  : GrammarVMCommand(CREATE_PRODUCTION_FRAGMENT, location),
  productionFragment(b) {
}


AmbiguityCFG::CreateProductionFragmentCommand::
  ~CreateProductionFragmentCommand() {
}


CFG::Base* AmbiguityCFG::CreateProductionFragmentCommand::
  getProductionFragment() {
  return this->productionFragment;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::CombineCommand::CombineCommand(Loc& location)
  : GrammarVMCommand(COMBINE, location) {
}


AmbiguityCFG::CombineCommand::~CombineCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::ReturnCommand::ReturnCommand(Loc& location)
  : GrammarVMCommand(RETURN, location) {
}


AmbiguityCFG::ReturnCommand::~ReturnCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::DuplicateCommand::DuplicateCommand(Loc& location)
  : GrammarVMCommand(DUPLICATE, location) {
}


AmbiguityCFG::DuplicateCommand::~DuplicateCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::SwapCommand::SwapCommand(Loc& location)
  : GrammarVMCommand(SWAP, location) {
}


AmbiguityCFG::SwapCommand::~SwapCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::PopStackCommand::PopStackCommand(Loc& location)
  : GrammarVMCommand(POP_STACK, location) {
}


AmbiguityCFG::PopStackCommand::~PopStackCommand() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::CallFunctionCommand::CallFunctionCommand(
  Loc& location, std::string* functionName,
  std::list<std::string*> &parameterNames)
  : GrammarVMCommand(CALL_FUNCTION, location), functionName(functionName),
  parameterNames(parameterNames) {
}


AmbiguityCFG::CallFunctionCommand::~CallFunctionCommand() {
}


std::string* AmbiguityCFG::CallFunctionCommand::getFunctionName() {
  return this->functionName;
}


std::list<std::string*> AmbiguityCFG::CallFunctionCommand::getParameterNames() {
  return this->parameterNames;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
