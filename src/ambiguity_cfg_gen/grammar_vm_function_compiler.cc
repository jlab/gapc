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


#include "grammar_vm_function_compiler.hh"

#include <cassert>
#include <string>
#include <list>
#include <boost/format.hpp>

#include "../const.hh"
#include "../hashtable.hh"


AmbiguityCFG::GrammarVMFunctionCompiler::GrammarVMFunctionCompiler(
  Algebra* algebra)
  : algebra(algebra) {
  this->declaredVariables = NULL;
  this->controlFlowReturned = false;
}


AmbiguityCFG::GrammarVMFunctionCompiler::~GrammarVMFunctionCompiler() {
  delete this->declaredVariables;
}


// Releases all allocated objects and resets internal data structures.
void AmbiguityCFG::GrammarVMFunctionCompiler::clear() {
  // TODO(who?): loop through the map and release all VarDeclInfo
  // items, before removing them form the list.
  if (this->declaredVariables != NULL) {
    this->declaredVariables->cleanDeep();
  }
  this->controlFlowReturned = false;
}


// Main entry point for the processing of a function definition.
AmbiguityCFG::GrammarVMCode* AmbiguityCFG::GrammarVMFunctionCompiler::
  processFnDef(std::string* algebraFunctionName) {
  // before we start we clear all internal data structures.
  clear();

  // Store the current algebra function name for later use.
  // and inter procedural access in a global variable.
  this->currentAlgebraFunctionName = algebraFunctionName;

  // Now we need the algebra function definition. First check
  // if the algebra function is defined.
  if (this->algebra->fns.find(*algebraFunctionName) ==
      this->algebra->fns.end()) {
    throw LogError(
      "gap-00165: Algebra function with name '" + *algebraFunctionName +
      "' not defined in algebra.");
  }
  Fn_Def *algebra_function = this->algebra->fns[*algebraFunctionName];

  // Create an empty symbol table, and start to fill it with
  // all function parameters before statements can be processed.
  this->declaredVariables = new Util::SymbolTable<std::string, VarInfoItem*>();

  // Get the list of all defined function parameters
  std::list<std::string*> args = algebra_function->names;
  // Add all defined algebra function arguments to the
  // table of defined variable names, because they are
  // visible variables inside of the algebra function.
  for (std::list<std::string*>::iterator i = args.begin();
       i != args.end(); ++i) {
    std::string *parameterName = *i;
    VarDeclInfo *infoItem = new VarDeclInfo();
    infoItem->variableName = parameterName;
    infoItem->productionFragment = NULL;
    this->declaredVariables->setElement(*parameterName, infoItem);
  }

  // Create a new code block where all GrammarVM commands will
  // be appended to.
  this->grammarVMCode = new GrammarVMCode(
    this->currentAlgebraFunctionName, args);

  // Get the statements that represents the algebra function implementation
  std::list<Statement::Base*> stmts = algebra_function->stmts;

  // Walk through all statements and create the CFG candidates
  // from this.
  processStatementList(stmts);

  // Returns the code which was gathered when all statements
  // were processed.
  return this->grammarVMCode;
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatementList(
  std::list<Statement::Base*> stmts) {
  // TODO(who?): write a comment.
  for (std::list<Statement::Base*>::iterator i = stmts.begin();
       i != stmts.end(); ++i) {
    processStatement(*i);
  }
}


// This method is a proxy for all subclasses of Statement::Base due
// to the missing polymorphism of function calls in C++.
void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::Base* stmt) {
  // check if the current context is still open. In case a
  // return statement occurred, we found unreachable statements
  if (this->controlFlowReturned) {
    throw LogError(stmt->location, "gap-00145: Unreachable statement.");
  }
  // Otherwise choose the appropriate function to process the
  // given statement.
  if (stmt->is(Statement::RETURN)) {
    Statement::Return *ret = dynamic_cast<Statement::Return*>(stmt);
    processStatement(ret);
  } else if (stmt->is(Statement::FN_CALL)) {
    Statement::Fn_Call *cll = dynamic_cast<Statement::Fn_Call*>(stmt);
    processStatement(cll);
  } else if (stmt->is(Statement::VAR_DECL)) {
    Statement::Var_Decl *varDecl = dynamic_cast<Statement::Var_Decl*>(stmt);
    processStatement(varDecl);
  } else if (stmt->is(Statement::VAR_ASSIGN)) {
    Statement::Var_Assign *varAssign = dynamic_cast<Statement::Var_Assign*> (
      stmt);
    processStatement(varAssign);
  } else if (stmt->is(Statement::IF)) {
    Statement::If *ifStmt = dynamic_cast<Statement::If*> (stmt);
    processStatement(ifStmt);
  } else if (stmt->is(Statement::BLOCK)) {
    Statement::Block *blck = dynamic_cast<Statement::Block*> (stmt);
    processStatement(blck);
  } else if (stmt->is(Statement::WHILE)) {
    Statement::While* whl = dynamic_cast<Statement::While*> (stmt);
    processStatement(whl);
  } else if (stmt->is(Statement::FOR)) {
    Statement::For* fr = dynamic_cast<Statement::For*> (stmt);
    processStatement(fr);
  } else if (stmt->is(Statement::FOREACH)) {
    Statement::Foreach* frch = dynamic_cast<Statement::Foreach*> (stmt);
    processStatement(frch);
  } else {
    throw LogError(
      stmt->location,
      "gap-00111: Unhandled statment type. Most probably this kind of "
      "statement\nis not allowed in a canonical pretty print grammar used to "
      "generate\nan ambiguity CFG.");
  }
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::Return* stmt) {
  // Parse the returned expression before the return-command
  // is issued.
  processExpr(stmt->expr);
  // //grammarVM
  // just add the expression on top of the operand stack to the
  // list of result-expressions of the current algebra function.
  this->grammarVMCode->appendCommand(new ReturnCommand(stmt->location));
  // this->grammarVMCode->appendCommand(new LoadContextCommand(stmt->location));
  // this->grammarVMCode->appendCommand(new ClearContextCommand(
  //    stmt->location));
  // //////grammarVM
  // As a last step, set the flag for a returned
  // control flow TRUE.
  this->controlFlowReturned = true;
}


// Processes a function call, which will most of the time be
// a call to the built-in function 'append'.
// At the moment no other functions are allowed in a canonical
// algebra, thus an exception is thrown if anything but an
// append-call is encountered.
void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::Fn_Call* stmt) {
  // TODO(who?): find out, why the field builtin is not filled
  // correctly by the parser, and, most importantly, if
  // it is wise to fix this. Are there other side effects
  // that will prevent old tested code from working correctly
  // if this is changed?

  // switch for each different built-in function
  switch (stmt->builtin) {
    case Statement::Fn_Call::APPEND: {
      processAppendCall(stmt->args);
      break;
    }
    case Statement::Fn_Call::STR_APPEND: {
      processAppendCall(stmt->args);
      break;
    }
    default: {
      if (*stmt->name_ == "append") {
        processAppendCall(stmt->args);
      } else {
        // at the moment no other function call than 'append'
        // is allowed in a algebra function body definition.
        throw LogError(stmt->location,
          "gap-00107: Function call not supported.\nFound application "
          "of function '"
          + *stmt->name_ + "' (Builtin="
          + str(boost::format("%1%") % stmt->builtin) + ").");
      }
    }
  }
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::Var_Decl* stmt) {
  // Check if the variable is already stored in the table.
  // If it is already stored, there is something wrong in
  // the source code.
  if (declaredVariables->containsElementLocally(*stmt->name)) {
    throw LogError(
      stmt->location, "gap-00108: Identifier '" + *stmt->name +
      "' already defined in algebra function '" +
      *this->currentAlgebraFunctionName + "'.");
  }

  // Next create a result element and store it under the
  // name of the variable identifier as defined in this
  // variable-declaration statement.
  VarDeclInfo *infoItem = new VarDeclInfo();
  std::string* variableName = stmt->name;
  infoItem->variableName = variableName;
  // //grammarVM
  // we will store the new allocated variable into
  // the global context, thus we need to load the
  // global structure pointer onto the stack.
  this->grammarVMCode->appendCommand(new CreateVarCommand(
    stmt->location, variableName));
  this->grammarVMCode->appendCommand(new LoadThisCommand(stmt->location));
  // //////grammarVM
  if (stmt->rhs != NULL) {
    processExpr(stmt->rhs);
    // //grammarVM
    this->grammarVMCode->appendCommand(new StoreVarCommand(
      stmt->location, stmt->name));
    // //////grammarVM
  } else {
    infoItem->productionFragment = new CFG::Epsilon();
    // //grammarVM
    this->grammarVMCode->appendCommand(new CreateEpsilonCommand(
      stmt->location));
    this->grammarVMCode->appendCommand(new StoreVarCommand(
      stmt->location, stmt->name));
    // //////grammarVM
  }
  declaredVariables->setElement(*stmt->name, infoItem);
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::Var_Assign* stmt) {
  // The variable-assign statement simply copies the content of
  // one variable-info-item into the destination info item.


  // Get the variable-information for the variable-access.
  VarDeclInfo* assignedVariable = NULL;
  VarInfoItem* infoItem = processVarAccess(stmt->acc);
  VarInfoItem* infoItem_ = processVarAccess_(stmt->acc);
  std::string* addignedVariableName = NULL;
  /* Cast the pointer to our desired type. The previous method
     call must not yield an instance of type RecordDeclInfo
     (RECORD_DECL_INFO).*/
  if (infoItem->is(VAR_DECL_INFO)) {
    assignedVariable = reinterpret_cast<VarDeclInfo*>(infoItem);
    addignedVariableName = assignedVariable->variableName;
  } else {
    throw LogError(
      stmt->acc->location,
      "gap-00129: Variable access yielded a record type.");
  }
  if (infoItem_->is(VAR_DECL_INFO)) {
    VarDeclInfo* assignedVariable = reinterpret_cast<VarDeclInfo*>(infoItem_);
    addignedVariableName = assignedVariable->variableName;
  } else {
    throw LogError(
      stmt->acc->location,
      "gap-00129: Variable access yielded a record type.");
  }


  // Do we have a valid instance?
  assert(assignedVariable != NULL);


  // Then store the production fragment which results from
  // evaluating the expression on the right-hand-side of the
  // assignment.
  processExpr(stmt->rhs);
  // //grammarVM
  this->grammarVMCode->appendCommand(new StoreVarCommand(
    stmt->location, addignedVariableName));
  // //////grammarVM
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::Block* stmt) {
  // First we create a new visibility level for all
  // defined variables, then we start processing all
  // statements defined in this block.
  std::list<Statement::Base*>* stmts = stmt->stmts();
  // Store the current variable-store, and create a new one
  // wrapped around the current store. We will restore the
  // pointer after
  Util::SymbolTable<std::string, VarInfoItem*>* tmpStore =
    this->declaredVariables;
  this->declaredVariables = new Util::SymbolTable<std::string, VarInfoItem*> (
    this->declaredVariables);
  processStatementList(*stmts);
  // now restore the variable-store
  this->declaredVariables = tmpStore;
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::If* stmt) {
  // First create a new context for each part (then/else), and
  // store the current variable context so we can restore it
  // at the end of processing the if-statement.
  Util::SymbolTable<std::string, VarInfoItem*>* currentContext =
    this->declaredVariables;
  bool currentControlFlowValue = this->controlFlowReturned;
  Util::SymbolTable<std::string, VarInfoItem*>* thenContext =
    new Util::SymbolTable<std::string, VarInfoItem*> (this->declaredVariables);
  Util::SymbolTable<std::string, VarInfoItem*>* elseContext =
    new Util::SymbolTable<std::string, VarInfoItem*> (this->declaredVariables);

  // //grammarVM
  this->grammarVMCode->appendCommand(new LoadContextCommand(stmt->location));
  this->grammarVMCode->appendCommand(new DuplicateCommand(stmt->location));
  this->grammarVMCode->appendCommand(new WrapContextCommand(stmt->location));
  this->grammarVMCode->appendCommand(new DuplicateCommand(stmt->location));
  this->grammarVMCode->appendCommand(new StoreContextCommand(stmt->location));
  // //////grammarVM

  // Then modify both contexts according to the expression
  // guarding the if-statement:
  // TODO(who?): write some appropriate code!

  // Process both blocks of statements (then/else) separately.
  // The separation is achieved by instanciating two variable
  // contexts and using them as current declaredVariables context.
  this->declaredVariables = thenContext;
  this->controlFlowReturned = false;
  processStatementList(stmt->then);
  bool thenContextReturned = this->controlFlowReturned;

  // if the context was closed, we will not need it any longer.
  if (thenContextReturned) {
    // //grammarVM
    this->grammarVMCode->appendCommand(new PopStackCommand(stmt->location));
    // //////grammarVM
  } else {
    // //grammarVM
    this->grammarVMCode->appendCommand(new DuplicateCommand(stmt->location));
    this->grammarVMCode->appendCommand(
      new ClearLocalContextCommand(stmt->location));
    this->grammarVMCode->appendCommand(new SwapCommand(stmt->location));
    // //////grammarVM
  }

  // //grammarVM
  this->grammarVMCode->appendCommand(new WrapContextCommand(stmt->location));
  this->grammarVMCode->appendCommand(new DuplicateCommand(stmt->location));
  this->grammarVMCode->appendCommand(new StoreContextCommand(stmt->location));
  // //////grammarVM

  //
  this->declaredVariables = elseContext;
  this->controlFlowReturned = false;
  processStatementList(stmt->els);
  bool elseContextReturned = this->controlFlowReturned;
  // OBSOLETE:
  // Important: if the else-statement-list is empty we treat
  // this the same way as if the else-context returned. The
  // only difference is that this situation does not lead to
  // closing the enclosing context, since not all branches
  // of the statement returned. That's why we need an additional
  // variable.
  // bool elseStatementListIsEmpty = stmt->els.size() == 0;
  // if (elseStatementListIsEmpty) {
  //  elseContextReturned = true;
  // }

  // If the else-context was closed, we do not need it any longer.
  if (elseContextReturned) {
    // //grammarVM
    this->grammarVMCode->appendCommand(new PopStackCommand(stmt->location));
    // //////grammarVM
  } else {
    // //grammarVM
    this->grammarVMCode->appendCommand(new DuplicateCommand(stmt->location));
    this->grammarVMCode->appendCommand(
      new ClearLocalContextCommand(stmt->location));
    // //////grammarVM
  }

  // Restore the current context and the control-flow-returned flag.
  this->declaredVariables = currentContext;
  this->controlFlowReturned = currentControlFlowValue;

  // If the block did not end with a return-statement, we merge
  // all changes into the current context. There are two main
  // cases we distinguish: first, none block had a return statement,
  // creating two alternatives in the current context, and second
  // only one block had a return statement. Thirds none of the two
  // blocks had a return statement, in which case we do nothing.
  if (!thenContextReturned && !elseContextReturned) {
    // //grammarVM
    this->grammarVMCode->appendCommand(
      new MergeContextsCommand(stmt->location));
    this->grammarVMCode->appendCommand(new StoreContextCommand(stmt->location));
    // //////grammarVM
  } else {
    // Otherwise one of both contexts remained on top of the stack.
    // Which one depends on which part contained the return-statement.
    // If the THEN-part returned, there will be the context of the
    // ELSE-part that continues to be active. On the other hand if
    // the ELSE-part returned, there must be the THEN-part context
    // on top of the stack, which must be activated.
    if (!thenContextReturned) {
      // //grammarVM
      this->grammarVMCode->appendCommand(
        new StoreContextCommand(stmt->location));
      // //////grammarVM
    } else if (!elseContextReturned) {
      // //grammarVM
      this->grammarVMCode->appendCommand(new PopStackCommand(stmt->location));
      // //////grammarVM
    } else {
      // In this case we close the current context, because both
      // branches of the if-then-else statement returned, which
      // means that the surrounding block also returned, because
      // no path behind the if-then-else statement is reachable.
      this->controlFlowReturned = true;
    }
  }
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::While* stmt) {
  processLoopStatement(stmt);
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::For* stmt) {
  processLoopStatement(stmt);
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processStatement(
  Statement::Foreach* stmt) {
  processLoopStatement(stmt);
}


// Processes the append-function call by adding the appropriate
// CFG structures to the current state.
void AmbiguityCFG::GrammarVMFunctionCompiler::processAppendCall(
  std::list<Expr::Base*> &args) {
  // We start off by checking some basics about the list of arguments.
  // First, the list must contain at least two arguments
  if (args.size() < 2) {
    // alternatively an exception is due!
    // TODO(who?): decide if an exception is the better solution
    // in this situation.
    return;
  }

  // Because args is a std::list we have to use an iterator for accessing
  // the elements of this list.
  std::list<Expr::Base*>::iterator i = args.begin();

  // If the first element in the list is not a variable access,
  // this is an error
  if (!(*i)->is(Expr::VACC)) {
    throw LogError(
      (*i)->location,
      "gap-00109: First argument to append method is not a variable access");
  }

  // Get the destination variable of the append-call, e.g. the
  // variable that receives production fragments to append to
  // its current content.
  Expr::Vacc *firstParameterAccess = dynamic_cast<Expr::Vacc*> (*i);
  VarDeclInfo* firstVarDeclInfo = processVarAccess(firstParameterAccess);

  // //grammarVM
  std::string* accessedVarName = processVarAccess_(firstParameterAccess);
  this->grammarVMCode->appendCommand(new DuplicateCommand(
    firstParameterAccess->location));
  this->grammarVMCode->appendCommand(new LoadVarCommand(
    firstParameterAccess->location, accessedVarName));
  // //////grammarVM


  // Now we turn to the next element of the argument-list, which should
  // point to the expression that is appended to the first argument.
  i++;

  // just append the two CFG parts
  processExpr(*i);  // NOTE: this call issues further GrammarVM code

  // //grammarVM
  this->grammarVMCode->appendCommand(new CombineCommand((*i)->location));
  this->grammarVMCode->appendCommand(new StoreVarCommand(
    (*i)->location, accessedVarName));
  // //////grammarVM

  ////////////////////////
  // TODO(who?): free elements of the old firstVarDeclInfo->productionFragments
  // that are just pointer, which will get lost if we just copy the elements
  // of the new result into its place
  ////////////////////////

  // We need to store this variable info item, because it was
  // only loaded from the context. By loading, it is only copied
  // into a list of copied items, but not in the list of locally
  // changed items.
  this->declaredVariables->setElement(*accessedVarName, firstVarDeclInfo);
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processLoopStatement(
  Statement::Block_Base* bb) {
  // The list of statements belonging to the loop body.
  std::list<Statement::Base*> stmts = bb->statements;
  // Save the current context in order to restore it later.
  Util::SymbolTable<std::string, VarInfoItem*>* currentContext =
    this->declaredVariables;

  // now that we have prepared everything, we process the loop body
  processStatementList(stmts);

  // Restore the old context.
  this->declaredVariables = currentContext;

  // TODO(who?): remove this line after this method implementation has
  // been completed
  throw LogError(
    bb->location,
    "gap-00000: Not implemented exception: AmbiguityCFG::FnDefProcessor::"
    "processStatement (Statement::While* stmt)");
}


void AmbiguityCFG::GrammarVMFunctionCompiler::processExpr(Expr::Base* expr) {
  // Depending on the subclass of the expression instance
  // given as parameter, we process this instance.
  if (expr->is(Expr::VACC)) {
    Expr::Vacc *parameterAccess = dynamic_cast<Expr::Vacc*> (expr);
    // //grammarVM
    std::string* accessedVarName = processVarAccess_(parameterAccess);
    this->grammarVMCode->appendCommand(new LoadVarCommand(
      expr->location, accessedVarName));
    // //////grammarVM
  } else if (expr->is(Expr::CONST)) {
    Expr::Const* constantExpr = dynamic_cast<Expr::Const*> (expr);
    Const::Base* constant = constantExpr->base;
    if (constant->is(Const::CHAR)) {
      Const::Char* ch = dynamic_cast<Const::Char*> (constant);
      // //grammarVM
      this->grammarVMCode->appendCommand(new CreateTerminalCommand(
        expr->location, new std::string(1, ch->c)));
      // //////grammarVM
    } else if (constant->is(Const::STRING)) {
      Const::String* str = dynamic_cast<Const::String*> (constant);
      // //grammarVM
      this->grammarVMCode->appendCommand(new CreateTerminalCommand(
        expr->location, str->s));
      // //////grammarVM
    } else if (constant->is(Const::INT)) {
      Const::Int* constInt = dynamic_cast<Const::Int*> (constant);
      // //grammarVM
      this->grammarVMCode->appendCommand(new CreateTerminalCommand(
        expr->location, new std::string(
          str(boost::format("%1%") % constInt->i))));
      // //////grammarVM
    } else {
      throw LogError(
        constant->location,
        "gap-00105: Unsupported constant value type in parameter list of "
        "append-function call.");
    }
  } else if (expr->is(Expr::FN_CALL)) {
    Expr::Fn_Call *fnCall = dynamic_cast<Expr::Fn_Call*> (expr);
    std::string *functionName = fnCall->name;

    // this is set true if this is not a function call but
    // a data constructor call
    bool dataConstructor = false;

    // test if this is data constructor call to the shape_t
    // data type. this should equal a string terminal in the VM
    if (*functionName == "shape_t") {
      dataConstructor = true;
    } else if (this->algebra->fns.find(*functionName) ==
             this->algebra->fns.end()) {
      // test if the function is defined in the algebra
      throw LogError(
        expr->location,
        "gap-00104: Function call to an unknown function\nFound call to '"
        + *functionName + "'");
    }

    // ...and the list of arguments used for the actual function call
    std::list<Expr::Base*> args = fnCall->exprs;
    // There are two iterators: one for the parameter names,
    // the other for the list of argument expressions. We
    // iterate through both in parallel.
    std::list<Expr::Base*>::iterator j;
    for (j = args.begin(); j != args.end(); ++j) {
      processExpr(*j);
    }

    if (dataConstructor) {
      // check if the data constructor has only one parameter!
      if (args.size() != 1) {
        throw LogError(
          expr->location,
          "gap-00146: Initializer for data type '" + *functionName +
          "' has more than one element.");
      }
      // //grammarVM
      // do nothing at all, the expression should be placed on top of the
      // stack in a way a function call parameter has been placed there.
      // this->grammarVMCode->appendCommand (new CreateTerminalCommand (
      // expr->location, str->s));
      // //////grammarVM
    } else {
      // now that we know that there is a function definition,
      // retrieve the algebra function pointer...
      Fn_Def *algebraFunction = this->algebra->fns[*functionName];
      // The argument mapping must be filled with values, which is
      // done in a simple loop, one argument name at a time:
      std::list<std::string*> parameterNames = algebraFunction->names;
      // Both sizes of the argument list and the parameter names list
      // must be of same size.
      assert(args.size() == parameterNames.size());
      // //grammarVM
      this->grammarVMCode->appendCommand(new CallFunctionCommand(
        expr->location, functionName, parameterNames));
      // //////grammarVM
    }
  } else if (expr->is(Expr::PLUS)) {
    // The plus-operator is analoguous to the append operation,
    // except for the handling of the left-hand-side of the
    // operation. In this case we only produce the combination
    // of both expressions, and leave the result on the stack.
    Expr::Plus* plusExpr = dynamic_cast<Expr::Plus*> (expr);
    Expr::Base* fstOp = plusExpr->left();
    Expr::Base* sndOp = plusExpr->right();

    // Create expressions on the VM stack for both operands
    // of the PLUS-operation.
    processExpr(fstOp);
    processExpr(sndOp);

    // //grammarVM
    this->grammarVMCode->appendCommand(new CombineCommand(expr->location));
    // //////grammarVM
  } else {
    throw LogError(
      expr->location, "gap-00106: Unsupported type of expression call.");
  }
}


AmbiguityCFG::VarDeclInfo* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess(Expr::Vacc* varAccess) {
  VarInfoItem* infoItem = processVarAccess(varAccess->var_acc);
  if (infoItem->is(VAR_DECL_INFO)) {
    return reinterpret_cast<VarDeclInfo*>(infoItem);
  } else {
    throw LogError(varAccess->location,
      "gap-00127: Variable access yielded a record type.");
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess(Var_Acc::Base *acc) {
  if (acc->is(Var_Acc::PLAIN)) {
    Var_Acc::Plain* plain = dynamic_cast<Var_Acc::Plain*> (acc);
    return processVarAccess(plain);
  } else if (acc->is(Var_Acc::COMP)) {
    Var_Acc::Comp* comp = dynamic_cast<Var_Acc::Comp*> (acc);
    return processVarAccess(comp);
  } else if (acc->is(Var_Acc::ARRAY)) {
    Var_Acc::Array* arr = dynamic_cast<Var_Acc::Array*> (acc);
    return processVarAccess(arr);
  } else {
    throw LogError(acc->location, "gap-00124: Unsupported variable access.");
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess(Var_Acc::Plain* acc) {
  if (acc->name == NULL) {
    // if there is no name given, then there must be a variable
    // declaration instead, otherwise a variable access does not
    // make any sense
    assert(acc->vdecl != NULL);
    // The name of the accessed variable can be found in
    // the declared variable name.
    return getVarDeclInfo(acc->location, acc->vdecl->name);
  } else {
    return getVarDeclInfo(acc->location, acc->name);
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess(Var_Acc::Comp *acc) {
  VarInfoItem* baseInfoItem = processVarAccess(acc->lhs);
  if (baseInfoItem->is(RECORD_DECL_INFO)) {
    RecordDeclInfo* rec = dynamic_cast<RecordDeclInfo*> (acc);
    return rec->getVarInfoItem(acc->rhs);
  } else {
    throw LogError(
      acc->location,
      "gap-00126: Access to a plain variable via record accessor.");
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess(Var_Acc::Array *acc) {
  throw LogError(acc->location,
    "gap-00000: AmbiguityCFG::FnDefProcessor::processVarAccess "
    "(Var_Acc::Array *acc)");
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  getVarDeclInfo(Loc location, std::string *varName) {
  // Check if there is an info item in the hashtable...
  if (!declaredVariables->containsElement(*varName)) {
    throw LogError(location, "gap-00110: Identifier '" + *varName +
      "' not defined in algebra function '" + "[TODO: get alg_name]" + "'.");
  }
  // ...and return the info item
  return declaredVariables->getElement(*varName);
}


std::string* AmbiguityCFG::GrammarVMFunctionCompiler::processVarAccess_(
  Expr::Vacc* varAccess) {
  VarInfoItem* infoItem = processVarAccess_(varAccess->var_acc);
  if (infoItem->is(VAR_DECL_INFO)) {
    return (reinterpret_cast<VarDeclInfo*>(infoItem))->variableName;
  } else {
    throw LogError(varAccess->location,
      "gap-00127: Variable access yielded a record type.");
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess_(Var_Acc::Base *acc) {
  if (acc->is(Var_Acc::PLAIN)) {
    Var_Acc::Plain* plain = dynamic_cast<Var_Acc::Plain*> (acc);
    return processVarAccess_(plain);
  } else if (acc->is(Var_Acc::COMP)) {
    Var_Acc::Comp* comp = dynamic_cast<Var_Acc::Comp*> (acc);
    return processVarAccess_(comp);
  } else if (acc->is(Var_Acc::ARRAY)) {
    Var_Acc::Array* arr = dynamic_cast<Var_Acc::Array*> (acc);
    return processVarAccess_(arr);
  } else {
    throw LogError(acc->location, "gap-00124: Unsupported variable access.");
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess_(Var_Acc::Plain* acc) {
  if (acc->name == NULL) {
    // if there is no name given, then there must be a variable
    // declaration instead, otherwise a variable access does not
    // make any sense
    assert(acc->vdecl != NULL);
    // //grammarVM
    this->grammarVMCode->appendCommand(new LoadThisCommand(acc->location));
    // LoadVarCommand* loadVar = new LoadVarCommand (acc->vdecl->name);
    // this->grammarVM->execute (loadVar);
    // delete loadVar;
    // //////grammarVM
    // The name of the accessed variable can be found in
    // the declared variable name.
    return getVarDeclInfo_(acc->location, acc->vdecl->name);
  } else {
    // //grammarVM
    this->grammarVMCode->appendCommand(new LoadThisCommand(acc->location));
    // LoadVarCommand* loadVar = new LoadVarCommand (acc->name);
    // this->grammarVM->execute (loadVar);
    // delete loadVar;
    // //////grammarVM
    return getVarDeclInfo_(acc->location, acc->name);
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess_(Var_Acc::Comp *acc) {
  VarInfoItem* baseInfoItem = processVarAccess_(acc->lhs);
  if (baseInfoItem->is(RECORD_DECL_INFO)) {
    RecordDeclInfo* rec = dynamic_cast<RecordDeclInfo*> (acc);
    return rec->getVarInfoItem(acc->rhs);
  } else {
    throw LogError(
      acc->location,
      "gap-00126: Access to a plain variable via record accessor.");
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  processVarAccess_(Var_Acc::Array *acc) {
  throw LogError(
    acc->location,
    "gap-00000: AmbiguityCFG::FnDefProcessor::processVarAccess "
    "(Var_Acc::Array *acc)");
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::GrammarVMFunctionCompiler::
  getVarDeclInfo_(Loc location, std::string *varName) {
  // Check if there is an info item in the hashtable...
  if (!declaredVariables->containsElement(*varName)) {
    throw LogError(
      location, "gap-00110: Identifier '" + *varName +
      "' not defined in algebra function '" + "[TODO: get alg_name]" + "'.");
  }
  // ...and return the info item
  return declaredVariables->getElement(*varName);
}
