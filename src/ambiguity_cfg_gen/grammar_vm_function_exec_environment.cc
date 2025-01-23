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

#include "grammar_vm_function_exec_environment.hh"


#include <sstream>
#include <string>
#include <list>
#include <vector>
#include <boost/format.hpp>

#include "../log.hh"

#include "../cfg/cfg.hh"
#include "../printer/cfg_pretty_print.hh"


AmbiguityCFG::GrammarVMFunctionExecEnvironment::
  GrammarVMFunctionExecEnvironment(
    GrammarVM* vm, AmbiguityCFG::GrammarVMCode* code)
  : vm(vm), code(code) {
}


AmbiguityCFG::GrammarVMFunctionExecEnvironment::
  ~GrammarVMFunctionExecEnvironment() {
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::executeCode(
  AmbiguityCFG::VariableContext* c) {
  this->executeCode(new MultiVariableContext(c));
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::executeCode(
  AmbiguityCFG::MultiVariableContext* c) {
  // Wrap the context.
  this->context = new MultiVariableContext(c);
  Log::instance()->debugMessage("");
  Log::instance()->debugMessage("GrammarVM executing '"
    + *code->getFunctionName() + "'");
  this->execute(this->code->getCode());
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::execute(
  AmbiguityCFG::GrammarVMCommand* cmd) {
  switch (cmd->getType()) {
    case LOAD_INT: {
      Log::instance()->debugMessage("LOAD_INT");
      // TODO(who?): seems to unsused. Remove this command from the source code.
      break;
    }
    case LOAD_CHAR: {
      Log::instance()->debugMessage("LOAD_CHAR");
      // TODO(who?): seems to unsused. Remove this command from the source code.
      break;
    }
    case LOAD_STRING: {
      Log::instance()->debugMessage("LOAD_STRING");
      // TODO(who?): seems to unsused. Remove this command from the source code.
      break;
    }
    case LOAD_THIS: {
      Log::instance()->debugMessage("LOAD_THIS " + str(boost::format(
        "%1%") % this->context));
      // load a record-info-wrapper of the current global
      // variable context onto the stack.
      this->stack.push(new NamedAccessGrammarVMStackElement(this->context));
      break;
    }
    case LOAD_VAR: {
      LoadVarCommand* loadVar = dynamic_cast<LoadVarCommand*> (cmd);
      this->executeLoadVar(loadVar);
      break;
    }
    case STORE_VAR: {
      StoreVarCommand* storeVar = dynamic_cast<StoreVarCommand*> (cmd);
      this->executeStoreVar(storeVar);
      break;
    }
    case CREATE_VAR: {
      CreateVarCommand* createVar = dynamic_cast<CreateVarCommand*> (cmd);
      this->context->defineLocalVariable(createVar->getVariableName());
      break;
    }
    case LOAD_CONTEXT: {
      Log::instance()->debugMessage("LOAD_CONTEXT " + str(boost::format(
        "%1%") % this->context));
      this->stack.push(new ContextGrammarVMStackElement(this->context));
      break;
    }
    case STORE_CONTEXT: {
      GrammarVMStackElement* stackTop = this->stack.pop();
      if (!stackTop->is(CONTEXT_ELEMENT)) {
        throw LogError("gap-00219: Expected context element on stack.");
      }
      ContextGrammarVMStackElement* contextElement =
        dynamic_cast<ContextGrammarVMStackElement*> (stackTop);
      this->context = contextElement->getContext();
      Log::instance()->debugMessage("STORE_CONTEXT [" + str(boost::format(
        "%1%") % this->context) + "]");
      delete contextElement;
      break;
    }
    case WRAP_CONTEXT: {
      GrammarVMStackElement* stackTop = this->stack.pop();
      if (!stackTop->is(CONTEXT_ELEMENT)) {
        throw LogError("gap-00219: Expected context element on stack.");
      }
      ContextGrammarVMStackElement* contextElement =
        dynamic_cast<ContextGrammarVMStackElement*> (stackTop);
      MultiVariableContext* wrappedContext = new MultiVariableContext(
        contextElement->getContext());
      Log::instance()->debugMessage("WRAP_CONTEXT " + str(
        boost::format("%1%") % contextElement->getContext()));
      this->stack.push(new ContextGrammarVMStackElement(wrappedContext));
      delete contextElement;
      break;
    }
    case MERGE_CONTEXTS: {
      Log::instance()->debugMessage("MERGE_CONTEXTS");
      GrammarVMStackElement* fstElement = this->stack.pop();
      GrammarVMStackElement* sndElement = this->stack.pop();
      // check if both elements are of the type context
      if (!fstElement->is(CONTEXT_ELEMENT)) {
        throw LogError("gap-00219: Expected context element on stack.");
      }
      ContextGrammarVMStackElement* fstContextElement =
        dynamic_cast<ContextGrammarVMStackElement*> (fstElement);
      if (!sndElement->is(CONTEXT_ELEMENT)) {
        throw LogError("gap-00224: Expected context element on stack.");
      }
      ContextGrammarVMStackElement* sndContextElement =
        dynamic_cast<ContextGrammarVMStackElement*> (sndElement);
      this->stack.push(new ContextGrammarVMStackElement(
        new MultiVariableContext(
          fstContextElement->getContext(), sndContextElement->getContext())));
      delete fstElement;
      delete sndElement;
      break;
    }
    case CLEAR_CONTEXT: {
      GrammarVMStackElement* stackTop = this->stack.pop();
      // check if both elements are of the type context
      if (!stackTop->is(CONTEXT_ELEMENT)) {
        throw LogError("gap-00225: Expected context element on stack.");
      }
      ContextGrammarVMStackElement* contextElement =
        dynamic_cast<ContextGrammarVMStackElement*> (stackTop);
      Log::instance()->debugMessage("CLEAR_CONTEXT " + str(boost::format(
        "%1%") % contextElement->getContext()));
      contextElement->getContext()->removeLocalChanges();
      delete stackTop;
      break;
    }
    case CLEAR_LOCAL_CONTEXT: {
      GrammarVMStackElement* stackTop = this->stack.pop();
      // check if both elements are of the type context
      if (!stackTop->is(CONTEXT_ELEMENT)) {
        throw LogError("gap-00225: Expected context element on stack.");
      }
      ContextGrammarVMStackElement* contextElement =
        dynamic_cast<ContextGrammarVMStackElement*> (stackTop);
      Log::instance()->debugMessage("CLEAR_LOCAL_CONTEXT " + str(
        boost::format("%1%") % contextElement->getContext()));
      contextElement->getContext()->removeLocallyAddedItems();
      delete stackTop;
      break;
    }
    case CREATE_EPSILON: {
      Log::instance()->debugMessage("CREATE_EPSILON");
      // Just create an epsilon on top of the stack.
      CFG::Epsilon* epsilon = new CFG::Epsilon();
      this->stack.push(new ProductionFragmentGrammarVMStackElement(epsilon));
      break;
    }
    case CREATE_TERMINAL: {
      CreateTerminalCommand* createTerminal =
        dynamic_cast<CreateTerminalCommand*> (cmd);
      Log::instance()->debugMessage("CREATE_TERMINAL");
      CFG::Terminal* terminal = new CFG::Terminal(
        createTerminal->getTerminalString());
      this->stack.push(new ProductionFragmentGrammarVMStackElement(terminal));
      break;
    }
    case CREATE_NONTERMINAL: {
      CreateNonTerminalCommand* createNonTerminal =
        dynamic_cast<CreateNonTerminalCommand*> (cmd);
      Log::instance()->debugMessage("CREATE_NONTERMINAL '" +
        *createNonTerminal->getNonTerminalName() + "'");
      CFG::NonTerminal* nonTerminal = new CFG::NonTerminal(
        createNonTerminal->getNonTerminalName());
      this->stack.push(new ProductionFragmentGrammarVMStackElement(
        nonTerminal));
      break;
    }
    case CREATE_PRODUCTION_FRAGMENT: {
      Log::instance()->debugMessage("CREATE_PRODUCTION_FRAGMENT");
      CreateProductionFragmentCommand* createProductionFragment =
        dynamic_cast<CreateProductionFragmentCommand*> (cmd);
      this->stack.push(new ProductionFragmentGrammarVMStackElement(
        createProductionFragment->getProductionFragment()));
      break;
    }
    case COMBINE: {
      Log::instance()->debugMessage("COMBINE");
      this->executeCombine();
      break;
    }
    case RETURN: {
      Log::instance()->debugMessage("RETURN");
      this->executeReturn();
      break;
    }
    case DUPLICATE: {
      Log::instance()->debugMessage("DUPLICATE");
      // Read the top element from the stack, and push a cloned version
      // back onto the stack.
      GrammarVMStackElement* topElement = this->stack.peek()->clone();
      this->stack.push(topElement);
      break;
    }
    case SWAP: {
      Log::instance()->debugMessage("SWAP");
      GrammarVMStackElement* fstElement = this->stack.pop();
      GrammarVMStackElement* sndElement = this->stack.pop();
      this->stack.push(fstElement);
      this->stack.push(sndElement);
      break;
    }
    case POP_STACK: {
      Log::instance()->debugMessage("POP_STACK");
      GrammarVMStackElement* stackTop = this->stack.pop();
      delete stackTop;
      break;
    }
    case CALL_FUNCTION: {
      CallFunctionCommand* callFunction = dynamic_cast<CallFunctionCommand*> (
        cmd);
      this->executeCallFunction(callFunction);
      break;
    }
    default: {
      throw LogError("gap-00150: Unhandled Grammar VM Command type (" + str(
        boost::format("%1%") % cmd->getType()) + ")");
    }
  }
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::execute(
  std::list<AmbiguityCFG::GrammarVMCommand*> cmds) {
  for (std::list<GrammarVMCommand*>::iterator i = cmds.begin();
       i != cmds.end(); ++i) {
    this->execute(*i);
  }
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::reset() {
  this->stack.reset();
}


CFG::Base* AmbiguityCFG::GrammarVMFunctionExecEnvironment::
  getResultProductionFragment() {
  // The result has been gathered while the algebra function
  // definition was processed. Now before we can return the
  // results. We need to create a suitable instance that contains
  // all results. If there is just a single result in the list, we
  // return that result, otherwise we combine all elements from
  // the list into a ProductionAlternative instance and return
  // a pointer to that instance.
  switch (this->functionResult.size()) {
    case 0: {
      CFG::Base* res = new CFG::Epsilon();
      return res;
      break;
    }
    case 1: {
      CFG::Base* res = this->functionResult.front();
      return res;
      break;
    }
    default: {
      CFG::Base* res = new CFG::ProductionAlternative(this->functionResult);
      return res;
    }
  }
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::executeLoadVar(
  AmbiguityCFG::LoadVarCommand* loadVarCmd) {
  // precondition: the top of stack must contain an element
  // that implements the NamedAccess-interface which will be
  // used in connection with the variable name of this command
  // to load the content of a variable onto the stack.
  GrammarVMStackElement* stackTop = this->stack.pop();
  switch (stackTop->getType()) {
    case NAMED_ACCESS_ELEMENT: {
      NamedAccessGrammarVMStackElement* namedAccess =
        dynamic_cast<NamedAccessGrammarVMStackElement*> (stackTop);
      // first check if the accessed element is stored in the
      // source instance
      if (!namedAccess->getNamedAccessElement()->containsVarInfoItem(
        loadVarCmd->getVariableName())) {
        throw LogError(
          "gap-00203: named access to an element that is not stored in that"
          " instance (accessed name: "
          + *loadVarCmd->getVariableName() + ")");
      }
      VarInfoItem* infoItem = namedAccess->getNamedAccessElement()
        ->getVarInfoItem(loadVarCmd->getVariableName());
      switch (infoItem->getType()) {
        case VAR_DECL_INFO: {
          VarDeclInfo* varDeclInfo = dynamic_cast<VarDeclInfo*> (infoItem);
          // //logging
          Log::instance()->debugMessage(
            "LOAD_VAR [" + *varDeclInfo->variableName + "]@" +
            str(boost::format("%1%") % namedAccess->getNamedAccessElement()) +
            " '" +
            *createStringForProductionFragment(varDeclInfo->productionFragment)
            + "'");
          // //////logging
          this->stack.push(new ProductionFragmentGrammarVMStackElement(
            varDeclInfo->productionFragment));
          break;
        }
        case MULTI_VAR_DECL_INFO: {
          MultiVarDeclInfo* multiVarInfo = dynamic_cast<MultiVarDeclInfo*> (
            infoItem);
          // Write all simultaneous fragments of the multi variable
          // info item into a vector, to be passed to the constructor
          // of the new stack element.
          std::vector<CFG::Base*> fragments;
          for (int i = 0; i < multiVarInfo->getDimension(); i++) {
            CFG::Base* fragment = extractProductionFragmentFromVarInfoItem(
              multiVarInfo->getVarInfoItemAt(i));
            fragments.push_back(fragment);
          }
          // //logging
          Log::instance()->debugMessage(
            "LOAD_VAR [" + *loadVarCmd->getVariableName() + "]@" +
            str(boost::format("%1%") % namedAccess->getNamedAccessElement()) +
            " '" + *createStringForProductionFragments(fragments) + "'");
          // //////logging
          // push the multiple fragments onto the stack.
          this->stack.push(new MultiProductionFragmentGrammarVMStackElement(
            fragments));
          break;
        }
        case RECORD_DECL_INFO:
        case MULTI_RECORD_DECL_INFO: {
          Log::instance()->debugMessage(
            "LOAD_VAR (Named-Access) [" + *loadVarCmd->getVariableName() +
            "]@" + str(boost::format("%1%") %
            namedAccess->getNamedAccessElement()));
          NamedAccess* namedAccess = dynamic_cast<NamedAccess*> (infoItem);
          this->stack.push(new NamedAccessGrammarVMStackElement(namedAccess));
          break;
        }
        default: {
          throw LogError(
            "gap-00202: unexpected VarDeclInfoItem as result from a load "
            "variable operation (type=)");
        }
      }
      break;
    }
    // At the moment unused; we do not push any instance of
    // the type VarDeclItemGrammarVMStackElement onto the stack.
    /*
    case VAR_DECL_INFO_ELEMENT: {
      VarDeclItemGrammarVMStackElement* infoElement = dynamic_cast
      <VarDeclItemGrammarVMStackElement*> (stackTop);
      this->stack.push (new ProductionFragmentGrammarVMStackElement (
      infoElement->getVarInfoItem()->productionFragment));
      break;
    }
    */
    default: {
      throw LogError(
        "gap-00201: LOAD_VAR needs on top of the stack either a named-access"
        " element or var-decl-info element.");
    }
  }

  // Free memory of the popped element from stack
  delete stackTop;
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::executeStoreVar(
  AmbiguityCFG::StoreVarCommand* storeVarCmd) {
  // two elements on top of the stack determine
  // 1) where the data is stored to
  // 2) what is stored
  // in addition the store-command contains a variable name
  // which exacts the slot where the data is stored into.
  GrammarVMStackElement* fstElement = this->stack.pop();
  GrammarVMStackElement* sndElement = this->stack.pop();
  // Now there are exactly two different cases of what the
  // combination of those two top elements can consist of:
  // First, the stored data is of type ProductionFragment
  // and the destination is of type NamedAccess.
  // Second, the stored data is of type NamedAccess, and
  // the destination also is of the same type.
  if (!sndElement->is(NAMED_ACCESS_ELEMENT)) {
    throw LogError(
      "gap-00204: destination of STORE_VAR is not of the required "
      "named-access type.");
  }
  NamedAccessGrammarVMStackElement* destination =
    dynamic_cast<NamedAccessGrammarVMStackElement*> (sndElement);

  switch (fstElement->getType()) {
    case PRODUCTION_FRAGMENT_ELEMENT: {
      std::string* variableName = storeVarCmd->getVariableName();
      ProductionFragmentGrammarVMStackElement* productionFragmentElement =
        dynamic_cast<ProductionFragmentGrammarVMStackElement*> (fstElement);
      VarDeclInfo* infoItem = new VarDeclInfo();
      infoItem->variableName = variableName;
      infoItem->productionFragment =
        productionFragmentElement->getProductionFragment();
      // //logging
      Log::instance()->debugMessage("STORE_VAR [" + *variableName + "]@" + str(
        boost::format("%1%") % destination->getNamedAccessElement()) + " '" +
        *createStringForProductionFragment(infoItem->productionFragment) + "'");
      // //////logging
      destination->getNamedAccessElement()->setVarInfoItem(
        variableName, infoItem);
      break;
    }
    case NAMED_ACCESS_ELEMENT: {
      throw LogError("gap-00205-: type of stored element not supported.");
      destination->getNamedAccessElement()->setVarInfoItem(
        storeVarCmd->getVariableName(), NULL);
      break;
    }
    case MULTI_PRODUCTION_FRAGMENT_ELEMENT: {
      std::string* variableName = storeVarCmd->getVariableName();
      MultiProductionFragmentGrammarVMStackElement* multiFragments =
        dynamic_cast<MultiProductionFragmentGrammarVMStackElement*> (
          fstElement);
      std::vector<CFG::Base*> fragments =
        multiFragments->getProductionFragments();
      MultiVarDeclInfo* multiVarDeclInfo = new MultiVarDeclInfo(
        variableName, fragments);
      // //logging
      Log::instance()->debugMessage("STORE_VAR [" + *variableName + "]@" + str(
        boost::format("%1%") % destination->getNamedAccessElement()) + " '" +
        *createStringForProductionFragments(fragments) + "'");
      // //////logging
      destination->getNamedAccessElement()->setVarInfoItem(
        variableName, multiVarDeclInfo);
      break;
    }
    default: {
      throw LogError("gap-00205: type of stored element not supported.");
    }
  }

  // Free the memory of both stack elements that are
  // no longer used:
  delete fstElement;
  delete sndElement;
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::executeCombine() {
  // Two elements on top of the stack must contain both operands
  // of type production fragment. Both fragments are combined
  // into a new result. The new Result is pushed onto the stack
  // again for further usage.
  // Please note that the two operands are stored on the stack
  // in the reverse order, that is the first operand to the combine
  // operation is the second element seen from top of the stack.
  GrammarVMStackElement* sndElement = this->stack.pop();
  GrammarVMStackElement* fstElement = this->stack.pop();

  // Only a few combinations of types are valid. Both elements are
  // required to be of one of two type, namely PRODUCTION_FRAGMENT_ELEMENT
  // or MULTI_PRODUCTION_FRAGMENT_ELEMENT. Depending on the respective
  // types, there are four ways to combine this two elements. The
  // following if-then-else structure handles all four cases and throws
  // an exception if none criteria is matched.
  if (fstElement->is(PRODUCTION_FRAGMENT_ELEMENT)) {
    if (sndElement->is(PRODUCTION_FRAGMENT_ELEMENT)) {
      // Now combine both sequences, and push a new production sequence
      // wrapper which contains the result onto the stack
      ProductionFragmentGrammarVMStackElement* fstProductionFragmentElement =
        dynamic_cast<ProductionFragmentGrammarVMStackElement*> (fstElement);
      ProductionFragmentGrammarVMStackElement* sndProductionFragmentElement =
        dynamic_cast<ProductionFragmentGrammarVMStackElement*> (sndElement);
      CFG::Base* combinedResult =
        fstProductionFragmentElement->getProductionFragment()->combine(
          sndProductionFragmentElement->getProductionFragment());
      this->stack.push(new ProductionFragmentGrammarVMStackElement(
        combinedResult));
    } else if (sndElement->is(MULTI_PRODUCTION_FRAGMENT_ELEMENT)) {
      ProductionFragmentGrammarVMStackElement* fstProductionFragmentElement =
        dynamic_cast<ProductionFragmentGrammarVMStackElement*> (fstElement);
      MultiProductionFragmentGrammarVMStackElement*
        sndMultiProductionFragmentElement =
          dynamic_cast<MultiProductionFragmentGrammarVMStackElement*> (
            sndElement);
      std::vector<CFG::Base*> resultFragments;
      CFG::Base* fstProductionFragment =
        fstProductionFragmentElement->getProductionFragment();
      std::vector<CFG::Base*> multiFragments =
        sndMultiProductionFragmentElement->getProductionFragments();
      for (unsigned int i = 0; i < multiFragments.size(); i++) {
        resultFragments.push_back(fstProductionFragment->combine(
          multiFragments[i]));
      }
      this->stack.push(new MultiProductionFragmentGrammarVMStackElement(
        resultFragments));
    } else {
      throw LogError(
        "gap-00207: production fragment expected on top of stack.");
    }
  } else if (fstElement->is(MULTI_PRODUCTION_FRAGMENT_ELEMENT)) {
    if (sndElement->is(PRODUCTION_FRAGMENT_ELEMENT)) {
      MultiProductionFragmentGrammarVMStackElement*
        fstMultiProductionFragmentElement =
          dynamic_cast<MultiProductionFragmentGrammarVMStackElement*> (
            fstElement);
      ProductionFragmentGrammarVMStackElement* sndProductionFragmentElement =
        dynamic_cast<ProductionFragmentGrammarVMStackElement*> (sndElement);
      std::vector<CFG::Base*> resultFragments;
      std::vector<CFG::Base*> multiFragments =
        fstMultiProductionFragmentElement->getProductionFragments();
      CFG::Base* sndProductionFragment =
        sndProductionFragmentElement->getProductionFragment();
      for (unsigned int i = 0; i < multiFragments.size(); i++) {
        resultFragments.push_back(multiFragments[i]->combine(
          sndProductionFragment));
      }
      this->stack.push(new MultiProductionFragmentGrammarVMStackElement(
        resultFragments));
    } else if (sndElement->is(MULTI_PRODUCTION_FRAGMENT_ELEMENT)) {
      MultiProductionFragmentGrammarVMStackElement*
        fstMultiProductionFragmentElement =
          dynamic_cast<MultiProductionFragmentGrammarVMStackElement*> (
            fstElement);
      MultiProductionFragmentGrammarVMStackElement*
        sndMultiProductionFragmentElement =
          dynamic_cast<MultiProductionFragmentGrammarVMStackElement*> (
            sndElement);
      std::vector<CFG::Base*> resultFragments;
      std::vector<CFG::Base*> fstMultiFragments =
        fstMultiProductionFragmentElement->getProductionFragments();
      std::vector<CFG::Base*> sndMultiFragments =
        sndMultiProductionFragmentElement->getProductionFragments();
      // Before we start, we have to check if both dimensions of the
      // simultaneous results are the same.
      if (fstMultiFragments.size() != sndMultiFragments.size()) {
        throw LogError("gap-00214: dimension mismatch of operands ()");
      }
      // Combine each element of the first vector with the corresponding
      // element of the second vector.
      for (unsigned int i = 0; i < fstMultiFragments.size(); i++) {
        CFG::Base* combinedResult = fstMultiFragments[i]->combine(
          sndMultiFragments[i]);
        resultFragments.push_back(combinedResult);
      }
      this->stack.push(new MultiProductionFragmentGrammarVMStackElement(
        resultFragments));
    } else {
      throw LogError(
        "gap-00207: production fragment expected on top of stack.");
    }
  } else {
    throw LogError("gap-00206: production fragment expected on top of stack.");
  }

  // Free some memory for all unused instances:
  delete fstElement;
  delete sndElement;
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::executeReturn() {
  // The top element of the stack is combined into the
  // current overall result of the algebra function result.
  GrammarVMStackElement* stackTop = this->stack.pop();
  switch (stackTop->getType()) {
    case PRODUCTION_FRAGMENT_ELEMENT: {
      ProductionFragmentGrammarVMStackElement* productionFragment =
        dynamic_cast<ProductionFragmentGrammarVMStackElement*> (stackTop);
      this->functionResult.push_back(productionFragment
        ->getProductionFragment());
      break;
    }
    case MULTI_PRODUCTION_FRAGMENT_ELEMENT: {
      MultiProductionFragmentGrammarVMStackElement* multiProductionFragments =
        dynamic_cast<MultiProductionFragmentGrammarVMStackElement*> (stackTop);
      std::vector<CFG::Base*> productionFragments = multiProductionFragments
        ->getProductionFragments();
      CFG::ProductionAlternative* productionAlt =
        new CFG::ProductionAlternative();
      for (unsigned int i = 0; i < productionFragments.size(); i++) {
        productionAlt->addAlternative(productionFragments[i]);
      }
      this->functionResult.push_back(productionAlt);
      break;
    }
    default: {
      throw LogError(
        "gap-00208: production fragment expected on top of stack.");
    }
  }
  // Free the memory of the stack element we just
  // popped from the stack's top
  delete stackTop;
}


void AmbiguityCFG::GrammarVMFunctionExecEnvironment::executeCallFunction(
  CallFunctionCommand* callFunction) {
  Log::instance()->debugMessage(
    "CALL_FUNCTION_COMMAND [" + *callFunction->getFunctionName() + "]");
  // The new function will have a variable context as initial
  // environment to start with, based on the values found on
  // the stack.
  MultiVariableContext* newContext = new MultiVariableContext(
    new VariableContext());
  // for each expected parameter, get an element form the stack.
  // Please note that we are working with a stack, thats why the
  // access to each parameter is in reverse order: the last
  // parameter of the function will be popped first from the stack.
  // std::list<std::string*> parameterNames = this->vm-
  // >getParameterNamesForFunction (callFunction->getFunctionName());
  std::list<std::string*> parameterNames = callFunction->getParameterNames();
  // unsigned int arity = callFunction->getArity();
  for (std::list<std::string*>::reverse_iterator i = parameterNames.rbegin();
       i != parameterNames.rend(); ++i) {
    std::string* parameterName = *i;
    GrammarVMStackElement* stackTop = this->stack.pop();
    switch (stackTop->getType()) {
      case PRODUCTION_FRAGMENT_ELEMENT: {
        ProductionFragmentGrammarVMStackElement* productionElement =
          dynamic_cast<ProductionFragmentGrammarVMStackElement*> (stackTop);
        VarDeclInfo* infoItem = new VarDeclInfo();
        infoItem->variableName = parameterName;
        infoItem->productionFragment =
          productionElement->getProductionFragment();
        newContext->setVarInfoItem(parameterName, infoItem);
        break;
      }
      case MULTI_PRODUCTION_FRAGMENT_ELEMENT: {
        MultiProductionFragmentGrammarVMStackElement* multiProductionElement =
          dynamic_cast<MultiProductionFragmentGrammarVMStackElement*> (
            stackTop);
        MultiVarDeclInfo* infoItem =
          new MultiVarDeclInfo(parameterName,
            multiProductionElement->getProductionFragments());
        newContext->setVarInfoItem(parameterName, infoItem);
        break;
      }
      default: {
        throw LogError(callFunction->getLocation(), "");
      }
    }
  }
  // Now we are ready to call the function.
  CFG::Base* functionResult = this->vm->executeFunction(
    callFunction->getFunctionName(), newContext);
  this->stack.push(new ProductionFragmentGrammarVMStackElement(functionResult));
}


CFG::Base* AmbiguityCFG::GrammarVMFunctionExecEnvironment::
  extractProductionFragmentFromVarInfoItem(VarInfoItem* infoItem) {
  if (!infoItem->is(VAR_DECL_INFO)) {
    throw LogError(
      "gap-00212: Can not extract production fragment form variable "
      "information item. Found incompatible subtype ");
  }
  VarDeclInfo* varDeclInfo = dynamic_cast<VarDeclInfo*> (infoItem);
  return varDeclInfo->productionFragment;
}


std::string* AmbiguityCFG::GrammarVMFunctionExecEnvironment::
  createStringForProductionFragment(CFG::Base* productionFragment) {
  std::ostringstream structureString;
  Printer::CFGPrettyPrint pp(structureString);
  pp.ppBase(NULL, productionFragment);
  return new std::string(structureString.str());
}


std::string* AmbiguityCFG::GrammarVMFunctionExecEnvironment::
  createStringForProductionFragments(std::vector<CFG::Base*> fragments) {
  std::ostringstream strStream;
  strStream << "[";
  for (unsigned int i = 0; i < fragments.size(); i++) {
    strStream << *createStringForProductionFragment(fragments[i]);
    if (i < fragments.size() - 1) {
      strStream << ", ";
    }
  }
  strStream << "]";
  return new std::string(strStream.str());
}
