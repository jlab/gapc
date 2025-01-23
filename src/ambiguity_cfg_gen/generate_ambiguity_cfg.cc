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

#include "generate_ambiguity_cfg.hh"

#include <list>
#include <string>
#include <boost/format.hpp>

#include "../const.hh"
#include "algebra_function_info_attribute.hh"
#include "grammar_vm_function_compiler.hh"
#include "parameter_position_attribute.hh"
#include "regular_expression_info_attribute.hh"


AmbiguityCFG::GenerateAmbiguityCFG::GenerateAmbiguityCFG() {
  this->algebra = NULL;
  this->currentlyProcessedGrammarRuleName = NULL;
  this->grammar = NULL;
}


AmbiguityCFG::GenerateAmbiguityCFG::~GenerateAmbiguityCFG() {
}


// Creates a string CFG for this grammar using the
// given canonical string grammar and the start symbol
// aka 'axiom' to start with.
CFG::CFG* AmbiguityCFG::GenerateAmbiguityCFG::generateCFG(
  Algebra* canonical_algebra, Symbol::NT* axiom) {
  // Allocate memory for the grammar
  this->grammar = new CFG::CFG();

  assert(canonical_algebra != NULL);
  // prints out some kind of graph the grammar represents
  // print_dot (std::cout);
  this->algebra = canonical_algebra;

  // Create a compiler which will be used to transform the
  // algebra into grammarVM code.
  GrammarVMFunctionCompiler* grammarVMCompiler = new GrammarVMFunctionCompiler(
    this->algebra);

  // Before we process the grammar, we compile the
  // algebra into grammar VM code.
  for (hashtable<std::string, Fn_Def*>::iterator i = this->algebra->fns.begin();
       i != this->algebra->fns.end(); ++i) {
    // First prevent the GrammarVM compiler from processing
    // choice functions. Sometimes a choice function gets
    // mixed into the list of function definitions, sometimes
    // not. Especially if the choice function is not simply
    // pretty print, this can happen. Ask Georg why.
    if ((*i).second->is_Choice_Fn()) {
      continue;
    }
    // Call the grammar VM compiler for this algebra
    // function, and store the code in the VM.
    GrammarVMCode* code = grammarVMCompiler->processFnDef(
      new std::string((*i).first));
    this->grammarVM.addCompiledFunctionCode(code);
  }

  // Remmove the no longer needed garmmarVM compiler.
  delete grammarVMCompiler;

  // Check the algebra, if it is a pretty print algebra suitable
  // for ambiguity CFG generation:
  checkAlgebra(this->algebra);

  // There must be an axiom!
  assert(axiom != NULL);

  // push the axiom as a non-terminal for which the grammar rule
  // is missing.
  this->nonTerminalsToGenerate.push(axiom);

  // now process all non-terminals as long as there are unprocessed
  // elements on the stack 'nonTerminalsToGenerate'
  generateProductions();

  return grammar;
}


void AmbiguityCFG::GenerateAmbiguityCFG::checkAlgebra(Algebra* alg) {
  // Check if the choice function is of the required type
  checkChoiceFuntion(alg);
  // Check if the return type of the algebra is String
  checkReturnType(alg);
}


void AmbiguityCFG::GenerateAmbiguityCFG::checkChoiceFuntion(Algebra* alg) {
  // Assume we are not in pretty print mode with out algebra,
  // and try to find at least one choice function that is
  // of type PRETTY.
  bool prettyPrintMode = false;
  for (hashtable<std::string, Fn_Def*>::iterator i = alg->choice_fns.begin();
       i != alg->choice_fns.end(); ++i) {
    Fn_Def *f = i->second;
    if (f->choice_mode() == Mode::PRETTY) {
      prettyPrintMode = true;
      break;
    }
  }
  if (!prettyPrintMode) {
    Log::instance()->warning(
      alg->location,
      "gap-00133: This algebra choice funtion is not of type PRETTY. "
      "Is this a real pretty print algebra?");
  }
}


void AmbiguityCFG::GenerateAmbiguityCFG::checkReturnType(Algebra* alg) {
  bool allChoiceFunctionsAreOfTypeString = true;
  for (hashtable<std::string, Fn_Def*>::iterator i = alg->choice_fns.begin();
       i != alg->choice_fns.end(); ++i) {
    Fn_Def *f = i->second;
    Type::Base* choiceFnType = f->return_type;
    if (choiceFnType->is(Type::USAGE)) {
      Type::Usage* usage = dynamic_cast<Type::Usage*>(choiceFnType);
      choiceFnType = usage->base;
    }
    if (choiceFnType->is(Type::LIST)) {
      Type::List* list = dynamic_cast<Type::List*>(choiceFnType);
      Type::Base* listElementType = list->of;
      if (listElementType->is(Type::STRING)) {
        continue;
      } else if (listElementType->is(Type::EXTERNAL)) {
        Type::External* external = dynamic_cast<Type::External*>(
          listElementType);
        if (*external->name == "Rope") {
          continue;
        }
      }
    }
    allChoiceFunctionsAreOfTypeString = false;
  }
  if (!allChoiceFunctionsAreOfTypeString) {
    Log::instance()->warning(alg->location,
      "gap-00199: This algebra choice funtion has not the return type "
      "[String]. Is this a real pretty print algebra?");
  }
}


// Generates new productions as long as there are unprocessed
// non-terminals on the stack.
void AmbiguityCFG::GenerateAmbiguityCFG::generateProductions() {
  while (!this->nonTerminalsToGenerate.empty()) {
    // get one element from the stack...
    Symbol::NT *nonTerminal = this->nonTerminalsToGenerate.top();
    this->nonTerminalsToGenerate.pop();
    // ...then create a new grammar rule
    assert(nonTerminal != NULL);
    generateRule(nonTerminal);
  }
}


void AmbiguityCFG::GenerateAmbiguityCFG::generateRule(Symbol::NT *nt) {
  // The non-terminal we compute in terms of our own data structures:
  CFG::NonTerminal *nonTerminal = new CFG::NonTerminal(nt->name);

  // First check if the non-terminal has already been generated:
  if (grammar->containsProduction(nonTerminal)) {
    return;
  }

  // Store the name of this grammar as the current name. This field
  // is needed in the method 'generateFunctionDefFragment'.
  this->currentlyProcessedGrammarRuleName = nt->name;

  // Add the grammar production before processing it, because
  // otherwise it would be processed inititely through the
  // recursive calls in the for-loop while iterating through
  // all alternatives.
  CFG::GrammarProduction *grammarProduction = new CFG::GrammarProduction(
    nonTerminal);
  grammar->addProduction(grammarProduction);

  // a non-terminal consisting of a set of alternative productions
  // will naturally generate a list of grammar productions which
  // are merged in a production-alternative of a CFG.
  CFG::ProductionAlternative *productionAlternatives =
    new CFG::ProductionAlternative();

  // for each alternative on the right-hand-side of the non-terminal
  // definition of the gap-grammar we generate right-hand-side productions
  // of the CFG, and merge them into the production-alternative node
  // defined above.
  for (std::list<Alt::Base*>::iterator i = nt->alts.begin();
       i != nt->alts.end(); ++i) {
    productionAlternatives->addAlternative(generateFragment((*i)));
  }

  // Now add the list of alternatives to the grammar rule.
  grammarProduction->rhs = productionAlternatives;

  // When all structures have been created for the grammar rule,
  // we try to remove duplicate items:
  // grammarProduction->removeDuplicateAlternatives();
}


////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////


// Dispatching method for subclasses of Symbol::Base.
CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFragment(
  Symbol::Base *b) {
  if (b->is(Symbol::NONTERMINAL)) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*> (b);
    return generateFragment(nt);
  } else if (b->is(Symbol::TERMINAL)) {
    Symbol::Terminal *terminal = dynamic_cast<Symbol::Terminal*> (b);
    return generateFragment(terminal);
  } else {
    throw LogError(b->location,
      "gap-00117: Internal: Unhandled subclass type of base class "
      "Symbol::Base: AmbiguityCFG::GenerateAmbiguityCFG::generateFragment"
      " (Symbol::Base *b)");
  }
}


CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFragment(
  Symbol::NT *nt) {
  // The non-terminal we compute in terms of our own data structures:
  CFG::NonTerminal *nonTerminal = new CFG::NonTerminal(nt->name);

  // Check if the non-terminal's name is a name of a grammar
  // rule. If this is a valid grammar rule's name, push this
  // name on a stack of rules-to-produce non-terminals.
  if (!grammar->containsProduction(nonTerminal)) {
    this->nonTerminalsToGenerate.push(nt);
  }

  // just return the non-terminal
  return nonTerminal;
}


CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFragment(
  Symbol::Terminal *terminal) {
  if (*terminal->name == "EMPTY") {
    // the parser EMPTY generates the empty word, which
    // is in our world Epsilon
    return new CFG::Epsilon();
  } else if (*terminal->name == "LOC") {
    // this is a second way of parsing the empty word.
    return new CFG::Epsilon();
  } else if (*terminal->name == "CHAR") {
    // Create a special non-terminal, which is a reference
    // to a regular expression. The output format we are
    // about to generate requires us to put angle brackets
    // around a name of a regular expression. We also need
    // to define a corresponding regular expression, which
    // is one of the standart builtin terminal parsers.
    return new CFG::RegularExpression(
      new std::string("CHAR"), new std::string("."));
  } else if (*terminal->name == "BASE") {
    return new CFG::RegularExpression(
      new std::string("BASE"), new std::string("[acgtu]"));
  } else if (*terminal->name == "REGION") {
    return new CFG::RegularExpression(
      new std::string("REGION"), new std::string("[acgtu]*"));
  }

  throw LogError(terminal->location,
    "gap-00118: Internal: Not-implemented exception: AmbiguityCFG::"
    "GenerateAmbiguityCFG::generateFragment (Symbol::Terminal *terminal). "
    "Found terminal parser '" + *terminal->name + "'");
}


////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////


// This method is used as a proxy to route the generate fragment
// call to the appropriate implementation which handles each
// Alt-subtype accordingly.
CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFragment(
  Alt::Base* alt) {
  if (alt->is(Alt::SIMPLE)) {
    Alt::Simple *simple = dynamic_cast<Alt::Simple*>(alt);
    return generateFragment(simple);
  } else if (alt->is(Alt::LINK)) {
    Alt::Link *link = dynamic_cast<Alt::Link*>(alt);
    return generateFragment(link);
  } else if (alt->is(Alt::BLOCK)) {
    Alt::Block *block = dynamic_cast<Alt::Block*>(alt);
    return generateFragment(block);
  } else if (alt->is(Alt::MULTI)) {
    Alt::Multi *multi = dynamic_cast<Alt::Multi*>(alt);
    return generateFragment(multi);
  } else {
    throw LogError(alt->location,
      "gap-00119: Internal: Unhandled subclass type of base class Alt::Base: "
      "AmbiguityCFG::GenerateAmbiguityCFG::generateFragment (Alt::Base *alt)");
  }
}


// Generates the grammar right-hand-sides for a simple algebra function
// application.
CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFragment(
  Alt::Simple *alt) {
  // get the name of the algebra funtion
  std::string *name = alt->name;

  // First we check if this is a use of a predefined gapc function
  // like CHAR (char c)....
  if (*name == "CHAR") {
    // Extract the argument of this terminal-parser and use it
    // as a terminal symbol for the regular expression.
    return new CFG::RegularExpression(
      new std::string("CHAR"), new std::string("X"));
  } else if (this->algebra->fns.find(*name) == this->algebra->fns.end()) {
    // ...then we look up the algebra function definition from
    // the algebra using the name stored in Alt::Simple::name.
    throw LogError(
      alt->location, "gap-00120: Algebra function '" + *name +
      "' can not be found.");
  }
  Fn_Def *algebra_function = this->algebra->fns[*name];

  // Next we need the list of actual arguments
  std::list<Fn_Arg::Base*> args = alt->args;

  // With the help of the definition of the algebra function
  // we are able to create a CFG fragment from the arguments
  // given to the algebra function
  return generateFunctionDefFragment(name, algebra_function, args);
}


CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFragment(
  Alt::Block *alt) {
  CFG::ProductionAlternative* alts = new CFG::ProductionAlternative();
  for (std::list<Alt::Base*>::iterator i = alt->alts.begin();
       i != alt->alts.end(); ++i) {
    alts->addAlternative(generateFragment(*i));
  }
  return alts;
}


CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFragment(
  Alt::Link *alt) {
  // Handle the Alt::Link by processing the Symbol::Base reference
  // with the appropriate function via generateFragment (Symbol::Base *b)

  // Holds the bounds of the non-terminal of this Alt::Link
  // which arise through the application of a 'with'-clause.
  CFG::Bounds* bounds = new CFG::Bounds();

  // create boundary expressions for the known terminal
  // parsers CHAR, BASE and REGION
  for (std::list<Filter*>::iterator i = alt->filters.begin();
       i != alt->filters.end(); ++i) {
    Filter* filter = *i;
    if (filter->is(Filter::MIN_SIZE)) {
      std::list<Expr::Base*>::iterator j = filter->args.begin();
      if (j == filter->args.end()) {
        throw LogError("gap-00123: ");
      }

      // TODO(who?): change the program structure here! this
      // is duplicate code, see below!
      if ((*j)->is(Expr::CONST)) {
        Expr::Const* constantExpr = dynamic_cast<Expr::Const*> (*j);
        Const::Base* constant = constantExpr->base;
        if (constant->is(Const::INT)) {
          Const::Int* constInt = dynamic_cast<Const::Int*> (constant);
          bounds->setLowerBound(constInt->i);
        } else {
          throw LogError(
            "gap-00121: Unsupported argument type to min- or maxsize filter"
            " application");
        }
      } else {
        throw LogError(
          "gap-00122: Unsupported expression in min- or maxsize filter"
          " application.");
      }
    } else if (filter->is(Filter::MAX_SIZE)) {
      std::list<Expr::Base*>::iterator j = filter->args.begin();
      if (j == filter->args.end()) {
        throw LogError("gap-00124: ");
      }

      // TODO(who?): change the program structure here! this
      // is duplicate code, see above!
      if ((*j)->is(Expr::CONST)) {
        Expr::Const* constantExpr = dynamic_cast<Expr::Const*> (*j);
        Const::Base* constant = constantExpr->base;
        if (constant->is(Const::INT)) {
          Const::Int* constInt = dynamic_cast<Const::Int*> (constant);
          bounds->setUpperBound(constInt->i);
        } else {
          throw LogError(
            "gap-00121: Unsupported argument type to min- or maxsize filter"
            " application");
        }
      } else {
        throw LogError(
          "gap-00122: Unsupported expression in min- or maxsize filter"
          " application.");
      }

    } else {
      throw LogError(
        alt->location,
        "gap-00122: unsupported filter function '" + *filter->name + "'");
    }
  }

  // process the non-terminal or terminal symbol.
  CFG::Base* result = generateFragment(alt->nt);
  // create a wrapper also for this result? This wrapper could
  // receive annotations. At the moment this is not needed.
  // result = new CFG::BaseWrapper (result);

  // If the result is a regular expression we will handle
  // the given min- and max-sizes.
  if (result->is(CFG::REGULAR_EXPRESSION)) {
    CFG::RegularExpression* regexp =
      dynamic_cast<CFG::RegularExpression*> (result);
    regexp->setBounds(bounds);
    // Annotate the regular expression with an attribute, which helps
    // us identify the original AST structure which led to that
    // expression.
    regexp->setAttribute(new Util::RegularExpressionInfoAttribute(alt));
    // Also add this regular expression to the list of the
    // defined regular expressions. This can only be done at
    // this point because the adding the regexp relies on
    // the name, which in turn relies on the bounds for the
    // expression itself.
    grammar->addRegularExpression(regexp);
  }

  // create a grammar fragment for this non-terminal
  return result;
}


CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFragment(
  Alt::Multi *alt) {
  // std::cout << "Alt::Multi" << std::endl;
  throw LogError(alt->location,
    "gap-00115: Internal: Not-implemented exception: AmbiguityCFG::"
    "GenerateAmbiguityCFG::generateFragment (Alt::Multi *alt)");
}


////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////


CFG::Base* AmbiguityCFG::GenerateAmbiguityCFG::generateFunctionDefFragment(
  std::string* functionName, Fn_Def *algebra_function,
  std::list<Fn_Arg::Base*> &args) {
  // The list of arguments needs to be converted from expression
  // trees to lists of grammar fragments, hence a list of lists
  // is needed here.
  // hashtable<std::string, Base*> processedArgs;

  // Get the list of algebra function parameter names.
  std::list<std::string*> parameterNames = algebra_function->names;

  // The context for that contains all parameters and their
  // current values, will be filled by the for-loop a few
  // lines below.
  VariableContext* context = new VariableContext();

  // The zero-based position of a parameter. Inside of the loop
  // we use this variable as a position number of a algebra
  // function parameter.
  int variablePosition = 0;
  // Process all child nodes (i.g. arguments to the algebra function
  // application) before processing this algebra_function itself.
  std::list<Fn_Arg::Base*>::iterator i;
  std::list<std::string*>::iterator j;
  for (i = args.begin(), j = parameterNames.begin();
       i != args.end(); ++i, ++j, variablePosition++) {
    CFG::Base* variableValue = NULL;
    // we iterate through two lists in parallel, which
    // bears the risk of inconsistent data content. This
    // code bases on the assumption that both lists have
    // the same number of arguments, thus we 'assert':
    assert(j != parameterNames.end());
    std::string *parameterName = *j;
    if ((*i)->is(Fn_Arg::CONST)) {
      // We will create a terminal for the given constant expression,
      // but this depends heavily on the type of constant defined as
      // literal in the gap-source-code.
      // now we peal the constant expression out of its tree data structures
      Fn_Arg::Const *cnst = dynamic_cast<Fn_Arg::Const*> (*i);
      Const::Base *constant = &cnst->expr();
      if (constant->is(Const::CHAR)) {
        Const::Char *ch = dynamic_cast<Const::Char*> (constant);
        variableValue = new CFG::Terminal(new std::string(1, ch->c));
      } else if (constant->is(Const::STRING)) {
        Const::String *str = dynamic_cast<Const::String*>(constant);
        variableValue = new CFG::Terminal(str->s);
      } else if (constant->is(Const::INT)) {
        Const::Int *constInt = dynamic_cast<Const::Int*>(constant);
        variableValue = new CFG::Terminal(new std::string(str(boost::format(
          "%1%") % constInt->i)));
      } else {
        throw LogError(constant->location,
          "gap-00116: Unsupported constant value type in parameter list of "
          "append-function call.");
      }
    } else if ((*i)->is(Fn_Arg::ALT)) {
      Fn_Arg::Alt *alt = dynamic_cast<Fn_Arg::Alt*> (*i);
      variableValue = generateFragment(alt->alt);
    } else {
      throw LogError((*i)->location,
        "gap-00117: Unhandled subclass type of base class Fn_Arg::Base: "
        "AmbiguityCFG::GenerateAmbiguityCFG::generateFunctionDefFragment "
        "(Fn_Def *algebra_function, std::list<Fn_Arg::Base*> &args)");
    }
    assert(variableValue != NULL);
    // Wrap the result into a base-wrapper, which marks a CFG expression
    // as belonging together.
    variableValue = new CFG::BaseWrapper(variableValue);
    variableValue->setAttribute(new Util::ParameterPositionAttribute(
      variablePosition));
    // Now create a container for the current parameter value.
    VarDeclInfo* infoItem = new VarDeclInfo();
    infoItem->variableName = parameterName;
    infoItem->productionFragment = variableValue;
    infoItem->parameterPosition = variablePosition;
    context->setVarInfoItem(parameterName, infoItem);
  }
  // Execute the algebra function in the grammarVM, with the
  // context that contains all parameter values.
  CFG::Base* functionResult = this->grammarVM.executeFunction(
    functionName, context);
  // Before this result is returned, we annotate the CFG node
  // with some information about the algebra function that
  // produced this result.
  functionResult = new CFG::BaseWrapper(functionResult);
  Util::AlgebraFunctionInfoAttribute* functionInfoAttribute =
    new Util::AlgebraFunctionInfoAttribute();
  functionInfoAttribute->setAlgebraFunctionName(functionName);
  functionInfoAttribute->setGrammarRuleName(
    this->currentlyProcessedGrammarRuleName);
  functionInfoAttribute->setAlgebraFunctionDefinition(algebra_function);
  functionInfoAttribute->setAlgebraFunctionArguments(args);
  functionResult->setAttribute(functionInfoAttribute);

  return functionResult;
}
