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

#include "transform_gap_to_cfg.hh"

#include <list>
#include <string>
#include <boost/format.hpp>

#include "../const.hh"
#include "algebra_function_info_attribute.hh"
#include "grammar_vm_function_compiler.hh"
#include "parameter_position_attribute.hh"


AmbiguityCFG::GAPToCFGTransformer::GAPToCFGTransformer() {
  // Allocate memory for the grammar
  this->grammar = new CFG::CFG();
  this->currentlyProcessedGrammarRuleName = NULL;
}


AmbiguityCFG::GAPToCFGTransformer::~GAPToCFGTransformer() {
}


// Creates a string CFG for this grammar using the
// given canonical string grammar and the start symbol
// aka 'axiom' to start with.
CFG::CFG* AmbiguityCFG::GAPToCFGTransformer::generateCFG(Symbol::NT* axiom) {
  // Allocate memory for the grammar
  this->grammar = new CFG::CFG();

  // There must be an axiom!
  assert(axiom != NULL);

  // push the axiom as a non-terminal for which the grammar rule
  // is missing.
  this->nonTerminalsToGenerate.push(axiom);

  // now process all non-terminals as long as there are unprocessed
  // elements on the stack 'nonTerminalsToGenerate'
  generateProductions();

  // Return the result
  return this->grammar;
}


// Generates new productions as long as there are unprocessed
// non-terminals on the stack.
void AmbiguityCFG::GAPToCFGTransformer::generateProductions() {
  while (!this->nonTerminalsToGenerate.empty()) {
    // get one element from the stack...
    Symbol::NT *nonTerminal = this->nonTerminalsToGenerate.top();
    this->nonTerminalsToGenerate.pop();
    // ...then create a new grammar rule
    assert(nonTerminal != NULL);
    generateRule(nonTerminal);
  }
}


void AmbiguityCFG::GAPToCFGTransformer::generateRule(Symbol::NT *nt) {
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
  this->grammar->addProduction(grammarProduction);

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


// //////////////////////////////////////////////////////////////////
// //////////////////////////////////////////////////////////////////


// Dispatching method for subclasses of Symbol::Base.
CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
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
      "Symbol::Base: AmbiguityCFG::GAPToCFGTransformer::generateFragment"
      " (Symbol::Base *b)");
  }
}


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
  Symbol::NT *nt) {
  // The non-terminal we compute in terms of our own data structures:
  CFG::NonTerminal *nonTerminal = new CFG::NonTerminal(nt->name);

  // Check if the non-terminal's name is a name of a grammar
  // rule. If this is a valid grammar rule's name, push this
  // name on a stack of rules-to-produce non-terminals.
  if (!this->grammar->containsProduction(nonTerminal)) {
    this->nonTerminalsToGenerate.push(nt);
  }

  // just return the non-terminal
  return nonTerminal;
}


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
  Symbol::Terminal *terminal) {
  if (*terminal->name == "EMPTY") {
    // the parser EMPTY generates the empty word, which
    // is in our world Epsilon
    return new CFG::Epsilon();
  } else if (*terminal->name == "CHAR") {
    // Create a special non-terminal, which is a reference
    // to a regular expression. The output format we are
    // about to generate requires us to put angle brackets
    // around a name of a regular expression. We also need
    // to define a corresponding regular expression, which
    // is one of the standard builtin terminal parsers.
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
    "GAPToCFGTransformer::generateFragment (Symbol::Terminal *terminal)");
}


// //////////////////////////////////////////////////////////////////
// //////////////////////////////////////////////////////////////////


// This method is used as a proxy to route the generate fragment
// call to the appropriate implementation which handles each
// Alt-subtype accordingly.
CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
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
      "gap-00119: Internal: Unhandled subclass type of base class Alt::Base:"
      " AmbiguityCFG::GAPToCFGTransformer::generateFragment (Alt::Base *alt)");
  }
}


// Generates the grammar right-hand-sides for a simple algebra function
// application.
CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
  Alt::Simple *alt) {
  // get the name of the algebra function
  std::string *name = alt->name;

  // First we check if this is a use of a predefined gapc function
  // like CHAR (char c)....
  if (*name == "CHAR") {
    // Extract the argument of this terminal-parser and use it
    // as a terminal symbol for the regular expression.
    return new CFG::RegularExpression(
      new std::string("CHAR"), new std::string("X"));
  }

  // Next we need the list of actual arguments
  std::list<Fn_Arg::Base*> args = alt->args;

  // With the help of the definition of the algebra function
  // we are able to create a CFG fragment from the arguments
  // given to the algebra function
  return generateFunctionDefFragment(name, args);
}


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
  Alt::Block *alt) {
  CFG::ProductionAlternative* alts = new CFG::ProductionAlternative();
  for (std::list<Alt::Base*>::iterator i = alt->alts.begin();
       i != alt->alts.end(); ++i) {
    alts->addAlternative(generateFragment(*i));
  }
  return alts;
}


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
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
          "gap-00122: Unsupported expression in min- or maxsize filter "
          "application.");
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
            "gap-00121: Unsupported argument type to min- or maxsize filter "
            "application");
        }
      } else {
        throw LogError(
          "gap-00122: Unsupported expression in min- or maxsize filter "
          "application.");
      }

    } else {
      throw LogError(alt->location,
        "gap-00122: unsupported filter function '" + *filter->name + "'");
    }
  }

  // process the non-terminal or terminal symbol.
  CFG::Base* result = generateFragment(alt->nt);

  // If the result is a regular expression we will handle
  // the given min- and max-sizes.
  if (result->is(CFG::REGULAR_EXPRESSION)) {
    CFG::RegularExpression* regexp = dynamic_cast<CFG::RegularExpression*> (
      result);
    regexp->setBounds(bounds);
    // Also add this regular expression to the list of the
    // defined regular expressions. This can only be done at
    // this point because adding of the regexp relies on
    // the name, which in turn relies on the bounds for the
    // expression itself (if there is a grammar at all).
    if (this->grammar != NULL) {
      this->grammar->addRegularExpression(regexp);
    }
  }

  // create a grammar fragment for this non-terminal
  return result;
}


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
  Alt::Multi *alt) {
  // std::cout << "Alt::Multi" << std::endl;
  throw LogError(alt->location,
    "gap-00115: Internal: Not-implemented exception: AmbiguityCFG::"
    "GAPToCFGTransformer::generateFragment (Alt::Multi *alt)");
}


////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
  Fn_Arg::Base* arg) {
  switch (arg->getType()) {
    case Fn_Arg::CONST: {
      Fn_Arg::Const* cnst = dynamic_cast<Fn_Arg::Const*> (arg);
      return generateFragment(cnst);
    }
    case Fn_Arg::ALT: {
      Fn_Arg::Alt* alt = dynamic_cast<Fn_Arg::Alt*> (arg);
      return generateFragment(alt);
    }
    default: {
      throw LogError("gap-00389: Internal: unsupported Fn_Arg node type.");
    }
  }
}


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
  Fn_Arg::Alt* arg) {
  // Just process this kind of sub-node as a if it
  // is an Alt::Base instance.
  return generateFragment(arg->alt);
}


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFragment(
  Fn_Arg::Const* arg) {
  // We will create a terminal for the given constant expression,
  // but this depends heavily on the type of constant defined as
  // literal in the gap-source-code.
  // now we peal the constant expression out of its tree data structures
  Const::Base *constant = &arg->expr();
  if (constant->is(Const::CHAR)) {
    Const::Char *ch = dynamic_cast<Const::Char*> (constant);
    return new CFG::Terminal(new std::string(1, ch->c));
  } else if (constant->is(Const::STRING)) {
    Const::String *str = dynamic_cast<Const::String*> (constant);
    return new CFG::Terminal(str->s);
  } else if (constant->is(Const::INT)) {
    Const::Int *constInt = dynamic_cast<Const::Int*> (constant);
    return new CFG::Terminal(new std::string(str(
      boost::format("%1%") % constInt->i)));
  } else {
    throw LogError(constant->location,
      "gap-00116: Unsupported constant value type in parameter list of "
      "append-function call.");
  }
}


////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////


CFG::Base* AmbiguityCFG::GAPToCFGTransformer::generateFunctionDefFragment(
  std::string* functionName, std::list<Fn_Arg::Base*> &args) {
  // All parameters of the algebra function will be put into a
  // simple sequence instead of passing them on to a grammarVM
  // and calculate the outcome.
  CFG::ProductionSequence* cfgSequence = new CFG::ProductionSequence();

  // The zero-based position of a parameter. Inside of the loop
  // we use this variable as a position number of a algebra
  // function parameter.
  int variablePosition = 0;
  // Process all child nodes (i.g. arguments to the algebra function
  // application) before processing this algebra_function itself.
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i, variablePosition++) {
    CFG::Base* variableValue = generateFragment(*i);
    assert(variableValue != NULL);
    // Wrap the result into a base-wrapper, which marks a CFG expression
    // as belonging together.
    variableValue = new CFG::BaseWrapper(variableValue);
    variableValue->setAttribute(new Util::ParameterPositionAttribute(
      variablePosition));
    // just append the CFG node for the parameter to the sequence of
    // CFG nodes.
    cfgSequence->append(variableValue);
  }
  // Before this result is returned, we annotate the CFG node
  // with some information about the algebra function that
  // produced this result.
  CFG::Base* functionResult = new CFG::BaseWrapper(cfgSequence);
  Util::AlgebraFunctionInfoAttribute* functionInfoAttribute =
    new Util::AlgebraFunctionInfoAttribute();
  functionInfoAttribute->setAlgebraFunctionName(functionName);
  functionInfoAttribute->setGrammarRuleName(
    this->currentlyProcessedGrammarRuleName);
  functionInfoAttribute->setAlgebraFunctionArguments(args);
  functionResult->setAttribute(functionInfoAttribute);

  return functionResult;
}
