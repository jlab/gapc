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

#include <vector>
#include <list>
#include <string>
#include <utility>
#include <iostream>

#include "create_specialized_grammar.hh"


#include "boost/format.hpp"

#include "../ambiguity_cfg_gen/generate_ambiguity_cfg.hh"
#include "../arg.hh"
#include "../cfg/remove_unused_productions.hh"
#include "../const.hh"
#include "../instance.hh"
#include "../loc.hh"
#include "../log.hh"
#include "../para_decl.hh"
#include "../product.hh"
#include "../util/algebra_function_name_attribute.hh"
#include "../util/annotate_algebra_function_names.hh"
#include "../util/annotate_cycles.hh"
#include "../util/annotate_dead_ends.hh"
#include "../util/annotate_reducible_attributes.hh"
#include "../util/annotate_the_set_first.hh"
#include "../util/remove_all_attributes.hh"
#include "../util/remove_cycle_set_attributes.hh"
#include "../util/remove_first_set_attributes.hh"
#include "add_special_axiom_to_cfg.hh"
#include "remove_cfg_cycles.hh"
#include "rewrite_non_productive_cfg_rules.hh"

#include "../printer/cfg_pretty_print_cout.hh"


const char SpecializeGrammar::CreateSpecializedGrammar::
  theAlgebraRuntimeIncludeFileName[] = "rtlib/rules.hh";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theAlgebraAnswerTypeDefName[] = "answer_type";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theAlgebraStringTypeDefName[] = "string_type";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theAlphabetStringTypeDefName[] = "Rope";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theAlgebraResultVariableName[] = "result";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theSignatureName[] = "sig";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theAlgebraName[] = "alg";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theGrammarName[] = "grmmr";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theAxiomName[] = "axiom";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theChoiceFunctionName[] = "h";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theMergeRulesFunctionName[] = "merge";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theResultShapeFieldName[] = "shape";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theGetRuleNameFunctionName[] = "getRuleName";
const char SpecializeGrammar::CreateSpecializedGrammar::
  theRopeStringMatchingFilterName[] = "matchString";


SpecializeGrammar::CreateSpecializedGrammar::CreateSpecializedGrammar()
  : sourceProgramSignatureName(NULL), ast(NULL), signature(NULL),
    currentGrammarProductionName(NULL), algebraFunctionInfoAttribute(NULL),
    hiddenGrammarRuleFragments(NULL), cyclePathInformation(NULL) {
}


SpecializeGrammar::CreateSpecializedGrammar::~CreateSpecializedGrammar() {
}


AST* SpecializeGrammar::CreateSpecializedGrammar::createGrammar(
  AST* sourceProgram, std::string* instanceName) {
  // look up the instance that gives us the name of the
  // grammar and the algebra which creates the chape strings.
  Instance* instance = sourceProgram->instance(*instanceName);

  // check if the named instance is present in the gap-program
  if (instance == NULL) {
    throw LogError(
      "No instance with name '" + *instanceName +
      "' defined in the source code.");
  }

  // get the selected product of the instance and extract
  // the algebra from it. The product itself has a accessor
  // method which returns the algebra.
  Product::Base *product = instance->product;
  assert(product != NULL);

  // the algebra must be a simple single algebra, not any
  // kind of product
  if (!product->is(Product::SINGLE)) {
    throw LogError(
      instance->loc(),
      "For ambiguity checking it is required to use an instance that uses not"
      " a product of algebras, but simply a single algebra.");
  }

  Algebra* canonical_algebra = product->algebra();
  Grammar* grammar = instance->grammar();

  // Create a cfg generator, and start with processing the
  // AST and generating a CFG output file.
  AmbiguityCFG::GenerateAmbiguityCFG generator;
  CFG::CFG* cfg = generator.generateCFG(canonical_algebra, grammar->axiom);

  // do some annotation first, before we can start to generate
  // the gap program output.

  // std::cout << "annotate FIRST in grammar" << std::endl;
  Util::AnnotateTheSetFirst theSetFirstAnnotator;
  theSetFirstAnnotator.annotateGrammar(cfg);

  Util::AnnotateCycles cycleAnnotator;
  cycleAnnotator.annotateGrammar(cfg);



  // Rewrite the CFG into an equivalent grammar whose
  // grammar productions will be
  //
  RewriteNonProductiveCFGRules rewriteNonProductiveRule;
  // cfg = rewriteNonProductiveRule.rewriteGrammar(cfg);

  // CFG::UnusedProductionsRemover unusedProductionRemover;
  // cfg = unusedProductionRemover.removeUnusedProductions(cfg);

  // Restart all annotations again, we have changed the grammar
  // too much.
  Util::RemoveCycleSetAttributes removeCycleSetAttributes;
  removeCycleSetAttributes.removeFromGrammar(cfg);
  Util::RemoveFirstSetAttributes removeFirstSetAttributes;
  removeFirstSetAttributes.removeFromGrammar(cfg);

  // The two old ones again:
  theSetFirstAnnotator.annotateGrammar(cfg);
  cycleAnnotator.annotateGrammar(cfg);


  // Annotates the grammar with DeadEndAttributes.
  Util::AnnotateDeadEnds deadEndAnnotator;
  deadEndAnnotator.annotateGrammar(cfg);

  // Annotate the ReducibleElementAttribute to the grammar.
  Util::ReducibleAttributeAnnotator reduceAttributeAnnotator;
  reduceAttributeAnnotator.annotateGrammar(cfg);

  // ...then transform it into a cycle-free grammar:
  RemoveCFGCycles cycleRemover;
  cfg = cycleRemover.removeCycles(cfg);

  // Annotate new algebra function names, at least after the
  // cycles have been removed, because that step introduces
  // new rules, which originate form common rules, hence all
  // algebra function names would double or triple for any
  // grammar rule being the original.
  Util::AlgebraFunctionNameAnnotator functionNameAnnotator;
  functionNameAnnotator.annotateGrammar(cfg);

  // After all transformations have been done, we need
  // to add two more rules to the CFG graph, one of them
  // replacing the axiom, the other connecting the
  // new axiom-rule with the former axiom of the grammar.
  AddSpecialAxiomToCFG specialAxiomAdder;
  specialAxiomAdder.addSpecialAxiom(cfg);



  //////////////////////////////////////////////////////
  //////////////////////////////////////////////////////
  // Now we have a CFG which must be transformed into a
  // GAP grammar of a type that is compatible with the
  // AST of gapc.
  //////////////////////////////////////////////////////
  //////////////////////////////////////////////////////

  // AST* ast = new AST();
  this->ast = new AST();

  // Store the name of the signature of the source program,
  // which will be used by the resulting shape parser program
  // to generate an output grammar which uses the correct
  // signature name. This will facilitate incorporating the
  // output into a new running program.
  this->sourceProgramSignatureName = sourceProgram->signature->name;

  Grammar* gapGrammar = createGrammar(ast, cfg);
  // Do not delete this list, it is used in the AST as
  // list structure for all grammars.
  std::list<Grammar*>* grammarList = new std::list<Grammar*>();
  grammarList->push_back(gapGrammar);

  // Set the input type: character based input, we parse shape strings.
  Loc location;  // dummy location
  // NOTE: magic constant 'raw' taken from "../input.cc" as one of the
  // valid input type strings.
  this->ast->input.set("raw", location);
  // Set an import statement for the runtime environment
  // of the algebra functions.
  this->ast->imports.push_back(new Import(new std::string(
    theAlgebraRuntimeIncludeFileName), true, location));
  // And we need some typedefs in our gap-shape-parser, which
  // will make our code compatible to the grammar-env.hh classes
  // and methods.
  createTypeDefinitions(this->ast);
  // Add the algebra to the AST
  this->ast->algebras[theAlgebraName] = createAlgebra();
  // Set the grammars (in fact this is a list with only one
  // grammar at the moment).
  this->ast->set_grammars(grammarList);
  // While the grammar has been created, we filled our instance
  // variable 'signatureDecls', now is the time we add this
  // structure to the AST.
  this->signature->setDecls(this->signatureDecls);
  // The signature of the GAP program is similar to the signatureDecls.
  // This structure simply names all signature type arguments, usually
  // 'alphabet' and 'answer'.
  this->ast->signature = this->signature;
  // We define a simple instance for convenience of the user. This
  // instance simply combines our only grammar with our only algebra.
  // Pretty straight forward.
  Instance* defaultInstance = createDefaultInstance(gapGrammar);
  this->ast->instances[*defaultInstance->name()] = defaultInstance;

  return ast;
}


void SpecializeGrammar::CreateSpecializedGrammar::createTypeDefinitions(
  AST* ast) {
  Loc location;
  ast->add_type(new std::string(theAlgebraAnswerTypeDefName),
    location, new Type::External(theAlgebraAnswerTypeDefName));
  ast->add_type(new std::string(theAlgebraStringTypeDefName),
    location, new Type::External(theAlgebraStringTypeDefName));
  ast->add_type(new std::string(theAlphabetStringTypeDefName),
    location, new Type::External(theAlphabetStringTypeDefName));
  // Example, if this should be a type definition with internal
  // types and type names.
  // ast->add_type (new std::string ("gap_type_name"), location, new
  // Type::Def (new std::string ("gap_type_name"), location, new Type::Char()));
}


Grammar* SpecializeGrammar::CreateSpecializedGrammar::createGrammar(
  AST* ast, CFG::CFG* grammar) {
  std::string* grammarName = new std::string(theGrammarName);
  std::string* signatureName = new std::string(theSignatureName);
  std::string* axiomName = grammar->getAxiom()->getName();

  // Dummy-location needed as parameter in various structures
  // of the AST:
  Loc location;
  // The grammar structure: this is where we put our
  // grammar productions.
  Grammar* gapGrammar = new Grammar(
    *ast, grammarName, signatureName, axiomName, location);

  this->signature = new Signature(signatureName, location);
  hashtable <std::string, Arg*> signatureArgs;
  signatureArgs["alphabet"] = new Arg(new std::string("alphabet"), location);
  signatureArgs["answer"] = new Arg(new std::string("answer"), location);
  this->signature->args = signatureArgs;

  // the parser.y calls
  // Signature *s = new Signature(sig_name, @1 + @2)
  // driver.ast.add_sig_types(sig_args, s);


  std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
  for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin();
       i != productions.end(); i++) {
    this->hiddenGrammarRuleFragments = getHiddenCFGFragmentsAttribute(*i);
    this->cyclePathInformation = getCyclePathInformationAttribute(*i);
    this->currentGrammarProductionName =(*i)->lhs->getName();
    Symbol::NT* nt = processProduction(*i);
    gapGrammar->add_nt(nt);
    this->hiddenGrammarRuleFragments = NULL;
    this->cyclePathInformation = NULL;
    this->currentGrammarProductionName = NULL;
  }


  return gapGrammar;
}


Symbol::NT* SpecializeGrammar::CreateSpecializedGrammar::processProduction(
  CFG::GrammarProduction* production) {
  // Dummy location
  Loc location;

  assert(production != NULL);

  // The non-terminal this production is defining:
  Symbol::NT* nt = new Symbol::NT(production->lhs->getName(), location);
  nt->set_alts(*processProductionAlternative(production->rhs));

  // Watch out for a choice-function-application attribute, which
  // defines the choice function for the current grammar production,
  // or none if NULL
  ChoiceFunctionApplicationAttribute* choiceFunctionApplicationAttribute =
    getChoiceFunctionApplicationAttribute(production);
  if (choiceFunctionApplicationAttribute != NULL) {
    nt->eval_fn = choiceFunctionApplicationAttribute->getChoiceFunctionName();
  }

  return nt;
}


std::list<Alt::Base*>* SpecializeGrammar::CreateSpecializedGrammar::
  processProductionAlternative(CFG::ProductionAlternative* alt) {
  return processProductionAlternative(NULL, alt);
}


std::list<Alt::Base*>* SpecializeGrammar::CreateSpecializedGrammar::
  processProductionAlternative(
    Util::Attributable* infoContext, CFG::ProductionAlternative* alt) {
  std::list<Alt::Base*>* altList = new std::list<Alt::Base*>();

  for (CFG::ProductionAlternative::iterator i = alt->begin();
       i != alt->end(); i++) {
    std::pair<Alt::Base*, AlgebraParameterTypes> result =
      processProductionFragment(infoContext, *i);
    altList->push_back(result.first);
  }

  return altList;
}


std::pair<Alt::Base*, SpecializeGrammar::CreateSpecializedGrammar::
  AlgebraParameterTypes> SpecializeGrammar::CreateSpecializedGrammar::
  processProductionFragment(Util::Attributable* infoContext, CFG::Base* b) {
  // Dummy location used by the AST structures.
  Loc location;
  // The AST result structure for the given CFG node.
  Alt::Base* rhsResult = NULL;
  // The name of the algebra function which is applied to the CFG node.
  std::string* algebraFunctionName = getAlgebraFunctionName(b);
  // The result type of the AST structure.
  AlgebraParameterTypes resultType = VOID;

  std::cout << "+processProductionFragment+" << std::endl;
  if (algebraFunctionName != NULL) {
    std::cout << "got an algebra function name " <<
      *algebraFunctionName << std::endl;
  }

  // Depending on the type of the CFG node, we create an
  // appropriate AST structure:
  switch (b->getType()) {
    case CFG::BASE_WRAPPER: {
      std::cout << "+BASE_WRAPPER+ "; Printer::PrettyPrintCOut pp; pp.ppBase(
        NULL, b); std::cout << std::endl;
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*>(b);
      Util::AlgebraFunctionInfoAttribute* oldAlgebraFunctionInfoAttribute =
        this->algebraFunctionInfoAttribute;
      Util::AlgebraFunctionInfoAttribute* attribute =
        getAlgebraFunctionInfoAttribute(wrapper);
      std::cout << "going through a wrapper"
        << (algebraFunctionName == NULL ? "" : *algebraFunctionName)
        << std::endl;
      if (attribute != NULL) {
        std::cout << "setting a new function info attribute" << std::endl;
        this->algebraFunctionInfoAttribute = attribute;
      }

      // OMG, once again a special case handled here!
      // If this is a wrapped non-terminal, just create directly an algebra
      // function for it. We need to do this here, because the wrapper
      // itself is needed as the root node for the createAlgebraFunction
      // call. Also the algebra function name must be extracted in an
      // extra step, because now we do not have the usual recursive call
      // which extracts that name from the non-terminal.
      if (wrapper->getWrappedBase()->is(CFG::NONTERMINAL)) {
        std::cout << "special wrapper handling" << std::endl;
        // reload the algebra function name
        CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*>(
          wrapper->getWrappedBase());
        algebraFunctionName = getAlgebraFunctionName(nonTerminal);
        // This type of node will be transformed into an Alt::Link
        // node

        Alt::Base* lnk = new Alt::Link(nonTerminal->getName(), location);
        rhsResult = lnk;

        // Add an other method declaration to the signature:
        if (algebraFunctionName != NULL) {
          std::cout << "special wrapper handling ++" << std::endl;
          rhsResult = createAlgebraFunctionCallWrapper(
            algebraFunctionName, lnk);
          createAlgebraFunction(
            infoContext, algebraFunctionName, ANSWER, wrapper);
        }

        resultType = ANSWER;

        break;
      }
      std::cout << "normal wrapper handling" << std::endl;
      std::pair<Alt::Base*, SpecializeGrammar::CreateSpecializedGrammar::
        AlgebraParameterTypes> result = processProductionFragment(
          wrapper, wrapper->getWrappedBase());
      this->algebraFunctionInfoAttribute = oldAlgebraFunctionInfoAttribute;
      return result;
    }
    case CFG::EPSILON: {
      std::cout << "+EPSILON+ "; Printer::PrettyPrintCOut pp; pp.ppBase(
        NULL, b); std::cout << std::endl;
      // This type will be transformed into an Alt::Simple node
      // which has no arguments. The function this node will call
      // is the builtin-function EMPTY.
      Alt::Link* link = new Alt::Link(new std::string("EMPTY"), location);
      rhsResult = link;

      // Add an other method declaration to the signature:
      if (algebraFunctionName != NULL) {
        rhsResult = createAlgebraFunctionCallWrapper(algebraFunctionName, link);
        createAlgebraFunction(infoContext, algebraFunctionName, VOID, b);

        resultType = ANSWER;
      } else {
        resultType = ALPHABET;
      }

      break;
    }
    case CFG::TERMINAL: {
      std::cout << "+TERMINAL+ "; Printer::PrettyPrintCOut pp; pp.ppBase(
        NULL, b); std::cout << std::endl;
      CFG::Terminal* terminal = dynamic_cast<CFG::Terminal*>(b);

      // This type will be transformed into an Alt::Simple node
      // which resembles a terminal parser call. The terminal-parsers
      // we create here are only character parsers, since shape-strings
      // in general can be of an arbitrary character type, while
      // narrowing the character set of the generated gap-program
      // would gain us nothing.
      Alt::Simple* simple = new Alt::Simple(new std::string("CHAR"), location);
      rhsResult = simple;

      std::string* terminalValue = terminal->getValue();
      if (terminalValue->size() == 1) {
        Alt::Simple* simple = new Alt::Simple(
          new std::string("CHAR"), location);
        Fn_Arg::Const* parameter = new Fn_Arg::Const(
          new ::Const::Char(terminalValue->at(0)), location, false);
        // This is the list of arguments we pass to the GAP AST algebra
        // function application.
        std::list<Fn_Arg::Base*> args;
        args.push_back(parameter);
        simple->args = args;
        rhsResult = simple;
        resultType = ALPHABET;
      } else {
        Alt::Simple* simple = new Alt::Simple(
          new std::string("ROPE"), location);
        // add a filter to the ROPE parser, because match with a
        // constant string
        Filter* filter = new Filter(new std::string(
          theRopeStringMatchingFilterName), location);
        filter->type = Filter::WITH;
        std::list<Expr::Base*> filterArgs;
        filterArgs.push_back(new Expr::Const(new ::Const::String(
          *terminalValue)));
        filter->args = filterArgs;
        simple->filters.push_back(filter);
        rhsResult = simple;
        resultType = ALPHABET_STRING;
      }

      // Add an other method declaration to the signature:
      if (algebraFunctionName != NULL) {
        rhsResult = createAlgebraFunctionCallWrapper(
          algebraFunctionName, rhsResult);
        createAlgebraFunction(infoContext, algebraFunctionName, ALPHABET, b);

        resultType = ANSWER;
      }

      break;
    }
    case CFG::NONTERMINAL: {
      std::cout << "+NONTERMINAL+ "; Printer::PrettyPrintCOut pp;
        pp.ppBase(NULL, b); std::cout << std::endl;
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*>(b);
      // This type of node will be transformed into an Alt::Link
      // node

      Alt::Base* lnk = new Alt::Link(nonTerminal->getName(), location);
      rhsResult = lnk;

      // Add an other method declaration to the signature. This case
      // happens only if this is a non-terminal which had no algebra-function
      // applied to it in the (original) prototype grammar.
      if (algebraFunctionName != NULL) {
        rhsResult = createAlgebraFunctionCallWrapper(algebraFunctionName, lnk);
        createAlgebraFunction(infoContext, algebraFunctionName, ANSWER, b);
      }

      resultType = ANSWER;

      break;
    }
    case CFG::REGULAR_EXPRESSION: {
      std::cout << "+REGULAR_EXPRESSION+ "; Printer::PrettyPrintCOut pp;
        pp.ppBase(NULL, b); std::cout << std::endl;
      CFG::RegularExpression* regularExpression =
        dynamic_cast<CFG::RegularExpression*>(b);
      // each regular expression comes with an attribute that holds
      // a reference to the original GAP AST structure which translated
      // into this regular expression. We translate the regular
      // expression back into an AST structure by using the original
      // structure.
      Util::RegularExpressionInfoAttribute* regexpAttribute =
        getRegularExpressionInfoAttribute(regularExpression);
      assert(regexpAttribute != NULL);
      rhsResult = regexpAttribute->getBaseExpression();
      // This type will be transformed into an Alt::Simple node
      // where its arguments are of type Fn_Arg::Const, because
      // terminal parsers only have constants as their parameters.
      // Alt::Simple* simple = new Alt::Simple (
      //   new std::string ("CHAR"), location);
      // rhsResult = simple;

      // Add an other method declaration to the signature:
      if (algebraFunctionName != NULL) {
        rhsResult = createAlgebraFunctionCallWrapper(
          algebraFunctionName, rhsResult);
        createAlgebraFunction(infoContext, algebraFunctionName, VOID, b);

        resultType = ANSWER;
      } else {
        resultType = ALPHABET;
      }

      break;
    }
    case CFG::PRODUCTION_SEQUENCE: {
      std::cout << "+PRODUCTION_SEQUENCE+ "; Printer::PrettyPrintCOut pp;
        pp.ppBase(NULL, b); std::cout << std::endl;
      CFG::ProductionSequence* productionSequence =
        dynamic_cast<CFG::ProductionSequence*>(b);
      // This type will be transformed into a Alt::Simple node.
      // First we transform all elements of the sequence into
      // Fn_Arg instances, before we put them as arguments into
      // the new AST node.

      // This is for the grammar function call structure:
      std::list<Fn_Arg::Base*> args;
      // This is for the signature function declaration:
      std::list<AlgebraParameterTypes> parameterTypes;
      // This is a list of all CFG nodes that are passed as parameters
      // to the algebra function of the shape parser.
      std::list<CFG::Base*> parameterValues;
      // Now process all elements of the sequence.
      for (int i = 0; i < productionSequence->getSize(); i++) {
        std::pair<Alt::Base*, AlgebraParameterTypes> argumentResult =
          processProductionFragment(
            infoContext, productionSequence->elementAt(i));
        Alt::Base* argument = argumentResult.first;
        Fn_Arg::Alt* altArgument = new Fn_Arg::Alt(argument, location);
        args.push_back(altArgument);
        parameterTypes.push_back(argumentResult.second);
        parameterValues.push_back(productionSequence->elementAt(i));
      }

      Alt::Simple* simple = new Alt::Simple(algebraFunctionName, location);
      simple->args = args;
      rhsResult = simple;

      // Add an other method declaration to the signature:
      if (algebraFunctionName != NULL) {
        createAlgebraFunction(
          infoContext, algebraFunctionName, parameterTypes, parameterValues);

        resultType = ANSWER;
      } else {
        // This may not happen, because in GAP all sequences
        // are applied to an algebra function.
        throw LogError(
          "gap-00603: a sequence of CFG nodes has no algebra function name "
          "associated with it.");
      }

      break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      std::cout << "+PRODUCTION_ALTERNATIVE+ "; Printer::PrettyPrintCOut pp;
        pp.ppBase(NULL, b); std::cout << std::endl;
      // This type should not happen, since alternatives are
      // only allowed at the top level of the CFG graph. In case
      // this assumption is violated, we can still translate this
      // into an Alt::Block node.
      CFG::ProductionAlternative* productionAlternative =
        dynamic_cast<CFG::ProductionAlternative*>(b);
      std::list<Alt::Base*>* altList = processProductionAlternative(
        infoContext, productionAlternative);
      Alt::Block* blck = new Alt::Block(*altList, location);

      rhsResult = blck;
      resultType = ANSWER;

      break;
    }
    default: {
      throw LogError("gap-00601: Cannot transform CFG node type.");
    }
  }

  // The designated axiom-production which is simply used
  // as a proxy for the original axiom as a means to set
  // the axiom for the specialized grammar receives special
  // attention here.
  DesignatedAxiomAttribute* designatedAxiomAttribute =
    getDesignatedAxiomAttribute(b);
  if (designatedAxiomAttribute != NULL) {
    std::string* originalAxiomName =
      designatedAxiomAttribute->getOriginalAxiomName();
    std::string* designatedAxiomAlgebraFunctionName = new std::string("f00");
    createDesignatedAxiomAlgebraFunction(
      designatedAxiomAlgebraFunctionName, originalAxiomName);
    // Also wrap the result into an algebra function, because
    // this is the whole purpose of the additional production
    // that a special algebra function is generated which sets
    // the axiom name of the generated grammar in the 'rules'
    // data structure of the generated result.
    rhsResult = createAlgebraFunctionCallWrapper(
      designatedAxiomAlgebraFunctionName, rhsResult);
  }


  return std::pair<Alt::Base*, AlgebraParameterTypes>(rhsResult, resultType);
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::
  getAlgebraParameterType(AlgebraParameterTypes type) {
  switch (type) {
    case VOID: {
      return createSignatureVoidType();
    }
    case ANSWER: {
      return createAlgebraFunctionAnswerType();
    }
    case ALPHABET: {
      return createAlgebraFunctionAlphabetType();
    }
    case ALPHABET_STRING: {
      return createAlgebraAlphabetStringType();
    }
    default: {
      throw LogError(
        "gap-00650: Internal: unsupported algbebra parameter type.");
    }
  }
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::
  getAlgebraSignatureType(AlgebraParameterTypes type) {
  switch (type) {
    case VOID: {
      return createSignatureVoidType();
    }
    case ANSWER: {
      // return createSignatureAnswerType();
      return createSignatureType(new std::string("answer"));
    }
    case ALPHABET: {
      // return getAlphabetType();
      return createSignatureType(new std::string("alphabet"));
    }
    case ALPHABET_STRING: {
      return createAlgebraAlphabetStringType();
    }
    default: {
      throw LogError(
          "gap-00651: Internal: unsupported algbebra signature type.");
    }
  }
}


std::list<Type::Base*>* SpecializeGrammar::CreateSpecializedGrammar::
  transformaAlgebraSignatureTypes(std::list<AlgebraParameterTypes> types) {
  std::list<Type::Base*>* result = new std::list<Type::Base*>();

  for (std::list<AlgebraParameterTypes>::iterator i = types.begin();
       i != types.end(); i++) {
    result->push_back(getAlgebraSignatureType(*i));
  }

  return result;
}


Type::Alphabet* SpecializeGrammar::CreateSpecializedGrammar::getAlphabetType() {
  assert(this->ast != NULL);
  hashtable<std::string, Type::Base*>::iterator i =
    this->ast->types.find("alphabet");
  assert(i != this->ast->types.end());
  return dynamic_cast<Type::Alphabet*>((*i).second);
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::createSignatureType(
  std::string* typeName) {
  Loc location;
  return new Type::Usage(new Type::Signature(
    typeName, location, this->signature), location);
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createSignatureVoidType() {
  Loc location;
  return new Type::Usage(new Type::Void(location), location);
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createSignatureAnswerType() {
  Loc location;
  return new Type::Usage(new Type::Signature(new std::string(
    theAlgebraAnswerTypeDefName), location, this->signature), location);
}


std::string* SpecializeGrammar::CreateSpecializedGrammar::
  getAlgebraFunctionName(CFG::Base* b) {
  Util::Attribute* attribute = b->getAttribute(
    "Util::AlgebraFunctionNameAttribute");
  Util::AlgebraFunctionNameAttribute* functionNameAttribute = (
    Util::AlgebraFunctionNameAttribute*)attribute;

  if (functionNameAttribute != NULL) {
    return functionNameAttribute->getAlgebraFunctionName();
  } else {
    return NULL;
  }
}


Alt::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createAlgebraFunctionCallWrapper(
    std::string* algebraFunctionName, Alt::Base* arg) {
  Loc location;
  std::list<Fn_Arg::Base*> args;
  Fn_Arg::Alt* algebraFunctionArgument = new Fn_Arg::Alt(arg, location);
  args.push_back(algebraFunctionArgument);
  return createAlgebraFunctionCallWrapper(algebraFunctionName, args);
}


Alt::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createAlgebraFunctionCallWrapper(
    std::string* algebraFunctionName, std::list<Fn_Arg::Base*> args) {
  Loc location;
  Alt::Simple* algebraFunctionCall = new Alt::Simple(
    algebraFunctionName, location);
  algebraFunctionCall->args = args;
  return algebraFunctionCall;
}


void SpecializeGrammar::CreateSpecializedGrammar::addSignatureDeclaration(
  std::string* algebraFunctionName, AlgebraParameterTypes parameterType) {
  Loc location;
  std::list<AlgebraParameterTypes> parameterTypes;
  parameterTypes.push_back(parameterType);
  addSignatureDeclaration(algebraFunctionName, parameterTypes);
}


void SpecializeGrammar::CreateSpecializedGrammar::addSignatureDeclaration(
  std::string* algebraFunctionName,
  std::list<AlgebraParameterTypes> parameterTypes) {
  Loc location;
  Fn_Decl* algebraFunctionDecl = new Fn_Decl(createSignatureType(
    new std::string("answer")), algebraFunctionName, location);
  algebraFunctionDecl->set_types(transformaAlgebraSignatureTypes(
    parameterTypes));
  this->signatureDecls[*algebraFunctionName] = algebraFunctionDecl;
}


void SpecializeGrammar::CreateSpecializedGrammar::
  addSignatureChoiceDeclaration(std::string* choiceFunctionName) {
  Loc location;
  Type::Base* returnType = new Type::Choice(new Type::List(
    createSignatureType(new std::string("answer"))), location);
  // Actually the parameter type is the same as the return type,
  // but the compiler throws an exception when we use the same
  // instance of Type::Base for this name.
  Type::Base* parameterType = new Type::List(createSignatureType(
    new std::string("answer")));
  // Type::Base* returnType = new Type::List(createSignatureType(
  //   new std::string("answer")));
  Fn_Decl* choiceFunctionDecl = new Fn_Decl(
    returnType, choiceFunctionName, location);
  std::list<Type::Base*>* parameterTypes = new std::list<Type::Base*>();
  parameterTypes->push_back(parameterType);
  choiceFunctionDecl->set_types(parameterTypes);
  this->signatureDecls[*choiceFunctionName] = choiceFunctionDecl;
}


Algebra* SpecializeGrammar::CreateSpecializedGrammar::createAlgebra() {
  Loc location;
  Algebra* alg = new Algebra(
    new std::string(theAlgebraName), new std::string(
      theSignatureName), location);
  // alg->set_default_choice_fn_mode(new std::string("mode_ident"));

  hashtable<std::string, Type::Base*>* algebraParameters =
    new hashtable<std::string, Type::Base*>();
  (*algebraParameters)["alphabet"] = getAlgebraParameterType(ALPHABET);
  (*algebraParameters)["answer"] = getAlgebraParameterType(ANSWER);
  alg->set_params(algebraParameters);
  delete(algebraParameters);

  // Before we call it a day, we need an algebra choice function.
  createChoiceFunction();

  // Now set all algebra function definitions into the
  // new algebra. This definitions have been created indirectly
  // when the CFG graph was traversed, before this method has
  // been called.
  alg->set_fns(this->algebraFunctionDefinitions);

  return alg;
}


void SpecializeGrammar::CreateSpecializedGrammar::createChoiceFunction() {
  // A dummy location.
  Loc location;

  // Define the name of the choice function.
  std::string* choiceFunctionName = new std::string(theChoiceFunctionName);
  // The name of the list parameter of the choice function.
  std::string* parameterName = new std::string("l");
  // First we add the signature...
  addSignatureChoiceDeclaration(choiceFunctionName);

  // ...then we create a function definition, which is
  // the same for all algebras, hence some kind of static
  // generated function code.
  Fn_Def* choiceFunction = new Fn_Def(new Type::Choice(new Type::List(
    getAlgebraParameterType(ANSWER)), location), choiceFunctionName);

  // Create the list of parameters, which consists in this
  // case of only one element, the "list of result"
  std::list<Para_Decl::Base*> parameterDeclarations;
  parameterDeclarations.push_back(new Para_Decl::Simple(new Type::List(
    getAlgebraParameterType(ANSWER)), parameterName, location));
  choiceFunction->set_paras(parameterDeclarations);

  // Create the list of statements, before we create the statements
  // of the choice function
  std::list<Statement::Base*> statements;

  // Now just return the combination of all result from
  // the list of result.
  Expr::Fn_Call* mergeResultExpr = new Expr::Fn_Call(new std::string(
    theMergeRulesFunctionName));
  mergeResultExpr->add_arg(new Expr::Vacc(
    new Var_Acc::Plain(parameterName)));
  Expr::Fn_Call* buildListExpr = new Expr::Fn_Call(new std::string("list"));
  buildListExpr->add_arg(mergeResultExpr);
  Statement::Return* returnStatement = new Statement::Return(buildListExpr);
  statements.push_back(returnStatement);

  // Finally set the list of statements for the choice function.
  choiceFunction->set_statements(statements);

  // Set the mode to 'choice function'. Modes can be found in ../modes.hh
  Mode mode;
  mode.set(Mode::PRETTY);
  choiceFunction->set_mode(mode);
  // choiceFunction->set_mode(Mode(Mode::NONE));

  this->algebraFunctionDefinitions[*choiceFunctionName] = choiceFunction;
}


void SpecializeGrammar::CreateSpecializedGrammar::
  createDesignatedAxiomAlgebraFunction(
    std::string* designatedAxiomAlgebraFunctionName,
    std::string* originalAxiomName) {
  Loc location;

  // The algebra function definition structure which we first
  // fill with type definitions for the signature and statements
  // before we add it to the map of defined algebra functions.
  Fn_Def* algebraFunction = new Fn_Def(
    createAlgebraFunctionAnswerType(),
    designatedAxiomAlgebraFunctionName, location);

  // We start with the signature of the algebra function
  std::list<AlgebraParameterTypes> parameterTypes;
  parameterTypes.push_back(ANSWER);
  addSignatureDeclaration(designatedAxiomAlgebraFunctionName, parameterTypes);

  // Create the "list" of parameter declarations, which has only
  // one single element.
  std::list<Para_Decl::Base*> parameterDeclarations;
  std::string* parameterName = new std::string("p0");
  parameterDeclarations.push_back(new Para_Decl::Simple(
    getAlgebraParameterType(ANSWER), parameterName, location));
  algebraFunction->set_paras(parameterDeclarations);


  // Now for the list of statements, there are only three things
  // to do:
  // 1) We need a result variable which will be initialized with
  //    the contents of the only parameter 'p0'.
  // 2) A call to the function 'setAxiomName' definde in the
  //    runtime lib 'rtlib/rules.hh'.
  // 3) And a return-statement which returns the result.
  std::list<Statement::Base*> statements;

  // The algebra function's result variable of type answer_type
  // and with the name 'result'.
  Statement::Var_Decl* alg_resultVar = new Statement::Var_Decl(
    createAlgebraFunctionAnswerType(), new std::string(
      theAlgebraResultVariableName), new Expr::Vacc(parameterName));
  statements.push_back(alg_resultVar);

  // The axiom's name is retrieved by the the good old function
  // 'getRuleName', which maps rule name prefixes and shape strings
  // to a grammar rule name of the resulting grammar.
  Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(new std::string(
    theGetRuleNameFunctionName));
  getRuleNameCallExpr->add_arg(new Expr::Const(
    new Const::String(*originalAxiomName)));
  getRuleNameCallExpr->add_arg(new Var_Acc::Comp(new Var_Acc::Plain(
    new std::string(theAlgebraResultVariableName)),
    new std::string(theResultShapeFieldName)));
  // The call to 'setAxiomName' from the rtlib/rules.hh file
  // uses the variable 'rules' and the function call 'getRuleName'
  // (see above).
  Statement::Fn_Call* alg_insertProductionCall = new Statement::Fn_Call(
    "setAxiomName");
  alg_insertProductionCall->add_arg(*alg_resultVar);
  alg_insertProductionCall->add_arg(getRuleNameCallExpr);
  statements.push_back(alg_insertProductionCall);


  // Set the name of the signature by a call to the merhod 'setSignatureName'
  // defined in rules.hh.
  Statement::Fn_Call* alg_insertSetSignatureNameCall = new Statement::Fn_Call(
    "setSignatureName");
  alg_insertSetSignatureNameCall->add_arg(*alg_resultVar);
  alg_insertSetSignatureNameCall->add_arg(new Expr::Const(
    new Const::String(*this->sourceProgramSignatureName)));
  statements.push_back(alg_insertSetSignatureNameCall);


  // The return statement.
  Statement::Return* rtrn = new Statement::Return(*alg_resultVar);
  statements.push_back(rtrn);


  // All statements have been generated, thus add all
  // statements to the algebra function body.
  algebraFunction->set_statements(statements);

  // And: done, we have successfully assembled an algebra function
  // AST structure, which can be readily added to the map of
  // defined algebra functions.
  this->algebraFunctionDefinitions[*designatedAxiomAlgebraFunctionName] =
    algebraFunction;
}


void SpecializeGrammar::CreateSpecializedGrammar::createAlgebraFunction(
    Util::Attributable* infoContext, std::string* name,
    AlgebraParameterTypes parameterType, CFG::Base* parameterValue) {
  std::list<AlgebraParameterTypes> parameterTypes;
  parameterTypes.push_back(parameterType);
  std::list<CFG::Base*> parameterValues;
  parameterValues.push_back(parameterValue);
  createAlgebraFunction(infoContext, name, parameterTypes, parameterValues);
}


void SpecializeGrammar::CreateSpecializedGrammar::createAlgebraFunction(
  Util::Attributable* infoContext, std::string* name,
  std::list<AlgebraParameterTypes> parameterTypes,
  std::list<CFG::Base*> parameterValues) {
  std::cout << "createAlgebraFunction (" << *name << ")" << std::endl;
  // An algebra function for the shape parser has the following
  // properties:
  // 1) The list of parameters is named 'p<n>', where <n> is a
  //    sequence number starting at 0, hence this number corresponds
  //    to the position of the parameter in the signature of the
  //    function.
  // 2) All parameters of type 'answer_type' will be added together.
  //    This operation merges all rules stored in the parameter
  //    values and assigns the result to the function's result
  //    variable. If all parameters are of type other than
  //    'answer_type', the algebra function's result variable will
  //    be initialized as empty.
  // 3) The algebra function's result variable's shape-string is
  //    computed from the concatenation of all parameter values.
  //    A parameter of type 'ALPHABET' (which is a character in
  //    out application) is directly appended, while a parameter
  //    of type 'ANSWER' provides the value via its field 'shape'.
  //    The computed shape string is added to the result variable
  //    via the external function 'setShape' defined in the
  //    runtime lib file 'rtlib/rules.hh'.
  // 4) The algebra function has two variables which store the
  //    non-terminal name, and the body of the generated grammar
  //    rule. A grammar rule is generated by computing both values
  //    and then calling 'insertProduction' of the runtime lib
  //    defined in 'rtlib/rules.hh'. Usually the algebra function
  //    generates just one grammar production since each applied
  //    grammar production of the shape-parser has its own custom
  //    algebra function to be called. Since there are also
  //    hidden productions, we sometimes insert additional
  //    productions into the rules-result of the algebra-function.
  //    When hidden productions are generated, we use the same
  //    algebra function variables and external function calls
  //    as we did with the regular production.
  // 5) Finally the result is returned.
  Loc location;
  Fn_Def* algebraFunction = new Fn_Def(
    createAlgebraFunctionAnswerType(), name, location);

  // We start with the signature of the algebra function
  addSignatureDeclaration(name, parameterTypes);

  // Create a list of parameters with the corresponding
  // signature type, and a simple name, say 'p1', 'p2',...
  std::list<Para_Decl::Base*> parameterDeclarations;
  int pos = 0;
  for (std::list<AlgebraParameterTypes>::iterator i = parameterTypes.begin();
       i != parameterTypes.end(); i++) {
    std::string* parameterName = new std::string("p" + boost::str(
      boost::format("%1%") % pos++));
    parameterDeclarations.push_back(new Para_Decl::Simple(
      getAlgebraParameterType(*i), parameterName, location));
  }
  algebraFunction->set_paras(parameterDeclarations);



  ////////////////////////////////////////////////////
  // Now it is time for the statements of this algebra function:
  ////////////////////////////////////////////////////
  std::list<Statement::Base*> statements;

  // Each algebra function consists of two main parts:
  // 1) Construction of the current shape-string. This step
  //    reconstructs
  // 2) each grammar production from the original grammar
  //    which originated the grammar productions of the
  //    parser (each grammar production part has its own
  //    algebra function)


  // Create an expression which adds up all answer_typed
  // parameters. This expression is used as an initializer
  // for the alg_resultVar, that is the 'result' variable
  // of the algebra.
  pos = 0;
  Expr::Base* initExpr = NULL;
  Expr::Base* shapeExpr = NULL;
  for (std::list<AlgebraParameterTypes>::iterator i = parameterTypes.begin();
       i != parameterTypes.end(); i++) {
    std::string* parameterName = new std::string("p" + boost::str(
      boost::format("%1%") % pos++));
    Expr::Base* exprI = NULL;
    Expr::Base* exprS = NULL;
    switch (*i) {
      case ANSWER: {
        // exprI = new Expr::Vacc (parameterName, new std::string (
        //   theResultShapeFieldName));
        exprI = new Expr::Vacc(parameterName);
        exprS = new Expr::Vacc(parameterName, new std::string(
          theResultShapeFieldName));
        break;
      }
      case ALPHABET: {
        exprS = new Expr::Vacc(parameterName);
        break;
      }
      case VOID: {
        // No parameter can be void. Is this actually an exception?
        break;
      }
      case ALPHABET_STRING: {
        exprS = new Expr::Vacc(parameterName);
        break;
      }
      default: {
        throw LogError(
          "gap-00661: Internal: unhandled algebra parameter type.");
      }
    }
    if (exprI != NULL) {
      if (initExpr == NULL) {
        initExpr = exprI;
      } else {
        initExpr = create_Rope_PLUS_Rope_Expression(initExpr, exprI);
      }
    }
    if (exprS != NULL) {
      if (shapeExpr == NULL) {
        shapeExpr = exprS;
      } else {
        shapeExpr = create_Rope_PLUS_Rope_Expression(shapeExpr, exprS);
      }
    }
  }
  // Define the result-variable of the algebra:
  Statement::Var_Decl* alg_resultVar = NULL;
  if (initExpr == NULL) {
    alg_resultVar = new Statement::Var_Decl(
      createAlgebraFunctionAnswerType(), new std::string(
        theAlgebraResultVariableName));
  } else {
    alg_resultVar = new Statement::Var_Decl(
      createAlgebraFunctionAnswerType(), new std::string(
        theAlgebraResultVariableName), initExpr);
  }
  statements.push_back(alg_resultVar);

  // After the result variable has been initialized, we construct
  // the shape string from the input. Since the init of the algebra
  // result variable is similar to the construction of the shape
  // string, we created both AST structures in the same loop.
  // The function which sets the shape is defined in rules.hh
  // and is named 'setShape'.
  if (shapeExpr != NULL) {
    Statement::Fn_Call* alg_setShapeCall = new Statement::Fn_Call("setShape");
    alg_setShapeCall->add_arg(*alg_resultVar);
    alg_setShapeCall->add_arg(shapeExpr);
    statements.push_back(alg_setShapeCall);
  }


  Statement::Var_Decl* alg_ntVar = new Statement::Var_Decl(
    createInternalAlgebraStringType(), new std::string("nt"));
  statements.push_back(alg_ntVar);
  Statement::Var_Decl* alg_bodyVar = new Statement::Var_Decl(
    createInternalAlgebraStringType(), new std::string("body"));
  statements.push_back(alg_bodyVar);

  if (this->algebraFunctionInfoAttribute == NULL) {
    // Not each CFG-grammar construct had an algebra function
    // applied to it in the original GAP grammar. In this case
    // the original grammar can only have a single non-terminal
    // or terminal-parser, because sequences must be applied
    // to an algebra function.
    assert(parameterValues.size() == 1);
    CFG::Base* singleElement = parameterValues.front();
    std::list<Statement::Base*>* algebraFunctionArguments =
      createFunctionCallNoAlgFn(alg_ntVar, alg_bodyVar, singleElement);
    statements.insert(
      statements.end(),
      algebraFunctionArguments->begin(), algebraFunctionArguments->end());
    delete(algebraFunctionArguments);
  } else {
    std::list<Fn_Arg::Base*> originalArguments =
      this->algebraFunctionInfoAttribute->getAlgebraFunctionArguments();
    std::list<CFG::Base*> orderedParameterValues =
      orderAlgebraFunctionArguments(parameterValues, originalArguments);
    std::list<Statement::Base*>* algebraFunctionArguments =
      createFunctionCall(alg_ntVar, alg_bodyVar, orderedParameterValues);
    statements.insert(
      statements.end(),
      algebraFunctionArguments->begin(), algebraFunctionArguments->end());
    delete(algebraFunctionArguments);
  }

  // Add the new grammar-rule to the set of rules
  // in the algebra result. This is done by the function
  // 'insertProduction' which is defined in the file rules.hh
  statements.push_back(create_InsertProduction_Call(
    alg_resultVar, alg_ntVar, alg_bodyVar));

  // New 'technique': now we create the whole hidden path up
  // to the cycle's break point, which is a more general method
  // of handling broken cycles, because it should work as the
  // hidden-fragment attribute, but create richer rule-set
  // for longer cycles.
  /*
  // Each hidden production fragment must be generated as well.
  if (this->hiddenGrammarRuleFragments != NULL) {
    for (HiddenCFGFragmentsAttribute::iterator i =
      // this->hiddenGrammarRuleFragments->begin(); i != this->
      // hiddenGrammarRuleFragments->end(); i++) {
      // There is simply no way around the base-wrapper (wich supplies
      // us with the list of original arguments and the original
      // algebra function name), because the only way to not have this
      // wrapper around is that the CFG structure is a single element
      // which is a simple non-terminal or a simple terminal parser,
      // which do not have the need for an evaluation function.
      // Hidden elements may not have this, because this would mean
      // that the original GAP program had an infinite loop (which would
      // be like this: ruleX = f1(...) | ... | ruleX | f2(...)).
      // ... unless someone changed the ambiguity CFG generator used
      // as a front end to this component, which may conflict with
      // this implementation...
      assert ((*i)->is (CFG::BASE_WRAPPER));
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (*i);
      std::list<Statement::Base*>* resultStatements = createHiddenFunctionCall
      // (wrapper, alg_ntVar, alg_bodyVar, convertCFGFragmentToArgumentList
      // (wrapper->getWrappedBase()));
      statements.insert (statements.end(), resultStatements->begin(),
      // resultStatements->end());
      delete (resultStatements);
      statements.push_back (create_InsertProduction_Call (alg_resultVar,
      // alg_ntVar, alg_bodyVar));
    }
  }
  */

  // If this non-terminal lies on a cycle-path, all production parts
  // which do not consume any of the shape input are also generated.
  // Although a cycle might be broken up in a grammar production of
  // a non-terminal which is only indirectly reachable, we must close
  // this cycle in the resulting grammar to express the same language
  // as before.
  if (this->cyclePathInformation != NULL) {
    for (Util::CyclePathInfoAttribute::iterator i =
         this->cyclePathInformation->begin();
         i != this->cyclePathInformation->end(); i++) {
      std::string nonTerminalNameTheFragmentIsDefinedIn = (*i).first;
      CFG::Base* fragmentOnCyclePath = (*i).second;
      if (fragmentOnCyclePath->is(CFG::BASE_WRAPPER)) {
        CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (
            fragmentOnCyclePath);
        std::list<Statement::Base*>* resultStatements =
          createHiddenFunctionCall(
            nonTerminalNameTheFragmentIsDefinedIn, wrapper, alg_ntVar,
            alg_bodyVar,
            convertCFGFragmentToArgumentList(wrapper->getWrappedBase()));
        statements.insert(
          statements.end(), resultStatements->begin(),
          resultStatements->end());
        delete(resultStatements);
        statements.push_back(create_InsertProduction_Call(
          alg_resultVar, alg_ntVar, alg_bodyVar));
      } else {
        std::list<Statement::Base*>* resultStatements =
          createHiddenFunctionCallNoAlgFn(
            nonTerminalNameTheFragmentIsDefinedIn, alg_ntVar, alg_bodyVar,
            fragmentOnCyclePath);
        statements.insert(
          statements.end(), resultStatements->begin(), resultStatements->end());
        delete(resultStatements);
        statements.push_back(create_InsertProduction_Call(
          alg_resultVar, alg_ntVar, alg_bodyVar));
      }
    }
  }

  // Finally create an overall answer from this parts,
  // and 'return' the result-variable:
  Statement::Return* rtrn = new Statement::Return(*alg_resultVar);
  statements.push_back(rtrn);


  algebraFunction->set_statements(statements);
  /*
       delete($9);
       f->set_mode($1);
  */

  // We store the function in a field of this instance,
  // because at this point the method was generated on the fly,
  // but will be written out only after the whole CFG graph
  // has been traversed.
  if (this->algebraFunctionDefinitions.find(*name) !=
      this->algebraFunctionDefinitions.end()) {
    throw LogError(
      "gap-00640: Internal: Algebra '" + *name +
      "' function already generated.");
  }
  this->algebraFunctionDefinitions[*name] = algebraFunction;
}


std::list<Statement::Base*>* SpecializeGrammar::CreateSpecializedGrammar::
  createFunctionCallNoAlgFn(
    Statement::Var_Decl* ntVar,
    Statement::Var_Decl* bodyVar, CFG::Base* argument) {
  std::list<Statement::Base*>* result = new std::list<Statement::Base*>();

  // Create a new grammar rule name from the original rule
  // name and the currently parsed string, which is called the
  // shape-string (alg_shapeVar).
  Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(new std::string(
    theGetRuleNameFunctionName));
  assert(this->currentGrammarProductionName != NULL);
  getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
    *this->currentGrammarProductionName)));
  getRuleNameCallExpr->add_arg(new Var_Acc::Comp(new Var_Acc::Plain(
    new std::string(theAlgebraResultVariableName)), new std::string(
      theResultShapeFieldName)));
  Statement::Var_Assign* alg_ntVar = new Statement::Var_Assign(
    *ntVar, getRuleNameCallExpr);
  result->push_back(alg_ntVar);

  // No surrounding algebra function call needed for
  // this single element.
  Expr::Base* alg_ruleBodyExpr = createFunctionCallArgumentNoAlgFn(
    NULL, argument);
  assert(alg_ruleBodyExpr != NULL);
  Statement::Var_Assign* alg_bodyVar = new Statement::Var_Assign(
    *bodyVar, alg_ruleBodyExpr);
  result->push_back(alg_bodyVar);

  return result;
}


Expr::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createFunctionCallArgumentNoAlgFn(
    Util::Attributable* infoContext, CFG::Base* fragment) {
  switch (fragment->getType()) {
    case CFG::BASE_WRAPPER: {
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*>(fragment);
      // At the moment there is no need for an annotated wrapper
      // at this point in the data structure, but one never knows.
      // For extensibility in the future, the wrapper semantic is
      // provided here, too.
      return createFunctionCallArgumentNoAlgFn(
        wrapper, wrapper->getWrappedBase());
    }
    case CFG::EPSILON: {
      return new Expr::Const(new Const::String("EMPTY"));
    }
    case CFG::TERMINAL: {
      CFG::Terminal* terminal = dynamic_cast<CFG::Terminal*>(fragment);
      return new Expr::Const(new Const::String(
        "CHAR ('" + *terminal->getValue() + "')"));
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*>(
        fragment);

      // Create an expression which calls the 'getRuleName' function
      // defined in rules.hh. The parameter we use to load the shape
      // from is fixed for 'p0', because this method transforms CFG
      // node elements which came from GAP grammar constructs that
      // had no algebra function around them, which by definition of
      // the GAP syntax can only be single elements. This in turn
      // causes, that the algebra function of our shape parser has
      // exactly one parameter, whose name is 'p0'.
      Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(
        new std::string(theGetRuleNameFunctionName));
      getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
        *nonTerminal->getName())));
      getRuleNameCallExpr->add_arg(new Expr::Vacc(
        new std::string("p0"), new std::string(theResultShapeFieldName)));

      return getRuleNameCallExpr;
    }
    default: {
      // This may not happen! The type of our single argument
      // must be either epsilon, a terminal, non-terminal or a
      // regular expression.
    }
  }

  // CAUTION: the result might be NULL in case of an unhandled
  // CFG node type (there is no exception here).
  return NULL;
}


std::list<Statement::Base*>* SpecializeGrammar::CreateSpecializedGrammar::
  createFunctionCall(
    Statement::Var_Decl* ntVar,
    Statement::Var_Decl* bodyVar, std::list<CFG::Base*> arguments) {
  assert(this->algebraFunctionInfoAttribute != NULL);
  std::string* originalAlgebraFunctionName =
    this->algebraFunctionInfoAttribute->getAlgebraFunctionName();
  std::list<Fn_Arg::Base*> originalArguments =
    this->algebraFunctionInfoAttribute->getAlgebraFunctionArguments();
  assert(originalAlgebraFunctionName != NULL);
  // not a pointer, therefore cannot be NULL!
  // assert(originalArguments != NULL);


  std::list<Statement::Base*>* result = new std::list<Statement::Base*>();


  // Create a new grammar rule name from the original rule
  // name and the currently parsed string, which is called the
  // shape-string (alg_shapeVar).
  Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(new std::string(
    theGetRuleNameFunctionName));
  assert(this->currentGrammarProductionName != NULL);
  getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
    *this->currentGrammarProductionName)));
  getRuleNameCallExpr->add_arg(new Var_Acc::Comp(new Var_Acc::Plain(
    new std::string(theAlgebraResultVariableName)),
    new std::string(theResultShapeFieldName)));
  Statement::Var_Assign* alg_ntVar = new Statement::Var_Assign(
    *ntVar, getRuleNameCallExpr);
  result->push_back(alg_ntVar);

  // After the rule name has been created, we build the
  // body of the production.
  Expr::Base* alg_ruleBodyExpr = createFunctionCallArguments(arguments);
  alg_ruleBodyExpr = create_Rope_PLUS_Rope_Expression(new Expr::Const(
    new Const::String(*originalAlgebraFunctionName + " (")), alg_ruleBodyExpr);
  alg_ruleBodyExpr = create_Rope_PLUS_Rope_Expression(
    alg_ruleBodyExpr, new Expr::Const(new Const::String(")")));
  Statement::Var_Assign* alg_bodyVar = new Statement::Var_Assign(
    *bodyVar, alg_ruleBodyExpr);
  result->push_back(alg_bodyVar);

  // Done. Return.
  return result;
}


Expr::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createFunctionCallArguments(std::list<CFG::Base*> arguments) {
  Expr::Base* result = NULL;

  for (std::list<CFG::Base*>::iterator i = arguments.begin();
       i != arguments.end(); i++) {
    Expr::Base* argResult = createFunctionCallArgument(NULL, *i);
    if (result == NULL) {
      result = argResult;
    } else {
      result = create_Rope_PLUS_Rope_Expression(
        result, new Expr::Const(new Const::String(", ")));
      result = create_Rope_PLUS_Rope_Expression(result, argResult);
    }
  }

  return result;
}


Expr::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createFunctionCallArgument(
    Util::Attributable* infoContext, CFG::Base* fragment) {
  Loc location;
  switch (fragment->getType()) {
    case CFG::BASE_WRAPPER: {
      std::cout << "passing through a wrapper" << std::endl;
      // This is either a function call, or a wrapped argument.
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*>(fragment);
      return createFunctionCallArgument(wrapper, wrapper->getWrappedBase());
    }
    case CFG::EPSILON: {
      return new Expr::Const(new Const::String("EMPTY"));
    }
    case CFG::TERMINAL: {
      CFG::Terminal* terminal = dynamic_cast<CFG::Terminal*>(fragment);
      return new Expr::Const(new Const::String(
        "CHAR ('" + *terminal->getValue() + "')"));
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal =
        dynamic_cast<CFG::NonTerminal*>(fragment);

      // At this point we must have passed through a CFG::BaseWrapper
      // instance, if this non-terminal belongs to the parameter list
      // of the transformed CFG graph. Otherwise this is a non-terminal
      // from the original grammar which is not bound to any shape
      // specialized grammar-production.
      if (infoContext != NULL) {
        std::cout << "infoContext != NULL" << std::endl;
        // Util::ParameterPositionAttribute* positionAttribute =
        //  getPositionAttribute (infoContext);
        // assert (positionAttribute != NULL);
        // int parameterPosition = positionAttribute->getParameterPosition();
        ActualParameterPositionAttribute* actualPositionAttribute =
          getActualPositionAttribute(infoContext);
        int actualPosition = actualPositionAttribute->getActualPosition();

        // Create an expression which calls the 'getRuleName' function
        // defined in rules.hh
        Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(
          new std::string(theGetRuleNameFunctionName));
        getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
          *nonTerminal->getName())));
        getRuleNameCallExpr->add_arg(new Expr::Vacc(new std::string(
          boost::str(boost::format("p%1%") % actualPosition)),
          new std::string(theResultShapeFieldName)));

        return getRuleNameCallExpr;
      } else {
        std::cout << "infoContext is NULL" << std::endl;
        // In this case we process an unbound non-terminal, which
        // stems from the original algebra function call from the
        // original GAP grammar. This non-terminal is naturally
        // not bound to any shape, since its parse result which it
        // yields in the original grammar has no effect in this
        // transformed shape grammar. Hence we do not map this name
        // according to any currently parsed shape via 'getRuleName',
        // but use the original name instead.
        return new Expr::Const(new Const::String(*nonTerminal->getName()));
      }
    }
    case CFG::REGULAR_EXPRESSION: {
      return new Expr::Const(new Const::String("CHAR"));
    }
    case CFG::PRODUCTION_SEQUENCE: {
      // break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      // break;
    }
    default: {
      throw LogError("gap-00646: Unhandled CFG node type.");
    }
  }

  // This point should not be reached!.
  return NULL;
}


std::list<CFG::Base*> SpecializeGrammar::CreateSpecializedGrammar::
  convertCFGFragmentToArgumentList(CFG::Base* fragment) {
  std::list<CFG::Base*> result;

  switch (fragment->getType()) {
    case CFG::BASE_WRAPPER:
    case CFG::EPSILON:
    case CFG::TERMINAL:
    case CFG::NONTERMINAL: {
      result.push_back(fragment);
      break;
    }
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* sequence =
        dynamic_cast<CFG::ProductionSequence*>(fragment);

      for (CFG::ProductionSequence::iterator i = sequence->begin();
           i != sequence->end(); i++) {
        result.push_back(*i);
      }

      break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      result.push_back(fragment);
      break;
    }
    default: {
      throw LogError("gap-00677: Internal: unhandled CFG node type.");
    }
  }

  return result;
}


std::list<Statement::Base*>* SpecializeGrammar::CreateSpecializedGrammar::
  createHiddenFunctionCallNoAlgFn(
    std::string productionNT, Statement::Var_Decl* ntVar,
    Statement::Var_Decl* bodyVar, CFG::Base* argument) {
  std::list<Statement::Base*>* result = new std::list<Statement::Base*>();


  // Create a new grammar rule name from the original rule
  // name and the currently parsed string, which is called the
  // shape-string (alg_shapeVar).
  Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(new std::string(
    theGetRuleNameFunctionName));
  assert(this->currentGrammarProductionName != NULL);
  getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
    productionNT)));
  getRuleNameCallExpr->add_arg(new Var_Acc::Comp(new Var_Acc::Plain(
    new std::string(theAlgebraResultVariableName)), new std::string(
      theResultShapeFieldName)));
  Statement::Var_Assign* alg_ntVar = new Statement::Var_Assign(
    *ntVar, getRuleNameCallExpr);
  result->push_back(alg_ntVar);

  // After the rule name has been created, we build the
  // body of the production.
  Expr::Base* alg_ruleBodyExpr = createHiddenFunctionCallArgumentNoAlgFn(
    NULL, argument);
  Statement::Var_Assign* alg_bodyVar = new Statement::Var_Assign(
    *bodyVar, alg_ruleBodyExpr);
  result->push_back(alg_bodyVar);

  // Done. Return.
  return result;
}


Expr::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createHiddenFunctionCallArgumentNoAlgFn(
    Util::Attributable* infoContext, CFG::Base* fragment) {
  Loc location;
  switch (fragment->getType()) {
    case CFG::BASE_WRAPPER: {
      // This is either a function call, or a wrapped argument.
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*>(fragment);
      return createHiddenFunctionCallArgumentNoAlgFn(
        wrapper, wrapper->getWrappedBase());
    }
    case CFG::EPSILON: {
      return new Expr::Const(new Const::String("EMPTY"));
    }
    case CFG::TERMINAL: {
      CFG::Terminal* terminal = dynamic_cast<CFG::Terminal*>(fragment);
      return new Expr::Const(new Const::String(
        "CHAR ('" + *terminal->getValue() + "')"));
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*>(fragment);

      // Try to get a cycle break point attribute, if the non-terminal
      // is annotated with it.
      // SpecializeGrammar::CycleBreakPointAttribute* cycleBreakPointAttribute =
      //   getCycleBreakPointAttribute (nonTerminal);
      Util::CycleMarkAttribute* cycleMarkAttribute = getCycleMarkAttribute(
        fragment);

      // Create an expression which calles the 'getRuleName' function
      // defined in rules.hh
      Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(new std::string(
        theGetRuleNameFunctionName));
      if (cycleMarkAttribute == NULL) {
        getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
          *nonTerminal->getName())));
        getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String("")));
      } else {
        getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
          *nonTerminal->getName())));
        getRuleNameCallExpr->add_arg(new Var_Acc::Comp(new Var_Acc::Plain(
          new std::string(theAlgebraResultVariableName)), new std::string(
            theResultShapeFieldName)));
      }

      return getRuleNameCallExpr;
    }
    case CFG::REGULAR_EXPRESSION: {
      return new Expr::Const(new Const::String("CHAR"));
    }
    case CFG::PRODUCTION_SEQUENCE: {
      // break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      // break;
    }
    default: {
      throw LogError("gap-00646: Unhandled CFG node type.");
    }
  }

  // This point should not be reached!.
  return NULL;
}


std::list<Statement::Base*>* SpecializeGrammar::CreateSpecializedGrammar::
  createHiddenFunctionCall(
    std::string productionNT, Util::Attributable* infoContext,
    Statement::Var_Decl* ntVar, Statement::Var_Decl* bodyVar,
    std::list<CFG::Base*> arguments) {
  assert(infoContext != NULL);
  Util::AlgebraFunctionInfoAttribute* algebraFunctionInfoAttribute =
    getAlgebraFunctionInfoAttribute(infoContext);
  assert(algebraFunctionInfoAttribute != NULL);
  std::string* originalAlgebraFunctionName =
    algebraFunctionInfoAttribute->getAlgebraFunctionName();
  std::list<Fn_Arg::Base*> originalArguments =
    algebraFunctionInfoAttribute->getAlgebraFunctionArguments();
  assert(originalAlgebraFunctionName != NULL);
  // not a pointer, therefore cannot be NULL!
  // assert(originalArguments != NULL);


  // Before we start, we need to order all arguments and create
  // a list of all real parameters to the original algebra function.
  std::list<CFG::Base*> orderedParameterValues = orderAlgebraFunctionArguments(
    arguments, originalArguments);


  std::list<Statement::Base*>* result = new std::list<Statement::Base*>();


  // Create a new grammar rule name from the original rule
  // name and the currently parsed string, which is called the
  // shape-string (alg_shapeVar).
  Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(new std::string(
    theGetRuleNameFunctionName));
  assert(this->currentGrammarProductionName != NULL);
  getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
    productionNT)));
  getRuleNameCallExpr->add_arg(new Var_Acc::Comp(new Var_Acc::Plain(
    new std::string(theAlgebraResultVariableName)), new std::string(
      theResultShapeFieldName)));
  Statement::Var_Assign* alg_ntVar = new Statement::Var_Assign(
    *ntVar, getRuleNameCallExpr);
  result->push_back(alg_ntVar);

  // After the rule name has been created, we build the
  // body of the production.
  // Expr::Base* alg_ruleBodyExpr = createHiddenFunctionCallArguments
  // (infoContext, arguments);
  Expr::Base* alg_ruleBodyExpr = createHiddenFunctionCallArguments(
    infoContext, orderedParameterValues);
  alg_ruleBodyExpr = create_Rope_PLUS_Rope_Expression(new Expr::Const(
    new Const::String(*originalAlgebraFunctionName + " (")), alg_ruleBodyExpr);
  alg_ruleBodyExpr = create_Rope_PLUS_Rope_Expression(
    alg_ruleBodyExpr, new Expr::Const(new Const::String(")")));
  Statement::Var_Assign* alg_bodyVar = new Statement::Var_Assign(
    *bodyVar, alg_ruleBodyExpr);
  result->push_back(alg_bodyVar);

  // Done. Return.
  return result;
}


Expr::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createHiddenFunctionCallArguments(
    Util::Attributable* infoContext, std::list<CFG::Base*> arguments) {
  Expr::Base* result = NULL;

  for (std::list<CFG::Base*>::iterator i = arguments.begin();
       i != arguments.end(); i++) {
    Expr::Base* argResult = createHiddenFunctionCallArgument(NULL, *i);
    if (result == NULL) {
      result = argResult;
    } else {
      result = create_Rope_PLUS_Rope_Expression(result, new Expr::Const(
        new Const::String(", ")));
      result = create_Rope_PLUS_Rope_Expression(result, argResult);
    }
  }

  return result;
}


Expr::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createHiddenFunctionCallArgument(
    Util::Attributable* infoContext, CFG::Base* fragment) {
  Loc location;
  switch (fragment->getType()) {
    case CFG::BASE_WRAPPER: {
      // This is either a function call, or a wrapped argument.
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*>(fragment);
      return createHiddenFunctionCallArgument(
        wrapper, wrapper->getWrappedBase());
    }
    case CFG::EPSILON: {
      return new Expr::Const(new Const::String("EMPTY"));
    }
    case CFG::TERMINAL: {
      CFG::Terminal* terminal = dynamic_cast<CFG::Terminal*>(fragment);
      return new Expr::Const(new Const::String(
        "CHAR ('" + *terminal->getValue() + "')"));
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*>(fragment);

      // At this point we must have passed through a CFG::BaseWrapper
      // instance, otherwise we have an internal error.
      // SpecializeGrammar::CycleBreakPointAttribute* cycleBreakPointAttribute =
      //   getCycleBreakPointAttribute (nonTerminal);
      Util::CycleMarkAttribute* cycleMarkAttribute = getCycleMarkAttribute(
        fragment);

      // Create an expression which calles the 'getRuleName' function
      // defined in rules.hh
      Expr::Fn_Call* getRuleNameCallExpr = new Expr::Fn_Call(new std::string(
        theGetRuleNameFunctionName));
      if (cycleMarkAttribute == NULL) {
        getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
          *nonTerminal->getName())));
        getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String("")));
      } else {
        getRuleNameCallExpr->add_arg(new Expr::Const(new Const::String(
          *nonTerminal->getName())));
        getRuleNameCallExpr->add_arg(new Var_Acc::Comp(new Var_Acc::Plain(
          new std::string(theAlgebraResultVariableName)),
          new std::string(theResultShapeFieldName)));
      }

      return getRuleNameCallExpr;
    }
    case CFG::REGULAR_EXPRESSION: {
      return new Expr::Const(new Const::String("CHAR"));
    }
    case CFG::PRODUCTION_SEQUENCE: {
      // break;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      // break;
    }
    default: {
      throw LogError("gap-00646: Unhandled CFG node type.");
    }
  }

  // This point should not be reached!.
  return NULL;
}


Statement::Base* SpecializeGrammar::CreateSpecializedGrammar::
  create_InsertProduction_Call(
    Statement::Var_Decl* resVar, Statement::Var_Decl* ntVar,
    Statement::Var_Decl* bodyVar) {
  Statement::Fn_Call* alg_insertProductionCall = new Statement::Fn_Call(
    "insertProduction");
  alg_insertProductionCall->add_arg(*resVar);
  alg_insertProductionCall->add_arg(*ntVar);
  alg_insertProductionCall->add_arg(*bodyVar);
  return alg_insertProductionCall;
}


Expr::Base* SpecializeGrammar::CreateSpecializedGrammar::
  create_Rope_PLUS_Rope_Expression(Expr::Base* expr1, Expr::Base* expr2) {
  if (expressionIsConstantString(expr1) &&
      expressionIsConstantString(expr2)) {
    std::string combinedValue =
      *getConstantString(expr1) + *getConstantString(expr2);
    return new Expr::Const(new Const::String(combinedValue));
  } else {
    return new Expr::Plus(expr1, expr2);
  }
}


bool SpecializeGrammar::CreateSpecializedGrammar::expressionIsConstantString(
  Expr::Base* expr) {
  return getConstantString(expr) != NULL;
}


std::string* SpecializeGrammar::CreateSpecializedGrammar::getConstantString(
  Expr::Base* expr) {
  if (expr->is(Expr::CONST)) {
    Expr::Const* cnst = dynamic_cast<Expr::Const*>(expr);
    if (cnst->base->is(Const::STRING)) {
      Const::String* cnstStr = dynamic_cast<Const::String*>(cnst->base);
      return cnstStr->s;
    } else if (cnst->base->is(Const::CHAR)) {
      Const::Char* chr = dynamic_cast<Const::Char*>(cnst->base);
      return new std::string(1, chr->c);
    }
  }
  return NULL;
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createInternalAlgebraStringType() {
  return new Type::External(theAlgebraStringTypeDefName);
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createAlgebraAlphabetStringType() {
  return new Type::External(theAlphabetStringTypeDefName);
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createAlgebraFunctionAnswerType() {
  return new Type::External(theAlgebraAnswerTypeDefName);
}


Type::Base* SpecializeGrammar::CreateSpecializedGrammar::
  createAlgebraFunctionAlphabetType() {
  return new Type::Char();
}


std::list<CFG::Base*> SpecializeGrammar::CreateSpecializedGrammar::
  orderAlgebraFunctionArguments(
    std::list<CFG::Base*> parameterValues,
    std::list<Fn_Arg::Base*> originalArguments) {
  std::vector<CFG::Base*> orderedList(originalArguments.size());

  // Init all elements to NULL, by this we can find out later
  // which elements of this vector have been updated by actual
  // parameter values.
  for (unsigned int i = 0; i < orderedList.size(); i++) {
    orderedList[i] = NULL;
  }

  // Sort in the paprameter values of the algebra function call.
  // Each actual parameter has or has not its positional attribute,
  // which hints us at which position the actual parameter was applied
  // to the algebra function of the prototype grammar.
  unsigned int actualParameterPosition = 0;
  for (std::list<CFG::Base*>::iterator i = parameterValues.begin();
       i != parameterValues.end(); i++, actualParameterPosition++) {
    Util::ParameterPositionAttribute* positionAttribute =
      getPositionAttribute(*i);
    if (positionAttribute != NULL) {
      int position = positionAttribute->getParameterPosition();
      if (position != -1) {
        assert(((unsigned int)position) < orderedList.size());
        orderedList[position] = *i;
        // And add an attribute which tells us the exact position
        // of the parameter in the actual parameter list of the
        // algebra function.
        (*i)->setAttribute(new ActualParameterPositionAttribute(
          actualParameterPosition));
      }
    }
  }

  // All other values which were not provided by the parameter value
  // list, must be set to some sensible value. In this case it will be
  // the original algebra function argument form the prototype grammar.
  unsigned int pos = 0;
  for (std::list<Fn_Arg::Base*>::iterator i = originalArguments.begin();
       i != originalArguments.end(); i++, pos++) {
    assert(pos < orderedList.size());
    if (orderedList[pos] == NULL) {
      orderedList[pos] = this->gapToCFGTransformer.generateFragment(*i);
    }
  }

  // Now create a list from the vector of ordered parameter values.
  std::list<CFG::Base*> result;
  result.insert(result.end(), orderedList.begin(), orderedList.end());
  return result;
}


Statement::Base* SpecializeGrammar::CreateSpecializedGrammar::alg_append(
  Statement::Var_Decl* variableToAppendTo, std::string* str) {
  Statement::Fn_Call* appendCommaCall = new Statement::Fn_Call("append");
  appendCommaCall->add_arg(*variableToAppendTo);
  appendCommaCall->add_arg(new Expr::Const(new Const::String(*str)));
  return appendCommaCall;
}


Statement::Base* SpecializeGrammar::CreateSpecializedGrammar::alg_append(
  Statement::Var_Decl* variableToAppendTo,
  Statement::Var_Decl* appendedVariable) {
  Statement::Fn_Call* appendCommaCall = new Statement::Fn_Call("append");
  appendCommaCall->add_arg(*variableToAppendTo);
  appendCommaCall->add_arg(*appendedVariable);
  return appendCommaCall;
}


Statement::Base* SpecializeGrammar::CreateSpecializedGrammar::alg_append(
  Statement::Var_Decl* variableToAppendTo, Expr::Vacc* appendedExpr) {
  Statement::Fn_Call* appendCommaCall = new Statement::Fn_Call("append");
  appendCommaCall->add_arg(*variableToAppendTo);
  appendCommaCall->add_arg(appendedExpr);
  return appendCommaCall;
}


Util::AlgebraFunctionInfoAttribute* SpecializeGrammar::
  CreateSpecializedGrammar::getAlgebraFunctionInfoAttribute(
    Util::Attributable* attributableInstance) {
  if (attributableInstance == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = attributableInstance->getAttribute(
    "Util::AlgebraFunctionInfoAttribute");
  Util::AlgebraFunctionInfoAttribute* infoAttribute =
    dynamic_cast<Util::AlgebraFunctionInfoAttribute*>(attribute);

  return infoAttribute;
}


Util::ParameterPositionAttribute* SpecializeGrammar::CreateSpecializedGrammar::
  getPositionAttribute(Util::Attributable* attributableInstance) {
  if (attributableInstance == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = attributableInstance->getAttribute(
    "Util::ParameterPositionAttribute");
  Util::ParameterPositionAttribute* infoAttribute =
    dynamic_cast<Util::ParameterPositionAttribute*>(attribute);

  return infoAttribute;
}


SpecializeGrammar::ActualParameterPositionAttribute* SpecializeGrammar::
  CreateSpecializedGrammar::getActualPositionAttribute(
    Util::Attributable* attributableInstance) {
  if (attributableInstance == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = attributableInstance->getAttribute(
    "SpecializeGrammar::ActualParameterPositionAttribute");
  SpecializeGrammar::ActualParameterPositionAttribute* positionAttribute =
    dynamic_cast<SpecializeGrammar::ActualParameterPositionAttribute*>(
      attribute);

  return positionAttribute;
}


SpecializeGrammar::CycleBreakPointAttribute* SpecializeGrammar::
  CreateSpecializedGrammar::getCycleBreakPointAttribute(
    Util::Attributable* attributableInstance) {
  if (attributableInstance == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = attributableInstance->getAttribute(
    "SpecializeGrammar::CycleBreakPointAttribute");
  SpecializeGrammar::CycleBreakPointAttribute* infoAttribute =
    dynamic_cast<SpecializeGrammar::CycleBreakPointAttribute*>(attribute);

  return infoAttribute;
}


SpecializeGrammar::HiddenCFGFragmentsAttribute* SpecializeGrammar::
  CreateSpecializedGrammar::getHiddenCFGFragmentsAttribute(
    Util::Attributable* attributableInstance) {
  if (attributableInstance == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = attributableInstance->getAttribute(
    "SpecializeGrammar::HiddenCFGFragmentsAttribute");
  HiddenCFGFragmentsAttribute* infoAttribute =
    dynamic_cast<HiddenCFGFragmentsAttribute*>(attribute);

  return infoAttribute;
}


Util::CyclePathInfoAttribute* SpecializeGrammar::CreateSpecializedGrammar::
  getCyclePathInformationAttribute(Util::Attributable* attributableInstance) {
  if (attributableInstance == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = attributableInstance->getAttribute(
    "Util::CyclePathInfoAttribute");
  Util::CyclePathInfoAttribute* infoAttribute =
    dynamic_cast<Util::CyclePathInfoAttribute*>(attribute);

  return infoAttribute;
}


SpecializeGrammar::ChoiceFunctionApplicationAttribute* SpecializeGrammar::
  CreateSpecializedGrammar::getChoiceFunctionApplicationAttribute(
    Util::Attributable* attributableInstance) {
  if (attributableInstance == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = attributableInstance->getAttribute(
    "SpecializeGrammar::ChoiceFunctionApplicationAttribute");
  ChoiceFunctionApplicationAttribute* infoAttribute =
  dynamic_cast<ChoiceFunctionApplicationAttribute*>(attribute);

  return infoAttribute;
}


SpecializeGrammar::DesignatedAxiomAttribute* SpecializeGrammar::
  CreateSpecializedGrammar::getDesignatedAxiomAttribute(
    Util::Attributable* attributableInstance) {
  if (attributableInstance == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = attributableInstance->getAttribute(
    "SpecializeGrammar::DesignatedAxiomAttribute");
  DesignatedAxiomAttribute* infoAttribute =
    dynamic_cast<DesignatedAxiomAttribute*>(attribute);

  return infoAttribute;
}


Util::RegularExpressionInfoAttribute* SpecializeGrammar::
  CreateSpecializedGrammar::getRegularExpressionInfoAttribute(
    CFG::RegularExpression* regexpr) {
  if (regexpr == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = regexpr->getAttribute(
    "Util::RegularExpressionInfoAttribute");
  Util::RegularExpressionInfoAttribute* infoAttribute =
    dynamic_cast<Util::RegularExpressionInfoAttribute*>(attribute);

  return infoAttribute;
}


Util::CycleMarkAttribute* SpecializeGrammar::CreateSpecializedGrammar::
  getCycleMarkAttribute(CFG::Base* b) {
  if (b == NULL) {
    return NULL;
  }

  Util::Attribute* attribute = b->getAttribute("Util::CycleMarkAttribute");
  Util::CycleMarkAttribute* cycleMarkAttribute =(
    Util::CycleMarkAttribute*)attribute;

  return cycleMarkAttribute;
}


Instance* SpecializeGrammar::CreateSpecializedGrammar::createDefaultInstance(
  Grammar* grammar) {
  Loc location;
  Product::Base* product = new Product::Single(new std::string(
    theAlgebraName), location);
  Instance* inst = new Instance(new std::string("inst"), product, grammar);
  return inst;
}
