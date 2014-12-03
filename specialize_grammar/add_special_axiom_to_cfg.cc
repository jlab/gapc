

#include "add_special_axiom_to_cfg.hh"
#include "choice_function_application_attribute.hh"
#include "designated_axiom_attribute.hh"


SpecializeGrammar::AddSpecialAxiomToCFG::AddSpecialAxiomToCFG() {
}


SpecializeGrammar::AddSpecialAxiomToCFG::~AddSpecialAxiomToCFG() {
}


void SpecializeGrammar::AddSpecialAxiomToCFG::addSpecialAxiom (CFG::CFG* grammar) {
	// All we need are two more non-terminals, which do not
	// collide in their name-spaces with the existing rule names.
	// We do not make much of an effort here, just using some
	// pretty unusual names.
	CFG::NonTerminal* newAxiom = new CFG::NonTerminal (new std::string ("rule00"));
	CFG::NonTerminal* oldAxiom = (CFG::NonTerminal*)grammar->getAxiom()->clone();
	oldAxiom->clearAttributes();
	CFG::NonTerminal* intermediateNonTerminal = new CFG::NonTerminal (new std::string ("rule01"));
	
	// Create an intermediate grammar-rule, and add an attribute
	// to the rule hinting the gap-code generator of the specialize
	// grammar generator to create a choice function application
	// for this grammar-production.
	CFG::GrammarProduction* intermediateProduction = new CFG::GrammarProduction (intermediateNonTerminal);
	CFG::ProductionAlternative* newIntermediateRHS = new CFG::ProductionAlternative();
	newIntermediateRHS->addAlternative (oldAxiom);
	intermediateProduction->rhs = newIntermediateRHS;
	intermediateProduction->setAttribute (new ChoiceFunctionApplicationAttribute (new std::string ("h")));
	grammar->addProduction (intermediateProduction);
	
	// Create a new axiom-rule, and annotate an attribute which
	// gives a hint that this is the designated axiom grammar-rule.
	CFG::GrammarProduction* newAxiomProduction = new CFG::GrammarProduction (newAxiom);
	CFG::ProductionAlternative* newAxiomRHS = new CFG::ProductionAlternative();
	CFG::NonTerminal* intermediateNonTerminalRHS = (CFG::NonTerminal*)intermediateNonTerminal->clone();
	intermediateNonTerminalRHS->setAttribute (new DesignatedAxiomAttribute (oldAxiom->getName()));
	newAxiomRHS->addAlternative (intermediateNonTerminalRHS);
	newAxiomProduction->rhs = newAxiomRHS;
	grammar->addProduction (newAxiomProduction);
	
	grammar->setAxiom (newAxiom);
}

