

#include "remove_unused_productions.hh"


#include <iostream>


CFG::UnusedProductionsRemover::UnusedProductionsRemover() {
	
}


CFG::UnusedProductionsRemover::~UnusedProductionsRemover() {
	
}


CFG::CFG* CFG::UnusedProductionsRemover::removeUnusedProductions (CFG* grammar) {
	this->oldGrammar = grammar;
	this->newGrammar = new CFG();
	
	// Copy all reachable non-terminals into the new grammar.
	removeFromProductions();
	
	// Set the axiom of the new grammar.
	this->newGrammar->setAxiom (this->oldGrammar->getAxiom());
	
	return this->newGrammar;
}


void CFG::UnusedProductionsRemover::removeFromProductions() {
	GrammarProduction* production = this->oldGrammar->getProduction (this->oldGrammar->getAxiom());
	std::set<std::string>* visitedNonTerminals = new std::set<std::string>();
	removeFromProduction (production, visitedNonTerminals);
	delete (visitedNonTerminals);
}


void CFG::UnusedProductionsRemover::removeFromProduction (GrammarProduction* production, std::set<std::string>* visitedNonTerminals) {
	visitedNonTerminals->insert (*production->lhs->getName());
	this->newGrammar->addProduction (production);
	removeFromBase (production->rhs, visitedNonTerminals);
}


void CFG::UnusedProductionsRemover::removeFromBase (Base* b, std::set<std::string>* visitedNonTerminals) {
	switch (b->getType()) {
		case BASE_WRAPPER: {
			BaseWrapper* wrapper = dynamic_cast<BaseWrapper*> (b);
			
			removeFromBase (wrapper->getWrappedBase(), visitedNonTerminals);
			
			break;
		}
		case NONTERMINAL: {
			NonTerminal* nonTerminal = dynamic_cast<NonTerminal*> (b);
			
			if (visitedNonTerminals->find (*nonTerminal->getName()) == visitedNonTerminals->end()) {
				GrammarProduction* production = this->oldGrammar->getProduction (nonTerminal);
				removeFromProduction (production, visitedNonTerminals);
			}
			
			break;
		}
		case PRODUCTION_SEQUENCE: {
			ProductionSequence* sequence = dynamic_cast<ProductionSequence*> (b);
			
			for (ProductionSequence::iterator i = sequence->begin(); i != sequence->end(); i++) {
				removeFromBase (*i, visitedNonTerminals);
			}
			
			break;
		}
		case PRODUCTION_ALTERNATIVE: {
			ProductionAlternative* alternative = dynamic_cast<ProductionAlternative*> (b);
			
			for (ProductionAlternative::iterator i = alternative->begin(); i != alternative->end(); i++) {
				removeFromBase (*i, visitedNonTerminals);
			}
			
			break;
		}
		default: {
		}
	}
}

