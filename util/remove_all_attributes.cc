


#include "remove_all_attributes.hh"


Util::RemoveAllAttributes::RemoveAllAttributes() {
	
}


Util::RemoveAllAttributes::~RemoveAllAttributes() {
	
}


void Util::RemoveAllAttributes::removeFromGrammar (CFG::CFG* grammar) {
	
	std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
	for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin(); i != productions.end(); i++) {
		removeFromProduction (*i);
	}
	
}


void Util::RemoveAllAttributes::removeFromProduction (CFG::GrammarProduction* production) {
	production->clearAttributes();
	production->lhs->clearAttributes();
	removeFromBase (production->rhs);
}


void Util::RemoveAllAttributes::removeFromBase (CFG::Base* b) {
	switch (b->getType()) {
		case CFG::PRODUCTION_SEQUENCE: {
			CFG::ProductionSequence* sequence = dynamic_cast<CFG::ProductionSequence*> (b);
			
			for (CFG::ProductionSequence::iterator i = sequence->begin(); i != sequence->end(); i++) {
				removeFromBase (*i);
			}
			
			break;
		}
		case CFG::PRODUCTION_ALTERNATIVE: {
			CFG::ProductionAlternative* alternative = dynamic_cast<CFG::ProductionAlternative*> (b);
			
			for (CFG::ProductionAlternative::iterator i = alternative->begin(); i != alternative->end(); i++) {
				removeFromBase (*i);
			}
			
			break;
		}
		default: {
			// intentionally left blank
			break;
		}
	}
	b->clearAttributes();
}

