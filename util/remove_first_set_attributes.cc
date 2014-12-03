

#include "remove_first_set_attributes.hh"


Util::RemoveFirstSetAttributes::RemoveFirstSetAttributes() {
	
}


Util::RemoveFirstSetAttributes::~RemoveFirstSetAttributes() {
	
}


void Util::RemoveFirstSetAttributes::removeFromGrammar (CFG::CFG* grammar) {
	
	std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
	for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin(); i != productions.end(); i++) {
		removeFromProduction (*i);
	}
	
}


void Util::RemoveFirstSetAttributes::removeFromProduction (CFG::GrammarProduction* production) {
	production->removeAttribute ("Util::FirstSetAttribute");
	production->lhs->removeAttribute ("Util::FirstSetAttribute");
	removeFromBase (production->rhs);
}


void Util::RemoveFirstSetAttributes::removeFromBase (CFG::Base* b) {
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
		}
	}
	b->removeAttribute ("Util::FirstSetAttribute");
}


