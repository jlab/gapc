

#include "remove_cycle_set_attributes.hh"


Util::RemoveCycleSetAttributes::RemoveCycleSetAttributes() {
	
}


Util::RemoveCycleSetAttributes::~RemoveCycleSetAttributes() {
	
}


void Util::RemoveCycleSetAttributes::removeFromGrammar (CFG::CFG* grammar) {
	
	std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
	for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin(); i != productions.end(); i++) {
		removeFromProduction (*i);
	}
	
}


void Util::RemoveCycleSetAttributes::removeFromProduction (CFG::GrammarProduction* production) {
	production->removeAttribute ("Util::CycleAttribute");
	production->removeAttribute ("Util::CycleMarkAttribute");
	production->removeAttribute ("Util::LastCycleElementAttribute");
	production->lhs->removeAttribute ("Util::CycleAttribute");
	production->lhs->removeAttribute ("Util::CycleMarkAttribute");
	production->lhs->removeAttribute ("Util::LastCycleElementAttribute");
	removeFromBase (production->rhs);
}


void Util::RemoveCycleSetAttributes::removeFromBase (CFG::Base* b) {
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
	b->removeAttribute ("Util::CycleAttribute");
	b->removeAttribute ("Util::CycleMarkAttribute");
	b->removeAttribute ("Util::LastCycleElementAttribute");
}

