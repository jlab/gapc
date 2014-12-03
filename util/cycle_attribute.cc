

#include "cycle_attribute.hh"


Util::CycleAttribute::CycleAttribute (std::set<CycleSet*> cycleSets)
	: Attribute ("Util::CycleAttribute") {
	this->addCycleSets (cycleSets);
}


Util::CycleAttribute::CycleAttribute (CycleAttribute& a)
	: Attribute (a) {
	this->addCycleSets (a.cycleSets);
}


Util::CycleAttribute::~CycleAttribute() {
}


std::set<Util::CycleSet*> Util::CycleAttribute::getCycleSets() {
	return this->cycleSets;
}


Util::Attribute* Util::CycleAttribute::clone() {
	CycleAttribute* copy = new CycleAttribute (this->cycleSets);
	return copy;
}


bool Util::CycleAttribute::containsCycleSet (CycleSet* set) {
	for (std::set<CycleSet*>::iterator i = this->cycleSets.begin(); i != this->cycleSets.end(); i++) {
		if (*(*i) == *set) {
			return true;
		}
	}
	return false;
}


void Util::CycleAttribute::addCycleSets (std::set<CycleSet*> sets) {
	for (std::set<CycleSet*>::iterator i = sets.begin(); i != sets.end(); i++) {
		if (!this->containsCycleSet (*i)) {
			this->cycleSets.insert (*i);
		}
	}
}

