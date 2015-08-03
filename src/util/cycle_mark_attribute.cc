

#include "cycle_mark_attribute.hh"


Util::CycleMarkAttribute::CycleMarkAttribute()
	: Attribute ("Util::CycleMarkAttribute") {
}


Util::CycleMarkAttribute::CycleMarkAttribute (CycleMarkAttribute& a)
	: Attribute (a), cycleSets (a.cycleSets) {
}


Util::CycleMarkAttribute::~CycleMarkAttribute() {
}


void Util::CycleMarkAttribute::addCycleSet (CycleSet* set) {
	this->cycleSets.insert (set);
}


bool Util::CycleMarkAttribute::containsCycleSet (CycleSet* set) {
	for (std::set<CycleSet*>::iterator i = this->cycleSets.begin(); i != this->cycleSets.end(); i++) {
		if (*(*i) == *set) {
			return true;
		}
	}
	return false;
}


std::set<Util::CycleSet*> Util::CycleMarkAttribute::getCycleSets() {
	return this->cycleSets;
}


Util::Attribute* Util::CycleMarkAttribute::clone() {
	return new CycleMarkAttribute (*this);
}

