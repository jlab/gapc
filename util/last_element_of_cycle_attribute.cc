

#include "last_element_of_cycle_attribute.hh"


Util::LastCycleElementAttribute::LastCycleElementAttribute()
	: Attribute ("Util::LastCycleElementAttribute") {
}


Util::LastCycleElementAttribute::LastCycleElementAttribute (LastCycleElementAttribute& a)
	: Attribute (a) {
	for (iterator i = a.begin(); i != a.end(); i++) {
		this->cycleSets.insert (*i);
	}
}


Util::LastCycleElementAttribute::~LastCycleElementAttribute() {
}


void Util::LastCycleElementAttribute::addCycleSet (CycleSet* cycleSet) {
	this->cycleSets.insert (cycleSet);
}


std::set<Util::CycleSet*> Util::LastCycleElementAttribute::getCycleSets() {
	return this->cycleSets;
}


Util::LastCycleElementAttribute::iterator Util::LastCycleElementAttribute::begin() {
	return this->cycleSets.begin();
}


Util::LastCycleElementAttribute::iterator Util::LastCycleElementAttribute::end() {
	return this->cycleSets.end();
}


Util::Attribute* Util::LastCycleElementAttribute::clone() {
	return new LastCycleElementAttribute (*this);
}


