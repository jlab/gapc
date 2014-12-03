

#include "hidden_cfg_fragments_attribute.hh"


SpecializeGrammar::HiddenCFGFragmentsAttribute::HiddenCFGFragmentsAttribute()
	: Attribute ("SpecializeGrammar::HiddenCFGFragmentsAttribute") {
}


SpecializeGrammar::HiddenCFGFragmentsAttribute::HiddenCFGFragmentsAttribute (HiddenCFGFragmentsAttribute& a)
	: Attribute (a), hiddenFragments (a.hiddenFragments) {
}


SpecializeGrammar::HiddenCFGFragmentsAttribute::~HiddenCFGFragmentsAttribute() {
}


void SpecializeGrammar::HiddenCFGFragmentsAttribute::addHiddenFragment (CFG::Base* b) {
	this->hiddenFragments.push_back (b);
}


void SpecializeGrammar::HiddenCFGFragmentsAttribute::addHiddenFragments (std::set<CFG::Base*>* fragments) {
	for (std::set<CFG::Base*>::iterator i = fragments->begin(); i != fragments->end(); i++) {
		addHiddenFragment (*i);
	}
}


SpecializeGrammar::HiddenCFGFragmentsAttribute::iterator SpecializeGrammar::HiddenCFGFragmentsAttribute::begin() {
	return this->hiddenFragments.begin();
}


SpecializeGrammar::HiddenCFGFragmentsAttribute::iterator SpecializeGrammar::HiddenCFGFragmentsAttribute::end() {
	return this->hiddenFragments.end();
}


Util::Attribute* SpecializeGrammar::HiddenCFGFragmentsAttribute::clone() {
	return new HiddenCFGFragmentsAttribute (*this);
}


