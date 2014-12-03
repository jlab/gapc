

#include "designated_axiom_attribute.hh"


SpecializeGrammar::DesignatedAxiomAttribute::DesignatedAxiomAttribute (std::string* originalAxiomName)
	: Util::Attribute ("SpecializeGrammar::DesignatedAxiomAttribute"), originalAxiomName (originalAxiomName) {
}


SpecializeGrammar::DesignatedAxiomAttribute::DesignatedAxiomAttribute (DesignatedAxiomAttribute& a)
	: Util::Attribute (a) {
}


SpecializeGrammar::DesignatedAxiomAttribute::~DesignatedAxiomAttribute() {
}


std::string* SpecializeGrammar::DesignatedAxiomAttribute::getOriginalAxiomName() {
	return this->originalAxiomName;
}


Util::Attribute* SpecializeGrammar::DesignatedAxiomAttribute::clone() {
	return new DesignatedAxiomAttribute (*this);
}

