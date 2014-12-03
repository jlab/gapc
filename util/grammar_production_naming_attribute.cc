

#include "grammar_production_naming_attribute.hh"


Util::GrammarProductionNamingAttribute::GrammarProductionNamingAttribute (std::string* originalName)
	: Attribute ("Util::GrammarProductionNamingAttribute"), originalName (originalName) {
}


Util::GrammarProductionNamingAttribute::GrammarProductionNamingAttribute (GrammarProductionNamingAttribute& a)
	: Attribute (a), originalName (new std::string (*a.originalName)) {
}


Util::GrammarProductionNamingAttribute::~GrammarProductionNamingAttribute() {
}


std::string* Util::GrammarProductionNamingAttribute::getOriginalName() {
	return this->originalName;
}


Util::Attribute* Util::GrammarProductionNamingAttribute::clone() {
	return new GrammarProductionNamingAttribute (*this);
}

