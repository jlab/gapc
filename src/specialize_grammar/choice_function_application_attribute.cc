

#include "choice_function_application_attribute.hh"


SpecializeGrammar::ChoiceFunctionApplicationAttribute::ChoiceFunctionApplicationAttribute (std::string* choiceFunctionName)
	: Util::Attribute ("SpecializeGrammar::ChoiceFunctionApplicationAttribute"), choiceFunctionName (choiceFunctionName) {
}


SpecializeGrammar::ChoiceFunctionApplicationAttribute::ChoiceFunctionApplicationAttribute (ChoiceFunctionApplicationAttribute& a)
	: Util::Attribute (a), choiceFunctionName (choiceFunctionName) {
}


SpecializeGrammar::ChoiceFunctionApplicationAttribute::~ChoiceFunctionApplicationAttribute() {
}


std::string* SpecializeGrammar::ChoiceFunctionApplicationAttribute::getChoiceFunctionName() {
	return this->choiceFunctionName;
}


Util::Attribute* SpecializeGrammar::ChoiceFunctionApplicationAttribute::clone() {
	return new ChoiceFunctionApplicationAttribute (*this);
}

