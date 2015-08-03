

#include "algebra_function_name_attribute.hh"


Util::AlgebraFunctionNameAttribute::AlgebraFunctionNameAttribute (std::string* algebraFunctionName)
	: Attribute ("Util::AlgebraFunctionNameAttribute"), algebraFunctionName (algebraFunctionName) {
}


Util::AlgebraFunctionNameAttribute::AlgebraFunctionNameAttribute (AlgebraFunctionNameAttribute& a)
	: Attribute (a), algebraFunctionName (new std::string (*a.algebraFunctionName)) {
}


Util::AlgebraFunctionNameAttribute::~AlgebraFunctionNameAttribute() {
}


std::string* Util::AlgebraFunctionNameAttribute::getAlgebraFunctionName() {
	return this->algebraFunctionName;
}


Util::Attribute* Util::AlgebraFunctionNameAttribute::clone() {
	return new AlgebraFunctionNameAttribute (*this);
}

