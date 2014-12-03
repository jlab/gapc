

#include "algebra_function_info_attribute.hh"


Util::AlgebraFunctionInfoAttribute::AlgebraFunctionInfoAttribute()
	: Attribute ("Util::AlgebraFunctionInfoAttribute"), algebraFunctionName (NULL), grammarRuleName (NULL), algebraFunctionDefinition (NULL) {
}


Util::AlgebraFunctionInfoAttribute::AlgebraFunctionInfoAttribute (AlgebraFunctionInfoAttribute& a)
	: 	Attribute (a),
		algebraFunctionName (new std::string (*a.algebraFunctionName)),
		grammarRuleName (new std::string (*a.grammarRuleName)),
		algebraFunctionDefinition (a.algebraFunctionDefinition),
		algebraFunctionArgs (a.algebraFunctionArgs) {
}


Util::AlgebraFunctionInfoAttribute::~AlgebraFunctionInfoAttribute() {
}


void Util::AlgebraFunctionInfoAttribute::setAlgebraFunctionName (std::string* name) {
	this->algebraFunctionName = name;
}


std::string* Util::AlgebraFunctionInfoAttribute::getAlgebraFunctionName() {
	return this->algebraFunctionName;
}


void Util::AlgebraFunctionInfoAttribute::setGrammarRuleName (std::string* name) {
	this->grammarRuleName = name;
}


std::string* Util::AlgebraFunctionInfoAttribute::getGrammarRuleName() {
	return this->grammarRuleName;
}


void Util::AlgebraFunctionInfoAttribute::setAlgebraFunctionDefinition (Fn_Def* functionDefinition) {
	this->algebraFunctionDefinition = functionDefinition;
}


Fn_Def* Util::AlgebraFunctionInfoAttribute::getAlgebraFunctionDefinition() {
	return this->algebraFunctionDefinition;
}


void Util::AlgebraFunctionInfoAttribute::setAlgebraFunctionArguments (std::list<Fn_Arg::Base*> args) {
	this->algebraFunctionArgs = args;
}


std::list<Fn_Arg::Base*> Util::AlgebraFunctionInfoAttribute::getAlgebraFunctionArguments() {
	return this->algebraFunctionArgs;
}


Util::Attribute* Util::AlgebraFunctionInfoAttribute::clone() {
	return new AlgebraFunctionInfoAttribute (*this);
}

