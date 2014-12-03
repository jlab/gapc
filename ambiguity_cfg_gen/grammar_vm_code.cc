


#include "grammar_vm_code.hh"


AmbiguityCFG::GrammarVMCode::GrammarVMCode (std::string* functionName, std::list<std::string*> parameterNames)
	: functionName (functionName), parameterNames (parameterNames) {
}


AmbiguityCFG::GrammarVMCode::~GrammarVMCode() {
}


void AmbiguityCFG::GrammarVMCode::appendCommand (AmbiguityCFG::GrammarVMCommand* command) {
	this->commands.push_back (command);
}


std::string* AmbiguityCFG::GrammarVMCode::getFunctionName() {
	return this->functionName;
}


std::list<std::string*> AmbiguityCFG::GrammarVMCode::getParameterNames() {
	return this->parameterNames;
}


std::list<AmbiguityCFG::GrammarVMCommand*> AmbiguityCFG::GrammarVMCode::getCode() {
	return this->commands;
}

