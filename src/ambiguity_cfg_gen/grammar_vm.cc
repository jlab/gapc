

#include <sstream>
//#include <boost/format.hpp>

#include "../log.hh"

#include "grammar_vm.hh"
#include "../cfg/cfg.hh"


AmbiguityCFG::GrammarVM::GrammarVM() {
}


AmbiguityCFG::GrammarVM::~GrammarVM() {
}


void AmbiguityCFG::GrammarVM::addCompiledFunctionCode (GrammarVMCode* code) {
	this->algebraFunctions[*code->getFunctionName()] = code;
}


CFG::Base* AmbiguityCFG::GrammarVM::executeFunction (std::string* functionName, AmbiguityCFG::VariableContext* c) {
	return this->executeFunction (functionName, new MultiVariableContext (c));
}


CFG::Base* AmbiguityCFG::GrammarVM::executeFunction (std::string* functionName, AmbiguityCFG::MultiVariableContext* c) {
	// Check if this is a recursive call. If the function name is already
	// present in the set of all called functions during this execution,
	// we generate an error message including a stack trace, indicating
	// what chain of function calls led to the error.
	if (this->calledFunctionNames.find (*functionName) != this->calledFunctionNames.end()) {
		std::ostringstream strStream;
		strStream << "gap-00178: recursive function call of function '" + *functionName + "'" << std::endl;
		strStream << "Stack trace:" << std::endl;
		bool firstStackElement = true;
		for (std::vector<std::string>::reverse_iterator i = this->calledFunctionNamesStack.rbegin(); i != this->calledFunctionNamesStack.rend(); ++i) {
			if (!firstStackElement) {
				strStream << " called by" << std::endl;
			}
			firstStackElement = false;
			strStream << "\t'" << (*i) << "'";
		}
		throw LogError (strStream.str());
	}
	
	
	// Insert the function name into the set of called functions.
	this->calledFunctionNames.insert (*functionName);
	this->calledFunctionNamesStack.push_back (*functionName);
	
	
	// Try to find the code definition for the algebra function.
	if (this->algebraFunctions.find (*functionName) == this->algebraFunctions.end()) {
		throw LogError ("gap-00178: Unknown function '" + *functionName + "'");
	}
	GrammarVMCode* code = this->algebraFunctions[*functionName];
	
	// Now we create a grammar VM function execution environment
	// where the code can be executed.
	GrammarVMFunctionExecEnvironment executionEnv (this, code);
	executionEnv.executeCode (c);
	
	
	// Before we leave this method, remove the function name
	// from the set of called functions.
	this->calledFunctionNames.erase (*functionName);
	this->calledFunctionNamesStack.pop_back();
	
	
	// Then return the resultant production fragment.
	return executionEnv.getResultProductionFragment();
}


std::list<std::string*> AmbiguityCFG::GrammarVM::getParameterNamesForFunction (std::string* functionName) {
	if (this->algebraFunctions.find (*functionName) != this->algebraFunctions.end()) {
		GrammarVMCode* code = this->algebraFunctions[*functionName];
		return code->getParameterNames();
	}
	// If no code is available for the requested function name,
	// just return an empty list.
	std::list<std::string*> parameterNames;
	return parameterNames;
}

