

#ifndef _GRAMMAR_VM_CODE_HH_
#define _GRAMMAR_VM_CODE_HH_


#include <list>
#include <string>


#include "grammar_vm_command.hh"


namespace AmbiguityCFG {
	
	
	// This is a container class that holds code for the GrammarVM.
	// A single code represents a single algebra function. Each
	// algebra function has exactly one GrammarVMCode that is compiled
	// by the ambiguity-CFG-generator.
	class GrammarVMCode {
		
		private:
			
			// Stores the name of the algebra function.
			std::string* functionName;
			
			// A list of all parameter names this function has.
			std::list<std::string*> parameterNames;
			
			// Stores the list of commands the compiled code
			// consists of.
			std::list<GrammarVMCommand*> commands;
			
			
		public:
			
			GrammarVMCode (std::string* functionName, std::list<std::string*> parameterNames);
			~GrammarVMCode();
			
			
			// Appends a command to the list of commands.
			void appendCommand (GrammarVMCommand* command);
			
			// Returns the name of the algebra function that was
			// used to generate this code.
			std::string* getFunctionName();
			
			// Returns the list of names of the paramters.
			std::list<std::string*> getParameterNames();
			
			// Returns the list of GrammarVM commands this code
			// consists of.
			std::list<GrammarVMCommand*> getCode();
			
			
	};
	
	
}


#endif	// ifndef _GRAMMAR_VM_CODE_HH_


