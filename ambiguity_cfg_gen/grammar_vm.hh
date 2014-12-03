

#ifndef _GRAMMAR_VM_HH_
#define _GRAMMAR_VM_HH_

#include <list>
#include <set>
#include <vector>

#include "grammar_vm_code.hh"
#include "grammar_vm_command.hh"
#include "grammar_vm_function_exec_environment.hh"
#include "grammar_vm_stack.hh"
#include "variable_context.hh"



namespace AmbiguityCFG
{
	
	
	// The Grammar Virtual Machine (GrammarVM) is used to compute
	// grammar production fragments based on the current state of
	// a variable context. Basically this VM provides means to access
	// and manipulate the values of variables stored in the context.
	class GrammarVM {
		
		private:
			
			// The list of precompiled algebra functions is
			// stored as a map.
			hashtable<std::string, GrammarVMCode*> algebraFunctions;
			
			// This set holds the names of all called functions.
			// It is used for recursion-detection. Recursion may
			// not occur, because we can not calculate when the
			// base case of recursion is reached.
			std::set<std::string> calledFunctionNames;
			
			// This stack contains the names of called functions.
			// basically this information is used to print a stack
			// trace when a recursion was detected.
			std::vector<std::string> calledFunctionNamesStack;
			
			
		public:
			
			GrammarVM();
			~GrammarVM();
			
			
			// Adds a function definition to this grammar VM.
			void addCompiledFunctionCode (GrammarVMCode* code);
			
			// Executes a function which is referenced simply by
			// its name. As a second argument the context of initial
			// parameter values is passed. The result of the algebra
			// function is returned as a pointer to a grammar fragment.
			CFG::Base* executeFunction (std::string* functionName, VariableContext* c);
			
			// Executes a function which is referenced simply by
			// its name. As a second argument the context of initial
			// parameter values is passed. The result of the algebra
			// function is returned as a pointer to a grammar fragment.
			CFG::Base* executeFunction (std::string* functionName, MultiVariableContext* c);
			
			// Returns the list of parameter names of the function
			// named by the given parameter. This method returns an
			// empty list, if the function name is not defined in
			// this VM. Please note that this situation is not
			// distinguishable from that one when a function is
			// defined, but does not expect any parameters at all.
			std::list<std::string*> getParameterNamesForFunction (std::string* functionName);
			
			
	};
	
	
}


#endif	// ifndef _GRAMMAR_VM_HH_

