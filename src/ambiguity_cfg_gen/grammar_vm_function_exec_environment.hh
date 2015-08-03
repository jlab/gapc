


#ifndef _GRAMMAR_VM_FUNCTION_EXEC_ENVIRONMENT_HH_
#define _GRAMMAR_VM_FUNCTION_EXEC_ENVIRONMENT_HH_


#include <list>

#include "grammar_vm.hh"
#include "grammar_vm_code.hh"
#include "grammar_vm_command.hh"
#include "grammar_vm_stack.hh"
#include "variable_context.hh"


namespace AmbiguityCFG
{
	
	
	// Forward declaration of the grammar VM, since the two files
	// for this header and the grammar VM's header are circular
	// dependent.
	class GrammarVM;
	
	
	// The Grammar Virtual Machine (GrammarVM) is used to compute
	// grammar production fragments based on the current state of
	// a variable context. Basically this VM provides means to access
	// and manipulate the values of variables stored in the context.
	class GrammarVMFunctionExecEnvironment {
		
		private:
			
			// Holds a reference to the VM instance which owns
			// this function exec environment.
			GrammarVM* vm;
			
			// The stack of this grammar VM.
			GrammarVMStack stack;
			
			// The global variable context which contains all defined
			// variables and their current values.
			MultiVariableContext* context;
			
			// stores the code that will be executed by this VM
			GrammarVMCode* code;
			
			// The result of the current algebra function.
			std::list<CFG::Base*> functionResult;
			
			
		public:
			
			GrammarVMFunctionExecEnvironment (GrammarVM* vm, GrammarVMCode* code);
			~GrammarVMFunctionExecEnvironment();
			
			// Executes a block of GrammarVM-code.
			void executeCode (VariableContext* c);
			
			// Executes a block of GrammarVM-code.
			void executeCode (MultiVariableContext* c);
			
			// Executes a single command.
			void execute (GrammarVMCommand* cmd);
			// Executes a list of commands by using the iterator of the
			// list given as parameter and calling the execute-function
			// for a single command for each command in the list.
			void execute (std::list<GrammarVMCommand*> cmds);
			
			// Resets the grammar VM and its internal registers.
			// All stored values and registers are set to their
			// start values.
			void reset();
			
			// Returns the result that was assembled by the commands
			// executed by this grammar VM.
			CFG::Base* getResultProductionFragment();
			
			
		private:
			
			void executeLoadVar (LoadVarCommand* loadVarCmd);
			void executeStoreVar (StoreVarCommand* storeVarCmd);
			void executeCombine();
			void executeReturn();
			void executeCallFunction (CallFunctionCommand* callFunction);
			
			// Returns a production fragment that is stored in the
			// VarInfoItem. It is required that the parameter is
			// of the sub-class VarDeclInfoItem, otherwise an exception
			// is thrown.
			CFG::Base* extractProductionFragmentFromVarInfoItem (VarInfoItem* infoItem);
			
			// Creates a string representation of the production
			// fragment and returns it as a pointer.
			std::string* createStringForProductionFragment (CFG::Base* productionFragment);
			// Creates a string representation of the vector of
			// production fragments and returns them as a pointer
			// to that string.
			std::string* createStringForProductionFragments (std::vector<CFG::Base*> fragments);
			
			
	};
	
	
}



#endif	// ifndef _GRAMMAR_VM_FUNCTION_EXEC_ENVIRONMENT_HH_

