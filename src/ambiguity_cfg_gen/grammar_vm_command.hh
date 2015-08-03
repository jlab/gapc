

#ifndef _GRAMMAR_VM_COMMAND_HH_
#define _GRAMMAR_VM_COMMAND_HH_

#include <string>

#include "../loc.hh"

#include "../cfg/cfg.hh"
#include "variable_context.hh"


namespace AmbiguityCFG
{
	
	
	// Defines all types of grammar VM commands. The types will be used
	// to distinguish all subclasses of the GrammarVMCommand base class.
	enum GrammarVMCommandType {LOAD_INT, LOAD_CHAR, LOAD_STRING, LOAD_THIS, LOAD_VAR, STORE_VAR, CREATE_VAR, LOAD_CONTEXT, STORE_CONTEXT, WRAP_CONTEXT, MERGE_CONTEXTS, CLEAR_CONTEXT, CLEAR_LOCAL_CONTEXT, CREATE_EPSILON, CREATE_TERMINAL, CREATE_NONTERMINAL, CREATE_PRODUCTION_FRAGMENT, COMBINE, RETURN, DUPLICATE, SWAP, POP_STACK, CALL_FUNCTION};
	
	
	// This is the base class of all grammar VM commands.
	class GrammarVMCommand {
		
		private:
			
			// All subclasses of this class must provide a type
			// value, which can be used to find out the instance's
			// subclass type.
			GrammarVMCommandType type;
			
			// Stores the location of the statements in the gapc
			// code which produced this command while compilation.
			Loc& location;
			
			
		public:
			
			GrammarVMCommand (GrammarVMCommandType type, Loc& location);
			virtual ~GrammarVMCommand() = 0;
			
			// Checks the type of the subclass of this instance.
			bool is (GrammarVMCommandType type);
			// Gets the subclass type of the instance.
			GrammarVMCommandType getType();
			
			// Returns the location in the gapc source-code from which
			// this command was compiled.
			Loc getLocation();
			
			
	};
	
	
	// Loads an integer constant on the stack with the value
	// given as parameter to the constructor.
	class LoadIntCommand : public GrammarVMCommand {
		
		private:
			
			// Stores the constant value this command must load
			// onto the stack of the VM.
			int value;
			
			
		public:
			
			LoadIntCommand (Loc& location, int value);
			~LoadIntCommand();
			
			// Returns the integer value that is loaded by this command
			// onto the stack of the VM.
			int getValue();
			
			
	};
	
	
	// Loads the reference of the global variable context
	// onto the stack.
	class LoadThisCommand : public GrammarVMCommand {
		
		public:
			
			LoadThisCommand (Loc& location);
			~LoadThisCommand();
			
			
	};
	
	
	// Loads a variable definition from the variable context
	// for a variable with the name given as parameter in the
	// constructor.
	class LoadVarCommand : public GrammarVMCommand {
		
		private:
			
			std::string* variableName;
			
			
		public:
			
			LoadVarCommand (Loc& location, std::string* variableName);
			~LoadVarCommand();
			
			// Returns the name of the variable which must
			// be loaded from the variable context.
			std::string* getVariableName();
			
	};
	
	
	// Stores the production fragment that is found on top
	// of the stack into a variable.
	class StoreVarCommand : public GrammarVMCommand {
		
		private:
			
			std::string* variableName;
			
			
		public:
			
			StoreVarCommand (Loc& location, std::string* variableName);
			~StoreVarCommand();
			
			std::string* getVariableName();
			
			
	};
	
	
	// Creates a new variable info item in the current context.
	class CreateVarCommand : public GrammarVMCommand {
		
		private:
			
			// The name of the variable that will be defined in
			// the current context.
			std::string* variableName;
			
			
		public:
			
			CreateVarCommand (Loc& location, std::string* variableName);
			~CreateVarCommand();
			
			// Returns the name of the variable to be defined.
			std::string* getVariableName();
			
			
	};
	
	
	// Loads the current context onto the stack.
	class LoadContextCommand : public GrammarVMCommand {
		
		public:
			
			LoadContextCommand (Loc& location);
			~LoadContextCommand();
			
			
	};
	
	
	// Stores the context, which is stored on top of the stack, as
	// the current context. The top element of the stack is required
	// to be a context, otherwise an exception is thrown.
	class StoreContextCommand : public GrammarVMCommand {
		
		public:
			
			StoreContextCommand (Loc& location);
			~StoreContextCommand();
			
			
	};
	
	
	// Wraps the stack top element (which must be a context) into
	// a new context instance. In this way a new nesting level is
	// created.
	class WrapContextCommand : public GrammarVMCommand {
		
		public:
			
			WrapContextCommand (Loc& location);
			~WrapContextCommand();
			
			
	};
	
	
	// Merges the two contexts that are located on the top of
	// the stack. If the two topmost elements are not context
	// instances, an error is thrown.
	class MergeContextsCommand : public GrammarVMCommand {
		
		public:
			
			MergeContextsCommand (Loc& location);
			~MergeContextsCommand();
			
			
	};
	
	
	// Clears the content of the context that is at the top
	// of the stack. If no variable-context can be found on
	// the top of the stack, an exception is thrown.
	class ClearContextCommand : public GrammarVMCommand {
		
		public:
			
			ClearContextCommand (Loc& location);
			~ClearContextCommand();
			
			
	};
	
	
	// Clears all local definitions from the context, but leaving
	// changes to variables that have been defined in the parent
	// context untouched.
	class ClearLocalContextCommand : public GrammarVMCommand {
		
		public:
			
			ClearLocalContextCommand (Loc& location);
			~ClearLocalContextCommand();
			
			
	};
	
	
	// Creates an epsilon grammar fragment on top of the VM stack.
	class CreateEpsilonCommand : public GrammarVMCommand {
		
		public:
			
			CreateEpsilonCommand (Loc& location);
			~CreateEpsilonCommand();
			
			
	};
	
	
	// Creates a terminal on the stack with the given string
	// as terminal sequence.
	class CreateTerminalCommand : public GrammarVMCommand {
		
		private:
			
			// Stores the string of terminal symbols of which
			// the grammar terminal will be constructed.
			std::string* terminal;
			
			
		public:
			
			CreateTerminalCommand (Loc& location, std::string* terminal);
			~CreateTerminalCommand();
			
			// Returns the string of terminal symbols that represent
			// the terminal.
			std::string* getTerminalString();
			
			
	};
	
	
	// Creates a non-terminal on the stack with the given string
	// as non-terminal name.
	class CreateNonTerminalCommand : GrammarVMCommand {
		
		private:
			
			// Stores the non-terminal name.
			std::string* name;
			
			
		public:
			
			CreateNonTerminalCommand (Loc& location, std::string* name);
			~CreateNonTerminalCommand();
			
			// Returns the name of the non-terminal.
			std::string* getNonTerminalName();
			
			
	};
	
	
	// Creates a constant production fragment on the stack.
	class CreateProductionFragmentCommand : public GrammarVMCommand {
		
		private:
			
			// The production fragment that must be loaded
			// onto the stack is stored here:
			CFG::Base* productionFragment;
			
			
		public:
			
			CreateProductionFragmentCommand (Loc& location, CFG::Base* b);
			~CreateProductionFragmentCommand();
			
			// Returns the production fragment that must be
			// placed on top of the stack.
			CFG::Base* getProductionFragment();
			
			
	};
	
	
	// This command combines the two elements on top of
	// the stack into a new result.
	class CombineCommand : public GrammarVMCommand {
		
		public:
			
			CombineCommand (Loc& location);
			~CombineCommand();
			
			
	};
	
	
	
	// The return command adds the top element of the stack to the
	// list of result expressions for the current algebra function.
	class ReturnCommand : public GrammarVMCommand {
		
		public:
			
			ReturnCommand (Loc& location);
			~ReturnCommand();
			
			
	};
	
	
	// Duplicates the top element of the stack and puts it on the top
	// of the stack, making the stack containing two elements on top
	// which represent the same value.
	class DuplicateCommand : public GrammarVMCommand {
		
		public:
			
			DuplicateCommand (Loc& location);
			~DuplicateCommand();
			
			
	};
	
	
	// Swaps the two top elements on the stack. This commands requires
	// at least two elements on the stack, otherwise an exception is
	// thrown.
	class SwapCommand : public GrammarVMCommand {
		
		public:
			
			SwapCommand (Loc& location);
			~SwapCommand();
			
			
	};
	
	
	// Removes the top element of the stack. This  requires that the stack
	// contains at least one element.
	class PopStackCommand : public GrammarVMCommand {
		
		public:
			
			PopStackCommand (Loc& location);
			~PopStackCommand();
			
			
	};
	
	
	// Calls an algebra function from within an other function.
	// For a function call the stack must contain at least n
	// production fragments (or multi-sets of those), where n
	// is the arity of the function. These arguments are collected
	// into a new variable context which represents the initial
	// context of the called function. The result of the function
	// is pushed onto the stack of the calling function.
	class CallFunctionCommand : public GrammarVMCommand {
		
		private:
			
			// The name of the function that is invoked by
			// this command.
			std::string* functionName;
			
			// The list of names of all parameters in exactly
			// that order they appear on the signature of the
			// function definition.
			std::list<std::string*> parameterNames;
			
			
		public:
			
			CallFunctionCommand (Loc& location, std::string* functionName, std::list<std::string*> &parameterNames);
			~CallFunctionCommand();
			
			// Returns the name of the function to be called.
			std::string* getFunctionName();
			// Returns the list of all parameter names.
			std::list<std::string*> getParameterNames();
			
			
	};
	
	
}



#endif	// ifndef _GRAMMAR_VM_COMMAND_HH_

