

#ifndef _GRAMMAR_VM_STACK_HH_
#define _GRAMMAR_VM_STACK_HH_

#include <vector>

#include "../cfg/cfg.hh"
#include "var_info_item.hh"
#include "variable_context.hh"


namespace AmbiguityCFG
{
	
	
	// Forward declaration of the element class used by the stack.
	class GrammarVMStackElement;
	
	
	// The operand stack of the GrammarVM is a simple stack
	// implementation which provides some basic functionality
	// for pushing and popping elements to and from the stack.
	class GrammarVMStack {
		
		private:
			
			// The internal stack that holds all elements.
			std::vector<GrammarVMStackElement*> stack;
			
			
		public:
			
			GrammarVMStack();
			~GrammarVMStack();
			
			// Pushes a new element on top of the stack.
			void push (GrammarVMStackElement* element);
			// Removes the top element from the stack and returns it
			// as result.
			GrammarVMStackElement* pop();
			// Returns the top element of the stack, leaving the stack
			// content unchanged.
			GrammarVMStackElement* peek();
			
			// Resets the contents of the stack, by removing all elements
			// from the stack
			void reset();
			
			
	};
	
	
	// This enumeration defines the possible subclass types a
	// GrammarVMStackElement may have.
	enum GrammarVMStackElementType {PRODUCTION_FRAGMENT_ELEMENT, NAMED_ACCESS_ELEMENT, VAR_DECL_INFO_ELEMENT, MULTI_PRODUCTION_FRAGMENT_ELEMENT, CONTEXT_ELEMENT};
	
	
	// The grammar VM stack element is a wrapper class that
	// facilitates type administration of stack elements. With
	// the help of this class the grammar VM knows exactly which
	// kind of element is on top of the stack.
	class GrammarVMStackElement {
		
		protected:
			
			// The specific type of the subclass.
			GrammarVMStackElementType type;
			
			// Inits this instance and sets the instance type to
			// the value of the parameter.
			GrammarVMStackElement (GrammarVMStackElementType t);
			
			
		public:
			
			virtual ~GrammarVMStackElement();
			
			bool is (GrammarVMStackElementType type);
			GrammarVMStackElementType getType();
			
			virtual GrammarVMStackElement* clone() = 0;
			
			
	};
	
	
	// The wrapper for production fragment elements on the stack.
	class ProductionFragmentGrammarVMStackElement : public GrammarVMStackElement {
		
		private:
			
			// Stores the terminal this instance wraps.
			CFG::Base* productionFragment;
			
			
		public:
			
			ProductionFragmentGrammarVMStackElement (CFG::Base* b);
			~ProductionFragmentGrammarVMStackElement();
			
			// Returns the terminal element that is wrapped by this
			// instance.
			CFG::Base* getProductionFragment();
			
			GrammarVMStackElement* clone();
			
			
	};
	
	
	// Wraps a NamedAccess instance into a GrammarVMStackElement.
	class NamedAccessGrammarVMStackElement : public GrammarVMStackElement {
		
		private:
			
			// Stores the pointer to the named-access-instance.
			NamedAccess* access;
			
			
		public:
			
			NamedAccessGrammarVMStackElement (NamedAccess* element);
			~NamedAccessGrammarVMStackElement();
			
			// Returns the named-access element this instance wraps.
			NamedAccess* getNamedAccessElement();
			
			GrammarVMStackElement* clone();
			
			
	};
	
	
	// Wraps a VarDeclInfoItem for storage on the stack.
	class VarDeclItemGrammarVMStackElement : public GrammarVMStackElement {
		
		private:
			
			// Stores the info item wrapped by this instance.
			VarDeclInfo* infoItem;
			
			
		public:
			
			VarDeclItemGrammarVMStackElement (VarDeclInfo* infoItem);
			~VarDeclItemGrammarVMStackElement();
			
			// Returns the variable information items wrapped in this
			// instance.
			VarDeclInfo* getVarInfoItem();
			
			GrammarVMStackElement* clone();
			
			
	};
	
	
	// A stack element that holds a list of simultaneous
	// valid values.
	class MultiProductionFragmentGrammarVMStackElement : public GrammarVMStackElement {
		
		private:
			
			// The list of production fragments.
			std::vector<CFG::Base*> fragments;
			
			
		public:
			
			MultiProductionFragmentGrammarVMStackElement (std::vector<CFG::Base*> fragments);
			~MultiProductionFragmentGrammarVMStackElement();
			
			// Returns the list of production fragments this stack
			// element instance holds.
			std::vector<CFG::Base*> getProductionFragments();
			
			// Creates a copy of this stack element.
			GrammarVMStackElement* clone();
			
			
	};
	
	
	class ContextGrammarVMStackElement : public GrammarVMStackElement {
		
		private:
			
			// Stores the context pointer this stack element holds.
			MultiVariableContext* context;
			
			
		public:
			
			ContextGrammarVMStackElement (MultiVariableContext* cntxt);
			~ContextGrammarVMStackElement();
			
			// Returns the context this stack element holds.
			MultiVariableContext* getContext();
			
			// Creates a copy of this stack element.
			GrammarVMStackElement* clone();
			
			
	};
	
	
}


#endif	// ifndef _GRAMMAR_VM_STACK_HH_

