

#ifndef _VARIABLE_CONTEXT_HH_
#define _VARIABLE_CONTEXT_HH_

#include <set>
#include <vector>

#include "../hashtable.hh"

#include "../util/symbol_table.cc"
#include "var_info_item.hh"


namespace AmbiguityCFG {
	
	
	// Creates an association of variable names and
	// their current computational value. This store
	// is layered, which means that a new instance can
	// be wrapped around an existing one. When an old
	// instance is wrapped inside of a new instance,
	// a variable identifier that has already been used
	// in the old instance can be defined again, which
	// will shadow the old version of the variable.
	// when the wrapping instance is removed, the old
	// value is visible again. This technique is used
	// to create variable redefinition inside of nested
	// scope of visibility, e.g. with nested blocks.
	class VariableContext : public NamedAccess {
		
		private:
			
			// Holds a pointer to the parent variable-store. If this
			// is the root variable-store, the pointer is set to NULL.
			//VariableContext* parentContext;
			
			// Provides a mapping between variable names and
			// variable-information-items.
			//hashtable<std::string, VarInfoItem*> declaredVariables;
			Util::SymbolTable<std::string, VarInfoItem*>* declaredVariables;
			
			// A list that holds all locally defined variables. A
			// variable is locally defined, if it has a declaration
			// in the current block. We need this list to distinguish
			// locally defined entries from those which overwrite
			// global values.
			std::set<std::string> localDefinitions;
			
			// Stores all variables, that have been copied but not stored
			// in the local declared variable section. This hashtable stores
			// all elements, which had only read access but no write access.
			hashtable<std::string, VarInfoItem*> copiedButNotStoredVariables;

			// If a returns statement occurs at the end of a list
			// of statements, we close the variable context. When closed,
			// no other variable info item should be changed.
			bool closed;
			
			
		public:
			
			// Inits a new instance of this variable-store.
			VariableContext();
			// Inits a new instance of this variable-store with the
			// given parent instance.
			VariableContext (VariableContext* parent);
			~VariableContext();
			
			// Clears the whole contents of the variable-store.
			void clear();
			
			// Clears the local content of this context.
			void removeLocalChanges();
			// Removes all items that are only defined in this context
			// locally, but not in the parent context.
			void removeLocallyAddedItems();
			
			// Adds the variable name to the list of locally defined
			// variables. Variables which are marked with this method
			// will be handled differently when the local context is
			// cleared.
			void defineLocalVariable (std::string* variableName);
			
			
			// Closes this variable context.
			void close();
			// Determines whether this variable context is closed.
			bool isClosed();
			
			// Inserts a variable info item into the store.
			void setVarInfoItem (std::string* name, VarInfoItem* infoItem);
			// Returns the variable info item stored for the given name,
			// or produces an assertion violation if there is no info
			// item stored for the given name. This method should only
			// be called if testing with containsVariableInfoItem (std::string* variableName)
			// returned true for the same variable name.
			VarInfoItem* getVarInfoItem (std::string* variableName);
			// Returns TRUE if the variable store contains an info
			// item for the given variable name, otherwise FALSE.
			bool containsVarInfoItem (std::string* variableName);
			// Returns TRUE if the variable is stored directly in
			// this variable context, not in its parent context.
			bool containsVarInfoItemDirectly (std::string* variableName);
			
			
	};
	
	
	// A MultiVariableContext is a super-context for a set of VariableContexts.
	// Each single variable context in this multi-variable context represents
	// a valid setting of all variables at a given time.
	class MultiVariableContext : public NamedAccess {
		
		private:
			
			// The list of all VariableContexts this multi-context
			// holds.
			std::vector<VariableContext*> contexts;
			
			
		public:
			
			MultiVariableContext();
			MultiVariableContext (VariableContext* parentContext);
			MultiVariableContext (MultiVariableContext* parentContext);
			MultiVariableContext (MultiVariableContext* fstContext, MultiVariableContext* sndContext);
			~MultiVariableContext();
			
			// Clears the local content of this context.
			void removeLocalChanges();
			// Removes all items that are only defined in this context
			// locally, but not in the parent context.
			void removeLocallyAddedItems();
			
			// Adds the variable name to the list of locally defined
			// variables. Variables which are marked with this method
			// will be handled differently when the local context is
			// cleared.
			void defineLocalVariable (std::string* variableName);
			
			// Inserts a variable info item into the store.
			void setVarInfoItem (std::string* name, VarInfoItem* infoItem);
			// Returns the variable info item stored for the given name,
			// or produces an assertion violation if there is no info
			// item stored for the given name. This method should only
			// be called if testing with containsVariableInfoItem (std::string* variableName)
			// returned true for the same variable name.
			VarInfoItem* getVarInfoItem (std::string* variableName);
			// Returns TRUE if the variable store contains an info
			// item for the given variable name, otherwise FALSE.
			bool containsVarInfoItem (std::string* variableName);
			// Returns TRUE if the variable is stored directly in
			// this variable context, not in its parent context.
			bool containsVarInfoItemDirectly (std::string* variableName);
			
			
	};
	
	
}


#endif	// ifndef _VARIABLE_CONTEXT_HH_

