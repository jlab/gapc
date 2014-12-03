

#ifndef __CALL_TRACE_HH__
#define __CALL_TRACE_HH__


#include <map>
#include <vector>

#include "../util/annotate_cycles.hh"
#include "../util/naming_path.hh"
#include "set_of_cycle_sets.hh"


namespace Util {
	
	
	class CallTrace {
		
		private:
			
			typedef std::pair<std::string, Util::SetOfCycleSets*> PairedElementType;
			
			std::vector<PairedElementType> callTrace;
			std::map<std::string, PairedElementType> searchPool;
			
			
		public:
			
			CallTrace();
			CallTrace (CallTrace& t);
			~CallTrace();
			
			// Pushes a non-terminal name onto the call-stack.
			void push (CFG::NonTerminal* nt);
			// Pushes a non-terminal name onto a call-stack with
			// the cycle set that is currently processed.
			void push (CFG::NonTerminal* nt, Util::SetOfCycleSets* cycleSets);
			
			// Returns TRUE if the call-stack is empty.
			bool isEmpty();
			// Returns TRUE if this call TRACE contains at any
			// position the given key.
			bool contains (CFG::NonTerminal* nt);
			
			// Returns the CycleSet which is associated with the
			// name '*nt'.
			std::pair<std::string, Util::SetOfCycleSets*> searchForCycleSetContainingNonTerminal (CFG::NonTerminal* nt);
			
			// Returns the top element of the call-stack.
			Util::SetOfCycleSets* pop();
			
			// Reads the top element from the stack and returns it.
			// The stack contents is not changed by this method.
			std::pair<std::string, Util::SetOfCycleSets*> peek();
			
			// Returns a string representation of this trace/
			std::string toString();
			
			// Returns the naming path which reflects the call stack
			// structure.
			NamingPath* getNamingPath (CFG::NonTerminal* nonTerminal);
			
			
	};
	
	
}


#endif	// ifndef __CALL_TRACE_HH__

