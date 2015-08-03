

#ifndef __SET_OF_CYCLE_SETS_HH__
#define __SET_OF_CYCLE_SETS_HH__


#include <set>

#include "../cfg/cfg.hh"
#include "../util/cycle_set.hh"


namespace Util {
	
	
	class SetOfCycleSets {
		
		private:
			
			// This is the set of sets.
			std::set<CycleSet*> sets;
			
			
		public:
			
			SetOfCycleSets();
			SetOfCycleSets (std::set<CycleSet*> sets);
			SetOfCycleSets (SetOfCycleSets& s);
			~SetOfCycleSets();
			
			// Adds a cycle-set to this set-set.
			void addCycleSet (CycleSet* cycleSet);
			// Returns TRUE if the parameter is already stored in
			// this set-set.
			bool containsCycleSet (CycleSet* cycleSet);
			// Returns TRUE if the non-terminal is stored in any
			// of the cycle-sets held by this set-set.
			bool containsElement (CFG::NonTerminal* nonTerminal);
			// Returns TRUE if this set is empty;
			bool isEmpty();
			// Returns true, if the source non-terminal is dominated
			// by the destination non-terminal (the destination comes
			// before the source in the cycle-order).
			bool isBackReference (CFG::NonTerminal* source, CFG::NonTerminal* destination);
			
			// Returns a string representation of the set-set.
			std::string toString();
			
			
	};
	
	
}


#endif	// ifnef __SET_OF_CYCLE_SETS_HH__

