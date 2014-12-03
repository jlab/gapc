

#ifndef __CYCLE_ATTRIBUTE_HH__
#define __CYCLE_ATTRIBUTE_HH__


#include <set>

#include "attribute.hh"
#include "cycle_set.hh"


namespace Util {
	
	
	// Used to annotate a non-terminal in a grammar with cycle-information.
	class CycleAttribute : public Attribute {
		
		private:
			
			// Stores the set of non-terminals that belong to
			// the cycle.
			std::set<CycleSet*> cycleSets;
			
			
		public:
			
			CycleAttribute (std::set<CycleSet*> cycleSet);
			CycleAttribute (CycleAttribute& a);
			~CycleAttribute();
			
			std::set<CycleSet*> getCycleSets();
			
			virtual Attribute* clone();
			
			// Returns true, if the current cycle-mark-attribute
			// already contains the given set.
			bool containsCycleSet (CycleSet* set);
			
			
		private:
			
			// Adds all elements of the std::set to this attribute,
			// provided they are not already inserted. Dumplicate
			// sets will be removed by this provedure.
			void addCycleSets (std::set<CycleSet*> sets);
			
			
	};
	
	
}


#endif	// ifndef __CYCLE_ATTRIBUTE_HH__

