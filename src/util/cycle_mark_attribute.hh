

#ifndef __CYCLE_MARK_ATTRIBUTE_HH__
#define __CYCLE_MARK_ATTRIBUTE_HH__


#include "attribute.hh"
#include "cycle_set.hh"


namespace Util {
	
	
	// A marker attribute which is used to mark a non-terminal
	// as being part of a cycle. It contains information about
	// the cycle it is part of as a pointer to a CycleSet.
	class CycleMarkAttribute : public Attribute {
		
		private:
			
			// The cycle-set the marked non-terminal is part of.
			std::set<CycleSet*> cycleSets;
			
			
		public:
			
			CycleMarkAttribute();
			CycleMarkAttribute (CycleMarkAttribute& a);
			~CycleMarkAttribute();
			
			// Adds the given set to the set of cycle sets.
			void addCycleSet (CycleSet* set);
			// Returns true, if the current cycle-mark-attribute
			// already contains the given set.
			bool containsCycleSet (CycleSet* set);
			
			// Returns the set of cycle sets this attribute holds.
			std::set<CycleSet*> getCycleSets();
			
			virtual Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __CYCLE_MARK_ATTRIBUTE_HH__

