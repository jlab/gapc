

#ifndef __LAST_ELEMENT_OF_CYCLE_ATTRIBUTE_HH__
#define __LAST_ELEMENT_OF_CYCLE_ATTRIBUTE_HH__


#include <set>

#include "attribute.hh"
#include "cycle_set.hh"


namespace Util {
	
	
	// This attribute is used to mark GrammarProductions with
	// those cycles-sets which end in this production.
	class LastCycleElementAttribute : public Attribute {
		
		private:
			
			// Stores all CycleSets which have their last element
			// in cycle order in that GrammarProduction which will
			// be attributed with this LastCycleElement instance.
			std::set<CycleSet*> cycleSets;
			
			
		public:
			
			LastCycleElementAttribute();
			LastCycleElementAttribute (LastCycleElementAttribute& a);
			~LastCycleElementAttribute();
			
			// Adds a CycleSet to this attribute.
			void addCycleSet (CycleSet* cycleSet);
			// Returns the set of CycleSets held by this attribute.
			std::set<CycleSet*> getCycleSets();
			
			typedef std::set<CycleSet*>::iterator iterator;
			iterator begin();
			iterator end();
			
			virtual Util::Attribute* clone();
			
	};
	
	
}


#endif	// ifndef __LAST_ELEMENT_OF_CYCLE_ATTRIBUTE_HH__

