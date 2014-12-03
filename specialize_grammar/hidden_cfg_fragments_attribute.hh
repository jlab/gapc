

#ifndef __HIDDEN_CFG_FRAGMENTS_ATTRIBUTE_HH__
#define __HIDDEN_CFG_FRAGMENTS_ATTRIBUTE_HH__


#include <list>
#include <set>

#include "../cfg/cfg.hh"
#include "../util/attribute.hh"


namespace SpecializeGrammar {
	
	
	class HiddenCFGFragmentsAttribute : public Util::Attribute {
		
		private:
			
			// The list of all hidden CFG fragments.
			std::list<CFG::Base*> hiddenFragments;
			
			
		public:
			
			HiddenCFGFragmentsAttribute();
			HiddenCFGFragmentsAttribute (HiddenCFGFragmentsAttribute& a);
			virtual ~HiddenCFGFragmentsAttribute();
			
			void addHiddenFragment (CFG::Base* b);
			void addHiddenFragments (std::set<CFG::Base*>* fragments);
			
			typedef std::list<CFG::Base*>::iterator iterator;
			iterator begin();
			iterator end();
			
			virtual Util::Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __HIDDEN_CFG_FRAGMENTS_ATTRIBUTE_HH__

