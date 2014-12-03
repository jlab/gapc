

#ifndef __REMOVE_ALL_ATTRIBUTES_HH__
#define __REMOVE_ALL_ATTRIBUTES_HH__


#include "../cfg/cfg.hh"


namespace Util {
	
	
	class RemoveAllAttributes {
		
		public:
			
			RemoveAllAttributes();
			~RemoveAllAttributes();
			
			// Removes all attributes from the given grammar.
			void removeFromGrammar (CFG::CFG* grammar);
			
		private:
			
			void removeFromProduction (CFG::GrammarProduction* production);
			void removeFromBase (CFG::Base* b);
			
			
	};
	
	
}


#endif	// ifndef __REMOVE_ALL_ATTRIBUTES_HH__

