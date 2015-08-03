

#ifndef __REMOVE_UNUSED_PRODUCTIONS_HH__
#define __REMOVE_UNUSED_PRODUCTIONS_HH__


#include <set>
#include <string>

#include "cfg.hh"


namespace CFG {
	
	
	class UnusedProductionsRemover {
		
		private:
			
			// The source grammar
			CFG* oldGrammar;
			CFG* newGrammar;
			
			
		public:
			
			UnusedProductionsRemover();
			~UnusedProductionsRemover();
			
			// Removes all unused productions from this grammar,
			// and returns a new grammar, which contains only
			// those grammar productions which are reachable from
			// the axiom.
			CFG* removeUnusedProductions (CFG* grammar);
			
		private:
			
			void removeFromProductions();
			void removeFromProduction (GrammarProduction* production, std::set<std::string>* visitedNonTerminals);
			void removeFromBase (Base* b, std::set<std::string>* visitedNonTerminals);
			
			
	};
	
	
}


#endif	// ifndef __REMOVE_UNUSED_PRODUCTIONS_HH__

