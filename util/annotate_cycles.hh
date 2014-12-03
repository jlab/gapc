
#ifndef __ANNOTATE_CYCLES_HH__
#define __ANNOTATE_CYCLES_HH__


#include <set>

#include "../cfg/cfg.hh"
#include "attribute.hh"




namespace Util {
	
	
	// Forward declare the cycle-set.
	class CycleSet;
	class FirstSet;
	
	
	// This class implements the algorithm that annotates
	// a CFG with detailed information about its cycles it
	// contains. If the grammar does not have any cycles,
	// it will remain unchanged by this algorithm.
	class AnnotateCycles {
		
		private:
			
			CFG::CFG* grammar;
			hashtable<std::string, FirstSet> firstSets;
			
			
		public:
			
			AnnotateCycles();
			~AnnotateCycles();
			
			// Annotates the grammar with information about
			// cycles in the grammar productions. Each non-terminal
			// that is part of a cycle will be annotated with an
			// attribute that contains a list of all non-terminals
			// that participates in that cycle. The grammar gets
			// annotated with information about ...
			void annotateGrammar (CFG::CFG* grammar);
			
			
		private:
			
			std::set<std::pair<CycleSet*, std::list<CFG::NonTerminal*>* > > searchForCycles (CFG::NonTerminal* nt, CFG::GrammarProduction* production, CycleSet* visitedNonTerminals, std::list<CFG::NonTerminal*> callStack);
			std::set<std::pair<CycleSet*, std::list<CFG::NonTerminal*>* > > searchForCycles (CFG::NonTerminal* nt, CycleSet* visitedNonTerminals, CFG::Base* fragment, std::list<CFG::NonTerminal*> callStack);
			
			FirstSet getFirstSet (CFG::NonTerminal* nt);
			
			// Adds an other cycle-set to the node's cycle-mark-attribute.
			void addCycleMarkAttribute (Util::Attributable* attributableInstance, CycleSet* cycleSet);
			// Adds the cycle set to the LastCycleElementAttribute of the
			// grammar production.
			void addLastCycleElementAttribute (CFG::GrammarProduction* production, CycleSet* cycleSet);
			
			// Returns TRUE if the given CFG node is a non-terminal
			// which is wrapped in a CFG::BaseWrapper node.
			bool isWrappedNonTerminal (CFG::Base* b);

			
	};
	
	
}


#endif	// ifndef __ANNOTATE_CYCLES_HH__

