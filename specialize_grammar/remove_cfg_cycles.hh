

#ifndef __REMOVE_CFG_CYCLES_HH__
#define __REMOVE_CFG_CYCLES_HH__


#include <queue>


#include "../cfg/cfg.hh"
#include "../util/annotate_cycles.hh"
#include "../util/attribute.hh"
#include "../util/naming_domain.hh"
#include "call_trace.hh"
#include "set_of_cycle_sets.hh"


namespace SpecializeGrammar {
	
	
	// This is a container for information about the
	// productions and alternatives a cycle-path took.
	struct CyclePathInfo {
		std::string nonTerminalName;
		unsigned int startPosInPath;
		std::list< std::pair<std::string, CFG::Base*> >* path;
	};
	
	
	class RemoveCFGCycles {
		
		private:
			
			// The old grammar which will be transformed by this
			// class
			CFG::CFG* oldGrammar;
			// The new grammar which is the result of the transformation
			// performed by this class.
			CFG::CFG* newGrammar;
			
			// Stores all CFG fragments that will be hidden in the current
			// grammar production.
			std::map< std::string, std::set< CFG::Base* >* > hiddenCFGFragments;
			
			// This vector is actually used as a stack which holds
			// all production names and the CFG nodes that lie on
			// the path to the current position of the algorithm.
			std::vector< std::pair< std::string, CFG::Base* > > currentProductionFragmentTrace;
			
			// A map between a non-terminal name and a set of production
			// alternatives which lie on a path to a broken cycle. This
			// path fragments will be annotated to the production after
			// all alternatives of that production have been processed,
			// and all cycles found and broken, hence this set is filled
			// appropriately at that time.
			std::map<std::string, std::set<CyclePathInfo*>* > cyclePathsSearchSet;
			
			// This set contains the non-terminal names of those productions
			// that were completely reduced due to cycle break ups and subsequent
			// collaption of productions.
			std::set<std::string> collapsedProductions;
			
			
		public:
			
			RemoveCFGCycles();
			~RemoveCFGCycles();
			
			CFG::CFG* removeCycles (CFG::CFG* grammar);
			
			
		private:
			
			// Reserves names in the naming-domain for all non-terminals
			// that belong to dead-end CFG nodes. Through this we achieve
			// that the dead-end parts of a grammar are generated only
			// once, because they will always stay in the root-most level
			// of the naming domain, thus always be available, and hence
			// never needed to be generated.
			void reserveFixedNonTerminalNames (Util::NamingDomain* namingDomain);
			
			// Transforms a production, either as a member of a cycle
			// or as a normal production, depending on the cycle annotation
			// of the production node.
			void transformProduction (CFG::NonTerminal* productionNT, Util::CallTrace callTrace, Util::NamingDomain* namingDomain);
			
			// Creates a production instance, which handles all cycle
			// non-terminals which are backward directed different to
			// those which point in a forward direction.
			void transformCycleProduction (CFG::NonTerminal* productionNT, Util::SetOfCycleSets* currentCycleSets, Util::CallTrace callTrace, Util::NamingDomain* namingDomain);
			
			// Creates a copy of this CFG node element. This method
			// processed the element in cycle-mode, which will rename
			// all non-terminals in order to create unique grammar
			// productions for each cycle which can be reached by the
			// axiom of the grammar.
			CFG::Base* transformCycleElement (CFG::Base* b, Util::CallTrace callTrace, Util::NamingDomain* namingDomain);
			// Transforms an element strict, which means that each
			// element is added to the result graph, especially all
			// non-terminals, without regard to their cycle status.
			// This method is used to transform that part of a
			// production which may not be hidden (e.g. if the
			// CFG::Base instance is a sequence containing at least
			// one terminal parser).
			CFG::Base* transformElementStrict (CFG::Base* b, Util::CallTrace callTrace, Util::NamingDomain* namingDomain);
			// Translates the given CFG structure into an equivalent
			// structure with all names changed according to the
			// names stored in the naming-domain.
			CFG::Base* translateNaming (CFG::Base* fragment, Util::NamingDomain* namingDomain);
			// Uses the start non-terminal 'startNT' and collects
			// all elements from the stack named 'currentProductionFragmentTrace'
			// up to that non-terminal and produces a cycle set from it.
			void extractMetaCycleInfoFromTrace (std::string startNT, Util::NamingDomain* namingDomain);
			// Inserts an element into the mapping between non-terminal
			// names and base fragments which lie on a cycle path that
			// passes through one or more of the non-terminal's
			// production-alternatives.
			void insertCyclePathsSearchSetElement (std::string nonTerminalName, CyclePathInfo* pathInfo);
			// Removes the cycle path information stored for the
			// non-terminal with the name 'nonTerminalName'.
			void removeCyclePathsSearchSetElement (std::string nonTerminalName);
			// Returns the set of cycle-path-info items stored for the
			// non-terminal with the name 'nonTerminalName', or NULL
			// if no information are stored in the field cyclePathsSearchSet.
			std::set<CyclePathInfo*>* getCyclePathsSearchSetElement (std::string nonTerminalName);
			
			// Returns the cycle set of the CFG node, or NULL
			// if no cycle set if annotated to the node.
			std::set<Util::CycleSet*> getCycleSets (CFG::Base* b);
			// Returns the CycleSets stored as a CycleSetMarkAttribute.
			std::set<Util::CycleSet*> getCycleMarkSets (CFG::Base* b);
			// Returns the cycle set of the GrammarProduction, or NULL
			// if no cycle set if annotated to the node.
			std::set<Util::CycleSet*> getCycleSets (CFG::GrammarProduction* p);
			// Returns TRUE if the CFG node is annotated with a
			// ReducibleElementAttribute.
			bool elementIsReducible (CFG::Base* b);
			
			// Returns TRUE if the non-terminal is a back reference, i.g.
			// it points to a grammar production the current call trace
			// runs through.
			bool nonTerminalIsBackReference (CFG::NonTerminal* nonTerminal, Util::CallTrace callTrace);
			
			// Returns TRUE if the non-terminal is the last one
			// in the cycle, that is it closes the loop.
			bool nonTerminalClosesCycle (CFG::NonTerminal* nonTerminal, Util::CallTrace callTrace);
			
			
			// Returns TRUE if the CFG node instance is a non-terminal
			// wrapped into a CFG::BaseWrapper.
			bool isWrappedNonTerminal (CFG::Base* b);
			
			// Copies all attributes of the source node into the
			// destination node instance.
			void copyAttributes (CFG::Base* source, CFG::Base* destination);
			
			// Inserts a CFG node into the hideaway map.
			void insertCFGFragmentIntoHideawayMap (std::string name, CFG::Base* b);
			
			
	};
	
	
}


#endif	// idndef __REMOVE_CFG_CYCLES_HH__

