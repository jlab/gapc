

#ifndef _ANNOTATE_REDUCIBLE_ATTRIBUTES_HH__
#define _ANNOTATE_REDUCIBLE_ATTRIBUTES_HH__


#include <set>

#include "../cfg/cfg.hh"
#include "../util/annotate_cycles.hh"
#include "../util/annotate_the_set_first.hh"


namespace Util {
	
	
	// Annotates a CFG graph with the ReducibleElementAttribute,
	// which marks an element of the graph as collapsible, which
	// means the element is part of a cycle and may be omitted
	// in the cause of cycle removal.
	class ReducibleAttributeAnnotator {
		
		private:
			
			// Holds a pointer to the currently processed grammar,
			// because some deep down sub-routines require information
			// from this structure.
			CFG::CFG* grammar;
			
			
		public:
			
			ReducibleAttributeAnnotator();
			~ReducibleAttributeAnnotator();
			
			void annotateGrammar (CFG::CFG* grammar);
			
			
		private:
			
			void annotateProduction (CFG::GrammarProduction* production);
			bool annotateElement (CFG::Base* b);
			
			std::set<Util::CycleSet*> getCycleMarkSets (CFG::Base* b);
			Util::FirstSet getFirstSet (CFG::NonTerminal* nt);
			
			
	};
	
	
	// This is a marker atribute, which marks CFG nodes as reducible,
	// which means that the node is part of a cycle, and may possibly
	// collapse (vanish).
	class ReducibleElementAttribute : public Attribute {
		
		public:
			
			ReducibleElementAttribute();
			ReducibleElementAttribute (ReducibleElementAttribute& a);
			~ReducibleElementAttribute();
			
			virtual Util::Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef _ANNOTATE_REDUCIBLE_ATTRIBUTES_HH__

