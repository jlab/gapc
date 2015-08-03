

#ifndef __ANNOTATE_DEAD_ENDS_HH__
#define __ANNOTATE_DEAD_ENDS_HH__


#include <set>

#include "../cfg/cfg.hh"
#include "attribute.hh"


namespace Util {
	
	
	// Annotates each CFG node with a DeadEndAttribute if the node
	// is a dead end in the grammar graph, which means it does not
	// participate in any cycle.
	// This algorithm needs the set FIRST, and assumes that the
	// corresponding attribute has already been annotated.
	class AnnotateDeadEnds {
		
		private:
			
			// The grammar which this algorithm traverses. This
			// pointer is set when 'annotateGrammar' is called.
			CFG::CFG* grammar;
			
			
		public:
			
			AnnotateDeadEnds();
			~AnnotateDeadEnds();
			
			void annotateGrammar (CFG::CFG* grammar);
			
		private:
			
			bool annotateProduction (CFG::NonTerminal* nonTerminal, std::set<std::string>* visitedNonTerminals);
			bool annotateBase (CFG::Base* b, std::set<std::string>* visitedNonTerminals);
			
			// Returns TRUE if the CFG node can be the empty word (epsilon).
			bool elementIsNullable (CFG::Base* b);
			
			
	};
	
	
	// This is a marker attribute, which marks a CFG node
	// as a dead end, which means it is not involved in
	// any cycle whatsoever. Productions marked by this
	// attribute may be treated in the cycle reduction
	// algorithm as "constants" which can be accessed
	// by their original name, and may be simply cloned.
	class DeadEndAttribute : public Util::Attribute {
		
		public:
			
			DeadEndAttribute();
			DeadEndAttribute (DeadEndAttribute& a);
			~DeadEndAttribute();
			
			virtual Attribute* clone();
			
	};
	
}


#endif	// ifndef __ANNOTATE_DEAD_ENDS_HH__

