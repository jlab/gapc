

#ifndef __REWRITE_NON_PRODUCTIVE_CFG_RULES_HH__
#define __REWRITE_NON_PRODUCTIVE_CFG_RULES_HH__


#include <list>

#include "../cfg/cfg.hh"
#include "../util/attribute.hh"


namespace SpecializeGrammar {
	
	
	// Rewrites a given grammar into an equivalent grammar which
	// consists only of rules with sequences, that are either
	// productive (except the cycling non-terminals) or non-terminals
	// that can only accept epsilon (except the cycling non-terminals).
	//
	// Please note that as a prerequisite the cycle-annotator and
	// the FIRST-set-annotator must have been run on the CFG.
	class RewriteNonProductiveCFGRules {
		
		private:
			
			CFG::CFG* oldGrammar;
			CFG::CFG* newGrammar;
			
			
		public:
			
			RewriteNonProductiveCFGRules();
			~RewriteNonProductiveCFGRules();
			
			// Rewrites the grammar.
			CFG::CFG* rewriteGrammar (CFG::CFG* grammar);
			
			
		private:
			
			void rewriteProductions();
			void rewriteProduction (CFG::GrammarProduction* production);
			
			// 
			CFG::GrammarProduction* rewriteProductionWithoutEpsilon (CFG::GrammarProduction* production);
			// 
			CFG::Base* rewriteBaseWithoutEpsilon (CFG::Base* b);
			// 
			CFG::GrammarProduction* rewriteProductionWithEpsilon (CFG::GrammarProduction* production);
			// 
			CFG::Base* rewriteBaseWithEpsilon (CFG::Base* b);
			
			// Copies all attributes that are annotated to the
			// source instance to the destination instance.
			void copyAttributes (CFG::Base* source, CFG::Base* destination);
			
			
	};
	
	
	// This is simply a marker attribute which marks any
	// element of the CFG graph. A node which is marked
	// with this attribute can only derive epsilon, nothing
	// else.
	class EpsilonOnlyAttribute : public Util::Attribute {
		
		public:
			
			EpsilonOnlyAttribute();
			EpsilonOnlyAttribute (EpsilonOnlyAttribute& a);
			~EpsilonOnlyAttribute();
			
			virtual Util::Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __REWRITE_NON_PRODUCTIVE_CFG_RULES_HH__

