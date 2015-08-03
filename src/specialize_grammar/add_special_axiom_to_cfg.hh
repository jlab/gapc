

#ifndef __ADD_SPECIAL_AXIOM_TO_CFG_HH__
#define __ADD_SPECIAL_AXIOM_TO_CFG_HH__


#include "../cfg/cfg.hh"


namespace SpecializeGrammar {
	
	
	class AddSpecialAxiomToCFG {
		
		
		public:
			
			AddSpecialAxiomToCFG();
			~AddSpecialAxiomToCFG();
			
			// Adds two new rules to the grammar and annotated both
			// productions with attributes. the first production
			// defining the axiom, will be annotated as designated
			// axiom-production. The second production will be
			// annotated with a choice-function-attribute which
			// hints the gap-code generator which choice function
			// to use for the production.
			void addSpecialAxiom (CFG::CFG* grammar);
			
			
	};
	
	
}


#endif	// ifndef __ADD_SPECIAL_AXIOM_TO_CFG_HH__

