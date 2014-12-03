

#ifndef __ANNOTATE_ALGEBRA_FUNCTION_NAMES_HH__
#define __ANNOTATE_ALGEBRA_FUNCTION_NAMES_HH__


#include "../cfg/cfg.hh"


#include "algebra_function_name_attribute.hh"


namespace Util {
	
	
	// Forward declaration of the attribute, becuase we need it
	// in the interface definition of the annotator class.
	class AlgebraFunctionNameAttribute;
	
	
	// Simply annotates each alternative of a production with a
	// new algebra function name which is also unique.
	// This algorithm relies on the fact, that each grammar which
	// is given as input to 'annotateGrammar()' consists of
	// grammar productions which are basically a list of alternative
	// context free terms, where those terms do not contain alternatives
	// itself, but only sequences or basic grammar parts (e.g.
	// non-terminals, terminals or epsilons).
	class AlgebraFunctionNameAnnotator {
		
		public:
			
			AlgebraFunctionNameAnnotator();
			~AlgebraFunctionNameAnnotator();
			
			void annotateGrammar (CFG::CFG* grammar);
			
			
		private:
			
			void annotateProduction (CFG::Base* production);
			AlgebraFunctionNameAttribute* createNextAttribute();
			
			
	};
	
	
}


#endif	// ifndef __ANNOTATE_ALGEBRA_FUNCTION_NAMES_HH__

