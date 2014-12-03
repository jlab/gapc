


#ifndef _CFG_PRETTY_PRINT_COUT_HH_
#define _CFG_PRETTY_PRINT_COUT_HH_

#include "cfg_pretty_print.hh"


namespace Printer {
	
	
	// This is a simple extension of the CFG pretty printer which
	// uses std::cout as output stream.
	class PrettyPrintCOut : public CFGPrettyPrint {
		
		public:
			
			PrettyPrintCOut();
			
			
	};
	
	
}


#endif	// ifndef _CFG_PRETTY_PRINT_COUT_HH_