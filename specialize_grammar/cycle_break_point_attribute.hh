

#ifndef __CYCLE_BREAK_POINT_ATTRIBUTE_HH__
#define __CYCLE_BREAK_POINT_ATTRIBUTE_HH__


#include "../util/attribute.hh"


namespace SpecializeGrammar {
	
	
	class CycleBreakPointAttribute : public Util::Attribute {
		
		public:
			
			CycleBreakPointAttribute();
			CycleBreakPointAttribute (CycleBreakPointAttribute& a);
			virtual ~CycleBreakPointAttribute();
			
			virtual Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __CYCLE_BREAK_POINT_ATTRIBUTE_HH__

