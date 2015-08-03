

#include "cycle_break_point_attribute.hh"


SpecializeGrammar::CycleBreakPointAttribute::CycleBreakPointAttribute()
	: Attribute ("SpecializeGrammar::CycleBreakPointAttribute") {
	
}


SpecializeGrammar::CycleBreakPointAttribute::CycleBreakPointAttribute (CycleBreakPointAttribute& a)
	: Attribute (a) {
}


SpecializeGrammar::CycleBreakPointAttribute::~CycleBreakPointAttribute() {
	
}


Util::Attribute* SpecializeGrammar::CycleBreakPointAttribute::clone() {
	return new CycleBreakPointAttribute (*this);
}

