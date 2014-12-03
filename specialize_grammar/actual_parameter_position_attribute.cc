

#include "actual_parameter_position_attribute.hh"


SpecializeGrammar::ActualParameterPositionAttribute::ActualParameterPositionAttribute (unsigned int position)
	: Util::Attribute ("SpecializeGrammar::ActualParameterPositionAttribute"), parameterPosition (position) {
}


SpecializeGrammar::ActualParameterPositionAttribute::ActualParameterPositionAttribute (ActualParameterPositionAttribute& a)
	: Util::Attribute (a), parameterPosition (a.parameterPosition) {
}


SpecializeGrammar::ActualParameterPositionAttribute::~ActualParameterPositionAttribute() {
}


unsigned int SpecializeGrammar::ActualParameterPositionAttribute::getActualPosition() {
	return this->parameterPosition;
}


Util::Attribute* SpecializeGrammar::ActualParameterPositionAttribute::clone() {
	return new ActualParameterPositionAttribute (*this);
}

