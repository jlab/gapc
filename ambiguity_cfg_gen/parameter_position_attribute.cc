

#include "parameter_position_attribute.hh"


Util::ParameterPositionAttribute::ParameterPositionAttribute (int parameterPosition)
	: Attribute ("Util::ParameterPositionAttribute"), parameterPosition (parameterPosition) {
}


Util::ParameterPositionAttribute::ParameterPositionAttribute (ParameterPositionAttribute& a)
	: Attribute ("Util::ParameterPositionAttribute"), parameterPosition (a.parameterPosition) {
}


Util::ParameterPositionAttribute::~ParameterPositionAttribute() {
}


int Util::ParameterPositionAttribute::getParameterPosition() {
	return this->parameterPosition;
}


Util::Attribute* Util::ParameterPositionAttribute::clone() {
	return new ParameterPositionAttribute (*this);
}

