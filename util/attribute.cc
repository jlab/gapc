


#include "attribute.hh"


Util::Attribute::Attribute (std::string kindOfName)
	: kindOfName (kindOfName) {
}


Util::Attribute::Attribute (Attribute& a)
	: kindOfName (a.kindOfName) {
}


Util::Attribute::~Attribute() {
}


std::string Util::Attribute::getAttributeID() {
	return this->kindOfName;
}


bool Util::Attribute::isKindOf (std::string kindOfName) {
	return this->kindOfName == kindOfName;
}

