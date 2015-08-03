

#include "attributable.hh"


Util::Attributable::Attributable() {
}


Util::Attributable::Attributable (Attributable& a) {
	for (std::map<std::string, Util::Attribute*>::iterator i = a.attributeMap.begin(); i != a.attributeMap.end(); i++) {
		this->attributeMap[(*i).first] = (*i).second;
	}
}


Util::Attributable::~Attributable() {
}


void Util::Attributable::setAttribute (Util::Attribute* attr) {
	if (attr != NULL) {
		this->setAttribute (attr->getAttributeID(), attr);
	}
}


void Util::Attributable::setAttribute (std::string key, Util::Attribute* attr) {
	this->attributeMap[key] = attr;
}


Util::Attribute* Util::Attributable::getAttribute (std::string key) {
	return this->attributeMap[key];
}


bool Util::Attributable::containsAttribute (std::string key) {
	return this->attributeMap.find (key) != this->attributeMap.end();
}


bool Util::Attributable::removeAttribute (std::string key) {
	if (containsAttribute (key)) {
		this->attributeMap.erase (key);
		return true;
	}
	return false;
}


void Util::Attributable::clearAttributes() {
	this->attributeMap.clear();
}


Util::Attributable::iterator Util::Attributable::begin() {
	return this->attributeMap.begin();
}


Util::Attributable::iterator Util::Attributable::end() {
	return this->attributeMap.end();
}

