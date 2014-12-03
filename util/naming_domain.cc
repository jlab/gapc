

#include "naming_domain.hh"

#include <iostream>
#include "boost/format.hpp"


unsigned int Util::NamingDomain::nextNameNumber = 0;


Util::NamingDomain::NamingDomain()
	: parentDomain (NULL) {
}


Util::NamingDomain::NamingDomain (Util::NamingDomain* parentDomain)
	: parentDomain (parentDomain) {
}


Util::NamingDomain::~NamingDomain() {
}


bool Util::NamingDomain::containsName (std::string* name) {
	return containsName (*name);
}


bool Util::NamingDomain::containsName (std::string name) {
	if (this->aliasMap.find (name) != this->aliasMap.end()) {
		return true;
	}
	else if (this->parentDomain != NULL) {
		return this->parentDomain->containsName (name);
	}
	return false;
}


std::string* Util::NamingDomain::getAlias (std::string* name) {
	return getAlias (*name);
}


std::string* Util::NamingDomain::getAlias (std::string name) {
	if (this->aliasMap.find (name) != this->aliasMap.end()) {
		return this->aliasMap[name];
	}
	else if (this->containsName (name)) {
		// The alias must be defined in the parent domain,
		// because it is not in the local alias map, but
		// it is found in the whole nested map, which also
		// inplies that the parent domain is not NULL!
		assert (this->parentDomain != NULL);
		return this->parentDomain->getAlias (name);
	}
	else {
		// create a new alias for the name:
		std::string* newAliasName = new std::string ("rule" + str(boost::format ("%i") % nextNameNumber++));
		this->aliasMap[name] = newAliasName;
		return newAliasName;
	}
}

