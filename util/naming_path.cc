

#include "naming_path.hh"


// The separator character for the string representation
// of a naming path.
std::string Util::NamingPath::separatorChar = "/";


Util::NamingPath::NamingPath()
	: prefix (NULL), suffix (new std::string ("")) {
}


Util::NamingPath::NamingPath (std::string* name)
	: prefix (NULL), suffix (name) {
}


Util::NamingPath::NamingPath (NamingPath& p) {
	if (p.prefix != NULL) {
		this->prefix = new NamingPath (*p.prefix);
	}
	else {
		this->prefix = NULL;
	}
	this->suffix = new std::string (*p.suffix);
}


Util::NamingPath::~NamingPath() {
}


Util::NamingPath* Util::NamingPath::createSubPath (std::string* newName) {
	NamingPath* result = new NamingPath();
	result->prefix = new NamingPath (*this);
	result->suffix = newName;
	return result;
}


std::string Util::NamingPath::toString() {
	std::string result = "";
	
	if (this->prefix != NULL) {
		result = this->prefix->toString() + separatorChar;
	}
	else {
		result = separatorChar;
	}
	
	result += *this->suffix;
	
	return result;
}

