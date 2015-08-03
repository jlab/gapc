

#include "cycle_path_info_attribute.hh"


Util::CyclePathInfoAttribute::CyclePathInfoAttribute()
	: Attribute ("Util::CyclePathInfoAttribute") {
}


Util::CyclePathInfoAttribute::CyclePathInfoAttribute (CyclePathInfoAttribute& a)
	: Attribute (a) {
}


Util::CyclePathInfoAttribute::~CyclePathInfoAttribute() {
}


void Util::CyclePathInfoAttribute::addElement (std::string nonTerminalName, CFG::Base* fragment) {
	this->elements.push_back (std::pair<std::string, CFG::Base*> (nonTerminalName, fragment));
}


void Util::CyclePathInfoAttribute::addElements (std::list< std::pair<std::string, CFG::Base*> >* elems, unsigned int startPos) {
	unsigned int pos = 0;
	for (std::list< std::pair<std::string, CFG::Base*> >::iterator i = elems->begin(); i != elems->end(); i++, pos++) {
		if (pos >= startPos) {
			this->elements.push_back (std::pair<std::string, CFG::Base*> ((*i).first, (*i).second));
		}
	}
}


Util::CyclePathInfoAttribute::iterator Util::CyclePathInfoAttribute::begin() {
	return this->elements.begin();
}


Util::CyclePathInfoAttribute::iterator Util::CyclePathInfoAttribute::end() {
	return this->elements.end();
}


Util::Attribute* Util::CyclePathInfoAttribute::clone() {
	return new CyclePathInfoAttribute (*this);
}

