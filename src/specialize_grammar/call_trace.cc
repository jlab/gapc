

#include "call_trace.hh"


Util::CallTrace::CallTrace() {
}


Util::CallTrace::CallTrace (CallTrace& t)
	: callTrace (t.callTrace), searchPool (t.searchPool) {
}


Util::CallTrace::~CallTrace() {
}


void Util::CallTrace::push (CFG::NonTerminal* nt) {
	this->push (nt, NULL);
}


void Util::CallTrace::push (CFG::NonTerminal* nt, Util::SetOfCycleSets* cycleSets) {
	this->callTrace.push_back (PairedElementType (*nt->getName(), cycleSets));
	this->searchPool[*nt->getName()] = PairedElementType (*nt->getName(), cycleSets);
}


bool Util::CallTrace::isEmpty() {
	return this->callTrace.empty();
}


bool Util::CallTrace::contains (CFG::NonTerminal* nt) {
	return this->searchPool.find (*nt->getName()) != this->searchPool.end();
}


std::pair<std::string, Util::SetOfCycleSets*> Util::CallTrace::searchForCycleSetContainingNonTerminal (CFG::NonTerminal* nt) {
	for (std::map<std::string, PairedElementType>::iterator i = this->searchPool.begin(); i != this->searchPool.end(); i++) {
		if ((*i).second.second != NULL && (*i).second.second->containsElement (nt)) {
			return (*i).second;
		}
	}
	return std::pair<std::string, Util::SetOfCycleSets*> ("", NULL);
}


Util::SetOfCycleSets* Util::CallTrace::pop() {
	PairedElementType elem = this->callTrace.back();
	this->callTrace.pop_back();
	this->searchPool.erase (elem.first);
	return elem.second;
}


std::pair<std::string, Util::SetOfCycleSets*> Util::CallTrace::peek() {
	return this->callTrace.back();
}


std::string Util::CallTrace::toString() {
	std::string result;
	
	bool firstLoopRun = true;
	for (std::vector<PairedElementType>::iterator i = this->callTrace.begin(); i != this->callTrace.end(); i++) {
		PairedElementType element = *i;
		if (!firstLoopRun) {
			result += ", ";
		}
		std::string cycleSetAsString = "NLL";
		if (element.second != NULL) {
			cycleSetAsString = element.second->toString();
		}
		result += "(" + element.first + " | " + cycleSetAsString + ")";
		firstLoopRun = false;
	}
	
	// Make it look nice:
	return "{" + result + "}";
}


Util::NamingPath* Util::CallTrace::getNamingPath (CFG::NonTerminal* nonTerminal) {
	NamingPath* result = new NamingPath();
	
	// Now copy the contents back onto the stack.
	for (std::vector<PairedElementType>::iterator i = this->callTrace.begin(); i != this->callTrace.end(); i++) {
		PairedElementType element = *i;
		std::string* elementName = new std::string (element.first);
		NamingPath* nextResult = result->createSubPath (elementName);
		delete (result);
		result = nextResult;
		if ((*i).first == *nonTerminal->getName()) {
			break;
		}
	}
	
	return result;
}

