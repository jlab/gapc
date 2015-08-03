

#include "set_of_cycle_sets.hh"

#include <cassert>


Util::SetOfCycleSets::SetOfCycleSets() {
}


Util::SetOfCycleSets::SetOfCycleSets (std::set<CycleSet*> sets) {
	for (std::set<CycleSet*>::iterator i = sets.begin(); i != sets.end(); i++) {
		addCycleSet (*i);
	}
}


Util::SetOfCycleSets::SetOfCycleSets (SetOfCycleSets& s)
	: sets (s.sets) {
}


Util::SetOfCycleSets::~SetOfCycleSets() {
}


void Util::SetOfCycleSets::addCycleSet (CycleSet* cycleSet) {
	this->sets.insert (cycleSet);
}


bool Util::SetOfCycleSets::containsCycleSet (CycleSet* cycleSet) {
	for (std::set<CycleSet*>::iterator i = this->sets.begin(); i != this->sets.end(); i++) {
		if (*(*i) == *cycleSet) {
			return true;
		}
	}
	return false;
}


bool Util::SetOfCycleSets::containsElement (CFG::NonTerminal* nonTerminal) {
	for (std::set<CycleSet*>::iterator i = this->sets.begin(); i != this->sets.end(); i++) {
		if ((*i)->containsElement (nonTerminal)) {
			return true;
		}
	}
	return false;
}


bool Util::SetOfCycleSets::isEmpty() {
	return this->sets.empty();
}


bool Util::SetOfCycleSets::isBackReference (CFG::NonTerminal* source, CFG::NonTerminal* destination) {
	bool allSetsAreBackReferences = true;
	bool someSetIsBackReference = false;
	
	// This algorithm calculates both information: the source
	// and destination non-terminals are no back-reference in
	// no cycle-set at all, and both non-terminals are back
	// references in all cycle sets.
	// First we check if both, source and destination are
	// contained in this set-set.
	if (!containsElement (source) || !containsElement(destination)) {
		// should this be an error message?
		return false;
	}
	
	for (std::set<Util::CycleSet*>::iterator i = this->sets.begin(); i != this->sets.end(); i++) {
		bool singleResult = (*i)->isBackReference (source, destination);
		allSetsAreBackReferences &= singleResult;
		someSetIsBackReference |= singleResult;
	}
	
	// Both information should be complementary, that is only
	// one flag may be TRUE at a time.
	assert (allSetsAreBackReferences ^ !someSetIsBackReference);
	
	// At least one set needs to be a back reference in order to
	// establish a real back reference.
	return someSetIsBackReference;
}


std::string Util::SetOfCycleSets::toString() {
	std::string result;
	
	bool firstLoopRun = true;
	for (std::set<Util::CycleSet*>::iterator i = this->sets.begin(); i != this->sets.end(); i++) {
		if (!firstLoopRun) {
			result += ", ";
		}
		result += (*i)->toString();
		firstLoopRun = false;
	}
	
	return "<" + result + ">";
}

