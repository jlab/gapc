

#include "annotate_algebra_function_names.hh"


#include <sstream>

#include "../log.hh"
#include "algebra_function_name_attribute.hh"
#include "grammar_production_naming_attribute.hh"


Util::AlgebraFunctionNameAnnotator::AlgebraFunctionNameAnnotator() {
}


Util::AlgebraFunctionNameAnnotator::~AlgebraFunctionNameAnnotator() {
}


void Util::AlgebraFunctionNameAnnotator::annotateGrammar (CFG::CFG* grammar) {
	std::list<CFG::GrammarProduction*> productions = grammar->getProductions();
	for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin(); i != productions.end(); i++) {
		annotateProduction ((*i)->rhs);
	}
}


void Util::AlgebraFunctionNameAnnotator::annotateProduction (CFG::Base* b) {
	switch (b->getType()) {
		case CFG::NONTERMINAL: {
			CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (b);
			b->setAttribute (new Util::GrammarProductionNamingAttribute (nonTerminal->getName()));
			// fall directly through to the other cases for
			// EPSILON, TERMINA, and so on...
		}
		case CFG::EPSILON:
		case CFG::TERMINAL:
		case CFG::REGULAR_EXPRESSION:
		case CFG::PRODUCTION_SEQUENCE: {
			b->setAttribute (createNextAttribute());
			break;
		}
		case CFG::BASE_WRAPPER: {
			CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
			annotateProduction (wrapper->getWrappedBase());
			break;
		}
		case CFG::PRODUCTION_ALTERNATIVE: {
			CFG::ProductionAlternative* alt = dynamic_cast<CFG::ProductionAlternative*> (b);
			for (CFG::ProductionAlternative::iterator i = alt->begin(); i != alt->end(); i++) {
				annotateProduction (*i);
			}
			break;
		}
		default: {
			throw LogError ("gap-00501: Unhandled type CFG grammar part.");
			break;
		}
	}
}


Util::AlgebraFunctionNameAttribute* Util::AlgebraFunctionNameAnnotator::createNextAttribute() {
	// For creating a unique name for each algebra function,
	// we simply use a global counter, which is increased each
	// time a function name is created.
	static unsigned int functionCounter = 0;
	// We use a stream to assemble the function name,
	// it seams the easiest way to get the string representation
	// for an unsigned integer.
	std::stringstream strstream;
	strstream << "f" << functionCounter++;
	return new AlgebraFunctionNameAttribute (new std::string (strstream.str()));
}

