


#include "remove_cfg_cycles.hh"


#include <iostream>
#include <cassert>

#include "../util/cycle_attribute.hh"
#include "../util/annotate_reducible_attributes.hh"
#include "../util/cycle_set_utils.hh"
#include "cycle_break_point_attribute.hh"
#include "cycle_path_info_attribute.hh"
#include "hidden_cfg_fragments_attribute.hh"

#include "../printer/cfg_pretty_print_cout.hh"


SpecializeGrammar::RemoveCFGCycles::RemoveCFGCycles() {	
}


SpecializeGrammar::RemoveCFGCycles::~RemoveCFGCycles() {	
}


CFG::CFG* SpecializeGrammar::RemoveCFGCycles::removeCycles (CFG::CFG* grammar) {
	// Store the source grammar, create the destination grammar.
	this->oldGrammar = grammar;
	this->newGrammar = new CFG::CFG();
	
	// Create an empty call-trace and start the transformation.
	Util::CallTrace callTrace;
	Util::NamingDomain* namingDomain = new Util::NamingDomain();
	// First create all non-terminal aliases for the dead-end
	// grammar productions...
	//reserveFixedNonTerminalNames (namingDomain);
	// ...and an additional layer of naming domain.
	namingDomain = new Util::NamingDomain (namingDomain);
	transformProduction (this->oldGrammar->getAxiom(), callTrace, namingDomain);
	
	// Now set the non-terminal:
	CFG::NonTerminal* axiom = new CFG::NonTerminal (namingDomain->getAlias (grammar->getAxiom()->getName()));
	delete (namingDomain);
	this->newGrammar->setAxiom (axiom);
	
	// Done. Just return the new grammar.
	return this->newGrammar;
}


void SpecializeGrammar::RemoveCFGCycles::reserveFixedNonTerminalNames (Util::NamingDomain* namingDomain) {
	std::list<CFG::GrammarProduction*> productions = this->oldGrammar->getProductions();
	for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin(); i != productions.end(); i++) {
		CFG::GrammarProduction* production = *i;
		CFG::NonTerminal* lhs = production->lhs;
		namingDomain->getAlias (lhs->getName());
	}
}


void SpecializeGrammar::RemoveCFGCycles::transformProduction (CFG::NonTerminal* productionNT, Util::CallTrace callTrace, Util::NamingDomain* namingDomain) {
	CFG::GrammarProduction* production = this->oldGrammar->getProduction (productionNT);
	//std::set<Util::CycleSet*> cycleSets = getCycleSets (production);
	// Instead of the call above, we now want the cycle information
	// for a non-terminal directly. By this we know exactly if the non-terminal
	// path followed lies on a cycle.
	std::set<Util::CycleSet*> cycleSets = getCycleMarkSets (productionNT);
	Util::SetOfCycleSets* setOfCycleSets = new Util::SetOfCycleSets (cycleSets);
	// Start the transformation for this non-terminal with
	// the set of cycle-sets. Since this method is called recursively
	// the variables 'callTrace' and 'namingDomain' may not be
	// empty.
	transformCycleProduction (productionNT, setOfCycleSets, callTrace, namingDomain);
}


void SpecializeGrammar::RemoveCFGCycles::transformCycleProduction (CFG::NonTerminal* productionNT, Util::SetOfCycleSets* currentCycleSets, Util::CallTrace callTrace, Util::NamingDomain* namingDomain) {
	CFG::GrammarProduction* production = this->oldGrammar->getProduction (productionNT);
	CFG::NonTerminal* lhs = new CFG::NonTerminal (*production->lhs);
	// First we get an existing alias or create a new alias name for
	// the current non-terminal, BEFORE we wrap a new naming domain
	// around the current one. This ensures that the chosen name
	// will persist after we return from this method. This is
	// an intended behaviour, because the calling site is either
	// 'transformCycleElement' or 'transformElementStrict', which
	// will make use of this name directly after the call.
	std::string* newNonTerminalName = namingDomain->getAlias (lhs->getName());
	std::cout << "#created NT alias " << *newNonTerminalName << " for NT " << *lhs->getName() << std::endl;
	// Now wrap the naming-domain.
	namingDomain = new Util::NamingDomain (namingDomain);
	// Also push the current non-terminal on the call-trace.
	callTrace.push (productionNT, currentCycleSets);
	// Next create a new non-terminal instance for the new production.
	CFG::NonTerminal* newNonTerminal = new CFG::NonTerminal (newNonTerminalName);
	
	// Before a new produntion is created for the non-terminal, we
	// check if the non-terminal is not part of a cycle, and the production
	// is already in the CFG. In This case it is uneccesary to re-run
	// through the productions of the NT, because they would only lead to
	// unused productions, because this newly created production would simply
	// overwrite the previously created one, but with a own set of referenced
	// productions instead, leaving all referenced productions of the replaced
	// production dangling without any production that points to them.
	if (true) {
		if (this->newGrammar->containsProduction (newNonTerminal)) {
			return;
		}
	}
	
	// Create a new grammar production with a new name as well.
	CFG::GrammarProduction* newProduction = new CFG::GrammarProduction (newNonTerminal);
	CFG::Base* rhsResult = transformCycleElement (production->rhs, callTrace, namingDomain);
	assert (rhsResult->is (CFG::PRODUCTION_ALTERNATIVE));
	CFG::ProductionAlternative* resultAlts = dynamic_cast<CFG::ProductionAlternative*> (rhsResult);
	if (resultAlts->numberOfAlternatives() == 0) {
		// this is a collapsed production. Mark the non-terminal
		// in our set of collapsed non-terminals, before we go
		// on with processing
		this->collapsedProductions.insert (*newNonTerminalName);
	}
	else {
		newProduction->rhs = (CFG::ProductionAlternative*)rhsResult;
		// Before the new production can be added to the new grammar,
		// we must annotate the grammar production with a hideaway
		// attribute. This attribute contains all collapsed non-terminals.
		if (this->hiddenCFGFragments.find (*lhs->getName()) != this->hiddenCFGFragments.end()) {
			std::set< CFG::Base* >* fragments = this->hiddenCFGFragments[*lhs->getName()];
			HiddenCFGFragmentsAttribute* hiddenCFGFragmentsAttribute = new HiddenCFGFragmentsAttribute();
			hiddenCFGFragmentsAttribute->addHiddenFragments (fragments);
			newProduction->setAttribute (hiddenCFGFragmentsAttribute);
			this->hiddenCFGFragments.erase (*lhs->getName());
		}
		// Each broken cycle creates the need for special treatment
		// of those grammar rules which are not productive in this
		// current CFG (supposedly they were productive in the original
		// grammar but lost some of the terminal parsers during the
		// transformation through the string algebra), and must be
		// generated each time a productive part of the CFG is parsed.
		std::set<CyclePathInfo*>* setOfCycleSetInfos = getCyclePathsSearchSetElement (*productionNT->getName());
		if (setOfCycleSetInfos != NULL) {
			Util::CyclePathInfoAttribute* cyclePathInfoAttribute = new Util::CyclePathInfoAttribute();
			for (std::set<CyclePathInfo*>::iterator i = setOfCycleSetInfos->begin(); i != setOfCycleSetInfos->end(); i++) {
				cyclePathInfoAttribute->addElements ((*i)->path, (*i)->startPosInPath);
			}
			newProduction->setAttribute (cyclePathInfoAttribute);
		}
		// Now just add the new production.
		this->newGrammar->addProduction (newProduction);
	}
	// At last we dispose the naming domain we wrapped
	// around the parameter one.
	delete (namingDomain);
}


CFG::Base* SpecializeGrammar::RemoveCFGCycles::transformCycleElement (CFG::Base* b, Util::CallTrace callTrace, Util::NamingDomain* namingDomain) {
	switch (b->getType()) {
		case CFG::BASE_WRAPPER: {
			std::cout << "cyclic wrapper "; Printer::PrettyPrintCOut pp; pp.ppBase (NULL, b); std::cout << std::endl;
			CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
			
			CFG::Base* result = transformCycleElement (wrapper->getWrappedBase(), callTrace, namingDomain);
			if (result != NULL) {
				result = new CFG::BaseWrapper (result);
				copyAttributes (wrapper, result);
			}
			
			return result;
		}
		case CFG::NONTERMINAL: {
			std::cout << "cyclic nonterminal "; Printer::PrettyPrintCOut pp; pp.ppBase (NULL, b); std::cout << std::endl;
			CFG::NonTerminal* nt = dynamic_cast<CFG::NonTerminal*> (b);
			
			// Sometimes this method gets called when a single non-terminal
			// is not part of a cycle (directly by the part which dissects the
			// production alternatives, which is also part of this switch statemtn)
			// If the non-terminal has no cycle-set information annotated to it,
			// it is not part of a cycle.
			std::set<Util::CycleSet*> cycleSets = getCycleMarkSets (nt);
			if (cycleSets.empty()) {
				return transformElementStrict (nt, callTrace, namingDomain);
			}
			
			// Detect cycles here! Depending on this analysis, we either
			// leave the non-terminal unaltered (i.g. just clone it) or
			// we hide it away, that is put it in the algebra function
			// instead of the CFG.
			// It is not sufficient to check if the non-terminal has already
			// been processed (e.g. visitedNonTerminals.find (nt->getName()) == visitedNonTerminals.end())
			// because we only cut off a cycling non-terminal, if it is a
			// backward directed reference.
			//if (nonTerminalIsBackReference (nt, callTrace)) {
			if (nonTerminalClosesCycle (nt, callTrace)) {
				// Since this is the breaking element of a cycle, we annotate
				// it with an attribute. This helps us later identifying the
				// element out of a sequence of elements which is responsible
				// for hiding the whole alternative.
				nt->setAttribute (new CycleBreakPointAttribute());
				
				// We extract the information about the path this cycle took
				// through the different productions, and store it in a
				// separate list structure. This list structure will be used
				// after a whole production is transformed in a post processing
				// step which annotates the new CFG production with information
				// about all paths of broken cycles.
				extractMetaCycleInfoFromTrace (*nt->getName(), namingDomain);
				
				// Then just return NULL as a sign that we drop this element.
				return NULL;
			}
			else {
				// Before the recursive call to 'transformProduction' is made,
				// we create an additional layer of the naming-domain, and query
				// the name of the non-terminal. This creates a unique new name
				// for each non-terminal which is part of a cycle. We need this,
				// because productions for non-terminal calls on a cycle track
				// can collaps. This kind of productions are not correct for those
				// non-terminals which are not part of a cycle.
				Util::NamingDomain* newNamingDomain = new Util::NamingDomain (namingDomain);
				// Get an alias name for the non-terminal.
				std::string* newNonTerminalName = newNamingDomain->getAlias (nt->getName());
				std::cout << "got new NT name: " << *newNonTerminalName << " for NT " << *nt->getName() << std::endl;
				// Only if this non-terminal is not already on the call-trace,
				// a recursive call is performed. This is the same as in the
				// strict transformation part of this recursion scheme, but only
				// slightly more complicated, we need to check if the
				// transformed production collapsed completely.
				if (!callTrace.contains (nt)) {
					// Transform that production, and clone the original non-terminal.
					// This is the recursive call. We checked in the surrounding
					// if-statement that we are not running in circles.
					transformProduction (nt, callTrace, newNamingDomain);
					// First check if the transformed production resulted in a
					// completely collapsed production. In that case we also return
					// NULL in this branch
					if (this->collapsedProductions.find (*newNonTerminalName) != this->collapsedProductions.end()) {
						// We treat this also like a cycle break-point, because this
						// is the non-terminal which was deleted from the grammar
						// and maybe its surrounding production.
						nt->setAttribute (new CycleBreakPointAttribute());
						return NULL;
					}
				}
				// Just create a normal non-terminal and clone the
				// attributes held by the old one.
				CFG::NonTerminal* newNonTerminal = new CFG::NonTerminal (newNonTerminalName);
				copyAttributes (nt, newNonTerminal);
				return newNonTerminal;
			}
		}
		case CFG::PRODUCTION_SEQUENCE: {
			std::cout << "cyclic sequence ";Printer::PrettyPrintCOut pp; pp.ppBase (NULL, b); std::cout << std::endl;
			CFG::ProductionSequence* oldSequence = dynamic_cast<CFG::ProductionSequence*> (b);
			
			// get the non-terminal name that belongs to the grammar
			// rule this CFG node is part of:
			std::string grammarRuleNonTerminalName = callTrace.peek().first;
			CFG::NonTerminal* grammarRuleNonTerminal = new CFG::NonTerminal (new std::string (grammarRuleNonTerminalName));
			
			// First of all, check out the properties of each
			// sequence element.
			bool allElementsAreNonTerminals = true;
			int numberOfNonTerminalsWithCycleSets = 0;
			int numberOfCyclesOfNonTerminalsInThisSequence = 0;
			int numberOfNonTerminalsNullableOnly = 0;
			int totalNumberOfElements = 0;
			// Stores a pointer to an element of the sequence,
			// which not only derives epsilon alone. If this
			// sequence has only one such element, this variable
			// will point to exactly that one.
			for (CFG::ProductionSequence::iterator i = oldSequence->begin(); i != oldSequence->end(); i++) {
				// Check if all elements are non-terminals
				if (!isWrappedNonTerminal (*i) && !(*i)->is (CFG::NONTERMINAL)) {
					allElementsAreNonTerminals = false;
				}
				// Does the non-terminal have cycle-set information?
				std::set<Util::CycleSet*> cycleSets = getCycleMarkSets (*i);
				if (cycleSets.size() > 0) {
					numberOfNonTerminalsWithCycleSets++;
				}
				Util::SetOfCycleSets setSet (cycleSets);
				if (setSet.containsElement (grammarRuleNonTerminal)) {
					numberOfCyclesOfNonTerminalsInThisSequence++;
				}
				// Does this element derive only epsilon?
				if (Util::CycleSetUtils::elementIsNullableOnly (*i)) {
					numberOfNonTerminalsNullableOnly++;
				}
				totalNumberOfElements++;
			}
			
			
			if (!allElementsAreNonTerminals) {
				return transformElementStrict (oldSequence, callTrace, namingDomain);;
			}
			
			// Now if this is a sequence which consists only of
			// non-terminals, we transform it differently.
			CFG::ProductionSequence* newSequence = new CFG::ProductionSequence();
			copyAttributes (oldSequence, newSequence);
			
			// if there is only one element in this sequence, which
			// derives not only epsilon, we use this method itself
			// recursively, which may break a cycle eventually.
			if (totalNumberOfElements - numberOfNonTerminalsNullableOnly == 1 && numberOfCyclesOfNonTerminalsInThisSequence > 0) {
				bool hideTheWholeSequence = false;
				for (CFG::ProductionSequence::iterator i = oldSequence->begin(); i != oldSequence->end(); i++) {
					// Here we distinguish between the elements which reduce only
					// to epsilon, and this one element in the sequence which
					// reduces to something else. The former needs to be transformed
					// strict, because we absolutely need this call, since even
					// a parser which yields epsilon may have an algebra function
					// applied to it. The latter needs to be transformed via the
					// cycle-transformation method, because we are interested in
					// the result, especially if the result is NULL, which means
					// that the productive element was used to break the cycle.
					// In that case, we hide the whole sequence.
					if (!Util::CycleSetUtils::elementIsNullableOnly (*i)) {
						CFG::Base* transformedProductiveElement = transformCycleElement (*i, callTrace, namingDomain);
						if (transformedProductiveElement == NULL) {
							// The cycle was broken at exactly this point,
							// thus we hide away the whole sequence.
							hideTheWholeSequence = true;
						}
						else {
							newSequence->append (transformedProductiveElement);
						}
					}
					else {
						CFG::Base* transformedResult = transformElementStrict (*i, callTrace, namingDomain);
						newSequence->append (transformedResult);
					}
				}
				// If we discard the whole sequence, we just return NULL.
				// The sequence is discarded, when a cycle is broken, while
				// all other sequence elements derive only epsilon.
				if (hideTheWholeSequence) {
					return NULL;
				}
			}
			else {
				// Then transform each element according to our analysis
				// of each sequence element.
				for (CFG::ProductionSequence::iterator i = oldSequence->begin(); i != oldSequence->end(); i++) {
					CFG::Base* transformedElement = transformElementStrict (*i, callTrace, namingDomain);
					// We started a strict transformation, this may not
					// result in a NULL pointer!
					assert (transformedElement != NULL);
					newSequence->append (transformedElement);
				}
			}
			
			return newSequence;
		}
		case CFG::PRODUCTION_ALTERNATIVE: {
			std::cout << "cyclic alternative ";Printer::PrettyPrintCOut pp; pp.ppBase (NULL, b); std::cout << std::endl;
			CFG::ProductionAlternative* oldAlternatives = dynamic_cast<CFG::ProductionAlternative*> (b);
			CFG::ProductionAlternative* newAlternatives = new CFG::ProductionAlternative();
			copyAttributes (oldAlternatives, newAlternatives);
			
			for (CFG::ProductionAlternative::iterator i = oldAlternatives->begin(); i != oldAlternatives->end(); i++) {
				// Push an element onto the production fragment trace here, because
				// this is the place where production alternatives are handled, and
				// since the trace stores alternatives of the top level,
				// this is where they are available. Before this loop starts over,
				// we also remove the top most element from the stack. In that way
				// the stack always has a trace of all production alternatives that
				// led to any CFG node fragment handled by this method.
				this->currentProductionFragmentTrace.push_back (std::pair<std::string, CFG::Base*> (callTrace.peek().first, *i));
				CFG::Base* transformedElement = transformCycleElement (*i, callTrace, namingDomain);
				if (transformedElement == NULL) {
					// This element is hidden away, so add an entry
					// to the hideaway-map. The top of the call-trace
					// will contain the non-terminal of the production
					// this fragment is hidden in. The hidden fragment
					// itself translated according to the new naming
					// maintained by the naming-domain instance.
					insertCFGFragmentIntoHideawayMap (callTrace.peek().first, translateNaming (*i, namingDomain));
				}
				else {
					newAlternatives->addAlternative (transformedElement);
				}
				// Now before the loop starts over again, remove the top
				// element from the stack, because that element has been processed
				// and we turn to the next production alternative, which will then
				// be places on top of the stack.
				this->currentProductionFragmentTrace.pop_back();
			}
			
			// If all alternatives were reduced to NULL, we mark this
			// alternative as 'collapsed', which may be used in later
			// stages of the transformation when algebra functions are
			// generated and enriched with all reduces non-terminal
			// calls.
			if (newAlternatives->numberOfAlternatives() == 0) {
				newAlternatives->setAttribute (NULL);
			}
			
			return newAlternatives;
		}
		default: {
			return b->clone();
		}
	}
}


CFG::Base* SpecializeGrammar::RemoveCFGCycles::transformElementStrict (CFG::Base* b, Util::CallTrace callTrace, Util::NamingDomain* namingDomain) {
	switch (b->getType()) {
		case CFG::BASE_WRAPPER: {
			std::cout << "strict wrapper ";Printer::PrettyPrintCOut pp; pp.ppBase (NULL, b); std::cout << std::endl;
			CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
			
			CFG::Base* result = transformElementStrict (wrapper->getWrappedBase(), callTrace, namingDomain);
			assert (result != NULL);
			result = new CFG::BaseWrapper (result);
			copyAttributes (wrapper, result);
			
			return result;
		}
		case CFG::NONTERMINAL: {
			std::cout << "strict nonterminal ";Printer::PrettyPrintCOut pp; pp.ppBase (NULL, b); std::cout << std::endl;
			CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (b);
			// If the call-trace does not contain this non-terminal,
			// we first do a recursive call, to work through its
			// grammar rules. This check is a simple version of the
			// check performed in the method 'transformCycleElement',
			// and is intended to prevent endless recursion. Since we
			// create a clone of each element, no matter where this
			// non-terminal points to, we simply need to prevent the
			// case of repeated processing of any grammar-rule.
			if (!callTrace.contains (nonTerminal)) {
				transformProduction (nonTerminal, callTrace, namingDomain);
			}
			// Get an alias name for the non-terminal.
			std::string* newNonTerminalName = namingDomain->getAlias (nonTerminal->getName());
			// Just create a normal non-terminal
			CFG::NonTerminal* newNonTerminal = new CFG::NonTerminal (newNonTerminalName);
			copyAttributes (nonTerminal, newNonTerminal);
			return newNonTerminal;
		}
		case CFG::PRODUCTION_SEQUENCE: {
			std::cout << "strict sequence ";Printer::PrettyPrintCOut pp; pp.ppBase (NULL, b); std::cout << std::endl;
			CFG::ProductionSequence* oldSequence = dynamic_cast<CFG::ProductionSequence*> (b);
			CFG::ProductionSequence* newSequence = new CFG::ProductionSequence();
			copyAttributes (oldSequence, newSequence);
			
			for (CFG::ProductionSequence::iterator i = oldSequence->begin(); i != oldSequence->end(); i++) {
				newSequence->append (transformElementStrict (*i, callTrace, namingDomain));
			}
			
			return newSequence;
		}
		case CFG::PRODUCTION_ALTERNATIVE: {
			std::cout << "strict alternative ";Printer::PrettyPrintCOut pp; pp.ppBase (NULL, b); std::cout << std::endl;
			CFG::ProductionAlternative* oldAlternatives = dynamic_cast<CFG::ProductionAlternative*> (b);
			CFG::ProductionAlternative* newAlternatives = new CFG::ProductionAlternative();
			copyAttributes (oldAlternatives, newAlternatives);
			
			for (CFG::ProductionAlternative::iterator i = oldAlternatives->begin(); i != oldAlternatives->end(); i++) {
				newAlternatives->addAlternative (transformElementStrict (*i, callTrace, namingDomain));
			}
			
			return newAlternatives;
		}
		default: {
			return b->clone();
		}
	}
}


CFG::Base* SpecializeGrammar::RemoveCFGCycles::translateNaming (CFG::Base* b, Util::NamingDomain* namingDomain) {
	switch (b->getType()) {
		case CFG::BASE_WRAPPER: {
			CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
			
			CFG::Base* result = translateNaming (wrapper->getWrappedBase(), namingDomain);
			assert (result != NULL);
			result = new CFG::BaseWrapper (result);
			copyAttributes (wrapper, result);
			
			return result;
		}
		case CFG::NONTERMINAL: {
			CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (b);
			// Get an alias name for the non-terminal.
			std::string* newNonTerminalName = namingDomain->getAlias (nonTerminal->getName());
			// Just create a normal non-terminal
			CFG::NonTerminal* newNonTerminal = new CFG::NonTerminal (newNonTerminalName);
			copyAttributes (nonTerminal, newNonTerminal);
			return newNonTerminal;
		}
		case CFG::PRODUCTION_SEQUENCE: {
			CFG::ProductionSequence* oldSequence = dynamic_cast<CFG::ProductionSequence*> (b);
			CFG::ProductionSequence* newSequence = new CFG::ProductionSequence();
			copyAttributes (oldSequence, newSequence);
			
			for (CFG::ProductionSequence::iterator i = oldSequence->begin(); i != oldSequence->end(); i++) {
				newSequence->append (translateNaming (*i, namingDomain));
			}
			
			return newSequence;
		}
		case CFG::PRODUCTION_ALTERNATIVE: {
			CFG::ProductionAlternative* oldAlternatives = dynamic_cast<CFG::ProductionAlternative*> (b);
			CFG::ProductionAlternative* newAlternatives = new CFG::ProductionAlternative();
			copyAttributes (oldAlternatives, newAlternatives);
			
			for (CFG::ProductionAlternative::iterator i = oldAlternatives->begin(); i != oldAlternatives->end(); i++) {
				newAlternatives->addAlternative (translateNaming (*i, namingDomain));
			}
			
			return newAlternatives;
		}
		default: {
			return b->clone();
		}
	}
}


void SpecializeGrammar::RemoveCFGCycles::extractMetaCycleInfoFromTrace (std::string startNT, Util::NamingDomain* namingDomain) {
	unsigned int numberOfElements = this->currentProductionFragmentTrace.size();
	unsigned int startNTPos = 0;
	// Find the start point of the cycle in the path of
	// CFG node elements.
	for (unsigned int i = 0; i < this->currentProductionFragmentTrace.size(); i++) {
		if (this->currentProductionFragmentTrace[i].first == startNT) {
			startNTPos = i;
			break;
		}
	}
	// Then create a new list instance which contains only
	// those CFG node pointers which belong to the cycle path.
	std::list< std::pair<std::string, CFG::Base*> >* path = new std::list< std::pair<std::string, CFG::Base*> >();
	for (unsigned int i = startNTPos; i < this->currentProductionFragmentTrace.size(); i++) {
		std::pair<std::string, CFG::Base*> element = this->currentProductionFragmentTrace[i];
		path->push_back (std::pair<std::string, CFG::Base*> (*namingDomain->getAlias (element.first), translateNaming (element.second, namingDomain)));
	}
	// Lastly, add a new entry for each visited non-terminal
	// on the path to the non-terminal-to-path-info-map for
	// each non-terminal that lies on the path.
	for (unsigned int i = startNTPos; i < this->currentProductionFragmentTrace.size(); i++) {
		std::pair<std::string, CFG::Base*> element = this->currentProductionFragmentTrace[i];
		CyclePathInfo* pathInfo = new CyclePathInfo();
		pathInfo->nonTerminalName = element.first;
		pathInfo->startPosInPath = i - startNTPos;
		pathInfo->path = path;
		insertCyclePathsSearchSetElement (element.first, pathInfo);
	}
}


void SpecializeGrammar::RemoveCFGCycles::insertCyclePathsSearchSetElement (std::string nonTerminalName, CyclePathInfo* pathInfo) {
	if (this->cyclePathsSearchSet.find (pathInfo->nonTerminalName) == this->cyclePathsSearchSet.end()) {
		this->cyclePathsSearchSet[pathInfo->nonTerminalName] = new std::set<CyclePathInfo*>();
	}
	std::set<CyclePathInfo*>* setOfPathInfos = this->cyclePathsSearchSet[pathInfo->nonTerminalName];
	setOfPathInfos->insert (pathInfo);
}


void SpecializeGrammar::RemoveCFGCycles::removeCyclePathsSearchSetElement (std::string nonTerminalName) {
	if (this->cyclePathsSearchSet.find (nonTerminalName) != this->cyclePathsSearchSet.end()) {
		this->cyclePathsSearchSet.erase (nonTerminalName);
	}
}


std::set<SpecializeGrammar::CyclePathInfo*>* SpecializeGrammar::RemoveCFGCycles::getCyclePathsSearchSetElement (std::string nonTerminalName) {
	if (this->cyclePathsSearchSet.find (nonTerminalName) != this->cyclePathsSearchSet.end()) {
		return this->cyclePathsSearchSet[nonTerminalName];
	}
	return NULL;
}


std::set<Util::CycleSet*> SpecializeGrammar::RemoveCFGCycles::getCycleSets (CFG::Base* b) {
	Util::Attribute* attribute = b->getAttribute ("Util::CycleAttribute");
	Util::CycleAttribute* cycleAttribute = (Util::CycleAttribute*)attribute;
	
	if (cycleAttribute != NULL) {
		std::cout << "-found a cycleset for fragment" << std::endl;
		return cycleAttribute->getCycleSets();
	}
	else {
		std::cout << "-found NO cycleset for fragment" << std::endl;
		return std::set<Util::CycleSet*>();
	}
}


std::set<Util::CycleSet*> SpecializeGrammar::RemoveCFGCycles::getCycleMarkSets (CFG::Base* b) {
	CFG::Base* element = b;
	if (isWrappedNonTerminal (b)) {
		CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
		element = wrapper->getWrappedBase();
	}
	
	Util::Attribute* attribute = element->getAttribute ("Util::CycleMarkAttribute");
	Util::CycleMarkAttribute* cycleMarkAttribute = (Util::CycleMarkAttribute*)attribute;
	
	if (cycleMarkAttribute != NULL) {
		std::cout << "-found a cycleMarkSet for fragment" << std::endl;
		return cycleMarkAttribute->getCycleSets();
	}
	else {
		std::cout << "-found NO cycleMarkSet for fragment" << std::endl;
		return std::set<Util::CycleSet*>();
	}
}


std::set<Util::CycleSet*> SpecializeGrammar::RemoveCFGCycles::getCycleSets (CFG::GrammarProduction* p) {
	Util::Attribute* attribute = p->getAttribute ("Util::CycleAttribute");
	Util::CycleAttribute* cycleAttribute = (Util::CycleAttribute*)attribute;
	
	if (cycleAttribute != NULL) {
		std::cout << "-found a cycleset for production" << std::endl;
		return cycleAttribute->getCycleSets();
	}
	else {
		std::cout << "-found NO cycleset for production" << std::endl;
		return std::set<Util::CycleSet*>();
	}
}


bool SpecializeGrammar::RemoveCFGCycles::elementIsReducible (CFG::Base* b) {
	Util::Attribute* attribute = b->getAttribute ("Util::ReducibleElementAttribute");
	Util::ReducibleElementAttribute* reducibleAttribute = (Util::ReducibleElementAttribute*)attribute;
	
	if (reducibleAttribute != NULL) {
		return true;
	}
	else {
		return false;
	}
}


bool SpecializeGrammar::RemoveCFGCycles::nonTerminalIsBackReference (CFG::NonTerminal* nonTerminal, Util::CallTrace callTrace) {
	if (callTrace.contains (nonTerminal)) {
		std::pair<std::string, Util::SetOfCycleSets*> traceElement = callTrace.searchForCycleSetContainingNonTerminal (nonTerminal);
		CFG::NonTerminal* sourceNonTerminal = new CFG::NonTerminal (new std::string (traceElement.first));
		Util::SetOfCycleSets* cycleSet = traceElement.second;
		assert (cycleSet != NULL);
		if (cycleSet == NULL) {
			return true;
		}
		else {
			return cycleSet->isBackReference (nonTerminal, sourceNonTerminal);
		}
	}
	return false;
}


bool SpecializeGrammar::RemoveCFGCycles::nonTerminalClosesCycle (CFG::NonTerminal* nonTerminal, Util::CallTrace callTrace) {
	if (callTrace.contains (nonTerminal)) {
		// The call-trace is a copy on the stack of our
		// parameters for this function. we can safely drop elements
		// from the stack without affecting the rest of the software.
		
		// We check if all elements on the stack belong to a cycle (which
		// will be assumed is our cycle we are expecting). This is done by
		// testing the cycle-set stored for each non-terminal on the call
		// trace for being NULL. Each non-terminal along the way of a cycle
		// must have this set. A non-terminal which has been called as part
		// of a strict transformation will yield a call trace entry with
		// no cycle-set (i.g. NULL, or isEmpty(), depending on the
		// implementation style).
		while (!callTrace.isEmpty() && callTrace.peek().first != *nonTerminal->getName()) {
			std::pair<std::string, Util::SetOfCycleSets*> stackTop = callTrace.peek();
			if (stackTop.second->isEmpty()) {
				return false;
			}
			callTrace.pop();
		}
		// At this point the non-terminal must be the one closing
		// a loop cycle.
		return true;
	}
	return false;
}


bool SpecializeGrammar::RemoveCFGCycles::isWrappedNonTerminal (CFG::Base* b) {
	if (b == NULL) {
		return false;
	}
	
	if (b->is (CFG::BASE_WRAPPER)) {
		CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
		if (wrapper->getWrappedBase()->is (CFG::NONTERMINAL)) {
			return true;
		}
	}
	
	return false;
}


void SpecializeGrammar::RemoveCFGCycles::copyAttributes (CFG::Base* source, CFG::Base* destination) {
	for (Util::Attributable::iterator i = source->begin(); i != source->end(); i++) {
		destination->setAttribute ((*i).second);
	}
}


void SpecializeGrammar::RemoveCFGCycles::insertCFGFragmentIntoHideawayMap (std::string name, CFG::Base* b) {
	if (this->hiddenCFGFragments.find (name) == this->hiddenCFGFragments.end()) {
		this->hiddenCFGFragments[name] = new std::set< CFG::Base* >();
	}
	std::set< CFG::Base* >*  hideawaySet = this->hiddenCFGFragments[name];
	hideawaySet->insert (b);
}

