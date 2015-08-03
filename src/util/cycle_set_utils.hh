

#ifndef __CYCLE_SET_UTILS_HH__
#define __CYCLE_SET_UTILS_HH__


#include "attribute.hh"
#include "annotate_the_set_first.hh"
#include "cycle_mark_attribute.hh"
#include "../cfg/cfg.hh"


namespace Util {
	
	
	// This class provides utility mehtods which can be used anywhere in
	// this project. All methods are static. Preconditions of each method
	// are stated in the method-comment, please see below for more details.
	class CycleSetUtils {
		
		public:
			
			// Returns TRUE if the element if annotated with a Util::FirstSetAttribute
			// which contains epsilon as an element.
			// This method returns FALSE, if the given node is not annotated with
			// such an attribute!
			static bool elementIsNullable (CFG::Base* b) {
				Attribute* attribute = b->getAttribute ("Util::FirstSetAttribute");
				FirstSetAttribute* firstSetAttribute = (FirstSetAttribute*)attribute;
				
				if (firstSetAttribute == NULL) {
					return false;
				}
				else {
					CFG::Epsilon* epsilon = new CFG::Epsilon();
					bool result = firstSetAttribute->getFirstSet().containsElement (epsilon);
					delete (epsilon);
					return result;
				}
			}
			
			
			// Returns TRUE if the element if annotated with a Util::FirstSetAttribute
			// which contains nothing but epsilon as an element.
			// This method returns FALSE, if the given node is not annotated with
			// such an attribute!
			static bool elementIsNullableOnly (CFG::Base* b) {
				Attribute* attribute = b->getAttribute ("Util::FirstSetAttribute");
				FirstSetAttribute* firstSetAttribute = (FirstSetAttribute*)attribute;
				
				if (firstSetAttribute == NULL) {
					return false;
				}
				else {
					CFG::Epsilon* epsilon = new CFG::Epsilon();
					FirstSet firstSet = firstSetAttribute->getFirstSet();
					bool result = firstSet.containsElement (epsilon);
					delete (epsilon);
					return result && firstSet.size() == 1;
				}
			}
			
			
			// Returns TRUE if the element if annotated with a Util::FirstSetAttribute
			// which contains more then one element, excluding epsilon.
			// This method returns FALSE, if the given node is not annotated with
			// such an attribute!
			static bool elementIsProductive (CFG::Base* b) {
				Attribute* attribute = b->getAttribute ("Util::FirstSetAttribute");
				FirstSetAttribute* firstSetAttribute = (FirstSetAttribute*)attribute;
				
				if (firstSetAttribute == NULL) {
					return false;
				}
				else {
					CFG::Epsilon* epsilon = new CFG::Epsilon();
					FirstSet firstSet = firstSetAttribute->getFirstSet();
					delete (epsilon);
					int numberOfElementsWithoutEpsilon = firstSet.size() - (firstSet.containsElement (epsilon) ? 1 : 0);
					return numberOfElementsWithoutEpsilon > 0;
				}
			}
			
			
			// This method returns TRUE if the given non-terminal is
			// part of a cycle. If this is not a non-terminal instance
			// or not part of a cycle, FALSE is returned.
			static bool elementIsPartOfCycle (CFG::Base* b) {
				if (b->getType() != CFG::NONTERMINAL) {
					return false;
				}
				
				Attribute* attribute = b->getAttribute ("Util::CycleMarkAttribute");
				CycleMarkAttribute* cycleMarkAttribute = (CycleMarkAttribute*)attribute;
				
				if (cycleMarkAttribute == NULL) {
					return false;
				}
				else {
					return true;
				}
			}
			
			
	};
	
	
}


#endif	// ifndef __CYCLE_SET_UTILS_HH__

