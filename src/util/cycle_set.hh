

#ifndef __CYCLE_SET_HH__
#define __CYCLE_SET_HH__


#include <list>
#include <set>

#include "../cfg/cfg.hh"


namespace Util {
	
	
	class CycleSet {
		
		private:
			
			// Stores all non-terminal names that belong
			// to a cycle.
			std::set<std::string> set;
			
			// Stores all elements that are in the set of elements
			// in a list, which constitutes an order. The order is
			// establiched first, when the main entry point is set,
			// otherwise the order is unspecified.
			std::list<std::string> orderedElements;
			
			// The main entry point onto the cycle.
			CFG::NonTerminal* mainEntryPoint;
			
			
		public:
			
			CycleSet();
			~CycleSet();
			
			// Sets the main entry point for this cycle.
			void setMainEntryPoint (CFG::NonTerminal* mainEntryPoint);
			// Returns TRUE, if the given non-terminal is the last
			// element of the cycle.
			bool isLastElementInCycle (CFG::NonTerminal* nonTerminal);
			// Returns true, if the source non-terminal is dominated
			// by the destination non-terminal (the destination comes
			// before the source in the cycle-order).
			bool isBackReference (CFG::NonTerminal* source, CFG::NonTerminal* destination);
			
			// Adds a non-terminal to the set.
			void addElement (CFG::NonTerminal* nonTerminal);
			// Addls all elements of the list into the set in that
			// order in which they appear in the list.
			void addElements (std::list<CFG::NonTerminal*> elements);
			// Checks if the non-terminal is already present in
			// this set.
			bool containsElement (CFG::NonTerminal* nonTerminal);
			// Returns TRUE if this instance does not contain any
			// elements.
			bool isEmpty();
			
			// Calculates the intersection of the current instance
			// and the set which is passed as parameter. The new set
			// which is returned contains only those elements which
			// are in this instance as well as in the set given as
			// parameter.
			CycleSet intersect (CycleSet cycleSet);
			// Calculates the difference between the current
			// instance and the set given as parameter and
			// returns a new set which contains those elements
			// which were in this instance, but not in the
			// set which was passed as parameter.
			CycleSet difference (CycleSet cycleSet);
			
			// Determines whether two cycle-sets are equal.
			bool operator== (CycleSet& set);
			
			// Returns a string representation of the set.
			std::string toString();
			
			// The iterator which can be used to walk through all
			// elements in an ordered fashion.
			typedef std::list<std::string>::iterator iterator;
			
			iterator begin();
			iterator end();
			
			
	};
	
	
}


#endif	// ifndef __CYCLE_SET_HH__

