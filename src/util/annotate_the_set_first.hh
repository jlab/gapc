


#ifndef __THE_SET_FIRST_HH__
#define __THE_SET_FIRST_HH__



#include <set>
#include <string>
#include <map>

#include "../cfg/cfg.hh"
#include "attribute.hh"



namespace Util {
	
	
	// Forward declaration of the FIRST-set data structure.
	class FirstSet;
	
	
	// This class implements the algorithm to generate the set FIRST
	// for each non-terminal of a context free grammar given as an
	// instance of AmbiguityCFG::CFG.
	class AnnotateTheSetFirst {
		
		private:
			
			// Stores all FirstSets of any non-terminal.
			std::map<std::string, Util::FirstSet*> firstSetMap;
			
			
		public:
			
			AnnotateTheSetFirst();
			~AnnotateTheSetFirst();
			
			// Annotates the given grammar's non-terminals
			// with attributes holding the set FIRST of each
			// non-terminal. The set FIRST is the set of terminal
			// symbols a given non-terminal starts with.
			void annotateGrammar (CFG::CFG* grammar);
			
			
		private:
			
			// Extracts the terminals the given fragment starts with.
			// If they have already been present in the FIRST-set,
			// this method returns FALSE.
			bool extractFirstTerminals (FirstSet* firstSet, CFG::Base* fragment);
			// extrats the FIRST-set information from a sequence of
			// production fragments.
			bool extractFirstTerminalsFromSequence (FirstSet* firstSet, CFG::ProductionSequence* seq);
	
			// Annotates all productions of the grammar, and all
			// nodes of its CFG graph.
			void annotateProductions (CFG::CFG* grammar);
			// Annotates the production of the non-terminal with
			// the Util::FirstSetAttribute. It also descends into its
			// CFG graph with the method annotateBase, and writes
			// one annotation to each CFG node.
			void annotateProduction (CFG::GrammarProduction* production);
			// Annotates the given node with a Util::FirstSetAttribute
			// and descends recursively into its CFG graph.
			FirstSet annotateBase (CFG::Base* b);
			// Annotates the attributable element with the given FIRST-set.
			void annotateFirstSet (Attributable* a, FirstSet* set);
			
			
	};
	
	
	// The FIRST set is a set of terminals, with one exception:
	// we extend the normal set of terminals as used in the
	// literatur by an other symbol INF, which represents a cycle
	// in the grammar. Thus each non-terminal which participates
	// in a cycle contains this special symbol in its FIRST set.
	class FirstSet {
		
		private:
			
			// The set of terminals, which are stored internally
			// as strings.
			std::set<std::string> set;
			
			// A flag that is set true, if the set FIRST also contains
			// the special element INF.
			bool containsInfinity;
			
			
		public:
			
			FirstSet();
			~FirstSet();
			
			void addElement (CFG::Epsilon* elem);
			void addElement (CFG::Terminal* elem);
			void addElement (CFG::RegularExpression* elem);
			void addINF();
			void addElements (FirstSet set);
			
			void removeElement (CFG::Epsilon* elem);
			void removeElement (CFG::Terminal* elem);
			void removeElement (CFG::RegularExpression* elem);
			void removeINF();
			
			bool containsElement (CFG::Epsilon* elem);
			bool containsElement (CFG::Terminal* elem);
			bool containsElement (CFG::RegularExpression* elem);
			bool containsINF();
			
			// Removes all elements from the set.
			void clear();
			
			// Calculates the intersection of this instance with a second
			// FIRST-set.
			FirstSet intersect (FirstSet set);
			// Calculates the difference between this instance and the
			// set given as parameter.
			FirstSet difference (FirstSet set);
			// Returns TRUE if this instance contains no elements.
			bool isEmpty();
			// Returns the size of this set, which is the number of elements.
			unsigned int size();
			
			// Returns a string representation of this FIRST-set.
			std::string toString();
			
			
	};
	
	
	// This attribute can be used to annotate any instance of
	// an Attributable subclass with the set FIRST.
	class FirstSetAttribute : public Attribute {
		
		private:
			
			// Stores the set FIRST;
			Util::FirstSet firstSet;
			
			
		public:
			
			FirstSetAttribute (FirstSet firstSet);
			FirstSetAttribute (FirstSetAttribute& a);
			~FirstSetAttribute();
			
			// Returns the set FIRST this attribute holds.
			FirstSet getFirstSet();
			
			virtual Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __THE_SET_FIRST_HH__

