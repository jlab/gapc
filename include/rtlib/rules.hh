#ifndef RULES
#define RULES


#include "boost/format.hpp"
#include <iostream>


//this file creates the datatype "rules" for Bellman's GAP which is a double hash and should hold production rules of a grammar. This is necessary for generating thermodynamic matchers given a specific shape string.
struct rules {
	
	bool empty_;
	
	Rope shape;
	Rope signatureName;
	Rope axiomName;
	std::map<Rope, std::map<Rope, bool> > productions;
	
	
	rules() : empty_ (false) {
	}
	
	
	//rules(int i) : empty_(false) {
	//}
	
	
	rules& operator+= (const rules &a) {
		return *this;
	}
	
	
	void insertProduction (Rope nt, Rope rhs) {
		productions[nt].insert(std::pair<Rope,bool> (rhs,true));
	}
	
	
	void setShape (Rope s) {
		shape = s;
	}
	
	
	Rope toRope() const {
		Rope res = "grammar grmmr uses " + signatureName + " (axiom = " + axiomName + ") {\n";
		std::map<Rope, std::map<Rope, bool> >::const_iterator nt;
		std::map<Rope, bool>::const_iterator rhs;
		for(nt = productions.begin(); nt != productions.end(); nt++) {
			append (res, "  ", 2);
			append (res, nt->first);
			append (res, " = ", 3);
			for (rhs = nt->second.begin(); rhs != nt->second.end(); rhs++) {
				append(res, rhs->first);
				if (rhs != (--nt->second.end())) {
					append (res, " | ");
				}
			}
			append (res, " # h;\n", 6);
		}
		append (res, "}");
		return res;
	}
	
	
};


inline std::ostream &operator<<(std::ostream &s, const rules &r) {
	if (r.empty_) {
		s << 'E';
	}
	else {
		//~ s << "==== rep = '" << r.shape << "' === \n";
		s << r.toRope();
	}
	return s;
}


inline void empty(rules &e) {
	e.empty_ = true; 
}


inline bool isEmpty(const rules &e) {
	return e.empty_; 
}


inline void insertProduction(rules &me, Rope nt, Rope rhs) {
	me.insertProduction(nt, rhs);
}


inline void setShape(rules &me, Rope s) {
	me.setShape(s);
}


//In level 1 it might happen that two subshapes must be concatenated that both have unpaired bases at their tail and head, e.g. []_ and _[].
//In such a case, the concatenation is not simply ++ ([]_ + _[] != []__[]), but must recognize the double unpaired stretch and fuse them into one, such that []_ + _[] = []_[]
inline void appendShape(rules &me, Rope y) {
	Rope res;
	if (me.shape.size() <= 0) {
		res = y;
	}
	else if (y.size() <= 0) {
		res = me.shape;
	}
	else {
		res = me.shape;
		
		std::ostringstream left;
		left << me.shape;
		
		if (left.str()[left.str().length()-1] == '_') {
			std::ostringstream right;
			right << y;
			if (right.str()[0] != '_') {
				append(res, right.str()[0]);
			}
			for (unsigned int i = 1; i < right.str().length(); i++) {
				append(res, right.str()[i]);
			}
		}
		else {
			append(res, y);
		}
	}
	me.setShape(res);
}


inline rules operator+ (const rules &x, const rules &y) {
	rules res = x;
	res.setShape(x.shape + y.shape);
	std::map<Rope, std::map<Rope, bool> >::const_iterator nt;
	std::map<Rope, bool>::const_iterator rhs;
	for(nt = y.productions.begin(); nt != y.productions.end(); nt++) {
		for (rhs = nt->second.begin(); rhs != nt->second.end(); rhs++) {
			res.productions[nt->first].insert(std::pair<Rope,bool>(rhs->first, true));
		}
	}
	return res;
}


inline Rope toRope (const rules &me) {
	return me.toRope();
}


// NOT USED, see 'merge' below.
//for choice function: combines rules of several parses, which is necessary for the ambiguous grammar for shape level 1 and macrostate
inline rules merge (std::pair<List<rules, unsigned char>::Iterator, List<rules, unsigned char>::Iterator>& xs) {
	rules res;
	if (xs.first == xs.second) {
		empty(res);
		return res;
	}
	assert (!isEmpty(*xs.first));
	for (; xs.first != xs.second; ++xs.first) {
		Rope shape = (*(xs.first)).shape;
		res = res + *xs.first;
		setShape (res, shape);
	}
	return res;
}


inline rules merge (List_Ref<rules>& xs) {
	rules res;
	Rope shape;
	
	for (List_Ref<rules>::iterator i = xs->begin(); i != xs->end(); ++i) {
		shape = (*i).shape;
		res = res + *i;
	}
	
	res.shape = shape;
	return res;
}


// Returns a list of different rule sets. Each rule set from the
// parameter 'xs' is merged into a result rules instance if their
// shapes are equal. Hence the result list contains rules where
// no shape is the same as in any other rules instance of the result.
inline List_Ref<rules> groupByShape (List_Ref<rules>& xs) {
	std::map<Rope, rules> shapeMap;
	
	for (List_Ref<rules>::iterator i = xs->begin(); i != xs->end(); ++i) {
		rules r;
		if (shapeMap.find ((*i).shape) != shapeMap.end()) {
			r = shapeMap[(*i).shape];
		}
		r = r + *i;
		shapeMap[(*i).shape] = r;
	}
	
	List_Ref<rules> result;
	for (std::map<Rope, rules>::iterator i = shapeMap.begin(); i != shapeMap.end(); i++) {
		result->push_back ((*i).second);
	}
	return result;
}


inline Rope getRuleNameDebug (const Rope& r, const Rope& shape) {
	return r + "_" + shape;
}


inline Rope getRuleName (const Rope& r, const Rope& shape) {
	static std::map<Rope, Rope> mapping;
	static unsigned int ruleCounter = 0;
	
#ifdef SPECIALIZE_GRAMMAR_DEBUG
	return getRuleNameDebug (r, shape);
#endif
	
	Rope queryName = r + "#" + shape;
	if (mapping.find (queryName) == mapping.end()) {
		Rope newRuleName = Rope ("auto_gen_rule_");
		append (newRuleName, ruleCounter++);
		mapping[queryName] = newRuleName;
	}
	return mapping[queryName];
}


inline void setAxiomName (rules& r, Rope axiom) {
	r.axiomName = axiom;
}


inline void setSignatureName (rules& r, Rope sig) {
	r.signatureName = sig;
}


typedef rules res_type;		// deprecated; just for testing, remove later!
typedef rules answer_type;
typedef Rope string_type;


// Matches the given string with the input sequence's content.
// Returns TRUE is the given string is the same (length and byte
// by byte content) with the current input sequence section
// between i and j.
template<typename alphabet, typename pos_type, typename T>
inline bool matchString (const Basic_Sequence<alphabet, pos_type> &seq, T i, T j, const std::string str) {
	if (j - i != str.size()) {
		return false;
	}
	for (int pos = 0; pos < str.size(); pos++) {
		if (str.at (pos) != seq[i + pos]) {
			return false;
		}
	}
	return true;
}



#endif
