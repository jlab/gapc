/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2008-2011  Georg Sauthoff
         email: gsauthof@techfak.uni-bielefeld.de or gsauthof@sdf.lonestar.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

}}} */

#ifndef SRC_CFG_CFG_HH_
#define SRC_CFG_CFG_HH_


#include <string>
#include <vector>
#include <list>

#include "../hashtable.hh"

#include "../util/attributable.hh"



// This namespace defines the whole structure for a context free
// grammar as it is used as output format of the canonical algebra
// string grammar generator.
namespace CFG {


// Forward-declaration, they are needed by the class CFG.
class NonTerminal;
class GrammarProduction;
class RegularExpression;


// A context free grammar is a set of grammar rules. Each rule associates
// a set of productions to a nonterminal. A production is built from a
// non-terminal, a terminal, a sequence of terminals and non-terminals
// or a set of alternatives of productions (note: recursive description!)
class CFG : public Util::Attributable {
 private:
    // Stores an association of non-terminal names and grammar
    // productions
    hashtable<std::string, GrammarProduction*> grammarProductions;
    // Stores the axiom of the grammar.
    NonTerminal *axiom;
    // A mapping of all names of regular expression to the
    // expression itself
    hashtable<std::string, RegularExpression*> regularExpressions;

 public:
    CFG();
    ~CFG();

    // Sets the non-terminal as the axiom of the grammar. If
    // no grammar rule exists for the given non-terminal, an
    // exception is thrown. The first grammar rule added to
    // this grammar also sets the axiom to the left-hand-side
    // of that rule.
    void setAxiom(NonTerminal *nt);
    // Returns the axiom of the grammar.
    NonTerminal* getAxiom();

    // Adds the production given as parameter to the list
    // of productions of this grammar. If this is the first
    // production added, its left-hand-side is also set as
    // axiom for the grammar.
    void addProduction(GrammarProduction* prod);
    // Returns 'true' if a grammar-rule exists for the non-terminal,
    // otherwise 'false'.
    bool containsProduction(NonTerminal* nt);
    // Returns a production that is stored for a given non-terminal
    // If no rule can be found, a LogError is thrown. This method
    // uses directly the method getProduction (std::string* productionName)
    // of this class to get its job done.
    GrammarProduction* getProduction(NonTerminal* nonTerminal);
    // Returns a production that is stored for a non-terminal
    // whose name equals the parameter value 'productionName'.
    // If no rule can be found, a LogError is thrown.
    GrammarProduction* getProduction(std::string* productionName);
    // Returns the list of productions this grammar holds.
    // The first element of this list is always the production
    // belonging to the axiom of the grammar.
    std::list<GrammarProduction*> getProductions();

    // Adds a new regular expression to the table of regular
    // expressions of this grammar. If a regular expression with
    // the same name already exists, the old one will be replaced
    // by the new one given as parameter.
    void addRegularExpression(RegularExpression* regexp);
    // Returns a list of all regular expressions defined for
    // this grammar.
    std::list<RegularExpression*> getRegularExpressions();
};


// This enum contains all types that belong to the Base-hirarchy.
// It is part of the simple reflection implemenation.
enum Type {BASE, SNAPSHOT, EPSILON, TERMINAL, NONTERMINAL, REGULAR_EXPRESSION,
PRODUCTION_SEQUENCE, PRODUCTION_ALTERNATIVE, BASE_WRAPPER};


// The common base class of terminal and non-terminal symbols. It serves
// as implementation for a simple reflection mechanism that cpp lacks of.
class Base : public Util::Attributable {
 private:
    // This field is set by the constructor and receives
    // its value when a new instance is initialized. The
    // only constructor of the Base-class requires a Type-value
    // hence every subclass of this Base-class sets a specific
    // value that matches the subclass type.
    Type type;

    // The annotation is usually used to annotate any grammar
    // fragment with the algebra-function name that generated it.
    std::string* annotation;

 public:
    explicit Base(Type t);
    Base(Base& b);
    virtual ~Base();

    // Determines whether a subclass is of given type.
    bool is(Type t);
    // Returns the type value of the instance. This value can
    // be used to programmatically switch between class-types
    // for a given instance.
    Type getType();

    // Sets the annotation, which is a string representation
    // of whatever you like.
    void setAnnotation(std::string* annotation);
    // Returns the annotation.
    std::string* getAnnotation();

    // Creates a copy of the instance
    virtual Base* clone() = 0;

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj) = 0;

    // Combines a subclass of Base with an other subclass of
    // Base and returns an other instance and (potentially)
    // different subclass.
    // Note that combine always returns a new instance while
    // leaving all other instances unchanged.
    virtual Base* combine(Base* b);
};


// A SimpleElement groups those subclasses of Base, that represent
// a undividable entity, i.g. Termina, NonTerminal, RegularExpression.
class SimpleElement : public Base {
 public:
    explicit SimpleElement(Type t);
    SimpleElement(SimpleElement& s);
    virtual ~SimpleElement();

    Base* combine(Base* b);
};


// A ComplexElement goups the subclasses of Base which combine instances
// of base to a new structure, e.g. ProductionSequence, ProductionAlternative.
class ComplexElement : public Base {
 public:
    explicit ComplexElement(Type t);
    ComplexElement(ComplexElement& c);
    virtual ~ComplexElement();
};


// A BaseWrapper is simply a proxy that stores a pointer
// of a CFG::Base element. It is used by the ambiguity-cfg-generator
// to pass the parameter terms over to the grammar-VM. After that
// this terms are recognizable anywhere throughout the CFG graph,
// which enables tracking of parameter expressions that were
// used as input to algebra functions in the first place.
class BaseWrapper : public SimpleElement {
 private:
    // The wrapped value.
    Base* wrappedValue;

 public:
    explicit BaseWrapper(Base* b);
    BaseWrapper(BaseWrapper& wrapper);
    virtual ~BaseWrapper();


    // Returns the wrapped value.
    Base* getWrappedBase();


    // Creates a copy of the instance
    virtual Base* clone();

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj);
};


// A Snapshot of a production fragment represents a copy of
// a fragment at a certain time. Changes that are normally
// merged directly into a production fragment are queued until
// the original version is merged with the list of changes.
class Snapshot : public Base {
 private:
    // A reference to the original version of the
    // production fragment at the time the snapshot
    // has been created.
    Base* originalVersion;
    // Because of the associativity of the combine()
    // function we combine all changes locally without
    // applying them to the original version. This
    // will help dispinguish the original part and then
    // changed part of the production fragment.
    Base* changes;

 public:
    explicit Snapshot(Base* original);
    Snapshot(Snapshot& s);
    virtual ~Snapshot();

    // Returns the original production fragment when the
    // snapshot was taken
    Base* getOriginal();
    // Returns the changes
    Base* getChanges();
    // Adds a new production fragment that might be merged
    // into the original
    void addChange(Base* change);

    // Merges the list of changes with the original
    // production fragment.
    Base* applyChanges();
    // Merges the list of changes with a given 'original'
    // production fragment.
    Base* applyChanges(Base* original);

    // Create a clone from this instance.
    virtual Base* clone();
    // Creates a clone based on the same original, but without
    // the changes.
    Snapshot* cloneWithoutChanges();

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj);
};


// The empty string is represented by this special subclass.
class Epsilon : public SimpleElement {
 public:
    Epsilon();
    Epsilon(Epsilon& e);
    virtual ~Epsilon();

    // Creates a copy of the instance
    virtual Base* clone();

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj);
};


// A Terminal is a constant string of characters used in a
// grammar.
class Terminal : public SimpleElement {
 private:
    // The constant value this terminal represents.
    std::string *value;

 public:
    explicit Terminal(std::string *value);
    Terminal(Terminal& t);
    virtual ~Terminal();

    // Returns the std::string representataion of this terminal.
    std::string* getValue();

    // Creates a copy of the instance
    virtual Base* clone();

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj);

    // We make two terminals comparable for equality. This
    // operator returns TRUE if both string values are the same.
    bool operator== (Terminal& nt) {
      return *this->value == *nt.value;
    }
};


// This is a simple non-terminal.
class NonTerminal : public SimpleElement {
 private:
    // the name of the non-terminal
    std::string *name;

 public:
    explicit NonTerminal(std::string *name);
    NonTerminal(NonTerminal& n);
    virtual ~NonTerminal();

    // Returns the std::string representation of the
    // the non-terminal name.
    std::string *getName();

    // Creates a copy of the instance
    virtual Base* clone();

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj);

    // We make two non-terminals comparable for equality
    bool operator== (NonTerminal& nt) {
      return *this->name == *nt.name;
    }
};


class Bounds {
 private:
    // The minimum number of characters this regular
    // expression must yield.
    int lowerBound;
    // The maximum number of characters this regular
    // expression may yield.
    int upperBound;

 public:
    // Defines the value for "bound not defined."
    static const int UNDEFINED = -1;

    // Inits a new instance.
    Bounds();
    Bounds(Bounds& b);

    // Sets the lower bound of this regular expression, that
    // is the minimum number of characters the expression
    // must yield.
    void setLowerBound(int bound);
    // Gets the lower bound of this expression.
    int getLowerBound();

    // Sets the upper bound of this regular expression, that
    // is the maximum number of characters the expression
    // may yield.
    void setUpperBound(int bound);
    // Sets the upper bound of this expression.
    int getUpperBound();
};


class RegularExpression : public SimpleElement {
 private:
    // The name of a regular expression.
    std::string *name;

    // An expression encoded into terminal symbols
    std::string* expression;

    // Stores the bounds
    Bounds* bounds;

 public:
    // Inits a new instance.
    RegularExpression(std::string* name, std::string* expression);
    RegularExpression(RegularExpression& r);
    virtual ~RegularExpression();

    // Returns the name of the regular expression. The name
    // also includes the bounds (if there are any defined)
    // separated by underscores.
    std::string* getName();
    // Returns the expression encoded in a terminal string.
    std::string* getExpression();
    // Sets the size bounds for this regular expression.
    void setBounds(Bounds* bounds);
    // Returns the size bounds of this regular expression.
    // or NULL if no bounds have been specified.
    Bounds* getBounds();

    // Creates a copy of the instance
    virtual Base* clone();

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj);
};


// forward declaration because both classes use each other.
class ProductionSequence;
class ProductionAlternative;


class ProductionSequence : public ComplexElement {
  friend class ProductionAlternative;

 private:
    // A list of all grammar fragments which are grouped
    // as a sequence by this class.
    std::vector<Base*> sequence;

 public:
    ProductionSequence();
    ProductionSequence(ProductionSequence& s);
    virtual ~ProductionSequence();

    // Appends a fragment to this sequence instance.
    void append(Base *fragment);
    // Returns the number of elements in this sequence.
    int getSize();
    // Returns an element from the sequence at the given
    // position 'pos'.
    Base* elementAt(int pos);

 private:
    // Appends a list of elements to this sequence.
    void append(std::vector<Base*> seq);

 public:
    // Creates a list of combinations of this sequence
    // with the parameter. If the parameter is an alternative
    // node, the list contains all alternatives that can
    // be created from this sequence and each alternative.
    Base* combine(Base *prod);

    // Creates a new instance that is a copy of this instance.
    virtual Base* clone();

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj);

    typedef std::vector<Base*>::iterator iterator;
    iterator begin();
    iterator end();
};


class ProductionAlternative : public ComplexElement {
  friend class ProductionSequence;

 private:
    // A list of all grammar fragments that form a block
    // of alternatives.
    std::vector<Base*> alternatives;

 public:
    ProductionAlternative();
    ProductionAlternative(ProductionAlternative& a);
    explicit ProductionAlternative(std::list<Base*> alts);
    virtual ~ProductionAlternative();

    // Adds an alternative to this block of alternatives.
    void addAlternative(Base *alt);
    // Returns a list of all alternatives held by this
    // block of alternatives.
    // std::list<Base*> getAlternatives();

    // Returns the number of alternatives this node holds.
    unsigned int numberOfAlternatives();

    // Combines this block of alternatives with the given
    // grammar fragment 'prod'.
    Base* combine(Base *prod);

    // Creates a new instance that is a copy of this instance.
    virtual Base* clone();

    // Tests whether two instances of this class are equal.
    // To be equal, both instances must be of the same subclass,
    // and contain the same structural elements.
    virtual bool equals(Base* obj);

    typedef std::vector<Base*>::iterator iterator;
    iterator begin();
    iterator end();
};


// This class is exactly what it name suggests.
class GrammarProduction : public Util::Attributable {
 public:
    NonTerminal *lhs;
    ProductionAlternative *rhs;

 public:
    explicit GrammarProduction(NonTerminal *lhs);
    GrammarProduction(GrammarProduction& p);
    ~GrammarProduction();

    // Removes all alternatives from the rhs-node that are
    // equal. Note that this incorporates only a simple check
    // if there are rules that are equal to each other, not
    // if there are two production fragments that generate the
    // same language. The last question is not decidable since
    // CFG are not closed under intersection.
    void removeDuplicateAlternatives();

    // Creates a deep copy of the grammar production.
    GrammarProduction* clone();
};


}  // namespace CFG


#endif  // SRC_CFG_CFG_HH_
