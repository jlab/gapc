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

#include "cfg.hh"

#include <string>
#include <list>
#include <vector>

#include "boost/format.hpp"
#include "../log.hh"


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::CFG::CFG() {
  this->axiom = NULL;
}


CFG::CFG::~CFG() {
}


void CFG::CFG::setAxiom(NonTerminal *nt) {
  if (nt == NULL) {
    throw LogError("Internal: NonTerminal must not be null.");
  }
  if (!this->containsProduction(nt)) {
    throw LogError(
      "gap-00176: Internal: No rule exists for non-terminal '"
      + *nt->getName() + "'.");
  }
  this->axiom = nt;
}


CFG::NonTerminal* CFG::CFG::getAxiom() {
  return this->axiom;
}


void CFG::CFG::addProduction(GrammarProduction *prod) {
  if (this->axiom == NULL) {
    this->axiom = prod->lhs;
  }
  grammarProductions[*prod->lhs->getName()] = prod;
}


bool CFG::CFG::containsProduction(NonTerminal *nt) {
  return grammarProductions.find(*nt->getName()) != grammarProductions.end();
}


CFG::GrammarProduction* CFG::CFG::getProduction(NonTerminal* nonTerminal) {
  return this->getProduction(nonTerminal->getName());
}


CFG::GrammarProduction* CFG::CFG::getProduction(std::string* productionName) {
  if (grammarProductions.find(*productionName) == grammarProductions.end()) {
    throw LogError(
      "gap-00175: Internal: there is no production stored for the given name '"
      + *productionName +
      "': *CFG::CFG::getProduction (std::string *productionName)");
  }
  return grammarProductions[*productionName];
}


std::list<CFG::GrammarProduction*> CFG::CFG::getProductions() {
  std::list<GrammarProduction*> results;

  // if there are no productions associated with this
  // grammar, an empty list is returned
  if (this->axiom == NULL) {
    return results;
  }

  // the first grammar rule in the list is the rule belonging
  // to the axiom of the grammar
  GrammarProduction *axiomProduction = this->getProduction(
    this->axiom->getName());
  results.push_back(axiomProduction);

  // then add all other productions to the result-list
  for (hashtable<std::string, GrammarProduction*>::iterator i =
       grammarProductions.begin(); i != grammarProductions.end(); ++i) {
    // if the grammar production belongs to the axiom,
    // we skip that one, since we already added it before
    // this for-loop.
    if (*this->axiom->getName() != (*i).first) {
      results.push_back((*i).second);
    }
  }

  return results;
}


void CFG::CFG::addRegularExpression(::CFG::RegularExpression* regexp) {
  if (regularExpressions.find(*regexp->getName()) == regularExpressions.end()) {
    regularExpressions[*regexp->getName()] = regexp;
  }
}


std::list<CFG::RegularExpression*> CFG::CFG::getRegularExpressions() {
  std::list<RegularExpression*> result;

  for (hashtable<std::string, RegularExpression*>::iterator i =
       regularExpressions.begin(); i != regularExpressions.end(); ++i) {
    result.push_back((*i).second);
  }

  return result;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::Base::Base(Type t) {
  type = t;
  annotation = new std::string("");
}


CFG::Base::Base(Base& b)
  : Attributable(b) {
  this->type = b.type;
  this->annotation = b.annotation;
}


CFG::Base::~Base() {
}


bool CFG::Base::is(Type t) {
  return t == type;
}


CFG::Type CFG::Base::getType() {
  return type;
}


void CFG::Base::setAnnotation(std::string* annotation) {
  this->annotation = annotation;
}


std::string* CFG::Base::getAnnotation() {
  return this->annotation;
}


CFG::Base* CFG::Base::combine(::CFG::Base *b) {
  // The second operand must not be NULL!
  assert(b != NULL);
  // Depending on the subclass of the current instance proceed as follows:
  switch (this->getType()) {
    case ::CFG::EPSILON: {
      return b->clone();
    }
    case ::CFG::BASE_WRAPPER:
    case ::CFG::TERMINAL:
    case ::CFG::NONTERMINAL:
    case ::CFG::REGULAR_EXPRESSION: {
      ProductionSequence *prod = new ProductionSequence();
      prod->append(this->clone());
      prod->append(b->clone());
      return prod;
    }
    case ::CFG::PRODUCTION_SEQUENCE: {
      ProductionSequence *seq = dynamic_cast<ProductionSequence*>(
        this->clone());
      return seq->combine(b->clone());
    }
    case ::CFG::PRODUCTION_ALTERNATIVE: {
      ProductionAlternative *alt = dynamic_cast<ProductionAlternative*>(
        this->clone());
      return alt->combine(b->clone());
    }
    case ::CFG::SNAPSHOT: {
      Snapshot* snapshot = dynamic_cast<Snapshot*>(this->clone());
      snapshot->addChange(b->clone());
      return snapshot;
    }
    default:
      throw LogError(
        "gap-00101: Internal: The class CFG::Base may not be used directly.");
  }
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::SimpleElement::SimpleElement(Type t)
  : Base(t) {
}


CFG::SimpleElement::SimpleElement(SimpleElement& s)
  : Base(s) {
}


CFG::SimpleElement::~SimpleElement() {
}


CFG::Base* CFG::SimpleElement::combine(::CFG::Base* b) {
  switch (b->getType()) {
    case ::CFG::EPSILON:
      return this->clone();
    case ::CFG::TERMINAL:
    case ::CFG::NONTERMINAL:
    case ::CFG::SNAPSHOT:
    case ::CFG::REGULAR_EXPRESSION:
    case ::CFG::BASE_WRAPPER:
    case ::CFG::PRODUCTION_SEQUENCE: {
      ProductionSequence* sequenceCopy = new ProductionSequence();
      sequenceCopy->append(this->clone());
      sequenceCopy->append(b->clone());
      return sequenceCopy;
    }
    case ::CFG::PRODUCTION_ALTERNATIVE: {
      ProductionAlternative* alt = dynamic_cast<ProductionAlternative*> (b);
      ProductionAlternative* newAlts = new ProductionAlternative();
      // iterate over all alternatives and
      for (ProductionAlternative::iterator i = alt->begin();
           i != alt->end(); ++i) {
        ProductionSequence* sequenceCopy = new ProductionSequence();
        sequenceCopy->append(this->clone());
        sequenceCopy->append((*i)->clone());
        newAlts->addAlternative(sequenceCopy);
      }
      return newAlts;
    }
    default:
      throw LogError(
        "gap-00102: Unhandled CFG fragment type in function implementation\n"
        "CFG::SimpleElement::combine (Base* b).");
  }
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::ComplexElement::ComplexElement(Type t)
  : Base(t) {
}


CFG::ComplexElement::ComplexElement(ComplexElement& c)
  : Base(c) {
}


CFG::ComplexElement::~ComplexElement() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::BaseWrapper::BaseWrapper(Base* wrappedValue)
  : SimpleElement(::CFG::BASE_WRAPPER), wrappedValue(wrappedValue) {
}


CFG::BaseWrapper::BaseWrapper(BaseWrapper& wrapper)
  : SimpleElement(wrapper), wrappedValue(wrapper.wrappedValue->clone()) {
}


CFG::BaseWrapper::~BaseWrapper() {
}


CFG::Base* CFG::BaseWrapper::getWrappedBase() {
  return this->wrappedValue;
}


CFG::Base* CFG::BaseWrapper::clone() {
  return new BaseWrapper(*this);
}


bool CFG::BaseWrapper::equals(Base* obj) {
  if (obj->is(BASE_WRAPPER)) {
    BaseWrapper* wrapper = dynamic_cast<BaseWrapper*> (obj);
    return this->wrappedValue->equals(wrapper->wrappedValue);
  }
  return false;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::Snapshot::Snapshot(Base* original)
  : Base(SNAPSHOT), originalVersion(original), changes(new Epsilon()) {
}


CFG::Snapshot::Snapshot(Snapshot& s)
  : Base(s) {
  this->originalVersion = s.originalVersion->clone();
  this->changes = s.changes->clone();
}


CFG::Snapshot::~Snapshot() {
}


CFG::Base* CFG::Snapshot::getChanges() {
  return this->changes;
}


CFG::Base* CFG::Snapshot::getOriginal() {
  return this->originalVersion;
}


void CFG::Snapshot::addChange(Base* change) {
  this->changes = this->changes->combine(change);
}


CFG::Base* CFG::Snapshot::applyChanges() {
  return this->applyChanges(this->originalVersion);
}


CFG::Base* CFG::Snapshot::applyChanges(Base* original) {
  return original->combine(this->changes);
}


CFG::Base* CFG::Snapshot::clone() {
  return new Snapshot(*this);
}


CFG::Snapshot* CFG::Snapshot::cloneWithoutChanges() {
  return new Snapshot(this->originalVersion->clone());
}


bool CFG::Snapshot::equals(::CFG::Base* obj) {
  if (obj->is(SNAPSHOT)) {
    Snapshot* snapshot = dynamic_cast<Snapshot*>(obj);
    return this->originalVersion->equals(snapshot->originalVersion) &&
           this->changes->equals(snapshot->changes);
  }
  return false;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::Epsilon::Epsilon()
  : SimpleElement(::CFG::EPSILON) {
}


CFG::Epsilon::Epsilon(Epsilon& e)
  : SimpleElement(e) {
}


CFG::Epsilon::~Epsilon() {
}


CFG::Base* CFG::Epsilon::clone() {
  return new Epsilon(*this);
}


bool CFG::Epsilon::equals(::CFG::Base* obj) {
  return obj->is(::CFG::EPSILON);
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::Terminal::Terminal(std::string *v)
  : SimpleElement(::CFG::TERMINAL), value(v) {
}


CFG::Terminal::Terminal(Terminal& t)
  : SimpleElement(t), value(new std::string(*t.value)) {
}


CFG::Terminal::~Terminal() {
}


std::string *CFG::Terminal::getValue() {
  return value;
}


CFG::Base* CFG::Terminal::clone() {
  return new Terminal(*this);
}


bool CFG::Terminal::equals(::CFG::Base* obj) {
  if (obj->is(::CFG::TERMINAL)) {
    Terminal* t = dynamic_cast<Terminal*>(obj);
    return *this->value == *t->value;
  }
  return false;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::NonTerminal::NonTerminal(std::string *name)
  : SimpleElement(::CFG::NONTERMINAL), name(name) {
}


CFG::NonTerminal::NonTerminal(NonTerminal& n)
  : SimpleElement(n), name(new std::string(*n.name)) {
}


CFG::NonTerminal::~NonTerminal() {
}


std::string *CFG::NonTerminal::getName() {
  return this->name;
}


CFG::Base* CFG::NonTerminal::clone() {
  return new NonTerminal(*this);
}


bool CFG::NonTerminal::equals(::CFG::Base* obj) {
  if (obj->is(::CFG::NONTERMINAL)) {
    NonTerminal* nt = dynamic_cast<NonTerminal*> (obj);
    return *this->name == *nt->name;
  }
  return false;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::Bounds::Bounds()
  : lowerBound(::CFG::Bounds::UNDEFINED), upperBound(::CFG::Bounds::UNDEFINED) {
}


CFG::Bounds::Bounds(Bounds& b)
  : lowerBound(b.lowerBound), upperBound(b.upperBound) {
}


void CFG::Bounds::setLowerBound(int bound) {
  assert(bound >= UNDEFINED);
  this->lowerBound = bound;
}


int CFG::Bounds::getLowerBound() {
  return this->lowerBound;
}


void CFG::Bounds::setUpperBound(int bound) {
  assert(bound >= UNDEFINED);
  this->upperBound = bound;
}


int CFG::Bounds::getUpperBound() {
  return this->upperBound;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::RegularExpression::RegularExpression(
  std::string* name, std::string* expression)
  : SimpleElement(
    ::CFG::REGULAR_EXPRESSION), name(name),
    expression(expression), bounds(NULL) {
}


CFG::RegularExpression::RegularExpression(RegularExpression& r)
  : SimpleElement(r), name(new std::string(*r.name)), expression(
    new std::string(*r.expression)) {
  if (r.bounds != NULL) {
    this->bounds = new Bounds(*r.bounds);
  } else {
    this->bounds = NULL;
  }
}


CFG::RegularExpression::~RegularExpression() {
}


std::string* CFG::RegularExpression::getName() {
  std::string expressionName = *this->name;
  if (this->bounds != NULL &&
      (this->bounds->getLowerBound() != Bounds::UNDEFINED ||
       this->bounds->getUpperBound() != Bounds::UNDEFINED)) {
    expressionName += "_";
    if (this->bounds->getLowerBound() != Bounds::UNDEFINED) {
      expressionName += str(
        boost::format("%1%") % this->bounds->getLowerBound()) + "_";
    }
    if (this->bounds->getUpperBound() != Bounds::UNDEFINED) {
      expressionName += str(
        boost::format("%1%") % this->bounds->getUpperBound());
    }
  }
  return new std::string(expressionName);
}


std::string* CFG::RegularExpression::getExpression() {
  return this->expression;
}


void CFG::RegularExpression::setBounds(::CFG::Bounds* bounds) {
  this->bounds = bounds;
}


CFG::Bounds* CFG::RegularExpression::getBounds() {
  return this->bounds;
}


CFG::Base* CFG::RegularExpression::clone() {
  return new RegularExpression(*this);
}


bool CFG::RegularExpression::equals(::CFG::Base* obj) {
  if (obj->is(::CFG::REGULAR_EXPRESSION)) {
    RegularExpression* regExpr = dynamic_cast<RegularExpression*> (obj);
    return *this->name == *regExpr->name &&
           *this->expression == *regExpr->expression;
  }
  return false;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::ProductionSequence::ProductionSequence()
  : ComplexElement(PRODUCTION_SEQUENCE) {
}


CFG::ProductionSequence::ProductionSequence(ProductionSequence& s)
  : ComplexElement(s) {
  for (std::vector<Base*>::iterator i = s.sequence.begin();
       i != s.sequence.end(); i++) {
    this->sequence.push_back((*i)->clone());
  }
}


CFG::ProductionSequence::~ProductionSequence() {
}


void CFG::ProductionSequence::append(::CFG::Base* fragment) {
  switch (fragment->getType()) {
    case ::CFG::EPSILON:
      break;
    case ::CFG::BASE_WRAPPER:
    case ::CFG::TERMINAL:
    case ::CFG::NONTERMINAL:
    case ::CFG::REGULAR_EXPRESSION:
    case ::CFG::PRODUCTION_ALTERNATIVE: {
      sequence.push_back(fragment);
      break;
    }
    case ::CFG::SNAPSHOT: {
      Snapshot* snapshot = dynamic_cast<Snapshot*> (fragment);
      sequence.push_back(snapshot->applyChanges());
      break;
    }
    case ::CFG::PRODUCTION_SEQUENCE: {
      ProductionSequence *prod = reinterpret_cast<ProductionSequence*>(
        fragment);
      sequence.insert(
        sequence.end(), prod->sequence.begin(), prod->sequence.end());
      break;
    }
    default: {
      throw LogError(
        "gap-00174: Unhandled CFG fragment type in function implementation\n"
        "CFG::ProductionSequence::append (Base *fragment).");
    }
  }
}


void CFG::ProductionSequence::append(std::vector< ::CFG::Base* > seq) {
  sequence.insert(sequence.end(), seq.begin(), seq.end());
}


CFG::Base* CFG::ProductionSequence::combine(::CFG::Base *prod) {
  // depending on the type of the production, we create new results as follows:
  // terminals, non-terminals and sequences create just a clone of the current
  // instance and append their contents. The cardinality of results is not
  // changed by that operation, meaning one sequence results in exactly one
  // new sequence. On the other hand, production-alternatives create one result
  // per alternative, meaning one alternative node with n alternatives results
  // in a list of productions containing n single elements.
  switch (prod->getType()) {
    case ::CFG::EPSILON:
      return this->clone();
    case ::CFG::BASE_WRAPPER:
    case ::CFG::TERMINAL:
    case ::CFG::NONTERMINAL:
    case ::CFG::SNAPSHOT:
    case ::CFG::REGULAR_EXPRESSION:
    case ::CFG::PRODUCTION_SEQUENCE: {
      ProductionSequence* sequenceCopy = dynamic_cast<ProductionSequence*> (
        this->clone());
      sequenceCopy->append(prod->clone());
      return sequenceCopy;
    }
    case ::CFG::PRODUCTION_ALTERNATIVE: {
      ProductionAlternative* alt = (::CFG::ProductionAlternative*)prod;
      ProductionAlternative* newAlts = new ProductionAlternative();
      // iterate over all alternatives and
      for (std::vector<Base*>::iterator i = alt->alternatives.begin();
           i != alt->alternatives.end(); ++i) {
        ProductionSequence* sequenceCopy = dynamic_cast<ProductionSequence*> (
          this->clone());
        sequenceCopy->append((*i)->clone());
        newAlts->addAlternative(sequenceCopy);
      }
      return newAlts;
    }
    default: {
      throw LogError(
        "gap-00177: Unhandled CFG fragment type in function implementation\n"
        "CFG::ProductionSequence::combine (CFG::Base *prod).");
    }
  }
}


CFG::Base* CFG::ProductionSequence::clone() {
  return new ProductionSequence(*this);
}


int CFG::ProductionSequence::getSize() {
  return sequence.size();
}


CFG::Base* CFG::ProductionSequence::elementAt(int pos) {
  return sequence[pos];
}


bool CFG::ProductionSequence::equals(::CFG::Base* obj) {
  if (obj->is(PRODUCTION_SEQUENCE)) {
    ProductionSequence* seq = dynamic_cast<ProductionSequence*> (obj);
    std::vector<Base*>::iterator j = seq->sequence.begin();
    for (std::vector<Base*>::iterator i = this->sequence.begin();
         i != this->sequence.end(); ++i, ++j) {
      // If the operand's list of elements ends before our
      // own list ends, both sequences can not be the same:
      if (j == seq->sequence.end()) {
        return false;
      }
      // Now we have to check the list details. The two corresponding
      // elements (the elements at the same position of both sequences)
      // must match as well. If not, we take the short way out:
      if (!(*i)->equals(*j)) {
        return false;
      }
    }
    // If our own list is exhausted, but the operand's list
    // still contains elements, both sequences can not be equal.
    // That means, we only return TRUE if the operand's sequence
    // is at end, too:
    if (j == seq->sequence.end()) {
      return false;
    }
    return true;
  }
  return false;
}


CFG::ProductionSequence::iterator CFG::ProductionSequence::begin() {
  return this->sequence.begin();
}


CFG::ProductionSequence::iterator CFG::ProductionSequence::end() {
  return this->sequence.end();
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::ProductionAlternative::ProductionAlternative()
  : ComplexElement(::CFG::PRODUCTION_ALTERNATIVE) {
}


CFG::ProductionAlternative::ProductionAlternative(ProductionAlternative& a)
  : ComplexElement(a) {
  for (std::vector<Base*>::iterator i = a.alternatives.begin();
       i != a.alternatives.end(); i++) {
    this->alternatives.push_back((*i)->clone());
  }
}


CFG::ProductionAlternative::ProductionAlternative(
  std::list< ::CFG::Base* > alts)
  : ComplexElement(::CFG::PRODUCTION_ALTERNATIVE) {
  alternatives.insert(alternatives.end(), alts.begin(), alts.end());
}


CFG::ProductionAlternative::~ProductionAlternative() {
}


void CFG::ProductionAlternative::addAlternative(::CFG::Base* alt) {
  // if the subclass of the alt-parameter is of type
  // mbiguityCFG::ProductionAlternative we just merge the list
  // of alternatives to keep the nesting hirarchy as flat
  // as possible
  if (alt->is(::CFG::PRODUCTION_ALTERNATIVE)) {
    ProductionAlternative *prod = dynamic_cast<ProductionAlternative*> (alt);
    this->alternatives.insert(
      this->alternatives.end(), prod->alternatives.begin(),
      prod->alternatives.end());
  } else {
    alternatives.push_back(alt);
  }
}


CFG::Base* CFG::ProductionAlternative::combine(::CFG::Base* prod) {
  // depending on the production fragment that has been
  // passed as parameter to this method, we either return a
  // copy of this instance, or append the production
  // to the list of alternatives.
  switch (prod->getType()) {
    case ::CFG::EPSILON:
      return this->clone();
    case ::CFG::BASE_WRAPPER:
    case ::CFG::TERMINAL:
    case ::CFG::NONTERMINAL:
    case ::CFG::REGULAR_EXPRESSION:
    case ::CFG::PRODUCTION_SEQUENCE: {
      ProductionAlternative* newAlt = new ProductionAlternative();
      for (std::vector<Base*>::iterator i = this->alternatives.begin();
           i != this->alternatives.end(); ++i) {
        Base* newProd = (*i)->combine(prod->clone());
        newAlt->addAlternative(newProd);
      }
      return newAlt;
    }
    case ::CFG::PRODUCTION_ALTERNATIVE: {
      // just merge the production alternative into this one:
      ProductionAlternative* newAlt = dynamic_cast<ProductionAlternative*> (
        this->clone());
      newAlt->addAlternative(prod);
      return newAlt;
    }
    default:
      throw LogError(
        "gap-00178: Unhandled CFG fragment type in function implementation\n"
        "CFG::ProductionAlternative::combine (CFG::Base* prod).");
  }
}


CFG::Base* CFG::ProductionAlternative::clone() {
  return new ProductionAlternative(*this);
}


/*
std::list< ::CFG::Base* > CFG::ProductionAlternative::getAlternatives() {
  std::list<Base*> results;
  for (std::vector<Base*>::iterator i = alternatives.begin();
       i != alternatives.end(); ++i) {
    results.push_back (*i);
  }
  return results;
}
*/


unsigned int CFG::ProductionAlternative::numberOfAlternatives() {
  return this->alternatives.size();
}


bool CFG::ProductionAlternative::equals(::CFG::Base* obj) {
  if (obj->is(PRODUCTION_ALTERNATIVE)) {
    // ProductionAlternative* alt = dynamic_cast<ProductionAlternative*> (obj);
    // return true;
    throw LogError(
      "gap-00000: CFG::ProductionAlternative::equals (CFG::Base* obj)");
  }
  return false;
}


CFG::ProductionAlternative::iterator CFG::ProductionAlternative::begin() {
  return this->alternatives.begin();
}


CFG::ProductionAlternative::iterator CFG::ProductionAlternative::end() {
  return this->alternatives.end();
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


CFG::GrammarProduction::GrammarProduction(::CFG::NonTerminal *lhs)
  : lhs(lhs) {
}


CFG::GrammarProduction::GrammarProduction(GrammarProduction& p)
  : Attributable(p), lhs(new NonTerminal(*p.lhs)), rhs(
    new ProductionAlternative(*p.rhs)) {
}


CFG::GrammarProduction::~GrammarProduction() {
}


void CFG::GrammarProduction::removeDuplicateAlternatives() {
  // First of all we create a copy of the list of alternatives.
  std::vector<Base*> oldAlternatives;
  for (ProductionAlternative::iterator i = this->rhs->begin();
       i != this->rhs->end(); ++i) {
    oldAlternatives.push_back(*i);
  }

  ProductionAlternative* newAlternatives = new ProductionAlternative();
  // Now we iterate through the list of alternatives in a double nested
  // loop and try to find rules that are equal.
  for (unsigned int i = 0; i < oldAlternatives.size(); i++) {
    bool keepItem = true;
    for (unsigned int j = i + 1; j < oldAlternatives.size(); j++) {
      if (oldAlternatives[i]->equals(oldAlternatives[j])) {
        keepItem = false;
      }
    }
    if (keepItem) {
      newAlternatives->addAlternative(oldAlternatives[i]);
    }
  }

  this->rhs = newAlternatives;

  // TODO(who?): remove all old alternatives here, because they are unused.
}


CFG::GrammarProduction* CFG::GrammarProduction::clone() {
  return new GrammarProduction(*this);
}
