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

#include "rewrite_non_productive_cfg_rules.hh"

#include <string>
#include <cassert>
#include <list>

#include "../log.hh"
#include "../util/annotate_cycles.hh"
#include "../util/annotate_the_set_first.hh"
#include "../util/cycle_set_utils.hh"


SpecializeGrammar::RewriteNonProductiveCFGRules::RewriteNonProductiveCFGRules()
  : oldGrammar(NULL),
    newGrammar(NULL) {
}


SpecializeGrammar::RewriteNonProductiveCFGRules::
  ~RewriteNonProductiveCFGRules() {
}


CFG::CFG* SpecializeGrammar::RewriteNonProductiveCFGRules::rewriteGrammar(
  CFG::CFG* grammar) {
  this->oldGrammar = grammar;
  this->newGrammar = new CFG::CFG();

  // rewrite the grammar, start with the axiom
  rewriteProductions();

  // At the end set the axiom
  std::string* axiomName = this->oldGrammar->getAxiom()->getName();
  CFG::NonTerminal* newAxiom = new CFG::NonTerminal(axiomName);
  this->newGrammar->setAxiom(newAxiom);

  return this->newGrammar;
}


void SpecializeGrammar::RewriteNonProductiveCFGRules::rewriteProductions() {
  std::list<CFG::GrammarProduction*> productions =
    this->oldGrammar->getProductions();
  for (std::list<CFG::GrammarProduction*>::iterator i = productions.begin();
       i != productions.end(); i++) {
    rewriteProduction(*i);
  }
}


void SpecializeGrammar::RewriteNonProductiveCFGRules::rewriteProduction(
  CFG::GrammarProduction* production) {
  // First split off all
  CFG::GrammarProduction* epsilonProduction = rewriteProductionWithEpsilon(
    production);
  // ...then create an embracing rule
  CFG::GrammarProduction* noEpsilonProduction =
    rewriteProductionWithoutEpsilon(production);

  // Fill the production alternative with one of each or both.
  CFG::ProductionAlternative* newProductionAlternative =
    new CFG::ProductionAlternative();
  if (epsilonProduction != NULL) {
    CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (
      epsilonProduction->lhs->clone());
    newProductionAlternative->addAlternative(nonTerminal);
    this->newGrammar->addProduction(epsilonProduction);
  }
  if (noEpsilonProduction != NULL) {
    CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (
      noEpsilonProduction->lhs->clone());
    newProductionAlternative->addAlternative(nonTerminal);
    this->newGrammar->addProduction(noEpsilonProduction);
  }

  CFG::NonTerminal* lhs = dynamic_cast<CFG::NonTerminal*> (
    production->lhs->clone());
  CFG::GrammarProduction* newProduction = new CFG::GrammarProduction(lhs);
  newProduction->rhs = newProductionAlternative;
  this->newGrammar->addProduction(newProduction);
}


CFG::GrammarProduction* SpecializeGrammar::RewriteNonProductiveCFGRules::
  rewriteProductionWithoutEpsilon(CFG::GrammarProduction* production) {
  CFG::Base* rhs = rewriteBaseWithoutEpsilon(production->rhs);
  if (rhs != NULL) {
    CFG::NonTerminal* lhs = new CFG::NonTerminal(new std::string(
      *production->lhs->getName() + "_no_eps"));
    CFG::GrammarProduction* newProduction = new CFG::GrammarProduction(lhs);
    newProduction->rhs = dynamic_cast<CFG::ProductionAlternative*>(rhs);
    return newProduction;
  }
  return NULL;
}


CFG::Base* SpecializeGrammar::RewriteNonProductiveCFGRules::
  rewriteBaseWithoutEpsilon(CFG::Base* b) {
  switch (b->getType()) {
    case CFG::EPSILON: {
      return NULL;
    }
    case CFG::TERMINAL:
    case CFG::REGULAR_EXPRESSION: {
      return b->clone();
    }
    case CFG::BASE_WRAPPER: {
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
      CFG::Base* result = rewriteBaseWithEpsilon(wrapper->getWrappedBase());

      if (result != NULL) {
        result = new CFG::BaseWrapper(result);
      }

      return result;
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (b);
      if (!Util::CycleSetUtils::elementIsNullable(nonTerminal)) {
        return nonTerminal->clone();
      } else {
        // If the non-terminal can derive epsilon, we want only
        // that part of the non-terminal, which cannot derive epsilon,
        // hence we use the "_no_eps" suffix.
        if (Util::CycleSetUtils::elementIsProductive(nonTerminal)) {
          std::string* nonTerminalWithoutEpsilonName = new std::string(
            *nonTerminal->getName() + "_no_eps");
          CFG::NonTerminal* newNonTerminal = new CFG::NonTerminal(
            nonTerminalWithoutEpsilonName);
          copyAttributes(nonTerminal, newNonTerminal);
          return newNonTerminal;
        } else {
          return NULL;
        }
      }
    }
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* sequence =
        dynamic_cast<CFG::ProductionSequence*> (b);

      // If there are no elements in the input sequence, we
      // just return a clone of the original.
      if (sequence->getSize() == 0) {
        return sequence->clone();
      }

      int totalElementCount = 0;
      int nullableOnlyElementCount = 0;
      int elementsPartOfCycleCount = 0;
      bool allElementsAreNonTerminals = true;
      std::list<CFG::Base*> resultElements;
      std::list<int> positionOfCycleElements;
      for (CFG::ProductionSequence::iterator i = sequence->begin();
           i != sequence->end(); i++) {
        CFG::Base* result = rewriteBaseWithoutEpsilon(*i);
        // Push it all, even the NULLs. We only know what to do
        // with it after we got them all.
        resultElements.push_back(result);
        // Some statistics about the sequence itself...
        if ((*i)->getType() != CFG::NONTERMINAL) {
          allElementsAreNonTerminals = false;
        }
        if (Util::CycleSetUtils::elementIsNullableOnly(*i)) {
          nullableOnlyElementCount++;
        }
        if (Util::CycleSetUtils::elementIsPartOfCycle(*i)) {
          elementsPartOfCycleCount++;
          // The current position in the sequence equals
          // the total element count.
          positionOfCycleElements.push_back(totalElementCount);
        }
        totalElementCount++;
      }

      if (totalElementCount == nullableOnlyElementCount) {
        // If all elements derive only epsilon and nothing more,
        // we return nothing, since this is not a valid result
        // containing only productions which can not derive epsilon.
        return NULL;
      } else if (allElementsAreNonTerminals) {
        // Only for those sequences which contain elements that
        // are part of a cycle, we do this extra work. The second
        // condition that must be met is that all elements that
        // are not part of any cycle, are nullable.
        if (elementsPartOfCycleCount > 0 && nullableOnlyElementCount == 0) {
          // We are about to create a mass of alternatives out of
          // this single sequence. All alternatives will be stored
          // in the newAlternative CFG node, and returned at
          // the end of this block.
          CFG::ProductionAlternative* newAlternative =
            new CFG::ProductionAlternative();

          for (std::list<int>::iterator p = positionOfCycleElements.begin();
               p != positionOfCycleElements.end(); p++) {
            int cycleElementPos = *p;
            // Each position must be checked again for their
            // computability into a separated representation.
            // Check out of all elements except the cycle element
            // are nullable. Without this property we can not
            // transform this element into its special representation.
            int nullableElementsCount = 0;
            int productiveElementsCount = 0;
            int l = 0;
            for (CFG::ProductionSequence::iterator i = sequence->begin();
                 i != sequence->end(); i++, l++) {
              // Perform two different checks, depending on this
              // element being the cycle element, or one of the rest.
              if (l == cycleElementPos) {
                // The cycle element must have the property
                // of being productive
                if (!Util::CycleSetUtils::elementIsProductive(*i)) {
                  // is there any requirement imposed on this
                  // element??
                }
              } else {
                if (Util::CycleSetUtils::elementIsNullable(*i)) {
                  nullableElementsCount++;
                }
                if (Util::CycleSetUtils::elementIsProductive(*i)) {
                  productiveElementsCount++;
                }
              }
            }

            // We need all other elements nullable, otherwise we can not
            // create a production which contains the cycle element with
            // all other elements deriving only epsilon.
            if (totalElementCount - nullableElementsCount != 1) {
              continue;
            }

            if (totalElementCount - productiveElementsCount != 1) {
              continue;
            }

            // Otherwise create a whole bunch of sequences, exponential
            // in the total number of elements of the original sequence.
            int numberOfAlternativesToGenerate = 1 << (totalElementCount - 1);
            for (int genCount = 0;
                 genCount < numberOfAlternativesToGenerate; genCount++) {
              CFG::ProductionSequence* newSequence =
                new CFG::ProductionSequence();

              int code = genCount;
              l = 0;
              for (CFG::ProductionSequence::iterator i = sequence->begin();
                   i != sequence->end(); i++, l++) {
                assert((*i)->getType() == CFG::NONTERMINAL);
                CFG::NonTerminal* sequenceElement =
                  dynamic_cast<CFG::NonTerminal*> (*i);
                if (l == cycleElementPos) {
                  CFG::NonTerminal* newNonTerminal = NULL;
                  if (!Util::CycleSetUtils::elementIsNullable(
                    sequenceElement)) {
                    newNonTerminal = dynamic_cast<CFG::NonTerminal*> (
                      sequenceElement->clone());
                  } else {
                    newNonTerminal = new CFG::NonTerminal(new std::string(
                      *sequenceElement->getName() + "_no_eps"));
                  }
                  copyAttributes(sequenceElement, newNonTerminal);
                  newSequence->append(newNonTerminal);
                } else {
                  // Divisible by two means the pure epsilon rule,
                  // otherwise we use the pure-non-epsilon rule.
                  std::string* newNonTerminalName = NULL;
                  if (code % 2 == 0) {
                    newNonTerminalName = new std::string(
                      *sequenceElement->getName() + "_eps");
                  } else {
                    newNonTerminalName = new std::string(
                      *sequenceElement->getName() + "_no_eps");
                  }
                  newSequence->append(new CFG::NonTerminal(
                    newNonTerminalName));


                  // Shift the code by one bit.
                  code = code/2;
                }
              }

              newAlternative->addAlternative(newSequence);
            }
          }

          return newAlternative;
        }
      }


      // As a fall back, just clone the original, because the
      // above tricks apply only if all elements of the list
      // are non-terminals, and exactly one element is part of
      // a cycle.
      return sequence->clone();
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      CFG::ProductionAlternative* alternative =
        dynamic_cast<CFG::ProductionAlternative*> (b);
      CFG::ProductionAlternative* newAlternative =
        new CFG::ProductionAlternative();


      for (CFG::ProductionAlternative::iterator i = alternative->begin();
           i != alternative->end(); i++) {
        CFG::Base* result = rewriteBaseWithoutEpsilon(*i);
        if (result != NULL) {
          newAlternative->addAlternative(result);
        }
      }

      // Only if any alternative is left, we return an
      // cfg-alternative-element.
      if (newAlternative->numberOfAlternatives() == 0) {
        return NULL;
      }

      return newAlternative;
    }
    default: {
      throw LogError("gap-00725: Unsupported CFG node type in level 1.");
    }
  }
}


CFG::GrammarProduction* SpecializeGrammar::RewriteNonProductiveCFGRules::
rewriteProductionWithEpsilon(CFG::GrammarProduction* production) {
  CFG::Base* rhs = rewriteBaseWithEpsilon(production->rhs);
  if (rhs != NULL) {
    CFG::NonTerminal* lhs = new CFG::NonTerminal(new std::string(
      *production->lhs->getName() + "_eps"));
    lhs->setAttribute(new EpsilonOnlyAttribute());
    CFG::GrammarProduction* newProduction = new CFG::GrammarProduction(lhs);
    newProduction->rhs = dynamic_cast<CFG::ProductionAlternative*>(rhs);
    this->newGrammar->addProduction(newProduction);
    newProduction->setAttribute(new EpsilonOnlyAttribute());
    return newProduction;
  }
  return NULL;
}


CFG::Base* SpecializeGrammar::RewriteNonProductiveCFGRules::
  rewriteBaseWithEpsilon(CFG::Base* b) {
  switch (b->getType()) {
    case CFG::EPSILON: {
      b->setAttribute(new EpsilonOnlyAttribute());
      return b->clone();
    }
    case CFG::TERMINAL:
    case CFG::REGULAR_EXPRESSION: {
      return NULL;
    }
    case CFG::BASE_WRAPPER: {
      CFG::BaseWrapper* wrapper = dynamic_cast<CFG::BaseWrapper*> (b);
      CFG::Base* result = rewriteBaseWithEpsilon(wrapper->getWrappedBase());

      if (result != NULL) {
        result = new CFG::BaseWrapper(result);
      }

      return result;
    }
    case CFG::NONTERMINAL: {
      CFG::NonTerminal* nonTerminal = dynamic_cast<CFG::NonTerminal*> (b);
      if (Util::CycleSetUtils::elementIsNullable(nonTerminal)) {
        std::string* nonTerminalWithoutEpsilonName = new std::string(
          *nonTerminal->getName() + "_eps");
        CFG::NonTerminal* newNonTerminal = new CFG::NonTerminal(
          nonTerminalWithoutEpsilonName);
        newNonTerminal->setAttribute(new EpsilonOnlyAttribute());
        copyAttributes(nonTerminal, newNonTerminal);
        return newNonTerminal;
      } else {
        return NULL;
      }
    }
    case CFG::PRODUCTION_SEQUENCE: {
      CFG::ProductionSequence* sequence =
        dynamic_cast<CFG::ProductionSequence*> (b);

      // If the sequence does not contain anything, we assume
      // nothing, not even that this sequence can produce epsilon.
      if (sequence->getSize() == 0) {
        return NULL;
      }

      CFG::ProductionSequence* newSequence = new CFG::ProductionSequence();
      copyAttributes(sequence, newSequence);
      bool sequenceContainsInvalidElements = false;
      for (CFG::ProductionSequence::iterator i = sequence->begin();
           i != sequence->end(); i++) {
        CFG::Base* result = rewriteBaseWithEpsilon(*i);
        if (result == NULL) {
          // There are invalid elements in this sequence which
          // make it not completely nullable.
          sequenceContainsInvalidElements = true;
          break;
        }
        newSequence->append(result);
      }

      // If the transformation revealed at least one element
      // that was not completely nullable, we can not return
      // the sequence, not even a part of it. Just NULL.
      if (sequenceContainsInvalidElements) {
        return NULL;
      }


      // The whole lot consists of epsilons or epsilon-only
      // deriving elements. Mark this with a special attribute.
      newSequence->setAttribute(new EpsilonOnlyAttribute());

      return newSequence;
    }
    case CFG::PRODUCTION_ALTERNATIVE: {
      CFG::ProductionAlternative* alternative =
        dynamic_cast<CFG::ProductionAlternative*> (b);
      CFG::ProductionAlternative* newAlternative =
        new CFG::ProductionAlternative();
      copyAttributes(alternative, newAlternative);

      for (CFG::ProductionAlternative::iterator i = alternative->begin();
           i != alternative->end(); i++) {
        CFG::Base* result = rewriteBaseWithEpsilon(*i);
        if (result != NULL) {
          newAlternative->addAlternative(result);
        }
      }

      // Only if any alternative is left, we return an
      // cfg-alternative-element.
      if (newAlternative->numberOfAlternatives() == 0) {
        return NULL;
      }

      // The whole lot consists of epsilons or epsilon-only
      // deriving elements. Mark this with a special attribute.
      newAlternative->setAttribute(new EpsilonOnlyAttribute());


      return newAlternative;
    }
    default: {
      throw LogError("gap-00726: Unsupported CFG node type in level 1.");
    }
  }
}


void SpecializeGrammar::RewriteNonProductiveCFGRules::copyAttributes(
  CFG::Base* source, CFG::Base* destination) {
  for (Util::Attributable::iterator i = source->begin();
       i != source->end(); i++) {
    destination->setAttribute((*i).second);
  }
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


SpecializeGrammar::EpsilonOnlyAttribute::EpsilonOnlyAttribute()
  : Util::Attribute("SpecializeGrammar::EpsilonOnlyAttribute") {
}


SpecializeGrammar::EpsilonOnlyAttribute::EpsilonOnlyAttribute(
  EpsilonOnlyAttribute& a)
  : Util::Attribute("SpecializeGrammar::EpsilonOnlyAttribute") {
}


SpecializeGrammar::EpsilonOnlyAttribute::~EpsilonOnlyAttribute() {
}


Util::Attribute* SpecializeGrammar::EpsilonOnlyAttribute::clone() {
  return new EpsilonOnlyAttribute(*this);
}
