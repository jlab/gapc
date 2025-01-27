/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2011-2023  Stefan Janssen
         email: stefan.m.janssen@gmail.com or stefan.janssen@computational.bio.uni-giessen.de

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

#include <utility>
#include <vector>
#include <set>
#include <list>
#include <string>
#include "grammar_transformation.hh"
#include "../expr/new.hh"

bool Grammar::check_outside_parse_empty_word() {
  if (this->ast.outside_generation()) {
    for (std::vector<Yield::Size>::const_iterator i =
         this->axiom->multi_ys().begin();
         i != this->axiom->multi_ys().end(); ++i) {
      if ((*i).low() > 0) {
        std::ostringstream msg;
        msg << "The minimal yield size of your grammar '" << *this->name
            << "' is ";
        (*i).low().put(msg);
        msg << ", i.e. it cannot parse the empty input string ''."
            << " For outside grammar generation, this means you are lacking a"
            << " recursion basis which will result in empty results for "
            << "ALL outside candidates! Try adding an alternative like "
            << "nil(EMPTY) to your axiom.";
        Log::instance()->warning(this->location, msg.str());
        return false;
      }
    }
  }
  return true;
}

void Grammar::check_outside_requested_nonexisting_nts() {
  /* double check that all NTs do exist that the user requested to
   * be reported in the outside grammar.
   */

  if (!this->ast.get_outside_nt_list()) {
    // the user did not request any outside NT to be reported
    return;
  }
  assert(this->ast.get_outside_nt_list());

  // a list that collects the names of non existent NTs
  std::list<std::string> *warn_missing_nts = new std::list<std::string>();

  // iterate through NTs which the user requested to be reported
  for (std::vector<std::string>::const_iterator i =
       this->ast.get_outside_nt_list()->begin();
       i != this->ast.get_outside_nt_list()->end(); ++i) {
    // ignore the special user input "ALL", since this is per definition not
    // a non-terminal in the grammar
    if ((*i).compare(OUTSIDE_ALL) == 0) {
      continue;
    }

    // next, check if NT with user given name is present in grammar
    if (this->NTs.find(*i) == this->NTs.end()) {
      // if not, add this NT name to the list
      warn_missing_nts->push_back(*i);
    }
  }

  if (warn_missing_nts->size() > 0) {
    std::string msg = std::string(
      "You requested outside grammar generation and\nreporting results for "
      "the following non-terminals, which do NOT exist in your grammar '" +
      *this->name + "':\n");
    for (std::list<std::string>::iterator i = warn_missing_nts->begin();
         i != warn_missing_nts->end(); ++i) {
      msg += "  '" + *i + "'";
      if (next(i) != warn_missing_nts->end()) {
        msg += "\n";
      }
    }
    throw LogError(this->location, msg);
  }

  delete warn_missing_nts;
}

// traverses the grammar and collect all algebra function names used
// such that after traversal, we can ask if an algebra function,
// given its name, is actually part of the grammar
struct AlgfctUsedInGrammar : public Visitor {
 private:
  std::set<std::string> used_algfct;
  bool is_traversed = false;

 public:
  void visit(Alt::Base &a) {
    Alt::Simple *as = dynamic_cast<Alt::Simple*>(&a);
    if (as) {
      used_algfct.insert(*as->name);
    }
    is_traversed = true;
  }

  bool is_used(std::string *algfct_name) {
    assert(is_traversed);
    return used_algfct.find(*algfct_name) != used_algfct.end();
  }
};

bool is_terminal_type(Type::Base *t) {
  if ((t->is(Type::ALPHABET)) || (t->is(Type::VOID)) ||
      (t->is(Type::REALVOID)) || (t->is(Type::CHAR)) ||
      (t->is(Type::STRING)) || (t->is(Type::BOOL)) || (t->is(Type::SEQ)) ||
      (t->is(Type::SUBSEQ)) || (t->is(Type::INT)) || (t->is(Type::FLOAT)) ||
      (t->is(Type::RATIONAL))) {
    return true;
  }
  if ((t->is(Type::BIGINT)) || (t->is(Type::SHAPE)) || (t->is(Type::INTEGER))) {
    return false;
  }
  if ((t->is(Type::EXTERNAL)) || (t->is(Type::TUPLEDEF))) {
    return false;
  }
  if (t->is(Type::USAGE)) {
    return (t->is_terminal()) && (is_terminal_type(t->simple()));
  }
  if (t->is(Type::MULTI)) {
    Type::Multi *tm = dynamic_cast<Type::Multi*>(t);
    for (std::list<Type::Base*>::const_iterator i = tm->types().begin();
         i != tm->types().end(); ++i) {
      if (!is_terminal_type(*i)) {
        return false;
      }
    }
    return true;
  }
  if (t->is(Type::SINGLE)) {
    return t->is_terminal();
  }
  if (t->is(Type::REFERENCABLE)) {  // a pointer to an object
    return (dynamic_cast<Type::Referencable*>(t)->base->is_terminal());
  }
  if (t->is(Type::SIGNATURE) ||  // a basic ADP component
      t->is(Type::TABLE) ||  // a DP matrix to store non-terminal results
      t->is(Type::LIST) ||  // the answer list of an algebra function
      t->is(Type::TUPLE) ||  // a pair of results
      t->is(Type::CHOICE) ||  // flag an algebra function as a choice function
      t->is(Type::RANGE) ||  // codegen, set of begin/end iterators
      t->is(Type::SIZE)  // internal type for code generation = unsigned int
      ) {
    return false;
  }
  if ((dynamic_cast<Type::Def*>(t)) ||  // definition of a new user type
      (dynamic_cast<Type::Table*>(t)) ||  // a DP matrix, see Type::TABLE
      (dynamic_cast<Type::Generic*>(t))  // base class with a name
     ) {
    return false;
  }
  if (dynamic_cast<Type::Base*>(t)) {
    // last resort, check if the base class is flagged a being a terminal
    return t->is_terminal();
  }

  throw LogError(t->location, "Unknown Type, thus I cannot determine if it is "
                              "a terminal type or not. Please extend function "
                              "is_terminal_type in src/outside/grammar_transfo"
                              "rmation.cc of gapc sources!");
  return false;
}

bool Instance::check_multiple_answer_types(bool for_outside_generation) {
  if (!for_outside_generation) {
    // no need to check, if no outside transformation was requested
    return true;
  }

  AlgfctUsedInGrammar v = AlgfctUsedInGrammar();
  this->grammar_->traverse(v);

  // identify individual algebras used in the algebra product of the instance
  unsigned int num_errors = 0;
  for (Product::iterator i = Product::begin(this->product); i != Product::end();
       ++i) {
    if ((*i)->is(Product::SINGLE)) {
      Algebra *alg = dynamic_cast<Product::Single*>(*i)->algebra();
      for (hashtable<std::string, Fn_Def*>::const_iterator i = alg->fns.begin();
           i != alg->fns.end(); ++i) {
        Fn_Decl *algfct = (*i).second;

        // do not check choice functions
        if (algfct->is_Choice_Fn()) {
          continue;
        }

        // ignore algebra function if not used in instance' grammar, i.e.
        // it might be declared in signature and algebra(s) but not used
        // in grammar definition
        if (!v.is_used(algfct->name)) {
          continue;
        }

        // only check algebra functions whose return type is NOT a terminal
        // (type)
        if (!is_terminal_type(algfct->return_type)) {
          for (std::list<Type::Base*>::const_iterator t = algfct->types.begin();
               t != algfct->types.end(); ++t) {
            // only check rhs components that are not terminal (types)
            if (!is_terminal_type(*t)) {
              // check if return type is NOT equal to non-terminal types on the
              // rhs
              if (!algfct->return_type->simple()->is_eq(*(*t)->simple())) {
                std::ostringstream msg;
                msg << "return type '"
                    << *algfct->return_type
                    << "' is different to the type '"
                    << *(*t)
                    << "',\nwhich you are using on the r.h.s. of the function "
                    << "definition '"
                    << *(algfct->name)
                    << "' in algebra '"
                    << *(alg->name)
                    << "'.\nThis will lead to a compile error, since"
                    << " you requested outside grammar generation.\nThe outside"
                    << " grammar parts will contain production rules where "
                    << "l.h.s. and r.h.s. non-termials of '" + (*(algfct->name))
                    << + "' are swapped,\nbut we lack definitions for these "
                    << "swapped versions in your algebras!";
                Log::instance()->error(alg->location, "type mismatch");
                Log::instance()->error((*t)->location, msg.str());
                num_errors++;

                // one warning per algebra function should be enough
                break;
              }
            }
          }
        }
      }
    }
  }

  return num_errors == 0;
}

void Grammar::check_overlays_exists() {
  struct FindOverlay : public Visitor {
    std::vector<Loc> overlay_locations;

    void visit_begin(Alt::Simple &alt) {
      if (alt.has_index_overlay() && !alt.is_partof_outside()) {
        overlay_locations.push_back(alt.location);
      }
    }
  };

  if (this->ast.outside_generation() && (!this->is_partof_outside())) {
    FindOverlay v = FindOverlay();
    this->traverse(v);
    if (v.overlay_locations.size() > 0) {
      for (std::vector<Loc>::const_iterator i = v.overlay_locations.begin();
           i != v.overlay_locations.end(); ++i) {
        Log::instance()->warning(*i,
            "You requested outside grammar generation, but your inside grammar "
            "contains manual index overlays. As these cannot be automatically "
            "converted from an inside to an outside fashion, the resulting "
            "program most likely produces wrong results!");
      }
    }
  }
}

/* iterates through one lhs NT and reports the first occurrence of an
 * Alt::Block, i.e.
 * - hold a pointer to the Alt::Block,
 * - hold a pointer to the top level Alt::Base on the rhs of the NT that holds
 *   the Alt::Block
 * - and either
 *   + a pointer to the Symbol::NT, if the Block is on the top level rhs
 *   + or a pointer to the Alt::Base which is the parent of the Alt::Block
 *     together with the a pointer to the "Handle" (= Fn_Arg::Alt) enclosing
 *     the Alt::Block */
struct FindFirstBlock : public Visitor {
  // pointer to the first found block
  Alt::Block *block = nullptr;

  // pointer to the Fn_Arg::Alt that encloses the first found block - iff it's
  // parent is an Alt::Base
  Fn_Arg::Alt *block_fnarg = nullptr;

  // the top level alternative that contains (somewhere) the first found block
  Alt::Base *topalt = nullptr;

  // the direct Alt::Base parent of the first found block - iff it is not a
  // Symbol::NT
  Alt::Base *parent_alt = nullptr;

  // the direct Symbol::NT parent of the first found block - iff it is not an
  // Alt::Block
  Symbol::NT *parent_nt = nullptr;

  FindFirstBlock() : block(nullptr), block_fnarg(nullptr), parent_alt(nullptr),
                     parent_nt(nullptr) {
  }

  void visit(Symbol::NT &nt) {
    if (!block) {
      parent_alt = nullptr;
      block_fnarg = nullptr;
      parent_nt = &nt;
    }
  }
  void visit_itr(Symbol::NT &nt) {
    if (!block) {
      parent_alt = nullptr;
      block_fnarg = nullptr;
      parent_nt = &nt;
    }
  }

  void visit_begin(Alt::Simple &alt) {
    if (!block) {
      parent_alt = &alt;
      parent_nt = nullptr;
      if (alt.top_level) {
        topalt = &alt;
      }
    }
  }
  void visit(Alt::Link &alt) {
    // can only point to a rhs non-terminal
  }
  void visit_begin(Alt::Block &alt) {
    if ((!block) && (alt.alts.size() > 0)) {
      block = &alt;
      if (alt.top_level) {
        topalt = &alt;
      }
    }
  }
  void visit(Alt::Multi &alt) {
    if (!block) {
      parent_alt = &alt;
      parent_nt = nullptr;
      if (alt.top_level) {
        topalt = &alt;
      }
    }
  }

  void visit(Fn_Arg::Alt &arg) {
    if (!block) {
      block_fnarg = &arg;
    }
  }

  void visit(Grammar &g) {
    throw LogError(
      "Please only apply at individual NTs, not the full grammar!");
  }
};

void resolve_blocks(Symbol::NT *nt) {
  if (nt) {
    // check if there is any Alt::Block at the rhs of the NT
    FindFirstBlock v_block = FindFirstBlock();
    nt->traverse(v_block);

    // iterate through all alternatives until no more Alt::Block can be found
    while (v_block.block) {
      // determine the top level alternative in rhs of NT that holds the
      // Alt::Block
      std::list<Alt::Base*>::iterator topalt = nt->alts.begin();
      for (; topalt != nt->alts.end(); ++topalt) {
        if ((*topalt) == v_block.topalt) {
          break;
        }
      }

      // Alt::Block can either occur within an algebra function like
      // struct = cadd(foo, {joe, user})
      if (v_block.parent_alt && !v_block.parent_nt) {
        if (v_block.parent_alt->is(Alt::SIMPLE)) {
          // parent of the block is an Alt::Simple, i.e. has a list of children
          for (std::list<Alt::Base*>::iterator child =
               v_block.block->alts.begin();
               child != v_block.block->alts.end(); ++child) {
            // create a clone of the full alternative (up to the top level) that
            // contains this block. This will invalidate all pointer information
            // we have for the block ...
            Alt::Base *clone = (*v_block.topalt).clone();

            // ... thus acquire these info again, but for the clone, which is
            // not yet part of any non-terminal
            FindFirstBlock v_clone = FindFirstBlock();
            clone->traverse(v_clone);

            // now replace the block in the clone with the child of the original
            // block
            v_clone.block_fnarg->alt = *child;

            // carry over filters that are attached to the block, from the block
            // to the child in the clone
            v_clone.block_fnarg->alt->filters.insert(
              v_clone.block_fnarg->alt->filters.end(),
              v_block.block->filters.begin(),
              v_block.block->filters.end());
            v_clone.block_fnarg->alt->multi_filter.insert(
              v_clone.block_fnarg->alt->multi_filter.end(),
              v_block.block->multi_filter.begin(),
              v_block.block->multi_filter.end());

            // insert new (partially, since it can still hold further Blocks)
            // alternative into rhs of the NT
            nt->alts.insert(topalt, clone);
          }
          // remove original top-alternative, which holds the found Alt::Block
          nt->alts.remove(v_block.topalt);
        } else if (v_block.parent_alt->is(Alt::LINK)) {
          throw LogError("a Link is a leaf and thus cannot contain a block!");
        } else if (v_block.parent_alt->is(Alt::BLOCK)) {
          throw LogError("parent block should have been removed already!");
        } else if (v_block.parent_alt->is(Alt::MULTI)) {
          throw LogError("Alternative is not allowed in Multi-Track link.");
        } else {
          throw LogError("this is an unknown Alt subclass");
        }

      // or directly as a top level alternative of the non-termial,
      // like struct = {joe, user}
      } else if (!v_block.parent_alt && v_block.parent_nt) {
        for (std::list<Alt::Base*>::iterator child =
             v_block.block->alts.begin();
             child != v_block.block->alts.end(); ++child) {
          Alt::Base *clone = (*child)->clone();

          // since parent is lhs non-terminal and block itself will be removed,
          // children will become top level alternatives
          clone->top_level = Bool(true);

           // don't forget to carry over filters ...
          clone->filters.insert(clone->filters.end(),
              v_block.block->filters.begin(),
              v_block.block->filters.end());

          // ... and filters for multitrack
          clone->multi_filter.insert(clone->multi_filter.end(),
              v_block.block->multi_filter.begin(),
              v_block.block->multi_filter.end());

          // insert new (partially, since it can still hold further Blocks)
          // alternative into rhs of the NT
          nt->alts.insert(topalt, clone);
        }

        nt->alts.remove(*topalt);
      } else {
        throw LogError("each Alt::Block should have a parent!");
      }

      // check if there exist further Alt::Blocks, if not, we exit the while
      // loop
      v_block = FindFirstBlock();
      nt->traverse(v_block);
    }
  }
}

/* Counts the number of components a rule will inject into parse-trees, e.g.
 *   hairpin = hl(BASE, REGION, BASE) : increase by 1 due to hl
 *   plus = CHAR('+') : no increase since only terminal used on rhs
 *   weak = hairpin | bulgeL : increase by 2 due to 2 non-terminals
 * At least one component is necessary to subject the production rule to
 * outside generation and/or outside axiom determination
 */
struct Count_parseComponents : public Visitor {
  unsigned int number_components = 0;

  Count_parseComponents() {
    number_components = 0;
  }

  void visit_begin(Alt::Simple &alt) {
    if (!alt.is_terminal()) {
      number_components++;
    }
  }
  void visit(Alt::Link &alt) {
    if (alt.nt->is(Symbol::NONTERMINAL)) {
      number_components++;
    }
  }
};

/* let all Alt::Simple grammar elements know which left hand NT called this
 * alternative. Necessary to construct proper outside guards, which need to know
 * lhs NT table dimensions.
 */
struct Propagate_lhsNT : public Visitor {
  Symbol::NT *lhsNT;

  explicit Propagate_lhsNT(Symbol::NT *lhsNT) : lhsNT(lhsNT) {
  }

  void visit_begin(Alt::Simple &alt) {
    alt.outside_lhsNT = this->lhsNT;
  }
};


/* iterates through the rhs alternatives of an NT and creates a clone of an
 * alternative where ONE (but not all) rhs NT is swapped with the lhs NT, e.g.
 *   struct = cadd(dangle, weak) | sadd(BASE, struct) will result in
 * a) outside_dangle = cadd(outside_struct, weak)
 * b) outside_weak = cadd(dangle, outside_struct)
 * c) outside_struct = sadd(BASE, outside_struct) */
struct Flip_lhs_rhs_nonterminals : public Visitor {
  /* a list to store all newly generated clone alternatives. Each entry is a
   * pair to save the new lhs non-terminal together with the modified rhs
   * alternative */
  std::list<std::pair<Symbol::NT*, Alt::Base*> > *alt_clones;

  // a clone of the original inside lhs NT
  Symbol::NT *lhs_nt;

  // the rhs top level alternative
  Alt::Base *topalt = nullptr;

  explicit Flip_lhs_rhs_nonterminals(Symbol::NT *nt) {
    // initialize the for now empty list of flipped alternative production rules
    alt_clones = new std::list<std::pair<Symbol::NT*, Alt::Base*> >();

    // clone the given inside lhs NT
    lhs_nt = nt->clone(nt->track_pos(), true);
    // and prefix it's name with "outside_"
    lhs_nt->name = new std::string(OUTSIDE_NT_PREFIX + *(nt->name));
    lhs_nt->orig_name = lhs_nt->name;
    // remove all alternatives
    lhs_nt->alts.clear();
  }
  ~Flip_lhs_rhs_nonterminals() {
    delete alt_clones;
    // delete lhs_nt->name; --> leads to failing mod_test_outside?!
    delete lhs_nt;
  }
  void visit(Alt::Base &alt) {
    if (alt.top_level) {
      // record the current top level alternative. Starting point for cloning
      topalt = &alt;
    }
  }
  void visit(Alt::Link &alt) {
    // skip links to terminal parser
    if (alt.nt->is(Symbol::NONTERMINAL)) {
      /* grammar might contain "alias" non-terminals that only point to
       * terminals like:
       *   plus = CHAR('+')
       * if the link points to such an "alias" non-terminal, we need to treat it
       * as if it is a terminal, i.e. skip it for producing outside alternatives
       * We therefore here count the number of used non-terminal and algebra
       * functions in the linked non-terminal
       */
      Count_parseComponents nrNTs = Count_parseComponents();
      alt.nt->traverse(nrNTs);
      if (nrNTs.number_components > 0) {
        /* a bit hacky: we need to create exact copies of the inside alternative
         * production rule, but if we clone all components will have different
         * pointers as they are different objects. Thus, we
         *   a) safely store the original rhs NT (orig_rhs_nt) away
         *      + non-terminal parameters
         *   b) create a second clone of the rhs NT, but prefix its name with
         *      "outside_" and remove all alternatives
         *   c) next, we overwrite the current rhs NT of the Alt::Link with the
         *      lhs NT (which was already prefixed with "outside_")
         *   d) NOW clone the modified production rule
         *   e) restore the state before cloning of the inside production rule
         *      + non-terminal parameters
         */

        // a)
        Symbol::NT *orig_rhs_nt = dynamic_cast<Symbol::NT*>(alt.nt)->clone(
            dynamic_cast<Symbol::NT*>(alt.nt)->track_pos(), true);
        std::list<Expr::Base*> orig_alt_ntparas = alt.get_ntparas();

        // b)
        Symbol::NT *outside_rhs_nt = dynamic_cast<Symbol::NT*>(alt.nt)->clone(
            dynamic_cast<Symbol::NT*>(alt.nt)->track_pos(), true);
        outside_rhs_nt->name = new std::string(
            OUTSIDE_NT_PREFIX + *(outside_rhs_nt->name));
        outside_rhs_nt->orig_name = outside_rhs_nt->name;
        outside_rhs_nt->alts.clear();

        // c)
        // don't set NT here, since we don't know the correct lhs pointer
        alt.nt = nullptr;
        alt.m_ys = lhs_nt->multi_ys();
        alt.name = lhs_nt->name;

        /* the inside lhs NT might have nt Parameters. If so we need to ensure
         * that the rhs call of the now outside NT is done with the same nt
         * parameters. The body is copy & paste from Expr/new.cc
         */
        alt.remove_ntparas();
        if (lhs_nt->ntargs().size() > 0) {
          alt.set_ntparas(Loc(), sync_ntparams(lhs_nt->ntargs()));
        }


        // d)
        Alt::Base *topaltclone = topalt->clone();
        /* if the topaltclone is Alt::Simple, we need to let it know its lhs
         * calling NT (or better its table dimensions) for downstream
         * construction of outside guards in the generated code */
        Propagate_lhsNT vlhsnt = Propagate_lhsNT(outside_rhs_nt);
        topaltclone->traverse(vlhsnt);
        alt_clones->push_back(std::make_pair(outside_rhs_nt, topaltclone));

        // e)
        alt.nt = orig_rhs_nt;
        alt.m_ys = orig_rhs_nt->multi_ys();
        alt.name = orig_rhs_nt->name;
        if (orig_alt_ntparas.size() > 0) {
          alt.set_ntparas(alt.location, &orig_alt_ntparas);
        } else {
          alt.remove_ntparas();
        }
      }
    }
  }

  void visit(Grammar &g) {
    throw LogError(
      "Please only apply at individual NTs, not the full grammar!");
  }
};

struct Flag_Outside_Grammar_Components : public Visitor {
  void visit(Symbol::Terminal &s) {
    s.set_partof_outside();
  }
  void visit(Symbol::NT &s) {
    s.set_partof_outside();
  }
  void visit(Alt::Base &a) {
    a.set_partof_outside();
  }
  void visit(Fn_Arg::Base &f) {
    f.set_partof_outside();
  }
};

/* end points of the inside grammar, i.e. production rules that terminate, are
 * the starting points of the outside grammar. Thus, we here iterate through
 * the inside part of the grammar, collect the lhs non-terminal names of these
 * terminating productions and finally create one novel non-terminal which we
 * designate to be the new outside axiom with as many alternatives as we
 * have collected lhs non-terminals before.
 * Edge case: if there is only one lhs non-terminal, we can directly use this
 * as outside axiom, without creating a new non-terminal first. */
struct Collect_and_Insert_outside_axioms : public Visitor {
  // counts the number of non-terminating rhs productions
  unsigned int rhs_nts = 0;

  // collects the lhs non-terminals that point to terminating rhs,
  // i.e. end point of inside = starting points of outside
  struct cmpNTname {
    // define sort order of std::set by name of the Symbol::NT
    // default would probably operate on pointer value?!
    bool operator() (Symbol::NT* const &a, Symbol::NT* const &b) const {
      return *a->name > *b->name;
    }
  };
  std::set<Symbol::NT*, cmpNTname> *terminating_lhs_nts;

  // a reference to the already created new outside non-terminals, into which
  // the new axiom must be added
  hashtable<std::string, Symbol::Base*> outside_nts;

  Collect_and_Insert_outside_axioms(
      hashtable<std::string, Symbol::Base*> outside_nts) :
        outside_nts(outside_nts) {
    terminating_lhs_nts = new std::set<Symbol::NT*, cmpNTname>();
  }
  ~Collect_and_Insert_outside_axioms() {
    delete terminating_lhs_nts;
  }

  void visit(Alt::Link &alt) {
    /* also check that alt.nt is not NULL as at this stage outside NTs are not
     * yet initialized, i.e. name is set correctly but the pointer to the NT
     * is null. */
    if (alt.nt && alt.nt->is(Symbol::NONTERMINAL)) {
      rhs_nts++;
    }
  }

  void visit_itr(Symbol::NT &nt) {
    if (rhs_nts == 0) {
      // we identified the (inside)nt as one that only points to terminating
      // productions. Thus, we search for its outside counterparts and add
      // this counterpart to the set of terminating_lhs_nts
      hashtable<std::string, Symbol::Base*>::iterator it_outside_nt =
        outside_nts.find(std::string(OUTSIDE_NT_PREFIX + *nt.name));
      if (it_outside_nt != outside_nts.end()) {
        assert((*it_outside_nt).second->is(Symbol::NONTERMINAL));
        terminating_lhs_nts->insert(
          dynamic_cast<Symbol::NT*>((*it_outside_nt).second));
      }
    }
    rhs_nts = 0;  // for next iteration
  }

  void visit_end(Grammar &grammar) {
    Symbol::NT *nt_axiom;
    if (terminating_lhs_nts->size() == 1) {
      // if there is only one candidate NT, we simple make this NT the new axiom
      hashtable<std::string, Symbol::Base*>::iterator a = grammar.NTs.find(
          *((*terminating_lhs_nts->begin())->name));
      // assert that the single found axiom candidate is actually in the NTs of
      // the grammar
      assert(a != grammar.NTs.end());
      // also assert that the axiom candidate is of type Symbol::NT
      // (not Symbol::Terminal)
      assert((*a).second->is(Symbol::NONTERMINAL));
      nt_axiom = dynamic_cast<Symbol::NT*>((*a).second);
    } else if (terminating_lhs_nts->size() > 1) {
      // it is more complicated if there are several NTs
      // we then need to create a novel lhs NT with the name "outside_axioms"
      std::string *axiom_name = new std::string(
          OUTSIDE_NT_PREFIX + std::string("axioms"));

      // we should double check that the user did not already define a NT with
      // this name
      hashtable<std::string, Symbol::Base*>::iterator it_ntclash =
          grammar.NTs.find(*axiom_name);
      if (it_ntclash != grammar.NTs.end()) {
        throw LogError((*it_ntclash).second->location, "Please avoid using '"
            + *axiom_name + "' as l.h.s. non-terminal name, when requesting "
            + "outside grammar generation!");
      }

      // create a fresh new non terminal which will become the outside axiom
      nt_axiom = new Symbol::NT(axiom_name, Loc());
      // but change its name
      nt_axiom->name = axiom_name;
      nt_axiom->orig_name = nt_axiom->name;
      // and clear all current alternatives
      nt_axiom->alts.clear();
      // remove choice function, as we chose/sum from/over different axiom
      // candidates
      nt_axiom->eval_fn = nullptr;

      // add all axiom candidates as alternatives to the new axiom
      for (std::set<Symbol::NT*>::iterator i = terminating_lhs_nts->begin();
           i != terminating_lhs_nts->end(); ++i) {
        Alt::Link *link = new Alt::Link((*i)->name, Loc());
        link->name = (*i)->name;
        link->set_tracks((*i)->tracks(), (*i)->track_pos());
        link->m_ys = Yield::Multi((*i)->tracks());
        link->top_level = Bool(true);
        if ((*i)->ntargs().size() > 0) {
          link->set_ntparas(Loc(), sync_ntparams((*i)->ntargs()));
        }
        nt_axiom->alts.push_back(link);
      }

      // a visitor to flag all sub-components as being outside
      Flag_Outside_Grammar_Components v_flag =
        Flag_Outside_Grammar_Components();
      nt_axiom->traverse(v_flag);

      // add new lhs non-terminal to grammar
      grammar.add_nt(nt_axiom);
    } else {
      throw LogError("You inside grammar does not seem to terminate, which "
          "should have been reported earlier via semantic checks?!");
    }

    // set grammar's axiom non-terminal
    grammar.axiom = nt_axiom;

    // set grammar's axiom name
    grammar.axiom_name = nt_axiom->name;

    // redirect grammar's axiom's location
    grammar.axiom_loc = nt_axiom->location;

    // initialize links of the new axiom
    grammar.init_axiom();
  }
};

/* There must be one production rule that realizes the transition from outside
 * to inside parts of the grammar. The target non-terminal must be the original
 * inside axiom and the source is the outside_ version of the original inside
 * axiom (not the outside axiom). However,
 */
Alt::Link *inject_outside_inside_transition(
    Grammar *grammar,
    hashtable<std::string, Symbol::Base*> &outside_nts,
    std::string name_inside_axiom) {
  std::string name_lhs_outside = std::string(
      OUTSIDE_NT_PREFIX + name_inside_axiom);

  /* if the axiom was never called on the rhs of the inside grammar part, the
   * outside counterpart of the inside axiom does not yet exist. Thus we create
   * it here */
  if (outside_nts.find(name_lhs_outside) == outside_nts.end()) {
    Symbol::NT *nt_lhs_outside = grammar->axiom->clone(
        grammar->axiom->track_pos(), false);
    nt_lhs_outside->name = new std::string(name_lhs_outside);
    nt_lhs_outside->orig_name = nt_lhs_outside->name;
    nt_lhs_outside->alts.clear();
    outside_nts[name_lhs_outside] = nt_lhs_outside;
  }

  Symbol::NT *lhs_outside = dynamic_cast<Symbol::NT*>(
    outside_nts[name_lhs_outside]);
  assert(lhs_outside);
  Symbol::NT *rhs_inside = dynamic_cast<Symbol::NT*>(
    grammar->NTs[name_inside_axiom]);
  assert(rhs_inside);

  /* we need to add a special filter to the transition, to ensure that
   * it is only invoked with the FULL input sequence(s). Don't forget multi-
   * track programs. */
  Alt::Link *link = new Alt::Link(new std::string(name_inside_axiom), Loc());
  /* nullptr as we don't know the pointer to lhs NT.
   * Will be resolved through init_nt_links(), just provide the correct name! */
  link->nt = nullptr;
  link->name = rhs_inside->name;
  Filter *f = new Filter(new std::string("complete_track"), Loc());
  f->type = Filter::WITH;
  std::list<Filter*> *comptracks = new std::list<Filter*>();
  for (unsigned int track = 0; track < rhs_inside->tracks(); ++track) {
    comptracks->push_back(f);
  }
  link->set_tracks(rhs_inside->tracks(), rhs_inside->track_pos());
  link->set_outside_inside_transition();
  if (rhs_inside->tracks() == 1) {
    link->filters.push_back(f);
  } else {
    link->add_multitrack_filter(*comptracks, f->type, Loc());
  }
  link->top_level = Bool(true);

  // flag this single link as the transition from outside to inside
  link->set_outside_inside_transition();

  // finally add this new transition to the list of alternatives
  lhs_outside->alts.push_back(link);

  // return pointer to the new outside_inside transition to later initialize
  // multi yield sizes, once the linked NT has been initialized
  return link;
}

void Grammar::convert_to_outside() {
  // a visitor to later flag all sub-components as being outside
  Flag_Outside_Grammar_Components v_flag = Flag_Outside_Grammar_Components();

  hashtable<std::string, Symbol::Base*> outside_nts;
  std::string name_inside_axiom = *this->axiom_name;

  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    if ((*i).second->is(Symbol::NONTERMINAL)) {
      Symbol::NT *nt_inside = dynamic_cast<Symbol::NT*>((*i).second);

      // don't operate on the original inside non-terminal, but on a clone in
      // which Alt::Block applications have been resolved
      Symbol::NT *nt_inside_resolved = nt_inside->clone(
          nt_inside->track_pos(), true);
      resolve_blocks(nt_inside_resolved);

      Flip_lhs_rhs_nonterminals v = Flip_lhs_rhs_nonterminals(
          nt_inside_resolved);
      nt_inside_resolved->traverse(v);

      // add new alternatives and new non-terminals to existing grammar
      for (std::list<std::pair<Symbol::NT*, Alt::Base*> >::iterator i =
           v.alt_clones->begin(); i != v.alt_clones->end(); ++i) {
        std::string *nt_name = (*i).first->name;
        hashtable<std::string, Symbol::Base*>::iterator it_nt =
            outside_nts.find(*nt_name);
        if (it_nt == outside_nts.end()) {
          outside_nts[*nt_name] = (*i).first;
        }
        it_nt = outside_nts.find(*nt_name);

        dynamic_cast<Symbol::NT*>((*it_nt).second)->alts.push_back((*i).second);
      }
    }
  }

  /* inject one alternative to the inside axiom which enables the transition
   * from outside parts into the original inside part of the grammar */
  Alt::Link *ln_transition = inject_outside_inside_transition(
      this, outside_nts, name_inside_axiom);

  // now add the new outside NTs to the grammar
  for (hashtable<std::string, Symbol::Base*>::iterator i = outside_nts.begin();
       i != outside_nts.end(); ++i) {
    /* flag the new NT as being outside, to tell apart automatically generated
     * NTs for outside parts from user defined inside parts. Necessary for e.g.
     *  - reverse index construction
     */
    (*i).second->traverse(v_flag);

    // add new outside non-terminal to the grammar
    this->add_nt(dynamic_cast<Symbol::NT*>((*i).second));
  }

  // inject new outside axiom (also flag as being outside)
  Collect_and_Insert_outside_axioms v = Collect_and_Insert_outside_axioms(
      outside_nts);
  this->traverse(v);

  // flag the grammar itself as being an outside version
  this->set_partof_outside();

  // initialize rhs non-terminals, i.e. point to the correct NT from the used
  // name.
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    (*i).second->init_links(*this);
  }

  // initialize m_ys arrays for newly constructed axiom + outside->inside
  // transition
  this->axiom->init_multi_ys();
  ln_transition->init_multi_ys();

  /* initialize yield sizes for the outside-inside transition, which is one of
   * the alternatives of the new outside axiom.
   * Can only be done AFTER links have been initialized, i.e. nt members are
   * no longer nullptr. */
  for (std::list<Alt::Base*>::iterator a = axiom->alts.begin();
       a != axiom->alts.end(); ++a) {
    if ((*a)->is(Alt::LINK) &&
        (dynamic_cast<Alt::Link*>(*a)->is_outside_inside_transition())) {
      (*a)->init_multi_ys();
    }
  }

  // TODO(smj): is this the proper location?
  /* NT-table dimension optimization (all start quadratic, but depending on
   * yield size, some NT-tables can be reduced to linear or even constant
   * "tables") must be re-considered, since original inside NTs will now
   * be called from outside NTs and former constant tables (e.g. consuming
   * the full input) might now be within a variable infix size and thus
   * must become quadratic again.
   * We therefore here reset the flag of all NTs to enable re-computation
   * of optimal table dimensions ... within the outside context.
   */
//  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
//       i != NTs.end(); ++i) {
//    if ((*i).second->is(Symbol::NONTERMINAL)) {
//      dynamic_cast<Symbol::NT*>((*i).second)->reset_table_dim();
//    }
//  }

  /* re-run "check_semantics" to properly initialize novel non-
   * terminals, links to non-terminals, update yield size analysis and
   * update table dimensions for NTs
   */
  this->check_semantic();

  Log::instance()->verboseMessage(
    "Grammar has been modified into an outside version.");
}
