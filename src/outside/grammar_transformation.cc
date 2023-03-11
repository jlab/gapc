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

#include "grammar_transformation.hh"

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

  /* TODO(sjanssen): I need to find out if following types are either terminals
   * or not: Signature, Table, Base, List, Name, Tuple, Def, Choice, Size,
   *         Range, Table, Generic, Referencable
   */
  assert(false && "I have been lazy and did not yet define if above mentioned"
                  " types are terminals or not");
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

  // pointer to the Fn_Arg::Alt that encloses the first found block - iff it's parent is an Alt::Base
  Fn_Arg::Alt *block_fnarg = nullptr;

  // the top level alternative that contains (somewhere) the first found block
  Alt::Base *topalt = nullptr;

  // the direct Alt::Base parent of the first found block - iff it is not a Symbol::NT
  Alt::Base *parent_alt = nullptr;

  // the direct Symbol::NT parent of the first found block - iff it is not an Alt::Block
  Symbol::NT *parent_nt = nullptr;

  FindFirstBlock() : block(nullptr), block_fnarg(nullptr), parent_alt(nullptr), parent_nt(nullptr) {
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
    throw LogError("Please only apply at individual NTs, not the full grammar!");
  }
};

void resolve_blocks(Symbol::NT *nt) {
  if (nt) {
    // check if there is any Alt::Block at the rhs of the NT
    FindFirstBlock v_block = FindFirstBlock();
    nt->traverse(v_block);

    // iterate through all alternatives until no more Alt::Block can be found
    while (v_block.block) {
      std::list<Alt::Base*>::iterator topalt = nt->alts.begin();
      for (; topalt != nt->alts.end(); ++topalt) {
        if ((*topalt) == v_block.topalt) {
          break;
        }
      }

      // Alt::Block can either occur within an algebra function like struct = cadd(foo, {joe, user})
      if (v_block.parent_alt && !v_block.parent_nt) {
        if (v_block.parent_alt->is(Alt::SIMPLE)) {
          // parent of the block is an Alt::Simple, i.e. has a list of children
          for (std::list<Alt::Base*>::iterator child = v_block.block->alts.begin(); child != v_block.block->alts.end(); ++child) {
            // create a clone of the full alternative (up to the top level) that contains this block. This will invalidate all pointer information we have for the block ...
            Alt::Base *clone = (*v_block.topalt).clone();

            // ... thus acquire these info again, but for the clone, which is not yet part of any non-terminal
            FindFirstBlock v_clone = FindFirstBlock();
            clone->traverse(v_clone);

            // now replace the block in the clone with the child of the original block
            v_clone.block_fnarg->alt = *child;

            // carry over filters that are attached to the block, from the block to the child in the clone
            v_clone.block_fnarg->alt->filters.insert(v_clone.block_fnarg->alt->filters.end(),
               v_block.block->filters.begin(),
               v_block.block->filters.end());
            v_clone.block_fnarg->alt->multi_filter.insert(v_clone.block_fnarg->alt->multi_filter.end(),
               v_block.block->multi_filter.begin(),
               v_block.block->multi_filter.end());

            // insert new (partially, since it can still hold further Blocks) alternative into rhs of the NT
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

      // or directly as a top level alternative of the non-termial, like struct = {joe, user}
      } else if (!v_block.parent_alt && v_block.parent_nt) {
        for (std::list<Alt::Base*>::iterator child = v_block.block->alts.begin(); child != v_block.block->alts.end(); ++child) {
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

          // insert new (partially, since it can still hold further Blocks) alternative into rhs of the NT
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

void Grammar::convert_to_outside() {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
         i != NTs.end(); ++i) {
    if ((*i).second->is(Symbol::NONTERMINAL)) {
      std::cerr << ((*i).first) << "\n";
      resolve_blocks(dynamic_cast<Symbol::NT*>((*i).second));
    }
  }

  Log::instance()->verboseMessage(
    "Grammar has been modified into an outside version.");
}
