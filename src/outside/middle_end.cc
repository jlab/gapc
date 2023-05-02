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

#include "middle_end.hh"


void Alt::Base::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track,
    std::list<Statement::For*> &simple_loops) {
}
void Alt::Simple::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track,
    std::list<Statement::For*> &simple_loops
    ) {
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    (*i)->outside_collect_parsers(left_parsers, right_parsers, num_outside_nts,
                                  track, this->loops);
  }
}
void Alt::Link::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track,
    std::list<Statement::For*> &simple_loops) {

  if (this->nt->is_partof_outside() || this->is_outside_inside_transition()) {
    num_outside_nts++;
  } else {
    Parser *p = new Parser(this->multi_ys()(track), this->left_indices,
                           this->right_indices, simple_loops);
    if (num_outside_nts < 1) {
      left_parsers.push_back(p);
    } else {
      right_parsers.insert(right_parsers.begin(), p);
    }
  }
}
void Alt::Block::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track,
    std::list<Statement::For*> &simple_loops) {
  // as Blocks should have been resolved for all outside components
  assert(false);
}
void Alt::Multi::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track,
    std::list<Statement::For*> &simple_loops) {
  size_t j = 0;
  assert(track < list.size());
  std::list<Base*>::iterator i = list.begin();
  for (; j < track; ++i, ++j) {}

  // each component is in a single-track context
  (*i)->outside_collect_parsers(left_parsers, right_parsers, num_outside_nts,
                                0, simple_loops);
}

void Fn_Arg::Base::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track,
    std::list<Statement::For*> &simple_loops) {
}
void Fn_Arg::Const::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track,
    std::list<Statement::For*> &simple_loops) {
  Parser *p = new Parser(this->multi_ys()(track), this->left_indices,
                         this->right_indices, simple_loops);
  if (num_outside_nts < 1) {
    left_parsers.push_back(p);
  } else {
    right_parsers.insert(right_parsers.begin(), p);
  }
}
void Fn_Arg::Alt::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track,
    std::list<Statement::For*> &simple_loops) {
  alt->outside_collect_parsers(left_parsers, right_parsers, num_outside_nts,
                               track, simple_loops);
}

Yield::Size sum_ys(std::vector<Parser*> parser,
    std::vector<Parser*>::iterator itr_start,
    std::vector<Parser*>::iterator itr_end,
    size_t track) {
  Yield::Size ys;
  // don't risk undefined yield sizes!
  ys.set(0, 0);

  for (std::vector<Parser*>::iterator i = itr_start;
       (i != parser.end()) && (i != itr_end); ++i) {
    ys += (*i)->yield_size;
  }

  return ys;
}

void Alt::Base::outside_uppropagate_indices(
    Expr::Vacc *left, Expr::Vacc *right, size_t track) {}
void Alt::Simple::outside_uppropagate_indices(
    Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
         ++i) {
    (*i)->outside_uppropagate_indices(left, right, track);
  }
  left_indices[track] = (*args.begin())->left_indices[track];
  if (left_indices[track] == nullptr) {
    // fall back if outside NT is leftmost component of alternative
    left_indices[track] = left;
  }
  right_indices[track] = (*args.rbegin())->right_indices[track];
  if (right_indices[track] == nullptr) {
    // fall back if outside NT is rightmost component of alternative
    right_indices[track] = right;
  }
}
void Alt::Link::outside_uppropagate_indices(
    Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  // Links point to grammar leaves and should already have left/right indices
  // set in phase 2
  if (!this->nt->is_partof_outside()) {
    assert(this->left_indices[track]);
    assert(this->right_indices[track]);
  }
}
void Alt::Block::outside_uppropagate_indices(
    Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  assert(false);  // Alt::Block should have been resolved already
}
void Alt::Multi::outside_uppropagate_indices(
    Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  size_t j = 0;
  assert(track < list.size());
  std::list<Base*>::iterator i = list.begin();
  for (; j < track; ++i, ++j) {}

  // each component is in a single-track context
  (*i)->outside_uppropagate_indices(left, right, 0);

  left_indices[track] = (*i)->get_left_index(0);
  right_indices[track] = (*i)->get_right_index(0);
}
void Fn_Arg::Base::outside_uppropagate_indices(
    Expr::Vacc *left, Expr::Vacc *right, size_t track) {}
void Fn_Arg::Alt::outside_uppropagate_indices(
    Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  alt->outside_uppropagate_indices(left, right, track);
  this->left_indices[track] = alt->get_left_index(track);
  this->right_indices[track] = alt->get_right_index(track);
}
void Fn_Arg::Const::outside_uppropagate_indices(
    Expr::Vacc *left, Expr::Vacc *right, size_t track) {}

void iterate_indices(bool is_left_not_right,
                     std::vector<Parser*> *parser,
                     unsigned int &k, size_t track,
                     unsigned int tracks,
                     Expr::Base *next_var,
                     Expr::Base *last_var,
                     Expr::Vacc *upstream_index,
                     Yield::Size ys_all) {
  Yield::Size lhs;
  lhs.set(0, 0);
  for (std::vector<Parser*>::iterator i = parser->begin();
       i != parser->end(); ++i) {
    Yield::Size ys = (*i)->yield_size;
    Yield::Size rhs = sum_ys(*parser, std::next(i), parser->end(), track);
    Yield::Size rhs_ys(rhs);
    rhs_ys += ys;

#ifdef LOOPDEBUG
  std::cerr << (is_left_not_right ? "2" : "4") << ") ";
#endif
    next_var = Alt::next_index_var(
        k, track, next_var, last_var, upstream_index,
        ys, lhs, rhs, &((*i)->simple_loops), true, false, is_left_not_right);

    // copy and paste from Alt::Simple::init_indices
    std::pair<Expr::Base*, Expr::Base*> res(0, 0);
    if ((ys_all.low() == ys_all.high())) {
      // no moving boundary between here and the outside_nt
      if (is_left_not_right) {
        res.first = last_var->minus(rhs_ys.low());
        res.second = last_var->minus(rhs.low());
      } else {
        res.second = last_var->plus(rhs_ys.low());
        res.first = last_var->plus(rhs.low());
      }
      lhs += ys;
    } else {
      if (lhs.low() == lhs.high()) {
        if (is_left_not_right) {
          res.first = last_var->plus(lhs.low());
        } else {
          res.second = last_var->minus(lhs.low());
        }
        if (ys.low() == ys.high()) {
          if (is_left_not_right) {
            res.second = last_var->plus(lhs.low())->plus(ys.low());
          } else {
            res.first = last_var->minus(lhs.low())->minus(ys.low());
          }
          lhs += ys;
        } else {
          if (rhs.low() == rhs.high()) {
            if (is_left_not_right) {
              res.second = next_var->minus(rhs.low());
            } else {
              res.first = next_var->plus(rhs.low());
            }
            lhs += ys;
          } else {
            if (is_left_not_right) {
              res.second = next_var;
            } else {
              res.first = next_var;
            }
            lhs.set(0, 0);
            last_var = next_var;
          }
        }
      } else {
        assert(rhs_ys.low() == rhs_ys.high());
        if (is_left_not_right) {
          res.first = next_var->minus(rhs_ys.low());
          res.second = next_var->minus(rhs.low());
        } else {
          res.second = next_var->plus(rhs_ys.low());
          res.first = next_var->plus(rhs.low());
        }
        lhs += ys;
      }
    }
    if (tracks > (*i)->left_indices.size()) {
      // must be a single track component in a multitrack context
      (*i)->left_indices.at(0) = res.first;
      (*i)->right_indices.at(0) = res.second;
    } else {
      (*i)->left_indices.at(track) = res.first;
      (*i)->right_indices.at(track) = res.second;
    }
  }
}

void outside_init_indices(
    Alt::Base *alt, Expr::Vacc *left, Expr::Vacc *right, unsigned int &k,
    size_t track, Expr::Vacc *left_most, Expr::Vacc *right_most) {
#ifdef LOOPDEBUG
  if (alt->is(Alt::SIMPLE)) {
    std::cerr << "  alt::Simple '"
              << *(dynamic_cast<Alt::Simple*>(alt)->name) << "':\n";
  } else if (alt->is(Alt::LINK)) {
    std::cerr << "  alt::Link '"
              << *(dynamic_cast<Alt::Link*>(alt)->name) << "':\n";
  } else {
    std::cerr << "  alt '?':\n";
  }
#endif
  std::vector<Parser*> left_parser;
  std::vector<Parser*> right_parser;
  unsigned int num_outside_nts = 0;

  /* phase 1: traverse whole sub-tree of alternative (can include multiple
   * levels) and collect
   * all grammar components that "parse" subwords from the input (can
   * be empty) */
  std::list<Statement::For *> loops;
  if (alt->is(Alt::SIMPLE)) {
    loops = dynamic_cast<Alt::Simple*>(alt)->loops;
  }
  alt->outside_collect_parsers(left_parser, right_parser, num_outside_nts,
                               track, loops);

  // by design, there must be exactly one rhs outside NT in each alternative
  // only exception is the transition from outside to inside grammar parts
  assert(num_outside_nts == 1);

  // phase 2: based on the collected Parsers, assign left and right indices
  // to Parsers for grammar components LEFT of outside NT
  Yield::Size ys_all = sum_ys(left_parser, left_parser.begin(),
                              left_parser.end(), track);
#ifdef LOOPDEBUG
  std::cerr << "1) ";
#endif
  Yield::Size ys_lhs = sum_ys(left_parser, left_parser.begin(),
      left_parser.begin(), track);
  Yield::Size ys;
  ys.set(0, 0);
  Expr::Base *next_var = Alt::next_index_var(k, track, left, left_most, left,
                                             ys, ys_lhs, ys_all,
                                             &loops, true, true, true);
  Expr::Base *last_var = next_var;
  iterate_indices(true, &left_parser, k, track, alt->multi_ys().tracks(),
                  next_var, last_var, left, ys_all);
  // for grammar components RIGHT of outside NT
  if (right_parser.size() > 0) {
    ys_all = sum_ys(right_parser, right_parser.begin(), right_parser.end(),
                    track);
#ifdef LOOPDEBUG
  std::cerr << "3) ";
#endif
    Yield::Size ys_rhs = sum_ys(right_parser, right_parser.begin(),
        right_parser.begin(), track);
    last_var = Alt::next_index_var(k, track, right, right_most, right,
                                   ys, ys_all, ys_rhs,
                                   &loops, true, true, false);
    iterate_indices(false, &right_parser, k, track, alt->multi_ys().tracks(),
                    next_var, last_var, right, ys_all);
  }

  GetOutsideLink v = GetOutsideLink();
  alt->traverse(v);
  if ((left_parser.size() == 0) && (right_parser.size() == 0) &&
      v.outside_link) {
    if (v.outside_link->is_outside_inside_transition()) {
      v.outside_link->Base::init_indices(left->plus(right), right->minus(left),
                                         k, track);
    } else {
      // must be a direct link to an non-terminal
      v.outside_link->Base::init_indices(left, right, k, track);
    }
  }

  // Phase 3: propagate left/right indices from Parser towards the top
  alt->outside_uppropagate_indices(left, right, track);

  // Phase 4: set left/right indices of outside NT to the top level left/right
  // indices
  if (v.outside_fn_arg) {
    v.outside_fn_arg->init_indices(alt->get_left_index(track),
                                   alt->get_right_index(track), k, track);
  } else {
    v.outside_link->init_indices(alt->get_left_index(track),
                                 alt->get_right_index(track), k, track);
  }

  if (alt->is(Alt::SIMPLE)) {
    dynamic_cast<Alt::Simple*>(alt)->loops.insert(
        dynamic_cast<Alt::Simple*>(alt)->loops.begin(),
        loops.begin(), loops.end());
  }
}

void Alt::Simple::init_outside_guards() {
  std::list<Expr::Base*> l;
  assert(m_ys.tracks() == left_indices.size());

  size_t track = 0;
  for (std::vector<Expr::Base*>::iterator i = left_indices.begin();
       i != left_indices.end(); ++i, ++track) {
    // obtain yield sizes for components left/right of outside NT
    std::vector<Parser*> left_parser;
    std::vector<Parser*> right_parser;
    unsigned int num_outside_nts = 0;
    std::list<Statement::For *> loops;
    this->outside_collect_parsers(left_parser, right_parser, num_outside_nts,
                                  track, loops);
    if (num_outside_nts != 1) {
      /* we branched into a pure inside context, this will always happen if
       * where an inside production uses multiple non terminals on its rhs, e.g.
       * an inside rule like struct = cadd(dangle, struct) will lead to two
       * outside rules: outside_dangle = cadd(outside_struct, struct) and
       *                outside_struct = cadd(dangle, outside_struct)
       * which hold inside and outside parts. */
      continue;
    }

    // obtain outside NT
    GetOutsideLink v = GetOutsideLink();
    this->traverse(v);

    // create conditions like
    // if (((t_0_i >= (t_0_left_most + 6)) &&
    //     ((t_0_j + 4) <= t_0_right_most))) {
    l.push_back(
      new Expr::Greater_Eq(
          v.outside_link->nt->left_indices[track],
          v.outside_link->nt->left_most_indices[track]->plus(
              sum_ys(left_parser, left_parser.begin(),
                     left_parser.end(), track).low())));
    l.push_back(
      new Expr::Less_Eq(
          v.outside_link->nt->right_indices[track]->plus(
              sum_ys(right_parser, right_parser.begin(),
                     right_parser.end(), track).low()),
          v.outside_link->nt->right_most_indices[track]));
  }

  // only create guards for outside situations, but not for inside parts in an
  // outside context. See above comment.
  if (l.size() > 0) {
    Expr::Base *cond  = Expr::seq_to_tree<Expr::Base, Expr::And>(
      l.begin(), l.end());

    guards_outside = new Statement::If(cond);
    ret_decl_empty_block(guards_outside);
  }
}

