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
    size_t track) {
}
void Alt::Simple::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track) {

  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    (*i)->outside_collect_parsers(left_parsers, right_parsers, num_outside_nts, track);
  }
}
void Alt::Link::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track) {

  if (this->nt->is_partof_outside() || this->is_outside_inside_transition()) {
    num_outside_nts++;
  } else {
    Parser *p = new Parser(this->multi_ys()(track), (this->left_indices), (this->right_indices));
    if (num_outside_nts < 1) {
      left_parsers.push_back(p);
    } else {
      right_parsers.push_back(p);
    }
  }
}
void Alt::Block::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track) {
  // as Blocks should have been resolved for all outside components
  assert(false);
}
void Alt::Multi::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track) {
  size_t j = 0;
  assert(track < list.size());
  std::list<Base*>::iterator i = list.begin();
  for (; j < track; ++i, ++j) {}

  // each component is in a single-track context
  (*i)->outside_collect_parsers(left_parsers, right_parsers, num_outside_nts, 0);
}

void Fn_Arg::Base::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track) {
}
void Fn_Arg::Const::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track) {
  Parser *p = new Parser(this->multi_ys()(track), (this->left_indices), (this->right_indices));
  if (num_outside_nts < 1) {
    left_parsers.push_back(p);
  } else {
    right_parsers.push_back(p);
  }
}
void Fn_Arg::Alt::outside_collect_parsers(
    std::vector<Parser*> &left_parsers,
    std::vector<Parser*> &right_parsers,
    unsigned int &num_outside_nts,
    size_t track) {
  alt->outside_collect_parsers(left_parsers, right_parsers, num_outside_nts, track);
}

Yield::Size sum_ys(std::vector<Parser*> parser,
    std::vector<Parser*>::iterator itr_start,
    std::vector<Parser*>::iterator itr_end,
    size_t track) {
  Yield::Size ys;

  for (std::vector<Parser*>::iterator i = itr_start; (i != parser.end()) && (i != itr_end); ++i) {
    ys += (*i)->yield_size;
  }

  return ys;
}

void Alt::Base::outside_uppropagate_indices(Expr::Vacc *left, Expr::Vacc *right, size_t track) {}
void Alt::Simple::outside_uppropagate_indices(Expr::Vacc *left, Expr::Vacc *right, size_t track) {
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
void Alt::Link::outside_uppropagate_indices(Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  // Links point to grammar leaves and should already have left/right indices set in phase 2
  if (!this->nt->is_partof_outside()) {
    assert(this->left_indices[track]);
    assert(this->right_indices[track]);
  }
}
void Alt::Block::outside_uppropagate_indices(Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  assert(false);  // Alt::Block should have been resolved already
}
void Alt::Multi::outside_uppropagate_indices(Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  size_t j = 0;
  assert(track < list.size());
  std::list<Base*>::iterator i = list.begin();
  for (; j < track; ++i, ++j) {}

  // each component is in a single-track context
  (*i)->outside_uppropagate_indices(left, right, 0);

  left_indices[track] = (*i)->left_indices[0];
  right_indices[track] = (*i)->right_indices[0];
}
void Fn_Arg::Base::outside_uppropagate_indices(Expr::Vacc *left, Expr::Vacc *right, size_t track) {}
void Fn_Arg::Alt::outside_uppropagate_indices(Expr::Vacc *left, Expr::Vacc *right, size_t track) {
  alt->outside_uppropagate_indices(left, right, track);
  this->left_indices[track] = alt->left_indices[track];
  this->right_indices[track] = alt->right_indices[track];
}
void Fn_Arg::Const::outside_uppropagate_indices(Expr::Vacc *left, Expr::Vacc *right, size_t track) {}

void outside_init_indices(Alt::Base *alt, Expr::Vacc *left, Expr::Vacc *right, unsigned int &k, size_t track) {
  std::vector<Parser*> left_parser;
  std::vector<Parser*> right_parser;
  unsigned int num_outside_nts = 0;

  //TODO(smj): how about has_index_overlay?!

  // phase 1: traverse whole sub-tree of alternative (can include multiple levels) and collect
  // all grammar components that "parse" subwords from the input (can be empty)
  alt->outside_collect_parsers(left_parser, right_parser, num_outside_nts, track);

  // by design, there must be exactly one rhs outside NT in each alternative
  // only exception is the transition from outside to inside grammar parts
  assert(num_outside_nts == 1);

  // phase 2: based on the collected Parsers, assign left and right indices to Parsers
  Yield::Size ys_all = sum_ys(left_parser, left_parser.begin(), left_parser.end(), track);
  Expr::Base *last_var = next_index_var(k, track, left, left, right, ys_all, Yield::Size(), ys_all);
  Expr::Base *next_var = last_var;

  Yield::Size lhs;
  // LEFT of outside NT
  for (std::vector<Parser*>::iterator i = left_parser.begin(); i != left_parser.end(); ++i) {
    Yield::Size ys = (*i)->yield_size;
    Yield::Size rhs = sum_ys(left_parser, std::next(i), left_parser.end(), track);
    Yield::Size rhs_ys(rhs);
    rhs_ys += ys;

    next_var = next_index_var(k, track, next_var, last_var, left, ys, lhs, rhs);

    // copy and paste from Alt::Simple::init_indices
    std::pair<Expr::Base*, Expr::Base*> res(0, 0);
    if (ys_all.low() == ys_all.high()) {
      // no moving boundary between here and the outside_nt
      res.first = last_var->minus(rhs_ys.low());
      res.second = last_var->minus(rhs.low());
      lhs += ys;
    } else {
      if (lhs.low() == lhs.high()) {
        res.first = last_var->plus(lhs.low());
        if (ys.low() == ys.high()) {
          res.second = last_var->plus(lhs.low())->plus(ys.low());
          lhs += ys;
        } else {
          if (rhs.low() == rhs.high()) {
            res.second = next_var->minus(rhs.low());
            lhs += ys;
          } else {
            res.second = next_var;
            lhs.set(0, 0);
            last_var = next_var;
          }
        }
      } else {
        assert(rhs_ys.low() == rhs_ys.high());
        res.first = next_var->minus(rhs_ys.low());
        res.second = next_var->minus(rhs.low());
        lhs += ys;
      }
    }
    if (alt->multi_ys().tracks() > (*i)->left_indices.size()) {
      // must be a single track component in a multitrack context
      (*i)->left_indices.at(0) = res.first;
      (*i)->right_indices.at(0) = res.second;
    } else {
      (*i)->left_indices.at(track) = res.first;
      (*i)->right_indices.at(track) = res.second;
    }
  }

  // RIGHT of outside NT
  if (right_parser.size() > 0) {
    lhs = sum_ys(right_parser, right_parser.begin(), right_parser.end(), track);
    last_var = right;
    lhs.set(0, 0);
    for (std::vector<Parser*>::iterator i = right_parser.begin(); i != right_parser.end(); ++i) {
      Yield::Size ys = (*i)->yield_size;
      Yield::Size rhs = sum_ys(right_parser, std::next(i), right_parser.end(), track);
      Yield::Size rhs_ys(rhs);
      rhs_ys += ys;

      next_var = next_index_var(k, track, next_var, last_var, right, ys, lhs, rhs);

      // copy and paste from Alt::Simple::init_indices
      std::pair<Expr::Base*, Expr::Base*> res(0, 0);
      if (lhs.low() == lhs.high()) {
        res.first = last_var->plus(lhs.low());
        if (ys.low() == ys.high()) {
          res.second = last_var->plus(lhs.low())->plus(ys.low());
          lhs += ys;
        } else {
          if (rhs.low() == rhs.high()) {
            // edge case: we encounter right end but need another moving boundary for outside context
            next_var = next_index_var(k, track, next_var, last_var, right, ys, lhs, Yield::Size(Yield::UP));

            res.second = next_var->minus(rhs.low());
            lhs += ys;
          } else {
            res.second = next_var;
            lhs.set(0, 0);
            last_var = next_var;
          }
        }
      } else {
        assert(rhs_ys.low() == rhs_ys.high());
        res.first = next_var->minus(rhs_ys.low());
        res.second = next_var->minus(rhs.low());
        lhs += ys;
      }
      if (alt->multi_ys().tracks() > (*i)->left_indices.size()) {
        // must be a single track component in a multitrack context
        (*i)->left_indices.at(0) = res.first;
        (*i)->right_indices.at(0) = res.second;
      } else {
        (*i)->left_indices.at(track) = res.first;
        (*i)->right_indices.at(track) = res.second;
      }
    }
  }

  GetOutsideLink v = GetOutsideLink();
  alt->traverse(v);
  if ((left_parser.size() == 0) && (right_parser.size() == 0) && v.outside_link) {
    if (v.outside_link->is_outside_inside_transition()) {
      v.outside_link->Base::init_indices(left->plus(right), right->minus(left), k, track);
    } else {
      // must be a direct link to an non-terminal
      v.outside_link->Base::init_indices(left, right, k, track);
    }
  }

  // Phase 3: propagate left/right indices from Parser towards the top
  alt->outside_uppropagate_indices(left, right, track);

  // Phase 4: set left/right indices of outside NT to the top level left/right indices
  v.outside_link->left_indices[track] = alt->left_indices[track];
  v.outside_link->right_indices[track] = alt->right_indices[track];
}


Expr::Base *next_index_var(unsigned &k, size_t track,
  Expr::Base *next_var, Expr::Base *last_var, Expr::Base *right,
  const Yield::Size &ys, const Yield::Size &lhs, const Yield::Size &rhs) {
  if (ys.low() != ys.high()) {
    if (rhs.low() == rhs.high()) {
      return right;
    } else {
      std::ostringstream o;
      o << "t_" << track << "_k_" << k;
      k++;
      Expr::Vacc *ivar = new Expr::Vacc(new std::string(o.str()));

      assert(lhs.low() == lhs.high());
      std::pair<Expr::Base*, Expr::Base*> index(0, 0);

      Yield::Size lhs_ys(lhs);
      lhs_ys += ys;

      if (rhs.high() == Yield::UP) {
        index.first = last_var->plus(lhs_ys.low());
      } else {
        // e.g. second maxsize filter in grammar/forloops5
        Expr::Cond *ce = new Expr::Cond(
          new Expr::Greater_Eq(right->minus(
            last_var->plus(lhs_ys.low())), rhs.high()),
          right->minus(rhs.high()), last_var->plus(lhs_ys.low()));
        index.first = ce;
      }

      index.second = right->minus(rhs.low());

      Expr::Base *cond = new Expr::Less_Eq(ivar, index.second);
      // e.g. first maxsize filter in grammar/forloops5
      if (lhs_ys.high() < Yield::UP) {
        cond = new Expr::And(
          cond, new Expr::Less_Eq (ivar, last_var->plus(lhs_ys.high())));
      }

      Statement::Var_Decl *loopvariable = new Statement::Var_Decl(
        new ::Type::Size(), ivar, index.first);
      // flag this variable as being an iterator e.g. in for-loops,
      // such that it won't have a trailing indent for code generation
      loopvariable->set_itr(true);
      Statement::For *f = new Statement::For (loopvariable, cond);
      //loops.push_back(f);
      return ivar;
    }
  } else {
    return next_var;
  }
}
