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

#ifndef SRC_OUTSIDE_MIDDLE_END_HH_
#define SRC_OUTSIDE_MIDDLE_END_HH_

#include "../visitor.hh"
#include "../alt.hh"
#include "../fn_arg.hh"
#include "../symbol.hh"
#include "../expr.hh"
#include "../statement.hh"

// copy and paste from alt.cc
Expr::Base *next_index_var(unsigned &k, size_t track,
  Expr::Base *next_var, Expr::Base *last_var, Expr::Base *right,
  const Yield::Size &ys, const Yield::Size &lhs, const Yield::Size &rhs);

struct Init_Indices_Outside_Args : public Visitor {
  size_t track;
  unsigned int k;
  Alt::Base *topalt;

  Init_Indices_Outside_Args(unsigned int &k, size_t track) {
    this->track = track;
    this->k = k;
  }

  void visit_end(Alt::Simple &a) {
    /* initialize left and right indices, simply by taking the
     *   left index of the left-most argument and
     *   right index of the right-most argument */
    (&a)->Base::init_indices(
        (*a.args.begin())->left_indices.at(track),
        (*a.args.rbegin())->right_indices.at(track),
        k,
        track);
  }

  void visit(Fn_Arg::Alt &f) {
    f.left_indices = f.alt->left_indices;
    f.right_indices = f.alt->right_indices;
  }
};

struct Init_Indices_OutsideNT : public Visitor {
  size_t track;
  unsigned int k;
  Alt::Base *topalt;

  Init_Indices_OutsideNT(Alt::Base *topalt, unsigned int &k, size_t track) {
    this->track = track;
    this->k = k;
    this->topalt = topalt;
  }

//  void visit(Fn_Arg::Alt &f) {
//
//  }

  void visit(Alt::Link &s) {
    if (s.nt->is(Symbol::NONTERMINAL) && s.nt->is_partof_outside()) {
      std::cerr << *s.nt->name << "\n";
      if (topalt->left_indices.at(track) && topalt->right_indices.at(track)) {
      (&s)->Base::init_indices(
          topalt->left_indices.at(track),
          topalt->right_indices.at(track),
          k,
          track);
      }
    }
  }
};

struct Init_Indices_Outside : public Visitor {
 private:
  int num_outside_nts = 0;
  std::vector<void*> _left;
  enum Type { ALT, FN_ARG, SYMBOL };
  std::vector<Type> _left_types;
  std::vector<void*> _right;
  std::vector<Type> _right_types;

  Alt::Link *_outsideNT = nullptr;

  void stack_component(void *ptr_component, Type t) {
    if (num_outside_nts < 1) {
      _left.push_back(ptr_component);
      _left_types.push_back(t);
    } else {
      _right.push_back(ptr_component);
      _right_types.push_back(t);
    }
  }

  void assert_one_outside_nt() {
    // by design, there must be exactly one rhs outside NT in each alternative
    // only exception is the transition from outside to inside grammar parts
    assert(num_outside_nts == 1);
  }

 public:

  void init_indices(Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track) {
    assert_one_outside_nt();

    Yield::Size ys_all = sum_ys_left(nullptr, nullptr, track);
    Expr::Base *last_var = next_index_var(k, track, NULL, left, right, ys_all, Yield::Size(), ys_all);
    Expr::Base *next_var = last_var;

    Yield::Size lhs;
    // LEFT of outside NT
    std::vector<Type>::iterator i_type = _left_types.begin();
    for (std::vector<void*>::iterator i = _left.begin(); i != _left.end(); ++i, ++i_type) {
      Yield::Size ys = sum_ys_left(*i, *i, track);
      Yield::Size rhs = sum_ys_left(*std::next(i), nullptr, track);
      Yield::Size rhs_ys(rhs);
      rhs_ys += ys;

//      std::cerr << "L: " << lhs << " - " << ys << " - " << rhs;

      if (*i_type == ALT) {
        Alt::Base *a = static_cast<Alt::Base*>(*i);
        //std::cerr << (*a).left_indices.size() << " <- Alt\n";
        if (a->is(Alt::LINK)) {
//          std::cerr << " : " << *(dynamic_cast<Alt::Link*>(a)->name);
          next_var = next_index_var(k, track, next_var, last_var, left, ys, lhs, rhs);
//          std::cerr << " next_var = " <<  *next_var;

          // copy and paste from Alt::Simple::init_indices
          std::pair<Expr::Base*, Expr::Base*> res(0, 0);
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

          a->init_indices(res.first, res.second, k, track);
          // end copy and paste

//          std::cerr << " (left = " << *res.first << ", right = " << *res.second << ") ";
        } else if (a->is(Alt::SIMPLE)) {
//          std::cerr << " : Alt::Simple " << *(dynamic_cast<Alt::Simple*>(a)->name);
        }
      } else if (*i_type == FN_ARG) {
        Fn_Arg::Base *f = static_cast<Fn_Arg::Base*>(*i);
//        std::cerr << " : Fn_Arg";
        //std::cerr << (*f).left_indices.size() << " <- Fn_Arg\n";
        //continue;
      } else if (*i_type == SYMBOL) {
        Symbol::Base *s = static_cast<Symbol::Base*>(*i);
//        std::cerr << " : Symbol";
        //std::cerr << (*s).left_indices.size() << " <- Symbol\n";
//        accum_ys_center_to_left += (*s).multi_ys()(track);
      }

//      std::cerr << "\n";
    }


    // RIGHT of outside NT
    lhs = sum_ys_right(nullptr, nullptr, track);
//    next_var = right;
    last_var = right;
    lhs.set(0, 0);
    i_type = _right_types.begin();
    for (std::vector<void*>::iterator i = _right.begin(); i != _right.end(); ++i, ++i_type) {
      Yield::Size ys = sum_ys_right(*i, *i, track);
//      Yield::Size rhs = Yield::Size(Yield::UP); //sum_ys_right(*std::next(i), nullptr, track);
      Yield::Size rhs = sum_ys_right(*std::next(i), nullptr, track);
      Yield::Size rhs_ys(rhs);
      rhs_ys += ys;

//      std::cerr << "R: " << lhs << " - " << ys << " - " << rhs;

      if (*i_type == ALT) {
        Alt::Base *a = static_cast<Alt::Base*>(*i);
        //std::cerr << (*a).left_indices.size() << " <- Alt\n";
        if (a->is(Alt::LINK)) {
//          std::cerr << " : " << *(dynamic_cast<Alt::Link*>(a)->name);
          next_var = next_index_var(k, track, next_var, last_var, right, ys, lhs, rhs);
//          std::cerr << " next_var = " <<  *next_var;

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

          a->init_indices(res.first, res.second, k, track);
          // end copy and paste

//          std::cerr << " (left = " << *res.first << ", right = " << *res.second << ") ";
        } else if (a->is(Alt::SIMPLE)) {
//          std::cerr << " : Alt::Simple " << *(dynamic_cast<Alt::Simple*>(a)->name);
        }
      } else if (*i_type == FN_ARG) {
        Fn_Arg::Base *f = static_cast<Fn_Arg::Base*>(*i);
//        std::cerr << " : Fn_Arg";
        //std::cerr << (*f).left_indices.size() << " <- Fn_Arg\n";
        //continue;
      } else if (*i_type == SYMBOL) {
        Symbol::Base *s = static_cast<Symbol::Base*>(*i);
//        std::cerr << " : Symbol";
        //std::cerr << (*s).left_indices.size() << " <- Symbol\n";
//        accum_ys_center_to_left += (*s).multi_ys()(track);
      }

//      std::cerr << "\n";
    }

//    std::vector<Type>::reverse_iterator ir_type = _left_types.rbegin();
//    //Expr::Base *last_var = next_index_var(k, track, NULL, left, right, ys_all, Yield::Size(), ys_all);
//    for (std::vector<void*>::reverse_iterator ir = _right.rbegin(); ir != _right.rend(); ++ir, ++ir_type) {
//      Yield::Size ys = sum_ys_right(*ir, *ir, track, false);
//      Yield::Size rhs = sum_ys_right(*std::next(ir), nullptr, track, false);
//      Yield::Size rhs_ys(rhs);
//      rhs_ys += ys;
//
//      std::cerr << lhs << " - " << ys << " - " << rhs;
//
//      if (*i_type == ALT) {
//        Alt::Base *a = static_cast<Alt::Base*>(*ir);
//        //std::cerr << (*a).left_indices.size() << " <- Alt\n";
//        if (a->is(Alt::LINK)) {
//          std::cerr << " : " << *(dynamic_cast<Alt::Link*>(a)->name);
//          next_var = next_index_var(k, track, next_var, last_var, last_var, ys, lhs, rhs);
//          std::cerr << " next_var = " <<  *next_var;
//
//          // copy and paste from Alt::Simple::init_indices
//          std::pair<Expr::Base*, Expr::Base*> res(0, 0);
//          if (lhs.low() == lhs.high()) {
//            res.first = last_var->plus(lhs.low());
//            if (ys.low() == ys.high()) {
//              res.second = last_var->plus(lhs.low())->plus(ys.low());
//              lhs += ys;
//            } else {
//              if (rhs.low() == rhs.high()) {
//                res.second = next_var->minus(rhs.low());
//                lhs += ys;
//              } else {
//                res.second = next_var;
//                lhs.set(0, 0);
//                last_var = next_var;
//              }
//            }
//          } else {
//            assert(rhs_ys.low() == rhs_ys.high());
//            res.first = next_var->minus(rhs_ys.low());
//            res.second = next_var->minus(rhs.low());
//            lhs += ys;
//          }
//
//          a->init_indices(res.first, res.second, k, track);
//          // end copy and paste
//
//          std::cerr << " (left = " << *res.first << ", right = " << *res.second << ") ";
//        } else if (a->is(Alt::SIMPLE)) {
//          std::cerr << " : Alt::Simple " << *(dynamic_cast<Alt::Simple*>(a)->name);
//        }
//      } else if (*i_type == FN_ARG) {
//        Fn_Arg::Base *f = static_cast<Fn_Arg::Base*>(*ir);
//        std::cerr << " : Fn_Arg";
//        //std::cerr << (*f).left_indices.size() << " <- Fn_Arg\n";
//        //continue;
//      } else if (*i_type == SYMBOL) {
//        Symbol::Base *s = static_cast<Symbol::Base*>(*ir);
//        std::cerr << " : Symbol";
//        //std::cerr << (*s).left_indices.size() << " <- Symbol\n";
////        accum_ys_center_to_left += (*s).multi_ys()(track);
//      }
//
//      std::cerr << "\n";
//    }

    if ((_left.size() == 0) && (_right.size() == 0) && _outsideNT) {
      if (_outsideNT->is_outside_inside_transition()) {
        _outsideNT->Base::init_indices(left->plus(right), right->minus(left), k, track);
      } else {
        // must be a direct link to an non-terminal
        _outsideNT->Base::init_indices(left, right, k, track);
      }
    }
  }

  Yield::Size sum_ys_left(void* start, void* end, size_t track) {
    Yield::Size ys;

    bool seen_start = false;
    if (start == nullptr) {
      seen_start = true;
    }
    std::vector<Type>::iterator i_type = _left_types.begin();
    for (std::vector<void*>::iterator i = _left.begin(); i != _left.end(); ++i, ++i_type) {
      if (*i == start) {
        seen_start = true;
      }
      if (seen_start) {
        if (*i_type == ALT) {
          Alt::Base *a = static_cast<Alt::Base*>(*i);
          if (a->is(Alt::LINK)) {
            ys += (*a).multi_ys()(track);
          }
        } else if (*i_type == SYMBOL) {
          Symbol::Base *s = static_cast<Symbol::Base*>(*i);
          ys += (*s).multi_ys()(track);
        }
      }
      if (*i == end) {
        break;
      }
    }

    return ys;
  }
  Yield::Size sum_ys_right(void* start, void* end, size_t track) {
    Yield::Size ys;

    bool seen_start = false;
    if (start == nullptr) {
      seen_start = true;
    }
    std::vector<Type>::iterator i_type = _right_types.begin();
    for (std::vector<void*>::iterator i = _right.begin(); i != _right.end(); ++i, ++i_type) {
      if (*i == start) {
        seen_start = true;
      }
      if (seen_start) {
        if (*i_type == ALT) {
          Alt::Base *a = static_cast<Alt::Base*>(*i);
          if (a->is(Alt::LINK)) {
            ys += (*a).multi_ys()(track);
          }
        } else if (*i_type == SYMBOL) {
          Symbol::Base *s = static_cast<Symbol::Base*>(*i);
          ys += (*s).multi_ys()(track);
        }
      }
      if (*i == end) {
        break;
      }
    }

    return ys;
  }

  Init_Indices_Outside() : num_outside_nts(0) {
  }

  void visit(Symbol::Terminal &s) {
    stack_component(&s, SYMBOL);
  }
  void visit(Symbol::NT &s) {
    stack_component(&s, SYMBOL);
  }
  void visit(Alt::Link &a) {
    if (a.nt->is_partof_outside() || a.is_outside_inside_transition()) {
      num_outside_nts++;
      std::cerr << "gefunden: " << *a.nt->name << "\n";
      /* drop the link itself as it has been previously added via the
       * visit(Alt::Base) call */
      _left.pop_back();
      _left_types.pop_back();
      _outsideNT = &a;
    }
  }
  void visit(Alt::Base &a) {
    stack_component(&a, ALT);
  }
  void visit(Fn_Arg::Base &f) {
    stack_component(&f, FN_ARG);
  }
};

#endif /* SRC_OUTSIDE_MIDDLE_END_HH_ */
