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


#ifndef RTLIB_TRACES_HH_
#define RTLIB_TRACES_HH_

#include <vector>
#include <tuple>
#include <cstdarg>
#include <string>
#include <utility>
#include <cassert>
#include <array>

// max. number of indices that need to be tracked gets determined
// at compile-time and stored in MAX_INDEX_COMPONENTS in out.hh
#ifndef MAX_INDEX_COMPONENTS
#define MAX_INDEX_COMPONENTS 2
#endif

// efficiently stores NT table indices for tracing
class index_components {
 private:
  std::array<unsigned int, MAX_INDEX_COMPONENTS> indices;

 public:
  index_components() = default;

  explicit index_components(unsigned int i) : indices{} {
    indices[0] = i;
  }

  index_components(unsigned int i, unsigned int j) : indices{} {
    indices[0] = i;
    indices[1] = j;
  }

  template<typename ...Ts>
  explicit index_components(const Ts&&... values) : indices{values...} {}

  friend inline bool operator==(const index_components &lhs,
                                const index_components &rhs) {
    return lhs.indices == rhs.indices;
  }
};

inline bool is_same_index(const index_components &lhs_idx,
                          const index_components &rhs_idx,
                          const std::string &lhs_nt,
                          const std::string &rhs_nt) {
  return lhs_idx == rhs_idx && lhs_nt == rhs_nt;
}

/* this records the use of a sub-solution of another NT result <name of other 
 * NT, index in other NT table with variable number of dimensions, weight 
 * of edge> */
template<typename answer = double>
using Trace = std::tuple<std::string, index_components, answer>;
/* Since a candidate can be composed of multiple sub-solutions e.g. 
 * mult(A, '*', B), we collect Traces for A and B */
template<typename answer = double>
using NTtrace = std::tuple<answer, std::vector<Trace<answer>>>;
/* collection of all traces of an NT in forward pass, together with their
 * values. This later normalized into edge weights. */

// type for "traces" vector in NT tables
template<typename answer = double>
using Traces = std::vector<Trace<answer>>;

// answer type can be any primitve number type or "Batch"
template<typename answer = double>
class candidate {
 private:
  answer value;
  answer q;
  std::vector<Trace<answer>> sub_components;

 public:
  candidate() : value(0.0), q(0.0) {}
  explicit candidate(answer value) : value(value), q(0.0) {}

  void set_value(answer value) {
#if defined(PYTORCH_MOD) && defined(BATCHED_INPUT)
    // need to copy batch data here
    this->value = answer(value.batch->data);
#else
    this->value = value;
#endif
  }

  void set_q(answer q) {
#if defined(PYTORCH_MOD) && defined(BATCHED_INPUT)
    // need to copy batch data here
    this->q = answer(q.batch->data);
#else
    this->q = q;
#endif
  }

  void add_sub_component(std::string &&otherNT,
                         index_components &&indices) {
    sub_components.emplace_back(std::move(otherNT),
                                std::move(indices), answer());
  }

  std::vector<Trace<answer>> *get_sub_components() {
    return &sub_components;
  }

  inline answer get_value() const {
    return value;
  }

  inline answer get_q() const {
    return q;
  }

  /* we need to normalize the individual trace values into probabilities
   * trace values can come in different flavors, depending on what the
   * user uses as scoring schema. See Algebra::check_derivative
   */
  std::vector<Trace<answer>> get_normalized_candidate(
    answer eval, answer (func)(answer a, answer b)) const {
    // copy sub_components content into res vector
    std::vector<Trace<answer>> res(this->sub_components);

    // calculate func result
    answer val = func(this->get_value() , eval);

    // add calculated result to every Trace of res vector
    for (size_t i = 0; i < res.size(); ++i) {
      std::get<2>(res[i]) = val;
    }

    return res;
  }

  // overload of get_normalized_candidate taking a function ptr argument
  // with two const reference parameters
  std::vector<Trace<answer>> get_normalized_candidate(
    answer eval, answer (func)(const answer &a, const answer &b)) const {
    // copy sub_components content into res vector
    std::vector<Trace<answer>> res(this->sub_components);

    // calculate func result
    answer val = func(this->get_value() , eval);

    // add calculated result to every Trace of res vector
    for (size_t i = 0; i < res.size(); ++i) {
      std::get<2>(res[i]) = val;
    }

    return res;
  }

  std::vector<Trace<answer>> get_soft_max_hessian_candidate(
    answer eval) const {
    // copy sub_components content into res vector
    std::vector<Trace<answer>> res(this->sub_components);

    // calculate func result
    answer val = this->get_q() - (eval * this->get_value());

    // add calculated result to every Trace of res vector
    for (size_t i = 0; i < res.size(); ++i) {
      std::get<2>(res[i]) = val;
    }

    return res;
  }

// void empty() {
//   empty(this->value);
// }
};

template<typename answer = double>
using NTtraces = std::vector<candidate<answer>>;

// once all use of sub-solutions for candidates is finished, we need to
// normalize their contributions
template<typename answer = double>
void normalize_traces(std::vector<Trace<answer>> *tabulated,
                      const std::vector<candidate<answer>> &candidates,
                      answer eval,
                      answer (func)(answer a, answer b)) {
  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<answer>> comp =
      candidates[i].get_normalized_candidate(eval, func);
    std::move(comp.begin(), comp.end(), std::back_inserter(*tabulated));
  }
}

// overload of normalize_traces taking a function ptr argument
// with two const reference parameters
template<typename answer = double>
void normalize_traces(std::vector<Trace<answer>> *tabulated,
                      const std::vector<candidate<answer>> &candidates,
                      answer eval,
                      answer (func)(const answer &a, const answer &b)) {
  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<answer>> comp =
      candidates[i].get_normalized_candidate(eval, func);
    std::move(comp.begin(), comp.end(), std::back_inserter(*tabulated));
  }
}

template<typename answer = double>
void soft_max_hessian_product(std::vector<Trace<answer>> *tabulated,
                              const NTtraces<answer> &candidates, answer eval) {
  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<answer>> comp =
      candidates[i].get_soft_max_hessian_candidate(eval);
    std::move(comp.begin(), comp.end(), std::back_inserter(*tabulated));
  }
}

template<typename answer = double>
inline answer get_trace_weights(const std::vector<Trace<answer>> &traces,
                                const std::string &to_nt,
                                const index_components &to_indices,
                                answer e) {
  answer res = 0.0;
  for (size_t trace = 0; trace < traces.size(); ++trace) {
    const Trace<answer> &curr_trace = traces[trace];
    if (is_same_index(std::get<1>(curr_trace), to_indices,
                      std::get<0>(curr_trace), to_nt)) {
      res += e * std::get<2>(curr_trace);
    }
  }

  return res;
}

#endif  // RTLIB_TRACES_HH_
