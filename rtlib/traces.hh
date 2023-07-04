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

template<unsigned int N, typename ...T>
inline std::array<unsigned int, N> init_indices(const T&&... values) {
  return std::array<unsigned int, N>{values...};
}

// efficiently stores NT table indices for tracing
template<unsigned int N>
class index_components {
 private:
  unsigned int n;
  std::array<unsigned int, N> indices;

 public:
  index_components() : n(N) {}

  explicit index_components(unsigned int i) : n(N) {
    indices[0] = i;
  }

  index_components(unsigned int i, unsigned int j) : n(N) {
    indices[0] = i;
    indices[1] = j;
  }

  template<typename ...Ts>
 explicit index_components(const Ts&&... values)
    : n(N), indices(init_indices<N>(std::move(values...))) {}

  template<unsigned int SIZE>
  friend bool operator==(const index_components<SIZE> &lhs,
                         const index_components<SIZE> &rhs);
};

template<unsigned int N>
bool is_same_index(const index_components<N> &lhs_idx,
                          const index_components<N> &rhs_idx,
                          const std::string &lhs_nt,
                          const std::string &rhs_nt) {
  return lhs_idx == rhs_idx && lhs_nt == rhs_nt;
}

template<unsigned int N>
bool operator==(const index_components<N> &lhs,
                const index_components<N> &rhs) {
  if (lhs.n != rhs.n) {
      return false;
    }

    bool equal = true;
    for (unsigned int i = 0; i < lhs.n; ++i) {
      equal &= lhs.indices[i] == rhs.indices[i];
    }

  return equal;
}

/* this records the use of a sub-solution of another NT result <name of other 
 * NT, index in other NT table with variable number of dimensions, weight 
 * of edge> */
template<typename answer = double, unsigned int N = 2>
using Trace = std::tuple<std::string, index_components<N>, answer>;
/* Since a candidate can be composed of multiple sub-solutions e.g. 
 * mult(A, '*', B), we collect Traces for A and B */
template<typename answer = double, unsigned int N = 2>
using NTtrace = std::tuple<answer, std::vector<Trace<answer, N>>>;
/* collection of all traces of an NT in forward pass, together with their
 * values. This later normalized into edge weights. */

// type for "traces" vector in NT tables
template<typename answer = double, unsigned int N = 2>
using Traces = std::vector<Trace<answer, N>>;

// answer type can be any primitve number type or "Batch"
template<typename answer = double, unsigned int N = 2>
class candidate {
 private:
  answer value;
  answer q;
  std::vector<Trace<answer, N>> sub_components;

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
                         index_components<N> &&indices) {
    sub_components.emplace_back(std::move(otherNT),
                                std::move(indices), answer());
  }

  std::vector<Trace<answer, N>> *get_sub_components() {
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
  std::vector<Trace<answer, N>> get_normalized_candidate(
    answer eval, answer (func)(answer a, answer b)) const {
    // copy sub_components content into res vector
    std::vector<Trace<answer, N>> res(this->sub_components);

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
  std::vector<Trace<answer, N>> get_normalized_candidate(
    answer eval, answer (func)(const answer &a, const answer &b)) const {
    // copy sub_components content into res vector
    std::vector<Trace<answer, N>> res(this->sub_components);

    // calculate func result
    answer val = func(this->get_value() , eval);

    // add calculated result to every Trace of res vector
    for (size_t i = 0; i < res.size(); ++i) {
      std::get<2>(res[i]) = val;
    }

    return res;
  }

  std::vector<Trace<answer, N>> get_soft_max_hessian_candidate(
    answer eval) const {
    // copy sub_components content into res vector
    std::vector<Trace<answer, N>> res(this->sub_components);

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

template<typename answer = double, unsigned int N = 2>
using NTtraces = std::vector<candidate<answer, N>>;

// once all use of sub-solutions for candidates is finished, we need to
// normalize their contributions
template<typename answer = double, unsigned int N = 2>
inline std::vector<Trace<answer, N>>
normalize_traces(std::vector<Trace<answer, N>> *tabulated,
                 const std::vector<candidate<answer>> &candidates,
                 answer eval,
                 answer (func)(answer a, answer b)) {
  std::vector<Trace<answer, N>> res;

  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<answer, N>> comp = candidates[i].
                                        get_normalized_candidate(eval, func);
    res.insert(res.end(), comp.begin(), comp.end());
  }

  return res;
}

// overload of normalize_traces taking a function ptr argument
// with two const reference parameters
template<typename answer = double, unsigned int N = 2>
inline std::vector<Trace<answer, N>>
normalize_traces(std::vector<Trace<answer, N>> *tabulated,
                 const std::vector<candidate<answer>> &candidates,
                 answer eval,
                 answer (func)(const answer &a, const answer &b)) {
  std::vector<Trace<answer, N>> res;

  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<answer, N>> comp = candidates[i].
                                        get_normalized_candidate(eval, func);
    res.insert(res.end(), comp.begin(), comp.end());
  }

  return res;
}

template<typename answer = double, unsigned int N = 2>
inline std::vector<Trace<answer, N>>
soft_max_hessian_product(std::vector<Trace<answer, N>> *tabulated,
                         const NTtraces<answer> &candidates,
                         answer eval) {
  std::vector<Trace<answer, N>> res;

  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<answer, N>> comp = candidates[i].
                                        get_soft_max_hessian_candidate(eval);
    res.insert(res.end(), comp.begin(), comp.end());
  }

  return res;
}

template<typename answer = double, unsigned int N = 2>
inline answer get_trace_weights(const std::vector<Trace<answer, N>> &traces,
                                const std::string &to_nt,
                                const index_components<N> &to_indices,
                                answer e) {
  answer res = 0.0;
  for (size_t trace = 0; trace < traces.size(); ++trace) {
    const Trace<answer, N> &curr_trace = traces[trace];
    if (is_same_index(std::get<1>(curr_trace), to_indices,
                      std::get<0>(curr_trace), to_nt)) {
      res += e * std::get<2>(curr_trace);
    }
  }

  return res;
}

#endif  // RTLIB_TRACES_HH_
