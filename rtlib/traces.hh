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

// efficiently stores NT table indices for tracing
class index_components {
 public:
  unsigned int n;
  unsigned int indices[3];

  index_components() : n(0) {}

  index_components(unsigned int n, unsigned int i) : n(n) {
    indices[0] = i;
  }

  index_components(unsigned int n, unsigned int i,
                   unsigned int j) : n(n) {
    indices[0] = i;
    indices[1] = j;
  }

  index_components(unsigned int n, unsigned int i,
                   unsigned int j, unsigned int k) : n(n) {
    indices[0] = i;
    indices[1] = j;
    indices[2] = k;
  }
};

inline bool operator==(const index_components &lhs,
                       const index_components &rhs) {
  if (lhs.n != rhs.n) {
    return false;
  }

  bool equal = true;
  for (unsigned int i = 0; i < lhs.n; ++i) {
    equal &= lhs.indices[i] == rhs.indices[i];
  }

  return equal;
}

inline bool is_same_index(const index_components &lhs,
                          const index_components &rhs) {
  return lhs == rhs;
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
// typedef std::vector<NTtrace> NTtraces;


/* create a vector of index components (one or multitrack, linear or quadratic
 * tables) for a variable number of components therefore, the first argument
 * must be the number of those components!
 * https://stackoverflow.com/questions/1579719/variable-number-of-parameters\
 * -in-function-in-c */
inline
std::vector<unsigned int> *make_index(unsigned int num_components, ...) {
  std::vector<unsigned int> *indices = new std::vector<unsigned int>();
  // Requires the last fixed parameter (to get the address)
  va_list idx_components;
  va_start(idx_components, num_components);
  for (unsigned int j = 0; j < num_components; j++) {
    indices->push_back(va_arg(idx_components, unsigned int));
  }
  va_end(idx_components);

  return indices;
}

inline bool is_same_index(const std::vector<unsigned int> &a,
                          const std::vector<unsigned int> &b) {
  return a == b;
}

template<typename answer = double>
class candidate {
 private:
  answer value;
  answer q;
  std::vector<Trace<answer>> sub_components;

 public:
  candidate() = default;
  explicit candidate(answer value) : value(value) {}

  void set_value(answer value) {
#ifdef PYTORCH_MOD
    this->value = clone(value);
#else
    this->value = value;
#endif
  }

  void set_q(answer q) {
    this->q = q;
  }

  void add_sub_component(std::string &&otherNT,
                         index_components &&indices) {
#ifdef PYTORCH_MOD
    sub_components.emplace_back(otherNT, indices,
                                tensor_from_scalar(0.0, BATCH_SIZE));
#else
    sub_components.emplace_back(otherNT, indices, answer());
#endif
  }

  std::vector<Trace<answer>> *get_sub_components() {
    return &sub_components;
  }

  answer get_value() const {
    return value;
  }

  answer get_q() const {
    return q;
  }

  /* we need to normalize the individual trace values into probabilities
   * trace values can come in different flavors, depending on what the
   * user uses as scoring schema. See Algebra::check_derivative
   */
  std::vector<Trace<answer>>
  get_normalized_candidate(answer eval,
                           answer (func)(answer a, answer b)) const {
    const std::vector<Trace<answer>> &sub_comp = this->sub_components;
    std::vector<Trace<answer>> res(sub_comp.size());

    for (size_t i = 0; i < sub_comp.size(); ++i) {
      res[i] = {std::get<0>(sub_comp[i]), std::get<1>(sub_comp[i]),
                func(this->get_value() , eval)};
    }

    return res;
  }

  std::vector<Trace<answer>> get_soft_max_hessian_candidate(answer eval) const {
    const std::vector<Trace<answer>> &sub_comp = this->sub_components;
    std::vector<Trace<answer>> res(sub_comp.size());

    for (size_t i = 0; i < sub_comp.size(); ++i) {
      res[i] = {std::get<0>(sub_comp[i]), std::get<1>(sub_comp[i]),
                this->get_q() - (eval * this->get_value())};
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
inline std::vector<Trace<answer>>
normalize_traces(std::vector<Trace<answer>> *tabulated,
                 const std::vector<candidate<answer>> &candidates,
                 answer eval, answer (func)(answer a, answer b)) {
  std::vector<Trace<answer>> res;

  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<answer>> comp = candidates[i].
                                        get_normalized_candidate(eval, func);
    res.insert(res.end(), comp.begin(), comp.end());
  }

  return res;
}

template<typename answer = double>
inline std::vector<Trace<answer>>
soft_max_hessian_product(std::vector<Trace<answer>> *tabulated,
                         const NTtraces<answer> &candidates, answer eval) {
  std::vector<Trace<answer>> res;

  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<answer>> comp = candidates[i].
                                        get_soft_max_hessian_candidate(eval);
    res.insert(res.end(), comp.begin(), comp.end());
  }

  return res;
}

inline
double get_trace_weights(const std::vector<Trace<>> &traces,
                         const std::string &to_nt,
                         const index_components &to_indices,
                         double e) {
  double res = 0.0;
  for (std::vector<Trace<>>::const_iterator trace = traces.begin();
       trace != traces.end(); ++trace) {
    // TODO(sjanssen): move both conditions into is_same_index
    if (is_same_index(std::get<1>(*trace), to_indices) &&
        (std::get<0>(*trace) == to_nt)) {
      res += e * std::get<2>(*trace);
    }
  }

  return res;
}

#ifdef PYTORCH_MOD
inline
tensor get_trace_weights(const std::vector<Trace<tensor>> &traces,
                         const std::string &to_nt,
                         const std::vector<unsigned int> &to_indices,
                         tensor e) {
  tensor res = torch::zeros(BATCH_SIZE);

  for (std::vector<Trace<tensor>>::const_iterator trace = traces.begin();
       trace != traces.end(); ++trace) {
    if (is_same_index(std::get<1>(*trace), to_indices) &&
        (std::get<0>(*trace) == to_nt)) {
      res += e * std::get<2>(*trace);
    }
  }

  return res;
}
#endif

// template<typename answer>
// using NTtraces = typename std::vector<candidate<answer> >::NTtraces;
/* collection of all traces of an NT in forward pass, together with their
 * values. This later normalized into edge weights. */
/* collection of all traces of an NT in forward pass, together with their
 * values. This later normalized into edge weights. */
// typedef std::vector<candidate<double> > NTtraces;

#endif  // RTLIB_TRACES_HH_
