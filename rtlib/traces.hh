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

/* this records the use of a sub-solution of another NT result <name of other 
 * NT, index in other NT table with variable number of dimensions, weight 
 * of edge> */
template<typename T = double>
using Trace = std::tuple<std::string, std::vector<unsigned int>, T>;
/* Since a candidate can be composed of multiple sub-solutions e.g. 
 * mult(A, '*', B), we collect Traces for A and B */
template<typename T = double>
using NTtrace = std::tuple<T, std::vector<Trace<T>>>;
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

template<typename T = double>
class candidate {
 private:
  T value;
  T q;
  std::vector<Trace<T>> sub_components;

 public:
  candidate() = default;
  explicit candidate(T value) : value(value) {}

  void set_value(T value) {
    this->value = value;
  }

  void set_q(T q) {
    this->q = q;
  }

  void add_sub_component(const std::string &otherNT,
                         std::vector<unsigned int> *indices) {
    sub_components.push_back({otherNT, *indices, NULL});
  }

  std::vector<Trace<T>> *get_sub_components() {
    return &sub_components;
  }

  T get_value() const {
    return value;
  }

  T get_q() const {
    return q;
  }

  /* we need to normalize the individual trace values into probabilities
   * trace values can come in different flavors, depending on what the
   * user uses as scoring schema. See Algebra::check_derivative
   */
  std::vector<Trace<T>>
  get_normalized_candidate(T eval, T (func)(T a, T b)) const {
    const auto &sub_comp = this->sub_components;
    std::vector<Trace<T>> res(sub_comp.size());

    for (size_t i = 0; i < sub_comp.size(); ++i) {
      res[i] = {std::get<0>(sub_comp[i]), std::get<1>(sub_comp[i]),
                func(this->get_value() , eval)};
    }

    return res;
  }

  std::vector<Trace<T>> get_soft_max_hessian_candidate(T eval) const {
    const auto &sub_comp = this->sub_components;
    std::vector<Trace<T>> res(sub_comp.size());

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

template<typename T = double>
using NTtraces = std::vector<candidate<T>>;

// once all use of sub-solutions for candidates is finished, we need to
// normalize their contributions
template<typename T = double>
inline std::vector<Trace<T>>
normalize_traces(std::vector<Trace<T>> *tabulated,
                 const std::vector<candidate<T>> &candidates,
                 T eval, T (func)(T a, T b)) {
  std::vector<std::tuple<std::string, std::vector<unsigned int>, T > > res;

  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<T>> comp = candidates[i].
                                   get_normalized_candidate(eval, func);
    res.insert(res.end(), comp.begin(), comp.end());
  }

  return res;
}

template<typename T = double>
inline std::vector<Trace<T>>
soft_max_hessian_product(std::vector<Trace<T>> *tabulated,
                         const std::vector<candidate<T>> &candidates, T eval) {
  std::vector<std::tuple<std::string, std::vector<unsigned int>, T > > res;

  for (size_t i = 0; i < candidates.size(); ++i) {
    std::vector<Trace<T>> comp = candidates[i].
                                   get_soft_max_hessian_candidate(eval);
    res.insert(res.end(), comp.begin(), comp.end());
  }

  return res;
}

inline
double get_trace_weights(const std::vector<Trace<>> &traces,
                         const std::string &to_nt,
                         const std::vector<unsigned int> &to_indices,
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
  tensor res = torch::zeros(e.sizes());
  std::vector<Trace<tensor>>::const_iterator trace = traces.begin();
  for (; trace != traces.end(); ++trace) {
    // TODO(sjanssen): move both conditions into is_same_index
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
