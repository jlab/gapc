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
typedef std::tuple<std::string, std::vector<unsigned int>, double > Trace;
/* Since a candidate can be composed of multiple sub-solutions e.g. 
 * mult(A, '*', B), we collect Traces for A and B */
typedef std::tuple<double, std::vector<Trace> > NTtrace;
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

inline
bool is_same_index(std::vector<unsigned int> a, std::vector<unsigned int> b) {
  if (a.size() != b.size()) {
    return false;
  }
  std::vector<unsigned int>::const_iterator bi = b.begin();
  for (std::vector<unsigned int>::const_iterator ai = a.begin();
       ai != a.end(); ++ai, ++bi) {
    if (*ai != *bi) {
      return false;
    }
  }
  return true;
}

class candidate {
 private:
  double value;
  std::vector<Trace> sub_components;

 public:
  explicit candidate(double value) {
    this->value = value;
  }

  candidate() {
    this->value = 0.0;
  }

  void set_value(double value) {
    this->value = value;
  }

  void add_sub_component(std::string otherNT,
                         std::vector<unsigned int> *indices) {
    sub_components.push_back({otherNT, *indices, NULL});
  }

  std::vector<Trace> *get_sub_components() {
    return &sub_components;
  }

  double get_value() const {
    return value;
  }

  std::vector<Trace> get_normalized_candidate(double eval) const {
    std::vector<Trace> res;
    for (std::vector<Trace>::const_iterator part = this->sub_components.begin();
         part != this->sub_components.end(); ++part) {
      res.push_back({std::get<0>(*part), std::get<1>(*part),
                     this->get_value() / eval});
    }
    return res;
  }
//  void empty() {
//    empty(this->value);
//  }
};
typedef std::vector<candidate> NTtraces;

// once all use of sub-solutions for candidates is finished, we need to
// normalize their contributions
inline
std::vector<Trace> normalize_traces(std::vector<Trace> *tabulated,
                                    const std::vector<candidate> &candidates,
                                    double eval) {
  std::vector<std::tuple<std::string, std::vector<unsigned int>, double > > res;
  for (std::vector<candidate>::const_iterator i = candidates.begin();
       i != candidates.end(); ++i) {
    std::vector<Trace> comp = (*i).get_normalized_candidate(eval);
    res.insert(res.end(), comp.begin(), comp.end());
  }
  return res;
}

inline
double get_trace_weights(const std::vector<Trace> &traces,
                         const std::string &to_nt,
                         const std::vector<unsigned int> &to_indices,
                         double e) {
  double res = 0.0;
  for (std::vector<Trace>::const_iterator trace = traces.begin();
       trace != traces.end(); ++trace) {
    // TODO(sjanssen): move both conditions into is_same_index
    if (is_same_index(std::get<1>(*trace), to_indices) &&
        (std::get<0>(*trace) == to_nt)) {
      res += e * std::get<2>(*trace);
    }
  }

  return res;
}

// template<typename answer>
// using NTtraces = typename std::vector<candidate<answer> >::NTtraces;
/* collection of all traces of an NT in forward pass, together with their
 * values. This later normalized into edge weights. */
/* collection of all traces of an NT in forward pass, together with their
 * values. This later normalized into edge weights. */
// typedef std::vector<candidate<double> > NTtraces;

#endif  // RTLIB_TRACES_HH_
