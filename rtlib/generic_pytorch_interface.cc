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

#include <iostream>
#include <cassert>
#include <vector>
#ifdef FLOAT_ACC
  #include <iomanip>
  #include <limits>
#endif

#include "rtlib/string.hh"
#include "rtlib/list.hh"
#include "rtlib/hash.hh"
#include "rtlib/asymptotics.hh"
#include "rtlib/generic_opts.hh"
#include "torch/extension.h"

// main objects need to be global so both functions have access
gapc::class_name obj;
#ifdef SECOND_DERIVATIVE
  gapc::class_name_D2 obj_D2;
#endif

/*
   for now, all functions need to be called with two
   input strings (just like out when compiling out_main.cc);
   but they can also take tensors/inputs directly from Python/
   Pytorch (see const torch::Tensor &inp placerholder argument)
*/

std::vector<torch::Tensor> forward_D1(const torch::Tensor &inp,
                                      const char *seq1, const char *seq2) {
  // execute forward pass and return first derivative forward score matrices
  obj.init(inp, seq1, seq2);
// #if defined(__SUNPRO_CC) && __SUNPRO_CC <= 0x5100
//   #warning Enable sync_with_stdio because of Sun CC 12 Compiler Bug
// #else
//   std::ios_base::sync_with_stdio(false);
// #endif
//   std::cin.tie(0);
// #ifdef FLOAT_ACC
//   std::cout << std::setprecision(FLOAT_ACC) << std::fixed;
// #endif
  gapc::add_event("start");
  obj.cyk();
  std::vector<torch::Tensor> forward_score_matrices;
  gapc::return_type ans = obj.run();
  obj.get_forward_score_matrices(forward_score_matrices);
  gapc::add_event("end_computation");
  return forward_score_matrices;
}

std::vector<torch::Tensor> backward_D1(const torch::Tensor &inp,
                                       const char *seq1, const char *seq2) {
  // execute backward pass and return first derivative backward score matrices
  std::vector<torch::Tensor> backward_score_matrices;
  obj.get_backward_score_matrices(backward_score_matrices);
  gapc::add_event("end");
  return backward_score_matrices;
}

#ifdef SECOND_DERIVATIVE
std::vector<torch::Tensor> forward_D2(const torch::Tensor &inp,
                                      const char *seq1, const char *seq2) {
  // execute forward pass and return second derivative backward score matrices
  obj_D2.init(inp, seq1, seq2, &obj);
  gapc::add_event("start second derivative");
  std::vector<torch::Tensor> forward_score_matrices;
  gapc::return_type ans = obj_D2.run();
  obj_D2.get_forward_score_matrices(forward_score_matrices);
  gapc::add_event("end_computation of second derivative");
  return forward_score_matrices;
}

std::vector<torch::Tensor> backward_D2(const torch::Tensor &inp,
                                       const char *seq1, const char *seq2) {
  // execute backward pass and return second derivative backward score matrices
  std::vector<torch::Tensor> backward_score_matrices;
  obj_D2.get_backward_score_matrices(backward_score_matrices);
  gapc::add_event("end_result of second derivative");
  return backward_score_matrices;
}
#endif

// pybind11 binding instructions
PYBIND11_MODULE(TORCH_EXTENSION_NAME, m) {
  m.def("forward_D1", &forward_D1,
        "Calculate the 1st derivative score matrix for the forward pass");
  m.def("backward_D1", &backward_D1,
        "Calculate the 1st derivative score matrix for the backward pass.\n"
        "Make sure to execute forward_D1 beforehand!");
#ifdef SECOND_DERIVATIVE
  m.def("forward_D2", &forward_D2,
        "Calculate the 2nd derivative score matrix for the forward pass.\n"
        "Make sure to execute forward_D1 and backward_D1 beforehand!");
  m.def("backward_D2", &backward_D2,
        "Calculate the 2nd derivative score matrix for the backward pass.\n"
        "Make sure to execute forward_D1, backward_D1 and "
        "forward_D2 beforehand!");
#endif
}

