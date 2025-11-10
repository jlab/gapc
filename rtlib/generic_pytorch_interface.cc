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

    * Author: fymue
    * Generic interface that allows GAPC derivative code to be compiled
    * to a Python module exposing a forward and backward function
    * for every derivative;
    * requires the Pytorch C++ Extension API and uses pybind11 to compile
    * the GAPC forward and backward functions to Python functions

}}} */

#include <iostream>
#include <cassert>
#include <vector>
#include <utility>
#include "rtlib/generic_opts.hh"
#include "torch/extension.h"

// answer type will be defined in out.hh, which gets
// included into this file during the make process
#ifndef ANSWER_TYPE
#define ANSWER_TYPE double
#endif

/*
 * the number of input parameters of the forward
 * functions depend on the GAP-L input<> declaration;
 * to ensure that the correct number of inputs are
 * declared in the function signatures,
 * the macro "INPUT_PARAMS" will be defined in
 * the included header file and insert the correct
 * number of input parameters into the forward
 * functions
 */
#if !defined(INPUT_PARAMS) && !defined(INPUT_ARGS)
using tensor = torch::Tensor;
#define INPUT_PARAMS tensor &inp_1, tensor &inp_2
#define INPUT_ARGS   inp_1, inp_2
#endif

// main objects need to be global so all functions have access to it
static gapc::class_name obj;
#ifdef SECOND_DERIVATIVE
static gapc::class_name_D2 obj_D2;
#endif

std::vector<torch::Tensor> forward_D1(INPUT_PARAMS) {
  // execute forward pass and return first derivative forward score matrices
  obj.init(INPUT_ARGS);
  obj.cyk();
  obj.run();

  return obj.get_forward_score_matrices();
}

std::vector<torch::Tensor> backward_D1() {
  // execute backward pass and return first derivative backward score matrices
  return obj.get_backward_score_matrices();
}

#ifdef SECOND_DERIVATIVE
std::vector<torch::Tensor> forward_D2(INPUT_PARAMS) {
  // execute forward pass and return second derivative forward score matrices
  obj_D2.init(INPUT_ARGS, &obj);
  obj_D2.run();

  return obj_D2.get_forward_score_matrices();
}

std::vector<torch::Tensor> backward_D2() {
  // execute backward pass and return second derivative backward score matrices
  return obj_D2.get_backward_score_matrices();
}
#endif

// pybind11 binding instructions
PYBIND11_MODULE(TORCH_EXTENSION_NAME, m) {
  m.doc() = (std::string("GAP-C call: \n") + GAPC_CALL_STRING).c_str();
  m.def("forward_D1", &forward_D1,
        "Calculate the 1st derivative score matrices for the forward pass");
  m.def("backward_D1", &backward_D1,
        "Calculate the 1st derivative score matrices for the backward pass.\n"
        "This function doesn't require any inputs since it uses data \n"
        " that gets internally created when calling 'forward_D1',\n"
        "so make sure to execute 'forward_D1' beforehand!");
#ifdef SECOND_DERIVATIVE
  m.def("forward_D2", &forward_D2,
        "Calculate the 2nd derivative score matrices for the forward pass.\n"
        "Make sure to execute 'forward_D1' and 'backward_D1' beforehand!");
  m.def("backward_D2", &backward_D2,
        "Calculate the 2nd derivative score matrices for the backward pass.\n"
        "This function doesn't require any inputs since it uses data \n"
        " that gets internally created when calling 'forward_D2',\n"
        "Make sure to execute 'forward_D1', 'backward_D1' and "
        "'forward_D2' beforehand!");
#endif
}
