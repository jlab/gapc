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

    * author: fymue
    * contains wrapper types for torch::Tensor objects, which are used
    * when generating a pytorch module from GAPC derivative code;
    * there are two wrapper types:
    *   - TensorSlice (analogous to "Subsequence" type)
    *   - TensorChar  (analogous to "char" type)
    * these types are essentially drop-in replacements of "Subsequence"
    * and "char" in the generated code, which takes Tensors as input
    * instead of strings/sequences
}}} */

#ifndef RTLIB_TENSOR_HH_
#define RTLIB_TENSOR_HH_

#include "torch/extension.h"

using tensor = torch::Tensor;

template<typename pos_type = unsigned int>
class TensorSlice {
 public:
  pos_type i, j;
  const tensor *t;

  TensorSlice() : i(0), j(0), t(NULL) {}
  TensorSlice(
    const tensor &t, pos_type i, pos_type j) : t(&t), i(i), j(j) {}
  
  void empty() {
    t = NULL;
  }

  bool isEmpty() const {
    return !t;
  }

  pos_type size() const {
    return j - i;
  }
};

template<typename pos_type = unsigned int>
class TensorChar {
 public:
  pos_type i;
  const tensor *t;

  TensorChar() : i(0), t(NULL) {}
  TensorChar(const tensor &t, pos_type i) : t(&t), i(i) {}
  
  void empty() {
    t = NULL;
  }

  bool isEmpty() const {
    return !t;
  }

  constexpr pos_type size() const {
    return 1;
  }
};

#endif  // RTLIB_TENSOR_HH_