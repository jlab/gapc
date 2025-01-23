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

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE vector_sparse
#include <utility>
#include <algorithm>
#include <boost/test/unit_test.hpp>
#include "macros.hh"

#include "../../rtlib/vector_sparse.hh"

struct cmp_stefan {
  bool operator()(const std::pair<char, int> &src,
                  const std::pair<char, int> &dst) const {
    return (src.second < dst.second);
  }
};

BOOST_AUTO_TEST_CASE(stapel) {
  Stapel<size_t> stack;
  stack.resize(3);
  stack.push(23);
  stack.push(42);
  stack.push(187);
  const size_t t[3] = { 23, 42, 187 };
  size_t o[3] = { 0 };
  size_t j = 0;
  for (Stapel<size_t>::iterator i = stack.begin(); i != stack.end(); ++i, ++j)
    o[j] = *i;
  for (size_t i = 0; i < 3; ++i)
    CHECK_EQ(t[i], o[i]);
}

struct A {
  static size_t count;
  A() {
    ++count;
  }
  A(const A &a) {
    ++count;
  }
  ~A() {
    --count;
  }
};

size_t A::count = 0;

BOOST_AUTO_TEST_CASE(vector) {
  A a;
  Vector_Sparse<A> *vector = new Vector_Sparse<A>();
  vector->resize(4096);
  Vector_Sparse<A> &v = *vector;
  v.init(23, a);
  v.init(42, a);
  v.init(187, a);
  CHECK_EQ(A::count, 4u);
  delete vector;
  CHECK_EQ(A::count, 1u);
}

BOOST_AUTO_TEST_CASE(vector2) {
  Vector_Sparse<size_t> v;
  v.resize(4096);
  v.init(23, 46);
  v.init(42, 84);
  v.init(187, 374);
  CHECK_EQ(v(23), 2u*23u);
  CHECK_EQ(v(42), 2u*42u);
  CHECK_EQ(v(187), 2u*187u);
}

BOOST_AUTO_TEST_CASE(reverse) {
  Stapel<size_t> stack;
  stack.resize(3);
  stack.push(23);
  stack.push(42);
  stack.push(187);
  const size_t t[3] = { 187, 42, 23 };
  size_t o[3] = { 0 };
  size_t j = 0;
  for (Stapel<size_t>::reverse_iterator i = stack.rbegin(); i != stack.rend();
       ++i, ++j)
    o[j] = *i;
  for (size_t i = 0; i < 3; ++i)
    CHECK_EQ(t[i], o[i]);
}

BOOST_AUTO_TEST_CASE(vector_itr) {
  Vector_Sparse<size_t> v;
  v.resize(4096);
  v.init(23, 46);
  v.init(42, 84);
  v.init(187, 374);
  size_t j = 0;
  const size_t a[3] = { 46, 84, 374 };
  size_t t[3] = { 0 };
  for (Vector_Sparse<size_t>::iterator i = v.begin(); i != v.end(); ++i, ++j)
    t[j] = *i;
  for (size_t i = 0; i < 3; ++i)
    CHECK_EQ(a[i], t[i]);
}

BOOST_AUTO_TEST_CASE(sort) {
  // https://github.com/jlab/gapc/issues/40
  // SMJ: I found that sorting vector_sparce did yield wrong results if
  //      executed on OSX
  // fix: added missing iterator operators

  Vector_Sparse<std::pair<char, int>, unsigned int> array(19);

  std::pair<char, int> ret_0;
  ret_0.first = '[';
  ret_0.second = -650;
  array.init(0, ret_0);

  std::pair<char, int> ret_1;
  ret_1.first = '[';
  ret_1.second = -740;
  array.init(1, ret_1);

  std::pair<char, int> ret_2;
  ret_2.first = '[';
  ret_2.second = -370;
  array.init(2, ret_2);

  std::pair<char, int> ret_4;
  ret_4.first = '[';
  ret_4.second = -470;
  array.init(3, ret_4);

  std::pair<char, int> ret_5;
  ret_5.first = '[';
  ret_5.second = 200;
  array.init(4, ret_5);

  std::pair<char, int> ret_6;
  ret_6.first = '[';
  ret_6.second = 410;
  array.init(5, ret_6);

  std::pair<char, int> ret_7;
  ret_7.first = '[';
  ret_7.second = 80;
  array.init(6, ret_7);

  std::sort(array.begin(), array.end(), cmp_stefan());

  CHECK_EQ(array(0).second, -740);
  CHECK_EQ(array(1).second, -650);
  CHECK_EQ(array(2).second, -470);
  CHECK_EQ(array(3).second, -370);
  CHECK_EQ(array(4).second, 80);
  CHECK_EQ(array(5).second, 200);
  CHECK_EQ(array(6).second, 410);
}
