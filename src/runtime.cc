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


#include "runtime.hh"
#include <vector>

Runtime::Asm::Poly & Runtime::Asm::Poly::operator=(const Runtime::Poly &p) {
  if (p.is_exp()) {
    set_exp();
    return *this;;
  }
  if (!p.degree()) {
    if (!p[0]) {
      n = 0;
    } else {
      n = 1;
    }
  } else {
    n = p.degree() + 1;
  }
  return *this;
}

bool Runtime::Asm::Poly::operator==(const Runtime::Poly &p) const {
    if (is_exp() && p.is_exp())
      return true;
    if (is_exp() || p.is_exp())
      return false;
    if (!n && p == 0)
      return true;
    if (n == 1 && p[0])
      return true;
    return n-1 == p.degree();
}

Runtime::Poly::Poly(const Yield::Size &s)
  : exponential(false) {
  Yield::Poly p(s.high());
  if (s.low() < s.high())
    p -= s.low();
  if (p == 0)
    p = 1;
  init(p);
}

Runtime::Poly::Poly(const Yield::Multi &s)
  : exponential(false) {
  assert(s.tracks());

  Yield::Multi::const_iterator i = s.begin();
  Poly p(*i);
  ++i;
  for (; i != s.end(); ++i)
    p *= Poly(*i);
  *this = p;
}

Runtime::Poly::Poly(const Table &t)
  : exponential(false) {
  assert(t.type() != Table::NONE);

  Poly p;

  Poly x(t.up);
  switch (t.type()) {
    case Table::CONSTANT :
      p.set(1, 0);
      break;
    case Table::LINEAR :
      if (t.bounded())
        p = x;
      else
        p.set(1, 1);
      break;
    case Table::QUADRATIC :
      if (t.bounded())
        p = x * x;
      else
        p.set(1, 2);
      break;
    default: break;
  }

  if ((t.type() == Table::CONSTANT || t.type() == Table::LINEAR)
      && t.sticky() == Table::LEFT)
    p *= Poly(t.left_rest());
  if ((t.type() == Table::CONSTANT || t.type() == Table::LINEAR)
      && t.sticky() == Table::RIGHT)
    p *= Poly(t.right_rest());

  *this = p;
}

Runtime::Poly::Poly(const std::vector<Table> &t)
  : exponential(false) {
  Poly r(1);
  for (std::vector<Table>::const_iterator i = t.begin(); i != t.end(); ++i) {
    Poly p(*i);
    r *= p;
  }
  *this = r;
}

#include <cmath>

void Runtime::Poly::divide_by_n() {
  if (exponential)
    return;
  if (!n) {
    assert(coefficients[0]);
    double r = std::sqrt(static_cast<double>(coefficients[0]));
    coefficients[0] = r;
  }
  if (n > 0) {
    --n;
    coefficients.erase(coefficients.begin());
  } else {
    if (!coefficients[0]) {
      coefficients[0] = 1;
    }
  }
}


#include <sstream>

std::ostream &Runtime::Poly::put(std::ostream &out) const {
  // s.t. iomanip like setw work for the whole object
  std::ostringstream s;
  if (exponential) {
    s << "2^n";
    out << s.str();
    return out;
  }
  bool first = true;
  for (size_t i = n; i > 1; i--) {
    uint32_t c = coefficients[i];
    if (c) {
      if (!first)
        s << " + ";
      else
        first = false;
      if (c > 1)
        s << c;
      s << "n^" << i << ' ';
    }
  }
  if (n > 0 && coefficients[1]) {
    if (!first)
      s << " + ";
    if (coefficients[1] > 1)
      s << coefficients[1];
    s  << "n";
    first = false;
  }
  // n>=0 && is always true
  if (coefficients[0]) {
    if (!first)
      s << " + ";
    s << coefficients[0];
    first = false;
  }
  if (first)
    s << '0';
  out << s.str();
  return out;
}
