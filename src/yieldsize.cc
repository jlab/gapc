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

#include "yieldsize.hh"
#include <list>
#include <vector>

#include "filter.hh"

namespace Yield {

void Multi::put(std::ostream &s) const {
  assert(!array.empty());
  s << '{';
  std::vector<Size>::const_iterator i = array.begin();
  s << *i;
  ++i;
  for (; i != array.end(); ++i)
    s << ", " << *i;
  s << '}';
}

void Size::with_min(const Filter &f) {
  if (f.type != Filter::WITH || !f.is(Filter::MIN_SIZE))
    return;
  uint32_t l = f.uint_arg();
  if (min < Poly(l))
    min = Poly(l);
}
void Size::with_max(const Filter &f) {
  if (f.type != Filter::WITH || !f.is(Filter::MAX_SIZE))
    return;
  uint32_t l = f.uint_arg();
  if (max > l)
    max = l;
}
void Size::with(const std::list<Filter*> &l) {
  for (std::list<Filter*>::const_iterator i = l.begin(); i != l.end(); ++i) {
    with_min(**i);
    with_max(**i);
  }
}

void Multi::with(const std::vector<std::list<Filter*> > &l) {
  if (l.empty())
    return;
  [[maybe_unused]] size_t a = array.size(), b = l.size();
  assert(a == b);
  std::vector<std::list<Filter*> >::const_iterator j = l.begin();
  for (std::vector<Size>::iterator i = array.begin(); i != array.end();
      ++i, ++j)
    (*i).with(*j);
}

bool Multi::is_low_zero() const {
  bool r = true;
  for (std::vector<Size>::const_iterator i = array.begin(); i != array.end();
      ++i) {
    r = r && ((*i).low() == 0);
  }
  return r;
}

void Multi::min_high(const Multi &o) {
  [[maybe_unused]] size_t a = array.size(), b = o.array.size();
  assert(a == b);
  std::vector<Size>::const_iterator j = o.array.begin();
  for (std::vector<Size>::iterator i = array.begin(); i != array.end();
      ++i, ++j)
    if ((*j).high() < (*i).high())
      (*i).set_high((*j).high());
}
void Multi::max_high(const Multi &o) {
  [[maybe_unused]] size_t a = array.size(), b = o.array.size();
  assert(a == b);
  std::vector<Size>::const_iterator j = o.array.begin();
  for (std::vector<Size>::iterator i = array.begin(); i != array.end();
      ++i, ++j)
    if ((*i).high() < (*j).high())
      (*i).set_high((*j).high());
}
void Multi::sub_high_low(const Multi &o) {
  [[maybe_unused]] size_t a = array.size(), b = o.array.size();
  assert(a == b);
  std::vector<Size>::const_iterator j = o.array.begin();
  for (std::vector<Size>::iterator i = array.begin(); i != array.end();
      ++i, ++j)
    if ((*i).high() != Poly(UP))
      (*i).high() -= (*j).low();
}

bool Multi::leq_high(const Multi &o) const {
  bool r = false;
  [[maybe_unused]] size_t a = array.size(), b = o.array.size();
  assert(a == b);
  std::vector<Size>::const_iterator j = o.array.begin();
  for (std::vector<Size>::const_iterator i = array.begin(); i != array.end();
      ++i, ++j)
    r = r || ((*i).high() < (*j).high()) || ((*i).high() == (*j).high());
  return r;
}

bool Multi::has_moving() const {
  for (std::vector<Size>::const_iterator i = array.begin(); i != array.end();
       ++i)
    if ((*i).low() != (*i).high())
      return true;
  return false;
}

}  // namespace Yield
