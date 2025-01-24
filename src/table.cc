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


#include "table.hh"
#include <string>


void Table::print(std::ostream &s) const {
  s << '(';
  switch (dim) {
    case NONE : s << "none"; break;
    case CONSTANT : s << "const"; break;
    case LINEAR : s << "linear"; break;
    case QUADRATIC : s << "quadratic"; break;
  }

  if (dim == LINEAR && sticky_) {
    s << ',';
    switch (sticky_) {
      case LEFT : s << "left(" << is_cyk_left() ; break;
      case RIGHT : s << "right(" << is_cyk_right() ; break;
      default: assert(0);
    }
    s << ')';
  }
  if (dim == CONSTANT || (dim == LINEAR && sticky_ == LEFT))
    s << ",left:" << left_rest_;
  if (dim == CONSTANT || (dim == LINEAR && sticky_ == RIGHT))
    s << ",right:" << right_rest_;
  s << ')';
}

void Table::print_short(std::ostream &s, const std::string &name) const {
  switch (type()) {
    case Table::CONSTANT : s << '%' << name << '%'; break;
    case Table::LINEAR : s << '%' << name ; break;
    case Table::QUADRATIC : s << name ; break;
    case Table::NONE: s << name << "(NONE)"; break;
  }
  if (bounded())
    s << "<<";
}

bool Table::delete_left_index() const {
  return (dim == CONSTANT || (dim == LINEAR && sticky_ == LEFT)) &&
    left_rest_.high() == 0;
}
bool Table::delete_right_index() const {
  return (dim == CONSTANT || (dim == LINEAR && sticky_ == RIGHT)) &&
    right_rest_.high() == 0;
}

void Table::set_sticky(Sticky s) {
  if (sticky_ != NO_INDEX && sticky_ != s) {
    dim = QUADRATIC;
    sticky_ = NO_INDEX;
    right_rest_ = Yield::Size();
    left_rest_ = Yield::Size();
    return;
  }
  sticky_ = s;
}
