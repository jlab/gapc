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

#include "mode.hh"
#include <string>

const Mode::Number Mode::map_type_to_number[] = {
  ZERO,  // NONE
  ONE,  // SYNOPTIC
  MANY,  // PRETTY
  MANY,  // CLASSIFY
  ONE,  // SCORING
  MANY,  // KSCORING
};

const Mode::pair Mode::map_string_to_mode[] = {
  { "none", NONE },
  { "synoptic", SYNOPTIC },
  { "pretty", PRETTY },
  { "classify", CLASSIFY },
  { "scoring", SCORING },
  { "kscoring", KSCORING },
  { "stringrep", PRETTY },
  { NULL, NONE }
};

hashtable<std::string, Mode::Type> Mode::table;

void Mode::set(Mode::Type t) {
  number = map_type_to_number[t] > ONE ? Yield::Poly(Yield::UP) :
    Yield::Poly(static_cast<uint32_t>(map_type_to_number[t]));
  type = t;
}

void Mode::init_table() {
  for (unsigned int i = 0; map_string_to_mode[i].a != NULL; i++) {
    table[map_string_to_mode[i].a] = map_string_to_mode[i].b;
  }
}

bool Mode::set(const std::string &name) {
  assert(table.size());
  hashtable<std::string, Type>::iterator i = table.find(name);
  if (i == table.end())
    return false;
  set(i->second);
  return true;
}

std::ostream &Mode::put(std::ostream &s) const {
  assert(type == map_string_to_mode[type].b);
  s << map_string_to_mode[type].a << '(' << number << ')';
  return s;
}
