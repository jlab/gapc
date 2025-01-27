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


#include "input.hh"
#include <list>
#include <string>

#include "log.hh"


const char Input::map[][6] = {
  "raw",
  "rna",
  "upper"
};


void Input::set(const std::string &s, const Loc &l) {
  assert(modes_.empty());
  modes_.push_back(str_to_mode(s, l));
}


Input::Mode Input::str_to_mode(const std::string &s, const Loc &l) {
  for (size_t i = 0; i <= MODE_END; ++i) {
    if (map[i] == s) {
      return Mode (i);
    }
  }

  Log::instance()->error(l, "Unknown input mode: " + s +
    "\nUse for example 'rna' or the default mode.");

  return Mode (0);
}


void Input::set(const std::list<std::string*> &s, const Loc &l) {
  for (std::list<std::string*>::const_iterator i = s.begin();
       i != s.end(); ++i) {
    modes_.push_back(str_to_mode(**i, l));
  }
}


void Input::set_tracks(size_t t) {
  if (!modes_.empty()) {
    return;
  }
  modes_.resize(t);
}


std::string* Input::toString(Mode m) {
  if (m < MODE_END && m >= 0) {
    return new std::string(map[m]);
  }
  return new std::string("");
}
