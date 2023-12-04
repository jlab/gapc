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

#ifndef RTLIB_SHAPE_ALPH_HH_
#define RTLIB_SHAPE_ALPH_HH_

#include <math.h>

template <typename T, typename Size>
struct ShapeAlph {
  enum {
    /* number bits for a character to split a byte into
     * must divide 8 without rest, i.e. can be 2, 4, 8
     * make sufficient space for the number of different
     * characters in your alphabet
     */
    char_width = 4
  };

 private:
  unsigned int char_states = pow(
    2, static_cast<int>(this->char_width))-1;

  void set_one(T &t, Size n) const {
    T x = T(1) << n;
    t |= x;
  }

 public:
  void operator()(T &t, char x, Size l) const {
    switch (x) {
      case '[' :
        t |= T(1) << (l-(char_width-1));
        // set_zero(t, l);
        // set_one(t, l-1);
        break;
      case ']' :
        t |= T(2) << (l-(char_width-1));
        // set_one(t, l);
        // set_zero(t, l-1);
        break;
      case '_' :
        // set_one(t, l);
        // set_one(t, l-1);
        t |= T(3) << (l-(char_width-1));
        break;
      case 'G' :
        // set_one(t, l);
        // set_one(t, l-1);
        t |= T(4) << (l-(char_width-1));
        break;
      default: assert(false);
    }
  }
  char to_char(T &t, Size i) const {
    switch (t >> i & T(char_states)) {
      case 1 : return '[';
      case 2 : return ']';
      case 3 : return '_';
      case 4 : return 'G';
      default: return 0;
    }
  }
};

#endif  // RTLIB_SHAPE_ALPH_HH_
