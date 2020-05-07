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

#ifndef SHAPE_ALPH_HH
#define SHAPE_ALPH_HH

template <typename T, typename Size>
struct ShapeAlph {
  enum { char_width = 2 };
  private:
  void set_one(T &t, Size n) const
  {
    T x = T(1) << n;
    t |= x;
  }
  public:
  void operator()(T &t, char x, Size l) const
  {
    switch (x) {
      case '[' :
        t |= T(1) << l-1;
        //set_zero(t, l);
        //set_one(t, l-1);
        break;
      case ']' :
        t |= T(2) << l-1;
        //set_one(t, l);
        //set_zero(t, l-1);
        break;
      case '_' :
        //set_one(t, l);
        //set_one(t, l-1);
        t |= T(3) << l-1;
        break;
      default: assert(false);
    }
  }
  char to_char(T &t, Size i) const
  {
    switch (t >> i & T(3)) {
      case 1 : return '[';
      case 2 : return ']';
      case 3 : return '_';
      default: return 0;
    }
  }
};

#endif
