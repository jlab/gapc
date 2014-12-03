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

#ifndef CM_ALPH_HH
#define CM_ALPH_HH

/*


*.   M  1
*_   D  2
-.   I  3

[_   l  4
[.   L  5
]_   r  6
].   R  7
[(   P  8
])   K  9





// reverse _*   D  0001  1   8

raus reverse .-   >  0011  3   12



// eigentlich *- ->  *_   *  1010  10  5




 */

template <typename T, typename Size>
struct CmAlph {
  enum { char_width = 4 };
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
      case 'M' :
        t |= T(1) << l-3;
        break;
      case 'D' :
        t |= T(2) << l-3;
        break;
      case 'I' :
        t |= T(3) << l-3;
        break;
      case 'l' :
        t |= T(4) << l-3;
        break;
      case 'L' :
        t |= T(5) << l-3;
        break;
      case 'r' :
        t |= T(6) << l-3;
        break;
      case 'R' :
        t |= T(7) << l-3;
        break;
      case 'P' :
        t |= T(8) << l-3;
        break;
      case 'K' :
        t |= T(9) << l-3;
        break;
      default: assert(false);
    }
  }
  char to_char(T &t, Size i) const
  {
    switch (t >> i & T(15)) {
      case 1 : return 'M';
      case 2 : return 'D';
      case 3 : return 'I';
      case 4 : return 'l';
      case 5 : return 'L';
      case 6 : return 'r';
      case 7 : return 'R';
      case 8 : return 'P';
      case 9 : return 'K';
      default: return 0;
    }
  }
};

#endif
