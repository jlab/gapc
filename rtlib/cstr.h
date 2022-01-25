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

#ifndef RTLIB_CSTR_H_
#define RTLIB_CSTR_H_

inline
char *int_to_str(char *s, unsigned char *len, int j) {
  int i = j;
  s[11] = 0;
  char *c = s+9;
  *c = '0';
  c++;
  if (!i)
    c--;
  else if (i < 0)
    i *= -1;
  while (i) {
    c--;
    *c = '0' + i % 10;
    i /= 10;
  }
  if (j < 0) {
    c--;
    *c = '-';
  }
  *len = 10 - (c-s);
  return c;
}

#endif  // RTLIB_CSTR_H_
