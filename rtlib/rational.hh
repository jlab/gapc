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


#ifndef RATIONAL_HH
#define RATIONAL_HH

#include <gmpxx.h>

typedef mpq_class Rational;


#include "sequence.hh"

template<typename pos_type>
inline Rational  CONST_RATIO(Sequence &seq, pos_type i, pos_type j, 
    const Rational &x)
{
  assert(i == j);
  return x;
}

inline void empty(Rational &x)
{
  x = -1;
}

//template<> inline bool isEmpty(double x)
inline bool isEmpty(const Rational &x)
{
  return x == -1;
}

#endif
