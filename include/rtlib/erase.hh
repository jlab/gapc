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


#ifndef ERASE_HH
#define ERASE_HH

// no-op for bultin
// no-op for String -> does ref-counting and non-pointer usage
// List needs overloaded version
template<typename T> inline void erase(T &x)
{
}

template<typename S, typename T>
inline
void erase(std::pair<S, T> &x)
{
  erase(x.first);
  erase(x.second);
}

// multi track versions:
template<typename T1, typename T2> inline void erase(T1 &x1, T2 &x2)
{
  erase(x1);
  erase(x2);
}

#endif
