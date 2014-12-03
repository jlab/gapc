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


#ifndef TYPE_FWD_HH
#define TYPE_FWD_HH

namespace Type {
  class Base;
  class List;
  class Tuple;
  class TupleDef;
  class Signature;
  class Alphabet;
  class Def;
  class Choice;
  class Void;
  class RealVoid;
  class Int;
  class Integer;
  class Size;
  class Float;
  class Single;
  class String;
  class Char;
  class Bool;
  class Usage;
  class Range;
  class Seq;
  class Table;
  class Subseq;
  class Shape;
  class Referencable;
  class Rational;
  class BigInt;
  class External;
  
  class Eval_List;
  class Backtrace;
  class Backtrace_List;

  class Name;

  class Multi;
}

#include <string>
#include <list>
#include <utility>

typedef std::pair<Type::Name*, std::string*> Tuple_Pair;
typedef std::list<Tuple_Pair*> Tuple_List;

#endif
