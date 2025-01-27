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

#ifndef SRC_TYPE_BASE_HH_
#define SRC_TYPE_BASE_HH_

#include <cassert>
#include <sstream>
#include <string>

#include "../loc.hh"
#include "../printer_fwd.hh"



#define MAKE_CLONE(X) X *clone() { return new X(*this); }

namespace Type {

enum Type { NONE, LIST, TUPLE, TUPLEDEF, SIGNATURE, ALPHABET, DEF,
CHOICE,
VOID, INT, INTEGER, SIZE, FLOAT, STRING, CHAR, BOOL, REALVOID,
USAGE,
RANGE,  // codegen, set of begin/end iterators
SEQ,
TABLE,
SUBSEQ,
SHAPE,
REFERENCABLE,  // only hint to backend
RATIONAL,
BIGINT,
EXTERNAL,

BACKTRACE,
BACKTRACE_LIST,
EVAL_LIST,

SINGLE,

MULTI,
MULTI_DECL
};


class Base {
 protected:
    Type type;
    bool terminal;
    Base(Type t, const Loc &l) : type(t), terminal(false), location(l) {}
    explicit Base(Type t) : type(t), terminal(false) {}

 public:
    virtual Base *clone() = 0;
    virtual ~Base();

    Loc location;
    virtual bool is(Type t) const;
    Type getType() { return this->type; }

    virtual Base * simple();
    virtual const Base * const_simple() const;
    virtual bool is_eq(const Base & base) const;


    bool operator== (const Base &a) const {
      assert(false);
      return false;
      // return is_eq(a) const;
    }

    bool operator< (const Base &a) const {
      return type == NONE && a.type != NONE;
    }

    virtual std::ostream & put(std::ostream &s) const = 0;

    void set_terminal() { terminal = true; }
    bool is_terminal() { return terminal; }

    virtual Base *left();
    virtual Base *right();
    virtual Base *component();
    virtual Base *deref();

    virtual void print(Printer::Base &s) const = 0;
    void to_dot(std::ostream &out);
};


inline std::ostream & operator<< (std::ostream &s, const Base &p) {
  return p.put(s);
}


}  // namespace Type

void replaceAll(std::string& str, const std::string& from,
  const std::string& to);

#endif  // SRC_TYPE_BASE_HH_
