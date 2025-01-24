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


#ifndef SRC_CONST_HH_
#define SRC_CONST_HH_

#include <string>
#include "yieldsize.hh"
#include "type.hh"
#include "loc.hh"


namespace Const {
enum Type { CHAR, INT, FLOAT, STRING, SIZE, RATIONAL, BOOL };


class Base {
 private:
  Type type;

 public:
  Loc location;

 protected:
  Yield::Size ys;
  ::Type::Base *datatype;
  Base(Type t, const Loc &l) : type(t), location(l), datatype(NULL) {}
  explicit Base(Type t) : type(t), datatype(NULL) {}

 public:
  virtual ~Base();


  bool is(Type t) {
    return type == t;
  }


  const Yield::Size & yield_size() {
    return ys;
  }


  ::Type::Base *data_type() {
    return datatype;
  }


  bool set_data_type(::Type::Base *t, const Loc &l) {
    bool b = set_if_compatible(datatype, t, location, l);
    return b;
  }


  void print_type(std::ostream &s);

  virtual void put(std::ostream &s);
};


class Number : public Base {
 protected:
  Number(const std::string &n, Type t, const Loc &l) : Base(t, l) {
    uint32_t i = static_cast<uint32_t>(n.length());
    ys.set(i, i);
  }


  explicit Number(Type t) : Base(t) {}

 public:
  virtual void setNegative();
};


class Char : public Base {
 private:
 public:
  char c;
  Char(const std::string &n, const Loc &l);
  explicit Char(char x) : Base(CHAR), c(x) {}
  void put(std::ostream &s);
};


class Bool : public Base {
 private:
 public:
  bool b;
  explicit Bool(bool t) : Base(BOOL), b(t) {
  datatype = new ::Type::Bool();
  }
  void put(std::ostream &s);
};


class Int : public Number {
 private:
 public:
  int i;

  explicit Int(int n) : Number(INT) {
    i = n;
    datatype = new ::Type::Int();
  }

  Int(const std::string &n, const Loc &l) : Number(n, INT, l) {
    std::istringstream(n) >> i;
    datatype = new ::Type::Int();
  }


  void setNegative() {
    Number::setNegative();
    i = i*-1;
  }


  void put(std::ostream &s);
};


class Float : public Number {
 private:
 public:
  double f;


  Float(const std::string &n, const Loc &l) : Number(n, FLOAT, l) {
    std::istringstream(n) >> f;
    datatype = new ::Type::Float();
  }


  explicit Float(double d);


  void setNegative() {
    Number::setNegative();
    f = f*-1.0;
  }


  void put(std::ostream &s);
};


class Size : public Number {
 public:
  size_t u;

  explicit Size(size_t a) : Number(SIZE), u(a) {
    datatype = new ::Type::Size();
  }


  void setNegative() { assert(false); }

  void put(std::ostream &s);
};


class String : public Base {
 private:
 public:
  std::string *s;


  String(std::string *n, const Loc &l) : Base(STRING, l), s(n) {
    uint32_t i = static_cast<uint32_t>(n->length());
    ys.set(i, i);
    datatype = new ::Type::String();
  }


  explicit String(const std::string &n) : Base(STRING) {
    s = new std::string(n);
  }


  void put(std::ostream &s);
  void put_noquote(std::ostream &s);
};


class Rational : public Base {
 public:
  std::string *a, *b;

  Rational(std::string *x, std::string *y, const Loc &l)
    : Base(RATIONAL, l), a(x), b(y) {
    datatype = new ::Type::Rational();
    uint32_t i = x->size() + y->size() + 1;
    ys.set(i, i);
  }


  void put(std::ostream &s);
};


inline std::ostream &operator<<(std::ostream &s, Base &b) {
  b.put(s);
  return s;
}


}  // namespace Const


#endif  // SRC_CONST_HH_
