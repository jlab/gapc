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


#include "const.hh"
#include <string>
#include "log.hh"

Const::Char::Char(const std::string &n, const Loc &l)
  : Base(CHAR, l) {
  assert(n.size() > 0);
  if (n[0] == '\\') {
    if (n.size() == 2 && n[1] < '0' && n[1] > '9') {
      switch (n[1]) {
        case 'n' : c = '\n'; break;
        case 't' : c = '\t'; break;
        default: {
                   std::ostringstream o;
                   o << "Unknown escaped char: " <<  n[1];
                   throw LogError(l, o.str());
                 }
      }
    } else {
      std::string suff(n.substr(1));
      Int i(suff, l);
      c = static_cast<char>(i.i);
    }
  } else {
    c = n[0];
  }

  ys.set(1, 1);
  datatype = new ::Type::Char();
}

Const::Base::~Base() {}

void Const::Base::print_type(std::ostream &s) {
  if (datatype)
    s << *datatype;
  else
    s << "NULL";
}


void Const::Base::put(std::ostream &s) {
}

void Const::Int::put(std::ostream &s) {
  s << i;
}

void Const::Size::put(std::ostream &s) {
  s << u;
}

void Const::Float::put(std::ostream &s) {
  s << std::scientific << f;
}

void Const::Char::put(std::ostream &s) {
  if (c < 32) {
    // s << "\'\\x" << std::hex << int(c) << std::dec << '\'';
    s << int(c);
    return;
  }
  s << '\'' << c << '\'';
}

void Const::String::put(std::ostream &o) {
  o << '"' << *s << '"';
}
void Const::String::put_noquote(std::ostream &o) {
  o << *s;
}

void Const::Rational::put(std::ostream &o) {
  o << "Rational(\"" << *a << "/" << *b << "\")";
}

void Const::Bool::put(std::ostream &o) {
  o << (b ? "true" : "false");
}


void Const::Number::setNegative() {
  Yield::Poly r(ys.high());
  r += Yield::Poly(1);
  ys.set(ys.low(), r);
}

Const::Float::Float(double d)
  : Number(FLOAT), f(d) {
  datatype = new ::Type::Float();
}
