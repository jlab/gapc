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

#include "base.hh"

#include <cstdlib>
#include <string>

Type::Base::~Base() {}


bool Type::Base::is(Type t) const {
  /* extra rule to match Rope (which is external) with String in terminal 
     arguments like ROPE("stefan") */
  if ((t == Type::STRING) && (type == Type::EXTERNAL)) {
    return true;
  }
  return type == t;
}

bool Type::Base::is_eq(const Base & base) const {
  return base.const_simple()->is(type);
}

Type::Base * Type::Base::simple() {
  return this;
}


const Type::Base * Type::Base::const_simple() const {
  return this;
}

Type::Base *Type::Base::left() {
  std::abort();
  return 0;
}

Type::Base *Type::Base::right() {
  std::abort();
  return 0;
}

Type::Base *Type::Base::component() {
  std::abort();
  return 0;
}

Type::Base *Type::Base::deref() {
  return this;
}

// replaces from with to in str
void replaceAll(std::string& str, const std::string& from,
                const std::string& to) {
  if (from.empty())
    return;
  size_t start_pos = 0;
  while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
    str.replace(start_pos, from.length(), to);
    // In case 'to' contains 'from', like replacing 'x' with 'yx'
    start_pos += to.length();
  }
}

// graphViz compatible text representation of datatype
void Type::Base::to_dot(std::ostream &out) {
  std::ostringstream dtype_stream;
  this->put(dtype_stream);
  std::string dtype = dtype_stream.str();
  replaceAll(dtype, std::string("<"), std::string("&lt;"));
  replaceAll(dtype, std::string(">"), std::string("&gt;"));
  out << dtype;
}
