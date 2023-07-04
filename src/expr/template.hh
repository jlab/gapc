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


#ifndef SRC_EXPR_TEMPLATE_HH_
#define SRC_EXPR_TEMPLATE_HH_

#include <ostream>
#include <string>
#include <vector>
#include <utility>
#include "base.hh"

namespace Expr {

struct TemplateArg {
  std::string type, name;

  TemplateArg(const std::string &type, const std::string &name)
    : type(type), name(name) {}

    friend std::ostream &operator<<(std::ostream &stream, const TemplateArg &a);
};

inline std::ostream &operator<<(std::ostream &stream, const TemplateArg &a) {
  stream << a.type << ' ' << a.name;
  return stream;
}

class Template : public Base {
 public:
  std::vector<TemplateArg> args;

  Template() : Base(Expr::TEMPLATE) {}

  void put(std::ostream &s) const {
    if (!args.size()) {
      return;
    }
    s << "template<";
    size_t arg_idx = 0;
    s << args[arg_idx++];
    for (; arg_idx < args.size(); ++arg_idx) {
      s << ", " << args[arg_idx];
    }
    s << '>';
  }

  void add(const TemplateArg &arg) {
    args.push_back(arg);
  }

  void add(TemplateArg &&arg) {
    args.emplace_back(std::move(arg));
  }
};

}  // namespace Expr

#endif  // SRC_EXPR_TEMPLATE_HH_
