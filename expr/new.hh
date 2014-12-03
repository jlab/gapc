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

#ifndef EXPR_NEW_HH
#define EXPR_NEW_HH

#include "base.hh"
#include "vacc.hh"

#include <list>

#include "../para_decl_fwd.hh"

namespace Type { class Base; }

namespace Expr {
  class New : public Base {
    private:
      ::Type::Base *obj_;
      std::list<Base*> args_;
    public:
      New(::Type::Base *x)
        : Base(NEW), obj_(x)
      {
      }
      template<typename Iterator>
      void add_args(Iterator begin, Iterator end)
      {
        for (; begin != end; ++begin)
          args_.push_back(new Vacc(*begin));
      }
      void add_args(const std::list<Para_Decl::Base*> &args);
      void add_arg(Expr::Base *e) { args_.push_back(e); }
      void put(std::ostream &s) const;

      const ::Type::Base *obj() const { return obj_; }
      const std::list<Base*> &args() const { return args_; }
  };

  class This : public Base {
    private:
    public:
      This()
        : Base(THIS)
      {}
      void put(std::ostream &s) const;
  };
}

#endif
