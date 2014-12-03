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

#ifndef TYPE_MULTI_HH
#define TYPE_MULTI_HH

#include "base.hh"
#include <list>

#include "../para_decl.hh"


namespace Type {

  class Multi : public Base {
    private:
      std::list<Base*> types_;
    public:
      MAKE_CLONE(Multi);
      Multi(const std::list<Base*> &ts,
          const Loc &l)
        : Base(MULTI, l), types_(ts)
      {
      }
      Multi(const std::list<Base*> &ts)
        : Base(MULTI), types_(ts)
      {
      }

      bool is_eq(const Base & base) const;

      void print(Printer::Base &s) const;
      std::ostream & put(std::ostream &s) const;

      const std::list<Base*> &types() const { return types_; }

  };


}


#endif
