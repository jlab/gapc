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

#include "new.hh"

#include "../type.hh"

void Expr::New::put(std::ostream &s) const
{
  s << " new " << *obj_ << '(';
  for (std::list<Base*>::const_iterator i = args_.begin();
       i != args_.end(); ++i)
    s << **i << ", ";
  s << ')';
}

void Expr::This::put(std::ostream &s) const
{
  s << "this";
}

#include "../para_decl.hh"

void Expr::New::add_args(const std::list<Para_Decl::Base*> &args)
{
  for (std::list<Para_Decl::Base*>::const_iterator i = args.begin();
      i != args.end(); ++i) {
    Para_Decl::Simple *p = dynamic_cast<Para_Decl::Simple*>(*i);
    if (p) {
      args_.push_back(new Expr::Vacc(p->name()));
    } else {
      Para_Decl::Multi *p = dynamic_cast<Para_Decl::Multi*>(*i);
      for (std::list<Para_Decl::Simple*>::const_iterator j = p->list().begin();
          j != p->list().end(); ++j)
        args_.push_back(new Expr::Vacc((*j)->name()));
    }
  }
}
