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

#include "multi.hh"
#include <list>

#include "../printer.hh"

void Type::Multi::print(Printer::Base &s) const {
  s.print(*this);
}


std::ostream & Type::Multi::put(std::ostream &s) const {
  s << " < ";

  std::list<Base*>::const_iterator i = types_.begin();
  if (i != types_.end()) {
    (*i)->put(s);
    ++i;
  }
  for (; i != types_.end(); ++i) {
    s << ", ";
    (*i)->put(s);
  }

  s << " > ";

  return s;
}

bool Type::Multi::is_eq(const Base &base) const {
  const Multi *o = dynamic_cast<const Multi*>(base.const_simple());
  if (!o)
    return false;
  if (types_.size() != o->types().size())
    return false;
  const std::list<Base*> &ts = o->types();
  std::list<Base*>::const_iterator j = ts.begin();
  for (std::list<Base*>::const_iterator i = types_.begin(); i != types_.end();
       ++i, ++j)
    if (!(*i)->is_eq(**j))
      return false;
  return true;
}
