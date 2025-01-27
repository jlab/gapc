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


#include "para_decl.hh"
#include <list>
#include <string>

#include "log.hh"

#include "type/multi.hh"


namespace Para_Decl {
Base::~Base() {
}

Multi::Multi(const std::list<Base*> &l, const Loc &lo) : Base(lo, MULTI) {
  std::list< ::Type::Base*> types;
  for (std::list<Base*>::const_iterator i = l.begin(); i != l.end(); ++i) {
    Simple *s = dynamic_cast<Simple*> (*i);
    if (!s) {
      Log::instance()->error((*i)->location(),
        "No nested multi track declaration allowed.");
      continue;
    }
    list_.push_back(s);

    types.push_back(s->type());
  }
  type_ = new ::Type::Multi (types, lo);
}


void Multi::replace(::Type::Base *t) {
  ::Type::Multi *m = dynamic_cast< ::Type::Multi* > (t);
  assert(m);

  assert(m->types().size() == list_.size());
  std::list< ::Type::Base* >::const_iterator j = m->types().begin();
  for (std::list<Para_Decl::Simple*>::iterator i = list_.begin();
       i != list_.end(); ++i, ++j) {
    (*i)->replace(*j);
  }
}


Base *Simple::copy() const {
  Simple *o = new Simple(*this);
  o->name_ = new std::string(*name_);
  return o;
}


Base *Multi::copy() const {
  Multi *o = new Multi (*this);
  o->list_.clear();
  for (std::list<Simple*>::const_iterator i = list_.begin();
       i != list_.end(); ++i) {
    Simple *t = dynamic_cast<Simple*> ((*i)->copy());
    assert(t);
    o->list_.push_back(t);
  }
  return o;
}
}  // namespace Para_Decl
