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


#include "filter.hh"
#include <string>

#include "log.hh"
#include "expr.hh"
#include "const.hh"


void Filter::init_builtin() {
  hashtable<std::string, Builtin>::iterator i;
  if ((i = table.find(*name)) != table.end()) {
    builtin = i->second;
  }
  if (builtin == MAX_SIZE || builtin == MIN_SIZE) {
    if (args.size() != 1) {
      Log::instance()->error(location, "Filter " + (*name) +
          " has too much arguments.");
    } else {
      Expr::Base *e = args.front();
      if (e->is(Expr::CONST)) {
        Expr::Const *c  = dynamic_cast<Expr::Const*>(e);
        if (!c->base->is(Const::INT)) {
          Log::instance()->error(e->location,
              "Not an integer as argument of filter " + (*name) + ".");
        } else {
          Const::Int *i = dynamic_cast<Const::Int*>(c->base);
          if (i->i < 0) {
            Log::instance()->error(e->location,
                "Negative argument makes no sense here.");
          }
        }
      } else {
        Log::instance()->error(e->location,
            "Not an const as argument of filter " + (*name) + ".");
      }
    }
  }
}

hashtable<std::string, Filter::Builtin> Filter::table;

void Filter::init_table() {
  table["maxsize"] = MAX_SIZE;
  table["minsize"] = MIN_SIZE;
}

size_t Filter::uint_arg() const {
  assert(args.size() == 1);
  Expr::Base *e = args.front();
  assert(e->is(Expr::CONST));
  Expr::Const *c = dynamic_cast<Expr::Const*>(e);
  assert(c->base->is(Const::INT));
  Const::Int *i = dynamic_cast<Const::Int*>(c->base);
  assert(i->i >= 0);
  return size_t(i->i);
}

Filter::Filter(std::string *n, const Loc &l) : builtin(NONE), stateful(false),
  instance(0), name(n), location(l), type(NO_TYPE) {
    init_stateful_filters();
}

void Filter::init_stateful_filters() {
  if (!(name->substr(0, 3) == "sf_" || *name == "iupac"))
    return;
  static size_t counter = 0;
  instance = counter++;
  stateful = true;
}

std::string Filter::id() const {
  std::ostringstream o;
  o << "filter_" << *name << instance;
  return o.str();
}
