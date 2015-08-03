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


#ifndef FILTER_HH
#define FILTER_HH

#include "hashtable.hh"
#include "loc.hh"

#include "expr_fwd.hh"

#include <string>
#include <list>

class Filter {
	
	public:
	
		enum Builtin { NONE, MAX_SIZE, MIN_SIZE };
		
		
	private:
		
		Builtin builtin;
		bool stateful;
		size_t instance;
		
		// FIXME change to non-static, if user defined filters are possible
		//       (like with add_predefined)
		static hashtable<std::string, Builtin> table;
		
		
	public:
		
		enum Type { NO_TYPE, WITH, SUCHTHAT, WITH_OVERLAY, SUCHTHAT_OVERLAY };
		
		std::string *name;
		Loc location;
		std::list<Expr::Base*> args;
		Type type;
		
		Filter(std::string *n, const Loc &l);
		bool check_const_args();
		
		bool is(Builtin b) const { return builtin == b; }
		bool is(Type t) const { return type == t; }
		static void init_table();
		void init_builtin();
		
		size_t uint_arg() const;
		
		
	private:
		
		void init_stateful_filters();
		
		
	public:
		
		bool is_stateful() const { return stateful; }
		std::string id() const;
		
		
};

#endif
