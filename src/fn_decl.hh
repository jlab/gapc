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


#ifndef FN_DECL_HH
#define FN_DECL_HH

#include "hashtable.hh"

#include "type.hh"
#include "loc.hh"
#include "log.hh"

#include "algebra.hh"

#include "yieldsize.hh"

#include <list>
#include <string>
#include <iostream>


class Fn_Decl {
	
	private:
		
		Yield::Size ys;
		void init (Type::Base *r);
		
		bool in_use_;
		
		
	protected:
		
		// A flag that is set TRUE if this is a choice function
		// declaration.
		bool choice_fn;
		
		bool types_equal (Fn_Decl &d);
		
		
	public:
		
		// The return type of this function.
		Type::Base* return_type;
		// The list of parameter types. This does not establish
		// a type-to-name mapping of the list of function parameters,
		// since this is just a declarations, and comes without
		// parameter names. Please see the Fn_Def.names list of
		// names which has all names for the types listed in this
		// list.
		std::list<Type::Base*> types;
		// The name of the function.
		std::string* name;
		// The source code location of the function declaration.
		Loc location;
		
		Fn_Decl (Type::Base *r, std::string *n, const Loc &l);
		Fn_Decl (Type::Base *r, std::string *n);
		Fn_Decl() : in_use_ (false), choice_fn (false), return_type (0), name (NULL) {}
		virtual ~Fn_Decl();
		
		virtual void set_types (std::list<Type::Base*> *l);
		
		bool is_Choice_Fn() const { return choice_fn; }
		
		static hashtable<std::string, Fn_Decl*> builtins;
		static void init_table();
		
		void replace (Type::Base *a, Type::Base *b);
		
		virtual void replace_types (std::pair<std::string*, Type::Base*> &alph, std::pair<std::string*, Type::Base*> &answer);
		
		static void add_fn_decl (hashtable<std::string, Fn_Decl *> &h, Fn_Decl *f);
		
		bool operator== (const Fn_Decl &d) const;
		
		const Yield::Size &yield_size() const { return ys; }
		void set_yield_size (const Yield::Size &a) { ys = a; }
		
		bool in_use() const { return in_use_; } 
		void set_in_use (bool b = true) { in_use_ = b; }
		
		
	protected:
		
		std::list<Type::Base*> nttypes_;
		
		
	public:
		
		void set_nttypes(std::list<Type::Base*> *l);
		const std::list<Type::Base*> &nttypes() const { return nttypes_; }
		
		
};


std::ostream &operator<<(std::ostream &s, const Fn_Decl &f);


#endif	// ifndef FN_DECL_HH

