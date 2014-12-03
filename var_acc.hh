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


#ifndef VAR_ACC_HH
#define VAR_ACC_HH

#include "loc.hh"
#include "bool.hh"

#include <string>
#include <ostream>

#include "expr_fwd.hh"
#include "statement_fwd.hh"


namespace Var_Acc {
	
	
	enum Type { ARRAY, PLAIN, COMP };
	
	
	// The base class for all variable access classes.
	class Base {
		
		private:
			
			Type type;
			
			
			protected:
			
			Base(Type t, const Loc&l) : type(t), location(l) {}
			Base(Type t) : type(t) {}
			Bool itr_access;
			
			
		public:
			
			virtual ~Base();
			Loc location;
			bool is(Type t) { return type == t; }
			
			// This method will write the variable access's
			// string representation out to the given stream.
			virtual void put (std::ostream &s) const;
			
			void set_itr (bool b) { itr_access = b; }
			bool is_itr() const { return itr_access; }
			
			
	};
	
	
	// The plain access simply models a direct memory access to a location
	// named by a variable. 
	class Plain : public Base {
		
		public:
			
			std::string *name;
			Statement::Var_Decl *vdecl;
			
			Plain(std::string *n, const Loc &l) : Base(PLAIN, l), name(n), vdecl(NULL) {}
			Plain(std::string *n) : Base(PLAIN), name(n), vdecl(NULL) {}
			Plain (Statement::Var_Decl &a);
			
			void put (std::ostream &s) const;
			
			
	};
	
	
	// The composed memory access models nested structures like
	// a 'struct'. which is a container for a structured memory area
	// whose subcomponents can be accessed by named identifier.
	class Comp : public Base {
		
		public:
			
			Base *lhs;
			std::string *rhs;
			
			Comp(Base *lh, std::string *n, const Loc &l) : Base(COMP, l), lhs(lh), rhs(n)  { }
			Comp(Base *lh, std::string *n) : Base(COMP), lhs(lh), rhs(n)  { }
			Comp(const Statement::Var_Decl &vdecl, int n);
			
			void put(std::ostream &s) const;
			
			
	};
	
	
	// The array access to a memory location.
	class Array : public Base {
		
		public:
			
			Base *lhs;
			Expr::Base *expr;
			
			Array(Base *lh, Expr::Base *e, const Loc &l) : Base(ARRAY, l), lhs(lh), expr(e) {}
			Array(Base *lh, Expr::Base *e) : Base(ARRAY), lhs(lh), expr(e) {}
			
			void put(std::ostream &s) const;
			
			
	};
	
	
	// Overloaded stream operator which simply maps the
	// operator application to the function call 'put'
	// which has a virtual implementation for each subclass.
	inline std::ostream &operator<< (std::ostream &s, const Base &b)
	{
		b.put (s);
		return s;
	}
	
	
}


#endif

