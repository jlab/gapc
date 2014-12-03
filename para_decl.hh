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

#ifndef PARA_DECL
#define PARA_DECL

#include <string>
#include <list>

#include "type_fwd.hh"

#include "loc.hh"

namespace Para_Decl {
	
	
	// The types a subclass of Para_Decl::Base can have.
	enum Type { SIMPLE, MULTI };
	
	
	// FIXME location
	class Base {
		
		private:
			
			Loc loc;
			
			// The type of the subclass.
			Type type;
			
			
		protected:
			
			Base (Type t) : type (t) {}
			Base (const Loc &l, Type t) : loc(l), type (t) {}
			virtual ~Base();
			
			
		public:
			
			const Loc &location() const { return loc; }
			
			virtual void replace (::Type::Base *t) = 0;
			
			virtual Base *copy() const = 0;
			
			// Returns TRUE if the type of the instance is of a
			// given type.
			bool is (Type t) { return this->type == t; }
			
			
	};
	
	
	class Simple : public Base {
		
		private:
			
			::Type::Base *type_;
			std::string *name_;
			
			
		public:
			
			Simple (::Type::Base *t, std::string *n)
				: Base (SIMPLE), type_(t), name_(n)
			{
			}
			
			Simple (::Type::Base *t, std::string *n, const Loc &l)
				: Base (l, SIMPLE), type_(t), name_(n)
			{
			}
			
			::Type::Base* type() { return type_; }
			std::string* name() { return name_; }
			
			void replace (::Type::Base *t) { type_ = t; }
			void replace (std::string *n) { name_ = n; }
			
			Base *copy() const;
			
			
	};
	
	
	class Multi : public Base {
		
		private:
			
			std::list<Simple*> list_;
			::Type::Base *type_;
			
			
		public:
			
			Multi (const std::list<Simple*> &l)
				: Base (MULTI), list_(l), type_(0)
			{
			}
			
			Multi (const std::list<Base*> &l, const Loc &lo);
			
			::Type::Base *type() { return type_; }
			const std::list<Simple*> &list() const { return list_; }
			
			void replace (::Type::Base *t);
			
			Base *copy() const;
			
			
	};
	
	
}


#endif


