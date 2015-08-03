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

#ifndef STATEMENT_BASE_HH
#define STATEMENT_BASE_HH


#include "../loc.hh"

#include "../statement_fwd.hh"
#include "../expr_fwd.hh"

#include "../bool.hh"


class Fn_Def;


namespace Printer { class Base; }


#include <boost/tuple/tuple.hpp>
#include <list>
#include <stack>
#include <cassert>


namespace Statement {
	
	enum Type { RETURN, IF, VAR_DECL, BLOCK, FOR, FOREACH,
		VAR_ASSIGN, FN_CALL, BACKTRACE_DECL, BACKTRACE_NT_DECL,
		HASH_DECL, BREAK, MARKER_DECL, TABLE_DECL, CONTINUE, WHILE,
                DECREASE, INCREASE, SORTER, SWITCH

	};
	
	
	class Base {
		
		private:
			
			Type type;
			
			
		public:
			
			Bool disabled_;
			Loc location;
			
			
		protected:
			
			Base(Type t) : type(t) {}
			Base(Type t, const Loc& l) : type(t), location(l) {}
			
			
		public:
			
			bool is(Type t) { return type == t; }
			Type getType() { return this->type; };
			
			void disable() { disabled_ = true; }
			bool is_disabled() const { return disabled_; }
			
			virtual ~Base();
			virtual void print(Printer::Base &p) const = 0;
			
			virtual std::list<Base*> *stmts();
			
			virtual Var_Decl* var_decl();
			virtual void replace(Var_Decl &decl, Expr::Base *expr);
			
			virtual Base *copy() const;
			
			
	};


	
	template <class Iterator> typename std::iterator_traits<Iterator>::value_type nest_for_loops(Iterator begin, Iterator end)
	{
		assert(begin != end);
		typename std::iterator_traits<Iterator>::value_type last = *begin;
		++begin;
		for (; begin != end; ++begin) {
			last->statements.push_back(*begin);
			last = *begin;
		}
		return last;
	}
	
	
	class Iterator {
		
		private:
			
			std::list<Statement::Base*>::iterator i;
			std::list<Statement::Base*>::iterator j;
			typedef boost::tuple<std::list<Statement::Base*>::iterator,
			std::list<Statement::Base*>::iterator,
			std::list<Statement::Base*>*> stuple;
			std::stack<stuple> stack;
			bool end;
			std::list<Statement::Base*>* list;
			
			void fwd();
			
			
		public:
			
			Iterator(std::list<Statement::Base*> &l)
				: end(false)
			{
				i = l.begin();
				j = l.end();
				list = &l;
				fwd();
			}
			
			
			Iterator()
				: end(true), list(0)
			{
			}
			
			
			Iterator &operator++();
			
			
			Statement::Base *&operator*()
			{
				assert(i != j);
				assert(*i);
				return *i;
			}
			
			
			void erase()
			{
				delete *i;
				i = list->erase(i); 
			}
			
			
			bool operator==(const Iterator &itr) const
			{
				return end == itr.end;
			}
			
			
			bool operator!=(const Iterator &itr) const
			{
				return !(*this == itr);
			}
			
			
	};
	
	
	typedef Iterator iterator;
	
	iterator begin(std::list<Statement::Base*> &l);
	iterator begin(Fn_Def &fn);
	iterator end();
	
	
}


// For debug purposes
std::ostream &operator<<(std::ostream &s, const Statement::Base &b);

#endif
