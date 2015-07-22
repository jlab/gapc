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


#ifndef EXPR_FN_CALL_HH
#define EXPR_FN_CALL_HH


#include "base.hh"


class Fn_Decl;
class Fn_Def;
class Filter;


#include "../type_fwd.hh"
#include "../var_acc_fwd.hh"
#include "../para_decl_fwd.hh"
#include "../bool.hh"

#include <string>
#include <map>
#include <list>
#include <vector>


namespace Expr {
	
	class Fn_Call : public Base {
			
		public:
			
			enum Builtin { NONE, NOT_EMPTY, SPLICE_LEFT, GET_RANGE,
				IS_TABULATED, GET_TABULATED, IS_EMPTY, GET_FRONT,
                                GET_BACK, ERASE_ELEMENT, INSERT_ELEMENT, ROUND_TO_DIGIT,
                                
				// algebra choice fns
				MINIMUM,
				MAXIMUM,
				SUM,
				UNIQUE,
				LIST,
				//
				EVALUATE,
				EXECUTE_BACKTRACK,
				//
				SPLICE_RIGHT,
				EXECUTE_BACKTRACK_ONE,
				EXECUTE_BACKTRACK_K,
				//
				MARKED,
				DUMMY_BT,
				//
				EXPSUM,
				EXP,
				LOG,
				EXECUTE_BACKTRACK_K_ONE,
				EXP2,
				LOG2,
				EXP2SUM,
				BITSUM,
				POW
			};
			
			
			static const char *map_builtin_to_string[];
			static std::map<std::string, Builtin> map_string_to_builtin;
			static void init_builtins();
			std::string *name;
			Builtin builtin;
			
			Fn_Call(std::string *n, const Loc &l);
			
			
			Fn_Call(std::string *n)
				: Base(FN_CALL), name(n), builtin(NONE), type_param(NULL) {
			}
			
			
			Fn_Call(Builtin b)
				: Base(FN_CALL), name(NULL), builtin(b), type_param(NULL) {
			}
			
			
			Fn_Call(const Fn_Decl &f);
			Fn_Call(const Fn_Def &f);
			Fn_Call(std::string *n, std::list<Statement::Var_Decl*> &l);
			Fn_Call(const Filter &f);
			
			// The list of arguments passed in this function call.
			std::list<Base*> exprs;
			// like for template fns T empty<T>()
			::Type::Base *type_param;
			
			void add_arg(Statement::Var_Decl &v);
			void add(const std::vector<Statement::Var_Decl*> &l);
			void add_arg(const Statement::Table_Decl &v);
			void add(const Statement::Table_Decl &v);
			void add_arg(std::string *n);
			void add_arg(Expr::Base *e);
			void add_arg(Var_Acc::Base *e);
			
			void add(const std::vector<Expr::Base*> &l, const std::vector<Expr::Base*> &r);
			
			
			void add(const std::list<Base*> &l) {
				exprs.insert(exprs.end(), l.begin(), l.end());
			}
			
			
			void add(const std::list<Para_Decl::Base*> &l);
			
			void put(std::ostream &s) const;
			
			Fn_Call *fn_call();
			void replace(Statement::Var_Decl &decl, Base *expr);
			
			Fn_Call *clone();
			
			Bool is_obj;
			
			
		private:
			
			void put_arg(std::ostream &s, Expr::Base *e) const;
			
			
		public:
			
			Base *copy() const;
			
			
	};
	
	
}


#endif

