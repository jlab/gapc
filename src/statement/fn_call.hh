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


#ifndef STATEMENT_FN_CALL_HH
#define STATEMENT_FN_CALL_HH

#include "base.hh"
#include "../expr_fwd.hh"
#include "../var_acc_fwd.hh"
#include "../bool.hh"

#include <list>
#include <string>


namespace Statement {
	
	class Fn_Call : public Base {
		
		public:
			
			enum Builtin { NONE, PUSH_BACK, ERASE, CLEAR, TABULATE, EMPTY,
			     APPEND, STR_APPEND, PARETO_YUKISH, ASSERT,
			     SET_VALUE, HASH_FILTER,
			     APPEND_FILTER,
			     UPDATE,
			     MARK,
			     FINALIZE,
			     INNER,
                             MARK_POSITION,
                             JOIN_MARKED,
                             PARETO_DOMINATION_SORT
			};
			
			
			// A public list that contains the names of all builtin
			// functions gapc has. Use
			// 'new std::string (Statement::Fn_Call::map_builtin_to_string[f->builtin])'
			// to create a string when 'name_ == NULL'
			static const char *map_builtin_to_string[];
			// The type of builtin function this call represents, or
			// NONE if some other (possibly extern) function-call.
			Builtin builtin;
			// The name of the function being called, or NULL if this
			// is a builtin function. ATTENTION: this field is also
			// filled with a value if this is a builtin function parsed
			// by the parser module from source code. for convenience
			// use the method Fn_Call::name() instead of direct access
			// to this field!
			std::string *name_;
			// The list of arguments passed to the function.
			std::list<Expr::Base*> args;
			
			
			Fn_Call (std::string *n, std::list<Expr::Base*> *l, const Loc &loc);
			
			
			Fn_Call (const std::string &n)
				: Base(FN_CALL), builtin(NONE), name_(new std::string(n))
			{
			}
			
			
			Fn_Call (Builtin b)
				: Base(FN_CALL), builtin(b), name_(NULL)
			{
			}
			
			
			Fn_Call(Builtin b, Var_Decl &vdecl)
				: Base(FN_CALL), builtin(b), name_(NULL)
			{
				add_arg(vdecl);
			}
			
			
			std::string name() const;
                        void add_arg(Expr::Vacc *vdecl);
			void add_arg(Var_Decl &vdecl);
			void add_arg(Table_Decl &vdecl);
			void add(Table_Decl &vdecl);
			void add_arg(Var_Acc::Base *vacc);
			void add_arg(std::string *n);
			
			
			void add_arg(Expr::Base* e)
			{
				args.push_back(e);
			}
			
			
			void print(Printer::Base &p) const;
			
			void replace(Var_Decl &decl, Expr::Base *expr);
			
			Bool is_obj;
			Base *copy() const;
			
			
	};
	
	
}


#endif

