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

#ifndef EXPR_VACC_HH
#define EXPR_VACC_HH

#include "base.hh"

#include "../statement_fwd.hh"
#include "../var_acc_fwd.hh"

#include <string>
#include <ostream>


namespace Expr {
	
	
	class Vacc : public Base {
		
		public:
		
			Var_Acc::Base *var_acc;
			
			
			Vacc(Var_Acc::Base *v, const Loc &l) : Base(VACC, l), var_acc(v) {
			}
			
			
			Vacc(Var_Acc::Base *v) : Base(VACC), var_acc(v) {
			}
			
			
			Vacc(std::string *n);
			Vacc(Statement::Var_Decl &vdecl);
			
			Vacc(std::string* a, std::string *b);
			std::string *name();
			
			void put(std::ostream &s) const;
			
			Statement::Var_Decl *var_decl();
			
			Vacc *vacc();
			
			Base *copy() const;
			
			
	};
	
	
}


#endif

