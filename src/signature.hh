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


#ifndef SIGNATURE_HH
#define SIGNATURE_HH

#include "hashtable.hh"
#include "loc.hh"
#include "type.hh"

#include "signature_base.hh"
#include "algebra.hh"

#include "mode.hh"


#include <string>
#include <iostream>
#include <cassert>


class Arg;
class Fn_Decl;

class Signature;

class Fn_Def;

class Algebra;

std::ostream &operator<< (std::ostream &s, const Signature &sig);


class Generate_Stmts;


// The signature defines the set of function declarations
// an algebra must implement. It correspongs directly to
// the notion of signature in Bellman's GAP.
class Signature : public Signature_Base {
	
	private:
		
		Algebra *algebra;
		
		
	public:
		
		hashtable <std::string, Arg*> args;
		
		hashtable <std::string, Fn_Decl*> decls;
		hashtable <std::string, Fn_Decl*> choice_fns;
		
		Signature (std::string *n, const Loc &l) : Signature_Base (n, l), algebra (NULL) {}
		
		Fn_Decl* decl (const std::string &s);
		
		void setDecls (hashtable <std::string, Fn_Decl*> &d);
		
		void replace (Type::Base *a, Type::Base *b);
		
		void print();
		
		bool check();
		
		void set_args (hashtable<std::string, Arg*> &h);
		
		Type::Base *var_lookup (const Type::Signature *t);
		Type::Base *var_lookup (const Type::Alphabet *t);
		
		void set_algebra (Algebra *a) {
			assert(!algebra);
			algebra = a;
		}
		
		void reset_algebra() { algebra = NULL; }
		
		Algebra *generate (std::string *n, std::string *mode);
		
		
	private:
		
		Algebra *generate_count (std::string *n);
		Algebra *generate_enum (std::string *n);
		Algebra *generate_algebra (std::string *n, Mode::Type mode_type, Type::Base *answer_type, Type::Base *alph, const Generate_Stmts &generate_stmts);
		Algebra *generate_algebra (std::string *n, Mode::Type mode_type, Type::Base *answer_type, const Generate_Stmts &generate_stmts);
		
		
	public:
		
		Algebra *generate_backtrace (std::string *n, Type::Base *value_type, Type::Base *pos_type, Type::Base *alph);
		
		
};


#endif

