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


#ifndef INSTANCE_HH
#define INSTANCE_HH

#include "loc.hh"

#include <string>

#include "printer_fwd.hh"
#include "statement_fwd.hh"


class Grammar;
namespace Product { class Base; };
namespace yy { class Parser; }
class Algebra;
class Fn_Def;


class Instance {
	
		friend class yy::Parser;
	
	private:
		
		std::string *name_;
		
		
	public:
		
		Product::Base *product;
		
		
	private:
		
		Loc location;
		Grammar *grammar_;
		
		
	public:
		
		Instance(std::string *n, Product::Base *p, const Loc &l);
		Instance(std::string *n, Product::Base *p, Grammar *g);
		Instance(Algebra *a, Algebra *b);
                Instance(Product::Base *a, Algebra *b);
               
		
		const Loc &loc() const { return location; }
		
		void set_grammar(Grammar *g) { grammar_ = g; }
		Grammar *grammar() { return grammar_; }
		
		bool init(Instance *instance);
		
		void eliminate_lists();
		
		std::ostream &put(std::ostream &s) const;
		
		void codegen();
		
		void print_code(Printer::Base &s);
		
		std::string *lookup(const std::string &n);
		
		std::string *name() { return name_; }
		
		Statement::Hash_Decl *generate_hash_decl(const Fn_Def &fn, bool kbest);
		
		bool replace_classified_product();
		
		void check_alphabets();
		
		
};


inline std::ostream &operator<<(std::ostream &s, const Instance &i)
{
	return i.put(s);
}


#endif
