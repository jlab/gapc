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


#ifndef ALGEBRA_HH
#define ALGEBRA_HH


#include "loc.hh"
#include "hashtable.hh"

#include "yieldsize.hh"

#include "signature_base.hh"

#include "product_fwd.hh"
#include "type_fwd.hh"
#include "printer_fwd.hh"

#include "expr.hh"

#include "mode.hh"

#include <string>
#include <ostream>


class Fn_Def;
class Signature;
class Filter;


class Algebra : public Signature_Base
{
	
	public:
		
		
	private:
		
		std::string *default_choice_fn_mode;
		void init_choice_fns();
		
		
	public:
		
		hashtable<std::string, Fn_Def*> fns;
		hashtable<std::string, Fn_Def*> choice_fns;
		hashtable<std::string, Type::Base*> params;
		Signature *signature;
		std::string *signature_name;
                
		Fn_Def *choice_fn(Fn_Def *f);
		
		Algebra(std::string *n, std::string *s, Loc l)
			:	Signature_Base(n, l), default_choice_fn_mode(0),
				signature(NULL), signature_name(s)
		{
		}
		
		
		Algebra(Algebra &a, Algebra &b);
		
		
		Algebra(std::string *n, Loc l)
			:	Signature_Base(n, l), default_choice_fn_mode(0),
				signature(NULL), signature_name(NULL)
		{
		}
		
		
		Algebra(std::string *n)
			:	Signature_Base(n), default_choice_fn_mode(0),
				signature(NULL), signature_name(NULL)
		{
		}
		
		
		Algebra()
			:	Signature_Base(), default_choice_fn_mode(0),
				signature(NULL), signature_name(NULL)
		{
		}
		
		
		Fn_Decl* decl(const std::string &s);
		
		
		Algebra& operator= (const Algebra& a)
		{
			fns = a.fns;
			choice_fns = a.choice_fns;
			params = a.params;
			signature = a.signature;
			signature_name = new std::string(*(a.signature_name));
			default_choice_fn_mode = a.default_choice_fn_mode;
			return *this;
		}
		
		
		void set_params(hashtable<std::string, Type::Base*> *p);
		static void add_sig_var(hashtable<std::string, Type::Base*> &h, std::pair<std::string*, Type::Base*> &p, const Loc &l);
		
		bool check_signature(Signature &s);
		
		void set_fns(const hashtable<std::string, Fn_Def*> &h);
		
		Fn_Def *fn_def(const std::string &name);
		
		bool check_params(Signature &s);
		
		std::ostream &put(std::ostream &s) const;
		
		void annotate_terminal_arguments(Signature &s);
		
		void codegen(Product::Two &product);
		void codegen();
		void init_fn_suffix(const std::string &s);
		
		void print_code(Printer::Base &s);
		
		void derive_role();
		
		void set_default_choice_fn_mode(std::string *s);
		
		Type::Base *answer_type();
		
		bool is_compatible(Mode::Type t);
		
		void install_choice_filter(Filter &filter);
		
		void add_choice_specialisations(Product::Two &product);
		
		Algebra *copy() const;
		
		
};

inline std::ostream &operator<<(std::ostream &s, const Algebra &a)
{
	return a.put(s);
}

#endif
