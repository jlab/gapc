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


#ifndef FN_ARG_HH
#define FN_ARG_HH

#include "runtime.hh"
#include "table.hh"
#include "loc.hh"
#include "grammar.hh"

// because of Alt::Type
#include "alt.hh"

#include "alt_fwd.hh"
#include "const_fwd.hh"
#include "expr_fwd.hh"
#include "statement_fwd.hh"
#include "type_fwd.hh"


class Visitor;


namespace Fn_Arg {
	
	
	enum Type { ALT, CONST };
	
	
	class Base {
			
		private:
			
			Type type;
			
			
		protected:
			
			bool productive;
			::Type::Base *datatype;
			
			bool eliminated;
			
			
		public:
			
			bool terminal_type;
			
			
		protected:
			
			std::list<Statement::Base*> statements_;
			Base(Type t, const Loc &l);
			
			
		public:
			
			virtual ~Base();
			virtual Base *clone() = 0;
			
			Loc location;
			std::vector<Expr::Base*> left_indices;
			std::vector<Expr::Base*> right_indices;
			void set_tracks(size_t t);
			
			
		protected:
			
			std::vector<Statement::Var_Decl*> ret_decls_;
			std::vector<Statement::Var_Decl*> var_decls_;
			
			
		public:
			
			Statement::Var_Decl *ret_decl();
			Statement::Var_Decl *var_decl();
			const std::vector<Statement::Var_Decl*> &ret_decls() const
			{ return ret_decls_; }
			const std::vector<Statement::Var_Decl*> &var_decls() const
			{ return var_decls_; }
			
			bool is(Type t) const { return type == t; }
			Type getType() { return this->type; }
			
			virtual bool init_links(Grammar &grammar);
			virtual bool init_productive() = 0;
			bool is_productive() { return productive; }
			
			virtual size_t width() = 0;
			
			virtual bool is(::Alt::Type t) = 0;
			virtual ::Alt::Base *alt_ref() { assert(false); return 0; }
			virtual const ::Alt::Base *alt_ref() const { assert(false); return 0; }
			virtual void print_link(std::ostream &s) = 0;
			
			virtual Runtime::Poly runtime(std::list<Symbol::NT*> &active_list, Runtime::Poly accum_rt) = 0;
			virtual Runtime::Poly init_in_out() = 0;
			
			virtual bool set_data_type(::Type::Base *t, const Loc &l) = 0;
			virtual ::Type::Status infer_missing_types() = 0;
			::Type::Base *data_type() { return datatype; }
			virtual void print_type(std::ostream &s) = 0;
			
			virtual bool returns_list() = 0;
			virtual ::Type::Base *ext_data_type() = 0;
			bool is_eliminated() { return eliminated; }
			
			void reset_types();
			
			virtual const Yield::Poly& list_size() const = 0;
			
			virtual void traverse(Visitor &v) = 0;
			virtual void init_indices(Expr::Base *left, Expr::Base *right,
			unsigned int &k, size_t track);
			
			virtual void init_ret_decl(unsigned int i, const std::string &prefix);
			
			virtual void codegen(AST &ast) = 0;
			virtual std::list<Statement::Base*> &statements() = 0;
			
			virtual void print_dot_edge(std::ostream &out, Symbol::NT &nt);
			
			virtual void print(std::ostream &s) = 0;
			
			virtual bool choice_set() = 0;
		protected:
			
			Yield::Multi m_ys;
			
			
		public:
			
			virtual void init_multi_ys() = 0;
			virtual const Yield::Multi &multi_ys() const { return m_ys; }
			
			
	};
	
	
	class Alt : public Base {
			
		public:
			
			::Alt::Base *alt;
			Alt(::Alt::Base *a, const Loc &l) : Base(ALT, l), alt(a) {}
			
			Base *clone();
			
			bool init_links(Grammar &grammar);
			bool init_productive();
			
			size_t width();
			
			bool is(::Alt::Type t);
			::Alt::Base *alt_ref() { return alt; }
			const ::Alt::Base *alt_ref() const { return alt; }
			void print_link(std::ostream &s);
			
			Runtime::Poly runtime(std::list<Symbol::NT*> &active_list, Runtime::Poly accum_rt);
			
			Runtime::Poly init_in_out();
			
			bool set_data_type(::Type::Base *t, const Loc &l);
			::Type::Status infer_missing_types();
			void print_type(std::ostream &s);
			
			bool returns_list();
			::Type::Base *ext_data_type();
			const Yield::Poly& list_size() const;
			
			void traverse(Visitor &v);
			void init_indices(Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track);
			
			void codegen(AST &ast);
			std::list<Statement::Base*> &statements();
			
			void print_dot_edge(std::ostream &out, Symbol::NT &nt);
			
			void print(std::ostream &s);
			
			void init_multi_ys();
			const Yield::Multi &multi_ys() const;
			
			void init_ret_decl(unsigned int i, const std::string &prefix);
			bool choice_set();
			
	};
	
	
	class Const : public Base {
			
		private:
			
			Yield::Size ys;
			Yield::Poly list_size_;
			::Const::Base *expr_;
			
			
		public:
			
			Const (::Const::Base *e, const Loc &l);
			
			Base *clone();
			
			::Const::Base &expr() { assert(expr_); return *expr_; }
			bool init_productive();
			
			size_t width();
			
			bool is(::Alt::Type t);
			void print_link(std::ostream &s);
			
			Runtime::Poly runtime(std::list<Symbol::NT*> &active_list, Runtime::Poly accum_rt);
			
			Runtime::Poly init_in_out();
			
			bool set_data_type(::Type::Base *t, const Loc &l);
			::Type::Status infer_missing_types();
			void print_type(std::ostream &s);
			
			bool returns_list();
			::Type::Base *ext_data_type();
			const Yield::Poly& list_size() const;
			
			void traverse(Visitor &v);
			void init_indices(Expr::Base *left, Expr::Base *right,
			unsigned int &k, size_t track);
			
			void codegen(AST &ast);
			std::list<Statement::Base*> &statements();
			
			void print(std::ostream &s);
			
			void init_multi_ys();
			bool choice_set();
			
	};
	
	
}

#endif
