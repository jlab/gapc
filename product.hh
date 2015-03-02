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


#ifndef PRODUCT_HH
#define PRODUCT_HH

#include "loc.hh"
#include "algebra.hh"
#include "tree_iterator.hh"
#include "mode.hh"

#include "hashtable.hh"
#include <string>
#include <list>

#include "const_fwd.hh"
#include "printer_fwd.hh"

#include "bool.hh"

#include "expr/fn_call.hh"

class Default;
class Filter;


namespace Product {
	
	
	enum Type { SINGLE, TIMES, KLASS, CARTESIAN, NOP, OVERLAY, TAKEONE, PARETO};
	
	
	class Base {
			
		private:
			
			Type type_;
			
			
		protected:
			
			Loc location;
			Algebra *algebra_;
			Algebra *bt_score_algebra_;
			
			
		public:
			
			Algebra *algebra() { return algebra_; }
			Algebra *bt_score_algebra();
			
			
		protected:
			
			std::string fn_suffix;
			
			Filter *filter_;
			
			Bool eliminate_lists_computed;
			
			Base(Type t, const Loc &l);
			Base(Type t);
			
			
		public:
			
			virtual ~Base();
			
			bool is(Type t) { return type_ == t; }
			
			Type type() const { return type_; }
			
			std::list<Default*> defaults;
			
			bool check_defaults();
			
			void set_algebra(Algebra *a) { algebra_ = a; }
			
			virtual bool init() = 0;
			
			void reduce_return_type();
			virtual void eliminate_lists() = 0;
			
			virtual void init_fn_suffix(const std::string &p) = 0;
			
			virtual void codegen() = 0;
			
			virtual void print_code(Printer::Base &s) = 0;
			
			
			virtual Algebra *nth_algebra(unsigned int &n) = 0;
			//virtual Var_Acc:Base *nth_access(unsigned int n) = 0;
			virtual unsigned int width() = 0;
			virtual bool contains_only_times();
			
			virtual Base *left();
			virtual Base *right();
			
			virtual Base * left_most();
			virtual Base * right_most();
			
			virtual Base * optimize_shuffle_products();
			
			virtual void collect_fns(std::list<Fn_Def*> &l, const std::string &name);
			
			void set_filter(Filter *f) { assert(!filter_); filter_ = f; }
			Filter *filter() { return filter_; }
			
			
			//FIXME protected:
		public:
			
			void install_choice_filter();
			
			virtual bool contains(Type t);
			
			virtual void set_in_use(const Fn_Decl &);
			
			
		protected:
			
			static Bool no_coopt_;
			static Bool no_coopt_class_;
			
			Var_Acc::Base *src_vacc;
			Var_Acc::Base *dst_vacc;
			void generate_filter_decl(
			std::list<Statement::Base*> &hash_code,
			std::list<Statement::Var_Decl*> &filters) const;
			
			
		public:
			
			virtual void init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst);
			virtual void generate_hash_decl(const Fn_Def &fn,
				std::list<Statement::Base*> &hash_code,
				std::list<Statement::Var_Decl*> &filters,
				std::list<Statement::Base*> &finalize_code,
				std::list<Statement::Base*> &init_code,
				std::list<Statement::Base*> &equal_score_code,
				std::list<Statement::Base*> &compare_code
				) const;
			
			bool left_is_classify();
			bool one_per_class();
			
			static void set_no_coopt()
			{
				no_coopt_ = true;
			}
			static bool no_coopt() { return no_coopt_; }
			
			static void set_no_coopt_class()
			{
				no_coopt_class_ = true;
			}
			static bool no_coopt_class() { return no_coopt_class_; }
			
			virtual Base *replace_classified(bool &x) = 0;
			
			
	};
	
	
	class Single : public Base {
			
		private:
			
			std::string *name_;
			
			
		public:
			
			Single(std::string *n, const Loc &l) : Base(SINGLE, l), name_(n) { }
			Single(Algebra *a);
			
			bool init();
			void eliminate_lists();
			
			void init_fn_suffix(const std::string &p);
			void codegen();
			
			void print_code(Printer::Base &s);
			
			unsigned int width();
			Algebra *nth_algebra(unsigned int &n);
			bool contains_only_times();
			
			Base * left_most();
			Base * right_most();
			
			const std::string &name() const { assert(name_); return *name_; }
			
			void init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst);
			void generate_hash_decl(const Fn_Def &fn,
				std::list<Statement::Base*> &hash_code,
				std::list<Statement::Var_Decl*> &filters,
				std::list<Statement::Base*> &finalize_code,
				std::list<Statement::Base*> &init_code,
				std::list<Statement::Base*> &equal_score_code,
				std::list<Statement::Base*> &compare_code
				) const;
			
			Base *replace_classified(bool &x);
			
			
	};
	
	
	class Two : public Base {
			
		protected:
			
			Base *l;
			Base *r;
			
			
		public:
			
			Two(Type t, const Loc &lo, Base *a, Base *b) : Base(t, lo), l(a), r(b) {}
			Two(Type t, Base *a, Base *b) : Base(t), l(a), r(b) {}
			Two(Type t, const Two &x);
			bool init();
			void eliminate_lists();
			
			void init_fn_suffix(const std::string &p);
			void codegen();
			
			void print_code(Printer::Base &s);
			
			Base *left() { return l; }
			Base *right() { return r; }
			
			unsigned int width();
			Algebra *nth_algebra(unsigned int &n);
			
			Base * left_most();
			Base * right_most();
			Base *optimize_shuffle_products();
			
			Mode & left_mode(const std::string &s);
			Mode & right_mode(const std::string &s);
                        
                        Expr::Fn_Call::Builtin right_choice_fn_type(const std::string &s) const;
			Expr::Fn_Call::Builtin left_choice_fn_type(const std::string &s) const;
                        
			void collect_fns(std::list<Fn_Def*> &l, const std::string &name);
			
			bool contains(Type t);
			
			void set_in_use(const Fn_Decl &);
			
			void init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst);
			
			Base *replace_classified(bool &x);
			
			
	};
	
	
	class Times : public Two {
			
		private:
			
			
		public:
			
			Times(Base *a, Base *b, const Loc &lo);
			Times(Base *a, Base *b) : Two(TIMES, a, b) { }
			Times(const Two &t);
			bool init();
			bool contains_only_times();
			
			Base *optimize_shuffle_products();
			
			void generate_hash_decl(const Fn_Def &fn,
				std::list<Statement::Base*> &hash_code,
				std::list<Statement::Var_Decl*> &filters,
				std::list<Statement::Base*> &finalize_code,
				std::list<Statement::Base*> &init_code,
				std::list<Statement::Base*> &equal_score_code,
				std::list<Statement::Base*> &compare_code
				) const;
			
			
	};
	
	
	class Klass : public Two {
			
		private:
			
			Const::Number *parameter;
			
			
		public:
			
			Klass(Base *a, Base *b, const Loc &l) : Two(KLASS, l, a, b),
			parameter(NULL) { }
			bool init();
			
			Base *replace_classified(bool &x);
			
			
	};
	
	
	class Cartesian : public Two {
			
		public:
			
			Cartesian(Base *a, Base *b, const Loc &l) : Two(CARTESIAN, l, a, b) {}
			bool init();
			
			void generate_hash_decl(const Fn_Def &fn,
				std::list<Statement::Base*> &hash_code,
				std::list<Statement::Var_Decl*> &filters,
				std::list<Statement::Base*> &finalize_code,
				std::list<Statement::Base*> &init_code,
				std::list<Statement::Base*> &equal_score_code,
				std::list<Statement::Base*> &compare_code
				) const;
			
			
	};
	
	
	class Nop : public Two {
			
		public:
			
			Nop(Times &times);
			bool contains_only_times();
			
			
	};
	
	
	class Overlay : public Two {
			
		public:
			
			Overlay(Base *a, Base *b, const Loc &l) : Two(OVERLAY, l, a, b) {}
			bool init();
			bool contains_only_times();
			
			void eliminate_lists();
			void codegen();
			void print_code(Printer::Base &s);
			void init_fn_suffix(const std::string &p);
			
			
	};
	
	
	class Takeone : public Two {
			
		public:
			
			Takeone(Base *a, Base *b, const Loc &l) : Two(TAKEONE, l, a, b) {}
			bool init();
			
			
	};
	
        
        class Pareto : public Two {
                public: 
                    enum ParetoType {NoSort, Sort, ISort};
            
		private:

                    ParetoType pareto_type;
                    
		public:
                    			
			Pareto(Base *a, Base *b, const Loc &l) :
                           Two(PARETO, l, a, b), pareto_type(NoSort) {}
                           
			bool init();
                        
                        void set_pareto_type(int i);
                        
                        void generate_hash_decl(const Fn_Def &fn,
				std::list<Statement::Base*> &hash_code,
				std::list<Statement::Var_Decl*> &filters,
				std::list<Statement::Base*> &finalize_code,
				std::list<Statement::Base*> &init_code,
				std::list<Statement::Base*> &equal_score_code,
				std::list<Statement::Base*> &compare_code
				) const;
                        
                        ParetoType get_pareto_type() {
                            return pareto_type;
                        };
	
	};
	
	typedef Tree::Iterator<Base, Two> iterator;
	
	
	inline iterator begin(Base *b) { return iterator(b); }
	inline iterator end() { return iterator(); }
	
	
}

#endif
