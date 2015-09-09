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


#ifndef FN_DEF_HH
#define FN_DEF_HH

#include "fn_decl.hh"
#include "algebra.hh"
#include "mode.hh"

#include "expr/fn_call.hh"

#include "hashtable.hh"

#include "bool.hh"

#include <string>
#include <list>

#include "type_fwd.hh"
#include "statement_fwd.hh"
#include "product_fwd.hh"
#include "printer_fwd.hh"
#include "symbol_fwd.hh"

#include "para_decl_fwd.hh"

#include "operator_fwd.hh"

namespace Para_Decl { class Base; }


class Loc;
class Filter;


// A Fn_Def represents a function declaration of an
// algebra function. It extends the Fn_Decl which is
// just the declaration of the function, and adds a
// body of statements and a list of parameter names
// to the definition.
class Fn_Def : public Fn_Decl
{
	
	
	friend class Printer::CC;
	friend class Printer::Cpp;
	
         	
	public:
            
                 enum Gen_Type { STANDARD, NULLARY, CHOICE_SPECIALIZATION };
		                             
		// The list of statements as defined in the function
		// body.
		std::list<Statement::Base*> stmts;
		
		// The list of argument names, in order of
		// appearance in the function signature in the
		// source code.
		std::list<std::string*> names;
		
                // type of the function
                // differentiate between choice functions
                // and nullary functions for specialized ADP
                Gen_Type gen_type;
                
                // strings to name possible sorter and comperator
                std::string* comperator_suffix;
                std::string* sorter_suffix;
           
                std::string* nullary_sort_ob;
                
	private:
		
		// Mapping between parameter names and parameter-type
		// descriptions.
		hashtable<std::string, Type::Base*> parameters;
		
		
	public:
		
		// The list of parameter declarations.
		std::list<Para_Decl::Base*> paras;
		
		
	public:
		
		Fn_Def *copy_head(Type::Base *t, std::string *s);
		
		
		Fn_Def(Type::Base *r, std::string *n, const Loc &l)
			: Fn_Decl(r, n, l), gen_type(STANDARD), comperator_suffix(new std::string("_comperator")),
                        sorter_suffix(new std::string("_sorter")), nullary_sort_ob(NULL),
                        adaptor(NULL), comparator(NULL), sorter(NULL), choice_fn_type_(Expr::Fn_Call::NONE)
		{
		}
		
		
		Fn_Def(Type::Base *r, std::string *n)
			: Fn_Decl(r, n), gen_type(STANDARD), comperator_suffix(new std::string("_comperator")),
                        sorter_suffix(new std::string("_sorter")), nullary_sort_ob(NULL),
                        adaptor(NULL), comparator(NULL), sorter(NULL),  choice_fn_type_(Expr::Fn_Call::NONE)
		{
		}
		
		
		Fn_Def(Fn_Def &a, Fn_Def &b);
		
		
		Fn_Def()
			: Fn_Decl(), gen_type(STANDARD), comperator_suffix(new std::string("_comperator")),
                        sorter_suffix(new std::string("_sorter")), nullary_sort_ob(NULL),
                        adaptor(NULL), comparator(NULL),  sorter(NULL),  choice_fn_type_(Expr::Fn_Call::NONE)
		{
		}
		
		
		Fn_Def(const Fn_Decl &other);
		
		void set_paras(const std::list<Para_Decl::Base*> &l);
		
		void add_para(Type::Base *type, std::string *n);
		void add_paras(const std::list<Statement::Var_Decl*> &l);
		void add_para(Symbol::NT &nt);
		
		void annotate_terminal_arguments(Fn_Decl &d);
		
		void set_statements(const std::list<Statement::Base*> &l);
		
		
	private:
		
		std::string target_name_;
		std::list<Statement::Var_Decl*> v_list;
		std::list<Statement::Var_Decl*> w_list;
		
		void init_var_decl(Para_Decl::Simple *a, Para_Decl::Simple *b, Para_Decl::Simple *c, const std::string &o1, const std::string &o2);
		
		void init_var_decls(Fn_Def &a, Fn_Def &b);
		Fn_Def *adaptor;
                
                // comparator is a stub for the comparator functions needed for yukish
                // sorted Pareto and specialized ADP
                // comparator can also hold the maximal number of comparable dimensions
		Operator *comparator;
                Operator *sorter;
                
                
                            
		void times_cg_with_rhs_choice (Fn_Def &a, Fn_Def &b, Product::Two &product, Statement::Var_Decl *answer, std::list<Statement::Base*> *loop_body, Statement::Var_Decl *elem);
		void times_cg_without_rhs_choice (Fn_Def &a, Fn_Def &b, Product::Two &product, Statement::Var_Decl *answer, std::list<Statement::Base*> *loop_body, Statement::Var_Decl *elem);
		
                bool get_sort_grab_list(std::list<bool> &o, Product::Base &product);
                bool is_pareto_instance(Product::Base &product) ;
		void get_pareto_dimensions(Product::Base &product, std::list<Statement::Base*> &base,
                    int *i, int *D, Statement::Var_Decl *last_decl, std::string prefix,
                    std::list<std::pair<Product::Base*, bool> > &products,
                    std::list<Statement::Var_Decl*> &decls , int float_acc);
                Product::Two codegen_pareto_move_to_first_all_dim(Statement::Var_Decl * & c1, Statement::Var_Decl * & c2, std::list<Statement::Base*> *stmts, Product::Base &product);
                int codegen_pareto_comparator_all_dim(Statement::Var_Decl *c1, Statement::Var_Decl *c2, Statement::Var_Decl *dim, Operator &comp, Product::Base &product );
                            
	public:
		
		void add_simple_choice_fn_adaptor();
		void init_fn_suffix(const std::string &s);
		
		
		const std::string &target_name() const
		{
			return target_name_;
		}
		
		
		void set_target_name(const std::string &s)
		{
			target_name_ = s;
		}
		
		
		void codegen();
		void codegen(Fn_Def &a, Fn_Def &b, Product::Two &product);
                void codegen_sort(Product::Two &product);
                void codegen_sorting_nullary(Product::Two &product);
                void codegen_multi_sort(Product::Base &product, std::list<Statement::Base*> *stmts);
		void codegen_choice(Fn_Def &a, Fn_Def &b, Product::Two &product);
		void codegen_times(Fn_Def &a, Fn_Def &b, Product::Two &product);
                void codegen_pareto_nosort(Fn_Def &a, Fn_Def &b, Product::Two &product);
                void codegen_pareto_multi_nosort(Fn_Def &a, Fn_Def &b, Product::Two &product);
                void codegen_pareto_isort(Fn_Def &a, Fn_Def &b, Product::Two &product);
                void codegen_pareto_multi_lex(Fn_Def &a, Fn_Def &b, Product::Two &product);
                void codegen_pareto_multi_yukish(Fn_Def &a, Fn_Def &b, Product::Two &product, int cutoff, int dim);
                void codegen_pareto_domination_nosort(Fn_Def &a, Fn_Def &b, Product::Two &product);
                void codegen_pareto_lex(Fn_Def &a, Fn_Def &b, Product::Two &product);
		void codegen_nop(Product::Two &product);
		void codegen_cartesian(Fn_Def &a, Fn_Def &b, Product::Two &product);
		void codegen_takeone(Fn_Def &a, Fn_Def &b, Product::Two &product);
		void init_range_iterator();
                void init_range_iterator(Fn_Def &a, Fn_Def &b, Product::Two &product);
                
                void init_comparator_adaptor();
                void init_sorter_adaptor();
                int codegen_compare(Product::Base &product);
                void codegen_sorter(Product::Base &product);
                
                
		void remove_return_list();
		Mode derive_role() const;
		
		Expr::Fn_Call::Builtin choice_fn_type() const;
		
		
                void set_gen_type(Gen_Type t) {
                     gen_type = t;
                }
                Gen_Type get_gen_type() {
                    return gen_type;
                }
  
                
	private:
		
		Expr::Fn_Call::Builtin choice_fn_type_;
		
		
	public:
		
		void set_choice_fn_type(Expr::Fn_Call::Builtin x)
		{
			choice_fn_type_ = x;
		}
		
		
	private:
		
		Mode mode_;
		
		Bool disabled_;
		
		
	public:
		
		void set_mode(const Mode &m)
		{
			mode_ = m;
		}
		
		
		void set_mode(std::string *s);
		
		
		Mode &choice_mode()
		{
			return mode_;
		}
		
		
		const Mode &choice_mode() const
		{
			return mode_;
		}
		
		
		void reduce_return_type();
		
		void install_choice_filter(Filter &filter);
		
		void optimize_classify();
		
		void add_choice_specialization(Fn_Def &a, Fn_Def &b, Product::Two &product);
		
		
		void disable()
		{
			disabled_ = true;
		}
		
		
		bool disabled() const
		{
			return disabled_;
		}
		
		
		void replace_types(std::pair<std::string*, Type::Base*> &alph, std::pair<std::string*, Type::Base*> &answer);
		
		
	private:
		
		std::list<Para_Decl::Base*> ntparas_;
		
		
	public:
		
		void set_ntparas(std::list<Para_Decl::Base*> *l);
		
		
		const std::list<Para_Decl::Base*> &ntparas() const
		{
			return ntparas_;
		}
		
		
		bool check_ntparas(const Fn_Decl &d);
		
		Fn_Def *copy() const;
		
		
};


inline bool operator==(const Fn_Decl &a, const Fn_Def &b)
{
	return a == *dynamic_cast<const Fn_Decl*>(&b);
}


#endif

