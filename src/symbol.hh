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


#ifndef SYMBOL_HH
#define SYMBOL_HH

#include <list>

#include "grammar.hh"
#include "yieldsize.hh"
#include "runtime.hh"
#include "table.hh"
#include "loc.hh"
#include "alt.hh"

#include "type.hh"

#include "codegen.hh"

#include <algorithm>

#include "bool.hh"

#include "alt_fwd.hh"
#include "expr_fwd.hh"
#include "statement_fwd.hh"
#include "para_decl_fwd.hh"
#include "adp_mode.hh"


class Fn_Decl;
class Signature_Base;
class Visitor;
class Fn_Def;


/*
 * This namespace defines three classes that are used in the AST
 * to model the terminals and non-terminals of a grammar. Both types
 * (terminal and non-terminal) are extended from the same base class
 * named Base.
 */
namespace Symbol {
	
	
	class NT;
	
	
	enum Type { TERMINAL, NONTERMINAL };
	
        
	class Base {
		
		private:
			
			Type type;
			
		protected:
                        
                        ADP_Mode::Adp_Specialization adp_specialization;
                        ADP_Mode::Adp_Join adp_join;
                    
			Bool never_tabulate_;
			bool tabulated;
			bool reachable;
			bool productive;
			unsigned int self_rec_count;
			
			
		public:
			
			bool active;
			bool self_rec_started;
			
			unsigned int selfreccount() const
			{
				return self_rec_count;
			}
			
			bool never_tabulate() const
			{
				return never_tabulate_;
			}
			
			
		protected:
			
			Runtime::Poly in_calls;
			Runtime::Poly out_calls;
			
			::Type::Base *datatype;
			
			bool eliminated;
			
			
		public:
			
			bool terminal_type;
			
			const Runtime::Poly &incalls() const
			{
				return in_calls;
			}
			const Runtime::Poly &outcalls() const
			{
				return out_calls;
			}
			
			
		protected:
			
			Yield::Poly list_size_;
			bool rt_computed;
			
			Base(std::string *n, Type t, const Loc &l);
			
			
		public:
			
			virtual ~Base();
			std::string *name, *orig_name;
			Loc location;
			std::vector<Expr::Base*> left_indices;
			std::vector<Expr::Base*> right_indices;
			bool is(Type t) const
			{
				return type == t;
			}
                        
                        void set_adp_specialization(ADP_Mode::Adp_Specialization a) {
                            adp_specialization = a;
                        }
                        ADP_Mode::Adp_Specialization get_adp_specialization() {
                            return adp_specialization;
                        }
                        
                        void set_adp_join(ADP_Mode::Adp_Join a) {
                            adp_join = a;
                        }
                        ADP_Mode::Adp_Join get_adp_join() {
                            return adp_join;
                        }
                        
			bool is_reachable() const
			{
				return reachable;
			}
			
			virtual bool init_links(Grammar &grammar) = 0;
			
			bool is_productive()
			{
				return productive;
			}
			virtual bool init_productive() = 0;
			
			virtual size_t width() = 0;
			
			virtual void init_table_dim(const Yield::Size &a, const Yield::Size &b, std::vector<Yield::Size> &temp_ls, std::vector<Yield::Size> &temp_rs, size_t track);
			
			virtual void print_link(std::ostream &s) = 0;
			
			virtual void clear_runtime() = 0;
			virtual Runtime::Poly runtime(std::list<NT*> &active_list, const Runtime::Poly &accum_rt) = 0;
			bool is_tabulated()
			{
				return tabulated;
			}
			void set_tabulated();
			void set_tabulated(bool b);
			
			virtual std::ostream & put(std::ostream &s) const = 0;
			
			void reset_in_out()
			{
				in_calls = 0;
				out_calls = 0;
			}
			
			Runtime::Poly score() const 
			{
				assert(in_calls > 0);
				assert(out_calls > 0);
				return in_calls * out_calls;
			}
			
			virtual void init_in_out(const Runtime::Poly &p) = 0;
			virtual void init_in_out() = 0;
			virtual void put_table_conf(std::ostream &s);
			
			virtual void init_self_rec();
			
			::Type::Base *data_type()
			{
				return datatype;
			}
			bool set_data_type(::Type::Base *t, const Loc &l);
			bool set_data_type(::Type::Base *t);
			virtual bool insert_types(Signature_Base &s) = 0;
			virtual ::Type::Status infer_missing_types() = 0;
			virtual void print_type(std::ostream &s) = 0;
			
			virtual bool eliminate_lists() = 0;
			bool is_eliminated()
			{
				return eliminated;
			}
			
			
			const Yield::Poly &list_size() const
			{
				return list_size_;
			}
			void set_list_size(const Yield::Poly &p)
			{
				list_size_ = p;
			}
			virtual bool init_list_sizes() = 0;
			
			virtual void traverse(Visitor &v) = 0;
			
			virtual void print_dot_edge(std::ostream &out) = 0;
			void print_dot_node(std::ostream &out);
			
			
		protected:
			
			size_t tracks_;
			size_t track_pos_;
			
			
		public:
			
			size_t tracks() const
			{
				return tracks_;
			}
			virtual void set_tracks(size_t x, size_t y);
			size_t track_pos() const
			{
				return track_pos_;
			}
			
			
		protected:
			
			Yield::Multi m_ys;
			
			
		public:
			
			virtual void setup_multi_ys() = 0;
			virtual void init_multi_ys() = 0;
			const Yield::Multi &multi_ys() const
			{
				return m_ys;
			}
			
			
		public:
			
			virtual bool multi_detect_loop(const Yield::Multi &left, const Yield::Multi &right, Symbol::NT *nt);
			virtual bool multi_detect_loop();
			
			
	};
	
	
	class Terminal : public Base {
		
		private:
			
			Yield::Size ys;
			
			// This flag is set TRUE, if the instance represents
			// a predefined terminal parser, as those which are
			// initialized in the method
			// Terminal::add_predefined(Grammar &grammar).
			bool predefinedTerminalParser;
			
			
		public:
			
			Terminal(std::string *n, const Loc &l);
			
			bool init_links(Grammar &grammar);
			
			bool init_productive();
			
			size_t width();
			
			Yield::Size &writeable_ys()
			{
				return ys;
			}
			
			void print_link(std::ostream &s);
			
			void clear_runtime();
			Runtime::Poly runtime(std::list<NT*> &active_list, const Runtime::Poly &accum_rt);
			
			std::ostream & put(std::ostream &s) const;
			
			void init_in_out(const Runtime::Poly &p);
			void init_in_out();
			
			bool insert_types(Signature_Base &s);
			::Type::Status infer_missing_types();
			void print_type(std::ostream &s);
			
			bool eliminate_lists();
			bool init_list_sizes();
			
			void traverse(Visitor &v);
			
			void print_dot_edge(std::ostream &out);
			
			void force_type(::Type::Base* t)
			{
				datatype = t;
			}
			
			void setup_multi_ys();
			void init_multi_ys();
			
			void setPredefinedTerminalParser (bool isPredefined);
			bool isPredefinedTerminalParser();
			
			
	};
	
	
	/*
	 * A non-terminal is basically a list of alternatives (or just
	 * one) sequence of terminal or non-terminal parses. The main
	 * used when iterating through the grammar tree is the (alas)
	 * public field "alts".
	 */
	class NT : public Base {
		
		private:
			
			// Rang-number of a non-terminal. This number is used
			// for access into list like structure when the yield
			// size of this non-terminal is computed.
			size_t grammar_index_;
			
			bool recompute;
			Runtime::Asm::Poly rec;
			Runtime::Poly runtime_;
			
			bool tab_dim_ready;
			
                        
			
		//private:
		public:
			
			// A non-terminal defines a list of alternative grammar
			// rules. There is one pointer stored for each alternative.
			std::list<Alt::Base*> alts;
			void set_alts(const std::list<Alt::Base*> &a);
			
			
		public:
			
			std::string *eval_fn;
			Fn_Decl *eval_decl;
                        
                        std::string *eval_nullary_fn;
                        std::string *specialised_comparator_fn;
                        std::string *specialised_sorter_fn;
                        
                        Statement::Var_Decl* marker; 
			
			std::list<Statement::If*> guards;
			
			NT(std::string *n, const Loc &l);
			
			NT *clone(size_t track_pos);
			
			void set_grammar_index(size_t i)
			{
				grammar_index_ = i;
			}
			
			bool has_eval_fn()
			{
				return eval_fn;
			}
			
			void set_eval_fn(std::string *n);
			bool set_eval_decl(Signature_Base &s);
			
			bool init_links(Grammar &grammar);
			
			bool init_productive();
			
			void collect_lr_deps(std::list<NT*> &list);
			
			size_t width();
			
                        void set_adp_specialization(ADP_Mode::Adp_Specialization a, std::string s_null, std::string s_comp, std::string s_comp_dim);
			
		private:
			
			std::vector<Table> table_dims;
			
			
		public:
			
			const std::vector<Table> &tables() const
			{
				return table_dims;
			}
			
			
			void init_table_dim(const Yield::Size &a, const Yield::Size &b,
			std::vector<Yield::Size> &temp_ls,
			std::vector<Yield::Size> &temp_rs,
			size_t track);
			
			void print_link(std::ostream &s);
			
			void clear_runtime();
			Runtime::Poly runtime(std::list<NT*> &active_list, const Runtime::Poly &accum_rt);
			
			std::ostream & put(std::ostream &s) const;
			
			bool operator<(const NT &b) const;
			
			void init_in_out(const Runtime::Poly &p);
			void init_in_out();
			void put_table_conf(std::ostream &s);
			
			void init_self_rec();
			
			bool insert_types(Signature_Base &s);
			::Type::Status infer_missing_types();
			void print_type(std::ostream &s);
			
			bool eliminate_lists();
			void reset_types();
			bool init_list_sizes();
			
			void traverse(Visitor &v);
			
			void inline_nts(Grammar *grammar);
			bool is_inlineable();
			
			void init_indices(Expr::Vacc *left, Expr::Vacc *right,
			unsigned int &k, size_t track);
			
			
			void gen_ys_guards(std::list<Expr::Base*> &ors) const;
			void init_guards(Code::Mode mode);
			void put_guards(std::ostream &s);
			
			
		private:
			
			Statement::Var_Decl *ret_decl;
			
			
		public:
			
			Statement::Var_Decl &return_decl()
			{
				assert(ret_decl);
				return *ret_decl;
			}
			Statement::Table_Decl *table_decl;
			Statement::Var_Decl *zero_decl;
			
			
		private:
			
			std::list<Statement::Base*> post_alt_stmts;
			std::list<Fn_Def*> code_;
			std::list<Statement::Base*> ret_stmts;
			::Type::Base *data_type_before_eval();
                        void add_specialised_arguments(Statement::Fn_Call *fn, bool keep_coopt);
			void set_ret_decl_rhs(Code::Mode mode);
			void init_ret_stmts(Code::Mode mode);
			std::list<Statement::Base*> table_guard;
			void init_table_decl(const AST &ast);
			void init_table_code(const Code::Mode &mode);
			void init_zero_decl();
			
			void replace(Statement::Var_Decl &decl, Statement::iterator begin, Statement::iterator end);
			void eliminate_list_ass();
			void add_cyk_stub(AST &ast);
			void subopt_header(AST &ast, Fn_Def *score_code, Fn_Def *f, std::list<Statement::Base*> &stmts);
			
			
		public:
			
			void codegen(AST &ast);
			Fn_Def *code()
			{
				return code_.front();
			}
			std::list<Fn_Def*> & code_list()
			{
				return code_;
			}
			
			void print_dot_edge(std::ostream &out);
			
			void optimize_choice(::Type::List::Push_Type push);
			void optimize_choice(::Type::List::Push_Type push, Statement::Hash_Decl *h);
			
			
		private:
			
			void set_rec(const Runtime::Poly &c);
			void set_recs(const Runtime::Poly &c, std::list<NT*> &active_list);
			
			Statement::Base *build_return_empty(const Code::Mode &mode);
			
			void marker_cond(Code::Mode &mode, std::list<Expr::Base*> &cond) const;
			void marker_code(const Code::Mode &mode, std::list<Statement::Base*> &ret_stmts, Expr::Base *v) const;
			
			
		public:
			
			void set_tracks(size_t x, size_t y);
			
			void setup_multi_ys();
			void init_multi_ys();
			
			
		public:
			
			bool multi_detect_loop(const Yield::Multi &left, const Yield::Multi &right, Symbol::NT *nt);
			bool multi_detect_loop();
			
			void multi_propagate_max_filter(std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size);
			void multi_propagate_max_filter(std::vector<Yield::Multi> &nt_sizes);
			void update_max_ys(const Yield::Multi &m);
			
			void multi_init_calls();
			
			void window_table_dim();
			
			
		private:
			
			std::list<Para_Decl::Base*> ntargs_;
			
			
		public:
			
			void set_ntargs(std::list<Para_Decl::Base*> *l);
			const std::list<Para_Decl::Base*> &ntargs() const
			{
				return ntargs_;
			}
			
			
	};
	
	
}


std::ostream & operator<<(std::ostream &s, const Symbol::Base &p);


#endif
