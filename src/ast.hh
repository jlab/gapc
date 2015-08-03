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


#ifndef AST_H
#define AST_H

#include <list>
#include <string>
#include <sstream>
#include <ostream>

#include <set>

#include "hashtable.hh"

#include "log.hh"
#include "loc.hh"
#include "yieldsize.hh"
#include "grammar.hh"
#include "symbol.hh"
#include "type.hh"

#include "fn_decl.hh"

#include "algebra.hh"

#include "input.hh"

#include "codegen.hh"

#include "bool.hh"

#include "expr_fwd.hh"
#include "statement_fwd.hh"
#include "printer_fwd.hh"
#include "product.hh"


class Signature;
class Instance;
class Backtrack_Base;

class Options;


class Import {
		
	private:
	
	
	public:
	
		std::string *name;
		bool verbatimDeclaration;
		Loc location;
		
		// Inits a new instance with the given module name and
		// its source code location of the line which triggered
		// creation of this instance.
		Import (std::string *s, const Loc &l) : name(s) {
			this->location = l;
			this->verbatimDeclaration = false;
		}
		
		// Inits a new instance with the given module name, a boolean
		// flag that is set true if the module name is to be taken
		// as verbatim text for the generated "#import" declaration
		// of the generated C++ output, and the source code location
		// of the line which triggered creation of this instance.
		Import (std::string *s, bool verbatimDeclaration, const Loc &l)
			: name(s), verbatimDeclaration (verbatimDeclaration) {
			this->location = l;
		}
		
};


class Default {
  // FIXME

};


class AST {
	
	private:
	
		Code::Mode cg_mode;
		std::list<Statement::Hash_Decl*> hash_decls_;
		
		Product::Base *product_;
		
		std::list<Grammar*> *grammars_;
		Grammar *selected_grammar;
		
                Product::Sort_Type back_track_paretosort;
                
                static const std::string duplicate_suffix;
                static const std::string comperator_suffix;
                static const std::string sorter_suffix;
                
                ADP_Mode::Adp_Specialization adp_specialization;
                ADP_Mode::Adp_Join adp_join;
                
                ADP_Mode::Rtlib_Header rtlib_header;
                
                int pareto_cutoff;
                int float_acc;
	public:
	
		AST();
		
		void set_product(Product::Base *p) { product_ = p; }
		Product::Base *product() { return product_; }
		
		// The list of all import-declarations in a gapc-program.
		// this list is extended directly by the parser (which is
		// generated from the parser description file parser.y).
		std::list<Import*> imports;
		
		// Stores the input declaration of a gapc program.
		Input input;
		
		// The signature of a gapc program.
		Signature *signature;
		
		// A table filled with all defined algebras of a gapc
		// program.
		hashtable<std::string, Algebra*> algebras;
		// A set of all algebras seen so far by an algorithm.
		// This field is used by the functions TODO: which
		// functions use that field?
		std::set<std::string> algebra_seen;
		
		void set_grammars(std::list<Grammar*> *);
		bool grammar_defined(const std::string &n) const;
		Grammar *grammar() const;
		Grammar *grammar(const std::string&);
		void select_grammar(const std::string &instname);
	
		
		hashtable<std::string, Instance*> instances;
		Instance *first_instance;
		Instance *instance_;
		
		// The table "types" and the list "type_def_list" are
		// filled with type declarations given in a gapc source
		// code file. Although their access is public, both are
		// set by the parser (see parser.y) through the function call
		// add_type(std::string *name, const Loc &l, Type::Base *base).
		hashtable<std::string, Type::Base*> types;
		// See comment directly above.
		std::list<Type::Base*> type_def_list;
		
		hashtable<std::string, Fn_Decl*> filters;
		
		std::vector<Statement::Var_Decl*> seq_decls;
		
		Type::Base *get_type(const std::string &name, const Loc &l);
		Type::Base *get_type(const std::string &name);
		void add_type(std::string *name, const Loc &l, Type::Base *base);
		void add_sig_types(hashtable<std::string, Arg*> & args, Signature *s);
		
		bool check_signature();
		
		bool check_algebras();
		
		bool check_instances(Instance *instance);
		
		void print_instances(std::ostream &s);
		
		Instance *instance(const std::string &n);
		bool insert_instance(std::string &n);
		bool insert_instance(Instance *inst);
		bool instance_grammar_eliminate_lists(std::string &n);
		bool instance_grammar_eliminate_lists(Instance *inst);
		void warn_missing_choice_fns(Instance *instance);
		void warn_missing_choice_fns();
		// Checks the runtime  of the selected table configuration,
		// and prints out a message if not.
		void warn_user_table_conf_suboptimal();
		
		// Tests for optimal table design and returns TRUE if
		// the selected table design has optimal runtime, FALSE
		// otherwise.
		bool tableDesignIsOptimal();
		
		void codegen();
		
		void print_code(Printer::Base &out);
		
		void derive_roles();
		
		void optimize_choice(Instance &i);
		void optimize_classify(Instance &i);
                
		// FIXME probably remove these get/setters
		bool cyk() const { return cg_mode == Code::Mode::CYK; }
		void set_cyk() { cg_mode = Code::Mode::CYK; }
		
		bool backtrace() const { return cg_mode == Code::Mode::BACKTRACK; }
		void set_backtrace(bool b = true) { cg_mode = Code::Mode::BACKTRACK; }
		
		const Code::Mode & code_mode() const { return cg_mode; }
                Code::Mode & code_mode() { return cg_mode; }
		
		void set_code_mode(const Code::Mode &m) { cg_mode = m; }
	
                // different versions of Pareto have been implemented
                // this function is the switch
                void set_pareto_version(Instance &inst, int version);
                
                void set_pareto_dim(Instance &inst, bool dim);
                void set_pareto_cutoff(Instance &inst, int cutoff);
                void set_float_accuracy(Instance &inst, int float_accuracy);
                void set_back_track_paretosort(Product::Sort_Type st) {
                    back_track_paretosort = st;
                }
                
                const ADP_Mode::Rtlib_Header get_rtlib_header() const {
                    return rtlib_header;
                }
                
                void set_adp_version(Instance &inst, int i, int step_mode, int pareto);
                
                void set_adp_header(int spec,  int pareto, bool multi_pareto, int step_mode);
                
                void duplicate_choice_functions(Algebra *a, std::string duplicate_suffix, 
                    std::string comperator_suffix, std::string sorter_suffix, std::string nullary_sort_suffix);
                
                int get_pareto_cutoff() const {
                    return pareto_cutoff;
                }
                
                int get_float_acc() const {
                    return float_acc;
                }
                
	private:
	
		// FIXME
		Product::Base *backtrack_product;
		Filter *backtrack_filter;
		Product::Two *original_product;
		public:
		Instance *split_instance_for_backtrack(std::string &n);
		std::pair<Instance*, Instance*> split_classified(const std::string &n);
		void backtrack_gen(Backtrack_Base &bt);
		
		void warn_unused_fns(Instance &i);
		
		void check_backtrack(const Options &opts);
		
		const std::list<Statement::Hash_Decl*> &hash_decls() const
		{ return hash_decls_; }
		
		void set_class_name(const std::string &n);
	
	
	private:
	
		Type::Base *char_type;
		void update_alphabet_types(Type::Base *res);
		public:
		void derive_temp_alphabet();
		void update_seq_type(Instance &i);
		
	
	private:
	
		void set_tracks();
		
		
	public:
	
		Bool window_mode;
		void set_window_mode(bool w);
		
		Bool kbest;
		
		std::list<std::pair<Filter*, Expr::Fn_Call*> > sf_filter_code;
		
                Product::Base * get_backtrack_product() {
                    return backtrack_product;
                }
                
			
};



#endif
