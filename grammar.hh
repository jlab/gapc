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


#ifndef GRAMMAR_HH
#define GRAMMAR_HH

#include <string>
#include <list>
#include "hashtable.hh"

#include "runtime.hh"
#include "loc.hh"

#include "terminal.hh"

#include "symbol_fwd.hh"
#include "printer_fwd.hh"


class Arg;
class Signature;
class Signature_Base;
class Visitor;
class AST;
class Algebra;


class Grammar {
	
	private:
		
		AST &ast;
		Runtime::Asm::Poly asm_rt;
		
		std::list<Symbol::NT*> ordering;
		
		// Holds a list of all non-terminals that are defined
		// in this grammar.
		std::list<Symbol::NT*> nt_list;
		// Stores a list of non-terminals that will be added in
		// a delayed fashion.
		std::list<Symbol::NT*> new_nts;
		
		void renumber_nts();
		void move_new_nts();
		
		
	public:
		
		// The name of the grammar as defined in the grammar name of
		// the gap-source-code file.
		std::string *name;
		// The name of the signature the grammar implements.
		std::string *sig_name;
		// The name of the axiom, which must be a non-terminal defined
		// in the source code.
		std::string *axiom_name;
		// The location in the source code of the axiom.
		Loc axiom_loc;
		// The location in the source code of the grammar itself.
		Loc location;
		// temp list, filled by the bison parser when the source code
		// is parsed, with the list of table names that must be tabulated.
		// Its content is used when check_semantic() calls init_tabulated(),
		// which simply checks each element of this hashtable and adds
		// it to the hashtable tabulated if it is a non-terminal.
		hashtable<std::string, Arg*> tab_names;
		
		// Stores all non-terminal instances, and makes them
		// accessible by their name as a string. This hashtable
		// is used when the grammar graph is built.
		hashtable<std::string, Symbol::Base*> NTs;
		// Provides fast access to non-terminal instances when
		// their name is given, for all non-terminals that are
		// tabulated.
		hashtable<std::string, Symbol::NT*> tabulated;
		// Holds the reference to the axiom of the grammar. This
		// is the root node of the grammar tree.
		Symbol::NT* axiom;
		
		
		// Inits the grammar with the values for the AST, the grammar name,
		// the name of the signature, the axiom's name and the location of
		// the grammar. In addition this grammar is added to the list of
		// predefined terminals.
		Grammar(AST &as, std::string *n, std::string *s, std::string *a, const Loc &l)
			: ast(as),  name(n), sig_name(s), axiom_name(a), location(l), axiom(NULL)
		{
			Terminal::add_predefined(*this);
		}
		
		void add_nt(Symbol::NT *nt);
		void add_nt_later(Symbol::NT *nt);
		const std::list<Symbol::NT*> &nts() const
		{
			return nt_list;
		}
		
		size_t nt_number();
		
		// Fills the hashtable 'tabulated' with all non-terminals
		// that are listed in the temp-list 'tab_names'. In addition
		// to this each non-terminal is marked for tabulation.
		bool init_tabulated();
		bool init_axiom();
		// Marks all non-terminals as reachable if they are reachable
		// by the axiom. In addition the internal grammar graph is linked
		// while it is traversed.
		bool init_nt_links();
		
		bool remove_unreachable();
		
		void print_nts();
		void print_links();
		
		bool has_nonproductive_NTs();
		
		void init_table_dims();
		void window_table_dims();
		
		void init_calls();
		
		bool init_tracks();
		
		// FIXME
		//void normalize();
		// Grammar* getCFG(Algebra &canonical_alg);
		
		bool check_semantic();
		
		const Runtime::Asm::Poly & runtime_by_width();
		
		Runtime::Poly runtime();
		
		void clear_runtime();
		
		bool set_tabulated(std::vector<std::string> &v);
		void clear_tabulated();
		
		void init_in_out();
		void set_tabulated(hashtable<std::string, Symbol::NT*> &temp);
		void set_all_tabulated();
		Runtime::Poly asm_opt_runtime();
		void approx_table_conf(bool opt_const = true, unsigned int const_div = 5);
		void put_table_conf(std::ostream &s);
		void set_tabulated(Symbol::Base *nt);
		
		void init_self_rec();
		
		bool check_signature(Signature_Base &s);
		
		void print_type(std::ostream &s);
		
		void eliminate_lists();
		
		void reset_types();
		void init_list_sizes();
		
		void traverse(Visitor &v);
		
		void inline_nts();
		
		void init_indices();
		void print_indices();
		void init_guards();
		void print_guards();
		
		void init_decls();
		void init_decls(const std::string &prefix);
		
		void codegen(AST &ast);
		void print_code(Printer::Base &out);
		
		void print_dot(std::ostream &out);
		
		void dep_analysis();
		
		std::list<Symbol::NT*> & topological_ord()
		{
			return ordering;
		}
		
		// check if Alt::Simple term have const only args
		
		void remove(Symbol::NT *x);
		
		void init_multi_yield_sizes();
		void print_multi_ys() const;
		
		bool multi_detect_loops();
		
		void multi_propagate_max_filter();
		
		
};


#endif
