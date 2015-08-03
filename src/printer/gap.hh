


#ifndef __PRINTER____GAP_HH__
#define __PRINTER____GAP_HH__


#include <ostream>
#include <stack>

#include "../ast.hh"



// This namespace defines a pretty printer for the
// bellman's GAP source code data structure (a.k.a. the AST).
namespace Printer {
	
	
	class GapPrinter {
		
		private:
			
			// The output-stream being commonly used by all methods
			// writing a part of the GAP program.
			std::ostream& oStream;
			
			// A stack of indentions
			std::stack<std::string> indentionStrings;
			
			
		public:
			
			GapPrinter (std::ostream& oStream);
			~GapPrinter();
			
			// Prints the Bellman's GAP Program into the stream.
			void print (AST* ast);
			
			
		private:
			
			void print (std::list<Import*> imports);
			void print (Import* import);
			void print (Input* input);
			void print (std::vector<Input::Mode> m);
			void print (hashtable<std::string, Type::Base*> types);
			
			void print (Signature* sig);
			void print (Algebra* alg);
			void print (Grammar* grammar);
			void print (Instance* instance);
			void print (Product::Base* b);
			
			void print (Fn_Def* fn);
			void print (Para_Decl::Base* b);
			void print (Para_Decl::Simple* s);
			void print (Para_Decl::Multi* m);
			
			void print (std::string name, Symbol::Base* b);
			void print (Symbol::Base* b);
			void print (Symbol::NT* nt);
			void print (Symbol::Terminal* t);
			
			void print (Alt::Base* b);
			void print (Alt::Simple* s);
			void print (Alt::Link* l);
			void print (Alt::Block* b);
			void print (Alt::Multi* m);
			
			void print (std::list<Filter*> fs);
			
			void print (Fn_Arg::Base* b);
			void print (Fn_Arg::Alt* b);
			void print (Fn_Arg::Const* b);
			
			void print (std::list<Statement::Base*> b);
			void print (Statement::Base* b);
			void print (Statement::Return* r);
			void print (Statement::If* i);
                        void print (Statement::Switch* i);
			void print (Statement::Var_Decl* i);
			void print (Statement::Block* b);
			void print (Statement::Break* b);
                        void print (Statement::Increase* c);
                        void print (Statement::Decrease* c);
			void print (Statement::Continue* c);
			void print (Statement::For* f);
			void print (Statement::Foreach* r);
                        void print (Statement::Sorter* r);
			void print (Statement::Var_Assign* a);
			void print (Statement::Fn_Call* f);
			
			void print (Var_Acc::Base* b);
			
			void print (Const::Base* b);
			
			void print (Expr::Base* b);
			
			void print (std::string name, Arg* arg);
			void print (Fn_Decl* decl);
			
			void print (Mode &mode);
			
			void print (Type::Base* t);
			void print (Type::List* t);
			void print (Type::Signature* sig);
			void print (Type::Usage* u);
			void print (Type::Seq* u);
			void print (Type::Subseq* u);
			
			// Increases the level of indention.
			void incIndention();
			// Decreases the leven of indention.
			void decIndention();
			// Return the current indention string.
			std::string indention();
			
			
	};
	
	
}


#endif	// ifdef __PRINTER____GAP_HH__

