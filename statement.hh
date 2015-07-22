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


#ifndef STATEMENT_HH
#define STATEMENT_HH

#include "loc.hh"
#include "bool.hh"

#include <string>
#include <list>
#include <cassert>

#include "operator_fwd.hh"

#include "expr_fwd.hh"
#include "type_fwd.hh"
#include "var_acc_fwd.hh"

#include "statement_fwd.hh"

namespace Printer { class Base; }


#include "statement/base.hh"
#include "statement/block_base.hh"


namespace Statement {
	
	
	
	class Return : public Base {
		
		public:
		
			Return() : Base(RETURN), expr(NULL)
			{
			}
			
			
			Return(Expr::Base *e) : Base(RETURN), expr(e)
			{
			}
			
			
			Return(Expr::Base *e, const Loc &l) : Base(RETURN, l), expr(e)
			{
			}
			
			
			Return(Var_Decl &vdecl);
			
			Return(std::string *n);
			
			Expr::Base *expr;
			
			void print(Printer::Base &p) const;
			
			Base *copy() const;
			
			
	};
	
	
	class Break : public Base {
		
		public:
		
			Break() : Base(BREAK)
			{
			}
			
			
			Break(const Loc &l) : Base(BREAK, l)
			{
			}
			
			
			void print(Printer::Base &p) const;
			
			Base *copy() const;
			
			
	};
	
	
	class Continue : public Base {
		
		public:
		
                        Continue() : Base(CONTINUE)
			{
			}
                    
			Continue(const Loc &l) : Base(CONTINUE, l)
			{
			}
			
			
			void print(Printer::Base &p) const;
			
			Base *copy() const;
			
			
	};
	
	
	class If : public Base {
			
		private:
		
			void push(std::list<Base*> &l, Base* stmt);
			
			
		public:
		
			If()
				: Base(IF), cond(NULL)
			{
			}
			
			
			If (Expr::Base *c, Base *t)
				: Base(IF), cond(c)
			{
				push(then, t);
			}
			
			
			If (Expr::Base *c, Base *t, const Loc &l)
				: Base(IF, l), cond(c)
			{
				push(then, t);
			}
			
			
			If (Expr::Base *c, Base *t, Base *e, const Loc &l)
				: Base(IF, l), cond(c)
			{
				push(then, t);
				push(els, e);
			}
			
			
			If (Expr::Base *c, Base *t, Base *e)
				: Base(IF), cond(c)
			{
				push(then, t);
				push(els, e);
			}
			
			
			If (Expr::Base *c)
				: Base(IF), cond(c)
			{
			}
			
			
			Expr::Base *cond;
			std::list<Base*> then;
			std::list<Base*> els;
			void print(Printer::Base &p) const;
			
			void replace(Var_Decl &decl, Expr::Base *expr);
			
			Base *copy() const;
			
			
	};
        
        
        class Switch : public Base {
            public:
                         Switch(Expr::Base *c): Base(SWITCH), cond(c)
			{
			}
                         
                        Expr::Base *cond;
                        std::list<std::pair<std::string, std::list<Base*> > > cases;
                        std::list<Base*> defaul;
                        
                        std::list<Base*> *add_case(std::string *n);
                        
                        void print(Printer::Base &p) const;
        };
        
        
        class Decrease : public Base {
            
            public:
		
                std::string *name;
                
                Decrease() : Base(DECREASE)
                {
                }
                
                Decrease(std::string *n) : Base(DECREASE), name(n)
                {
                }
                
                Decrease(const Loc &l) : Base(DECREASE, l)
			{
			}
                
                void print(Printer::Base &p) const;
			
		Base *copy() const;
            
        };
        
        class Increase : public Base {
            
            public:
		
                std::string *name;
                
                Increase() : Base(INCREASE)
                {
                }
                
                Increase(std::string *n) : Base(INCREASE), name(n)
                {
                }
                
                Increase(const Loc &l) : Base(INCREASE, l)
			{
			}
                
                void print(Printer::Base &p) const;
			
		Base *copy() const;
            
        };
	
	
	class Var_Decl : public Base {
		
		private:
			
			Bool use_as_itr;
			
			
		public:
			
			::Type::Base *type;
			std::string *name;
			Expr::Base *rhs;
			
			Var_Decl(::Type::Base *t, std::string *n);
			
			
			Var_Decl(::Type::Base *t, const std::string &n)
				: Base(VAR_DECL), type(t), rhs(NULL)
			{
				name = new std::string(n);
			}
				
				
			Var_Decl(::Type::Base *t, std::string *n, const Loc &l)
				: Base(VAR_DECL, l), type(t), name(n), rhs(NULL)
			{
			}
			
			
			Var_Decl(::Type::Base *t, std::string *n, Expr::Base *e);
			
			
			Var_Decl(::Type::Base *t, std::string n, Expr::Base *e)
				: Base(VAR_DECL), type(t), rhs(e)
			{
				name = new std::string(n);
			}
			
			
			Var_Decl(::Type::Base *t, std::string *n, Expr::Base *e, const Loc &l)
				: Base(VAR_DECL, l), type(t), name(n), rhs(e)
			{
			}
			
			
			Var_Decl(::Type::Base *t, Expr::Base *e, Expr::Base *f);
			Var_Decl(const Var_Decl &v);
			
			
			void set_rhs(Expr::Base *a)
			{
				rhs = a;
			}
			
			Var_Acc::Base *left();
			Var_Acc::Base *right();
			
			void print(Printer::Base &p) const;
			
			Var_Decl *var_decl();
			void replace(Var_Decl &decl, Expr::Base *expr);
			
			bool operator==(const Var_Decl &other) const;
			
			
			void set_itr(bool b)
			{
				use_as_itr = b;
			}
			
			
			bool is_itr() const
			{
				return use_as_itr;
			}
			
			
			Var_Decl *clone() const;
			
			Base *copy() const;
			
			
	};
	
	
	// probably only for target code
	class For : public Block_Base {
		
		public:
		
			Var_Decl *var_decl;
			Expr::Base *cond;
			Statement::Base *inc;
			
			
			For(Var_Decl *v, Expr::Base* e)
				: Block_Base(FOR), var_decl(v), cond(e), inc(NULL)
			{
			}
				
				
			For(Var_Decl *v, Expr::Base *e, Statement::Base *i, const Loc &l)
				: Block_Base(FOR, l), var_decl(v), cond(e), inc(i)
			{
			}
				
				
			void print(Printer::Base &p) const;
			
			Base *copy() const;
			
			
	};
	
	
	class Foreach : public Block_Base {
		
		public:
			
			Var_Decl *elem;
			Var_Decl *container;
                        bool iteration;
                        
			Foreach(Var_Decl *i, Var_Decl *l);
			void print(Printer::Base &p) const;
			
			void replace(Var_Decl &decl, Expr::Base *expr);
			
			void set_itr(bool b);
			
                        void set_iteration(bool b);
			
	};
        
        
        class Sorter : public Block_Base {
            
                public:
                        std::string *op;
                        Var_Decl *list;
                        
                        Sorter(Operator *op, Var_Decl *l);                    
                        
                        Sorter(std::string *op, Var_Decl *l)
                            : Block_Base(SORTER), op(op),  list(l)
                        {}
                        
                        void print(Printer::Base &p) const;
            
        };
	
	class Var_Assign : public Base {
		
		private:
			
			Expr::Type op_;
			
			
		public:
			
			Var_Acc::Base *acc;
			Expr::Base *rhs;
			Var_Assign(Var_Decl &a);
			Var_Assign(Var_Decl &a, Var_Decl &b);
			Var_Assign(Var_Decl &a, Expr::Base *b);
			Var_Assign(Var_Acc::Base *a, Expr::Base *b, const Loc &l);
			Var_Assign(Var_Acc::Base *a, Expr::Base *b);
			Var_Assign(Var_Acc::Base *a, Var_Decl &v);
			void print(Printer::Base &p) const;
			
			void set_op(Expr::Type t);
			std::string op_str() const;
			
			Base *copy() const;
			
			
	};
	
	
	class Block : public Block_Base {
		
		public:
			
			Block() : Block_Base(BLOCK) {
			}
			
			
			Block(const std::list<Base*> &stmts, const Loc &l)
				: Block_Base(BLOCK, l) {
				statements = stmts;
			}
			
			
			void print(Printer::Base &p) const;
			
			
			void push_back(Base* b)
			{
				statements.push_back(b);
			}
			
			Base *copy() const;
			
			
	};
	
	
}


#endif
