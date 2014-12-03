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


#ifndef TYPE_HH
#define TYPE_HH

#include "type/base.hh"

#include "loc.hh"
#include "hashtable.hh"

#include "log.hh"

#include "bool.hh"

#include "printer_fwd.hh"
#include "statement_fwd.hh"

#include <string>
#include <list>
#include <iostream>
#include <sstream>
#include <cassert>


class Signature;
class Table;


namespace Type {
	
	
	enum Status { READY, RUNNING, ERROR };
	
	
	// ERROR Type
	
	class Base;
	
	void add_predefined (hashtable<std::string, Base*> &table); 
	
	
	
	// Needed to track locations in the source code
	class Usage : public Base {
		
		private:
			
			
		public:
			
			MAKE_CLONE (Usage);
			Base *base;
			
			Usage (Base *b, const Loc &l) : Base(USAGE, l), base(b) {}
			Usage (Base *b) : Base(USAGE), base(b) {}
			
			bool is_eq (const Base & base) const;
			Base * simple();
			const Base * const_simple() const;
			std::ostream & put (std::ostream &s) const;
			
			void print (Printer::Base &s) const;
			
			
	};
	
	
	bool set_if_compatible(Base * &a, Base *b, const Loc &l1, const Loc &l2);
	
	
	class List : public Base {
		
		public:
			
			enum Push_Type { NORMAL, MIN, MAX, SUM, MIN_OTHER, MAX_OTHER, MIN_SUBOPT, MAX_SUBOPT, HASH };
			
			
		private:
			
			Push_Type push_type_;
			Statement::Hash_Decl *hash_decl_;
			
			
		public:
			
			MAKE_CLONE(List);
			
			List(Base *b, const Loc &l) : Base(LIST, l), push_type_(NORMAL), hash_decl_(0), of(b) {}
			List(Base *b) : Base(LIST), push_type_(NORMAL), hash_decl_(0), of(b) {}
			
			Base *of;
			bool is_eq(const Base & base) const;
			std::ostream & put(std::ostream &s) const;
			Base *left();
			Base *right();
			Base *component();
			void print(Printer::Base &s) const;
			
			std::string push_str() const;
			
			Push_Type push_type() const { return push_type_; }
			
			void set_push_type(Push_Type x);
			void set_hash_decl(Statement::Hash_Decl *h);
			
			const Statement::Hash_Decl &hash_decl() const {
				assert(hash_decl_);
				return *hash_decl_;
			}
			
			Statement::Hash_Decl *hdecl() { return hash_decl_; }
			
			
	};
	
	
	class Name {
		
		public:
			
			Base *lhs;
			Loc location;
			
			Name (Base *lh, const Loc &l) : lhs (lh), location (l) {}
			Name (Base *lh) : lhs (lh) {}
			
			std::ostream & put (std::ostream &s) const;
			
			bool is_eq (Name & n) const;
			
			
	};
	
	
	inline std::ostream & operator<<(std::ostream &s, const Name &p)
	{
		return p.put(s);
	}
	
	
	class Tuple : public Base {
		
		public:
			
			MAKE_CLONE (Tuple);
			
			Tuple (const Loc &l) : Base(TUPLE, l) {} ;
			Tuple (Base *a, Base *b);
			
			hashtable<std::string, Name*> members;
			
			std::list<std::string*> names;
			
			typedef std::pair<Name*, std::string*> Tuple_Pair;
			typedef std::list<Tuple_Pair*> Tuple_List;
			
			std::list<Tuple_Pair*> list;
			
			void init (Tuple_List *l);
			
			bool is_eq (const Base & base) const;
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			Base *left();
			Base *right();
			Base *component();
			
			
	};
	
	
	// Used to distinguish user defined tuples from
	// generated ones
	// see affinelocsim as use case ...
	class TupleDef : public Tuple {
		
		public:
			
			MAKE_CLONE (TupleDef);
			std::string name;
			
			TupleDef (const Loc &l) : Tuple (l) { type = TUPLEDEF; }
			
			bool is_eq (const Base & base) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Alphabet : public Base {
		
		public:
			
			MAKE_CLONE(Alphabet);
			
			Base *temp;
			::Signature *signature;
			
			Alphabet (Loc &l) : Base (ALPHABET, l), temp (NULL), signature (NULL) {}
			Alphabet() : Base (ALPHABET), temp (NULL), signature (NULL) {}
			
			std::ostream & put (std::ostream &s) const;
			bool is_eq (const Base &base) const;
			Base * simple();
			const Base * const_simple() const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Def : public Base {
		
		public:
			
			MAKE_CLONE (Def);
			std::string *name;
			Base *rhs;
			
			Def (std::string *n, Loc &l, Base *b) : Base(DEF, l), name(n), rhs(b)
			{
				if (b->is(TUPLEDEF)) {
					TupleDef *t = dynamic_cast<TupleDef*>(b);
					t->name = *n;
				}
			}
			
			Def (std::string *n, Base *b) :
			Base (DEF), name(n), rhs(b) {}
			std::ostream & put (std::ostream &s) const;
			Base * simple();
			const Base * const_simple() const;
			bool is_eq (const Base & base) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Choice : public Base {
		
		public:
			
			MAKE_CLONE (Choice);
			Base *rest;
			
			Choice (Base *r, const Loc &l) : Base (CHOICE, l), rest (r) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Void : public Base {
		
		public:
			
			MAKE_CLONE (Void);
			
			Void (Loc &l) : Base (VOID, l) {}
			Void() : Base (VOID) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class RealVoid : public Base {
		
		public:
			
			MAKE_CLONE (RealVoid);
			
			RealVoid() : Base (REALVOID) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Int : public Base {
		
		public:
			
			MAKE_CLONE (Int);
			
			Int (Loc &l) : Base (INT, l) {}
			Int() : Base (INT) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Integer : public Base {
		
		public:
			
			MAKE_CLONE (Integer);
			Integer (Loc &l) : Base (INTEGER, l) {}
			Integer() : Base (INTEGER) {}
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Size : public Base {
		
		public:
			
			
			MAKE_CLONE (Size);
			
			Size() : Base (SIZE) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Float : public Base {
		
		public:
			
			MAKE_CLONE (Float);
			
			Float (Loc &l) : Base (FLOAT, l) {}
			Float() : Base (FLOAT) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Single : public Base {
		
		public:
			
			MAKE_CLONE (Single);
			Single (Loc &l) : Base (FLOAT, l) {}
			Single() : Base (SINGLE) {}
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class String : public Base {
		
		public:
			
			MAKE_CLONE (String);
			
			String (Loc &l) : Base (STRING, l) {}
			String() : Base (STRING) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Char : public Base {
		
		public:
			
			MAKE_CLONE (Char);
			
			Char(Loc &l) : Base (CHAR, l) {}
			Char() : Base (CHAR) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Bool : public Base {
		
		public:
			
			MAKE_CLONE(Bool);
			
			Bool(Loc &l) : Base (BOOL, l) {}
			Bool() : Base (BOOL) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Signature : public Base {
		
		private:
			
			std::string *_name;
			::Signature *signature;
			
			
		public:
			
			MAKE_CLONE (Signature);
			
			Signature (std::string *n, Loc &l, ::Signature *s) : Base(SIGNATURE, l), _name(n), signature(s) {}
			Signature() : Base (SIGNATURE), _name (NULL), signature (NULL) {}
			
			std::ostream & put (std::ostream &s) const;
			bool is_eq (const Base & base) const;
			Base * simple();
			const Base * const_simple() const;
			
			const std::string &name() const { return *_name; }
			void print (Printer::Base &s) const;
			
			
	};
	
	
	// represents a pair of begin end iterator
	class Range : public Base {
		
		public:
			
			MAKE_CLONE (Range);
			enum Component { NONE, LEFT, RIGHT };
			
			Base *element_type;
			Base *original_tuple;
			Component component;
			
			Range (Base *b) : Base(RANGE), element_type(b), original_tuple(NULL), component(NONE) {}
			Range(Base *b, Base *o, Component c) : Base(RANGE), element_type(b), original_tuple(o), component(c) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			bool is_eq (const Base & base) const;
			
			
	};
	
	
	// the type of the input ...
	class Seq : public Base {
		
		public:
			
			MAKE_CLONE (Seq);
			Base *element_type;
			
			Seq() : Base (SEQ), element_type (NULL)
			{
				element_type = new Char();
			}
			Seq (Base *b) : Base(SEQ), element_type(b) {}
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	class Table : public Base {
		
		public:
			
			MAKE_CLONE (Table);
			
			::Bool cyk;
			Base *element_type;
			::Table *table;
			
			Table (Base *e, ::Table *t) : Base (SEQ), element_type (e), table (t) {}
			
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Subseq : public Base {
		
		private:
			
			Seq *seq;
			
			
		public:
			
			MAKE_CLONE (Subseq);
			Subseq() : Base(SUBSEQ), seq(NULL) {}
			Subseq (Seq *s) : Base(SUBSEQ), seq(s) {}
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Generic : public Base {
		
		public:
			
			std::string name;
			Generic (Type t) : Base(t) {}
			Generic (Type t, const Loc &l) : Base(t, l) {}
			std::ostream & put (std::ostream &s) const;
			
			
	};
	
	
	class Shape : public Generic {
		
		public:
			
			MAKE_CLONE(Shape);
			Shape() : Generic(SHAPE) { name = "shape"; }
			Shape (const Loc &l) : Generic(SHAPE, l) { name = "shape"; }
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class Referencable : public Base {
		
		private:
			
			
		public:
			
			MAKE_CLONE(Referencable);
			Base *base;
			Referencable (Base *b) : Base(REFERENCABLE), base(b) {}
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			Base *deref();
			
			
	};
	
	
	class Rational : public Generic {
		
		public:
			
			MAKE_CLONE (Rational);
			Rational() : Generic(RATIONAL) { name = "rational"; }
			Rational (const Loc &l) : Generic(RATIONAL, l) { name = "rational"; }
			void print (Printer::Base &s) const;
			
			
	};
	
	class BigInt : public Generic {
		
		public:
			
			MAKE_CLONE (BigInt);
			BigInt() : Generic(BIGINT) { name = "bigint"; }
			BigInt (const Loc &l) : Generic(BIGINT, l) { name = "bigint"; }
			void print (Printer::Base &s) const;
			
			
	};
	
	
	class External : public Base {
		
		private:
			
			
		public:
			
			MAKE_CLONE (External);
			std::string *name;
			External (std::string *n, const Loc &l) : Base(EXTERNAL, l), name(n) {}
			External (std::string *n) : Base(EXTERNAL), name(n) {}
			External (const std::string &n) : Base(EXTERNAL), name(new std::string(n)) {}
			std::ostream & put (std::ostream &s) const;
			void print (Printer::Base &s) const;
			bool is_eq (const Base & base) const;
			
			
	};
	
	
}


#endif

