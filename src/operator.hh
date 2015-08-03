/* 
 * File:   operator_struct.hh
 * Author: gatter
 *
 * Created on June 22, 2015, 12:21 PM
 */

#ifndef OPERATOR_HH
#define	OPERATOR_HH


#include "printer_fwd.hh"
#include "type_fwd.hh"
#include "para_decl_fwd.hh"
#include "statement_fwd.hh"

class Operator {
    
	friend class Printer::Cpp;
        
private:
        
         // The return type of the operator function
	Type::Base* return_type;
        // name of the operator container
        std::string* name;
public:
        // name of the created object container
        std::string* object;
private: 
        // The list of parameter declarations for the operator function.
        std::list<Para_Decl::Base*> paras;
        
        // add a variable definition of a constant static value in container
        std::list<Statement::Var_Decl*> const_values;
        
public:
        // The list of statements of the operator function
	std::list<Statement::Base*> stmts;
        
        
        Operator(Type::Base *r, std::string *ob) :
            return_type(r),  object(ob) { 
            name = new std::string(*ob);
            name->append("_container");
        }
        
        void add_para(Type::Base *type, std::string *n);
        
        void add_const_value(Statement::Var_Decl *v);
};


#endif	/* OPERATOR_HH */

