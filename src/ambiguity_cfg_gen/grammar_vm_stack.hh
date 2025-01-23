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

#ifndef SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_STACK_HH_
#define SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_STACK_HH_

#include <vector>

#include "../cfg/cfg.hh"
#include "var_info_item.hh"
#include "variable_context.hh"


namespace AmbiguityCFG {
// Forward declaration of the element class used by the stack.
class GrammarVMStackElement;


// The operand stack of the GrammarVM is a simple stack
// implementation which provides some basic functionality
// for pushing and popping elements to and from the stack.
class GrammarVMStack {
 private:
    // The internal stack that holds all elements.
    std::vector<GrammarVMStackElement*> stack;

 public:
    GrammarVMStack();
    ~GrammarVMStack();

    // Pushes a new element on top of the stack.
    void push(GrammarVMStackElement* element);
    // Removes the top element from the stack and returns it
    // as result.
    GrammarVMStackElement* pop();
    // Returns the top element of the stack, leaving the stack
    // content unchanged.
    GrammarVMStackElement* peek();

    // Resets the contents of the stack, by removing all elements
    // from the stack
    void reset();
};


// This enumeration defines the possible subclass types a
// GrammarVMStackElement may have.
enum GrammarVMStackElementType {PRODUCTION_FRAGMENT_ELEMENT,
NAMED_ACCESS_ELEMENT, VAR_DECL_INFO_ELEMENT,
MULTI_PRODUCTION_FRAGMENT_ELEMENT, CONTEXT_ELEMENT};


// The grammar VM stack element is a wrapper class that
// facilitates type administration of stack elements. With
// the help of this class the grammar VM knows exactly which
// kind of element is on top of the stack.
class GrammarVMStackElement {
 protected:
    // The specific type of the subclass.
    GrammarVMStackElementType type;

    // Inits this instance and sets the instance type to
    // the value of the parameter.
    explicit GrammarVMStackElement(GrammarVMStackElementType t);

 public:
    virtual ~GrammarVMStackElement();

    bool is(GrammarVMStackElementType type);
    GrammarVMStackElementType getType();

    virtual GrammarVMStackElement* clone() = 0;
};


// The wrapper for production fragment elements on the stack.
class ProductionFragmentGrammarVMStackElement : public GrammarVMStackElement {
 private:
    // Stores the terminal this instance wraps.
    CFG::Base* productionFragment;

 public:
    explicit ProductionFragmentGrammarVMStackElement(CFG::Base* b);
    ~ProductionFragmentGrammarVMStackElement();

    // Returns the terminal element that is wrapped by this
    // instance.
    CFG::Base* getProductionFragment();

    GrammarVMStackElement* clone();
};


// Wraps a NamedAccess instance into a GrammarVMStackElement.
class NamedAccessGrammarVMStackElement : public GrammarVMStackElement {
 private:
    // Stores the pointer to the named-access-instance.
    NamedAccess* access;

 public:
    explicit NamedAccessGrammarVMStackElement(NamedAccess* element);
    ~NamedAccessGrammarVMStackElement();

    // Returns the named-access element this instance wraps.
    NamedAccess* getNamedAccessElement();

    GrammarVMStackElement* clone();
};


// Wraps a VarDeclInfoItem for storage on the stack.
class VarDeclItemGrammarVMStackElement : public GrammarVMStackElement {
 private:
    // Stores the info item wrapped by this instance.
    VarDeclInfo* infoItem;

 public:
    explicit VarDeclItemGrammarVMStackElement(VarDeclInfo* infoItem);
    ~VarDeclItemGrammarVMStackElement();

    // Returns the variable information items wrapped in this
    // instance.
    VarDeclInfo* getVarInfoItem();

    GrammarVMStackElement* clone();
};


// A stack element that holds a list of simultaneous
// valid values.
class MultiProductionFragmentGrammarVMStackElement :
  public GrammarVMStackElement {
 private:
    // The list of production fragments.
    std::vector<CFG::Base*> fragments;

 public:
    MultiProductionFragmentGrammarVMStackElement(
      std::vector<CFG::Base*> fragments);
    ~MultiProductionFragmentGrammarVMStackElement();

    // Returns the list of production fragments this stack
    // element instance holds.
    std::vector<CFG::Base*> getProductionFragments();

    // Creates a copy of this stack element.
    GrammarVMStackElement* clone();
};


class ContextGrammarVMStackElement : public GrammarVMStackElement {
 private:
    // Stores the context pointer this stack element holds.
    MultiVariableContext* context;

 public:
    explicit ContextGrammarVMStackElement(MultiVariableContext* cntxt);
    ~ContextGrammarVMStackElement();

    // Returns the context this stack element holds.
    MultiVariableContext* getContext();

    // Creates a copy of this stack element.
    GrammarVMStackElement* clone();
};


}  // namespace AmbiguityCFG


#endif  // SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_STACK_HH_
