

#include "grammar_vm_stack.hh"

#include <cassert>
#include <iostream>


AmbiguityCFG::GrammarVMStack::GrammarVMStack() {
}


AmbiguityCFG::GrammarVMStack::~GrammarVMStack() {
}


void AmbiguityCFG::GrammarVMStack::push (GrammarVMStackElement* element) {
	this->stack.push_back (element);
}


AmbiguityCFG::GrammarVMStackElement* AmbiguityCFG::GrammarVMStack::pop() {
	assert (this->stack.size() > 0);
	GrammarVMStackElement* element = this->stack.back();
	this->stack.pop_back();
	return element;
}


AmbiguityCFG::GrammarVMStackElement* AmbiguityCFG::GrammarVMStack::peek() {
	assert (this->stack.size() > 0);
	return this->stack.back();
}


void AmbiguityCFG::GrammarVMStack::reset() {
	this->stack.clear();
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::GrammarVMStackElement::GrammarVMStackElement (AmbiguityCFG::GrammarVMStackElementType t)
	: type (t) {
}


AmbiguityCFG::GrammarVMStackElement::~GrammarVMStackElement() {
}


bool AmbiguityCFG::GrammarVMStackElement::is (AmbiguityCFG::GrammarVMStackElementType t) {
	return this->type == t;
}


AmbiguityCFG::GrammarVMStackElementType AmbiguityCFG::GrammarVMStackElement::getType() {
	return this->type;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::ProductionFragmentGrammarVMStackElement::ProductionFragmentGrammarVMStackElement (CFG::Base* b)
	: GrammarVMStackElement (PRODUCTION_FRAGMENT_ELEMENT), productionFragment (b) {
}


AmbiguityCFG::ProductionFragmentGrammarVMStackElement::~ProductionFragmentGrammarVMStackElement() {
}


CFG::Base* AmbiguityCFG::ProductionFragmentGrammarVMStackElement::getProductionFragment() {
	return this->productionFragment;
}


AmbiguityCFG::GrammarVMStackElement* AmbiguityCFG::ProductionFragmentGrammarVMStackElement::clone() {
	ProductionFragmentGrammarVMStackElement* newInstance = new ProductionFragmentGrammarVMStackElement (this->productionFragment);
	return newInstance;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::NamedAccessGrammarVMStackElement::NamedAccessGrammarVMStackElement (AmbiguityCFG::NamedAccess* element)
	: GrammarVMStackElement (NAMED_ACCESS_ELEMENT), access (element) {
}


AmbiguityCFG::NamedAccessGrammarVMStackElement::~NamedAccessGrammarVMStackElement() {
}


AmbiguityCFG::NamedAccess* AmbiguityCFG::NamedAccessGrammarVMStackElement::getNamedAccessElement() {
	return this->access;
}


AmbiguityCFG::GrammarVMStackElement* AmbiguityCFG::NamedAccessGrammarVMStackElement::clone() {
	NamedAccessGrammarVMStackElement* newInstance = new NamedAccessGrammarVMStackElement (this->access);
	return newInstance;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::VarDeclItemGrammarVMStackElement::VarDeclItemGrammarVMStackElement (VarDeclInfo* infoItem)
	: GrammarVMStackElement (VAR_DECL_INFO_ELEMENT), infoItem (infoItem) {
}


AmbiguityCFG::VarDeclItemGrammarVMStackElement::~VarDeclItemGrammarVMStackElement() {
}


AmbiguityCFG::VarDeclInfo* AmbiguityCFG::VarDeclItemGrammarVMStackElement::getVarInfoItem() {
	return this->infoItem;
}


AmbiguityCFG::GrammarVMStackElement* AmbiguityCFG::VarDeclItemGrammarVMStackElement::clone() {
	VarDeclItemGrammarVMStackElement* newInstance = new VarDeclItemGrammarVMStackElement (this->infoItem);
	return newInstance;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::MultiProductionFragmentGrammarVMStackElement::MultiProductionFragmentGrammarVMStackElement (std::vector<CFG::Base*> fragments)
	: GrammarVMStackElement (MULTI_PRODUCTION_FRAGMENT_ELEMENT), fragments (fragments) {
}


AmbiguityCFG::MultiProductionFragmentGrammarVMStackElement::~MultiProductionFragmentGrammarVMStackElement() {
}


std::vector<CFG::Base*> AmbiguityCFG::MultiProductionFragmentGrammarVMStackElement::getProductionFragments() {
	return this->fragments;
}


AmbiguityCFG::GrammarVMStackElement* AmbiguityCFG::MultiProductionFragmentGrammarVMStackElement::clone() {
	MultiProductionFragmentGrammarVMStackElement* newInstance = new MultiProductionFragmentGrammarVMStackElement (this->fragments);
	return newInstance;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::ContextGrammarVMStackElement::ContextGrammarVMStackElement (MultiVariableContext* cntxt)
	: GrammarVMStackElement (CONTEXT_ELEMENT), context (cntxt) {
}


AmbiguityCFG::ContextGrammarVMStackElement::~ContextGrammarVMStackElement() {
}


AmbiguityCFG::MultiVariableContext* AmbiguityCFG::ContextGrammarVMStackElement::getContext() {
	return this->context;
}


AmbiguityCFG::GrammarVMStackElement* AmbiguityCFG::ContextGrammarVMStackElement::clone() {
	ContextGrammarVMStackElement* newInstance = new ContextGrammarVMStackElement (this->context);
	return newInstance;
}

