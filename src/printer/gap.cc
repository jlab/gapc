


#include "gap.hh"

#include "../arg.hh"
#include "../const.hh"
#include "../fn_arg.hh"
#include "../fn_def.hh"
#include "../instance.hh"
#include "../log.hh"
#include "../para_decl.hh"
#include "../product.hh"
#include "../signature.hh"
#include "../statement.hh"
#include "../statement/fn_call.hh"
#include "../symbol.hh"
#include "../var_acc.hh"



Printer::GapPrinter::GapPrinter (std::ostream& oStream)
	: oStream (oStream) {
}


Printer::GapPrinter::~GapPrinter() {
}


void Printer::GapPrinter::print (AST* ast) {
	
	// As a prelude print imports, typedefs and input statements
	print (ast->imports);
	print (&ast->input);
	
	// Make some space between major program parts.
	oStream << std::endl << std::endl;
	
	// Write out the type definitions of the AST.
	print (ast->types);
	
	// Make some space between major program parts.
	oStream << std::endl << std::endl;
	
	// First write the signature to the stream.
	assert (ast->signature);
	print (ast->signature);
	
	// Make some space between major program parts.
	oStream << std::endl << std::endl;
	
	
	// Then print all algebras needed:
	for (hashtable<std::string, Algebra*>::iterator i = ast->algebras.begin(); i != ast->algebras.end(); i++) {
		std::string instanceName = (*i).first;
		Algebra* alg = (*i).second;
		print (alg);
		// Make some space between major program parts.
		oStream << std::endl << std::endl;
	}
	
	// Last but not least, write the grammar to the stream:
	print (ast->grammar());
	
	// Make some space between major program parts.
	oStream << std::endl << std::endl;
	
	// At the end we print the instance definitions
	for (hashtable<std::string, Instance*>::iterator i = ast->instances.begin(); i != ast->instances.end(); i++) {
		std::string instanceName = (*i).first;
		Instance* inst = (*i).second;
		print (inst);
	}
	
}


void Printer::GapPrinter::print (std::list<Import*> imports) {
	for (std::list<Import*>::iterator i = imports.begin(); i != imports.end(); i++) {
		print (*i);
	}
}


void Printer::GapPrinter::print (Import* import) {
	oStream << "import ";
	if (import->verbatimDeclaration) {
		oStream << "\"" << *import->name << "\"";
	}
	else {
		oStream << *import->name;
	}
	oStream << std::endl;
}


void Printer::GapPrinter::print (Input* input) {
	print (input->modes());
}


void Printer::GapPrinter::print (std::vector<Input::Mode> m) {
	if (m.size() == 1) {
		oStream << "input ";
		oStream << *Input::toString (m[0]);
		oStream << std::endl;
	}
	else if (m.size() > 1) {
		oStream << "input <";
		bool firstLoopRun = true;
		for (unsigned int i = 0; i < m.size(); i++) {
			if (!firstLoopRun) {
				oStream << ", ";
			}
			oStream << *Input::toString (m[i]);
			firstLoopRun = false;
		}
		oStream << ">" << std::endl;
	}
}


void Printer::GapPrinter::print (hashtable<std::string, Type::Base*> types) {
	for (hashtable<std::string, Type::Base*>::iterator i = types.begin(); i != types.end(); i++) {
		std::string aliasName = (*i).first;
		Type::Base* t = (*i).second;
		// Only the type defs will be printed out,
		// all other things are internal stuff.
		if (t->is (Type::DEF)) {
			Type::Def* def = dynamic_cast<Type::Def*> (t);
			oStream << "type ";
			print (def->rhs);
			oStream << " = " << aliasName << std::endl;
		}
		else if (t->is (Type::EXTERNAL)) {
			Type::External* def = dynamic_cast<Type::External*> (t);
			oStream << "type " << *def->name << " = extern" << std::endl;
		}
	}
}


void Printer::GapPrinter::print (Signature* sig) {
	oStream << "signature " << *sig->name << " (";
	bool firstLoopRun = true;
	for (hashtable <std::string, Arg*>::iterator i = sig->args.begin(); i != sig->args.end(); i++) {
		if (!firstLoopRun) {
			oStream << ", ";
		}
	    print ((*i).first, (*i).second);
	    firstLoopRun = false;
	}
	oStream << ") {" << std::endl;
    incIndention();
    
	for (hashtable <std::string, Fn_Decl*>::iterator i = sig->decls.begin(); i != sig->decls.end(); i++) {
		// Only the function declarations of non-choice-functions
		// will be printed in this loop. Since the list of decls
		// contains both types, we also have to check the list of
		// choice funtions:
		if (sig->choice_fns.find ((*i).first) == sig->choice_fns.end()) {
			oStream << indention();
		    print ((*i).second);
		    oStream << ";" << std::endl;
		}
	}
    
	for (hashtable <std::string, Fn_Decl*>::iterator i = sig->choice_fns.begin(); i != sig->choice_fns.end(); i++) {
		oStream << indention();
	    print ((*i).second);
	    oStream << ";" << std::endl;
	}
	
	decIndention();
	oStream << "}" << std::endl;
    
}


void Printer::GapPrinter::print (Algebra* alg) {
	// Start with the header:
	oStream << "algebra " << *alg->name << " implements " << (alg->signature_name == NULL ? "" : *alg->signature_name);
	
	// print the algebra parameters
	oStream << " (";
	bool firstLoopRun = true;
	for (hashtable<std::string, Type::Base*>::iterator i = alg->params.begin(); i != alg->params.end(); i++) {
		if (!firstLoopRun) {
			oStream << ", ";
		}
		oStream << (*i).first << " = ";
		print ((*i).second);
		firstLoopRun = false;
	}
	oStream << ") {" << std::endl;
	incIndention();
	
	// First all "normal" algebra functions...
	for (hashtable<std::string, Fn_Def*>::iterator i = alg->fns.begin(); i != alg->fns.end(); i++) {
		//if (alg->choice_fns.find ((*i).first) == alg->choice_fns.end()) {
		if (!(*i).second->is_Choice_Fn()) {
			print ((*i).second);
		}
	}
	
	// ... then all choice functions.
	for (hashtable<std::string, Fn_Def*>::iterator i = alg->choice_fns.begin(); i != alg->choice_fns.end(); i++) {
		print ((*i).second);
	}
	
	decIndention();
	oStream << "}" << std::endl;
}


void Printer::GapPrinter::print (Grammar* grammar) {
	oStream << indention() << "grammar " << *grammar->name << " uses " << *grammar->sig_name << " (axiom = " << *grammar->axiom_name << ") {" << std::endl;
	incIndention();
	
	// Print the 'tabulated' table definition first,
	// but only if there are tables marked for tabulation:
	if (grammar->tabulated.size() > 0) {
		oStream << "tabulated {";
		bool firstLoopRun = true;
		for (hashtable<std::string, Symbol::NT*>::iterator i = grammar->tabulated.begin(); i != grammar->tabulated.end(); i++) {
			if (!firstLoopRun) {
				oStream << ", ";
			}
			oStream << (*i).first;
			firstLoopRun = false;
		}
		oStream << "}" << std::endl;
	}
	
	// Now print all non-terminals, starting with the axiom.
	for (hashtable<std::string, Symbol::Base*>::iterator i = grammar->NTs.begin(); i != grammar->NTs.end(); i++) {
		if ((*i).second->is (Symbol::TERMINAL)) {
			Symbol::Terminal* terminal = dynamic_cast<Symbol::Terminal*> ((*i).second);
			if (terminal->isPredefinedTerminalParser()) {
				continue;	// the for loop
			}
		}
		print ((*i).first, (*i).second);
	}
	
	decIndention();
	oStream << "}" << std::endl;
}


void Printer::GapPrinter::print (Instance* instance) {
	oStream << "instance " << *instance->name() << " = " << *instance->grammar()->name << " (";
	print (instance->product);
	oStream << ");" << std::endl;
}


void Printer::GapPrinter::print (Product::Base* b) {
	switch (b->type()) {
		case Product::SINGLE: {
			Product::Single* single = dynamic_cast<Product::Single*> (b);
			oStream << single->name();
			break;
		}
		case Product::TIMES: {
			Product::Two* two = dynamic_cast<Product::Two*> (b);
			print (two->left());
			oStream << " * ";
			print (two->right());
			break;
		}
		case Product::KLASS:
		case Product::CARTESIAN:
		case Product::NOP:
		case Product::OVERLAY:
		case Product::TAKEONE: {
			oStream << "IMPLEMENT PRODUCT PRINT OUT";
		}
		default: {
			throw LogError ("gap-00765: unsupported product type.");
		}
	}
}


void Printer::GapPrinter::print (Fn_Def* fn) {
	oStream << indention();
	print (fn->choice_mode());
	
	if (fn->is_Choice_Fn()) {
		oStream << "choice ";
	}
	
	print (fn->return_type);
	oStream << " " << *fn->name;
	
	// List all parameters of this function (types and names)
	if (fn->paras.size() > 0) {
		oStream << " (";
		bool firstLoopRun = true;
		for (std::list<Para_Decl::Base*>::iterator i = fn->paras.begin(); i != fn->paras.end(); i++) {
			if (!firstLoopRun) {
				oStream << ", ";
			}
			print (*i);
			firstLoopRun = false;
		}
		oStream << ")";
	}
	
	oStream << " {" << std::endl;
	incIndention();
	
	// Print the list of statements next:
	print (fn->stmts);
	
	decIndention();
	oStream << indention() << "}" << std::endl;
}


void Printer::GapPrinter::print (Para_Decl::Base* b) {
	if (b->is (Para_Decl::SIMPLE)) {
		Para_Decl::Simple* s = dynamic_cast<Para_Decl::Simple*> (b);
		print (s);
	}
	else if (b->is (Para_Decl::MULTI)) {
		Para_Decl::Multi* m = dynamic_cast<Para_Decl::Multi*> (b);
		print (m);
	}
	else {
		throw LogError ("gap-00410: Unhandled Para_Decl type.");
	}
}


void Printer::GapPrinter::print (Para_Decl::Simple* s) {
	print (s->type());
	oStream << " " << *s->name();
}


void Printer::GapPrinter::print (Para_Decl::Multi* m) {
	oStream << "<";
	std::list<Para_Decl::Simple*> paras = m->list();
	bool firstLoopRun = true;
	for (std::list<Para_Decl::Simple*>::iterator i = paras.begin(); i != paras.end(); i++) {
		if (!firstLoopRun) {
			oStream << ", ";
		}
		print (*i);
		firstLoopRun = false;
	}
	oStream << ">";
}


void Printer::GapPrinter::print (std::string name, Symbol::Base* b) {
	//if (b->is_reachable()) {
		oStream << indention() << name << " = ";
		print (b);
		oStream << ";" << std::endl;
	//}
}


void Printer::GapPrinter::print (Symbol::Base* b) {
	if (b->is (Symbol::TERMINAL)) {
		Symbol::Terminal* t = dynamic_cast<Symbol::Terminal*> (b);
		print (t);
	}
	else if (b->is (Symbol::NONTERMINAL)) {
		Symbol::NT* nt = dynamic_cast<Symbol::NT*> (b);
		print (nt);
	}
	else {
		throw LogError ("gap-00400: Unhandled symbol type in program printer.");
	}
}


void Printer::GapPrinter::print (Symbol::NT* nt) {
	bool firstLoopRun = true;
	for (std::list<Alt::Base*>::iterator i = nt->alts.begin(); i != nt->alts.end(); i++) {
		if (!firstLoopRun) {
			oStream << " | ";
		}
		print (*i);
		firstLoopRun = false;
	}
	// If there is an eval function, print it:
	if (nt->eval_fn != NULL) {
		oStream << " # " << *nt->eval_fn;
	}
}


void Printer::GapPrinter::print (Symbol::Terminal* t) {
	//oStream << *t->orig_name;
	oStream << *t->name;
}


void Printer::GapPrinter::print (Alt::Base* b) {
	if (b->is (Alt::SIMPLE)) {
		Alt::Simple* s = dynamic_cast<Alt::Simple*> (b);
		print (s);
	}
	else if (b->is (Alt::LINK)) {
		Alt::Link* l = dynamic_cast<Alt::Link*> (b);
		print (l);
	}
	else if (b->is (Alt::BLOCK)) {
		Alt::Block* a = dynamic_cast<Alt::Block*> (b);
		print (a);
	}
	else if (b->is (Alt::MULTI)) {
		Alt::Multi* m = dynamic_cast<Alt::Multi*> (b);
		print (m);
	}
	else {
		throw LogError ("gap-00401: Unhandled Alt-node in program printer.");
	}
	// At the end print the filters of this expression:
	print (b->filters);
}


void Printer::GapPrinter::print (Alt::Simple* s) {
	oStream << *s->name;
	if (s->args.size() > 0) {
		oStream << " (";
		bool firstLoopRun = true;
		for (std::list<Fn_Arg::Base*>::iterator i = s->args.begin(); i != s->args.end(); i++) {
			if (!firstLoopRun) {
				oStream << ", ";
			}
			print (*i);
			firstLoopRun = false;
		}
		oStream << ")";
	}
}


void Printer::GapPrinter::print (Alt::Link* l) {
	oStream << *l->name;
}


void Printer::GapPrinter::print (Alt::Block* b) {
	oStream << "(";
	bool firstLoopRun = true;
	for (std::list<Alt::Base*>::iterator i = b->alts.begin(); i != b->alts.end(); i++) {
		if (!firstLoopRun) {
			oStream << " | ";
		}
		print (*i);
		firstLoopRun = false;
	}
	oStream << ")";
}


void Printer::GapPrinter::print (Alt::Multi* m) {
	throw LogError ("gap-00402: Unhandled Alt-node type in program printer.");
}


void Printer::GapPrinter::print (std::list<Filter*> fs) {
	for (std::list<Filter*>::iterator i = fs.begin(); i != fs.end(); i++) {
		Filter* filter = *i;
		if (filter->is (Filter::MIN_SIZE)) {
			oStream << " with minsize ";
		}
		else if (filter->is (Filter::MAX_SIZE)) {
			oStream << " with maxsize ";
		}
		else if (filter->is (Filter::WITH)) {
			oStream << " with " << *filter->name;
		}
		else if (filter->is (Filter::SUCHTHAT)) {
			oStream << " suchthat ";
		}
		else if (filter->is (Filter::WITH_OVERLAY)) {
			oStream << " with overlay ";
		}
		else if (filter->is (Filter::SUCHTHAT_OVERLAY)) {
			oStream << " suchthat overlay ";
		}
		else {
			throw LogError ("gap-00407: Unhandled filter-type.");
		}
		
		oStream << "(";
		bool firstLoopRun = true;
		for (std::list<Expr::Base*>::iterator j = filter->args.begin(); j != filter->args.end(); j++) {
			if (!firstLoopRun) {
				oStream << ", ";
			}
			print (*j);
			firstLoopRun = false;
		}
		oStream << ")";

	}
}


void Printer::GapPrinter::print (Fn_Arg::Base* b) {
	if (b->is (Fn_Arg::ALT)) {
		Fn_Arg::Alt* a = dynamic_cast<Fn_Arg::Alt*> (b);
		print (a);
	}
	else if (b->is (Fn_Arg::CONST)) {
		Fn_Arg::Const* c = dynamic_cast<Fn_Arg::Const*> (b);
		print (c);
	}
	else {
		throw LogError ("gap-00405: Unhandled Fn_Arg-Base type.");
	}
}


void Printer::GapPrinter::print (Fn_Arg::Alt* b) {
	print (b->alt);
}


void Printer::GapPrinter::print (Fn_Arg::Const* b) {
	Const::Base* constant = &b->expr();
	print (constant);
}


void Printer::GapPrinter::print (std::list<Statement::Base*> b) {
	for (std::list<Statement::Base*>::iterator i = b.begin(); i != b.end(); i++) {
		print (*i);
	}
}


void Printer::GapPrinter::print (Statement::Base* b) {
	switch (b->getType()) {
		case Statement::RETURN: {
			Statement::Return* r = dynamic_cast<Statement::Return*> (b);
			print (r);
			break;
		}
		case Statement::IF: {
			Statement::If* i = dynamic_cast<Statement::If*> (b);
			print (i);
			break;
		}
                case Statement::SWITCH: {
			Statement::Switch* i = dynamic_cast<Statement::Switch*> (b);
			print (i);
			break;
		}
		case Statement::VAR_DECL: {
			Statement::Var_Decl* v = dynamic_cast<Statement::Var_Decl*> (b);
			print (v);
			break;
		}
		case Statement::BLOCK: {
			Statement::Block* blck = dynamic_cast<Statement::Block*> (b);
			print (blck);
			break;
		}
		case Statement::BREAK: {
			Statement::Break* brk = dynamic_cast<Statement::Break*> (b);
			print (brk);
			break;
		}
                case Statement::DECREASE: {
			Statement::Decrease* c = dynamic_cast<Statement::Decrease*> (b);
			print (c);
			break;
		}
                case Statement::INCREASE: {
			Statement::Increase* c = dynamic_cast<Statement::Increase*> (b);
			print (c);
			break;
		}
		case Statement::CONTINUE: {
			Statement::Continue* c = dynamic_cast<Statement::Continue*> (b);
			print (c);
			break;
		}
		case Statement::FOR: {
			Statement::For* f = dynamic_cast<Statement::For*> (b);
			print (f);
			break;
		}
		case Statement::FOREACH: {
			Statement::Foreach* f = dynamic_cast<Statement::Foreach*> (b);
			print (f);
			break;
		}
                case Statement::SORTER: {
			Statement::Foreach* f = dynamic_cast<Statement::Foreach*> (b);
			print (f);
			break;
		}
		case Statement::VAR_ASSIGN: {
			Statement::Var_Assign* v = dynamic_cast<Statement::Var_Assign*> (b);
			print (v);
			break;
		}
		case Statement::FN_CALL: {
			Statement::Fn_Call* f = dynamic_cast<Statement::Fn_Call*> (b);
			print (f);
			break;
		}
		default: {
			throw LogError ("gap-00411: Unhandled Statement type in program print.");
		}
	}
}


void Printer::GapPrinter::print (Statement::Return* r) {
	oStream << indention();
	oStream << "return";
	if (r->expr != NULL) {
		oStream << " ";
		print (r->expr);
	}
	oStream << ";" << std::endl;
}


void Printer::GapPrinter::print (Statement::If* i) {
	oStream << indention() << "if (";
	print (i->cond);
	oStream << ") {" << std::endl;
	incIndention();
	print (i->then);
	decIndention();
	oStream << "}";
	if (i->els.size() != 0) {
		oStream << indention() << "else {" << std::endl;
		incIndention();
		print (i->els);
		decIndention();
		oStream << indention() << "}";
	}
	oStream << std::endl;
}


void Printer::GapPrinter::print (Statement::Var_Decl* i) {
	oStream << indention();
	print (i->type);
	oStream << " " << *i->name;
	if (i->rhs != NULL) {
		oStream << " = ";
		print (i->rhs);
	}
	oStream << ";" << std::endl;
}


void Printer::GapPrinter::print (Statement::Block* b) {
	oStream << indention() << "{" << std::endl;
	incIndention();
	print (b->statements);
	decIndention();
	oStream << indention() << "}" << std::endl;
}


void Printer::GapPrinter::print (Statement::Break* b) {
	oStream << indention() << "break;" << std::endl;
}

void Printer::GapPrinter::print (Statement::Decrease* c) {
	oStream << indention() << *c->name << "--;" << std::endl;
}

void Printer::GapPrinter::print (Statement::Increase* c) {
	oStream << indention() << *c->name << "++;" << std::endl;
}

void Printer::GapPrinter::print (Statement::Continue* c) {
	oStream << indention() << "continue;" << std::endl;
}


void Printer::GapPrinter::print (Statement::For* f) {
	oStream << indention() << "for (";
	print (f->var_decl);
	print (f->cond);
	oStream << "; ";
	print (f->inc);
	oStream << ") {";
	incIndention();
	print (f->statements);
	decIndention();
	oStream << indention() << "}" << std::endl;
}


void Printer::GapPrinter::print (Statement::Foreach* f) {
	oStream << indention() << "/* implement foreach () now! */" << std::endl;
}

void Printer::GapPrinter::print (Statement::Sorter* f) {
	oStream << indention() << "/* implement SORTER () now! */" << std::endl;
}

void Printer::GapPrinter::print (Statement::Switch* f) {
	oStream << indention() << "/* implement switch () now! */" << std::endl;
}

void Printer::GapPrinter::print (Statement::Var_Assign* a) {
	oStream << indention();
	print (a->acc);
	oStream << " = ";
	print (a->rhs);
	oStream << ";" << std::endl;
}


void Printer::GapPrinter::print (Statement::Fn_Call* f) {
	oStream << indention() << f->name() << " (";
	bool firstLoopRun = true;
	for (std::list<Expr::Base*>::iterator i = f->args.begin(); i != f->args.end(); i++) {
		if (!firstLoopRun) {
			oStream << ", ";
		}
		print (*i);
		firstLoopRun = false;
	}
	oStream << ");" << std::endl;
}


void Printer::GapPrinter::print (Var_Acc::Base* b) {
	b->put (oStream);
}


void Printer::GapPrinter::print (Const::Base* b) {
	// Just use the virtual method 'put()' of the
	// Const::Base class to write a string representation
	// to the output stream.
	b->put (oStream);
}


void Printer::GapPrinter::print (Expr::Base* b) {
	// Luckily there is an implementation that just
	// puts the string representation of the expression
	// into a stream.
	b->put (oStream);
}


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


void Printer::GapPrinter::print (std::string name, Arg* arg) {
	oStream /*<< name << " = "*/ << *arg->name;
}


void Printer::GapPrinter::print (Fn_Decl* fnDecl) {
	if (fnDecl->is_Choice_Fn()) {
		oStream << "choice ";
	}
	print (fnDecl->return_type);
	oStream << " " << *fnDecl->name << " (";
	bool firstLoopRun = true;
	for (std::list<Type::Base*>::iterator i = fnDecl->types.begin(); i != fnDecl->types.end(); i++) {
		if (!firstLoopRun) {
			oStream << ", ";
		}
		print (*i);
		firstLoopRun = false;
	}
	oStream << ")";
}


void Printer::GapPrinter::print (Mode &mode) {
	switch (mode.type) {
		case Mode::NONE: {
			break;
		}
		default: {
			oStream << Mode::map_string_to_mode[mode.type].a << " ";
		}
	}
}


void Printer::GapPrinter::print (Type::Base* t) {
	//t->put (oStream);
	//oStream << "[[ getType()=" << t->getType() << " ]]";
	switch (t->getType()) {
		case Type::USAGE: {
			Type::Usage* u = dynamic_cast<Type::Usage*> (t);
			print (u);
			break;
		}
		case Type::SIGNATURE: {
			Type::Signature* s = dynamic_cast<Type::Signature*> (t);
			print (s);
			break;
		}
		case Type::SEQ: {
			Type::Seq* s = dynamic_cast<Type::Seq*> (t);
			print (s);
			break;
		}
		case Type::SUBSEQ: {
			Type::Subseq* s = dynamic_cast<Type::Subseq*> (t);
			print (s);
			break;
		}
		case Type::LIST: {
			Type::List* list = dynamic_cast<Type::List*> (t);
			print (list);
			break;
		}
		default: {
			t->put (oStream);
			break;
		}
	}
}


void Printer::GapPrinter::print (Type::List* t) {
	oStream << "[";
	print (t->of);
	oStream << "]";
}


void Printer::GapPrinter::print (Type::Signature* sig) {
	oStream << sig->name();
}


void Printer::GapPrinter::print (Type::Usage* u) {
	print (u->base);
}


void Printer::GapPrinter::print (Type::Seq* s) {
	print (s->element_type);
}


void Printer::GapPrinter::print (Type::Subseq* s) {
	oStream << "Subsequence";
}


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


void Printer::GapPrinter::incIndention() {
	this->indentionStrings.push (indention() + "\t");
}


void Printer::GapPrinter::decIndention() {
	if (this->indentionStrings.size() > 0) {
		this->indentionStrings.pop();
	}
}


std::string Printer::GapPrinter::indention() {
	if (this->indentionStrings.size() > 0) {
		return this->indentionStrings.top();
	}
	else {
		return "";
	}
}

