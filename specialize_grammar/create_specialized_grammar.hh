

#ifndef __CREATE_SPECIALIZED_GRAMMAR_HH__
#define __CREATE_SPECIALIZED_GRAMMAR_HH__


#include "../alt.hh"
#include "../ambiguity_cfg_gen/algebra_function_info_attribute.hh"
#include "../ambiguity_cfg_gen/parameter_position_attribute.hh"
#include "../ambiguity_cfg_gen/regular_expression_info_attribute.hh"
#include "../ambiguity_cfg_gen/transform_gap_to_cfg.hh"
#include "../ast.hh"
#include "../cfg/cfg.hh"
#include "../fn_decl.hh"
#include "../grammar.hh"
#include "../hashtable.hh"
#include "../signature.hh"
#include "../symbol_fwd.hh"
#include "../type.hh"
#include "../util/annotate_cycles.hh"
#include "../util/attributable.hh"
#include "../util/cycle_mark_attribute.hh"
#include "actual_parameter_position_attribute.hh"
#include "choice_function_application_attribute.hh"
#include "cycle_break_point_attribute.hh"
#include "cycle_path_info_attribute.hh"
#include "designated_axiom_attribute.hh"
#include "hidden_cfg_fragments_attribute.hh"


namespace SpecializeGrammar {
	
	
	// Creates a specialized Bellman's GAP grammar from a
	// GAP instance. A specialized grammar is a grammar which
	// accepts a subset of candidates of the original grammar.
	// The restriction which candidates are filtered out by
	// the grammar is achieved by not generating these candidates
	// at all.
	class CreateSpecializedGrammar {
		
		private:
			
			// Stores the signature name of the gap source program.
			// This name is used in the designated-axiom's algebra
			// function implementation of the output parser-program.
			std::string* sourceProgramSignatureName;
			
			// A transformer which is able to translate between
			// gap AST structures and CFG nodes structures.
			AmbiguityCFG::GAPToCFGTransformer gapToCFGTransformer;
			
			// This pointer makes the AST accessible for the whole class
			AST* ast;
			
			// The signature of the new gap program as a 
			// local variable because it is generated in a
			// method which already has a different and more
			// important return type.
			Signature* signature;
			
			// Holds the mapping of signature function names to
			// their respective function declaration structures.
			// It is necessary to declare this as a field in this
			// class, because the methods which gather the
			// signature function declarations have a return type
			// of its own.
			hashtable <std::string, Fn_Decl*> signatureDecls;
			
			// This hashtable stores all algebra function definitions
			// on the fly while the CFG graph is traversed. After
			// that, this hashtable is used to set the defined functions
			// in the AST.
			hashtable<std::string, Fn_Def*> algebraFunctionDefinitions;
			
			// The name of the currently processed CFG grammar rule.
			std::string* currentGrammarProductionName;
			
			// A information context which holds stored data
			// about the current CFG sub-graph that is currently
			// processed by the methods 'processProductionFragment'
			// and 'createAlgebraFunction'.
			Util::AlgebraFunctionInfoAttribute* algebraFunctionInfoAttribute;
			
			// This set contains all hidden grammar rule fragments of
			// the currently processed grammar production. This pointer
			// is set NULL if no hidden fragments exist for the current
			// grammar production.
			HiddenCFGFragmentsAttribute* hiddenGrammarRuleFragments;
			
			// This attribute provides the process which generates the
			// algebra functions with information about broken cycle
			// paths.
			Util::CyclePathInfoAttribute* cyclePathInformation;
			
			// The file name of the include file of the runtime
			// methods each algebra function needs.
			static const std::string theAlgebraRuntimeIncludeFileName;
			static const std::string theAlgebraAnswerTypeDefName;
			static const std::string theAlgebraStringTypeDefName;
			static const std::string theAlphabetStringTypeDefName;
			static const std::string theAlgebraResultVariableName;
			static const std::string theSignatureName;
			static const std::string theAlgebraName;
			static const std::string theGrammarName;
			static const std::string theAxiomName;
			static const std::string theChoiceFunctionName;
			static const std::string theMergeRulesFunctionName;
			static const std::string theResultShapeFieldName;
			static const std::string theGetRuleNameFunctionName;
			static const std::string theRopeStringMatchingFilterName;
			
			
			// An enumeration of all types that are important for the
			// shape parser algebra.
			enum AlgebraParameterTypes {VOID, ANSWER, ALPHABET, ALPHABET_STRING};
			
			
		public:
			
			CreateSpecializedGrammar();
			~CreateSpecializedGrammar();
			
			AST* createGrammar (AST* sourceProgram, std::string* instanceName);
			
			
		private:
			
			Grammar* createGrammar (AST* ast, CFG::CFG* grammar);
			void createTypeDefinitions (AST* ast);
			
			Symbol::NT* processProduction (CFG::GrammarProduction* production);
			std::list<Alt::Base*>* processProductionAlternative (CFG::ProductionAlternative* alt);
			std::list<Alt::Base*>* processProductionAlternative (Util::Attributable* infoContext, CFG::ProductionAlternative* alt);
			std::pair<Alt::Base*, AlgebraParameterTypes> processProductionFragment (Util::Attributable* infoContext, CFG::Base* b);
			
			// Returns an AST compatible type instance for the given
			// enumeration value.
			Type::Base* getAlgebraParameterType (AlgebraParameterTypes type);
			// Returns an algebra signature type which corresponds to
			// the 'type' parameter.
			Type::Base* getAlgebraSignatureType (AlgebraParameterTypes type);
			// Transforms the list of internal algebra types into a
			// list of AST compatible types.
			std::list<Type::Base*>* transformaAlgebraSignatureTypes (std::list<AlgebraParameterTypes> types);
			// Returns the alphabet type of the signature.
			Type::Alphabet* getAlphabetType();
			// Returns a type that can be used in a signature.
			Type::Base* createSignatureType (std::string* typeName);
			// Returns the type 'void'.
			Type::Base* createSignatureVoidType();
			// Returns the algebra-function answer type.
			Type::Base* createSignatureAnswerType();
			// Returns the algebra function name annotated with an
			// attribute to the CFG::Base instance.
			std::string* getAlgebraFunctionName (CFG::Base* b);
			
			// Creates an algebra function call wrapped around the argument.
			Alt::Base* createAlgebraFunctionCallWrapper (std::string* algebraFunctionName, Alt::Base* arg);
			// Creates an algebra function call wrapped around the argument.
			Alt::Base* createAlgebraFunctionCallWrapper (std::string* algebraFunctionName, std::list<Fn_Arg::Base*> args);
			
			// Adds a new algebra function declaration to the signature.
			void addSignatureDeclaration (std::string* algebraFunctionName, AlgebraParameterTypes parameterType);
			// Adds a new algebra function declaration to the signature.
			void addSignatureDeclaration (std::string* algebraFunctionName, std::list<AlgebraParameterTypes> parameterTypes);
			// Adds a signature declaration for the choice function
			// to the signature.
			void addSignatureChoiceDeclaration (std::string* choiceFunctionName);
			
			
			// Creates an algebra for the shape parser.
			Algebra* createAlgebra();
			// Creates the choice function of the shape-parser
			// generated by this algorithm.
			void createChoiceFunction();
			// Creates an algebra function for the grammar-production
			// of the designated axiom-production of the preprocessed
			// CFG graph.
			void createDesignatedAxiomAlgebraFunction (std::string* designatedAxiomAlgebraFunctionName, std::string* originalAxiomName);
			// Creates an algebra function for the shape-parser.
			void createAlgebraFunction (Util::Attributable* infoContext, std::string* name, AlgebraParameterTypes parameterType, CFG::Base* parameterValue);
			void createAlgebraFunction (Util::Attributable* infoContext, std::string* name, std::list<AlgebraParameterTypes> parameterTypes, std::list<CFG::Base*> parameterValues);
			
			// Creates an algebra function part which generates a non-terminal
			// and a rule-body but without an original algebra function 
			// applied to it.
			std::list<Statement::Base*>* createFunctionCallNoAlgFn (Statement::Var_Decl* ntVar, Statement::Var_Decl* bodyVar, CFG::Base* argument);
			Expr::Base* createFunctionCallArgumentNoAlgFn (Util::Attributable* infoContext, CFG::Base* fragment);
			// Creates the AST statement structures needed to assign
			// values to the shape-parser algebra-function arguments
			// 'nt' and 'body'.
			std::list<Statement::Base*>* createFunctionCall (Statement::Var_Decl* ntVar, Statement::Var_Decl* bodyVar, std::list<CFG::Base*> arguments);
			Expr::Base* createFunctionCallArguments (std::list<CFG::Base*> arguments);
			Expr::Base* createFunctionCallArgument (Util::Attributable* infoContext, CFG::Base* fragment);
			// Converts a CFG node instance to a list of CFG node instances
			// used as a list of arguments to a hidden algebra function call.
			std::list<CFG::Base*> convertCFGFragmentToArgumentList (CFG::Base* fragment);
			// 
			std::list<Statement::Base*>* createHiddenFunctionCallNoAlgFn (std::string productionNT, Statement::Var_Decl* ntVar, Statement::Var_Decl* bodyVar, CFG::Base* arguments);
			Expr::Base* createHiddenFunctionCallArgumentNoAlgFn (Util::Attributable* infoContext, CFG::Base* fragment);
			// Creates a list of statements which contains the algebra function
			// code for generating hidden algebra function calls. This method
			// creates two expressions and assigns them to the variables 'nt'
			// and 'body' of the algebra function. Both variables are subject
			// to insertion into the rules-result of the algebra function.
			std::list<Statement::Base*>* createHiddenFunctionCall (std::string productionNT, Util::Attributable* infoContext, Statement::Var_Decl* ntVar, Statement::Var_Decl* bodyVar, std::list<CFG::Base*> arguments);
			// Creates an expression from the list of arguments which is assigned
			// to the variable 'nt' of the algebra function. The expression is
			// primarily a concatenation of sub-expressions generated by the
			// method SpecializeGrammar::createHiddenFunctionCallArgument().
			Expr::Base* createHiddenFunctionCallArguments (Util::Attributable* infoContext, std::list<CFG::Base*> arguments);
			// Creates a single sub-expression by transforming a single argument
			// form the argument list of a hidden algebra function call.
			Expr::Base* createHiddenFunctionCallArgument (Util::Attributable* infoContext, CFG::Base* fragment);
			// Creates an AST structure which represents a call to the
			// external function 'insertProduction', which takes three
			// arguments.
			Statement::Base* create_InsertProduction_Call (Statement::Var_Decl* resVar, Statement::Var_Decl* ntVar, Statement::Var_Decl* bodyVar);
			// Creates an addition expression, where both parameters are
			// added together. If both expressions represent character
			// constant content, a new instance of Expr::Const is returned
			// which holds the appended string value of both parameters.
			Expr::Base* create_Rope_PLUS_Rope_Expression (Expr::Base* expr1, Expr::Base* expr2);
			// Returns TRUE if the parameter is an instance of Expr::Const
			// with a constant string as value.
			bool expressionIsConstantString (Expr::Base* expr);
			// If Expr::Base is a constant string, then the string
			// which is represented by the instance 'expr' is returned.
			// otherwise this method returns NULL. This method also
			// returns a std::string instance for Const::Char content.
			std::string* getConstantString (Expr::Base* expr);
			// Returns a type instance which represents the internal
			// algebra string type, used to represent e.g. shapes or
			// text of anything.
			Type::Base* createInternalAlgebraStringType();
			// Returns the type used for parsing alphabet strings
			// from the input. This is at the moment Rope.
			Type::Base* createAlgebraAlphabetStringType();
			// Returns a type instance which represents the algebra
			// function's return type.
			Type::Base* createAlgebraFunctionAnswerType();
			// Returns a type instance which represents the algebra
			// function's alphabet type.
			Type::Base* createAlgebraFunctionAlphabetType();
			
			// Orders the list of algebra function parameters so they match
			// the order they had, when they were fed into the original
			// algebra function of the original gap-program.
			std::list<CFG::Base*> orderAlgebraFunctionArguments (std::list<CFG::Base*> parameterValues, std::list<Fn_Arg::Base*> originalArguments);
			
			// Creates an append statement, which appends a verbatim string
			// to the variable 'variableToAppendTo'.
			Statement::Base* alg_append (Statement::Var_Decl* variableToAppendTo, std::string* str);
			// Creates an append statement, which appends an other variable
			// to the variable 'variableToAppendTo'.
			Statement::Base* alg_append (Statement::Var_Decl* variableToAppendTo, Statement::Var_Decl* appendedVariable);
			// Creates an append statement, which appends the expression
			// to the variable 'variableToAppendTo'.
			Statement::Base* alg_append (Statement::Var_Decl* variableToAppendTo, Expr::Vacc* appendedExpr);
			
			Util::AlgebraFunctionInfoAttribute* getAlgebraFunctionInfoAttribute (Util::Attributable* attributableInstance);
			Util::ParameterPositionAttribute* getPositionAttribute (Util::Attributable* attributableInstance);
			SpecializeGrammar::ActualParameterPositionAttribute* getActualPositionAttribute (Util::Attributable* attributableInstance);
			SpecializeGrammar::CycleBreakPointAttribute* getCycleBreakPointAttribute (Util::Attributable* attributableInstance);
			HiddenCFGFragmentsAttribute* getHiddenCFGFragmentsAttribute (Util::Attributable* attributableInstance);
			Util::CyclePathInfoAttribute* getCyclePathInformationAttribute (Util::Attributable* attributableInstance);
			ChoiceFunctionApplicationAttribute* getChoiceFunctionApplicationAttribute (Util::Attributable* attributableInstance);
			DesignatedAxiomAttribute* getDesignatedAxiomAttribute (Util::Attributable* attributableInstance);
			Util::RegularExpressionInfoAttribute* getRegularExpressionInfoAttribute (CFG::RegularExpression* regexpr);
			Util::CycleMarkAttribute* getCycleMarkAttribute (CFG::Base* b);
			
			// Creates the default instance of our new GAP-program.
			Instance* createDefaultInstance (Grammar* grammar);
			
	};
	
	
}


#endif	// idndef __CREATE_SPECIALIZED_GRAMMAR_HH__

