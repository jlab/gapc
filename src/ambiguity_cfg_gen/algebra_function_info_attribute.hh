


#ifndef __ALGEBRA_FUNCTION_INFO_ATTRIBUTE_HH__
#define __ALGEBRA_FUNCTION_INFO_ATTRIBUTE_HH__


#include "../util/attribute.hh"
#include "../fn_arg.hh"
#include "../fn_def.hh"


namespace Util {
	
	
	// This attribute is used t annotate a CFG::BaseWrapper node
	// of the whole CFG::Base-result of an algebra function.
	class AlgebraFunctionInfoAttribute : public Attribute {
		
		private:
			
			// The name of the algebr algebra function.
			std::string* algebraFunctionName;
			// The name of the grammar rule which contained the
			// alternative that led to this annotated CFG::BaseWrapper
			// node.
			std::string* grammarRuleName;
			
			// The algebra function definition from the AST of
			// the source program.
			Fn_Def* algebraFunctionDefinition;
			// The list or arguments as found in the source code
			// AST of the GAP-program.
			std::list<Fn_Arg::Base*> algebraFunctionArgs;
			
			
		public:
			
			AlgebraFunctionInfoAttribute();
			AlgebraFunctionInfoAttribute (AlgebraFunctionInfoAttribute& a);
			virtual ~AlgebraFunctionInfoAttribute();
			
			
			void setAlgebraFunctionName (std::string* name);
			std::string* getAlgebraFunctionName();
			
			void setGrammarRuleName (std::string* name);
			std::string* getGrammarRuleName();
			
			void setAlgebraFunctionDefinition (Fn_Def* functionDefinition);
			Fn_Def *getAlgebraFunctionDefinition();
			
			void setAlgebraFunctionArguments (std::list<Fn_Arg::Base*> args);
			std::list<Fn_Arg::Base*> getAlgebraFunctionArguments();
			
			virtual Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __ALGEBRA_FUNCTION_INFO_ATTRIBUTE_HH__

