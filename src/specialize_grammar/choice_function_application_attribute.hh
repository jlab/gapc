


#ifndef __CHOICE_FUNCTION_APPLICATION_ATTRIBUTE_HH__
#define __CHOICE_FUNCTION_APPLICATION_ATTRIBUTE_HH__


#include "../util/attribute.hh"
#include "choice_function_application_attribute.hh"


namespace SpecializeGrammar {
	
	
	// This is an attribute which is used to mark a
	// CFG::GrammarProduction. A production marked this
	// way should be transformed into a gap-grammar-production
	// with a choice function applied to it of the same
	// name as stored in this attribute.
	class ChoiceFunctionApplicationAttribute : public Util::Attribute {
		
		private:
			
			// The name of the choice function this attribute
			// represents.
			std::string* choiceFunctionName;
			
			
		public:
			
			ChoiceFunctionApplicationAttribute (std::string* choiceFunctionName);
			ChoiceFunctionApplicationAttribute (ChoiceFunctionApplicationAttribute& a);
			virtual ~ChoiceFunctionApplicationAttribute();
			
			// Returns the name of the choice function this
			// attribute represents.
			std::string* getChoiceFunctionName();
			
			virtual Util::Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __CHOICE_FUNCTION_APPLICATION_ATTRIBUTE_HH__


