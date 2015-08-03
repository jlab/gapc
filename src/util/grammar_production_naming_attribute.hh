

#ifndef __GRAMMAR_PRODUCTION_NAMING_ATTRIBUTE_HH__
#define __GRAMMAR_PRODUCTION_NAMING_ATTRIBUTE_HH__


#include "attribute.hh"


namespace Util {
	
	
	// This attribute is used to annotate a non-terminal
	// with its original grammar production name.
	class GrammarProductionNamingAttribute : public Attribute {
		
		private:
			
			// Stores the original name.
			std::string* originalName;
			
			
		public:
			
			GrammarProductionNamingAttribute (std::string* originalName);
			GrammarProductionNamingAttribute (GrammarProductionNamingAttribute& a);
			~GrammarProductionNamingAttribute();
			
			// Returns the original name of the instance annotated
			// by this attribute.
			std::string* getOriginalName();
			
			// Creates a deep copy of this instance.
			virtual Util::Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __GRAMMAR_PRODUCTION_NAMING_ATTRIBUTE_HH__

