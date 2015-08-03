


#ifndef __DESIGNATED_AXIOM_ATTRIBUTE_HH__
#define __DESIGNATED_AXIOM_ATTRIBUTE_HH__


#include "../util/attribute.hh"


namespace SpecializeGrammar {
	
	
	class DesignatedAxiomAttribute : public Util::Attribute {
		
		private:
			
			// The original name of the axiom the designated
			// axiom-grammar-rule replaces.
			std::string* originalAxiomName;
			
			
		public:
			
			DesignatedAxiomAttribute (std::string* originalAxiomName);
			DesignatedAxiomAttribute (DesignatedAxiomAttribute& a);
			virtual ~DesignatedAxiomAttribute();
			
			// Returns the original name of the axiom.
			std::string* getOriginalAxiomName();
			
			virtual Util::Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __DESIGNATED_AXIOM_ATTRIBUTE_HH__

