

#ifndef __ATTRIBUTE_HH__
#define __ATTRIBUTE_HH__


#include <string>


namespace Util {
	
	
	class Attribute {
		
		private:
			
			// The internal name a subclass of the Attribute-class
			// was used to register it as an attribute.
			std::string kindOfName;
			
			
		public:
			
			Attribute (std::string kindOfName);
			Attribute (Attribute& a);
			virtual ~Attribute();
			
			// Returns TRUE of the name equals the kind-name
			// of this attribute.
			bool isKindOf (std::string kindOfName);
			
			// Returns the ID of this attribute (this is the same
			// value as the method 'isKindOf' expects and compares.
			std::string getAttributeID();
			
			//Virtual clone function which creates a copy of the instance.
			virtual Attribute* clone() = 0;
			
		
	};
	
	
}


#endif	// ifndef __ATTRIBUTE_HH__


