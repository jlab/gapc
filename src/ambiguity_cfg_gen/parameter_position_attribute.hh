


#ifndef __PARAMETER_POSITION_ATTRIBUTE_HH__
#define __PARAMETER_POSITION_ATTRIBUTE_HH__


#include "../util/attribute.hh"


namespace Util {
	
	
	// Annotates any instance with a parameter position in the
	// list of an algebra function.
	class ParameterPositionAttribute : public Attribute {
		
		private:
			
			// The position of the annotated instance in the parameter
			// list of an algebra function.
			int parameterPosition;
			
			
		public:
			
			ParameterPositionAttribute (int parameterPosition);
			ParameterPositionAttribute (ParameterPositionAttribute& a);
			virtual ~ParameterPositionAttribute();
			
			// Returns the parameter position.
			int getParameterPosition();
			
			// Returns a deep copy of this attribute.
			virtual Attribute* clone();
			
			
	};
	
	
}


#endif	// ifndef __PARAMETER_POSITION_ATTRIBUTE_HH__

