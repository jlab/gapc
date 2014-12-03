

#ifndef __ACTUAL_PARAMETER_POSITION_ATTRIBUTE_HH__
#define __ACTUAL_PARAMETER_POSITION_ATTRIBUTE_HH__


#include "../util/attribute.hh"


namespace SpecializeGrammar {
	
	
	class ActualParameterPositionAttribute : public Util::Attribute {
		
		
		private:
			
			// Stores the actual position.
			unsigned int parameterPosition;
			
			
		public:
			
			ActualParameterPositionAttribute (unsigned int position);
			ActualParameterPositionAttribute (ActualParameterPositionAttribute& a);
			virtual ~ActualParameterPositionAttribute();
			
			// Return the actual position.
			unsigned int getActualPosition();
			
			virtual Attribute* clone();
			
			
	};
	
	
}


#endif //ifndef__ACTUAL_PARAMETER_POSITION_ATTRIBUTE_HH__

