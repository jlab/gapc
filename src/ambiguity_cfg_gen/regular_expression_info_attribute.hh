

#ifndef __REGULAR_EXPRESSION_INFO_ATTRIBUTE_HH__
#define __REGULAR_EXPRESSION_INFO_ATTRIBUTE_HH__


#include "../util/attribute.hh"
#include "../alt.hh"


namespace Util {
	
	
	// This attribute is used to annotate a CFG::RegularExpression node
	// with the GAP AST structure which originated the regular expression.
	class RegularExpressionInfoAttribute : public Attribute {
		
		private:
			
			// The Alt::Base instance this attribute wraps.
			Alt::Base* baseExpression;
			
			
		public:
			
			RegularExpressionInfoAttribute (Alt::Base* b);
			RegularExpressionInfoAttribute (RegularExpressionInfoAttribute& a);
			virtual ~RegularExpressionInfoAttribute();
			
			// Returns the Alt::Base instance this attribute holds.
			Alt::Base* getBaseExpression();
			
			virtual Attribute* clone();
			
			
	};
	
}


#endif	// ifndef __REGULAR_EXPRESSION_INFO_ATTRIBUTE_HH__

