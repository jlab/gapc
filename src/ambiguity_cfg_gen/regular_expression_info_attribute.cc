

#include "regular_expression_info_attribute.hh"


Util::RegularExpressionInfoAttribute::RegularExpressionInfoAttribute (Alt::Base* b)
	: Attribute ("Util::RegularExpressionInfoAttribute"), baseExpression (b) {
}


Util::RegularExpressionInfoAttribute::RegularExpressionInfoAttribute (RegularExpressionInfoAttribute& a)
	: Attribute (a), baseExpression (a.baseExpression) {
}


Util::RegularExpressionInfoAttribute::~RegularExpressionInfoAttribute() {
}


Alt::Base* Util::RegularExpressionInfoAttribute::getBaseExpression() {
	return this->baseExpression;
}


Util::Attribute* Util::RegularExpressionInfoAttribute::clone() {
	return new RegularExpressionInfoAttribute (*this);
}

