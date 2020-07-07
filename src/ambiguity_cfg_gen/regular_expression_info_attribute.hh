/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2008-2011  Georg Sauthoff
         email: gsauthof@techfak.uni-bielefeld.de or gsauthof@sdf.lonestar.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

}}} */

#ifndef SRC_AMBIGUITY_CFG_GEN_REGULAR_EXPRESSION_INFO_ATTRIBUTE_HH_
#define SRC_AMBIGUITY_CFG_GEN_REGULAR_EXPRESSION_INFO_ATTRIBUTE_HH_


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
    RegularExpressionInfoAttribute(Alt::Base* b);
    RegularExpressionInfoAttribute(RegularExpressionInfoAttribute& a);
    virtual ~RegularExpressionInfoAttribute();

    // Returns the Alt::Base instance this attribute holds.
    Alt::Base* getBaseExpression();

    virtual Attribute* clone();
};

}  // namespace Util


#endif  // SRC_AMBIGUITY_CFG_GEN_REGULAR_EXPRESSION_INFO_ATTRIBUTE_HH_
