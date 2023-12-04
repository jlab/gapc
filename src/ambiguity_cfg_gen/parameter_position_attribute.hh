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


#ifndef SRC_AMBIGUITY_CFG_GEN_PARAMETER_POSITION_ATTRIBUTE_HH_
#define SRC_AMBIGUITY_CFG_GEN_PARAMETER_POSITION_ATTRIBUTE_HH_


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
    explicit ParameterPositionAttribute(int parameterPosition);
    ParameterPositionAttribute(ParameterPositionAttribute& a);
    virtual ~ParameterPositionAttribute();

    // Returns the parameter position.
    int getParameterPosition();

    // Returns a deep copy of this attribute.
    virtual Attribute* clone();
};


}  // namespace Util


#endif  // SRC_AMBIGUITY_CFG_GEN_PARAMETER_POSITION_ATTRIBUTE_HH_
