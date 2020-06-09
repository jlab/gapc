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


#ifndef SRC_SPECIALIZE_GRAMMAR_CHOICE_FUNCTION_APPLICATION_ATTRIBUTE_HH_
#define SRC_SPECIALIZE_GRAMMAR_CHOICE_FUNCTION_APPLICATION_ATTRIBUTE_HH_


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

      ChoiceFunctionApplicationAttribute(std::string* choiceFunctionName);
      ChoiceFunctionApplicationAttribute(
          ChoiceFunctionApplicationAttribute& a);
      virtual ~ChoiceFunctionApplicationAttribute();

      // Returns the name of the choice function this
      // attribute represents.
      std::string* getChoiceFunctionName();

      virtual Util::Attribute* clone();


  };


}


#endif  // SRC_SPECIALIZE_GRAMMAR_CHOICE_FUNCTION_APPLICATION_ATTRIBUTE_HH_
