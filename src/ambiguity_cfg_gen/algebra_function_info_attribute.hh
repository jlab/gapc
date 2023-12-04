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


#ifndef SRC_AMBIGUITY_CFG_GEN_ALGEBRA_FUNCTION_INFO_ATTRIBUTE_HH_
#define SRC_AMBIGUITY_CFG_GEN_ALGEBRA_FUNCTION_INFO_ATTRIBUTE_HH_

#include <string>
#include <list>
#include "../util/attribute.hh"
#include "../fn_arg.hh"
#include "../fn_def.hh"


namespace Util {


// This attribute is used t annotate a CFG::BaseWrapper node
// of the whole CFG::Base-result of an algebra function.
class AlgebraFunctionInfoAttribute : public Attribute {
 private:
    // The name of the algebr algebra function.
    std::string* algebraFunctionName;
    // The name of the grammar rule which contained the
    // alternative that led to this annotated CFG::BaseWrapper
    // node.
    std::string* grammarRuleName;

    // The algebra function definition from the AST of
    // the source program.
    Fn_Def* algebraFunctionDefinition;
    // The list or arguments as found in the source code
    // AST of the GAP-program.
    std::list<Fn_Arg::Base*> algebraFunctionArgs;

 public:
    AlgebraFunctionInfoAttribute();
    AlgebraFunctionInfoAttribute(AlgebraFunctionInfoAttribute& a);
    virtual ~AlgebraFunctionInfoAttribute();


    void setAlgebraFunctionName(std::string* name);
    std::string* getAlgebraFunctionName();

    void setGrammarRuleName(std::string* name);
    std::string* getGrammarRuleName();

    void setAlgebraFunctionDefinition(Fn_Def* functionDefinition);
    Fn_Def *getAlgebraFunctionDefinition();

    void setAlgebraFunctionArguments(std::list<Fn_Arg::Base*> args);
    std::list<Fn_Arg::Base*> getAlgebraFunctionArguments();

    virtual Attribute* clone();
};


}  // namespace Util


#endif  // SRC_AMBIGUITY_CFG_GEN_ALGEBRA_FUNCTION_INFO_ATTRIBUTE_HH_
