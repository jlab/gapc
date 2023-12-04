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

#ifndef SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_CODE_HH_
#define SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_CODE_HH_


#include <list>
#include <string>


#include "grammar_vm_command.hh"


namespace AmbiguityCFG {


// This is a container class that holds code for the GrammarVM.
// A single code represents a single algebra function. Each
// algebra function has exactly one GrammarVMCode that is compiled
// by the ambiguity-CFG-generator.
class GrammarVMCode {
 private:
    // Stores the name of the algebra function.
    std::string* functionName;

    // A list of all parameter names this function has.
    std::list<std::string*> parameterNames;

    // Stores the list of commands the compiled code
    // consists of.
    std::list<GrammarVMCommand*> commands;

 public:
    GrammarVMCode(
      std::string* functionName, std::list<std::string*> parameterNames);
    ~GrammarVMCode();


    // Appends a command to the list of commands.
    void appendCommand(GrammarVMCommand* command);

    // Returns the name of the algebra function that was
    // used to generate this code.
    std::string* getFunctionName();

    // Returns the list of names of the paramters.
    std::list<std::string*> getParameterNames();

    // Returns the list of GrammarVM commands this code
    // consists of.
    std::list<GrammarVMCommand*> getCode();
};


}  // namespace AmbiguityCFG


#endif  // SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_CODE_HH_
