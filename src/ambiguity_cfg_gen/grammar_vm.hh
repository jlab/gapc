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

#ifndef SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_HH_
#define SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_HH_

#include <list>
#include <set>
#include <vector>
#include <string>

#include "grammar_vm_code.hh"
#include "grammar_vm_command.hh"
#include "grammar_vm_function_exec_environment.hh"
#include "grammar_vm_stack.hh"
#include "variable_context.hh"



namespace AmbiguityCFG {


// The Grammar Virtual Machine (GrammarVM) is used to compute
// grammar production fragments based on the current state of
// a variable context. Basically this VM provides means to access
// and manipulate the values of variables stored in the context.
class GrammarVM {
 private:
    // The list of precompiled algebra functions is
    // stored as a map.
    hashtable<std::string, GrammarVMCode*> algebraFunctions;

    // This set holds the names of all called functions.
    // It is used for recursion-detection. Recursion may
    // not occur, because we can not calculate when the
    // base case of recursion is reached.
    std::set<std::string> calledFunctionNames;

    // This stack contains the names of called functions.
    // basically this information is used to print a stack
    // trace when a recursion was detected.
    std::vector<std::string> calledFunctionNamesStack;

 public:
    GrammarVM();
    ~GrammarVM();


    // Adds a function definition to this grammar VM.
    void addCompiledFunctionCode(GrammarVMCode* code);

    // Executes a function which is referenced simply by
    // its name. As a second argument the context of initial
    // parameter values is passed. The result of the algebra
    // function is returned as a pointer to a grammar fragment.
    CFG::Base* executeFunction(std::string* functionName, VariableContext* c);

    // Executes a function which is referenced simply by
    // its name. As a second argument the context of initial
    // parameter values is passed. The result of the algebra
    // function is returned as a pointer to a grammar fragment.
    CFG::Base* executeFunction(
      std::string* functionName, MultiVariableContext* c);

    // Returns the list of parameter names of the function
    // named by the given parameter. This method returns an
    // empty list, if the function name is not defined in
    // this VM. Please note that this situation is not
    // distinguishable from that one when a function is
    // defined, but does not expect any parameters at all.
    std::list<std::string*> getParameterNamesForFunction(
      std::string* functionName);
};


}  // namespace AmbiguityCFG


#endif  // SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_HH_
