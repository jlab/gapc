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


#ifndef SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_FUNCTION_EXEC_ENVIRONMENT_HH_
#define SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_FUNCTION_EXEC_ENVIRONMENT_HH_


#include <list>
#include <string>
#include <vector>

#include "grammar_vm.hh"
#include "grammar_vm_code.hh"
#include "grammar_vm_command.hh"
#include "grammar_vm_stack.hh"
#include "variable_context.hh"


namespace AmbiguityCFG {


// Forward declaration of the grammar VM, since the two files
// for this header and the grammar VM's header are circular
// dependent.
class GrammarVM;


// The Grammar Virtual Machine (GrammarVM) is used to compute
// grammar production fragments based on the current state of
// a variable context. Basically this VM provides means to access
// and manipulate the values of variables stored in the context.
class GrammarVMFunctionExecEnvironment {
 private:
    // Holds a reference to the VM instance which owns
    // this function exec environment.
    GrammarVM* vm;

    // The stack of this grammar VM.
    GrammarVMStack stack;

    // The global variable context which contains all defined
    // variables and their current values.
    MultiVariableContext* context;

    // stores the code that will be executed by this VM
    GrammarVMCode* code;

    // The result of the current algebra function.
    std::list<CFG::Base*> functionResult;

 public:
    GrammarVMFunctionExecEnvironment(GrammarVM* vm, GrammarVMCode* code);
    ~GrammarVMFunctionExecEnvironment();

    // Executes a block of GrammarVM-code.
    void executeCode(VariableContext* c);

    // Executes a block of GrammarVM-code.
    void executeCode(MultiVariableContext* c);

    // Executes a single command.
    void execute(GrammarVMCommand* cmd);
    // Executes a list of commands by using the iterator of the
    // list given as parameter and calling the execute-function
    // for a single command for each command in the list.
    void execute(std::list<GrammarVMCommand*> cmds);

    // Resets the grammar VM and its internal registers.
    // All stored values and registers are set to their
    // start values.
    void reset();

    // Returns the result that was assembled by the commands
    // executed by this grammar VM.
    CFG::Base* getResultProductionFragment();

 private:
    void executeLoadVar(LoadVarCommand* loadVarCmd);
    void executeStoreVar(StoreVarCommand* storeVarCmd);
    void executeCombine();
    void executeReturn();
    void executeCallFunction(CallFunctionCommand* callFunction);

    // Returns a production fragment that is stored in the
    // VarInfoItem. It is required that the parameter is
    // of the sub-class VarDeclInfoItem, otherwise an exception
    // is thrown.
    CFG::Base* extractProductionFragmentFromVarInfoItem(VarInfoItem* infoItem);

    // Creates a string representation of the production
    // fragment and returns it as a pointer.
    std::string* createStringForProductionFragment(
      CFG::Base* productionFragment);
    // Creates a string representation of the vector of
    // production fragments and returns them as a pointer
    // to that string.
    std::string* createStringForProductionFragments(
      std::vector<CFG::Base*> fragments);
};
}  // namespace AmbiguityCFG



#endif  // SRC_AMBIGUITY_CFG_GEN_GRAMMAR_VM_FUNCTION_EXEC_ENVIRONMENT_HH_
