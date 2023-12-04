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

#ifndef SRC_PRINTER_CFG_PRETTY_PRINT_HH_
#define SRC_PRINTER_CFG_PRETTY_PRINT_HH_

#include <string>

#include "../cfg/cfg.hh"


namespace Printer {


// This is a CFG pretty printer that takes a std::ostream as
// destination for its output and writes a textual representation
// of the grammar to the stream.
// The format of the output is soutable for the grammar ambiguity
// checker found at http://www.brics.dk/grammar/
class CFGPrettyPrint {
 private:
    // A reference to the output stream, which will be used to
    // print the grammar.
    std::ostream &oStream;

    // If this flag is set 'true', class tags are printed
    // into the output, making the data dtructures of the
    // grammar visible.
    bool taggedPrintMode;

    // The command line call that caused this pretty print.
    std::string* commandLineCall;

 public:
    explicit CFGPrettyPrint(std::ostream &oStream);

    // Sets the command line call.
    void setCommandLineCall(std::string* commandLineCall);

    // Starts the pretty print for the CFG.
    void prettyPrint(CFG::CFG* cfg);

 private:
    void ppGrammarProduction(CFG::GrammarProduction* prod);

 public:
    void ppBase(std::string* padding, CFG::Base* b);

 private:
    void ppBaseWrapper(std::string* padding, CFG::BaseWrapper* w);
    void ppSnapshot(std::string* padding, CFG::Snapshot* s);
    void ppEpsilon(CFG::Epsilon* e);
    void ppTerminal(CFG::Terminal* t);
    void ppNonTerminal(CFG::NonTerminal* nt);
    void ppProductionSequence(
      std::string* padding, CFG::ProductionSequence* seq);
    void ppProductionAlternative(
      std::string* padding, CFG::ProductionAlternative* alt);
    // Prints the whole regular expression rule
    void ppRegularExpressionRule(CFG::RegularExpression* regexp);
    // Prints the regexp-identifier only. This is necessary because
    // the name of the regexp is printed different in the grammar
    // than it would be printed in the definition of the regular
    // expression.
    void ppRegExpName(CFG::RegularExpression* regexp);
};


}  // namespace Printer


#endif  // SRC_PRINTER_CFG_PRETTY_PRINT_HH_
