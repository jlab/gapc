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

#ifndef SRC_CHECKPOINT_HH_
#define SRC_CHECKPOINT_HH_

#include <string>
#include "printer.hh"
#include "symbol.hh"
#include "statement/table_decl.hh"

namespace Printer {
class Checkpoint : public Base {
 public:
    size_t interval = 0;

    explicit Checkpoint(size_t interval): interval(interval) {}

    void include(Printer::Base &stream) {
      stream << "#include \"boost/serialization/vector.hpp\"" << endl;
      stream << "#include \"boost/archive/binary_iarchive.hpp\"" << endl;
      stream << "#include \"boost/archive/binary_oarchive.hpp\"" << endl;
      stream << "#include <atomic>" << endl;
      stream << "#include <thread>" << endl << endl;
    }

    void archive(Printer::Base &stream) {
      inc_indent(); inc_indent();
      stream << indent();
      stream << "void archive(const std::string &output_file) {" << endl;
      inc_indent();
      stream << indent() << "// save the DP table/array to disk" << endl;
      stream << indent() << "try {" << endl;
      inc_indent();
      stream << indent() << "std::ofstream array_fout(output_file, "
                            "std::ios::binary);" << endl;
      stream << indent() << "boost::archive::binary_oarchive "
                            "array_out(array_fout);" << endl;
      stream << indent() << "array_out << array;" << endl;
      stream << indent() << "array_fout.close();" << endl;
      dec_indent();
      stream << indent() << "} catch (std::exception &e) {" << endl;
      inc_indent();
      stream << indent() << "std::cerr << \"Error during archiving of \\\"\" "
                            "<< output_file << \"\\\" table!\""
             << endl;
      stream << indent() << "          << \" Will retry at "
                            "the next checkpoint...\\n\";" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      dec_indent(); dec_indent();
    }

    void init(Printer::Base &stream) {
      inc_indent(); inc_indent(); inc_indent();
      stream << indent() << "// read the DP array/table from disk "
                            "and put its contents into array" << endl;
      stream << indent() << "try {" << endl;
      inc_indent();
      stream << indent() << "std::ifstream array_fin(tname, "
                            "std::ios::binary);" << endl;
      stream << indent() << "boost::archive::binary_iarchive "
                            "array_in(array_fin);" << endl;
      stream << indent() << "array_in >> array;" << endl;
      stream << indent() << "array_fin.close();" << endl;
      stream << indent() << "// mark the already existing table values "
                            "in the tabulated vector" << endl;
      stream << indent() << "for (long unsigned int i = 0; i < array.size(); "
                            "i++) tabulated[i] = array[i];" << endl;
      dec_indent();
      stream << indent() << "} catch (const std::exception &e) {" << endl;
      inc_indent();
      stream << indent() << "std::cerr << \"Error \\\"\" << e.what() << \"\\\" "
                            "while reading \\\"\" << tname << "
                            "\"\\\" table!\"" << endl;
      stream << indent() << "          << \" This table will be "
                            "initialized empty!\\n\";" << endl;
      stream << indent() << "tabulated.clear();" << endl;
      stream << indent() << "array.clear();" << endl;
      stream << indent() << "array.resize(newsize);" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent(); dec_indent(); dec_indent();
    }

    typedef hashtable<std::string, Symbol::NT*> nt_tables;
    void archive_periodically(Printer::Base &stream, const nt_tables &tables) {
      inc_indent();
      stream << indent() << "void archive_periodically(std::atomic_bool "
                            "&cancel_token, size_t interval) {" << endl;
      inc_indent();
      stream << indent() << "// save all tables to the disk periodically "
                            "every interval seconds" << endl;
      stream << indent() << "cancel_token.store(true);" << endl;
      stream << indent() << "std::thread([=, &cancel_token]() mutable {"
             << endl;
      stream << indent() << "            while (cancel_token.load()) {"
             << endl;
      stream << indent() << "              std::this_thread::sleep_for("
                            "std::chrono::seconds(interval));" << endl;
      stream << indent() << "              "
                            "if (!cancel_token.load()) break;" << endl;

      for (auto i = tables.begin(); i != tables.end(); ++i) {
        std::string table_name = i->second->table_decl->name();
        stream << "              " << indent();
        stream << table_name << ".archive(\"" << table_name << "\");" << endl;
      }

      stream << "                }" << endl;
      stream << "              }).detach();" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      dec_indent();
    }
};
}  // namespace Printer

#endif  // SRC_CHECKPOINT_HH_
