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
    size_t interval = 0;  // user-specified interval (in seconds)

    explicit Checkpoint(size_t interval): interval(interval) {}

    void include(Printer::Base &stream) {
      stream << "#include \"boost/serialization/vector.hpp\"" << endl;
      stream << "#include \"boost/archive/binary_iarchive.hpp\"" << endl;
      stream << "#include \"boost/archive/binary_oarchive.hpp\"" << endl;
      stream << "#include \"boost/dll.hpp\"" << endl;
      stream << "#include <unistd.h>" << endl;
      stream << "#include <atomic>" << endl;
      stream << "#include <filesystem>" << endl;
      stream << "#include <thread>" << endl << endl;
    }

    void archive(Printer::Base &stream) {
      inc_indent(); inc_indent();
      stream << indent();
      stream << "void archive(const std::string &tname) {" << endl;
      inc_indent();
      stream << indent() << "// save the DP table/array to disk" << endl;
      stream << indent() << "try {" << endl;
      inc_indent();
      stream << indent() << "std::ofstream array_fout(table_path, "
                            "std::ios::binary);" << endl;
      stream << indent() << "if (!(array_fout.good())) {" << endl;
      stream << indent() << "  throw std::ofstream::failure(\"\");" << endl;
      stream << indent() << "}" << endl;
      stream << indent() << "boost::archive::binary_oarchive "
                            "array_out(array_fout);" << endl;
      stream << indent() << "array_out << array;" << endl;
      stream << indent() << "array_fout.close();" << endl;
      dec_indent();
      stream << indent() << "} catch (const std::ofstream::failure &e) {"
             << endl;
      stream << indent() << "  std::cerr << \"Couldn't create table archive "
             << "at path \\\"\" << table_path << \"\\\".\" " << endl;
      stream << indent() << "            << \"Please ensure that the directory "
             << "exists and that you have write permissions "
             << "for this directory.\\n\";" << endl;
      stream << indent() << "} catch (const std::exception &e) {" << endl;
      inc_indent();
      stream << indent() << "std::cerr << \"Error trying to archive \\\"\" "
                            "<< tname << \"\\\" table.\""
             << endl;
      stream << indent() << "          << \" Will retry at "
                            "the next checkpoint...\\n\";" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      dec_indent(); dec_indent();
    }

    void remove(Printer::Base &stream) {
      inc_indent(); inc_indent();
      stream << indent();
      stream << "void remove() {" << endl;
      inc_indent();
      stream << indent() << "std::filesystem::remove(table_path);" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      dec_indent(); dec_indent();
    }

    void init(Printer::Base &stream) {
      inc_indent(); inc_indent(); inc_indent();
      stream << indent() << "std::string executable_name = "
             << "boost::dll::program_location().filename().string();" << endl;
      stream << indent() << "int process_id = getpid();" << endl;
      stream << indent() << "std::string archive_name = tname + \"_\" + "
             << "executable_name + \"_\" + std::to_string(process_id);" << endl;
      stream << indent() << "if (!(path.empty())) {" << endl;
      stream << indent() << "  table_path = path;" << endl;
      stream << indent() << "} else {" << endl;
      stream << indent() << "  table_path = std::filesystem::current_path();"
             << endl << indent() << "}" << endl;
      stream << indent() << "table_path /= archive_name;" << endl;
      stream << indent() << "// read the DP array/table from disk "
                            "and put its contents into array" << endl;
      stream << indent() << "try {" << endl;
      inc_indent();
      stream << indent() << "std::ifstream array_fin(table_path, "
                            "std::ios::binary);" << endl;
      stream << indent() << "if (!(array_fin.good())) {" << endl;
      stream << indent() << "  throw std::ifstream::failure(\"\");" << endl;
      stream << indent() << "}" << endl;
      stream << indent() << "boost::archive::binary_iarchive "
                            "array_in(array_fin);" << endl;
      stream << indent() << "array_in >> array;" << endl;
      stream << indent() << "array_fin.close();" << endl;
      stream << indent() << "// mark the already existing table values "
                            "in the tabulated vector" << endl;
      stream << indent() << "for (long unsigned int i = 0; i < array.size(); "
                            "i++) tabulated[i] = array[i];" << endl;
      dec_indent();
      stream << indent() << "} catch (const std::ifstream::failure &e) {" 
             << endl;
      stream << indent() << "  std::cerr << \"\\\"\" + tname + \"\\\" archive\""
             << endl;
      stream << indent() << "            << " << "\"could not be opened or "
             << "hasn't been archived yet. \""
             << endl;
      stream << indent() << "            << \"This table will be "
             << "initialized empty.\\n\";" << endl;
      stream << indent() << "} catch (const std::exception &e) {" << endl;
      inc_indent();
      stream << indent() << "std::cerr << \"Error \\\"\" << e.what() << \"\\\" "
                            "trying to read \\\"\" << tname << "
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

    void remove_tables(Printer::Base &stream, const nt_tables &tables) {
       inc_indent();
       stream << indent() << "void remove_tables() {" << endl;
       inc_indent();
       for (auto i = tables.begin(); i != tables.end(); ++i) {
         std::string table_name = i->second->table_decl->name();
         stream << indent() << table_name << ".remove();" << endl;
       }
       dec_indent();
       stream << indent() << "}" << endl << endl;
       dec_indent();
    }

    std::string format_interval() {
      // format the user-provided checkpointing interval (for logging)
      int days = interval / 86400;
      int hours = (interval % 86400) / 3600;
      int minutes = ((interval % 86400) % 3600) / 60;
      int seconds = ((interval % 86400) % 3600) % 60;
      return std::to_string(days) + " days, " + 
             std::to_string(hours) + " hours, " +
             std::to_string(minutes) + " minutes and " +
             std::to_string(seconds) + " seconds";
    }
};
}  // namespace Printer

#endif  // SRC_CHECKPOINT_HH_
