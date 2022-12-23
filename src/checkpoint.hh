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
#include "type/base.hh"

typedef hashtable<std::string, Symbol::NT*> nt_tables;

// list of currently supported/serializable datatypes (all primitive)
constexpr Type::Type
SUPPORTED_TYPES[] = {Type::Type::VOID, Type::Type::INTEGER,
                     Type::Type::INT, Type::Type::FLOAT,
                     Type::Type::SIZE, Type::Type::SINGLE,
                     Type::Type::BIGINT};

namespace Printer {
class Checkpoint : public Base {
 public:
  static bool is_supported(const nt_tables &tables) {
     // check datatypes of every table (all tables must have supported type)
     bool answer_type_supported = true;

     for (auto table : tables) {
       Type::Type table_type = table.second->data_type()->getType();
       bool table_supported = false;

       for (Type::Type supported_type : SUPPORTED_TYPES) {
         if (table_type == supported_type) {
           table_supported = true;
           break;
         }
       }

       if (!table_supported) {
         answer_type_supported = false;
         break;
       }
     }

     return answer_type_supported;
  }

  void include(Printer::Base &stream) {
     stream << "#define CHECKPOINTING_INTEGRATED" << endl << endl;
     stream << "extern \"C\" {" << endl;
     stream << indent() << "#include <unistd.h>" << endl;
     stream << "}" << endl;
     stream << "#include \"boost/serialization/vector.hpp\"" << endl;
     stream << "#include \"boost/archive/binary_iarchive.hpp\"" << endl;
     stream << "#include \"boost/archive/binary_oarchive.hpp\"" << endl;
     stream << "#include \"boost/dll.hpp\"" << endl;
     stream << "#include \"boost/filesystem.hpp\"" << endl;
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
             << "at path \" << table_path << \".\\n\"" << endl;
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
     stream << indent() << "boost::filesystem::remove(table_path);" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent();
  }

  void get_table_path(Printer::Base &stream) {
    inc_indent(); inc_indent();
    stream << indent() << "std::string get_table_path() {" << endl;
    inc_indent();
    stream << indent() << "return table_path.string();" << endl;
    dec_indent();
    stream << indent() << "}" << endl << endl;
    dec_indent(); dec_indent();
  }

  void find_table(Printer::Base &stream) {
     inc_indent(); inc_indent();
     stream << indent() << "boost::filesystem::path find_table("
            << "boost::filesystem::path path, const std::string &tname) {"
            << endl;
     inc_indent();
     stream << indent() << "// find the table archive in the "
            << "user-specified directory" << endl;
     stream << indent() << "std::string curr_file;" << endl;
     stream << indent() << "for (const auto &entry : "
            << "boost::filesystem::directory_iterator(path)) {" << endl;
     inc_indent();
     stream << indent() << "curr_file = entry.path().string();" << endl;
     stream << indent() << "if (curr_file.find(tname) != curr_file.npos) {"
            << endl;
     inc_indent();
     stream << indent() << "return entry.path();" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     stream << indent() << "return boost::filesystem::path(\"\");" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent();
  }

  void init(Printer::Base &stream) {
     inc_indent(); inc_indent(); inc_indent();
     stream << indent() << "std::string executable_name = "
            << "boost::dll::program_location().filename().string();" << endl;
     stream << indent() << "int process_id = getpid();" << endl;
     stream << indent() << "std::string archive_name = executable_name + "
            << "\"_\" + std::to_string(process_id);" << endl;
     stream << indent() << "table_path = out_path / (archive_name + \"_\" + "
            << "tname);" << endl;
     stream << indent() << "std::string in_path_error_msg = "
            << "\"at user-specified path\";" << endl << endl;
     stream << indent() << "if (in_path.empty()) {" << endl;
     inc_indent();
     stream << indent() << "in_path = "
            << "parse_checkpoint_log(out_path, tname, executable_name, "
            << "arg_string);" << endl;
     stream << indent() << "in_path_error_msg = \"in Logfile\";" << endl;
     dec_indent();
     stream << indent() << "} else {" << endl;
     inc_indent();
     stream << indent() << "in_path = find_table(in_path, tname);" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     stream << indent() << "// read the DP array/table from disk "
                            "and put its contents into array" << endl;
     stream << indent() << "try {" << endl;
     inc_indent();
     stream << indent() << "std::ifstream array_fin(in_path, "
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
     stream << indent() << "  std::cerr << \"Info: \\\"\" + tname + \"\\\" "
            << "archive\"" << endl;
     stream << indent() << "            << " << "\" could not be found \""
            << " << in_path_error_msg << \" or hasn't been archived yet. \""
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

     stream << "               }" << endl;
     stream << "             }).detach();" << endl;
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

  void remove_log_file(Printer::Base &stream) {
     inc_indent();
     stream << indent() << "void remove_log_file() {" << endl;
     inc_indent();
     stream << indent() << "// remove the log file after "
            << "successfull termination of the program" << endl;
     stream << indent() << "boost::filesystem::remove(logfile_path);" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
  }

  void format_interval(Printer::Base &stream) {
     inc_indent();
     stream << indent() << "std::string format_interval(int interval) {"
            << endl;
     inc_indent();
     stream << indent() << "// format the user-provided checkpointing "
            << "interval (for logging)" << endl;
     stream << indent() << "int days = interval / 86400;" << endl;
     stream << indent() << "int hours = (interval % 86400) / 3600;" << endl;
     stream << indent() << "int minutes = ((interval % 86400) % 3600) / 60;"
            << endl;
     stream << indent() << "int seconds = ((interval % 86400) % 3600) % 60;"
            << endl;
     stream << indent() << "return std::to_string(days) + \" days, \" +"
            << endl;
     stream << indent() << "       std::to_string(hours) + \" hours, \" +"
            << endl;
     stream << indent() << "       std::to_string(minutes) + \" minutes "
            << "and \" + std::to_string(seconds) + \" seconds\";" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
  }

  void get_arg_string(Printer::Base &stream) {
     inc_indent();
     stream << indent() << "std::string get_arg_str(int argc, char **argv) {"
            << endl;
     inc_indent();
     stream << indent() << "// create a string from all relevant command "
            << "line arguments" << endl;
     stream << indent() << "std::stringstream arg_string;" << endl;
     stream << indent() << "int i = 1;" << endl;
     stream << indent() << "while (i < argc) {" << endl;
     inc_indent();
     stream << indent() << "if (std::strcmp(argv[i], \"--checkpointInterval\")"
            << " == 0 ||" << endl;
     stream << indent() << "    std::strcmp(argv[i], \"--checkpointPath\")"
            << " == 0) {" << endl;
     inc_indent();
     stream << indent() << "i += 2;" << endl;
     dec_indent();
     stream << indent() << "} else {" << endl;
     inc_indent();
     stream << indent() << "arg_string << argv[i] << \" \";" << endl;
     stream << indent() << "i++;" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     stream << indent() << "return arg_string.str();" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
  }

  void create_checkpoint_log(Printer::Base &stream, const nt_tables &tables) {
     inc_indent();
     stream << indent() << "void create_checkpoint_log(const gapc::Opts &opts, "
            << "const std::string &arg_string) {" << endl;
     inc_indent();
     stream << indent() << "// initialize a Log file to keep track "
            << "of archive paths" << endl;
     stream << indent() << "std::string executable_name = "
            << "boost::dll::program_location().filename().string();" << endl;
     stream << indent() << "std::string logfile_name = executable_name + "
            << "\"_checkpointing_log.txt\";" << endl;
     stream << indent() << "logfile_path = opts.checkpoint_out_path / "
            << "logfile_name;" << endl;
     stream << indent() << "std::ofstream fout(logfile_path, std::ios::out);"
            << endl;
     stream << indent() << "fout << \"# Format:\\n# [OPTIONS] argv[1] "
            << "argv[2] ...\\n\";" << endl;
     stream << indent() << "fout << \"# path/to/table\\n\";" << endl;
     stream << indent() << "fout << \"[OPTIONS] \"  << arg_string << \"\\n\";"
            << endl;
     for (auto i = tables.begin(); i != tables.end(); ++i) {
       std::string table_name = i->second->table_decl->name();
       stream << indent() << "fout << " << table_name << ".get_table_path()"
              << "<< \"\\n\";" << endl;
     }
     stream << indent() << "fout.close();" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
  }

  void parse_checkpoint_log(Printer::Base &stream) {
     inc_indent(); inc_indent();
     stream << indent() << "boost::filesystem::path parse_checkpoint_log("
            << "boost::filesystem::path path, const std::string &tname,"
            << endl << indent()
                        << "                                           "
            << "const std::string &executable_name, const std::string "
            << "&arg_string) {" << endl;
     inc_indent();
     stream << indent() << "// parse the checkpoint log and look "
            << "for checkpoints" << endl;
     stream << indent() << "// that were created with identical program "
            << "input (stored in arg_string)" << endl;
     stream << indent() << "path /= (executable_name + "
            << "\"_checkpointing_log.txt\");" << endl;
     stream << indent() << "boost::filesystem::path input_table_path(\"\");"
            << endl;
     stream << indent() << "std::ifstream fin(path);" << endl;
     stream << indent() << "if (!(fin.good())) return input_table_path;"
            << endl << endl;
     stream << indent() << "std::string line;" << endl;
     stream << indent() << "std::string options_line_start = \"[OPTIONS] \";"
            << endl;
     stream << indent() << "bool check = false;" << endl;
     stream << indent() << "while (std::getline(fin, line)) {" << endl;
     inc_indent();
     stream << indent() << "if (line[0] == '#') continue;" << endl;
     stream << indent() << "size_t i = line.find(options_line_start);" << endl;
     stream << indent() << "if (i != line.npos) {" << endl;
     inc_indent();
     stream << indent() << "check = false;" << endl;
     stream << indent() << "line.replace(i, options_line_start.length(), "
            << "\"\");" << endl;
     stream << indent() << "if (line == arg_string) check = true;" << endl;
     stream << indent() << "continue;" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     stream << indent() << "if (check && line.find(tname) != line.npos) {"
            << endl;
     inc_indent();
     stream << indent() << "input_table_path = boost::filesystem::path(line);"
            << endl;
     stream << indent() << "break;" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     stream << indent() << "return input_table_path;" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent();
  }
};
}  // namespace Printer

#endif  // SRC_CHECKPOINT_HH_
