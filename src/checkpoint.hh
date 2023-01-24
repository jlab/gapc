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
#include <array>
#include <vector>
#include <sstream>
#include "printer.hh"
#include "symbol.hh"
#include "statement/table_decl.hh"
#include "type/base.hh"

typedef hashtable<std::string, Symbol::NT*> nt_tables;

namespace Printer {
class Checkpoint : public Base {
 private:
  bool list_ref;  // true when table types are wrapped in ListRef
  std::vector<std::string> string_type_accessors;
  std::vector<std::string> block_type_accessors;

  // list of currently supported/serializable datatypes (all primitive)
  const std::array<Type::Type, 8>
  SUPPORTED_TYPES = {Type::Type::VOID, Type::Type::INTEGER,
                     Type::Type::INT, Type::Type::FLOAT,
                     Type::Type::SIZE, Type::Type::SINGLE,
                     Type::Type::BIGINT, Type::Type::STRING};

  // list of supported compound types
  // (can contain multiple subtypes, which all must be part of SUPPORTED_TYPES)
  const std::array<Type::Type, 1>
  COMPOUND_TYPES = {Type::Type::TUPLE};

  template<size_t n>
  bool has_type(std::array<Type::Type, n> arr, Type::Type type) {
    for (Type::Type supported_type : arr) {
         if (type == supported_type) {
           if (type == Type::Type::STRING) {
              strings = true;
           }
           return true;
         }
    }
    return false;
  }

  void add_string_accessor(Type::Type type,
                           std::vector<std::string> &str_type_accessor) {
    if (type == Type::Type::STRING) {
      std::stringstream string_type_accessor;
      for (auto el : str_type_accessor) string_type_accessor << el;
        // string_type_accessors.push_back(string_type_accessor.str() +
        //                                 ".links");
        string_type_accessors.push_back(string_type_accessor.str() +
                                        ".block_as_int");
      block_type_accessors.push_back(string_type_accessor.str() +
                                     ".get_block()");
      return;
    }
  }

  bool __is_supported(Type::Base *type) {
    Type::Type curr_type = type->getType();

    if (curr_type == Type::Type::LIST) {
       list_ref = true;
       return __is_supported(type->component());
    } else if (has_type(COMPOUND_TYPES, curr_type)) {
       // check if type has left and right (e.g. Tuple)
       bool has_left_and_right = has_type(COMPOUND_TYPES,
                                          type->component()->getType());
       if (has_left_and_right) {
         return __is_supported(type->left()) && __is_supported(type->right());
       } else {
         return __is_supported(type->component());
       }
    }

    return has_type(SUPPORTED_TYPES, curr_type);
  }

  void gen_string_accessors(Type::Base *type,
                            std::vector<std::string> &str_type_accessor) {
    // generate String/Rope accessors
    Type::Type curr_type = type->getType();

    if (curr_type == Type::Type::LIST) {
       gen_string_accessors(type->component(), str_type_accessor);
       return;
    } else if (has_type(COMPOUND_TYPES, curr_type)) {
       // check if type has left and right (e.g. Tuple)
       bool has_left_and_right = has_type(COMPOUND_TYPES,
                                          type->component()->getType());
       if (has_left_and_right) {
         str_type_accessor.push_back(".first");

         gen_string_accessors(type->left(), str_type_accessor);
         str_type_accessor.pop_back();

         str_type_accessor.push_back(".second");
         gen_string_accessors(type->right(), str_type_accessor);
         str_type_accessor.pop_back();

         return;
       } else {
         gen_string_accessors(type->component(), str_type_accessor);
         return;
       }
    }

    add_string_accessor(curr_type, str_type_accessor);
  }

 public:
  /*
     true if tables contain String/Rope objects;
     these are a lot more complicated to serialize than other types,
     so this boolean information is needed when generating the
     source code so additional code can be inserted to enable
     the proper serialization of these types
  */
  bool strings;

  Checkpoint() : list_ref(false), strings(false) {}

  bool is_supported(const nt_tables &tables) {
     // check datatypes of every table (all tables must have supported type)
     bool supported = true;
     Type::Base *table_type = nullptr;

     for (auto table : tables) {
       table_type = table.second->data_type();
       if (!__is_supported(table_type)) {
         supported = false;
         break;
       }
     }

     if (supported && strings) {
       /*
          generate accessors for String/Rope types (if they are part of type)
          needed to access these objects efficiently during (de)serializing;
          this only works if String/Rope objects are wrapped in a ListRef
          object, so return false if ListRef is not part of the type
       */
       if (!list_ref) {
         supported = false;
       } else {
         std::vector<std::string> str_type_accessor;
         gen_string_accessors(table_type, str_type_accessor);
       }
     }

     return supported;
  }

  void include(Printer::Base &stream, const nt_tables &tables) {
     stream << "#define CHECKPOINTING_INTEGRATED" << endl << endl;
     if (list_ref) {
       // set macro if tables contain List_Ref type
       stream << "#define LIST_REF" << endl << endl;
     }
     if (strings) {
       // define macros for easy/efficient access to String/Block objects
       for (size_t i = 0; i < string_type_accessors.size(); i++) {
         if (!(string_type_accessors[i].empty())) {
           stream << "#define S" << i+1 << " "
                  << string_type_accessors[i]
                     .substr(1, string_type_accessors[i].npos)<< endl;
         }

         stream << "#define B" << i+1 << " "
                  << block_type_accessors[i]
                     .substr(1, string_type_accessors[i].npos)<< endl;
       }
       stream << endl;

       // -------
       // define these macros so the code compiles;
       // will remove them once the final method to
       // (de)serialize strings is implemented
       stream << "#define RESTORE_STRING_LINKS(IDX)" << endl;
       stream << "#define ADD_VECTOR_INDEX_TO_STRING(IDX, TABLE_CLASS)" << endl;
       stream << "#define ADD_LISTREF_INDEX_TO_STRING(IDX)" << endl;
     }

     //      ----vvvvvvvv---- (disabled for now)
     if (strings && false) {
       // macro for restoring/overwriting links between Strings
       // after deserializing/loading all tables
       stream << "#define RESTORE_STRING_LINKS(IDX) { \\" << endl
              << "  auto &l = curr_table[IDX].ref(); \\" << endl;
       for (size_t i = 0; i < string_type_accessors.size(); i++) {
         stream << "  RESTORE_LINKS(S" << i+1 << ", B" << i+1 << ") \\"
                << endl;
       }
       stream << "}" << endl << endl;

       stream << "#define RESTORE_LINKS(S, B) { \\" << endl
              << "  for (size_t j = 0; j < l.size(); j++) { \\"
              << endl
              << "    auto *b = l[j].B; \\" << endl
              << "    if (b) { \\" << endl
              << "      auto &links = l[j].S; \\" << endl
              << "      unsigned int link_i = 0; \\" << endl
              << "      unsigned char c = 0; \\" << endl
              << "      while (c < b->pos) { \\" << endl
              << "        switch (b->array[c]) { \\" << endl
              << "          case b->LINK : \\" << endl
              << "          { \\" << endl
              << "            assert(!(links.empty())); \\" << endl
              << "            c++; \\" << endl
              << "            unsigned int table_class = links[link_i++]; \\"
              << endl
              << "            assert(table_class > 0 && "
              << "table_class < nt_tables::end); \\" << endl
              << "            auto &listref_el = (*(tables[--table_class]))"
              << "[links[link_i]].ref()[links[link_i + 1]]; \\" << endl
              << "            link_i += 2; \\" << endl
              << "            auto *nxt_b = listref_el.B1; \\" << endl
              << "            assert(nxt_b != nullptr); \\" << endl
              << "            nxt_b->inc_ref(); \\" << endl
              << "            uint8_t string_n = links[link_i++]; \\" << endl
              << "            switch (string_n) { \\" << endl;
       for (size_t i = 1; i < string_type_accessors.size(); i++) {
         stream << "              case " << i+1 << " : \\" << endl
                << "                nxt_b = listref_el.B" << i+1 << "; \\"
                << endl
                << "                break; \\" << endl;
       }
       stream << "            } \\" << endl
              << "            for (unsigned char p = 0; p < 8; p++) { \\"
              << endl
              << "              b->array[p+c] = ((unsigned char*)&nxt_b)[p]; \\"
              << endl
              << "            } \\" << endl
              << "            c += 8; \\" << endl
              << "            break; \\" << endl
              << "          } \\" << endl
              << "          case b->REP : \\" << endl
              << "            c += 6; \\" << endl
              << "            break; \\" << endl
              << "          case b->SEQ : \\" << endl
              << "            c += 3; \\" << endl
              << "            break; \\" << endl
              << "          default : \\" << endl
              << "            c++; \\" << endl
              << "        } \\" << endl
              << "      } \\" << endl
              << "    } \\" << endl
              << "  } \\" << endl
              << "}" << endl << endl;
       stream << "// add idx of String in vector to all String objects" << endl;
       stream << "#define ADD_VECTOR_INDEX_TO_STRING(IDX, TABLE_CLASS) \\"
              << endl
              << "  auto &l = array[IDX].ref(); \\" << endl
              << "  for (size_t i = 0; i < l.size(); i++) { \\"
              << endl;
       for (size_t i = 0; i < string_type_accessors.size(); i++) {
         stream << "    ADD_VEC(B" << i+1 << ", IDX, TABLE_CLASS) \\" << endl;
       }
       stream << "  }" << endl << endl;

       stream << "// add the vector idx/table class identifier to Block" << endl
              << "// (cf. nt_tables enum for table class enumeration)"  << endl;
       stream << "#define ADD_VEC(B, IDX, T) \\" << endl
              << "  if (l[i].B) { \\" << endl
              << "    if (l[i].B->vec_i == -1) l[i].B->vec_i = IDX; \\" << endl
              << "    if (l[i].B->table_class == 0) "
              << "l[i].B->table_class = T; \\" << endl
              << "  }" << endl << endl;

       stream << "// add idx of String in ListRef object to all "
              << "String objects" << endl;
       stream << "#define ADD_LISTREF_INDEX_TO_STRING(IDX) \\" << endl;
       for (size_t i = 0; i < string_type_accessors.size(); i++) {
         if (i < string_type_accessors.size() - 1) {
           stream << "  ADD_REF(B" << i+1 << ", IDX, " << i+1 << ") \\" << endl;
         } else {
           stream << "  ADD_REF(B" << i+1 << ", IDX, " << i+1 << ")" << endl;
         }
       }
       stream << endl;

       stream << "#define ADD_REF(B, IDX, B_N) \\" << endl
              << "  if (e.B) { \\" << endl
              << "    if (e.B->listref_i == -1) e.B->listref_i = IDX; \\"
              << endl
              << "    if (e.B->string_n == 0) e.B->string_n = B_N; \\" << endl
              << "  }" << endl << endl;

       stream << "enum nt_tables {" << endl;
       inc_indent();
       stream << indent() << "none," << endl;
       for (auto i = tables.begin(); i != tables.end(); ++i) {
         const std::string &table_name = i->second->table_decl->name();
         stream << indent() << table_name << "," << endl;
       }
       stream << indent() << "end" << endl;
       dec_indent();
       stream << "};" << endl << endl;
     }

     stream << "extern \"C\" {" << endl;
     stream << indent() << "#include <unistd.h>" << endl;
     stream << indent() << "#include <sys/resource.h>" << endl;
     stream << "}" << endl;
     stream << "#include \"boost/serialization/vector.hpp\"" << endl;
     stream << "#include \"boost/serialization/utility.hpp\"" << endl;
     stream << "#include \"boost/archive/binary_iarchive.hpp\"" << endl;
     stream << "#include \"boost/archive/binary_oarchive.hpp\"" << endl;
     stream << "#include \"boost/filesystem.hpp\"" << endl;
     stream << "#include \"rtlib/loaded.hh\"" << endl;
     stream << "#include <atomic>" << endl;
     stream << "#include <ctime>" << endl;
     stream << "#include <unordered_map>" << endl;
     stream << "#include <mutex>" << endl;
     stream << "#include <thread>" << endl << endl;
  }

  void restore_string_links(Printer::Base &stream, const nt_tables &tables) {
      inc_indent();
      stream << indent() << "void restore_string_links() {" << endl;
      inc_indent();
      size_t n_tables = tables.size();
      stream << indent() << "int n_tables = " << n_tables << ";" << endl;
      size_t c = 0;
      Type::Base *table_type = nullptr;
      for (auto i = tables.begin(); i != tables.end(); ++i) {
        c++;
        const std::string &table_name = i->second->table_decl->name();
        if (c == 1) {
          stream << indent() << "std::vector<"
                 << i->second->table_decl->datatype()
                 << ">" << endl;
          stream << indent() << "*tables[] = {"
                 << table_name << ".get_table()," << endl;
        } else {
          stream << indent() << "             ";
          if (c < n_tables) {
            stream << table_name << ".get_table()," << endl;
          } else {
            stream << table_name << ".get_table()};" << endl << endl;
          }
        }
      }
      stream << indent() << "// create link map" << endl;
      stream << indent() << "for (int i = 0; i < n_tables; i++) {" << endl;
      inc_indent();
      stream << indent() << "auto &curr_table = *(tables[i]);" << endl;
      stream << indent() << "for (size_t j = 0; "
             << "j < curr_table.size(); j++) {" << endl;
      inc_indent();
      stream << indent() << "auto &l = curr_table[j].ref();" << endl;
      stream << indent() << "for (size_t k = 0; k < l.size(); k++) {" << endl;
      inc_indent();
      for (size_t i = 0; i < string_type_accessors.size(); i++) {
        stream << indent() << "if (l[k].S" << i+1 << ") {" << endl;
        inc_indent();
        stream << indent() << "// add address of block of current string to map"
               << endl;
        stream << indent() << "assert(l[k].S" << i+1
               << " && l[k].B" << i+1 << ");" << endl;
        stream << indent() << "String::Block *b = l[k].B" << i+1 << ";" << endl;
        stream << indent() << "link_map[l[k].S" << i+1 << "] = b;" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
      }
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      stream << indent() << "// restore string links" << endl;

      stream << indent() << "for (int i = 0; i < n_tables; i++) {" << endl;
      inc_indent();
      stream << indent() << "auto &curr_table = *(tables[i]);" << endl;
      stream << indent() << "for (size_t j = 0; j < curr_table.size(); j++) {"
             << endl;
      inc_indent();
      stream << indent() << "auto &l = curr_table[j].ref();" << endl;
      stream << indent() << "for (size_t k = 0; k < l.size(); k++) {" << endl;
      inc_indent();
      for (size_t i = 0; i < string_type_accessors.size(); i++) {
        stream << indent() << "if (l[k].B" << i+1 << ") {" << endl;
        inc_indent();
        stream << indent() << "assert(l[k].S" << i+1
               << " && l[k].B" << i+1 << ");" << endl;
        stream << indent() << "String::Block *b = l[k].B" << i+1 << ";" << endl;
        stream << indent() << "unsigned char c = 0;" << endl;
        stream << indent() << "// replace addresses of all links with "
               << "new addresses of respective blocks after deserialization"
               << endl;
        stream << indent() << "while (c < b->pos) {" << endl;
        inc_indent();
        stream << indent() << "switch (b->array[c]) {" << endl;
        inc_indent();
        stream << indent() << "case b->REP :" << endl;
        inc_indent();
        stream << indent() << "c += 6;" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "case b->SEQ :" << endl;
        inc_indent();
        stream << indent() << "c += 3;" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "case b->LINK :" << endl;
        stream << indent() << "{" << endl;
        inc_indent();
        stream << indent() << "c++;" << endl;
        stream << indent() << "uintptr_t linked_block = "
               << "reinterpret_cast<uintptr_t>(b->get_link(c));" << endl;
        stream << indent() << "assert(link_map.find(linked_block) != "
               << "link_map.end());" << endl;
        stream << indent() << "String::Block *next_block = "
               << "link_map[linked_block];" << endl;
        stream << indent() << "assert(next_block);" << endl;
        stream << indent() << "// overwrite memory address of linked block"
               << "with memory address of the same block after deserialization"
               << endl;
        stream << indent() << "assert(c + sizeof(String::Block*) <= b->pos);"
               << endl;
        stream << indent() << "for (unsigned char t = 0; t < "
               << "sizeof(String::Block*); t++, c++) {"
               << endl;
        inc_indent();
        stream << indent() << "b->array[c] = ((unsigned char*)&next_block)[t];"
               << endl;
        dec_indent();
        stream << indent() << "}" << endl;
        stream << indent() << "next_block->inc_ref();" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
        stream << indent() << "default :" << endl;
        inc_indent();
        stream << indent() << "c++;" << endl;
        dec_indent(); dec_indent();
        stream << indent() << "}" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
        dec_indent();
      }
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
  }

  void archive(Printer::Base &stream) {
     inc_indent(); inc_indent();
     stream << indent();
     stream << "void archive(const std::string &tname) {" << endl;
     inc_indent();
     stream << indent() << "// save the DP table/array to disk" << endl;
     stream << indent() << "try {" << endl;
     inc_indent();
     stream << indent() << "/* create temp archive and replace last archive "
            << "with new archive" << endl
            << indent() << "   once new archive has been created instead "
            << "of just overwriting" << endl
            << indent() << "   the last archive to avoid file corruption "
            << "if process crashes during overwrite */" << endl;
     stream << indent() << "std::ofstream array_fout(tmp_out_table_path.c_str()"
            << ", std::ios::binary);" << endl;
     stream << indent() << "if (!(array_fout.good())) {" << endl;
     stream << indent() << "  throw std::ofstream::failure(\"\");" << endl;
     stream << indent() << "}" << endl;
     stream << indent() << "boost::archive::binary_oarchive "
                            "array_out(array_fout);" << endl;
     stream << indent() << "// lock the mutex so main thread can't "
            << "write during archiving" << endl;
     stream << indent() << "std::lock_guard<std::mutex> lock(m);" << endl;
     stream << indent() << "array_out << array;" << endl;
     stream << indent() << "array_fout.close();" << endl;
     stream << indent() << "boost::filesystem::rename(tmp_out_table_path, "
            << "out_table_path);" << endl;
     stream << indent() << "std::cerr << \"Info: Archived \\\"\" << tname << "
            << "\"\\\" table into \" << out_table_path << \".\" "
            << "<< std::endl;" << endl;
     dec_indent();
     stream << indent() << "} catch (const std::ofstream::failure &e) {"
             << endl;
     stream << indent() << "  std::cerr << \"Couldn't create table archive "
             << "at path \" << out_table_path << \".\\n\"" << endl;
     stream << indent() << "            << \"Please ensure that the directory "
             << "exists and that you have write permissions "
             << "for this directory.\\n\";" << endl;
     stream << indent() << "} catch (const std::exception &e) {" << endl;
     inc_indent();
     stream << indent() << "std::time_t curr_time = std::time(nullptr);"
            << endl;
     stream << indent() << "char curr_time_str[sizeof(\"hh:mm:ss\")];"
            << endl;
     stream << indent() << "std::strftime(curr_time_str, sizeof(curr_time_str),"
            << " \"%T\", std::localtime(&curr_time));" << endl;
     stream << indent() << "std::cerr << \"[\" << curr_time_str << \"] "
            << "Error trying to archive \\\"\" << tname << \"\\\" table.\""
             << endl;
     stream << indent() << "          << \" Will retry in \" "
            << "<< formatted_interval << \".\\n\";" << endl;
     stream << indent() << "boost::filesystem::remove(tmp_out_table_path);"
            << endl;
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
     stream << indent() << "boost::filesystem::remove(out_table_path);" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent();
  }

  void get_table(Printer::Base &stream, const Type::Base &dtype) {
     inc_indent(); inc_indent();
     stream << indent();
     stream << "std::vector<" << dtype << "> *get_table() {" << endl;
     inc_indent();
     stream << indent() << "return &array;" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent();
  }

  void get_out_table_path(Printer::Base &stream) {
    inc_indent(); inc_indent();
    stream << indent() << "std::string get_out_table_path() const {" << endl;
    inc_indent();
    stream << indent() << "return out_table_path.string();" << endl;
    dec_indent();
    stream << indent() << "}" << endl << endl;
    dec_indent(); dec_indent();
  }

  void get_tabulated_vals_percentage(Printer::Base &stream) {
     inc_indent(); inc_indent();
     stream << indent();
     stream << "double get_tabulated_vals_percentage() const {" << endl;
     inc_indent();
     stream << indent() << "return static_cast<double>(tabulated_vals_counter) "
            << "/ tabulated.size() * 100.0;" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent();
  }

  void init(Printer::Base &stream) {
     inc_indent(); inc_indent(); inc_indent();
     stream << indent() << "this->formatted_interval = formatted_interval;"
            << endl;
     stream << indent() << "out_table_path = out_path / (file_prefix + \"_\" + "
            << "tname);" << endl;
     stream << indent() << "tmp_out_table_path = out_path / (file_prefix + "
            << "\"_\" + tname + \"_new\");" << endl << endl;
     stream << indent() << "if (!(in_path.empty())) {" << endl;
     inc_indent();
     stream << indent() << "// read the DP array/table from disk "
                            "and put its contents into array" << endl;
     stream << indent() << "try {" << endl;
     inc_indent();
     stream << indent() << "parse_checkpoint_log(tname, arg_string, in_path);"
            << endl << endl;
     stream << indent() << "std::ifstream array_fin(in_table_path.c_str(), "
            << "std::ios::binary);" << endl;
     stream << indent() << "if (!(array_fin.good())) {" << endl;
     stream << indent() << "  throw std::ifstream::failure(\"\");" << endl;
     stream << indent() << "}" << endl;
     stream << indent() << "boost::archive::binary_iarchive "
                            "array_in(array_fin);" << endl;
     stream << indent() << "array_in >> array;" << endl;
     stream << indent() << "array_fin.close();" << endl << endl;
     stream << indent() << "// mark the already existing table values "
                            "in the tabulated vector" << endl;
     stream << indent() << "for (size_t i = 0; i < array.size(); "
            << "i++) {" << endl;
     inc_indent();
     stream << indent() << "if (is_loaded(array[i])) {" << endl;
     inc_indent();
     stream << indent() << "tabulated[i] = true;" << endl;
     stream << indent() << "tabulated_vals_counter++;" << endl;
     dec_indent();
     stream << indent() << "}"  << endl;
     dec_indent();
     stream << indent() << "}"  << endl << endl;
     stream << indent() << "std::cerr << \"Info: Successfully loaded checkpoint"
            << " for \\\"\" << tname << \"\\\" table. \"" << endl;
     stream << indent() << "          << \"Will continue calculating from here."
            << "\" << std::endl;" << endl;
     dec_indent();
     stream << indent() << "} catch (const ParseException &e) {"
            << endl;
     inc_indent();
     stream << indent() << "std::cerr << e.what() << std::endl;" << endl;
     stream << indent() << "array.resize(newsize);" << endl;
     dec_indent();
     stream << indent() << "} catch (const std::ifstream::failure &e) {"
            << endl;
     inc_indent();
     stream << indent() << "std::cerr << \"Info: \\\"\" + tname + \"\\\" "
            << "archive\"" << endl;
     stream << indent() << "          << " << "\" could not be found in "
            << "Logfile or hasn't been archived yet. \"" << endl;
     stream << indent() << "          << \"This table will be "
            << "initialized empty.\\n\";" << endl;
     stream << indent() << "tabulated.clear();" << endl;
     stream << indent() << "tabulated.resize(newsize);" << endl;
     stream << indent() << "array.clear();" << endl;
     stream << indent() << "array.resize(newsize);" << endl;
     dec_indent();
     stream << indent() << "} catch (const std::exception &e) {" << endl;
     inc_indent();
     stream << indent() << "std::cerr << \"Error \\\"\" << e.what() << \"\\\" "
                            "trying to read \\\"\" << tname << "
                            "\"\\\" table!\"" << endl;
     stream << indent() << "          << \" This table will be "
                            "initialized empty.\\n\";" << endl;
     stream << indent() << "tabulated.clear();" << endl;
     stream << indent() << "tabulated.resize(newsize);" << endl;
     stream << indent() << "array.clear();" << endl;
     stream << indent() << "array.resize(newsize);" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "} else {" << endl;
     inc_indent();
     stream << indent() << "array.resize(newsize);" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
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
       const std::string &table_name = i->second->table_decl->name();
       stream << "              " << indent();
       stream << table_name << ".archive(\"" << table_name << "\");" << endl;
     }
     stream << "              " << indent();
     stream << "update_checkpoint_log();" << endl;
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
       const std::string &table_name = i->second->table_decl->name();
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
     stream << indent() << "if (std::strcmp(argv[i], \"-p\")"
            << " == 0 ||" << endl;
     stream << indent() << "    std::strcmp(argv[i], \"-I\")"
            << " == 0 ||" << endl;
     stream << indent() << "    std::strcmp(argv[i], \"-O\")"
            << " == 0 ||" << endl;
     stream << indent() << "    std::strcmp(argv[i], \"--checkpointOutput\")"
            << " == 0 ||" << endl;
     stream << indent() << "    std::strcmp(argv[i], \"--checkpointInput\")"
            << " == 0 ||" << endl;
     stream << indent() << "    std::strcmp(argv[i], \"--checkpointInterval\")"
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

  void create_checkpoint_log(Printer::Base &stream, const nt_tables &tables,
                             const std::string &call_string,
                             const std::string &version_string) {
     inc_indent();
     stream << indent() << "void create_checkpoint_log("
            << "const gapc::Opts &opts, const std::string &arg_string) {"
            << endl;
     inc_indent();
     stream << indent() << "// initialize a Log file to keep track "
            << "of archive paths" << endl;
     stream << indent() << "std::ofstream fout(logfile_path.c_str(), "
            << "std::ios::out);" << endl;
     stream << indent() << "fout << \"# Format:\\n# [OPTIONS] argv[1] "
            << "argv[2] ...\\n\";" << endl;
     // stream << indent() << "fout << \"# [GAPC-CALL] \" << GAPC_CALL_STRING "
     //        << "\"\\n\";" << endl;
     for (auto i = tables.begin(); i != tables.end(); ++i) {
       const std::string &table_name = i->second->table_decl->name();
       stream << indent() << "fout << \"# path/to/" << table_name << "\\n\";"
              << endl;
     }
     stream << indent() << "fout << \"# [GAPC CALL] GAPC call string\\n\";"
            << endl;
     stream << indent() << "fout << \"# [GAPC VERSION] GAPC version\\n\";"
            << endl;
     stream << indent() << "fout << \"# [CHECKPOINT] cpu_time(s) max_rss(kb) ";
     for (auto i = tables.begin(); i != tables.end(); ++i) {
       const std::string &table_name = i->second->table_decl->name();
       stream << table_name << "(%) ";
     }
     stream << "\\n\";" << endl;
     stream << indent() << "fout << \"[OPTIONS] \" << arg_string << \"\\n\";"
            << endl;
     for (auto i = tables.begin(); i != tables.end(); ++i) {
       const std::string &table_name = i->second->table_decl->name();
       stream << indent() << "fout << " << table_name << ".get_out_table_path()"
              << "<< \"\\n\";" << endl;
     }
     stream << indent() << "fout << \"[GAPC CALL] "
            << call_string << "\\n\";" << endl;
     stream << indent() << "fout << \"[GAPC VERSION] "
            << version_string << "\\n\";" << endl;
     stream << indent() << "fout.close();" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
  }

  void update_checkpoint_log(Printer::Base &stream, const nt_tables &tables) {
     inc_indent();
     stream << indent() << "void update_checkpoint_log() {" << endl;
     inc_indent();
     stream << indent() << "// add cpu time and max rss from start of "
            << "program to the current checkpoint" << endl;
     stream << indent() << "std::ofstream fout(logfile_path.c_str(), "
            << "std::ios::out | std::ios::app);" << endl;
     stream << indent() << "struct rusage curr_usage;" << endl;
     stream << indent() << "int success = getrusage(RUSAGE_SELF, &curr_usage);"
            << endl;
     stream << indent() << "if (success == 0) {" << endl;
     inc_indent();
     stream << indent() << "long max_rss = curr_usage.ru_maxrss;" << endl;
     stream << indent() << "std::clock_t curr_cpu_time = std::clock();" << endl;
     stream << indent() << "double cpu_time_since_start = 1000.0 * "
           << "(curr_cpu_time - start_cpu_time) / CLOCKS_PER_SEC;" << endl;
     stream << indent() << "fout << \"[CHECKPOINT] \""
            << " << std::to_string(cpu_time_since_start / 1000.0) << \" \""
            << " << std::to_string(max_rss) << \" \";" << endl;
     for (auto i = tables.begin(); i != tables.end(); ++i) {
       const std::string &table_name = i->second->table_decl->name();
       stream << indent() << "fout << " << table_name
              << ".get_tabulated_vals_percentage() << \" \";" << endl;
     }
     stream << endl << indent() << "fout << \"\\n\";" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     stream << indent() << "fout.close();" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
  }

  void parse_checkpoint_log(Printer::Base &stream) {
     inc_indent(); inc_indent();
     stream << indent() << "void parse_checkpoint_log("
            << "const std::string &tname, const std::string &arg_string,"
            << endl
            << indent() << "                          "
            << "const boost::filesystem::path &path) {"
            << endl;
     inc_indent();
     stream << indent() << "// parse the checkpoint log and look "
            << "for checkpoints" << endl;
     stream << indent() << "// that were created with identical program "
            << "input (if that info is available)" << endl;
     stream << indent() << "std::ifstream fin(path.c_str(), "
            << "std::ios::binary);" << endl;
     stream << indent() << "if (!(fin.good())) return;"
            << endl << endl;
     stream << indent() << "std::string line;" << endl;
     stream << indent() << "std::string options_line_start = \"[OPTIONS] \";"
            << endl;
     stream << indent() << "while (std::getline(fin, line)) {" << endl;
     inc_indent();
     stream << indent() << "if (line[0] == '#') continue;" << endl;
     stream << indent() << "size_t i = line.find(options_line_start);" << endl;
     stream << indent() << "if (i != line.npos) {" << endl;
     inc_indent();
     stream << indent() << "line.replace(i, options_line_start.length(), "
            << "\"\");" << endl;
     stream << indent() << "if (line != arg_string) {" << endl;
     inc_indent();
     stream << indent() << "throw ParseException(\"Error: The checkpoint "
            << "for \\\"\" + tname + \"\\\" table was created with different \""
            << endl
            << indent() << "                     \"command line inputs than"
            << " this program was executed with.\\n\"" << endl
            << indent() << "                     \"       This table will "
            << "be initialized empty.\");" << endl;
     stream << indent() << "return;" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     stream << indent() << "if (line.find(tname) != line.npos) {"
            << endl;
     inc_indent();
     stream << indent() << "in_table_path = boost::filesystem::path(line);"
            << endl;
     stream << indent() << "return;" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent();
  }
};
}  // namespace Printer

#endif  // SRC_CHECKPOINT_HH_
