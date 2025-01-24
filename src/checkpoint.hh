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

    * Author: fymue
}}} */

#ifndef SRC_CHECKPOINT_HH_
#define SRC_CHECKPOINT_HH_

#include <string>
#include <vector>
#include <list>
#include <utility>
#include <sstream>
#include <iostream>
#include "printer.hh"
#include "symbol.hh"
#include "statement/table_decl.hh"
#include "type/base.hh"

// default checkpointing interval
// (modify here if you want to change it)
#define DEFAULT_CP_INTERVAL_SEC     3600
#define DEFAULT_CP_INTERVAL_MIN     (DEFAULT_CP_INTERVAL_SEC / 60)

typedef hashtable<std::string, Symbol::NT*> nt_tables;

namespace Printer {
/*
  contains methods that handle the insertion of the checkpointing
  routine into the generated header file;
  this class also extends Base to get access to indent-related methods
*/
class Checkpoint : public Base {
 private:
  /*
     true if table types are wrapped in List_Ref;
     this information is required whenever the String type
     is part of a table's datatype, because tables containing
     "String" objects need to be additionally processed after deserialization,
     which can only work if "String" objects are wrapped in a List_Ref object
  */
  bool list_ref;

  std::vector<std::string> string_type_accessors, block_type_accessors,
                           subseq_type_accessors;

  // currently supported/serializable GAPC-internal datatypes (2023-02-20)
  const std::vector<Type::Type>
  SUPPORTED_TYPES = {Type::Type::VOID, Type::Type::INTEGER,
                     Type::Type::INT, Type::Type::FLOAT,
                     Type::Type::SIZE, Type::Type::SINGLE,
                     Type::Type::BIGINT, Type::Type::STRING,
                     Type::Type::SHAPE, Type::Type::SUBSEQ,
                     Type::Type::EXTERNAL, Type::Type::CHAR};

/*
   currently supported/(de)serializable external datatyes (2023-02-20);
   except for Rope (which is part of rtlib), all of these types
   are defined in the fold-grammars repository;
   unfortunately programs that tabulate external types
   with "String" or "Subsequence" members cannot be checkpointed,
   because no member information can be generated for external
   types since C++ doesn't support reflection,
   which makes accessor generation and thus checkpointing
   those types impossible;
   if you want to checkpoint a program that tabulates non-
   supported external types (that also don't contain "String"
   or "Subsequence" objects), make sure to provide a "serialize"
   method for them (cf. e.g. rope.hh, line 242 or
   https://www.boost.org/doc/libs/1_80_0/libs/serialization/doc/tutorial.html)
   and to add their names to this vector;
*/
const std::vector<std::string>
SUPPORTED_EXTERNAL_TYPES = {"Rope", "answer_pknot_mfe", "pktype",
                            "answer_pknot_mfecovar", "mfecovar",
                            "pftuple", "answer_pknot_pfunc",
                            "shape_t", "answer_macrostate_mfe",
                            "answer_macrostate_pfunc",
                            "answer_ali_pfunc_macrostate",
                            "mfecovar_macrostate"};

  // check if the currently looked at type is contained
  // in SUPPORTED_TYPES or SUPPORTED_EXTERNAL_TYPES
  bool has_type(const std::vector<Type::Type> &supported_types,
                const std::vector<std::string> &supported_ext_types,
                Type::Type type, Type::Base *t) {
    for (Type::Type supported_type : supported_types) {
      if (type == supported_type) {
        if (type == Type::Type::STRING) {
          strings = true;
        } else if (type == Type::Type::SUBSEQ) {
          subseq = true;
        } else if (type == Type::Type::EXTERNAL) {
          // only allow external type "Rope" and some fold-grammars types
          Type::External *e = dynamic_cast<Type::External*>(t);
          bool supported_external_type = false;
          for (const std::string &supported_ext : supported_ext_types) {
            if (*e->name == supported_ext) {
              supported_external_type = true;
              break;
            }
          }
          if (!supported_external_type) {
            std::cerr << "Error: External type \"" << *e->name
                      << "\" cannot be serialized by default.\n"
                      << "Please provide a serialize method for this type.\n";
            return false;
          }
        }
        return true;
      }
    }
    std::cerr << "Error: Type \"" << *t << "\" cannot be serialized.\n";
    return false;
  }

  /*
     add the final accessor to the currently looked at type
     if the current type is "String", the it's block needs to be
     accessed (str.get_block()) as well as the address of that block
     as an integer (str.block_as_int);
     if the current type is "Subsequence", it's "seq" member needs
     to be accessed (subseq.seq)
  */
  void add_type_accessor(Type::Type type,
                         std::vector<std::string> &type_accessor) {
    if (type == Type::Type::STRING) {
      std::stringstream final_type_accessor;
      for (auto el : type_accessor) final_type_accessor << el;
      string_type_accessors.push_back(final_type_accessor.str() +
                                      ".block_as_int");
      block_type_accessors.push_back(final_type_accessor.str() +
                                     ".get_block()");
    } else if (type == Type::Type::SUBSEQ) {
      std::stringstream final_type_accessor;
      for (auto el : type_accessor) final_type_accessor << el;
      subseq_type_accessors.push_back(final_type_accessor.str() +
                                      ".seq");
    }
  }

  bool __is_supported(Type::Base *type) {
    Type::Type curr_type = type->getType();

    if (curr_type == Type::Type::LIST) {
       // List_Ref or Hash::Ref
       list_ref = true;
       return __is_supported(type->component());
    } else if (curr_type == Type::Type::TUPLE) {
       // algebra product
       // check if type has left and right (e.g. std::pair)
       bool has_left_and_right =
        type->component()->getType() == Type::Type::TUPLE;
       if (has_left_and_right) {
         return __is_supported(type->left()) && __is_supported(type->right());
       } else {
         return __is_supported(type->component());
       }
    } else if (curr_type == Type::Type::TUPLEDEF ||
               curr_type == Type::Type::DEF) {
      // user-defined type
      // (gets converted to struct; all members need to be supported)
      Type::TupleDef *s = NULL;
      if (curr_type == Type::Type::DEF) {
        Type::Def *d = dynamic_cast<Type::Def*>(type);

        // sometimes DEF is really just TUPLEDEF in disguise, so check for that
        if (d->rhs->getType() == Type::Type::TUPLEDEF) {
          s = dynamic_cast<Type::TupleDef*>(d->rhs);
        } else {
          return __is_supported(d->rhs);
        }
      } else {
        s = dynamic_cast<Type::TupleDef*>(type);
      }

      // check if every member of struct/user-defined type is supported
      for (std::list<std::pair<Type::Name*, std::string*>*>::const_iterator
        i = s->list.begin(); i != s->list.end(); ++i) {
        Type::Base *curr_type = (*i)->first->lhs->simple();
        if (!__is_supported(curr_type)) return false;
      }
      user_def = true;
      return true;
    } else if (curr_type == Type::Type::USAGE) {
      Type::Usage *u = dynamic_cast<Type::Usage*>(type);
      return __is_supported(u->base);
    }

    return has_type(SUPPORTED_TYPES, SUPPORTED_EXTERNAL_TYPES,
                    curr_type, type);
  }

  /*
     generate accessors for types so they can be indexed/addressed
     (required for "String", "Subsequence" and user-defined types);
     these accessors will be inserted into the generated code
     as macros, which will be expanded in the respective target
     functions (restore_string_links for "String" and
     "add_seq_to_subseqs" for "Subsequence") and used to access
     these objects efficiently without the need to do any searching
     where exactly these objects are stored
  */
  void gen_type_accessors(Type::Base *type,
                          std::vector<std::string> &type_accessor) {
    Type::Type curr_type = type->getType();

    if (curr_type == Type::Type::LIST) {
       gen_type_accessors(type->component(), type_accessor);
       return;
    } else if (curr_type == Type::Type::TUPLE) {
       // check if type has left and right (e.g. std::pair)
       bool has_left_and_right =
        type->component()->getType() == Type::Type::TUPLE;
       if (has_left_and_right) {
         type_accessor.push_back(".first");

         gen_type_accessors(type->left(), type_accessor);
         type_accessor.pop_back();

         type_accessor.push_back(".second");
         gen_type_accessors(type->right(), type_accessor);
         type_accessor.pop_back();

         return;
       } else {
         gen_type_accessors(type->component(), type_accessor);
         return;
       }
    } else if (curr_type == Type::Type::TUPLEDEF ||
               curr_type == Type::Type::DEF) {
      // user-defined type
      Type::TupleDef *s = NULL;
      if (curr_type == Type::Type::DEF) {
        Type::Def *d = dynamic_cast<Type::Def*>(type);

        // sometimes DEF is really just TUPLEDEF in disguise, so check for that
        if (d->rhs->getType() == Type::Type::TUPLEDEF) {
          s = dynamic_cast<Type::TupleDef*>(d->rhs);
        } else {
          return gen_type_accessors(d->rhs, type_accessor);
        }
      } else {
        s = dynamic_cast<Type::TupleDef*>(type);
      }

      for (std::list<std::pair<Type::Name*, std::string*>*>::const_iterator
        i = s->list.begin(); i != s->list.end(); ++i) {
        Type::Base *member_type = (*i)->first->lhs->simple();
        const std::string &member_name = *(*i)->second;  // name of member var

        type_accessor.push_back("." + member_name);
        gen_type_accessors(member_type, type_accessor);
        type_accessor.pop_back();
      }

      return;
    }

    add_type_accessor(curr_type, type_accessor);
  }

 public:
  /*
     true if tables contain "String" objects;
     these are a lot more complicated to deserialize than other types as
     they require the manual restoration of the links between Strings
     so this boolean information is needed when generating the
     source code so additional code can be inserted to enable
     the proper deserialization of these types
  */
  bool strings;

  /*
    true if tables contain "Subsequence" objects (require additional processing)
    this will most likely only be needed if a Subsequence object
    is part of a user-specified type which is tabulated as e.g.
    a pretty-print algebra
  */
  bool subseq;

  /*
     true if tables contain a user-defined type;
     this type is represented as a struct in the generated header file;
     if the checkpointing option was specified, this struct needs
     a serialize method so the tables containing this type can be
     (de)serialized properly
  */
  bool user_def;

  /*
     true if the currently parsed out class is a buddy class,
     in which case the checkpointing routine doesn't need to
     be integrated
  */
  bool is_buddy;

  /*
     true if cyk-style code generation was requested;
     this requires an adjustment to the checkpointing mechanism,
     because the functions that tabulate the non-terminal
     tables don't peform any lookups for already tabulated
     cells in cyk mode;
     instead all tables are filled in a (nested) loop,
     which ensures that all table values for indices smaller
     than the current index have already been calculated;
     in order to checkpoint programs with this codestyle,
     the loop indices are archived instead of the tabulated
     vector so the program knows at which point/at which loop
     iteration to continue calculating
  */
  bool cyk;

  Checkpoint() : list_ref(false), strings(false),
                 subseq(false), user_def(false),
                 is_buddy(false), cyk(false) {}

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

     if (supported && (strings || subseq || user_def)) {
       /*
          generate accessors for String/Subsequence/user-defined types;
          these are needed to access these objects efficiently during
          (de)serialization of the checkpointed archives;
          for Strings: only works if String objects are wrapped in a List_Ref
          object, so return false if List_Ref is not part of the type
       */
       if (strings && !list_ref) {
         supported = false;
       } else {
         std::vector<std::string> type_accessor;
         gen_type_accessors(table_type, type_accessor);
       }
     }

     return supported;
  }

  void macros(Printer::Base &stream) {
     stream << "#define DEFAULT_CHECKPOINT_INTERVAL "
            << DEFAULT_CP_INTERVAL_SEC << endl << endl;
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
           stream << "#define B" << i+1 << " "
                  << block_type_accessors[i]
                     .substr(1, block_type_accessors[i].npos)<< endl;
         }
       }
       stream << endl;
     }
     if (subseq) {
       // define macros for easy/efficient access to Subsequence objects
       for (size_t i = 0; i < subseq_type_accessors.size(); i++) {
         if (!(subseq_type_accessors[i].empty())) {
           stream << "#define SUBSEQ" << i+1 << " "
                  << subseq_type_accessors[i]
                     .substr(1, subseq_type_accessors[i].npos)<< endl;
         }
       }
       stream << endl;
     }
  }

  void include(Printer::Base &stream, const nt_tables &tables) {
     stream << "extern \"C\" {" << endl;
     stream << indent() << "#include <unistd.h>" << endl;
     stream << indent() << "#include <sys/resource.h>" << endl;
     stream << "}" << endl;
     stream << "#include \"boost/serialization/vector.hpp\"" << endl;
     stream << "#include \"boost/serialization/utility.hpp\"" << endl;
     stream << "#include \"boost/serialization/access.hpp\"" << endl;
     stream << "#include \"boost/archive/binary_iarchive.hpp\"" << endl;
     stream << "#include \"boost/archive/binary_oarchive.hpp\"" << endl;
     stream << "#include \"boost/filesystem.hpp\"" << endl;
     stream << "#include <atomic>" << endl;
     stream << "#include <ctime>" << endl;
     stream << "#include <unordered_map>" << endl;
     stream << "#include <algorithm>" << endl;
     if (cyk) {
       stream << "#include \"boost/archive/text_oarchive.hpp\"" << endl;
       stream << "#include \"boost/archive/text_iarchive.hpp\"" << endl;
       stream << "#ifdef _OPENMP" << endl;
       stream << "#include \"rtlib/fair_shared_mutex.hh\"" << endl;
       stream << "#else" << endl;
       stream << "#include \"rtlib/fair_mutex.hh\"" << endl;
       stream << "#endif" << endl;
     } else {
       stream << "#include <mutex>" << endl;
     }
     stream << "#include <thread>" << endl << endl;
  }

  /*
     this method restores the links between the String objects of the
     different tables;
     internally, String objects can only store 59 characters/bytes of data.
     In order to allow for longer strings, String objects are linked together
     similarly to a linked list;
     the links however are directly written into the data buffer of a String
     object and not stored as a ptr member of the object, which renders the
     links useless after deserialization since the memory addresses that were
     written into the data buffers of the strings are now no longer valid;
     to restore these links, this function loops over every string and looks at
     an additional new String member ("block_as_int"), which contains the address
     of the links as an integer value;
     these can be used to perform lookups whenever a link is parsed, at which
     point the broken address can be overwritten and the actual address
     of the linked string can be written into the data buffer, which restores
     the links between the strings
  */
  void restore_string_links(Printer::Base &stream, const nt_tables &tables) {
      inc_indent();
      stream << indent() << "void restore_string_links() {" << endl;
      inc_indent();
      size_t n_tables = tables.size();
      stream << indent() << "int n_tables = " << n_tables << ";" << endl;
      stream << indent() << "size_t max_table_size = 0;" << endl;
      stream << indent() << "std::unordered_map<uintptr_t, String::Block*> "
             << "link_map;" << endl;
      stream << indent() << "std::unordered_map<" << endl
             << indent() << " uintptr_t, std::vector<std::pair<int, size_t>>> "
             << "block_linked_at;" << endl;
      stream << indent() << "std::vector<std::pair<int, size_t>> "
             << "broken_listrefs, initial_broken_listrefs;" << endl;
      size_t c = 0;
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
      if (!cyk) {
        c = 0;
        for (auto i = tables.begin(); i != tables.end(); ++i) {
          c++;
          const std::string &table_name = i->second->table_decl->name();
          if (c == 1) {
            stream << indent() << "std::vector<bool> *tabulated[] = {"
                   << table_name << ".get_tabulated()," << endl;
          } else {
            stream << indent() << "                                  ";
            if (c < n_tables) {
              stream << table_name << ".get_tabulated()," << endl;
            } else {
              stream << table_name << ".get_tabulated()};" << endl << endl;
            }
          }
        }

        c = 0;
        for (auto i = tables.begin(); i != tables.end(); ++i) {
          c++;
          const std::string &table_name = i->second->table_decl->name();
          if (c == 1) {
            stream << indent() << "size_t *tabulated_counts[] = {"
                   << table_name << ".get_tabulated_count()," << endl;
          } else {
            stream << indent() << "                                  ";
            if (c < n_tables) {
              stream << table_name << ".get_tabulated_count()," << endl;
            } else {
              stream << table_name << ".get_tabulated_count()};"
              << endl << endl;
            }
          }
        }
      }
      stream << indent() << "// create link map" << endl;
      stream << indent() << "for (int i = 0; i < n_tables; i++) {" << endl;
      inc_indent();
      stream << indent() << "auto &curr_table = *(tables[i]);" << endl;
      stream << indent() << "max_table_size = "
             << "max(max_table_size, curr_table.size());" << endl;
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
        stream << indent() << "link_map[l[k].S" << i+1 << "] = b;"
               << endl << endl;
        stream << indent() << "unsigned char c = 0;" << endl;
        stream << indent() << "// store table/vector idx of every linked Block"
               << endl;
        stream << indent() << "// so invalid links can be removed properly"
               << endl;
        stream << indent() << "while (c < b->pos) {" << endl;
        inc_indent();
        stream << indent() << "switch (b->array[c]) {" << endl;
        inc_indent();
        stream << indent() << "case String::Block::REP :" << endl;
        inc_indent();
        stream << indent() << "c += 6;" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "case String::Block::SEQ :" << endl;
        inc_indent();
        stream << indent() << "c += 3;" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "case String::Block::LINK :" << endl;
        stream << indent() << "{" << endl;
        inc_indent();
        stream << indent() << "c++;" << endl;
        stream << indent() << "uintptr_t linked_block = "
               << "reinterpret_cast<uintptr_t>(b->get_link(c));" << endl;
        stream << indent() << "if (block_linked_at.find(linked_block) !="
               << " block_linked_at.end()) {" << endl;
        inc_indent();
        stream << indent() << "block_linked_at[linked_block]."
               << "emplace_back(i, j);" << endl;
        dec_indent();
        stream << indent() << "} else {" << endl;
        inc_indent();
        stream << indent() << "block_linked_at[linked_block] = "
               << "std::vector<std::pair<int, size_t>>{" << endl
               << "                                 "
               << "std::pair<int, size_t>{i, j}};" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
        stream << indent() << "c += 8;" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
        stream << indent() << "default :" << endl;
        inc_indent();
        stream << indent() << "c++;" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
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
      if (!cyk) {
        stream << indent() << "if (!((*(tabulated[i]))[j])) {" << endl;
        inc_indent();
        stream << indent() << "assert(l.size() == 0);" << endl;
        stream << indent() << "continue;" << endl;
        dec_indent();
        stream << indent() << "}" << endl << endl;
      }
      stream << indent() << "// true if an unresolvable link was found in "
             << "one of the Strings of the current ListRef" << endl;
      stream << indent() << "bool cant_resolve = false;" << endl;
      stream << indent() << "std::vector<String::Block*> valid_links;" << endl;
      stream << indent() << "for (size_t k = 0; k < l.size() "
             << "&& !cant_resolve; k++) {" << endl;
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
        stream << indent() << "while (c < b->pos && !cant_resolve) {" << endl;
        inc_indent();
        stream << indent() << "switch (b->array[c]) {" << endl;
        inc_indent();
        stream << indent() << "case String::Block::REP :" << endl;
        inc_indent();
        stream << indent() << "c += 6;" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "case String::Block::SEQ :" << endl;
        inc_indent();
        stream << indent() << "c += 3;" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "case String::Block::LINK :" << endl;
        stream << indent() << "{" << endl;
        inc_indent();
        stream << indent() << "c++;" << endl;
        stream << indent() << "uintptr_t linked_block = "
               << "reinterpret_cast<uintptr_t>(b->get_link(c));" << endl;
        stream << indent() << "if (link_map.find(linked_block) == "
               << "link_map.end()) {" << endl;
        inc_indent();
        /*
          check if link can be mapped to an existing Block in one of the tables;
          if not, this link can't be resolved and the String object
          wrapping this Block as well as all the other String objects
          in the current ListRef have to be deleted so they are recalculated
          once the algorithm requests this ListRef;
          to ensure that every other String/ListRef can resolve its links,
          we have to make sure that the to-be-deleted Strings aren't
          linked anywhere else;
          if they are, we also have to delete the Strings where these
          Strings are linked;
          we have to keep doing this until all Strings that need to be deleted
          aren't linked to any other strings
        */
        stream << indent() << "cant_resolve = true;" << endl;
        stream << indent() << "break;" << endl;
        dec_indent();
        stream << indent() << "}" << endl;
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
        stream << indent() << "valid_links.push_back(next_block);" << endl;
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
      stream << indent() << "if (cant_resolve) {" << endl;
      inc_indent();
      stream << indent() << "// mark current ListRef for delete/overwrite"
             << endl;
      stream << indent() << "initial_broken_listrefs.emplace_back(i, j);"
             << endl;
      dec_indent();
      stream << indent() << "} else  {" << endl;
      inc_indent();
      stream << indent() << "for (String::Block *b : valid_links) b->inc_ref();"
             << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      if (!cyk) {
      /*
         there won't be any broken links in cyk mode due to the way
         it fills the tables and we only possibly dump at the end of a
         complete loop iteration;
         only recursively call from the initial elements of broken_listrefs
       */
      stream << indent() << "std::vector<bool> already_checked"
             << "(n_tables * max_table_size);" << endl;
      stream << indent() << "for (auto &ilr : initial_broken_listrefs) {"
             << endl;
      inc_indent();
      stream << indent() << "std::vector<std::pair<int, size_t>> "
             << "additional{ilr};" << endl;
      stream << indent() << "size_t idx = ilr.first * "
             << "max_table_size + ilr.second;" << endl;
      stream << indent() << "already_checked[idx] = true;" << endl;
      stream << indent() << "find_broken_listrefs(additional, tables, "
             << "block_linked_at, max_table_size, already_checked);" << endl;
      stream << indent() << "for (auto &lr : additional) {" << endl;
      inc_indent();
      stream << indent() << "broken_listrefs.emplace_back(lr);" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      stream << indent() << "// mark the broken ListRefs "
             << "as not tabulated" << endl;
      stream << indent() << "for (const auto& b : "
             << "broken_listrefs) {" << endl;
      inc_indent();
      stream << indent() << "int table_i = b.first;" << endl;
      stream << indent() << "size_t vec_i = b.second;" << endl;
      stream << indent() << "bool is_tabulated = "
             << "(*(tabulated[table_i]))[vec_i];" << endl;
      stream << indent() << "if (is_tabulated) {" << endl;
      inc_indent();
      stream << indent() << "(*(tabulated[table_i]))[vec_i] = false;" << endl;
      stream << indent() << "(*(tabulated_counts[table_i]))--;" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      }
      dec_indent();
      stream << indent() << "}" << endl << endl;
      dec_indent();
  }

  void find_broken_listrefs(Printer::Base &stream, const nt_tables &tables) {
     inc_indent();
     stream << indent() << "void find_broken_listrefs("
            << "std::vector<std::pair<int, size_t>> &broken_listrefs," << endl
            << indent() << "std::vector<"
            << tables.begin()->second->table_decl->datatype()
            << "> *tables[]," << endl
            << indent() << "std::unordered_map<uintptr_t, "
            << "std::vector<std::pair<int, size_t>>> &block_linked_at," << endl
            << indent() << "size_t max_table_size, "
            << "std::vector<bool> &already_checked) {"
            << endl;
      inc_indent();
      stream << indent() << "// recursively add all ListRefs that "
             << "need to invalidated" << endl;
      stream << indent() << "// because they contain links to Strings that "
             << "also need" << endl;
      stream << indent() << "// to be invalidated to the broken_listrefs vector"
             << endl << endl;
      stream << indent() << "if (broken_listrefs.empty()) return;" << endl;
      stream << indent() << "bool has_link = false;" << endl;
      stream << indent() << "std::pair<int, size_t> &b = "
             << "broken_listrefs.back();" << endl;
      stream << indent() << "int table_i = b.first;" << endl;
      stream << indent() << "size_t vec_i = b.second;" << endl;
      stream << indent() << "auto &listref = (*(tables[table_i]))[vec_i].ref();"
             << endl;
      stream << indent() << "for (size_t i = 0; i < "
             << "listref.size(); i++) {" << endl;
      inc_indent();
      stream << indent() << "if (listref[i].B1) {" << endl;
      inc_indent();
      stream << indent() << "assert(listref[i].S1 && listref[i].B1);" << endl;
      stream << indent() << "uintptr_t b_int = listref[i].S1;" << endl;
      stream << indent() << "String::Block *b = listref[i].B1;" << endl;
      stream << indent() << "b->inc_ref();" << endl;
      stream << indent() << "if (block_linked_at[b_int].size() > 0) {" << endl;
      inc_indent();
      stream << indent() << "// if a String that is linked to "
             << "somewhere is found;" << endl;
      stream << indent() << "// the Strings that link to this "
            << "String need to be deleted as well" << endl;
      stream << indent() << "has_link = true;" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      stream << indent() << "// recursion anchor" << endl;
      stream << indent() << "// (will only be executed if a String of the "
             << "current ListRef is linked to somewhere)" << endl;
      stream << indent() << "if (has_link) {" << endl;
      inc_indent();
      stream << indent() << "for (size_t i = 0; i < listref.size(); i++) {"
             << endl;
      inc_indent();
      stream << indent() << "if (listref[i].B1) {" << endl;
      inc_indent();
      stream << indent() << "std::vector<std::pair<int, size_t>> &links = "
             << "block_linked_at[listref[i].S1];" << endl;
      stream << indent() << "for (auto& nxt_listref : links) {" << endl;
      inc_indent();
      stream << indent() << "size_t idx = nxt_listref.first * "
             << "max_table_size + nxt_listref.second;" << endl;
      stream << indent() << "if (!(already_checked[idx])) {" << endl;
      inc_indent();
      stream << indent() << "already_checked[idx] = true;" << endl;
      stream << indent() << "broken_listrefs.push_back(nxt_listref);" << endl;
      stream << indent() << "find_broken_listrefs(broken_listrefs, "
             << "tables, block_linked_at, max_table_size, already_checked);"
             << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      dec_indent();
      stream << indent() << "}" << endl << endl;
      dec_indent();
  }

  void add_seq_to_subseqs(Printer::Base &stream, const nt_tables &tables) {
     inc_indent();
     stream << indent() << "void add_seq_to_subseqs() {" << endl;
     inc_indent();
     stream << indent() << "// add seq ptr to all Subsequence objects" << endl;
     stream << indent() << "// (seq wasn't serialized for efficiency "
            << "since it's the same for every Subsequence object)" << endl;
     size_t c = 0;
     size_t n_tables = tables.size();
     stream << indent() << "int n_tables = " << n_tables << ";" << endl;
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
     stream << indent() << "for (int i = 0; i < n_tables; i++) {" << endl;
     inc_indent();
     stream << indent() << "auto &curr_table = *(tables[i]);" << endl;
     stream << indent() << "for (size_t j = 0; "
            << "j < curr_table.size(); j++) {" << endl;
     inc_indent();
     if (list_ref) {
       stream << indent() << "auto &l = curr_table[j].ref();" << endl;
       stream << indent() << "for (size_t k = 0; k < l.size(); k++) {" << endl;
       inc_indent();
       for (size_t s = 0; s < subseq_type_accessors.size(); s++) {
         stream << indent() << "l[k].SUBSEQ" << s+1 << " = &t_0_seq;" << endl;
       }
       dec_indent();
       stream << indent() << "}" << endl;
       dec_indent();
     } else {
       for (size_t s = 0; s < subseq_type_accessors.size(); s++) {
         stream << indent() << "curr_table[j].SUBSEQ"
                << s+1 << " = &t_0_seq;" << endl;
       }
     }
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
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
     if (!cyk) {
       stream << indent() << "// lock the mutex so main thread can't "
            << "write during archiving" << endl;
       stream << indent() << "std::lock_guard<std::mutex> lock(m);" << endl;
       stream << indent() << "array_out << array << tabulated << "
              << "tabulated_vals_counter;" << endl;
     } else {
       stream << indent() << "array_out << array << tabulated_vals_counter;"
              << endl;
     }
     stream << indent() << "array_fout.close();" << endl;
     stream << indent() << "boost::filesystem::rename(tmp_out_table_path, "
            << "out_table_path);" << endl;
     stream << indent() << "std::cerr << \"Info: Archived \\\"\" << tname << "
            << "\"\\\" table into \" << out_table_path" << endl
            << indent() << "          << \". Table is \" "
            << "<< get_tabulated_vals_percentage() << \"% filled.\" "
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
     stream << indent() << "char curr_time_str["
            << "sizeof(\"yyyy-mm-dd, hh:mm:ss\")];" << endl;
     stream << indent() << "std::strftime(curr_time_str, sizeof(curr_time_str),"
            << " \"%F, %T\", std::localtime(&curr_time));" << endl;
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

  void archive_cyk_indices(Printer::Base &stream, size_t n_tracks,
                           bool outside) {
     inc_indent();
     stream << indent() << "void archive_cyk_indices() {" << endl;
     inc_indent();
     stream << indent() << "// save the cyk loop indidces to disk" << endl;
     stream << indent() << "try {" << endl;
     inc_indent();
     stream << indent() << "std::ofstream array_fout("
            << "tmp_out_cyk_path.c_str());" << endl;
     stream << indent() << "if (!(array_fout.good())) {" << endl;
     stream << indent() << "  throw std::ofstream::failure(\"\");" << endl;
     stream << indent() << "}" << endl;
     stream << indent() << "boost::archive::text_oarchive "
                            "array_out(array_fout);" << endl;
     for (size_t i = 0; i < n_tracks; i++) {
       // since a mutex is locked before every loop iteration that prevents
       // any archiving before it is unlocked again,
       // we can directly dump the indices without having to worry
       // about potentially incomplete iterations
       std::string suffix = "";
       std::string first_index = "i";
       for (int io = 0; io < 2; ++io) {  // iterate through inside and outside
         stream << indent() << "array_out << t_" << i << "_" << first_index
                << suffix << ";" << endl;
         stream << indent() << "array_out << "
                << "t_" << i << "_j" << suffix << ";" << endl;
         if (!outside) {
           break;
         } else {
           // TODO(sjanssen) why can't I use OUTSIDE_IDX_SUFFIX from cyk.hh
           // here?
           suffix = "_outside";
           first_index = "diag";
           if (io == 0) {
             stream << indent() << "array_out << "
                    << "t_" << i << "_i" << suffix << ";" << endl;
           }
         }
       }
     }
     stream << indent() << "#ifdef _OPENMP" << endl;
     inc_indent();
     std::string suffix = "";
     for (int io = 0; io < 2; ++io) {  // iterate through inside and outside
       stream << indent() << "array_out << outer_loop_1_idx"
              << suffix << ";" << endl;
       stream << indent() << "array_out << outer_loop_2_idx"
              << suffix << ";" << endl;
       stream << indent() << "array_out << inner_loop_2_idx"
              << suffix << ";" << endl;
       if (!outside) {
         break;
       } else {
         suffix = "_outside";
       }
     }
     dec_indent();
     stream << indent() << "#endif" << endl;

     stream << indent() << "array_fout.close();" << endl;
     stream << indent() << "boost::filesystem::rename(tmp_out_cyk_path, "
            << "out_cyk_path);" << endl;
     stream << indent() << "std::cerr << \"Info: Archived cyk loop progress"
            << " into \" << out_cyk_path << \".\" "
            << "<< std::endl;" << endl;
     dec_indent();
     stream << indent() << "} catch (const std::ofstream::failure &e) {"
             << endl;
     stream << indent() << "  std::cerr << \"Couldn't create archive "
             << "at path \" << out_cyk_path << \".\\n\"" << endl;
     stream << indent() << "            << \"Please ensure that the directory "
             << "exists and that you have write permissions "
             << "for this directory.\\n\";" << endl;
     stream << indent() << "} catch (const std::exception &e) {" << endl;
     inc_indent();
     stream << indent() << "std::time_t curr_time = std::time(nullptr);"
            << endl;
     stream << indent() << "char curr_time_str["
            << "sizeof(\"yyyy-mm-dd, hh:mm:ss\")];" << endl;
     stream << indent() << "std::strftime(curr_time_str, sizeof(curr_time_str),"
            << " \"%F, %T\", std::localtime(&curr_time));" << endl;
     stream << indent() << "std::cerr << \"[\" << curr_time_str << \"] "
            << "Error trying to archive cyk loop progress.\\n\";" << endl;
     stream << indent() << "boost::filesystem::remove(tmp_out_cyk_path);"
            << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
  }

  void load_cyk_indices(Printer::Base &stream, size_t n_tracks, bool outside) {
     inc_indent();
     stream << indent() << "void load_cyk_indices() {" << endl;
     inc_indent();
     stream << indent() << "// read the cyk loop indices from disk " << endl;
     stream << indent() << "try {" << endl;
     inc_indent();
     stream << indent() << "std::ifstream array_fin(in_archive_path.c_str());"
            << endl;
     stream << indent() << "if (!(array_fin.good())) {" << endl;
     stream << indent() << "  throw std::ifstream::failure(\"\");" << endl;
     stream << indent() << "}" << endl;
     stream << indent() << "boost::archive::text_iarchive "
                            "array_in(array_fin);" << endl;
     for (size_t i = 0; i < n_tracks; i++) {
       std::string suffix = "";
       std::string first_index = "i";
       for (int io = 0; io < 2; ++io) {  // iterate through inside and outside
         stream << indent() << "array_in >> t_" << i << "_" << first_index
                << suffix << ";" << endl;
         stream << indent() << "array_in >> t_" << i << "_j" << suffix
                << ";" << endl;
         if (!outside) {
           break;
         } else {
           suffix = "_outside";
           first_index = "diag";
           if (io == 0) {
             stream << indent() << "array_in >> "
                    << "t_" << i << "_i" << suffix << ";" << endl;
           }
         }
       }
     }
     stream << indent() << "#ifdef _OPENMP" << endl;
     std::string suffix = "";
     for (int io = 0; io < 2; ++io) {  // iterate through inside and outside
       stream << indent() << "  array_in >> outer_loop_1_idx"
              << suffix << ";" << endl;
       stream << indent() << "  array_in >> outer_loop_2_idx"
              << suffix << ";" << endl;
       stream << indent() << "  array_in >> inner_loop_2_idx"
              << suffix << ";" << endl;
       if (!outside) {
          break;
        } else {
          suffix = "_outside";
        }
     }
     stream << indent() << "#endif" << endl;
     stream << indent() << "array_fin.close();" << endl << endl;
     stream << indent() << "std::cerr << \"Info: Successfully loaded "
            << "cyk loop indices. \"" << endl;
     stream << indent() << "          << \"Will continue calculating from here."
            << "\" << std::endl;" << endl;
     dec_indent();
     stream << indent() << "} catch (const std::ifstream::failure &e) {"
            << endl;
     inc_indent();
     stream << indent() << "std::cerr << \"Error: \\\"\" + "
            << "in_archive_path.string() + \"\\\" "
            << "archive\"" << endl;
     stream << indent() << "          << " << "\" could not be found.\\n\";"
            << endl;
     stream << indent() << "std::exit(1);" << endl;
     dec_indent();
     stream << indent() << "} catch (const std::exception &e) {" << endl;
     inc_indent();
     stream << indent() << "std::cerr << \"Error \\\"\" << e.what() << \"\\\" "
                            "trying to read cyk loop indices!\\n\";" << endl;
     stream << indent() << "std::exit(1);" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
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

  void get_tabulated(Printer::Base &stream) {
     inc_indent(); inc_indent();
     stream << indent();
     stream << "std::vector<bool> *get_tabulated() {" << endl;
     inc_indent();
     stream << indent() << "return &tabulated;" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent();
  }

  void get_tabulated_count(Printer::Base &stream) {
     inc_indent(); inc_indent();
     stream << indent();
     stream << "size_t *get_tabulated_count() {" << endl;
     inc_indent();
     stream << indent() << "return &tabulated_vals_counter;" << endl;
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
     stream << "std::string get_tabulated_vals_percentage() const {" << endl;
     inc_indent();
     stream << indent() << "char num_buffer[7];  // holds %.2f" << endl;
     stream << indent() << "std::snprintf(num_buffer, sizeof(num_buffer), "
            << "\"%.2f\", "
            << "static_cast<double>(tabulated_vals_counter) "
            << "/ array.size() * 100.0);" << endl;
     stream << indent() << "return std::string(num_buffer);" << endl;
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
     stream << indent() << "std::ifstream array_fin(in_archive_path.c_str(), "
            << "std::ios::binary);" << endl;
     stream << indent() << "if (!(array_fin.good())) {" << endl;
     stream << indent() << "  throw std::ifstream::failure(\"\");" << endl;
     stream << indent() << "}" << endl;
     stream << indent() << "boost::archive::binary_iarchive "
                            "array_in(array_fin);" << endl;
     if (!cyk) {
       stream << indent() << "array_in >> array >> tabulated >> "
              << "tabulated_vals_counter;" << endl;
     } else  {
       stream << indent() << "array_in >> array >> tabulated_vals_counter;"
              << endl;
     }
     stream << indent() << "array_fin.close();" << endl << endl;
     stream << indent() << "std::cerr << \"Info: Successfully loaded checkpoint"
            << " for \\\"\" << tname << \"\\\" table. \"" << endl;
     stream << indent() << "          << \"Will continue calculating from here."
            << "\" << std::endl;" << endl;
     dec_indent();
     stream << indent() << "} catch (const ParseException &e) {"
            << endl;
     inc_indent();
     stream << indent() << "std::cerr << e.what() << std::endl;" << endl;
     stream << indent() << "std::exit(1);" << endl;
     dec_indent();
     stream << indent() << "} catch (const std::ifstream::failure &e) {"
            << endl;
     inc_indent();
     stream << indent() << "std::cerr << \"Error: \\\"\" + tname + \"\\\" "
            << "archive\"" << endl;
     stream << indent() << "          << " << "\" could not be found in "
            << "Logfile or hasn't been archived yet.\\n\";" << endl;
     stream << indent() << "std::exit(1);" << endl;
     dec_indent();
     stream << indent() << "} catch (const std::exception &e) {" << endl;
     inc_indent();
     stream << indent() << "std::cerr << \"Error \\\"\" << e.what() << \"\\\" "
                            "trying to read \\\"\" << tname << "
                            "\"\\\" table!\\n\";" << endl;
     stream << indent() << "std::exit(1);" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "} else {" << endl;
     inc_indent();
     stream << indent() << "array.resize(newsize);" << endl;
     if (!cyk) {
       stream << indent() << "tabulated.clear();" << endl;
       stream << indent() << "tabulated.resize(newsize);" << endl;
     }
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent(); dec_indent(); dec_indent();
  }

  void archive_periodically(Printer::Base &stream, const nt_tables &tables) {
     inc_indent();
     stream << indent() << "void archive_periodically(std::atomic_bool "
                            "&cancel_token, size_t interval, "
            << "std::mutex &print_mutex" << endl;
     if (cyk) {
     stream << indent() << "                          "
            << "#ifdef _OPENMP" << endl
            << indent() << "                          "
            << ", fair_shared_mutex &mutex" << endl
            << indent() << "                          #else" << endl
            << indent() << "                          "
            << ", fair_mutex &mutex" << endl
            << indent() << "                          #endif" << endl;
     }
     stream << indent() << "                          ) {" << endl;
     inc_indent();
     stream << indent() << "// save all tables to the disk periodically "
                            "every interval seconds" << endl;
     stream << indent() << "cancel_token.store(true);" << endl;
     stream << indent()
              << "std::thread([this, interval, &cancel_token, &print_mutex";
     if (cyk) {
       stream << ", &mutex";
     }
     stream << "] {" << endl;
     stream << indent() << "            while (cancel_token.load()) {"
            << endl;
     stream << indent() << "              std::this_thread::sleep_for("
                            "std::chrono::seconds(interval));" << endl;
     stream << indent() << "              "
                            "if (!cancel_token.load()) break;" << endl;
     stream << indent() << "              "
            << "// need to aquire lock before printing to stderr in archive "
            << "methods to avoid potential interference during "
            << "simultaneous logging to stdout" << endl;
     stream << indent() << "              "
            << "std::lock_guard<std::mutex> print_lock(print_mutex);" << endl;
     if (cyk) {
       stream << indent() << "            #ifdef _OPENMP" << endl;
       stream << indent() << "              "
              << "std::lock_guard<fair_shared_mutex> lock(mutex);" << endl;
       stream << indent() << "            #else" << endl;
       stream << indent() << "              "
              << "std::lock_guard<fair_mutex> lock(mutex);" << endl;
       stream << indent() << "            #endif" << endl;
     }

     for (auto i = tables.begin(); i != tables.end(); ++i) {
       const std::string &table_name = i->second->table_decl->name();
       stream << "              " << indent();
       stream << table_name << ".archive(\"" << table_name << "\");" << endl;
     }
     if (cyk) {
       stream << "              " << indent();
       stream << "archive_cyk_indices();" << endl;
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
     if (cyk) {
       stream << indent() << "boost::filesystem::remove("
              << "out_cyk_path);" << endl;
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
            << "successful termination of the program" << endl;
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
     stream << indent() << "std::vector<std::string> ordered_args;" << endl;
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
     stream << indent() << "} else if (std::strcmp(argv[i], \"--keepArchives\")"
            << " == 0 ||" << endl;
     stream << indent() << "           std::strcmp(argv[i], \"-K\") == 0) {"
            << endl;
     inc_indent();
     stream << indent() << "++i;" << endl;
     dec_indent();
     stream << indent() << "} else {" << endl;
     inc_indent();
     stream << indent() << "std::stringstream arg_pair;" << endl;
     stream << indent() << "arg_pair << argv[i++] << \" \";" << endl;
     stream << indent() << "if (i < argc) arg_pair << argv[i++];" << endl;
     stream << indent() << "ordered_args.push_back(arg_pair.str());" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     stream << indent() << "std::sort(ordered_args.begin(), "
            << "ordered_args.end());" << endl;
     stream << indent() << "for (std::string &el : ordered_args) {" << endl;
     inc_indent();
     stream << indent() << "arg_string << el << \" \";" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     stream << indent() << "return arg_string.str();" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
  }

  void create_checkpoint_log(Printer::Base &stream, const nt_tables &tables) {
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
     for (auto i = tables.begin(); i != tables.end(); ++i) {
       const std::string &table_name = i->second->table_decl->name();
       stream << indent() << "fout << \"# [TABLE_NAME] path/to/"
              << table_name << "\\n\";" << endl;
     }
     if (cyk) {
       stream << indent() << "fout << \"# [CYK_INDICES] "
              << "path/to/cyk_indices\\n\";" << endl;
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
       stream << indent() << "fout << \"[" << table_name << "] \" << "
              << table_name << ".get_out_table_path() "
              << "<< \"\\n\";" << endl;
     }
     if (cyk) {
       stream << indent() << "fout << \"[CYK_INDICES] \""
              << "<< out_cyk_path.string() << \"\\n\";" << endl;
     }
     stream << indent() << "fout << \"[GAPC CALL] \" << GAPC_CALL_STRING "
            << "<< \"\\n\";" << endl;
     stream << indent() << "fout << \"[GAPC VERSION] \" << GAPC_VERSION_STRING "
            << "<< \"\\n\";" << endl;
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

  void parse_checkpoint_log(Printer::Base &stream, bool cyk) {
     inc_indent();
     if (!cyk) inc_indent();
     stream << indent() << "void parse_checkpoint_log("
            << "const std::string &archive_name, const std::string &arg_string,"
            << endl
            << indent() << "                          "
            << "const boost::filesystem::path &path) {"
            << endl;
     inc_indent();
     stream << indent() << "// parse the checkpoint log and look "
            << "for checkpoints" << endl;
     stream << indent() << "// that were created with identical program "
            << "input (if that info is available)" << endl;
     stream << indent() << "std::ifstream fin(path.c_str());" << endl;
     stream << indent() << "if (!(fin.good())) return;"
            << endl << endl;
     stream << indent() << "std::string line, curr_archive_name;" << endl;
     stream << indent() << "std::string options_line_start = \"[OPTIONS] \";"
            << endl;
     stream << indent() << "std::string archive_path_start = "
            << "\"[\" + archive_name + \"] \";" << endl;
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
            << "for \\\"\" + archive_name + \"\\\" "
            << "was created with different \"" << endl
            << indent() << "                     \"command line inputs than"
            << " this program was executed with.\\n\");" << endl;
     stream << indent() << "return;" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     stream << indent() << "i = line.find(archive_path_start);" << endl;
     stream << indent() << "if (i != line.npos) {" << endl;
     inc_indent();
     stream << indent() << "curr_archive_name = "
            << "line.substr(1, line.find(\"]\") - 1);" << endl;
     stream << indent() << "if (curr_archive_name == archive_name) {"
            << endl;
     inc_indent();
     stream << indent() << "line.replace(i, archive_path_start.length(), "
            << "\"\");" << endl;
     stream << indent() << "in_archive_path = boost::filesystem::path(line);"
            << endl;
     stream << indent() << "return;" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl;
     dec_indent();
     stream << indent() << "}" << endl << endl;
     dec_indent();
     if (!cyk) dec_indent();
  }
};
}  // namespace Printer

#endif  // SRC_CHECKPOINT_HH_
