#ifndef CHECKPOINT_HH
#define CHECKPOINT_HH

#include "printer.hh"
#include "symbol.hh"
#include "statement/table_decl.hh"

class Checkpoint {
  public:
    size_t interval = 0;

    Checkpoint (size_t interval): interval(interval) {}

    void include(Printer::Base &stream) {
      stream << "#include \"boost/serialization/vector.hpp\"" << endl;
      stream << "#include \"boost/archive/binary_iarchive.hpp\"" << endl;
      stream << "#include \"boost/archive/binary_oarchive.hpp\"" << endl;
      stream << "#include <atomic>" << endl;
      stream << "#include <thread>" << endl << endl;
    }

    void archive(Printer::Base &stream) {
    stream << "    void archive(const std::string &output_file) {" << endl;
    stream << "      // save the DP table/array to disk" << endl;
    stream << "      try {" << endl;
    stream << "        std::ofstream array_fout(output_file, std::ios::binary);" << endl;
    stream << "        boost::archive::binary_oarchive array_out(array_fout);" << endl;
    stream << "        array_out << array;" << endl;
    stream << "        array_fout.close();" << endl;
    stream << "      } catch (std::exception &e) {" << endl;
    stream << "        std::cerr << \"Error during archiving of \\\"\" << output_file << \"\\\" table!\"" << endl;
    stream << "                  << \" Will retry at the next checkpoint...\\n\";" << endl;
    stream << "      }" << endl;
    stream << "  }" << endl << endl;
    }

    void init(Printer::Base &stream) {
      stream <<"      // read the DP array/table from disk and put its contents into array" << endl;
      stream <<"      try {" << endl;
      stream <<"        std::ifstream array_fin(tname, std::ios::binary);" << endl;
      stream <<"        boost::archive::binary_iarchive array_in(array_fin);" << endl;
      stream <<"        array_in >> array;" << endl;
      stream <<"        array_fin.close();" << endl;
      stream <<"        // mark the already existing table values in the tabulated vector" << endl;
      stream <<"        for (long unsigned int i = 0; i < array.size(); i++) tabulated[i] = array[i];" << endl;
      stream <<"      } catch (const std::exception &e) {" << endl;
      stream <<"        std::cerr << \"Error \\\"\" << e.what() << \"\\\" while reading \\\"\" << tname << \"\\\" table!\"" << endl;
      stream <<"                  << \" This table will initialized empty!\\n\";" << endl;
      stream <<"        tabulated.clear();" << endl;
      stream <<"        array.clear();" << endl;
      stream <<"        array.resize(newsize);" << endl;
      stream <<"      }" << endl << endl;
    }
    
    typedef hashtable<std::string, Symbol::NT*> nt_tables;
    void archive_periodically(Printer::Base &stream, const nt_tables &tables) {
      stream << "  void archive_periodically(std::atomic_bool &cancel_token, size_t interval) {" << endl;
      stream << "    // save all tables to the disk periodically every interval seconds" << endl;
      stream << "    cancel_token.store(true);" << endl;
      stream << "    std::thread([=, &cancel_token]() mutable {" << endl;
      stream << "                while (cancel_token.load()) {" << endl;
      stream << "                  std::this_thread::sleep_for(std::chrono::seconds(interval));" << endl;
      stream << "                  if (!cancel_token.load()) break;" << endl;

      for (auto i = tables.begin(); i != tables.end(); ++i) {
          std::string table_name = i->second->table_decl->name();
          stream << "                  " << table_name << ".archive(\"" << table_name << "\");" << endl;
      }

      stream << "                }" << endl;
      stream << "              }).detach();" << endl;
      stream << "    }" << endl << endl;
    }
};

#endif  // CHECKPOINT_HH