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

#ifndef RTLIB_GENERIC_OPTS_HH_
#define RTLIB_GENERIC_OPTS_HH_

extern "C" {
  #include <getopt.h>
  #include <unistd.h>
  #include <ctype.h>
  #include <stdio.h>
}

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstring>
#include <vector>
#include <string>
#include <exception>
#include <utility>
#include <cassert>

#ifdef CHECKPOINTING_INTEGRATED
#include "boost/filesystem.hpp"
#endif

// define _XOPEN_SOURCE=500

namespace gapc {

class OptException : public std::exception {
 private:
    std::string msg;

 public:
    explicit OptException(const std::string &s) : std::exception(), msg(s) {
    }
    ~OptException() throw() { }
    const char* what() const throw() {
      return msg.c_str();
    }
};

class Opts {
 private:
    Opts(const Opts&);
    Opts &operator=(const Opts&);

    int parse_checkpointing_interval(const std::string &interval) {
      // parse the user-specified checkpointing interval
      std::stringstream tmp_interval(interval);
      std::string val;
      std::vector<int> interval_vals;

      try {
        // parse the checkpointing interval the user specified
        while (std::getline(tmp_interval, val, ':')) {
          // split the interval string at ':' and store values
          interval_vals.push_back(std::stoi(val));
        }

        if (interval_vals.size() != 4) {
          throw std::exception();
        }
      } catch (const std::exception &e) {
        throw OptException("Invalid interval format! "
                           "Must be d:h:m:s (e.g. 0:0:1:0).");
      }

      // calculate the interval length (in seconds)
      int cp_interval = interval_vals[0] * 86400 + interval_vals[1] * 3600 +
                        interval_vals[2] * 60 + interval_vals[3];

      if (cp_interval <= 0) {
        throw OptException("Interval cannot be <= 0 (is " +
                           std::to_string(cp_interval) + ").");
      }
      return cp_interval;
    }

 public:
    typedef std::vector<std::pair<const char*, unsigned> > inputs_t;
    inputs_t inputs;
    bool window_mode;
    unsigned int window_size;
    unsigned int window_increment;

    unsigned int delta;
    unsigned int repeats;
    unsigned k;

#ifdef CHECKPOINTING_INTEGRATED
    size_t checkpoint_interval;  // default interval: 3600s (1h)
    boost::filesystem::path  checkpoint_out_path;  // default path: cwd
    boost::filesystem::path  checkpoint_in_path;  // default: empty
    std::string user_file_prefix;
    bool keep_archives;  // default: delete after calculations completed
#endif
    unsigned int tile_size;
    int argc;
    char **argv;

    Opts()
      :
#ifdef WINDOW_MODE
      window_mode(true),
#else
      window_mode(false),
#endif
      window_size(0),
      window_increment(0),
      delta(0),
      repeats(1),
      k(3),
#ifdef CHECKPOINTING_INTEGRATED
      checkpoint_interval(DEFAULT_CHECKPOINT_INTERVAL),
      checkpoint_out_path(boost::filesystem::current_path()),
      checkpoint_in_path(boost::filesystem::path("")),
      user_file_prefix(""),
      keep_archives(false),
#endif
      tile_size(32),
      argc(0),
      argv(0) {}

    ~Opts() {
      for (inputs_t::iterator i = inputs.begin(); i != inputs.end(); ++i)
        delete[] (*i).first;
    }

    void help(char **argv) {
      std::cout << argv[0] << " ("
#ifdef WINDOW_MODE
        << " (-[wi] [0-9]+)*"
#endif
#ifdef LIBRNA_RNALIB_H_
        << " (-[tT] [0-9]+)? (-P PARAM-file)?"
#endif
        << " (-[drk] [0-9]+)* (-h)? (INPUT|-f INPUT-file)\n"
        << "--help   ,-h                          print this help message\n"
#ifdef CHECKPOINTING_INTEGRATED
        << "--checkpointInterval,-p  d:h:m:s      specify the periodic "
        << "checkpointing\n"
        << "                                      interval,default: 0:1:0:0 "
        << "(1h)\n"
        << "--checkpointOutput,-O    PATH/PREFIX  set path where to store "
        << "the checkpoints,\n"
        << "                                      default: current working "
        << "directory\n"
        << "                                      Optional: add custom prefix "
        << "for generated\n"
        << "                                      files to PATH (e.g. PATH:\n"
        << "                                      \"/path/to/dir/file_prefix\""
        << "\n"
        << "                                      will set PATH to \"/path/to/"
        << "dir/\"\n"
        << "                                      and PREFIX to \"file_prefix\""
        << ").\n"
        << "                                      Make sure to add a \"/\" to\n"
        << "                                      the end of PATH if you don't"
        << "\n"
        << "                                      wish to add a custom prefix "
        << "to the files.\n"
        << "--checkpointInput,-I     LOGFILE      set the path to the Logfile\n"
        << "                                      of the checkpoints you wish "
        << "to load.\n"
        << "                                      (This file was generated "
        << "along\n"
        << "                                      with the checkpoint archives."
        << "\n"
        << "                                      If it isn't available\n"
        << "                                      add the path to each archive "
        << "to a \n"
        << "                                      text file and provide the \n"
        << "                                      path to this file).\n"
        << "--keepArchives,-K                     don't delete checkpointing "
        << "archives\n"
        << "                                      after the program finished "
        << "its calculations\n"
#endif
#ifdef _OPENMP
        << "--tileSize,-L            N            set tile size in "
        << "multithreaded cyk \n"
        << "                                      loops (default: 32)\n"
        << "\n"
#endif
#if defined(GAPC_CALL_STRING) && defined(GAPC_VERSION_STRING)
        << "GAPC call:        \"" << GAPC_CALL_STRING << "\"\n"
        << "GAPC version:     \"" << GAPC_VERSION_STRING << "\"\n"
#endif
        << "\n";
    }

    void parse(int argc, char **argv) {
      int o = 0;
      char *input = 0;
      const option long_opts[] = {
            {"help", no_argument, nullptr, 'h'},
            {"checkpointInterval", required_argument, nullptr, 'p'},
            {"checkpointOutput", required_argument, nullptr, 'O'},
            {"checkpointInput", required_argument, nullptr, 'I'},
            {"keepArchives", no_argument, nullptr, 'K'},
            {"tileSize", required_argument, nullptr, 'L'},
            {nullptr, no_argument, nullptr, 0}};
      this->argc = argc;
      this->argv = argv;

#ifdef LIBRNA_RNALIB_H_
      char *par_filename = 0;
#endif
      while ((o = getopt_long(argc, argv, ":f:"
#ifdef WINDOW_MODE
              "w:i:"
#endif
#ifdef LIBRNA_RNALIB_H_
              "t:T:P:"
#endif
#ifdef CHECKPOINTING_INTEGRATED
              "p:I:KO:"
#endif
#ifdef _OPENMP
             "L:"
#endif
             "hd:r:k:H:", long_opts, nullptr)) != -1) {
        switch (o) {
          case 'f' :
            {
            std::ifstream file(optarg);
            file.exceptions(std::ios_base::badbit |
                std::ios_base::failbit |
                std::ios_base::eofbit);
            std::filebuf *buffer = file.rdbuf();
            size_t size = buffer->pubseekoff(0, std::ios::end, std::ios::in);
            buffer->pubseekpos(0, std::ios::in);
            input = new char[size+1];
            assert(input);
            buffer->sgetn(input, size);
            input[size] = 0;

            char *end = input+size;
            for (char *i = input; i != end; ) {
              char *s = std::strchr(i, '\n');
              if (s)
                *s = 0;
              size_t x = std::strlen(i)+1;
              char *j = new char[x];
              std::strncpy(j, i, x);
              inputs.push_back(std::make_pair(j, x-1));
              if (s)
                i = s + 1;
              else
                break;
            }
            delete[] input;
            }
            break;
          case 'w' :
            window_size = std::atoi(optarg);
            break;
          case 'i' :
            window_increment = std::atoi(optarg);
            break;
#ifdef LIBRNA_RNALIB_H_
          case 'T' :
          case 't' :
            temperature = std::atof(optarg);
            break;
          case 'P' :
            par_filename = optarg;
            break;
#endif
          case 'k' :
            k = std::atoi(optarg);
            break;
          case 'h' :
            help(argv);
            std::exit(0);
            break;
          case 'd' :
            delta = std::atoi(optarg);
            break;
          case 'r' :
            repeats = std::atoi(optarg);
            break;
#ifdef CHECKPOINTING_INTEGRATED
          case 'p' :
            checkpoint_interval = parse_checkpointing_interval(optarg);
            break;
          case 'I' :
          {
            boost::filesystem::path arg_path(optarg);
            if (arg_path.is_absolute()) {
              checkpoint_in_path = arg_path;
            } else {
              checkpoint_in_path = boost::filesystem::current_path() / arg_path;
            }
            if (!boost::filesystem::exists(checkpoint_in_path) ||
                !boost::filesystem::is_regular_file(checkpoint_in_path)) {
              throw OptException("Logfile could not be found at path \"" +
                                 checkpoint_in_path.string() +
                                 "\"!");
            }

            // check if current user has read permissions
            // for checkpoint input directory
            if (access(checkpoint_in_path.c_str(), R_OK) != 0) {
              throw OptException("Missing read permissions for"
                                 " Logfile \""
                                 + checkpoint_in_path.string()
                                 + "\"!");
            }
            break;
          }
          case 'O' :
          {
              boost::filesystem::path arg_path(optarg);
              boost::filesystem::path out_path = arg_path.parent_path();

              if (out_path.is_absolute()) {
                checkpoint_out_path = out_path;
              } else {
                checkpoint_out_path /= out_path;
              }

            user_file_prefix = arg_path.filename().string();

            if (!boost::filesystem::exists(checkpoint_out_path) ||
                !boost::filesystem::is_directory(checkpoint_out_path)) {
              throw OptException("The output path \"" +
                                 checkpoint_out_path.string() +
                                 "\" is not a directory!");
            }

            // check if current user has write permissions
            // for checkpoint output directory
            if (access(checkpoint_out_path.c_str(), W_OK) != 0) {
              throw OptException("Missing write permissions for"
                                 " output path \""
                                 + checkpoint_out_path.string()
                                 + "\"!");
            }
            break;
          }
          case 'K' :
            keep_archives = true;
            break;
#endif
#ifdef _OPENMP
          case 'L' :
            tile_size = std::atoi(optarg);
            break;
#endif
          case '?' :
          case ':' :
            {
              std::ostringstream os;
              os << "Missing argument of " << char(optopt);
              throw OptException(os.str());
            }
          default:
            {
              std::ostringstream os;
              os << "Unknown Option: " << char(o);
              throw OptException(os.str());
            }
        }
      }
      if (!input) {
        if (optind == argc)
          throw OptException("Missing input sequence or no -f.");
        for (; optind < argc; ++optind) {
          input = new char[std::strlen(argv[optind])+1];
          snprintf(input, std::strlen(argv[optind])+1, "%s", argv[optind]);
          unsigned n = std::strlen(input);
          inputs.push_back(std::make_pair(input, n));
        }
      }
      if (window_mode) {
        if (!window_size)
          throw OptException("window size (-w) is zero");
        if (!window_increment)
          throw OptException("window increment (-i) is zero");
        if (window_increment >= window_size )
          throw OptException("window_increment >= window_size");
      }
#ifdef LIBRNA_RNALIB_H_
      librna_read_param_file(par_filename);
#endif
    }
};

}  // namespace gapc

#endif  // RTLIB_GENERIC_OPTS_HH_
