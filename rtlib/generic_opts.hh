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

#ifndef GENERIC_OPTS_HH
#define GENERIC_OPTS_HH


#include <iostream>
#include <fstream>
#include <sstream>
#include <cstring>
#include <vector>

#include <exception>

#include <cassert>

//define _XOPEN_SOURCE=500

#include <unistd.h>
#include <cstdlib>

namespace gapc {

class OptException : public std::exception {
  private:
    std::string msg;
  public:
    OptException(const std::string &s) : std::exception(), msg(s)
    {
    }
    ~OptException() throw() { }
    const char* what() const throw()
    {
      return msg.c_str();
    }
};

class Opts {
  private:
    Opts(const Opts&);
    Opts &operator=(const Opts&);
  public:
    typedef std::vector<std::pair<const char*, unsigned> > inputs_t;
    inputs_t inputs;
    bool window_mode;
    unsigned int window_size;
    unsigned int window_increment;

    unsigned int delta;
    unsigned int repeats;
    unsigned k;

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
      k(3)
    {
    }

    ~Opts()
    {
      for (inputs_t::iterator i = inputs.begin(); i != inputs.end(); ++i)
        delete[] (*i).first;
    }

    void help(char **argv)
    {
      std::cout << argv[0] << " ("
#ifdef WINDOW_MODE
        << " (-[wi] [0-9]+)*"
#endif
#ifdef RNALIB_H
        << " (-[tT] [0-9]+)? (-P PARAM-file)?"
#endif
        << " (-[drk] [0-9]+)* (-h)? (INPUT|-f INPUT-file)\n";
    }

    void parse(int argc, char **argv)
    {
      int o = 0;
      char *input = 0;
#ifdef RNALIB_H
      char *par_filename = 0;
#endif
      while ((o = getopt(argc, argv, ":f:"
#ifdef WINDOW_MODE
              "w:i:"
#endif
#ifdef RNALIB_H
              "t:T:P:"
#endif
              "hd:r:k:")) != -1) {
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
#ifdef RNALIB_H
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
          std::strcpy(input, argv[optind]);
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
#ifdef RNALIB_H
      librna_read_param_file(par_filename);
#endif
    }

};

}

#endif
