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


#include "log.hh"

#include <algorithm>
#include <iostream>
#include <string>

Log::Log() : seen_errs(false), debug_mode(false), error_count(0),
              logLevel(NORMAL), out(&std::cerr) {
  self = this;
}


Log::~Log() {
}


void Log::error(const std::string &m) {
  writeLogMessage(NULL, m, ERROR);
}


void Log::error(const Loc& l, const std::string& m) {
  writeLogMessage(&l, m, ERROR);
}


void Log::error_continue(const Loc& l, const std::string& m) {
  // HACK: DEBUG as a log level results in printing no
  // header which is the same as msg_continue(...)
  *out << std::endl;
  writeLogMessageContinued(&l, m);
}


void Log::warning(const Loc& l, const std::string& m) {
  writeLogMessage(&l, m, WARN);
}


void Log::warning(const std::string& m) {
  writeLogMessage(NULL, m, WARN);
}


void Log::normalMessage(const Loc& l, const std::string& m) {
  writeLogMessage(&l, m, NORMAL);
}


void Log::normalMessage(const std::string& m) {
  writeLogMessage(NULL, m, NORMAL);
}


void Log::verboseMessage(const Loc& l, const std::string& m) {
  writeLogMessage(&l, m, VERBOSE);
}


void Log::verboseMessage(const std::string& m) {
  writeLogMessage(NULL, m, VERBOSE);
}


void Log::debugMessage(const Loc& l, const std::string& m) {
  writeLogMessage(&l, m, DEBUG);
}


void Log::debugMessage(const std::string& m) {
  writeLogMessage(NULL, m, DEBUG);
}


void Log::inc_error() {
  seen_errs = true;
  error_count++;
  if (error_count == 20) {
    throw LogThreshException();
  }
}


void Log::writeLogMessage(
  const Loc* l, const std::string &m, LogLevel messageLogLevel) {
  // Filter the message according to its log level. The log level
  // of the message must be greater or equal than the log level
  // of the logger, otherwise we just return.
  if (messageLogLevel < logLevel) {
    return;
  }
  switch (messageLogLevel) {
    case VERBOSE:
    case NORMAL: {
      *out << "Info: " << std::endl;
      break;
    }
    case WARN: {
      *out << "Warning: " << std::endl;
      break;
    }
    case ERROR: {
      inc_error();
      *out << "Error: " << std::endl;
      break;
    }
    case DEBUG: {
      break;
    }
    default: {
      // print nothing here, this case is used as a continued
      // log message output.
    }
  }
  // the lead in of the logmessage is written, now just write out
  // the plain message.
  writeLogMessageContinued(l, m);
}


void Log::writeLogMessageContinued(const Loc* l, const std::string &m) {
  // Depending on the presence of a location for the message
  // the message is simply printed out, or a source code location
  // mark is created.
  if (l == NULL) {
    *out << m << std::endl;
  } else {
    if (!l->begin.filename) {
      *out << "<builtin>: " << m << std::endl;
      return;
    }
    build_message(*out, *l, m);
  }
}


void Log::build_message(
  std::ostream &out, const Loc &l, const std::string &m) {
  if (!l.begin.filename) {
    out << m << std::endl;
    return;
  }
  std::string line;
  if (*l.begin.filename == "_PRODUCT_") {
    line = product_;
  } else {
    line = l.line();
  }

  out << line << std::endl;
  std::string mark;
  build_mark(l, &mark);
  out << mark << std::endl;
  out << l << ": " << m << std::endl;
}


void Log::build_mark(const Loc& l, std::string *s) {
  // vanilla bison 2.3 counts columns from 0
  // with sed hack in makefiles/lexyaccxx.mf from 1
  size_t off = l.begin.column;
  if (off) {
    off -= 1;  // bison 2.4 counts columns from 1
  }
  std::string t(off, ' ');
  size_t length = 0;
  if (l.end.line == l.begin.line) {
    length = l.end.column - l.begin.column;
  } else {
    length = 3;
  }
  length = std::min(length, size_t(160));
  if (length > 1) {
    std::string fill(length, '-');
    fill[0] = '^';
    fill[length-1] = '^';
    *s = t + fill;
    return;
  }
  std::string fill("^");
  *s = t + fill;
}


void Log::set_debug(bool b) {
  debug_mode = b;
  if (b == true) {
    this->logLevel = DEBUG;
  } else {
    this->logLevel = NORMAL;
  }
}



Log *Log::self;


#include <sstream>


LogError::LogError(const Loc &l, std::string m) {
  std::ostringstream o;
  o << "Error: \n";
  Log t;
  t.build_message(o, l, m);
  msg = o.str();
}
