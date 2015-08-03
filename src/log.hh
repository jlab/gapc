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


#ifndef LOG_HH 
#define LOG_HH

#include <iostream>
#include <exception>

#include "loc.hh"


class LogThreshException : public std::exception {
	
	private:
	
		std::string msg;
		
		
	public:
	
		LogThreshException() : std::exception(), msg("Log message threshold reached.")
		{
		}
		
		~LogThreshException() throw() { }
		
		const char* what() const throw()
		{
			return msg.c_str();
		}
		
		
};


class LogError : public std::exception {
	
	private:
		
		std::string msg;
		
		
	public:
		
		LogError(std::string m)
			: std::exception() {
			msg = "Error: " + m;
		}
		
		LogError(const Loc &l, std::string m);
		
		~LogError() throw() { }
		
		const char* what() const throw() {
			return msg.c_str();
		}
		
		
};


class Log {
	
	public:
		
		// Defines the level of logging. Their meaning is
		// DEBUG:	all messages are shown, including debug
		//			messages
		// VERBOSE:	verbose mode, shows many messages that are
		//			normally hidden, but sometimes useful for the
		//			end user
		// NORMAL:	the normal level of logging
		// WARN:	only warnings and errors are shown
		// ERROR:	only errors are displayed.
		enum LogLevel {DEBUG, VERBOSE, NORMAL, WARN, ERROR};
		
		
	private:
		
		static Log *self;
		bool seen_errs;
		bool debug_mode;
		unsigned int error_count;
		LogLevel logLevel;
		
		std::string product_;
		
		void inc_error();
		
		std::ostream *out;
		
		template <typename T> friend std::ostream &operator<<(Log &l, const T &t);
		
		
	protected:
		
		virtual void writeLogMessage (const Loc* l, const std::string &m, LogLevel messageLogLevel);
		virtual void writeLogMessageContinued (const Loc* l, const std::string &m);
		
		void build_mark(const Loc &l, std::string &s);
		
		
	public:
	
		static Log* instance() { return self; }
		static Log &o() { return *self; }
		
		// Inits a new instance of this logger. The logger is used throughout
		// the gapc compiler as a static instance, accessed by the method
		// 'instance()'. The first instance is created in gapc.cc as a member
		// variable of the class 'main'. Since fields are initialized before
		// any code is executed, it is safe to assume that there will always
		// be a static instance of the logger available.
		Log ();
		virtual ~Log();
		virtual void error(const std::string &m);
		virtual void error(const Loc & l, const std::string& m);
		virtual void error_continue(const Loc & l, const std::string& m);
		virtual void warning(const Loc & l, const std::string& m);
		virtual void warning(const std::string& m);
		virtual void normalMessage (const Loc & l, const std::string& m);
		virtual void normalMessage (const std::string& m);
		virtual void verboseMessage (const Loc & l, const std::string& m);
		virtual void verboseMessage (const std::string& m);
		virtual void debugMessage (const Loc & l, const std::string& m);
		virtual void debugMessage (const std::string& m);
		
		// This method is public only because LogError needs access to it.
		void build_message (std::ostream &out, const Loc &l, const std::string &m);
		
		bool seen_errors() { return seen_errs; }
		
		// sets the log level of this logger component
		void setLogLevel (LogLevel level) { logLevel = level; }
		
		bool is_debug() { return debug_mode; }
		//void set_debug() { debug_mode = true; }
		void set_debug (bool b = true);
		
		void set_product(const std::string &p) { product_ = p; }
		
		void set_ostream(std::ostream &o) { out = &o; }
	
	
};


template <typename T> std::ostream &operator<<(Log &l, const T &t)
{
	*l.out << t;
	return *l.out;
}

#endif
