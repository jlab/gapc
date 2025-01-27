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

#include "options.hh"
#include <string>


std::string basename(const std::string &f) {
  size_t r = f.find_last_of('.');
  if (r != std::string::npos) {
    return f.substr(0, r);
  } else {
    return f;
  }
}


std::string classname(const std::string &f) {
  size_t a = f.find_first_of('/');
  if (a == std::string::npos) {
    a = 0;
  } else {
    a++;
  }
  size_t b = f.find_last_of('.');
  if (b == std::string::npos) {
    b = f.size();
  }
  return f.substr(a, b-a);
}


std::string remove_dir(const std::string &f) {
  size_t a = f.find_last_of('/');
  if (a == std::string::npos) {
    a = 0;
  } else {
    a++;
  }
  size_t b = f.size();
  return f.substr(a, b-a);
}


#include "log.hh"


bool Options::check() {
  if (sample && subopt)
    Log::instance()->error("Can't combine --sample and --subopt");
  if (backtrack && subopt)
    Log::instance()->error("Can't combine --backtrace and --subopt");
  if (kbacktrack && subopt)
    Log::instance()->error("Can't combine --kbacktrace and --subopt");

  if (!kbacktrack && no_coopt)
    Log::instance()->error("--no-coopt is only supported with --kbacktrace");

  if (classified && (backtrack || subopt ) )
    Log::instance()->error(
      "Can't combine --subopt-classify with --backtrace/subopt options");

  if (!instance.empty() && !product.empty())
    Log::instance()->error("Can't combine --instance with --product");

  if (window_mode && cyk)
    Log::instance()->error(
      "Currently --window-mode is just possible without --cyk.");

  if (classified && kbest)
    Log::instance()->error("Use either --subopt-classify or --kbest");

  if (logLevel < 0 || logLevel > 4)
    Log::instance()->error("Log-level must be in the range of 0 to 4.");

  if (pareto < 0 || pareto > 4)
    Log::instance()->error("Pareto version must be in the range of 0 to 4.");

  if (cutoff < 10 )
    Log::instance()->error("Cut-off must be bigger than 10.");

  if (ambiguityCheck && specializeGrammar)
    Log::instance()->error(
      "options '--ambiguity' and '--specialize_grammar' do not work together.");

  if (specialization < 0 || specialization > 2)
    Log::instance()->error(
      "ADP specialization must be in the range of 0 to 2.");

  if (step_option < 0 || step_option > 1)
    Log::instance()->error("Step Mode must be either 0 or 1.");

  if (specialization == 2 &&  step_option == 1)
    Log::instance()->error("Step mode is not supported for sorted ADP.");

  if (float_acc < 0) {
    Log::instance()->error(
      "Floating point accuracy must be greater then 0 digits");
  }

  return !Log::instance()->seen_errors();
}
