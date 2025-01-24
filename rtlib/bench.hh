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

#ifndef RTLIB_BENCH_HH_
#define RTLIB_BENCH_HH_

#ifdef STATS

#include <string>
#include <list>
#include <utility>

#include <boost/date_time/posix_time/posix_time.hpp>

typedef boost::posix_time::ptime datum_t;

class Bench {
 private:
    std::list<std::pair<datum_t, std::string> > list;

 public:
    void add_event(const std::string &desc) {
      boost::posix_time::ptime a
        = boost::posix_time::microsec_clock::universal_time();
      list.push_back(std::make_pair(a, desc));
    }

    void put(std::ostream &o) const {
      o << "\n\nTimings:\n\n";
      std::list<std::pair<datum_t, std::string> >::const_iterator i =
        list.begin();
      assert(i != list.end());
      boost::posix_time::time_duration t =
        list.back().first - list.front().first;
      std::list<std::pair<datum_t, std::string> >::const_iterator j = i;
      ++j;
      for (;
           j != list.end(); ++j) {
        boost::posix_time::ptime a = (*i).first;
        boost::posix_time::ptime b = (*j).first;
        boost::posix_time::time_duration d = b-a;
        o << "[" << (*i).second << ", " << (*j).second << "]\t"
          << d << "\t"
          << double(d.total_microseconds())/double(t.total_microseconds())
          << " %\n";
        i = j;
      }
      o << "\n\n";
    }
};

inline
std::ostream &operator<<(std::ostream &o, const Bench &b) {
  b.put(o);
  return o;
}

#include "singleton.hh"

#endif

namespace gapc {
inline void add_event(const std::string &s) {
#ifdef STATS
  Singleton<Bench>::ref().add_event(s);
#endif
}

template <typename O> inline void print_events(O &o) {
#ifdef STATS
  o << Singleton<Bench>::ref();
#endif
}
}  // namespace gapc

#endif  // RTLIB_BENCH_HH_
