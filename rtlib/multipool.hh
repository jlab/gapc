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


#ifndef MULTIPOOL_HH
#define MULTIPOOL_HH


#include "map_pool.hh"

// tr1 has it
#include <boost/cstdint.hpp>

#include <cassert>
#include <cstring>

#include <vector>

template <class K>
class MultiPool {
  private:
    typedef Map::Pool<K> pool_t;

    std::vector<pool_t*> pools;
#ifndef NDEBUG
    size_t max_n;
#endif

    MultiPool(const MultiPool &);
    MultiPool &operator=(const MultiPool&);

    void extend(size_t n)
    {
      size_t old = pools.size();
      if (n<=old)
        return;
      pools.resize(n);
      for (size_t i = old; i<n; ++i)
        pools[i] = new pool_t((i+1));
    }

  public:
    MultiPool()
#ifndef NDEBUG
      : max_n(0)
#endif
    {
      pools.resize(1);
      pools[0] = new pool_t(1);
    }

    ~MultiPool()
    {
      for (typename std::vector<pool_t*>::iterator i = pools.begin();
          i!=pools.end(); ++i)
        delete *i;
    }

    void purge()
    {
      assert(false);
    }

    K * malloc(size_t n)
    {
      assert(n);
#ifndef NDEBUG
      if (n > max_n) {
        max_n = n;
        //std::cerr << "MAX N: " << max_n << '\n';
      }
#endif
      extend(n);
      K *r = pools[n-1]->malloc();
      assert(r);
      std::memset(r, 0, sizeof(K)*n);
      return r;
    }

    void free(K *x, size_t n)
    {
      assert(x);
      assert(n<=pools.size());
      pools[n-1]->free(x);
    }
};

#endif
