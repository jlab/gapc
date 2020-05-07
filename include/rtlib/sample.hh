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


#ifndef SAMPLE_HH
#define SAMPLE_HH


#ifdef USE_GSL


#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>

#ifndef __GSL_RNG_H__
#error "Could not find libgsl headers"
#error "Probably you just need to install the (suggested) development packages"
#error "e.g. under Debian/Ubuntu: apt-get install libgsl0-dev libatlas-base-dev"
#endif

#include <cassert>
#include <vector>

#include "singleton.hh"


namespace scil {

  class rng {
    private:
      gsl_rng *t;
    public:
      rng()
        : t(0)
      {
        gsl_rng_env_setup();
        const gsl_rng_type *rng_type = gsl_rng_default;
        t = gsl_rng_alloc(rng_type);
      }

      ~rng()
      {
        assert(t);
        gsl_rng_free(t);
      }

      const gsl_rng *operator*() const
      {
        return t;
      }
  };

  class ran_discrete {
    private:
      gsl_ran_discrete_t *x;
      rng &r;
      std::vector<double> array;
      enum State { CLEAR, PUSH, SAMPLE };
      State state;
    public:
      ran_discrete()
        : x(0), r(Singleton<scil::rng>::ref()), state(CLEAR)
      {
        array.reserve(4096/sizeof(double));
      }
      ran_discrete(rng &a)
        : x(0), r(a), state(CLEAR)
      {
        array.reserve(4096/sizeof(double));
      }
      ran_discrete(rng &a, size_t b)
        : x(0), r(a), state(CLEAR)
      {
        array.reserve(b);
      }
      ~ran_discrete()
      {
        if (x)
          gsl_ran_discrete_free(x);
      }
      void clear()
      {
        state = CLEAR;
        array.resize(0);
      }
      void push_back(double d)
      {
        assert(state == CLEAR || state == PUSH);
        state = PUSH;
        array.push_back(d);
      }
      void init()
      {
        assert(state == PUSH);
        state = SAMPLE;
        if (x)
          gsl_ran_discrete_free(x);
        x = gsl_ran_discrete_preproc(array.size(), &array[0]);
      }
      size_t sample()
      {
        assert(state == SAMPLE);
        size_t s = gsl_ran_discrete(*r, x);
        assert(s < array.size());
        return s;
      }
      double sample_value()
      {
        size_t k = sample();
        return array[k];
      }
  };

}

#endif

#endif

