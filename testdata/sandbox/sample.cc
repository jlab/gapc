/*
 * ./a.out 5 3 2 | grep Sampled | sort  | uniq -c
 *
 * compile via:
 *
 * g++ -Wall -g sample.cc /usr/lib/libgsl.a
 *
 * or
 * 
 * g++ -Wall -g sample.cc -static -lgsl
 *
 * or
 *
 * g++ -Wall -g sample.cc -lgsl /usr/lib/libatlas.so.3gf.0 /usr/lib/libcblas.so.3gf.0
 * <- WTF?!?
 */

#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>

#include <cassert>
#include <vector>

#include <stdlib.h>


/*
gsl_ran_discrete_t * gsl_ran_discrete_preproc (size_t K, const double * P);

size_t gsl_ran_discrete (const gsl_rng * r, const gsl_ran_discrete_t * g);

void gsl_ran_discrete_free (gsl_ran_discrete_t * g);
*/


/*

#include <gsl/gsl_rng.h>

gsl_rng * r;

int
main (void)
{
  const gsl_rng_type * T;

  gsl_rng_env_setup();

  T = gsl_rng_default;
  r = gsl_rng_alloc (T);
  
  printf("generator type: %s\n", gsl_rng_name (r));
  printf("seed = %u\n", gsl_rng_default_seed);
  printf("first value = %u\n", gsl_rng_get (r));
  return 0;
}

   */



namespace scil {

  class rng {
    private:
      gsl_rng *t;
    public:
      rng()
        : t(0)
      {
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


int main2(int argc, char **argv)
{
  gsl_rng_env_setup();
  const gsl_rng_type *rng_type = gsl_rng_default;
  gsl_rng *rng = gsl_rng_alloc(rng_type);

  printf("generator type: %s\n", gsl_rng_name(rng));
  printf("seed = %u\n", gsl_rng_default_seed);
  printf("first value = %u\n", gsl_rng_get(rng));

  size_t k = argc-1;
  double *array = (double*) malloc(k*sizeof(double));
  for (size_t i = 0; i < k; ++i)
    array[i] = atoi(argv[i+1]);

  gsl_ran_discrete_t *x = gsl_ran_discrete_preproc(k, array);
  for (size_t i = 0; i < 1000; ++i) {
    size_t sample = gsl_ran_discrete(rng, x);
    printf("Sampled %zu\n", sample);
  }

  gsl_ran_discrete_free(x);
  free(array);

  gsl_rng_free(rng);

  return 0;
}

int main(int argc, char **argv)
{
  gsl_rng_env_setup();
  scil::rng rng;
  scil::ran_discrete x(rng);
  for (size_t i = 0; i < unsigned(argc)-1; ++i)
    x.push_back(atoi(argv[i+1]));
  x.init();
  for (size_t i = 0; i < 100000; ++i)
    printf("Sampled %zu\n", x.sample());
  return 0;
}


