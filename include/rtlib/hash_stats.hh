#ifndef HASH_STATS
#define HASH_STATS

// workaround Sun CC 12 Segmentation fault
#if defined(__SUNPRO_CC) && __SUNPRO_CC <= 0x5100
#warning Because of a Sun CC compiler bug, the hash-table STAT-code is disabled
#undef STATS
#endif

#ifdef STATS
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/max.hpp>
#include <boost/accumulators/statistics/sum.hpp>
#include <boost/accumulators/statistics/variance.hpp>
#endif

namespace Hash {

#ifdef STATS 
  namespace ba = boost::accumulators;


  // FIXME adjust to hashtng
  class Stats {
    private:

    double collisions_;
    size_t internal_resizes;
    size_t reallocs;
    size_t coll_per_round;
    size_t size_per_round;
    ba::accumulator_set<size_t, ba::stats<ba::tag::max> > hi_water_load;
    ba::accumulator_set<size_t, ba::stats<ba::tag::max> > hi_water_use;
    ba::accumulator_set<double,
      ba::stats<ba::tag::mean, ba::tag::max, ba::tag::sum,
                ba::tag::variance > >
      probe_acc;
    ba::accumulator_set<double,
      ba::stats<ba::tag::mean, ba::tag::max, ba::tag::variance> >
      coll_acc;
    public:

    Stats()
      : collisions_(0), internal_resizes(0), reallocs(0),
        coll_per_round(0), size_per_round(0)
    {}

    void collision()
    {
      collisions_++;
      coll_per_round++;
    }
    void internal_resize()
    {
      internal_resizes++;
    }
    void realloc()
    {
      reallocs++;
    }
    void load(size_t i)
    {
      hi_water_load(i);
    }
    void use(size_t i)
    {
      hi_water_use(i);
    }
    void probe(double i)
    {
      probe_acc(i);
    }
    void reset()
    {
      assert(size_per_round);
      coll_acc(double(coll_per_round)/double(size_per_round)*100);
      coll_per_round = 0;
    }
    void size(size_t s)
    {
      size_per_round = std::max(s, size_per_round);
    }
    void put(std::ostream &o) const
    {
    o << "\nCollisions: " << collisions_
      << " \%Collisions per round: " << ba::mean(coll_acc) << " (mean) "
      << ba::max(coll_acc) << " (max) "
      << ba::variance(coll_acc) << " (var)\n"
      << "Probe length: " << ba::mean(probe_acc) << " (mean) "
      << ba::max(probe_acc) << " (max) " << ba::sum(probe_acc) << "(sum)"
      << " " << ba::variance(probe_acc) << " (var)\n" 
      << "Reallocs: " << reallocs
      << " Hi water load: " << ba::max(hi_water_load)
      << " Internal resizes: " << internal_resizes
      << " Hi water use: " << ba::max(hi_water_use);
    }
    double collisions() const
    {
      return collisions_;
    }
  };
#endif

  struct NoStats {
    void collision() { }
    void internal_resize() { }
    void realloc() { }
    void load(size_t i) const { }
    void use(size_t i) { }
    void probe(double i) { }
    void reset() {}
    void size(size_t i) {}

    double collisions() const { return 23; }
  };

#ifdef STATS
  inline std::ostream &operator<<(std::ostream &o, const Stats &stats)
  {
    stats.put(o);
    return o;
  }
#endif

  inline std::ostream &operator<<(std::ostream &o, const NoStats &stats)
  {
    return o;
  }

}

#endif
