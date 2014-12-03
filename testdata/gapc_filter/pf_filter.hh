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

#ifndef PF_FILTER_HH
#define PF_FILTER_HH

/*
List_Ref<std::pair<Shape, pfanswer> > out::h(List_Ref<std::pair<Shape, pfanswer> > i_orig)
{
  std::pair<List<std::pair<Shape, pfanswer> >::iterator, List<std::pair<Shape, pfanswer> >::iterator> range = get_range(i_orig);
  List_Ref<std::pair<Shape, pfanswer> > i = h(range);
  return pf_filter(i);
 */


// avoid use of hashtable: because of filterering #classes is
// relatively small -> constant factor of hashtable is
// significant in this usecase

inline
List_Ref<std::pair<Shape, pfanswer> >
pf_filter(List_Ref<std::pair<Shape, pfanswer> > &x)
{
  List<std::pair<Shape, pfanswer> > &l = x.ref();
  List_Ref<std::pair<Shape, pfanswer> > ret;
  List<std::pair<Shape, pfanswer> > &r = ret.ref();
  double sum = 0;
  for (List<std::pair<Shape, pfanswer> >::iterator i = l.begin();
       i != l.end(); ++i)
    sum += (*i).second.pf.q1;
  double thresh = 0.000001 * sum;
  for (List<std::pair<Shape, pfanswer> >::iterator i = l.begin();
       i != l.end(); ++i)
    if ((*i).second.pf.q1 > thresh)
      //push_back_class_syn(ret, *i);
      r.push_back(*i);

  // WTF?!? this way around the adpf_nonamp shape5*pf
  // rna200, tab-all, unger needs
  // 1/2 ram:
  // rss   683304 kb vs.
  // rss  1326996 kb
  
  List_Ref<std::pair<Shape, pfanswer> > foo;
  append_class_syn(foo, ret);
  return foo;
  
  //return ret;
}


// needed for (shape * (mfe * pf) * pretty) --kbacktrack ing since
// the 3rd component needs to be ignored (its synoptic)
inline bool operator==(const std::pair<Shape, std::pair<mfeanswer, pfanswer> >
    &a, const std::pair<Shape, std::pair<mfeanswer, pfanswer> > &b)
{
  return a.first == b.first && a.second.first == b.second.first;
}

// needed for (mfe * pf) * pretty --backtrack
inline bool operator==(const std::pair<mfeanswer, pfanswer> &a,
                       const std::pair<mfeanswer, pfanswer> &b)
{
  return a.first == b.first;
}

inline
List_Ref<std::pair<Shape, std::pair<mfeanswer, pfanswer> > >
pf_filter2(List_Ref<std::pair<Shape, std::pair<mfeanswer, pfanswer> > > &x)
{
  List<std::pair<Shape, std::pair<mfeanswer, pfanswer> > > &l = x.ref();
  List_Ref<std::pair<Shape, std::pair<mfeanswer, pfanswer> > > ret;
  List<std::pair<Shape, std::pair<mfeanswer, pfanswer> >  > &r = ret.ref();
  double sum = 0;
  for (List<std::pair<Shape, std::pair<mfeanswer, pfanswer> > >::iterator i = l.begin();
       i != l.end(); ++i)
    sum += (*i).second.second.pf.q1;
  double thresh = 0.000001 * sum;
  for (List<std::pair<Shape, std::pair<mfeanswer, pfanswer> > >::iterator i = l.begin();
       i != l.end(); ++i)
    if ((*i).second.second.pf.q1 > thresh)
      r.push_back(*i);

  return ret;
}

template <typename Iterator>
inline
List_Ref<std::pair<Shape, std::pair<mfeanswer, pfanswer> > >
pf_filter2itr(std::pair<Iterator, Iterator> x)
{
  List_Ref<std::pair<Shape, std::pair<mfeanswer, pfanswer> > > ret;
  List<std::pair<Shape, std::pair<mfeanswer, pfanswer> >  > &r = ret.ref();
  double sum = 0;
  for (Iterator i = x.first;
       i != x.second; ++i)
    sum += (*i).second.second.pf.q1;
  double thresh = 0.000001 * sum;
  for (Iterator i = x.first;
       i != x.second; ++i)
    if ((*i).second.second.pf.q1 > thresh)
      r.push_back(*i);

  return ret;
}

  template <typename T, typename U = uint32_t>
  struct PfInspector {
    double sum;
    PfInspector() : sum(0) {}
    U hash(const T &x) const
    {
      return hashable_value(x.first);
    }
    void update(T &dst, const T &src)
    {
      if (src.second.first < dst.second.first)
        dst.second.first = src.second.first;
      dst.second.second += src.second.second;
      sum += src.second.second.pf.q1;
    }
    bool equal(const T &a, const T &b) const
    {
      return a.first == b.first;
    }
    bool filter() const { return true; }
    bool filter(const T &x) const
    {
      double thresh = 0.000001 * sum;
      return x.second.second.pf.q1 <= thresh;
    }
  };

template <typename T>
struct p_func_filter_1
{
  double sum;
  p_func_filter_1()
    : sum(0) {}
  void update(const T &src)
  {
    sum += src.second.pf.q1;
  }
  bool ok(const T &x) const
  {
    double thresh = 0.000001 * sum;
    return x.second.pf.q1 > thresh;
  }
};

template <typename T>
struct p_func_filter_1_all
{
  double sum;
  p_func_filter_1_all()
    : sum(0) {}
  void update(const T &x)
  {
    // FIXME sum up q2, q3, q4, too?!
    sum +=
      //x.second.pf.q1;
      x.second.pf.q1 + x.second.pf.q2 + x.second.pf.q3 + x.second.pf.q4;
  }
  bool ok(const T &x) const
  {
    double thresh = 0.000001 * sum;
    return
      x.second.pf.q1 + x.second.pf.q2 + x.second.pf.q3 + x.second.pf.q4
      > thresh; 
  }
};

template <typename T>
struct p_func_filter
{

  static double cutoff_prob;

  double sum;
  p_func_filter()
    : sum(0) {}
  void update(const T &src)
  {
    sum += src.second.second.pf.q1;
  }
  bool ok(const T &x) const
  {
    double thresh = cutoff_prob  * sum;
    return x.second.second.pf.q1 > thresh;
  }
};

template <typename T>
struct p_func_filter_all
{

  static double cutoff_prob;

  double sum;
  p_func_filter_all()
    : sum(0) {}
  void update(const T &src)
  {
    sum += src.second.second.pf.q1;
  }
  bool ok(const T &x) const
  {
    double thresh = cutoff_prob  * sum;
    return
      x.second.second.pf.q1 +
      x.second.second.pf.q2 +
      x.second.second.pf.q3 +
      x.second.second.pf.q4
      > thresh;
  }
};

#ifdef GAPC_MOD_TRANSLATION_UNIT

template <typename T>
double p_func_filter<T>::cutoff_prob = 0.000001;

template <typename T>
double p_func_filter_all<T>::cutoff_prob = 0.000001;

#endif


#endif
