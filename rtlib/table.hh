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


// FIXME tables are codegen-erated now

#ifndef RTLIB_TABLE_HH_
#define RTLIB_TABLE_HH_


#include <vector>
#include <map>
#include <cstdlib>
#include <utility>
#include <algorithm>
#include <string>

// FIXME
#include <iostream>

#include "sequence.hh"
#include "list.hh"

namespace Table {

namespace TAG {
struct CONSTANT {
};
struct LINEAR {
};
struct QUADRATIC {
};
}  // namespace TAG

#ifndef DEBUG_CYK
template <typename pos_type, typename Tag = TAG::QUADRATIC>
class CYK {
 private:
 public:
    void resize(pos_type x) const {
    }
    void tabulate(pos_type x) const {
    }
    void un_tabulate(pos_type x) const {
    }
    bool is_tabulated(pos_type x) const {
      assert(0);
      std::abort();
      return false;
    }
    bool no_check() const { return true; }
    void clear() const {}
};
#endif

template <typename pos_type, typename Tag = TAG::QUADRATIC>
class Unger {
 private:
    std::vector<bool> tabulated;

 public:
    void resize(pos_type x) {
      tabulated.resize(x);
    }
    void tabulate(pos_type x) {
      tabulated[x] = true;
    }
    void un_tabulate(pos_type x) {
      tabulated[x] = false;
    }
    bool is_tabulated(pos_type x) const {
      return tabulated[x];
    }
    bool no_check() const { return false; }
    void clear() { tabulated.clear(); }
};

template <typename pos_type>
class Unger<pos_type, TAG::CONSTANT> {
 private:
    std::map<pos_type, bool> tabulated;

 public:
    void resize(pos_type x) const {
    }
    void tabulate(pos_type x) {
      tabulated[x] = true;
    }
    void un_tabulate(pos_type x) {
      tabulated[x] = false;
    }
    bool is_tabulated(pos_type x) const {
      return tabulated.find(x) != tabulated.end();
    }
    bool no_check() const { return false; }
    void clear() { tabulated.clear(); }
};

#ifdef DEBUG_CYK
template <typename pos_type, typename Tag = TAG::QUADRATIC>
class CYK : public Unger<pos_type, Tag> {
};
template <typename pos_type>
class CYK<pos_type, TAG::CONSTANT> : public Unger<pos_type, TAG::CONSTANT> {
};
#endif



#ifndef WINDOW_MODE
template <typename T, template<typename, typename> class Mode = Unger,
typename pos_type = unsigned int>
class Constant {
 private:
    pos_type n;
    Mode<pos_type, TAG::CONSTANT> marker;
    std::map<pos_type, T> elements;

    pos_type index(pos_type i, pos_type j) {
      assert(i <= n);
      assert(j <= n);
      return i * (n+1) + j;
    }

 public:
    typedef T element_type;
    pos_type count;

    template<typename alphabet, typename seqpos>
    explicit Constant(const Basic_Sequence<alphabet, seqpos> &seq)
      : count(0) {
      n = seq.size();
    }

    explicit Constant(pos_type l)
      : n(l), count(0) {
    }

    Constant()
      : n(0), count(0) {}

    void clear() {
      n = 0;
      count = 0;
      marker.clear();
      elements.clear();
    }

    template<typename alphabet, typename seqpos>
    void init(
      const Basic_Sequence<alphabet, seqpos> &seq,
      const std::string &name_) {
      n = seq.size();
    }

    bool is_tabulated(pos_type i, pos_type j) {
      return marker.is_tabulated(index(i, j));
    }

    T &get_tabulated(pos_type i, pos_type j) {
      assert(marker.no_check() || is_tabulated(i, j));
      return elements[index(i, j)];
    }

    void tabulate(pos_type i, pos_type j, T &element) {
      assert(marker.no_check() || !is_tabulated(i, j));
      elements[index(i, j)] = element;
      marker.tabulate(index(i, j));
#ifndef NO_STATS
      count++;
#endif
    }

    double ratio() {
      return static_cast<double>(100 * count) /
        (static_cast<double>((n*(n+1))/2));
    }

    void print_stats(std::ostream &o, std::string name) {
      o << "Table " << name << "(const):\t"
        << "(" << count << " entries, "
        <<  elements.size() << " map count) used\n";
    }

    template<typename alphabet, typename seqpos>
    void window_init(
      const Basic_Sequence<alphabet, seqpos> &seq, pos_type w_size,
      pos_type w_inc) {}
    void window_increment() {}
};
#endif

struct Left{};
struct Right{};

template <typename T, typename pos_type>
struct Lin {
  static pos_type row(pos_type i, pos_type j);
  static pos_type col(pos_type i, pos_type j);
};

template <typename pos_type>
struct Lin<Left, pos_type> {
  static pos_type row(pos_type i, pos_type j) {
    return i;
  }
  static pos_type col(pos_type i, pos_type j) {
    return j;
  }
};

template <typename pos_type>
struct Lin<Right, pos_type> {
  static pos_type row(pos_type i, pos_type j) {
    return j;
  }
  static pos_type col(pos_type i, pos_type j) {
    return i;
  }
};

#ifndef WINDOW_MODE
template <typename L, typename T,
template<typename, typename> class Mode = Unger,
typename pos_type = unsigned int>
class Linear {
 private:
    pos_type n;
    std::map<pos_type, pos_type> map;
    Mode<pos_type, TAG::LINEAR> marker;
    std::vector<T> elements;
    pos_type row;
    pos_type rows;

    pos_type index(pos_type i, pos_type j) {
      assert(i <= n);
      assert(j <= n);
      return i * (n+1) + j;
    }

    void init() {
      row = 0;
      rows = 1;
      marker.resize(n+1);
      elements.resize(n+1);
    }

    void resize() {
      rows *= 2;
      pos_type s = n+1;
      marker.resize(rows * s);
      elements.resize(rows * s);
    }

    typedef Lin<L, pos_type> M;

 public:
    pos_type count;
    typedef T element_type;

    template<typename alphabet, typename seqpos>
    explicit Linear(const Basic_Sequence<alphabet, seqpos> &seq)
      : count(0) {
      n = seq.size();
      init();
    }

    explicit Linear(pos_type l)
      : n(l), count(0) {
      init();
    }

    Linear() : n(0), count(0) {}

    void clear() {
      n = 0;
      count = 0;
      marker.clear();
      elements.clear();
      map.clear();
      init();
    }

    template<typename alphabet, typename seqpos>
    void init(
      const Basic_Sequence<alphabet, seqpos> &seq,
      const std::string &name_) {
      n = seq.size();
      init();
    }

    bool is_tabulated(pos_type i, pos_type j) {
      assert(i <= n);
      assert(j <= n);
      if (map.find(M::row(i, j)) == map.end())
        return false;
      return marker.is_tabulated(index(map[M::row(i, j)], M::col(i, j)));
    }

    T &get_tabulated(pos_type i, pos_type j) {
      assert(marker.no_check() || is_tabulated(i, j));
      return elements[index(map[M::row(i, j)], M::col(i, j))];
    }

    void tabulate(pos_type i, pos_type j, T &element) {
      assert(marker.no_check() || !is_tabulated(i, j));
      if (map.find(M::row(i, j)) == map.end()) {
        map.insert(std::pair<pos_type, pos_type>(M::row(i, j), row++));
        if (row-1 == rows)
          resize();
      }
      marker.tabulate(index(map[M::row(i, j)], M::col(i, j)));
      elements[index(map[M::row(i, j)], M::col(i, j))] = element;
#ifndef NO_STATS
      count++;
#endif
    }

    double ratio() {
      return static_cast<double>(100 * count) /
        (static_cast<double>(elements.size()));
    }

    void print_stats(std::ostream &o, std::string name) {
      o << "Table " << name << "(linear):\t"
        << ratio() << " % (" << count << " entries, "
        <<  map.size() << " map count) used\n";
    }

    template<typename alphabet, typename seqpos>
    void window_init(
      const Basic_Sequence<alphabet, seqpos> &seq, pos_type w_size,
      pos_type w_inc) {}
    void window_increment() {}
};
#endif

template <typename pos_type = unsigned int>
struct RawIndex {
  pos_type operator()(pos_type i, pos_type j, pos_type n) const {
    assert(i <= n);
    assert(j <= n);
    return i * (n+1) + j;
  }

  pos_type operator()(pos_type n) const {
    return (n+1)*(n+1);
  }
};

template <typename pos_type = unsigned int>
struct DiagIndex {
  pos_type operator()(pos_type i, pos_type j, pos_type n) const {
    assert(i <= j);
    assert(j <= n);
    return (j*(j+1))/2 + i;
  }

  pos_type operator()(pos_type n) const {
    return (n*(n+1))/2 + n + 1;
  }
};

template <typename pos_type = unsigned int>
struct Diag2Index {
  pos_type operator()(pos_type i, pos_type j, pos_type n) const {
    assert(i <= j);
    assert(j <= n);
    return i*(n-2)+j;
  }

  pos_type operator()(pos_type n) const {
    return (n*(n+1))/2 + n + 1;
  }
};

template <typename Index, typename pos_type = unsigned int>
struct WindowIndex {
  Index index;
  pos_type window_size;
  pos_type window_inc;
  pos_type seq_size;
  pos_type first;
  pos_type last;

  pos_type operator()(pos_type i, pos_type j, pos_type n) const {
    assert(i >= first);
    assert(j <= seq_size);
    assert(j <= first+window_size);

    pos_type a = i % (window_size+1);
    pos_type b = j % (window_size+1);
    if (a > b)
      std::swap(a, b);

    // std::cerr << "i " << i << " j " << j << " a " <<
    // a << " b " << b << std::endl;

    return index(a, b, window_size);
  }

  pos_type operator()(pos_type n) const {
    return index(window_size);
  }
};

template <typename T,
template<typename, typename> class Mode = Unger,
typename pos_type = unsigned int, class Index = DiagIndex<pos_type> >
class Quadratic {
 private:
    pos_type n;
    Mode<pos_type, TAG::QUADRATIC> marker;
    std::vector<T> elements;

#ifdef WINDOW_MODE
    WindowIndex<Index> index;
#else
    Index index;
#endif

#ifdef TRACE
    std::string name;
#endif

    void init() {
      pos_type s = index(n);
      marker.resize(s);
      elements.resize(s);
    }

 public:
#ifdef STATS
    pos_type count;
#endif

    typedef T element_type;

    template<typename alphabet, typename seqpos>
    explicit Quadratic(const Basic_Sequence<alphabet, seqpos> &seq)
#ifdef STATS
      : count(0)
#endif
    {
      n = seq.size();
      init();
    }

      explicit Quadratic(pos_type l)
        : n(l)
#ifdef STATS
          , count(0)
#endif
      {
        init();
      }

      Quadratic()
        : n(0)
#ifdef STATS
          , count(0)
#endif
      {}

      void clear() {
#ifdef STATS
        count = 0;
#endif
        n = 0;
        marker.clear();
        elements.clear();
      }

      template<typename alphabet, typename seqpos>
      void init(
        const Basic_Sequence<alphabet, seqpos> &seq,
        const std::string &name_) {
#ifdef STATS
        count = 0;
#endif
#ifdef TRACE
        name = name_;
        std::cerr << name << ' ' << seq.size()+1 << ' ' << seq.size()+1
          << '\n';
#endif
#ifdef WINDOW_MODE
        return;
#endif
        n = seq.size();
        init();
      }

#ifdef WINDOW_MODE
      template<typename alphabet, typename seqpos>
      void window_init(const Basic_Sequence<alphabet, seqpos> &seq,
        pos_type w_size, pos_type w_inc) {
#ifdef STATS
        count = 0;
#endif
        assert(w_inc);
        assert(w_inc < w_size);
        n = w_size;
        index.window_size = w_size;
        index.window_inc = w_inc;
        index.first = 0;
        index.last = w_size;
        index.seq_size = seq.size();
        init();
      }

      void window_increment() {
        pos_type inc = index.window_inc;
        if (index.first + index.window_inc > index.seq_size) {
          inc = std::min(index.seq_size - index.first, index.window_inc);
          assert(inc);
        }
        for (pos_type i = index.first; i < index.first + inc; ++i)
          for (pos_type j = i; j <= index.last; ++j) {
            // std::cerr << "Delete: ";
            marker.un_tabulate(index(i, j, index.last));
          }
        index.first += inc;
        index.last += inc;
      }
#else
      template<typename alphabet, typename seqpos>
      void window_init(
        const Basic_Sequence<alphabet, seqpos> &seq, pos_type w_size,
        pos_type w_inc) {}
      void window_increment() {}
#endif

      bool is_tabulated(pos_type i, pos_type j) {
#ifdef TRACE
        std::cerr << name << " 0 " << i << ' ' << j
          << " x"<< '\n';
#endif
        return marker.is_tabulated(index(i, j, n));
      }

      T &get_tabulated(pos_type i, pos_type j) {
#ifdef TRACE
        std::cerr << name << " 1 " << i << ' ' << j
          << ' ' << left_most(elements[index(i, j, n)]) << '\n';
#endif
        assert(marker.no_check() || is_tabulated(i, j));
        return elements[index(i, j, n)];
      }

      void tabulate(pos_type i, pos_type j, T &element) {
#ifdef TRACE
        std::cerr << name << " 2 " << i << ' ' << j
          << " x " << '\n';
#endif
        assert(marker.no_check() || !is_tabulated(i, j));
#ifdef STATS
        count++;
#endif
        elements[index(i, j, n)] = element;
        marker.tabulate(index(i, j, n));
      }

      // void clear();
      // FIXME
      // methods to access elements for external modules
      // usage visualization ...


#ifdef STATS
      double ratio() {
        return static_cast<double>(100 * count) /
          (static_cast<double>index(n) );
      }

      void print_stats(std::ostream &o, std::string name) {
        o << "Table " << name << ":\t"
          << ratio() << " % (" << count << " entries) used\n";
      }
#else
      void print_stats(std::ostream &o, std::string name) {
      }
#endif
};  // namespace

#ifdef WINDOW_MODE
template <typename T,
template<typename, typename> class Mode = Unger,
typename pos_type = unsigned int>
class Constant : public Quadratic<T, Mode, pos_type> {
 public:
    Constant() : Quadratic<T, Mode, pos_type>() {}
};
template <typename L, typename T,
template<typename, typename> class Mode = Unger,
typename pos_type = unsigned int>
class Linear : public Quadratic<T, Mode, pos_type> {
 public:
    Linear() : Quadratic<T, Mode, pos_type>() {}
};
#endif

}  // namespace Table

template <class Tab, typename pos_type>
inline bool is_tabulated(Tab &t, pos_type i, pos_type j) {
  return t.is_tabulated(i, j);
}

template <class Tab, typename pos_type>
inline typename Tab::element_type &get_tabulated(
  Tab &t, pos_type i, pos_type j) {
  return t.get_tabulated(i, j);
}

template <class Tab, typename pos_type>
inline void tabulate(Tab &t, pos_type i, pos_type j,
    typename Tab::element_type &e) {
  t.tabulate(i, j, e);
}

#endif  // RTLIB_TABLE_HH_
