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

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE shape
#include <iostream>
#include <ctime>
#include <string>
#include <boost/test/unit_test.hpp>

#include "macros.hh"

#include "../../rtlib/shape.hh"

#include <boost/random/linear_congruential.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/random.hpp>


#ifdef __APPLE__
  // work around weird Mac OS X type ambiguity problems
  // cf. http://stackoverflow.com/questions/11603818/why-is-there-ambiguity-between-uint32-t-and-uint64-t-when-using-size-t-on-mac-os

  #ifdef __x86_64__
    typedef uint64_t size_shape;
  #else
    typedef uint32_t size_shape;
  #endif
#else
  typedef size_t size_shape;
#endif

BOOST_AUTO_TEST_CASE(char_shape) {
  Fiber<unsigned char, unsigned char> f;
  f.append('[');
  f.append(']');
  f.append('_');
  f.append('_');
  f.append(']');
  std::ostringstream o;
  o << f;
  CHECK_EQ(o.str(), "[]__]");

  Fiber<unsigned char, unsigned char> g;
  g.append('_');
  g.append('_');
  g.append(']');
  std::ostringstream p;
  p << g;
  CHECK_EQ(p.str(), "__]");

  f.append(g);
  std::ostringstream r;
  r << f;
  CHECK_EQ(r.str(), "[]__]__]");

  Fiber<unsigned char, unsigned char> h;
  h = f;
  std::ostringstream s;
  s << h;
  CHECK_EQ(s.str(), "[]__]__]");

  h.append(f);
  std::ostringstream t;
  t << h;
  CHECK_EQ(t.str(), "[]__]__][]__]__]");
}

BOOST_AUTO_TEST_CASE(char_shape_odd) {
  Fiber<unsigned char, unsigned char> f;
  f.append('[');
  f.append(']');
  f.append('_');
  f.append('_');
  f.append(']');
  std::ostringstream o;
  o << f;
  CHECK_EQ(o.str(), "[]__]");

  Fiber<unsigned char, unsigned char> g;
  g.append('_');
  g.append(']');
  std::ostringstream p;
  p << g;
  CHECK_EQ(p.str(), "_]");

  f.append(g);
  std::ostringstream r;
  r << f;
  CHECK_EQ(r.str(), "[]__]_]");

  Fiber<unsigned char, unsigned char> h;
  h = f;
  std::ostringstream s;
  s << h;
  CHECK_EQ(s.str(), "[]__]_]");

  h.append(f);
  std::ostringstream t;
  t << h;
  CHECK_EQ(t.str(), "[]__]_][]__]_]");
}

BOOST_AUTO_TEST_CASE(char_shape_eq) {
  Fiber<unsigned char, unsigned char> f;
  f.append('[');
  f.append(']');
  f.append('_');
  f.append('_');
  f.append(']');
  Fiber<unsigned char, unsigned char> g;
  g.append('[');
  g.append(']');
  g.append('_');
  g.append('_');
  g.append(']');
  CHECK_EQ(f, g);
  Fiber<unsigned char, unsigned char> h;
  h.append('[');
  h.append(']');
  h.append('_');
  g.append('[');
  CHECK_NOT_EQ(f, h);
}

BOOST_AUTO_TEST_CASE(char_shape_less) {
  Fiber<unsigned char, unsigned char> f;
  f.append('[');
  f.append(']');
  f.append('_');
  f.append('_');
  f.append(']');
  Fiber<unsigned char, unsigned char> g;
  g.append('[');
  g.append(']');
  g.append('_');
  g.append('_');
  g.append('[');
  CHECK_LESS(g, f);
  CHECK_NOT_EQ(g, f);
  CHECK(!(f < g));
  Fiber<unsigned char, unsigned char> h;
  h.append('[');
  h.append(']');
  h.append('_');
  CHECK_LESS(h, g);
  CHECK_NOT_EQ(h, g);
  CHECK(!(g < h));
}

BOOST_AUTO_TEST_CASE(shape) {
  Shape h;
  h.append('[');
  h.append(']');
  std::ostringstream o;
  o << h;
  CHECK_EQ(o.str(), "[]");

  Shape g;
  char a[3] = { '[', '_', ']' };
  char s[100];
  int i;
  for (i = 0; i < 64; i++) {
    g.append(a[i%3]);
    s[i] = a[i%3];
  }
  s[i] = 0;
  std::ostringstream p;
  p << g;
  CHECK_EQ(p.str(), s);
}

#include <set>

BOOST_AUTO_TEST_CASE(shape_set) {
  Shape h;
  h.append('[');
  h.append(']');
  Shape g;
  CHECK_NOT_EQ(h, g);
  CHECK(!(h < g));
  CHECK_LESS(g, h);
  std::set<Shape> s;
  s.insert(g);
  s.insert(h);
  CHECK_EQ(s.size(), static_cast<uint32_t>(2));
  std::set<Shape>::iterator i = s.begin();
  std::ostringstream x;
  x << *i;
  ++i;
  std::ostringstream y;
  y << *i;
  CHECK_EQ(x.str(), "");
  CHECK_EQ(y.str(), "[]");
}

#include "../../rtlib/cm_alph.hh"

BOOST_AUTO_TEST_CASE(cm_alph) {
  // std::cerr << "======================================" << std::endl;
  typedef Fiber<size_shape, unsigned char,
                CmAlph<size_shape, unsigned char> > Ambi;
  Ambi s;
  s.append('D');
  s.append('I');
  std::ostringstream y;
  y << s;
  CHECK_EQ(y.str(), "DI");
  s.append('R');
  s.append('r');
  std::ostringstream x;
  x << s;
  CHECK_EQ(x.str(), "DIRr");

  Ambi t;
  Ambi u;
  std::ostringstream z;
  t.append('K');
  z << t;
  CHECK_EQ(z.str(), "K");
  t = u;
  t.append('R');
  z << t;
  CHECK_EQ(z.str(), "KR");
  t = u;
  t.append('r');
  z << t;
  CHECK_EQ(z.str(), "KRr");
  t = u;
  t.append('D');
  z << t;
  CHECK_EQ(z.str(), "KRrD");
  t = u;
  t.append('I');
  z << t;
  CHECK_EQ(z.str(), "KRrDI");
  t = u;
  t.append('L');
  z << t;
  CHECK_EQ(z.str(), "KRrDIL");
  t = u;
  t.append('P');
  z << t;
  CHECK_EQ(z.str(), "KRrDILP");
  t = u;
  t.append('M');
  z << t;
  CHECK_EQ(z.str(), "KRrDILPM");
  t = u;
  t.append('l');
  z << t;
  CHECK_EQ(z.str(), "KRrDILPMl");
}

BOOST_AUTO_TEST_CASE(cm_alph_app) {
  typedef Fiber<size_shape, unsigned char,
                CmAlph<size_shape, unsigned char> > Ambi;
  Ambi s;
  s.append('D');
  s.append('I');
  s.append('l');

  Ambi t;
  t.append('L');
  t.append('K');
  t.append('D');
  s.append(t);
  std::ostringstream z;
  z << s;
  CHECK_EQ(z.str(), "DIlLKD");
}

typedef boost::mt19937 rand_gen;
typedef boost::uniform_int<> rand_dist;

template <typename T, typename Size, typename alphset >
static
void app(Fiber<T, Size, alphset> *a, const std::string &s) {
  for (std::string::const_iterator i = s.begin(); i != s.end(); ++i)
    a->append(*i);
}

BOOST_AUTO_TEST_CASE(cm_alph_random) {
  typedef Fiber<uint32_t, unsigned char, CmAlph<uint32_t, unsigned char> >
    Ambi;

  rand_gen gen(static_cast<unsigned int>(std::time(0)));
  boost::variate_generator<rand_gen&, rand_dist>
    die_alph(gen, rand_dist(0, 8));
  boost::variate_generator<rand_gen&, rand_dist>
    die_len(gen, rand_dist(1, 60));

  const char alph[9] =
     { 'M', 'D', 'I', 'l', 'L', 'R', 'r', 'P', 'K' };

  for (int i = 0; i < 100; ++i) {
    Ambi a, b, x;
    std::string s, t;
    for (int a = 0; a < die_len(); ++a) {
      s.push_back(alph[die_alph()]);
    }
    for (int a = 0; a < die_len(); ++a) {
      t.push_back(alph[die_alph()]);
    }
    app(&a, s);
    app(&b, t);
    // std::cerr << "App: " << s << " " << t << '\n';
    x.append(a);
    x.append(b);
    std::ostringstream o;
    o << x;
    CHECK_EQ(o.str(), s+t);
  }
}

BOOST_AUTO_TEST_CASE(shape_random) {
  rand_gen gen(static_cast<unsigned int>(std::time(0)));
  boost::variate_generator<rand_gen&, rand_dist>
    die_alph(gen, rand_dist(0, 2));
  boost::variate_generator<rand_gen&, rand_dist>
    die_len(gen, rand_dist(1, 100));

  const char alph[3] =
     { '[', ']', '_' };

  for (int i = 0; i < 100; ++i) {
    Shape a, b, x;
    std::string s, t;
    for (int a = 0; a < die_len(); ++a) {
      s.push_back(alph[die_alph()]);
    }
    for (int a = 0; a < die_len(); ++a) {
      t.push_back(alph[die_alph()]);
    }
    app(&a, s);
    app(&b, t);
    // std::cerr << "App: " << s << " " << t << '\n';
    x.append(a);
    x.append(b);
    std::ostringstream o;
    o << x;
    CHECK_EQ(o.str(), s+t);
  }
}

BOOST_AUTO_TEST_CASE(shape_itr) {
  Shape a;
  a.append('[');
  Shape::iterator i = a.begin();
  CHECK(i != a.end());
  CHECK(i == a.begin());
  CHECK_EQ(*i, '[');
  ++i;
  CHECK(i == a.end());
  CHECK(i != a.begin());
}

BOOST_AUTO_TEST_CASE(shape_rev_itr) {
  Shape a;
  a.append('[');
  a.append('_');
  a.append(']');
  Shape::reverse_iterator i = a.rbegin();
  CHECK(i != a.rend());
  CHECK(i == a.rbegin());
  CHECK_EQ(*i, ']');
  ++i;
  CHECK(i != a.rend());
  CHECK(i != a.rbegin());
  CHECK_EQ(*i, '_');
  ++i;
  CHECK(i != a.rend());
  CHECK(i != a.rbegin());
  CHECK_EQ(*i, '[');
  ++i;
  CHECK(i == a.rend());
  CHECK(i != a.rbegin());
}

BOOST_AUTO_TEST_CASE(shape_rev_itr_cm) {
  typedef Fiber<size_shape, unsigned char, CmAlph<size_shape, unsigned char> >
    Str;
  Str a;
  a.append('D');
  a.append('I');
  a.append('l');
  Str::reverse_iterator i = a.rbegin();
  CHECK(i != a.rend());
  CHECK(i == a.rbegin());
  CHECK_EQ(*i, 'l');
  ++i;
  CHECK(i != a.rend());
  CHECK(i != a.rbegin());
  CHECK_EQ(*i, 'I');
  ++i;
  CHECK(i != a.rend());
  CHECK(i != a.rbegin());
  CHECK_EQ(*i, 'D');
  ++i;
  CHECK(i == a.rend());
  CHECK(i != a.rbegin());
}

BOOST_AUTO_TEST_CASE(shape_rev_itr_rand) {
  typedef Fiber<size_shape, unsigned char, CmAlph<size_shape, unsigned char> >
    Str;
  rand_gen gen(static_cast<unsigned int>(std::time(0)));
  boost::variate_generator<rand_gen&, rand_dist>
    die_len(gen, rand_dist(0, 50));
  boost::variate_generator<rand_gen&, rand_dist>
    die_alph(gen, rand_dist(0, 8));
  const char alph[9] =
     { 'M', 'D', 'I', 'l', 'L', 'R', 'r', 'P', 'K' };
  for (size_t x = 0; x < 100u; ++x) {
    std::string s;
    for (size_t i = 0; i < size_t(die_len()); ++i)
      s.push_back(alph[die_alph()]);
    Str a;
    app(&a, s);
    std::string t;
    for (Str::reverse_iterator i = a.rbegin(); i != a.rend(); ++i)
      t.push_back(*i);
    std::reverse(t.begin(), t.end());
    CHECK_EQ(s, t);
  }
}

BOOST_AUTO_TEST_CASE(shape_rev_itr_rand_set) {
  typedef Fiber<size_shape, unsigned char, CmAlph<size_shape, unsigned char> >
    Str;
  rand_gen gen(static_cast<unsigned int>(std::time(0)));
  boost::variate_generator<rand_gen&, rand_dist>
    die_len(gen, rand_dist(2, 50));
  boost::variate_generator<rand_gen&, rand_dist>
    die_alph(gen, rand_dist(0, 8));
  const char alph[9] =
     { 'M', 'D', 'I', 'l', 'L', 'R', 'r', 'P', 'K' };
  for (size_t x = 0; x < 100u; ++x) {
    std::string s;
    size_t l = die_len();
    for (size_t i = 0; i < l; ++i)
      s.push_back(alph[die_alph()]);

    Str a;
    app(&a, s);

    boost::variate_generator<rand_gen&, rand_dist>
      die_rep(gen, rand_dist(0, l-1));

    size_t rep = die_rep();

    size_t u = 0;
    Str::reverse_iterator i = a.rbegin();
    for (; u < rep; ++i, u++) {}
    assert(i != a.rend());
    char c = alph[die_alph()];
    // std::cout << "Replace " << rep << " th with " << c << "( " << s << " "
    //           << s.size() << " )" << std::endl;
    i.set(c);
    std::reverse(s.begin(), s.end());
    s[u] = c;
    std::reverse(s.begin(), s.end());


    std::string t;
    for (Str::reverse_iterator i = a.rbegin(); i != a.rend(); ++i)
      t.push_back(*i);

    std::reverse(t.begin(), t.end());
    CHECK_EQ(s, t);
  }
}

BOOST_AUTO_TEST_CASE(shape_itr_rand) {
  typedef Fiber<size_shape, unsigned char, CmAlph<size_shape, unsigned char> >
    Str;
  rand_gen gen(static_cast<unsigned int>(std::time(0)));
  boost::variate_generator<rand_gen&, rand_dist>
    die_len(gen, rand_dist(0, 50));
  boost::variate_generator<rand_gen&, rand_dist>
    die_alph(gen, rand_dist(0, 8));
  const char alph[9] =
     { 'M', 'D', 'I', 'l', 'L', 'R', 'r', 'P', 'K' };
  for (size_t x = 0; x < 100u; ++x) {
    std::string s;
    for (size_t i = 0; i < size_t(die_len()); ++i)
      s.push_back(alph[die_alph()]);
    Str a;
    app(&a, s);
    std::string t;
    for (Str::iterator i = a.begin(); i != a.end(); ++i)
      t.push_back(*i);
    CHECK_EQ(s, t);
  }
}

BOOST_AUTO_TEST_CASE(shape_drop) {
  Shape a;
  a.append('[');
  a.append('_');
  a.append(']');
  Shape::reverse_iterator i = a.rbegin();
  i.drop();
  i = a.rbegin();
  CHECK_EQ(*i, '_');
}

BOOST_AUTO_TEST_CASE(shape_push_after) {
  Shape a;
  a.append('[');
  a.append('[');
  a.append('[');
  a.append('_');
  a.append(']');
  Shape b = push_after_front(a, '[', ']');
  std::ostringstream o;
  o << b;
  CHECK_EQ(o.str(), "[[[]_]");
}

BOOST_AUTO_TEST_CASE(shape_push_before) {
  Shape a;
  a.append('[');
  a.append('[');
  a.append('[');
  a.append('_');
  a.append('_');
  Shape b = push_before_back(a, '_', ']');
  std::ostringstream o;
  o << b;
  CHECK_EQ(o.str(), "[[[]__");
}

BOOST_AUTO_TEST_CASE(empty_class) {
  Shape a;
  a.append('_');
  CHECK_EQ(a, '_');
  CHECK_NOT_EQ(a, '[');
  a.append('_');
  CHECK_NOT_EQ(a, '_');
  Shape b;
  b.append(']');
  CHECK_NOT_EQ(b, '_');
  b.append('_');
  CHECK_NOT_EQ(b, '_');
}

BOOST_AUTO_TEST_CASE(opplus) {
  Shape a;
  Shape b;
  b.append(Shape("_]"));
  a = '[' +  b + "[]";
  std::ostringstream o;
  o << a;
  CHECK_EQ(o.str(), "[_][]");
}

BOOST_AUTO_TEST_CASE(frontback) {
  Shape a;
  a.append(Shape("_]"));
  std::ostringstream o;
  o << a;
  CHECK_EQ(*o.str().begin(), front(a));
  CHECK_EQ(*o.str().rbegin(), back(a));
}

BOOST_AUTO_TEST_CASE(tailer) {
  Shape a;
  a.append(Shape("_["));
  a.append(Shape("]["));
  a.append(']');
  std::ostringstream o;
  o << tail(a);
  CHECK_EQ(o.str(), "[][]");
}
