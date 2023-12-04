#include "../rtlib/range.hh"

#include <list>

template <typename Iterator>
void hhh(Iterator first, Iterator second)
{
}

template <typename Iterator>
void hh(Iterator first, Iterator second)
{
  typedef Proxy::Iterator<Iterator, select1st<typename Iterator::value_type> > foo;
  hhh(foo(first), foo(second));
}

template <typename Iterator>
void h(Iterator first, Iterator second)
{
  typedef Proxy::Iterator<Iterator, select1st<typename Iterator::value_type> > foo;
  hh(foo(first), foo(second));
}

template <typename Iterator>
void hhhs(std::pair<Iterator,Iterator> p)
{
}

template <typename Iterator>
void hhs(std::pair<Iterator, Iterator> p)
{
  typedef Proxy::Iterator<Iterator, select1st<typename Iterator::value_type> > foo;
  hhhs(std::make_pair(foo(p.first), foo(p.second)));
}

template <typename Iterator>
void hs(std::pair<Iterator, Iterator> p)
{
  typedef Proxy::Iterator<Iterator, select1st<typename Iterator::value_type> > foo;
  hhs(std::make_pair(foo(p.first), foo(p.second)));
}

int main(int argc, char **argv)
{
  std::list<std::pair<int, int> > l;
  Proxy::Iterator<std::list<std::pair<int,int> >::iterator, select1st<std::pair<int, int> > > itr(l.begin());


  std::list<std::pair<std::pair<int, int>, int> > a;
  Proxy::Iterator<std::list<std::pair<std::pair<int, int>, int> >::iterator,
    select1st<std::pair<std::pair<int, int>, int> > > x(a.begin());

  Proxy::Iterator<Proxy::Iterator<std::list<std::pair<std::pair<int, int>, int> >::iterator, select1st<std::pair<std::pair<int, int>, int>  > >, select1st<std::pair<int, int> > > y(x);

  h(a.begin(), a.end());
  hs(std::make_pair(a.begin(), a.end()));

  splice_left(std::make_pair(a.begin(), a.end()));
  return 0;
}
