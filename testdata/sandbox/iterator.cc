#include <boost/iterator/transform_iterator.hpp>
#include <iostream>
#include <list>
#include <functional>


template<typename S, typename T>
struct First {
  S & operator() (std::pair<S,T> &p) const {
    return p.first;
  }
  typedef S result_type;
};

template<typename Pair>
struct select2nd : public std::unary_function<Pair,typename Pair::second_type> {
  typename Pair::first_type & operator() (Pair &p) const {
    return p.second;
  }
};


int main(int argc, char **argv)
{
  std::list<std::pair<int, int> > l;
  for (int i=0; i<20; ++i) {
    std::pair<int, int> p(i, i*i);
    l.push_back(p);
  }
  boost::transform_iterator<First<int, int>,
    std::list<std::pair<int, int> >::iterator>
    begin =
      boost::make_transform_iterator<First<int, int> >(l.begin());


  boost::transform_iterator<First<int, int>,
    std::list<std::pair<int, int> >::iterator>
      end(l.end());

  for (boost::transform_iterator<First<int, int>, std::list<std::pair<int, int> >::iterator> i = begin; i != end; ++i) {
    std::cout << *i << std::endl;
  }
 
  boost::transform_iterator<select2nd<std::pair<int,int> >,
    std::list<std::pair<int, int> >::iterator>
    b =
      boost::make_transform_iterator<select2nd<std::pair<int, int> > >(l.begin());
  boost::transform_iterator<select2nd<std::pair<int, int> >,
    std::list<std::pair<int, int> >::iterator>
      e(l.end());

  for (boost::transform_iterator<select2nd<std::pair<int, int> >, std::list<std::pair<int, int> >::iterator> i = b; i != e; ++i) {
    std::cout << *i << std::endl;
  }

  return 0;
}
