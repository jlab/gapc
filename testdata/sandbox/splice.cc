#include <iostream>
#include <list>

template <typename Iterator>
void foo(std::pair<Iterator, Iterator> range)
{
  for (Iterator i = range.first; i != range.second; ++i)
    std::cerr << *i << std::endl;
}

void bar(std::pair< std::list<std::pair<int, int> >::iterator,
                    std::list<std::pair<int, int> >::iterator> range)
{
  for (std::list<std::pair<int, int> >::iterator i = range.first;
      i != range.second; ++i)
    std::cerr << '(' << i->first << ',' << i->second << ')' <<std::endl;
}

int main(int argc, char **argv)
{
  std::list<int> l;
  for (int i = 0; i < 10; ++i)
    l.push_back(i);
  foo(std::pair<std::list<int>::iterator, std::list<int>::iterator>
      (l.begin(), l.end()));

  std::list<std::pair<int, int> > m;
  for (int i = 0; i < 10; ++i)
    m.push_back(std::pair<int, int>(i, i*i));
  bar(std::pair<std::list<std::pair<int,int> >::iterator, std::list<std::pair<int, int> >::iterator>
      (m.begin(), m.end()));
  return 0;
}
