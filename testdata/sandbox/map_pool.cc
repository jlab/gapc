
#include "../rtlib/map_pool.hh"


int main()
{
  std::vector<size_t*> array;
  //Map::Pool<size_t, 13107200> p;
  Map::Pool<size_t> p;
  for (size_t i = 0; i<100; ++i) {
    size_t *x = p.malloc();
    *x = i;
    array.push_back(x);
  }
  for (std::vector<size_t*>::iterator i = array.begin(); i != array.end(); ++i)
    p.free(*i);
  return 0;
}
