
#include <cassert>
#include <boost/cstdint.hpp>
#define UINT32_MAX 4294967295U

int main(int argc, char **argv)
{
  uint32_t a = UINT32_MAX;
  a+=1;
  assert(a==0);
  a = UINT32_MAX;
  uint32_t b = a;
  b+=a;
  assert(a==b);
  return 0;
}
