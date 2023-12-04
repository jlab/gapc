#include "../rtlib/singleton.hh"

#include "single_obj.hh"


void foo()
{
  std::cerr << "Constructed in foo: " << &Singleton<Foo>::ref() << std::endl;
}
