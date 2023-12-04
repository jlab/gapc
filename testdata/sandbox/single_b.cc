#include "../rtlib/singleton.hh"

#include "single_obj.hh"


void bar()
{
  std::cerr << "Constructed in bar: " << &Singleton<Foo>::ref() << std::endl;
}
