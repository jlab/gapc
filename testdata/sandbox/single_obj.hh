#ifndef SINGLE_OBJ_HH
#define SINGLE_OBJ_HH

#include <iostream>

struct Foo {
  Foo()
  {
    std::cerr << "This is: " << this << std::endl;
  }
};

#endif
