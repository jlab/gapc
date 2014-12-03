#include <iostream>


// const-ref is needed here, else
// invalid initialization of non-const reference of type ‘std::string&’ from a temporary of type ‘std::string’
// see http://gcc.gnu.org/ml/gcc-help/2006-04/msg00075.html
std::string &foo(const std::string &s)
{
  std::string t("blub");
  std::cerr << s << std::endl;
  // just a warning ...
  return t;
}


int main(int argc, char **argv)
{
  foo(std::string("foo"));
  return 0;
}

