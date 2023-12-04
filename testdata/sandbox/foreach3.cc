
#include <string>
#include <iostream>

int main(int argc, char **argv)
{
  std::string hello( "Hello, world!" );

  for (std::string::iterator i = hello.begin();
      i != hello.end(); ++i) {
    char ch = *i;
    std::cout << ch;
  }
  return 0;
}
