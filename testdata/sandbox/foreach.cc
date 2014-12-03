
#include <string>
#include <iostream>
#include <boost/foreach.hpp>

int main(int argc, char **argv)
{
  std::string hello( "Hello, world!" );

  BOOST_FOREACH( char ch, hello )
  {
    std::cout << ch;
  }
  return 0;
}
