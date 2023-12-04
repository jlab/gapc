#include <iostream>


struct C {
  char c;
  short i;
};


int main()
{
  C array[10];
  std::cerr << array << " " << array+1 << " sizeof C " << sizeof(C) << std::endl;
}
