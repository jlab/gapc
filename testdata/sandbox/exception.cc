#include <iostream>
#include <exception>

#include <cstring>

class BaseException : public std::exception {
  private:
    char z;
    char *msg;
  public:
    BaseException(char c) : std::exception(), z(c), msg(0)
    {
      msg = new char[100];
    }
    ~BaseException() throw() { delete[] msg; }
    const char* what() const throw()
    {
      if (!*msg) {
        std::strcpy(msg, "Unknown base '");
        msg[std::strlen(msg)] = z;
        msg[std::strlen(msg)+1] = 0;
        std::strcat(msg, "' in input.");
      }
      return msg;
    }
};

int main()
{
  throw BaseException('D');
  return 0;
}
