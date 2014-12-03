#include <boost/intrusive_ptr.hpp>

// intrusive_ptr_add_ref
// intrusive_ptr_release


#include <iostream>

class B
{
  public:
    size_t count;
    B()
      : count(0)
    {
      std::cerr << "construct B" << std::endl;
    }
    ~B()
    {
      std::cerr << "destruct B" << std::endl;
    }
};

void intrusive_ptr_add_ref(B *b)
{
  b->count++;
}

void intrusive_ptr_release(B *b)
{
  b->count--;
  if (!b->count)
    delete b;
}

int main()
{
  //boost::intrusive_ptr<B> x(new B());
  boost::intrusive_ptr<B> x = boost::intrusive_ptr<B>(new B());
  return 0;
}
