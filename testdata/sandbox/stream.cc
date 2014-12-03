
#include <iostream>

class Printer;

class Base {
  private:
  public:
    virtual void print(Printer &p) = 0;
};

class Type : public Base {
  private:
  public:
    char z;
    Type(char c) : z(c) {}
    void print(Printer &p);
};

class Printer {
  private:
    template<class T>
      friend
        inline Printer &operator<<(Printer &p, T c);
    friend
      inline
        Printer &operator<<(Printer &p, std::ostream& (*fn)(std::ostream&) );
    std::ostream &out;
    size_t line_number;
  public:
    Printer &stream;
    Printer(std::ostream &o) : out(o), line_number(0), stream(*this) {}

    virtual void print(Type &t);

    /*
    Printer &operator<<(Base &b)
    {
      b.print(*this);
      return *this;
    }

    virtual Printer &operator<<(Type &t)
    {
      std::cerr << "type: " << t.z << std::endl;
      return *this;
    }
    */

    /* not found if << applicated to child class object ...
    Printer &operator<<(char c)
    {
      if (c == '\n')
        std::cerr << "NL" << std::endl;
      else
        std::cerr << "X " << c << std::endl;
      return *this;
    }
    */
    
/*
    Printer &operator<<(Printer& (*fn)(Printer&) )
    {
      return fn(*this);
    }
    */

    /* not enough for std::endl
    template<typename T>
    Printer &operator<<(T& (*fn)(T&) )
    {
      fn(out);
      return *this;
    }
    */

    /*
    Printer &operator<<(std::ostream& (*fn)(std::ostream&) )
    {
      fn(out);
      return *this;
    }
    */

/*
    template<typename T>
    Printer &operator<<(T c) {
      std::cerr << "Y " << c << std::endl;
      return *this;
    }
    */
};

Printer & endl(Printer &p) { std::cerr << "NL\n"; return p; }

Printer &operator<<(Printer &p, Printer& (*fn)(Printer&) )
{
  return fn(p);
}

inline Printer &operator<<(Printer &p, std::ostream& (*fn)(std::ostream&) )
{
  fn(p.out);
  p.line_number ++;
  return p;
}

template<typename T>
inline Printer &operator<<(Printer &p, T c) {
  //p.out << "Y " << c << std::endl;
  p.out << c;
  return p;
}

inline Printer &operator<<(Printer &p, Base &b)
{
  b.print(p);
  return p;
}

void Type::print(Printer &p) { p.print(*this); }

void Printer::print(Type &t)
{
  stream << "type: " << t.z << std::endl;
}

class Printer2 : public Printer {
  private:
  public:
    Printer2(std::ostream &o) : Printer(o) {}

    void print(Type &t)
    {
      stream << "Printer2::type: " << t.z << std::endl;
    }

};


int main(int argc, char **argv)
{
  Printer p(std::cerr);
  p << 'H' << 'e' << 'l' << endl << 'l' << 'o' << 3 << std::endl;
  p << 'H' << 'e' << 'l' << std::endl << 'l' << 'o' << 3 << std::endl;
  Type t('S');
  Base *b = &t;
  p << 'H' << 'e' << 'l' << *b << 'l' << 'o' << 3 << std::endl;
  Printer2 p2(std::cerr);
  p2 << 'A' << *b << std::endl;
  return 0;
}


