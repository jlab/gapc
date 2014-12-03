#ifndef PKENERGY_HH
#define PKENERGY_HH


static const int npp = 10; //penalty for an unpaired base inside a pseudoknot
static const int mlinit = 380; //initialization cost for opening a new multiloop
static const int pkinit = 900; //initialization cost for opening a new pseudoknot
static const int pkmlinit = 600; //additional penalty for a pseudoknot inside front, middle or back of an existing outer pseudoknot
static const int pkissinit = 1200; //initialization cost for opening a new kissing hairpin

template<typename alphabet, typename pos_type, typename T>
inline bool midsize(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j,
    int a, int l)
{
  return abs(a) == l;
}


template <typename T, typename Size>
struct PkAlph {
  enum { char_width = 4 };
  public:
  void operator()(T &t, char x, Size l) const
  {
    switch (x) {
      case '[' :
        t |= T(1) << l-3;
        break;
      case ']' :
        t |= T(2) << l-3;
        break;
      case '_' :
        t |= T(3) << l-3;
        break;
      case '{' :
        t |= T(4) << l-3;
        break;
      case '}' :
        t |= T(5) << l-3;
        break;
      case '<' :
        t |= T(6) << l-3;
        break;
      case '>' :
        t |= T(7) << l-3;
        break;
      default: assert(false);
    }
  }
  char to_char(T &t, Size i) const
  {
    switch (t >> i & T(15)) {
      case 1 : return '[';
      case 2 : return ']';
      case 3 : return '_';
      case 4 : return '{';
      case 5 : return '}';
      case 6 : return '<';
      case 7 : return '>';
      default: assert(false); return 0;
    }
  }
};

typedef Fiber<size_t, unsigned char, PkAlph<size_t, unsigned char> > pkshape_t;

typedef pkshape_t myShape;

#endif
