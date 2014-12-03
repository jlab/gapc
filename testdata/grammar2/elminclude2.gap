
algebra buyer implements Bill(alphabet = char, answer = int) {

  int f(int i) { return i; }

  int add(int i, char c, int j)
  {
    return i + j;
  }

include "elminclude3.gap"


  choice [int] h([int] i)
  {
    return list(minimum(i));
  }
}
