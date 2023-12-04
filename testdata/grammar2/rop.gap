
type Rope = extern

signature Bill(alphabet, answer) {
  answer f(Rope, Subsequence);
  choice [answer] h([answer]);
}

algebra pretty implements Bill(alphabet = char, answer = Rope)
{
  Rope f(Rope x, Subsequence a)
  {
     return x;
  }
  choice [Rope] h([Rope] i)
  {
    return i;
  }
}

grammar bill uses Bill (axiom=formula) {
  formula = f (CONST_ROPE("HELLO"), REGION) # h ;
}

instance pretty = bill (pretty);

