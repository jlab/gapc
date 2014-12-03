
input < raw, raw >

type spair = ( string first, string second )

signature Align(alphabet, answer) {
  answer match( < alphabet, alphabet >, answer);
  answer nil( <int, int> );
  choice [answer] h([answer]);
}

grammar affinelocsim uses Align(axiom = alignment)
{


  alignment = nil( < SEQ, SEQ> )   |
              match( < CHAR, CHAR >, alignment) # h ;


}

