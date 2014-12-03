
input < raw, raw >


signature Align(alphabet, answer) {
  answer match( < alphabet, alphabet >, answer);
  answer nil( <int, int> );
  choice [answer] h([answer]);
}

grammar sankoff uses Align(axiom = sank)
{


  sank = match(<CHAR, CHAR>, sank) |
         ins(<EMPTY, CHAR>, sank) |
         del(<CHAR, EMPTY>, sank) |
         pmatch(<CHAR, CHAR>, sank, <CHAR, CHAR>, sank) |
         nil(<EMPTY, EMPTY>) # h ;

                // or use LOC ...


}

