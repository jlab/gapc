input <raw,raw>

signature MiniSat(alphabet, answer){
  answer cfr(answer,answer);
  answer ifr(answer,answer);
  answer unit(alphabet);
  choice [answer] h([answer]);
  
  answer lrdup(<answer,answer>,answer);
  answer end(<void,void>);
}

algebra count auto count;

// test for multi-track track_pos initialization (r -> r_0 and r_1)

grammar m1 uses MiniSat (axiom = ali) {
  ali = 
        lrdup(<gStart,gStart>,ali) |
        end(<EMPTY,EMPTY>) #h;

  gStart = ifr(unit(CHAR),g) |
           rStart   # h;
  rStart = 
           cfr(r,r) #h;

  g = ifr(unit(CHAR),g) |
      r        # h;
  r = unit(CHAR)	       |
      cfr(r,r) # h;

}

instance cnt = m1(count);
