type Rope = extern

type shapestring = Rope

signature Algebra(alphabet,answer) {
	answer sadd(alphabet, answer);
	answer cadd(answer, answer);
	answer nil(void);
	answer combine(answer, answer);
	answer ssadd(Subsequence, answer);
	answer addss(answer, Subsequence);
	answer hairpin(alphabet, alphabet, Subsequence, alphabet, alphabet);
	answer stack(alphabet, answer, alphabet);
	answer bulgeleft(alphabet, alphabet, Subsequence, answer, alphabet, alphabet);
	answer bulgeright(alphabet, alphabet, answer, Subsequence, alphabet, alphabet);
	answer iloop(alphabet, alphabet, Subsequence, answer, Subsequence, alphabet, alphabet);
	answer multiloop(alphabet, alphabet, answer, alphabet, alphabet);
	choice [answer] h([answer]);
}

algebra enum auto enum ;
algebra count auto count ;

algebra shape5 implements Algebra(alphabet = char, answer = shapestring) {
  shapestring sadd(char b, shapestring x) {
    if (x == "") {
      return "_" + x;
    } else {
      return x;
    }
  }
  
  shapestring cadd(shapestring x, shapestring y) {
    if (y == "_") {
      return x;
    } else {
      return x + y;
    }
  }
  
  shapestring nil(void) {
    return "";
  }
  
  shapestring combine(shapestring x, shapestring y) {
    return x + y;
  }
  
  shapestring ssadd(Subsequence r, shapestring x) {
    return x;
  }
  
  shapestring addss(shapestring x, Subsequence r) {
    return x;
  }
  
  shapestring hairpin(char llb, char lb, Subsequence u, char rb, char rrb) {
    return "[]";
  }
  
  shapestring stack(char lb, shapestring x, char rb) {
    return x;
  }
  
  shapestring bulgeleft(char llb, char lb, Subsequence u, shapestring x, char rb, char rrb) {
    return x;
  }
  
  shapestring bulgeright(char llb, char lb, shapestring x, Subsequence u, char rb, char rrb) {
    return x;
  }
  
  shapestring iloop(char llb, char lb, Subsequence lu, shapestring x, Subsequence ru, char rb, char rrb) {
    return x;
  }
  
  shapestring multiloop(char llb, char lb, shapestring x, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  choice [shapestring] h([shapestring] i) {
    return i;
  }
}

algebra shape4 implements Algebra(alphabet = char, answer = shapestring) {
  shapestring sadd(char b, shapestring x) {
    if (x == "") {
      return "_" + x;
    } else {
      return x;
    }
  }
  
  shapestring cadd(shapestring x, shapestring y) {
    if (y == "_") {
      return x;
    } else {
      return x + y;
    }
  }
  
  shapestring nil(void) {
    return "";
  }
  
  shapestring combine(shapestring x, shapestring y) {
    return x + y;
  }
  
  shapestring ssadd(Subsequence r, shapestring x) {
    return x;
  }
  
  shapestring addss(shapestring x, Subsequence r) {
    return x;
  }
  
  shapestring hairpin(char llb, char lb, Subsequence u, char rb, char rrb) {
    return "[]";
  }
  
  shapestring stack(char lb, shapestring x, char rb) {
    return x;
  }
  
  shapestring bulgeleft(char llb, char lb, Subsequence u, shapestring x, char rb, char rrb) {
    return x;
  }
  
  shapestring bulgeright(char llb, char lb, shapestring x, Subsequence u, char rb, char rrb) {
    return x;
  }
  
  shapestring iloop(char llb, char lb, Subsequence lu, shapestring x, Subsequence ru, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  shapestring multiloop(char llb, char lb, shapestring x, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  choice [shapestring] h([shapestring] i) {
    return i;
  }
}

algebra shape3 implements Algebra(alphabet = char, answer = shapestring) {
  shapestring sadd(char b, shapestring x) {
    if (x == "") {
      return "_" + x;
    } else {
      return x;
    }
  }
  
  shapestring cadd(shapestring x, shapestring y) {
    if (y == "_") {
      return x;
    } else {
      return x + y;
    }
  }
  
  shapestring nil(void) {
    return "";
  }
  
  shapestring combine(shapestring x, shapestring y) {
    return x + y;
  }
  
  shapestring ssadd(Subsequence r, shapestring x) {
    return x;
  }
  
  shapestring addss(shapestring x, Subsequence r) {
    return x;
  }
  
  shapestring hairpin(char llb, char lb, Subsequence u, char rb, char rrb) {
    return "[]";
  }
  
  shapestring stack(char lb, shapestring x, char rb) {
    return x;
  }
  
  shapestring bulgeleft(char llb, char lb, Subsequence u, shapestring x, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  shapestring bulgeright(char llb, char lb, shapestring x, Subsequence u, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  shapestring iloop(char llb, char lb, Subsequence lu, shapestring x, Subsequence ru, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  shapestring multiloop(char llb, char lb, shapestring x, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  choice [shapestring] h([shapestring] i) {
    return i;
  }
}

algebra shape2 implements Algebra(alphabet = char, answer = shapestring) {
  shapestring sadd(char b, shapestring x) {
    if (x == "") {
      return "_" + x;
    } else {
      return x;
    }
  }
  
  shapestring cadd(shapestring x, shapestring y) {
    if (y == "_") {
      return x;
    } else {
      return x + y;
    }
  }
  
  shapestring nil(void) {
    return "";
  }
  
  shapestring combine(shapestring x, shapestring y) {
    return x + y;
  }
  
  shapestring ssadd(Subsequence r, shapestring x) {
    return x;
  }
  
  shapestring addss(shapestring x, Subsequence r) {
    return x;
  }
  
  shapestring hairpin(char llb, char lb, Subsequence u, char rb, char rrb) {
    return "[]";
  }
  
  shapestring stack(char lb, shapestring x, char rb) {
    return x;
  }
  
  shapestring bulgeleft(char llb, char lb, Subsequence u, shapestring x, char rb, char rrb) {
    return "[_" + x + "]";
  }
  
  shapestring bulgeright(char llb, char lb, shapestring x, Subsequence u, char rb, char rrb) {
    return "[" + x + "_]";
  }
  
  shapestring iloop(char llb, char lb, Subsequence lu, shapestring x, Subsequence ru, char rb, char rrb) {
    return "[_" + x + "_]";
  }
  
  shapestring multiloop(char llb, char lb, shapestring x, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  choice [shapestring] h([shapestring] i) {
    return i;
  }
}

algebra shape1 implements Algebra(alphabet = char, answer = shapestring) {
  shapestring sadd(char b, shapestring x) {
    if (x != "" && front(x) == '_') {
      return x;
    } else {
      return "_" + x;
    }
  }
  
  shapestring cadd(shapestring x, shapestring y) {
    return x + y;
  }
  
  shapestring nil(void) {
    return "";
  }
  
  shapestring combine(shapestring x, shapestring y) {
    return x + y;
  }
  
  shapestring ssadd(Subsequence r, shapestring x) {
    return "_" + x;
  }
  
  shapestring addss(shapestring x, Subsequence r) {
    return x + "_";
  }
  
  shapestring hairpin(char llb, char lb, Subsequence u, char rb, char rrb) {
    return "[]";
  }
  
  shapestring stack(char lb, shapestring x, char rb) {
    return x;
  }
  
  shapestring bulgeleft(char llb, char lb, Subsequence u, shapestring x, char rb, char rrb) {
    return "[_" + x + "]";
  }
  
  shapestring bulgeright(char llb, char lb, shapestring x, Subsequence u, char rb, char rrb) {
    return "[" + x + "_]";
  }
  
  shapestring iloop(char llb, char lb, Subsequence lu, shapestring x, Subsequence ru, char rb, char rrb) {
    return "[_" + x + "_]";
  }
  
  shapestring multiloop(char llb, char lb, shapestring x, char rb, char rrb) {
    return "[" + x + "]";
  }
  
  choice [shapestring] h([shapestring] i) {
    return i;
  }
}

grammar readStructure uses Algebra(axiom = struct) {
	struct = sadd(CHAR('.'), struct) |
		 cadd(closed, struct) |
		 nil(EMPTY) # h;
	
	closed = hairpin    (CHAR('('), CHAR('('),                                             REGION with minsize(3) with onlychar('.'),          CHAR(')'), CHAR(')')) |
		 stack      (           CHAR('('),                                             closed,                                             CHAR(')')           ) |
		 bulgeleft  (CHAR('('), CHAR('('), REGION with onlychar('.'),                  closed,                                             CHAR(')'), CHAR(')')) |
		 bulgeright (CHAR('('), CHAR('('),                                             closed, REGION with onlychar('.'),                  CHAR(')'), CHAR(')')) |
		 iloop      (CHAR('('), CHAR('('), REGION with maxsize(30) with onlychar('.'), closed, REGION with maxsize(30) with onlychar('.'), CHAR(')'), CHAR(')')) |
		 multiloop  (CHAR('('), CHAR('('),                                             ml_components,                                             CHAR(')'), CHAR(')')) # h;

	ml_components = combine(block, comps) # h;
	
	block = closed |
		ssadd(REGION with onlychar('.'), closed) # h;
	
	comps = combine(block, comps) |
		block |
		addss(block, REGION with onlychar('.')) # h;
}

instance enum = readStructure (enum);

instance shape5 = readStructure (shape5);
instance shape4 = readStructure (shape4);
instance shape3 = readStructure (shape3);
instance shape2 = readStructure (shape2);
instance shape1 = readStructure (shape1);
