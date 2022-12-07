type Rope = extern

signature sig_elmamun(alphabet, answer) {
  answer number(int);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  answer minus(answer, alphabet, answer);
  answer nil(void);
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

algebra alg_score implements sig_elmamun(alphabet=char, answer=float) {
  float number(int value) {
    return 1.0;
  }
  float add(float left, char opSymbol, float right) {
    return left * right * exp(2);
  }
  float heinz(float left, Rope opSymbol, float right) {
    return left * right;
  }
  float mult(float left, char opSymbol, float right) {
    return left * right * exp(3);
  }
  float minus(float left, char opSymbol, float right) {
    return left * right;
  }
  float nil(void) {
    return 1.0;
  }
  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
} 

algebra alg_hessians implements sig_elmamun(alphabet=char, answer=float) {
  float number(int value) {
    return 0.0;
  }
  float add(float left, char opSymbol, float right) {
    return left + right + 2.0;
  }
  float heinz(float left, Rope opSymbol, float right) {
    return left + right;
  }
  float mult(float left, char opSymbol, float right) {
    return left + right + 3.0;
  }
  float minus(float left, char opSymbol, float right) {
    return left + right;
  }
  float nil(void) {
    return 0.0;
  }
  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_hessians implements sig_elmamun(alphabet=char, answer=float) {
  float number(int value) {
    return 0.0;
  }
  float add(float left, char opSymbol, float right) {
    return left + right + 2.0;
  }
  float heinz(float left, Rope opSymbol, float right) {
    return left + right;
  }
  float mult(float left, char opSymbol, float right) {
    return left + right + 3.0;
  }
  float minus(float left, char opSymbol, float right) {
    return left + right;
  }
  float nil(void) {
    return 0.0;
  }
  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}


grammar gra_elmamun uses sig_elmamun(axiom = formula) {
  formula = number(INT)
	  | add(formula, CHAR('+'), formula)
	  | mult(formula, CHAR('*'), formula)
	  | nil(EMPTY)
	  # h;
}

instance firstD = gra_elmamun(alg_score);
instance bothD = gra_elmamun(alg_score * alg_hessians);
