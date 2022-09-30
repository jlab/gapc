/*
# Calif El Mamun's Caravan
<img src="http://www.tabsir.net/images/Kadhimiya.gif" style="display: block; margin-left: auto; margin-right: auto; width: 10%;" />

Back in the old days, around the year 800, [Calif El Mamun of Bagdad](https://en.wikipedia.org/wiki/Al-Ma%27mun) planned to take his two sons on their first [hadj to Mekka](https://en.wikipedia.org/wiki/Hajj) ...
El Mamun called the camel dealer to negotiate about the required resources.

After a long day of bargaining, the bill was written in the sand.
<img src="static/Resources/elmamun_01.png" style="display: block; margin-left: auto; margin-right: auto; width: 50%;" />
(1 Calif, 2 sons, 3 baggage camels for each one, costing 4 tubes of oil, plus one riding camel for each one, costing 5 tubes of oil)
Computation was rather slow in those days ...

There came the evening prayers, and there came the wind ...
<img src="static/Resources/elmamun_02.png" style="display: block; margin-left: auto; margin-right: auto; width: 50%;" />
A mystery! Allah's hand had erased the parentheses, but left untouched the rest of the formula.

The dealer was eager to redraw the parentheses:
<img src="static/Resources/elmamun_03.png" style="display: block; margin-left: auto; margin-right: auto; width: 50%;" />
He even claimed that now the formula showed beautiful symmetry, pleasing the eye of God.
El Mamun was not good at figures, but he knew everything about camel dealers.
He felt suspicious.

El Mamun called for the mathematician. [Al Chwarizmi](https://en.wikipedia.org/wiki/Muhammad_ibn_Musa_al-Khwarizmi), famous already in those days, studied the formula carefully, before he spoke.
<img src="static/Resources/elmamun_04.png" style="display: block; margin-left: auto; margin-right: auto; width: 50%;" />
"Some possible answers are

  * `(1 + 2) * (3 * (4 + 5)) = 81`
  * `1 + ((2 * (3 * 4)) + 5) = 30`
  * `(1 + 2) * ((3 * 4) + 5) = 51`

which are all equal in the eyes of God."
Apparently, Al Chwarizmi was a wise as well as a cautious man.

El Mamun's contemplated this answer over night, and in the next morning, he ruled as follows:

  * The dealer should be buried in the sand, next to his formula `(1 + 2) * 3 * (4 + 5)`.
  * Someone had to take care of the dealer's camels, and the Calif volunteered to collect them in his stables.
  * Al Chwarizmi was awarded a research grant (51 tubes of oil) to study the  optimal placement of parentheses, both from a buyer's or from a seller's perspective (depending on which side of the counter the Calif might find himself).
  * Until this problem was solved, there should hold the provisional rule: "`*` takes  priority over `+`", wherever parentheses were lacking in some formula.

Some results from this episode can still be observed today!
<img src="static/Resources/elmamun_05.png" style="display: block; margin-left: auto; margin-right: auto; width: 50%;" />

  * El Mamun became very, very rich, and  his name gave rise to the word "mammon" in many modern languages.
  * Studying hard, Al Chwarizmi became the father of algorithmics. He did not solve the problem, because [Dynamic Programming](https://en.wikipedia.org/wiki/Dynamic_programming) was only developed by [Richard Bellman](https://en.wikipedia.org/wiki/Richard_E._Bellman) in the 1950s.
  * As a consequence of the question being  unsettled, today `*` still takes priority over `+`.

*/

type Rope = extern

signature sig_elmamun(alphabet, answer) {
  answer number(int);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  answer minus(answer, alphabet, answer);
  answer heinz(answer, Rope, answer);
  answer nil(void);
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

algebra alg_pretty implements sig_elmamun(alphabet=char, answer=Rope) {
  Rope number(int value) {
    Rope res;
    append(res, value);
    return res;
  }
  Rope add(Rope left, char opSymbol, Rope right) {
    Rope res;
    append(res, '(');
    append(res, left);
    append(res, opSymbol);
    append(res, right);
    append(res, ')');
    return res;
  }
  Rope heinz(Rope left, Rope opSymbol, Rope right) {
    Rope res;
    append(res, '(');
    append(res, left);
    append(res, opSymbol);
    append(res, right);
    append(res, ')');
    return res;
  }
  Rope mult(Rope left, char opSymbol, Rope right) {
    Rope res;
    append(res, '(');
    append(res, left);
    append(res, opSymbol);
    append(res, right);
    append(res, ')');
    return res;
  }
  Rope minus(Rope left, char opSymbol, Rope right) {
    Rope res;
    append(res, '(');
    append(res, left);
    append(res, opSymbol);
    append(res, right);
    append(res, ')');
    return res;
  }
  Rope nil(void) {
    Rope res;
    return res;
  }
  choice [Rope] h([Rope] candidates) {
    return candidates;
  }
}

algebra alg_buyer implements sig_elmamun(alphabet=char, answer=int) {
  int number(int value) {
    return value;
  }
  int add(int left, char opSymbol, int right) {
    return left + right;
  }
  int heinz(int left, Rope opSymbol, int right) {
    return left + right;
  }
  int mult(int left, char opSymbol, int right) {
    return left * right;
  }
  int minus(int left, char opSymbol, int right) {
    return left - right;
  }
  int nil(void) {
    return 0;
  }
  choice [int] h([int] candidates) {
    return list(minimum(candidates));
  }
}
algebra alg_seller extends alg_buyer {
  choice [int] h([int] candidates) {
    return list(maximum(candidates));
  }
}

algebra alg_time implements sig_elmamun(alphabet=char, answer=int) {
  int number(int value) {
    return 0;
  }
  int add(int left, char opSymbol, int right) {
    if (left > right) {
      return left + 2;
    } else {
      return right + 2;
    }
  }
  int heinz(int left, Rope opSymbol, int right) {
    if (left > right) {
      return left + 2;
    } else {
      return right + 2;
    }
  }
  int mult(int left, char opSymbol, int right) {
    if (left > right) {
      return left + 5;
    } else {
      return right + 5;
    }
  }
  int minus(int left, char opSymbol, int right) {
    if (left > right) {
      return left + 3;
    } else {
      return right + 3;
    }
  }
  int nil(void) {
    return 0;
  }
  choice [int] h([int] candidates) {
    return list(minimum(candidates));
  }
}

grammar gra_elmamun uses sig_elmamun(axiom = formula) {
  formula = number(INT)
	  | add(formula, CHAR, formula)
	  | mult(formula, CHAR('*'), formula)
	  | heinz(formula, ROPE("manfred"), formula)
	  | nil(EMPTY)
	  # h;
}

instance pp = gra_elmamun(alg_pretty);
instance enum = gra_elmamun(alg_enum);
instance count = gra_elmamun(alg_count);
instance sellerpp = gra_elmamun(alg_seller * alg_pretty);
instance buyerpp = gra_elmamun(alg_buyer * alg_pretty);
instance ppbuyer = gra_elmamun(alg_pretty * alg_buyer);
instance timepp = gra_elmamun(alg_time * alg_pretty);

// example input:
// "1+2*3*4+5"
