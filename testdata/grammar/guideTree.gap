type Rope = extern

signature gtAlgebra(alphabet, answer) {
	answer MatP(alphabet, answer, alphabet);
	answer MatL(alphabet, answer);
	answer MatR(answer, alphabet);
	answer Bif(answer, answer);
	answer End(void);
	answer Root(answer);
	answer BegL(answer);
	answer BegR(answer);
	choice [answer] h([answer]);
}

algebra count auto count;

algebra enum auto enum;

grammar guideTree uses gtAlgebra(axiom = start) {
	start = Root(s) # h;
	
	s = p | End(EMPTY) # h;
	
	p = MatP(CHAR('('), s, CHAR(')')) |
            MatL(CHAR('.'), s) |
            MatR(s, CHAR('.')) |
            Bif(BegL(p), BegR(p)) # h;
}

instance myEnum = guideTree(enum);
instance myCount = guideTree(count);
