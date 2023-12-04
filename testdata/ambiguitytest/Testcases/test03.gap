// Demonstrates a simple recursion in the grammar. The empty
// input string is also accepted


signature test (alphabet, answer) {
	answer nil (void);
	answer cons (alphabet, answer);
	choice [answer] h ([answer]);
}


algebra testAlg1 implements test (alphabet = char, answer = string) {
	string nil (void) {
		string str;
		return str;
	}
	string cons (char c, string str) {
		string res;
		append (res, c);
		append (res, str);
		return res;
	}
	choice [string] h ([string]l) {
		return l;
	}
}


algebra testAlg2 implements test (alphabet = char, answer = string) {
	string nil (void) {
		string str;
		return str;
	}
	string cons (char c, string str) {
		string res;
		append (res, cons (c, str));
		append (res, str);
		return res;
	}
	choice [string] h ([string]l) {
		return l;
	}
}


grammar test uses test (axiom = sequence) {
	sequence =	nil (EMPTY)   |
				cons (CHAR, sequence);
}


instance testInst1 = test (testAlg1);
instance testInst2 = test (testAlg2);

