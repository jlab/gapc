// Demonstrates an ambiguous grammar.


signature test (alphabet, answer) {
	answer nil (void);
	answer sngl (alphabet);
	answer seq (alphabet, answer);
	choice [answer] h ([answer]);
}


algebra testAlg1 implements test (alphabet = char, answer = string) {
	string nil (void) {
		string str;
		return str;
	}
	string sngl (char c) {
		string res;
		append (res, '.');
		return res;
	}
	string seq (char c, string str) {
		string res;
		append (res, '.');
		append (res, str);
		return res;
	}
	choice [string] h ([string]l) {
		return l;
	}
}


algebra cntAlg auto count;
algebra enumAlg auto enum;


grammar test uses test (axiom = sequence) {
	sequence =	nil (EMPTY) | sngl (CHAR) | seq (CHAR, sequence) # h;
}


instance testInst = test (testAlg1);
instance countInst = test (cntAlg);
