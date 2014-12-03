
signature test (alphabet, answer) {
	answer nil (void);
	choice [answer] h ([answer]);
}


algebra testAlg1 implements test (alphabet = char, answer = string) {
	string nil (void) {
		string str;
		return str;
	}
	choice [string] h ([string]l) {
		return l;
	}
}


grammar test uses test (axiom = struct) {
	struct = nil (EMPTY) # h;
}


instance testInst = test (testAlg1);

