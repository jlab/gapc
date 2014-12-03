// Demonstrates a simple recursion in the grammar. The empty
// input string is also accepted


signature test (alphabet, answer) {
	answer nil (void);
	answer right (answer, Subsequence);
	answer left (Subsequence, answer);
	answer pair (Subsequence, answer, Subsequence);
	choice [answer] h ([answer]);
}


algebra testAlg1 implements test (alphabet = char, answer = string) {
	string nil (void) {
		string str;
		return str;
	}
	string right (string str, Subsequence c) {
		string res;
		string a;
		append(a, "Kurt", 3);
		append(a, c);
		string b;
		append(b, 't');
		append(res, b);
		append(res, a);
		return res;
	}
	string left (Subsequence c, string str) {
		string res;
		append (res, c);
		append (res, str);
		return res;
	}
	string pair (Subsequence c1, string str, Subsequence c2) {
		string res;
		append (res, "((");
		append (res, str);
		append (res, ')');
		return res;
	}
	choice [string] h ([string]l) {
		return l;
	}
}


algebra cntAlg auto count;
algebra enumAlg auto enum;


grammar test uses test (axiom = sequence) {
	sequence =	nil (EMPTY)		|
				non_nil_seq		# h;
	non_nil_seq = 	right (sequence, REGION with minsize (4))			|
					left (REGION with maxsize (3) with minsize (1), sequence)			|
					pair (BASE, sequence, BASE)	with char_basepairing	# h;
}


instance testInst = test (testAlg1);
instance countInst = test (cntAlg);

