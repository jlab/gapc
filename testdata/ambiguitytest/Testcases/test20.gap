// Please see the README.txt surplied with the example
// for more information about this test case.


signature test (alphabet, answer) {
	answer nil (Subsequence);
	answer right (answer, Subsequence);
	answer left (Subsequence, answer);
	answer pair (Subsequence, answer, Subsequence);
	answer split (answer, answer);
	choice [answer] h ([answer]);
}


algebra testAlg1 implements test (alphabet = Subsequence, answer = string) {
	string nil (Subsequence s) {
		string str;
		return str;
	}
	string right (string str, Subsequence c) {
		string res;
		append (res, str);
		append (res, c); 
		return res;
	}
	string left (Subsequence c, string str) {
		string res;
		append (res, '.'); 
		append (res, str);
		return res;
	}
	string pair (Subsequence c1, string str, Subsequence c2) {
		string res;
		append (res, '('); 
		append (res, str);
		append (res, ')'); 
		return res;
	}
	string split (string str1, string str2) {
		string res;
		append (res, str1);
		append (res, str2);
		return res;
	}
	choice [string] h ([string]l) {
		return l;
	}
}


algebra cntAlg auto count;
algebra enumAlg auto enum;


grammar test uses test (axiom = sequence) {
	sequence =	nil (LOC)		|
				non_nil_seq		# h;
	non_nil_seq = 	right (sequence, BASE)		|
					left (BASE, sequence)		|
					pair (BASE, pair (BASE, pair (BASE, pair (BASE, pair (BASE, pair (BASE, region_x, BASE) with basepairing, BASE) with basepairing, BASE) with basepairing, BASE) with basepairing, BASE) with basepairing, BASE) with basepairing	|
					split (non_nil_seq, non_nil_seq) # h;
	region_x = pair (REGION with minsize(3), non_nil_seq, REGION with minsize(3)) # h;
}


instance testInst = test (testAlg1);
instance countInst = test (cntAlg);
