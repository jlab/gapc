// Please read the file README.txt for more details on this test.


signature test (alphabet, answer) {
	answer nil (void);
	answer right1 (answer, alphabet);
	answer right2 (answer, alphabet);
	answer left1 (alphabet, answer);
	answer left2 (alphabet, answer);
	answer pair (alphabet, answer, alphabet);
	answer split (answer, answer);
	choice [answer] h ([answer]);
}


algebra testAlg1 implements test (alphabet = char, answer = string) {
	string nil (void) {
		string str;
		return str;
	}
	// case 1: if-then
	string right1 (string str, char c) {
		string res;
		append (res, str);
		if (c == 'C') {
			append (res, '.');
		}
		return res;
	}
	// case 3: if-then(returned)
	string right2 (string str, char c) {
		string res;
		append (res, str);
		if (c == 'C') {
			append (res, '.');
			return '?';
		}
		return res;
	}
	// case 2: if-then-else
	string left1 (char c, string str) {
		string res;
		if (c == 'C') {
			append (res, 'C');
		}
		else {
			append (str, "str");
		}
		append (res, str);
		return res;
	}
	// case 4: if-then(return)-else
	string left2 (char c, string str) {
		string res;
		if (c == 'C') {
			append (res, 'C');
			return '!';
		}
		else {
			append (str, "str");
		}
		append (res, str);
		return res;
	}
	// case 5: if-then-else(return)
	string pair (char c1, string str, char c2) {
		string res;
		if (c1 == 'A') {
			append (res, '(');
		}
		else {
			append (res, str);
			append (res, ')');
			return res;
		}
		return res;
	}
	// case 6: if-then(return)-else(return)
	string split (string str1, string str2) {
		string res;
		if (str1 == "") {
			append (res, str1);
			return res;
		}
		else {
			append (res, str2);
			return "#";
		}
		//return res;
	}
	choice [string] h ([string]l) {
		return l;
	}
}


algebra cntAlg auto count;
algebra enumAlg auto enum;


grammar test uses test (axiom = sequence) {
	sequence =	nil				|
				right1 | right2	|
				left1 | left2	|
				pair 			|
				split			#h;
	nil = nil (EMPTY);
	right1 = right1 (sequence, CHAR);
	right2 = right2 (sequence, CHAR);
	left1 = left1 (CHAR, sequence);
	left2 = left2 (CHAR, sequence);
	pair = pair (CHAR, sequence, CHAR)	with char_basepairing;
	split = split (left1, left2);
}


instance testInst = test (testAlg1);
instance countInst = test (cntAlg);

