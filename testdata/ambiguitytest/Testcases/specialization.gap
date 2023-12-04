// Please see the README.txt for more details.


signature test (alphabet, answer) {
	answer orig_f0 (void);
	answer orig_f1 (answer, answer);
	answer orig_f1_ (answer, answer);
	answer orig_f2 (alphabet, alphabet, alphabet);
	answer orig_f3 (alphabet, answer, alphabet);
	answer orig_f4 (alphabet, answer);
	answer orig_f5 (alphabet);
	choice [answer] h ([answer]);
}


algebra test1Alg implements test (alphabet = char, answer = string) {
	string orig_f0 (void) {
		string res;
		return res;
	}
	string orig_f1 (string str1, string str2) {
		string res;
		append (res, str1);
		append (res, str2);
		return res;
	}
	string orig_f1_ (string str1, string str2) {
		string res;
		append (res, str2);
		append (res, str1);
		return res;
	}
	string orig_f2 (char ch1, char ch2, char ch3) {
		string res;
		append (res, "[]");
		return res;
	}
	string orig_f3 (char ch1, string str1, char ch2) {
		string res;
		append (res, "[");
		append (res, str1);
		append (res, "]");
		return res;
	}
	string orig_f4 (char ch1, string str1) {
		return str1;
	}
	string orig_f5 (char ch1) {
		string res;
		return res;
	}
	choice [string] h ([string]l) {
		return unique(l);
	}
}



algebra cnt auto count;



grammar test2 uses test (axiom = rule1) {
	rule1 =	orig_f1 (rule2, rule1)		|
			orig_f2 (CHAR, CHAR, CHAR)	# h;
	rule2 =	orig_f3 (CHAR, rule1, CHAR)	|
			orig_f5 (CHAR)				# h;
}



grammar test3 uses test (axiom = rule1) {
	rule1 =	orig_f1 (rule2, rule1)		|
			orig_f2 (CHAR, CHAR, CHAR)	# h;
	rule2 =	orig_f3 (CHAR, rule1, CHAR)	|
			orig_f4 (CHAR, rule3)		# h;
	rule3 =	orig_f2 (CHAR, CHAR, CHAR)	|
			orig_f5 (CHAR)				# h;
}



grammar test4 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_f1 (orig_rule3, orig_rule1)	|
					orig_f3 (CHAR, orig_rule2, CHAR)	# h;
	orig_rule2 =	orig_f3 (CHAR, orig_rule1, CHAR)	|
					orig_f4 (CHAR, orig_rule3)			# h;
	orig_rule3 =	orig_f5 (CHAR)						# h;
}



grammar test5 uses test (axiom = rule1) {
	rule1 =	orig_f1 (rule2, rule1)		|
			orig_f2 (CHAR, CHAR, CHAR)	# h;
	rule2 =	orig_f3 (CHAR, rule1, CHAR)	|
			orig_f4 (CHAR, rule3)		# h;
	rule3 =	orig_f2 (CHAR, CHAR, CHAR)	# h;
}



grammar test6 uses test (axiom = rule1) {
	rule1 =	orig_f4 (CHAR, rule1)		|
			orig_f3 (CHAR, rule2, CHAR)	|
			orig_f5 (CHAR)				# h;
	rule2 =	orig_f3 (CHAR, rule1, CHAR)	# h;
}



grammar test7 uses test (axiom = rule1) {
	rule1 =	orig_f4 (CHAR, rule2)		|
			orig_f1 (rule2, rule1)		|
			orig_f3 (CHAR, rule3, CHAR)  # h;
	rule2 =	orig_f3 (CHAR, rule1, CHAR)	|
			orig_f4 (CHAR, rule3)		|
			orig_f4 (CHAR, rule4)		# h;
	rule3 =	orig_f4 (CHAR, rule1)		|
			orig_f5 (CHAR)				# h;
	rule4 = orig_f4 (CHAR, rule5)		|
			orig_f3 (CHAR, rule1, CHAR)  #h;
	rule5 = orig_f4 (CHAR, rule2)		|
			orig_f2 (CHAR, CHAR, CHAR)	#h;
}



grammar test8 uses test (axiom = rule1) {
	rule1 =	orig_f1 (rule3, rule1)		|
			orig_f3 (CHAR, rule2, CHAR)	|
			orig_f5 (CHAR)				# h;
	rule2 =	orig_f3 (CHAR, rule1, CHAR)	|
			orig_f4 (CHAR, rule3)		|
			orig_f1 (rule3, rule1) 		# h;
	rule3 =	orig_f5 (CHAR)				# h;
}



// That is grammar 'small' from test33, ish.
// Modification: the EMPTY parser has been replaced
// by the CHAR parser, which results in connection
// with the algebra in the same string grammar.
grammar test10 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_rule2							# h;
	orig_rule2 =	orig_f4 (CHAR, orig_rule2)			|
					orig_f1 (orig_rule3, orig_rule2)	|
					orig_f5 (CHAR)						# h;
	orig_rule3 =	orig_f3 (CHAR, orig_rule2, CHAR)	# h;
}



// Next one is grammar 'medium' from test33.
grammar test11 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_rule2							# h;
	orig_rule2 =	orig_f4 (CHAR, orig_rule2)			|
					orig_f1 (orig_rule3, orig_rule2)	|
					orig_f5 (CHAR)						# h;
	orig_rule3 =	orig_rule4 | orig_rule5 | orig_rule6	# h;
	orig_rule4 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
	orig_rule5 =	orig_f4 (CHAR, orig_rule3)			# h;
	orig_rule6 =	orig_f3 (CHAR, orig_rule2, CHAR)	# h;
}



// Modification of grammar test11 in orig_rule2:
// the first application of orig_f4 points directly
// to orig_rule3 instead of orig_rule2. This tests
// the detection of cycles. orig_rule2 should not
// be part of a cycle, although it points into a
// cycle.
grammar test12 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_rule2							# h;
	orig_rule2 =	orig_f4 (CHAR, orig_rule3)			|
					orig_f1 (orig_rule3, orig_rule2)	|
					orig_f5 (CHAR)						# h;
	orig_rule3 =	orig_rule4 | orig_rule5 | orig_rule6	# h;
	orig_rule4 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
	orig_rule5 =	orig_f4 (CHAR, orig_rule3)			# h;
	orig_rule6 =	orig_f3 (CHAR, orig_rule2, CHAR)	# h;
}



// Modified test11, where orig_rule2 now has a cycle which
// consists of a non-terminal that has no algebra function
// applied to it, and as a second step an algebra function
// applied to the parser parts.
grammar test13 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_rule2							# h;
	orig_rule2 =	orig_rule3							|
					orig_f1 (orig_rule4, orig_rule2)	|
					orig_f5 (CHAR)						# h;
	orig_rule3 =	orig_f4 (CHAR, orig_rule2)			# h;
	orig_rule4 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
}



// Modified test12, contains one cycle over the rules
// {orig_rule2, orig_rule3, orig_rule5}, but has two alternatives
// in orig_rule2 pointing to orig_rule3. The interesting question
// is, if we need two differently named copies of those rules
// involved in the cycle, one for each of both pointer going out
// from orig_rule2.
grammar test14 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_rule2							# h;
	orig_rule2 =	orig_f4 (CHAR, orig_rule3)			|
					orig_f1 (orig_rule3, orig_rule4)	|
					orig_f5 (CHAR)						# h;
	orig_rule3 =	orig_rule4 | orig_rule5 | orig_rule6	# h;
	orig_rule4 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
	orig_rule5 =	orig_f4 (CHAR, orig_rule3)			# h;
	orig_rule6 =	orig_f3 (CHAR, orig_rule2, CHAR)	# h;
}



// Modified test14, now with an alternative in orig_rule2 which
// is part of the cycle, but has also orig_rule2 as a second
// symbol in the sequence (i.g. 'orig_f1 (orig_rule3, orig_rule2)').
grammar test15 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_rule2							# h;
	orig_rule2 =	orig_f4 (CHAR, orig_rule3)			|
					orig_f1 (orig_rule3, orig_rule2)	|
					orig_f5 (CHAR)						# h;
	orig_rule3 =	orig_rule4 | orig_rule5 | orig_rule6	# h;
	orig_rule4 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
	orig_rule5 =	orig_f4 (CHAR, orig_rule3)			# h;
	orig_rule6 =	orig_f3 (CHAR, orig_rule2, CHAR)	# h;
}



// A new version based on Test 15, whose main looping part
// orig_rule3 has been replaced by two alternatives which
// consists first of a direct loop, and second of a sequence
// which is not reducible. This should test if the minimization
// of the new grammar does not include the recursive part
// while still producing the irreducible part but without
// any annotations for the hidden non-terminal.
grammar test16 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_rule2							# h;
	orig_rule2 =	orig_f4 (CHAR, orig_rule3)			|
					orig_f1 (orig_rule3, orig_rule2)	|
					orig_f5 (CHAR)						# h;
	orig_rule3 =	orig_rule4							|
					orig_rule5							|
					orig_f1 (orig_rule5, orig_rule4)	# h;
	orig_rule4 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
	orig_rule5 =	orig_f4 (CHAR, orig_rule3)			# h;
}



// The same as test16 but with two alternatives of orig_rule3
// swapped in their order.
grammar test17 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_rule2							# h;
	orig_rule2 =	orig_f4 (CHAR, orig_rule3)			|
					orig_f1 (orig_rule3, orig_rule2)	|
					orig_f5 (CHAR)						# h;
	orig_rule3 =	orig_f1 (orig_rule5, orig_rule4)	|
					orig_rule4							|
					orig_rule5							# h;
	orig_rule4 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
	orig_rule5 =	orig_f4 (CHAR, orig_rule3)			# h;
}



// A Grammar that test if a cyclic use and a strict use of
// the same non-terminal in the same production is handled
// correctly.
grammar test18 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_f4 (CHAR, orig_rule2)			|
					orig_f1 (orig_rule2, orig_rule3)	# h;
	orig_rule2 =	orig_f4 (CHAR, orig_rule1)			|
					orig_f2 (CHAR, CHAR, CHAR)			# h;
	orig_rule3 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
}



// A Grammar that tests the mapping of original algebra function
// parameter positions to CFG parameter positions. For this test
// we use the special 'orig_f1_' algebra function which simply
// switches the order its parameters.
grammar test19 uses test (axiom = orig_rule1) {
	orig_rule1 =	orig_f4 (CHAR, orig_rule2)			|
					orig_f1_ (orig_rule2, orig_rule1)	|
					orig_f2 (CHAR, CHAR, CHAR)			# h;
	orig_rule2 =	orig_f2 (CHAR, CHAR, CHAR)			# h;
}



instance test2Inst = test2 (test1Alg);
instance test3Inst = test3 (test1Alg);
instance test4Inst = test4 (test1Alg);
instance test5Inst = test5 (test1Alg);
instance test6Inst = test6 (test1Alg);
instance test7Inst = test7 (test1Alg);
instance test10Inst = test10 (test1Alg);
instance test11Inst = test11 (test1Alg);
instance test11InstCnt = test11 (test1Alg * cnt);
instance test12Inst = test12 (test1Alg);
instance test13Inst = test13 (test1Alg);
instance test14Inst = test14 (test1Alg);
instance test15Inst = test15 (test1Alg);
instance test16Inst = test16 (test1Alg);
instance test17Inst = test17 (test1Alg);
instance test18Inst = test18 (test1Alg);
instance test19Inst = test19 (test1Alg);
instance test19InstCnt = test19 (test1Alg * cnt);

