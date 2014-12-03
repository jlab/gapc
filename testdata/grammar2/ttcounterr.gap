input <raw, raw>

signature Signature(alphabet, answer) {
	answer nil(<void, void>);
	answer del(<alphabet, void>, answer);
	answer ins(answer, <alphabet, void>);
	answer match(<alphabet, alphabet>, answer);
	choice [answer] h([answer]);
}

algebra count auto count ;
algebra enum auto enum ;


grammar Ali uses Signature (axiom = alignment) {
	alignment = nil(<EMPTY, EMPTY>)             |
	            del(<CHAR, EMPTY>, alignment)   |
	            ins(<EMPTY, CHAR>, alignment)   |
	            match(<CHAR, CHAR>, alignment)  # h ;
}

instance test = Ali(count);
