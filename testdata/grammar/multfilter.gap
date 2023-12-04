input <raw,raw>

signature Nuss ( alphabet , answer ) {

	  answer nil ( <void,void> );
	  answer right (answer , <Subsequence,Subsequence> );
	  answer pair ( <Subsequence,Subsequence> , answer , <Subsequence,Subsequence> );
	  answer split (answer , answer );
	  choice [ answer ] h([ answer ]);
}

algebra bpmax implements Nuss ( alphabet = char , answer = int )
{
	int nil( <void,void> ) { return 0; }
	int right ( int a, <Subsequence c,Subsequence d>) { return a; }
	int pair ( <Subsequence c,Subsequence d>, int m, <Subsequence e,Subsequence f>) { return m + 1; }
	int split ( int l, int r) { return l + r; }
	choice [int] h([ int ] l) { return list ( maximum (l)); }
}

algebra pretty implements Nuss ( alphabet = char , answer = string )
{
	string nil ( <void,void> )
	{
		string r;
		return r;
	}
	string right ( string a, <Subsequence c,Subsequence d>)
	{
		string r;
		append (r, a);
		append (r, '.');
		return r;
	}
	string pair ( <Subsequence c,Subsequence d>, string m, <Subsequence e,Subsequence f>)
	{
		string r;
		append (r, '(');
		append (r, m);
		append (r, ')');
		return r;
	}
	string split ( string l, string r)
	{
		string t;
		append (t, l);
		append (t, r);
		return t;
	}
	choice [ string ] h([ string ] l)
	{
	return l;
	}
}

grammar nussinov uses Nuss ( axiom = struct ) {
	struct = nil( <EMPTY,EMPTY> ) |
	right ( struct , <BASE,BASE> ) |
	split ( struct , bp) # h;
	
	bp = pair  ( <BASE,BASE> , struct , <BASE,BASE> ) with <char_basepairing,char_basepairing> # h;
}

instance bpmax = nussinov ( bpmax ) ;
instance bpmaxpp = nussinov ( bpmax * pretty ) ;
