/*

EXAMPLE 1 5   2 5   3 40   4 10   5 20   6 1   7 19

HEADER
<h1>Optimal Binary Search Tree</h1>

<p> Given a set of keys and their access probabilities, the <a
target="_blank"
href="http://en.wikipedia.org/wiki/Binary_search_tree#Optimal_binary_search_trees">optimal
binary search tree algorithm</a> (<a target="_blank"
href="http://de.wikipedia.org/wiki/Bellman-Algorithmus">German</a>)
computes the binary tree with minimal mean access time.  Why is
this a <i>sequence</i> analysis problem at all?  Because in a
search tree, the order of leaves is fixed.  The yield string of
any search tree must hold the keys in sorted order, i.e. the
algorithm takes the <i>sequence</i> of keys and their access
probabilities as input.
</p>
<p>
With a dynamic programming approach, the minimal expected access time results from the optimization phase,
 the underlying optimal tree structure is derived via backtracing.
</p>
<p>
Computing the mean access time is done via the evaluation algebra
<tt>mean</tt>. The place-holder (sort) <tt>answer</tt> is mapped to
a tuple datatype,
where the first component of a result is the mean access time of the
candidate and the second component is sum of the probabilities of
the yield of the candidate tree.
</p>

HEADER

*/


type acc = ( float mean, float yield )

type alph = ( int key, float prob)

type Rope = extern


signature Tree(alphabet, answer, inp)
{
	answer br(answer, inp, answer);
	answer lf(inp);
	inp f(alphabet, alphabet);
	
	answer nil(void);
	choice [answer] h([answer]);
}


algebra mean implements Tree(alphabet = int, answer = acc, inp = float) {
	acc br(acc l, float x, acc r) {
		acc res;
		res.mean = l.mean + r.mean + l.yield + r.yield + x;
		res.yield = l.yield + r.yield + x;
		return res;
	}
	
	acc lf(float x) {
		acc res;
		res.mean = x;
		res.yield = x;
		return res;
	}
	
	acc nil(void) {
		acc res;
		res.mean = 0;
		res.yield = 0;
		return res;
	}
	
	float f(int key, int prob) {
		float x = prob;
		return x/100.0;
	}
	
	choice [acc] h([acc] l) {
		return list(minimum(l));
	}
}


algebra pretty implements Tree(alphabet = int, answer = Rope, inp = int) {
	Rope br(Rope l, int x, Rope r)
	{
		Rope res;
		append(res, '(');
		append(res, l);
		append(res, ')');
		append(res, x);
		append(res, '(');
		append(res, r);
		append(res, ')');
		return res;
	}
	
	Rope lf(int x)
	{
		Rope res;
		append(res, x);
		return res;
	}
	
	Rope nil(void)
	{
		Rope res;
		return res;
	}
	
	int f(int key, int prob)
	{
		return key;
	}
	
	choice [Rope] h([Rope] l)
	{
		return l;
	}
}


algebra tikz implements Tree(alphabet = int, answer = Rope, inp = int) {
	Rope br(Rope l, int x, Rope r)
	{
		Rope res;
		append(res, " node {");
		append(res, x);
		append(res, "} { ");
		if (l != "") {
			append(res, "child { ");
			append(res, l);
			append(res, " } ");
		} else {
			append(res, "child[missing] { } ");
		}
		if (r != "") {
			append(res, "child { ");
			append(res, r);
			append(res, " }");
		} else {
			append(res, "child[missing] { } ");
		}
		append(res, " } ");
		return res;
	}
	
	Rope lf(int x)
	{
		Rope res;
		append(res, "node {");
		append(res, x);
		append(res, '}');
		return res;
	}
	
	Rope nil(void)
	{
		Rope res;
		//append(res, "node {}");
		return res;
	}
	
	int f(int key, int prob)
	{
		return key;
	}
	
	choice [Rope] h([Rope] l)
	{
		return l;
	}
}


algebra tikze2 implements Tree(alphabet = int, answer = Rope, inp = alph) {
	Rope br(Rope l, alph x, Rope r)
	{
		Rope res;
		append(res, " node {");
		append(res, "br");
		append(res, "} { ");
		append(res, "child { ");
		append(res, l);
		append(res, " } ");
		append(res, "child { node {$\\binom{");
		append(res, x.key);
		append(res, "}{");
		append(res, x.prob);
		append(res, "}$ } } ");
		append(res, "child { ");
		append(res, r);
		append(res, " }");
		append(res, " } ");
		return res;
	}
	
	Rope lf(alph x)
	{
		Rope res;
		append(res, "node {lf} { child { node {$\\binom{");
		append(res, x.key);
		append(res, "}{");
		append(res, x.prob);
		append(res, "}$ } } }");
		return res;
	}
	
	Rope nil(void)
	{
		Rope res;
		append(res, "node {nil} ");
		return res;
	}
	
	alph f(int key, int prob)
	{
		alph res;
		res.key = key;
		float p = prob;
		res.prob = p/100.0;
		return res;
	}
	
	choice [Rope] h([Rope] l)
	{
		return l;
	}
}


grammar btrees uses Tree(axiom = btree)
{
	btree =		br(btree, char, btree) |
				lf(char) |
				nil(EMPTY) # h ;
	char = f(CHAR, CHAR);
}


instance mean = btrees(mean);
instance meanpp = btrees(mean*pretty);
instance meant = btrees(mean*tikz);
instance pretty = btrees(pretty);
instance prettymean = btrees(pretty*mean);
instance tikz = btrees(tikz);
instance tikzmean = btrees(tikz*mean);



