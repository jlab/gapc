type triple = ( int multiplications, int rows, int columns )

signature Matrixchain(alphabet, answer) {

  answer single(alphabet, int, alphabet, int, alphabet);
  answer mult(answer, alphabet, answer);
  choice [answer] h([answer]);
}

algebra count implements Matrixchain(alphabet = char, answer = int) {

  int single(char boxl, int rows, char by, int columns, char boxr) {
    return 1;
  }

  int mult(int i, char times, int j) {
    return i * j;
  }

  choice [int] h([int] i)
  {
    return list(sum(i));
  }

}

algebra pretty implements Matrixchain(alphabet = char, answer = string) 
{
  string single(char boxl, int rows, char by, int columns, char boxr) {
    string r;
    append(r, '[');
    append(r, rows);
    append(r, 'x');
    append(r, columns);
    append(r, ']');
    return r;
  }

  string mult(string i, char times, string j)
  {
    string r;
    append(r, '(');
    append(r, i);
    append(r, '*');
    append(r, j);
    append(r, ')');
    return r;
  }

  choice [string] h([string] i)
  {
    return i;
  }
}

algebra acount auto count ;
algebra enum auto enum ;

algebra minmult implements Matrixchain(alphabet = char, answer = triple) 
{
  triple single(char boxl, int rows, char by, int columns, char boxr) {
    triple t;
    t.rows = rows;
    t.multiplications = 0;
    t.columns = columns;
    return t;
  }

  triple mult(triple i, char times, triple j)
  {
    triple t;
    t.rows = i.rows;
    t.multiplications = i.multiplications + j.multiplications + i.rows * i.columns * j.columns;
    t.columns = j.columns;
    return t;
  }

  choice [triple] h([triple] i)
  {
    return list(minimum(i));
  }
}

algebra minmem extends minmult
{
  triple mult(triple i, char times, triple j)
  {
    triple t;
    t.rows = i.rows;
    int a = max(max(i.multiplications,i.rows * i.columns + j.multiplications), i.rows * i.columns + i.rows * j.columns + j.rows * j.columns);
    int b = max(max(j.multiplications,j.rows * j.columns + i.multiplications), i.rows * i.columns + i.rows * j.columns + j.rows * j.columns);
    t.multiplications = min(a,b);
    t.columns = j.columns;
    return t;
  }
}

grammar matrixmult uses Matrixchain (axiom=matrices) {

  tabulated { matrices }

  matrices = single(boxl, INT, by, INT, boxr) |
             mult(matrices, times, matrices) # h ;

  boxl = CHAR('[');
  boxr = CHAR(']');
  by = CHAR('x');
  times = CHAR('*');
}

instance minmemminmult = matrixmult(minmem * minmult) ;


