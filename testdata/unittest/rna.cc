// link against -lboost_unit_test_framework
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE rtlib
#include <boost/test/unit_test.hpp>

#include "macros.hh"

#include "../../rtlib/rna.hh"

#include <algorithm>
#include <cstring>

BOOST_AUTO_TEST_CASE( pair )
{
  static char s[] = { A_BASE, G_BASE, C_BASE, U_BASE, C_BASE, C_BASE };
  Sequence seq(s);
  CHECK(stackpairing(seq, 0, 4));
  CHECK(basepairing(seq, 1, 3));
  CHECK(!stackpairing(seq, 2, 6));
}

BOOST_AUTO_TEST_CASE( convert )
{
  static char s[] = { A_BASE, G_BASE, C_BASE, U_BASE, C_BASE, C_BASE };
  static char t[] = { 'A', 'G', 'C', 'U', 'C', 'C' };
  Sequence p(s, 6);
  Sequence q(t, 6);
  for (unsigned int i = 0; i < 6; i++)
    CHECK_NOT_EQ(p[i], q[i]);
  char_to_rna(q);
  for (unsigned int i = 0; i < 6; i++)
    CHECK_EQ(p[i], q[i]);
}

#include "../../rtlib/rope.hh"
#include <cstring>

BOOST_AUTO_TEST_CASE ( app_subseq_rna )
{
  Rope r;
  Sequence s;
  const char inp[] = "gauuaga";
  s.copy(inp, std::strlen(inp));
  char_to_rna(s);
  Subsequence x(s, 3, 6);
  append_deep_rna(r, x);
  Rope q;
  append(q, "uag");
  CHECK_EQ(r, q);
}

BOOST_AUTO_TEST_CASE ( multi_pair )
{
  Basic_Sequence<M_Char> s;
  const char inp[] = "acgu#cccu#uccu#";
  s.copy(inp, std::strlen(inp));
  char_to_rna(s);
  CHECK(!basepairing(s, 0, 4));
  CHECK(!basepairing(s, 0, 4, 50));
  CHECK(basepairing(s, 0, 4, 30));
}

BOOST_AUTO_TEST_CASE ( multi_stack )
{
  Basic_Sequence<M_Char> s;
  const char inp[] = "acgu#cccu#uccu#";
  s.copy(inp, std::strlen(inp));
  char_to_rna(s);
  CHECK(!stackpairing(s, 0, 4));
  CHECK(!stackpairing(s, 0, 4, 50));
  CHECK(stackpairing(s, 0, 4, 30));
}

BOOST_AUTO_TEST_CASE ( iupac )
{
  char sequence[27] = "AAAgggcccAAAAggggccccAAAAA";
                    // 01234567890123456789012345
                    // 0         1         2
  Sequence seq(sequence);
  char_to_rna(seq);
  iupac_filter<char, unsigned> filter;
  const char pattern[] = "gbbc";
  filter.init(seq, pattern);
  CHECK(!filter.query(0, 4));
  CHECK(filter.query(0, 7));
  CHECK(filter.query(3, 7));
  CHECK(!filter.query(6, 14));
  CHECK(filter.query(6, 22));
  CHECK(filter.query(16, 21));
	
  char sequence2[14] = "ccacuccucccgg";
  Sequence seq2(sequence2);
  char_to_rna(seq2);
  iupac_filter<char, unsigned> filter2;
  const char pattern2[] = "ccuccuccc";
  filter2.init(seq2,pattern2);
  CHECK(!filter2.query(2,11));
  
  iupac_filter<char, unsigned> filter3;
  const char pattern3[] = "acuccuccc";
  filter3.init(seq2,pattern3);
  CHECK(filter3.query(2,11));

}
