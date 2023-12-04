

int main()
{

  enum base_t { N_BASE, A_BASE, C_BASE, G_BASE, U_BASE };
  char c = 'x';
  base_t t = 0;
  unsigned int i = 0;
  t = i;
  t = c;
  t = base_t(c);

  i = A_BASE;
  return 0;
}
