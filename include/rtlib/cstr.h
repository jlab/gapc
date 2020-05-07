
#ifndef RTLIB_CSTR_H
#define RTLIB_CSTR_H

inline
char *int_to_str(char *s, unsigned char *len, int j)
{
  int i = j;
  s[11] = 0;
  char *c = s+9;
  *c = '0';
  c++;
  if (!i)
    c--;
  else if (i < 0)
    i *= -1;
  while (i) {
    c--;
    *c = '0' + i % 10;
    i /= 10;
  }
  if (j < 0) {
    c--;
    *c = '-';
  }
  *len = 10 - (c-s);
  return c;
}

#endif
