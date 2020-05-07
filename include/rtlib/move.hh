#ifndef MOVE_HH
#define MOVE_HH

template <typename T>
inline
void move(T &a, T &b)
{
  a = b;
}

template <typename A, typename B>
inline
void move(std::pair<A, B> &a, std::pair<A, B> &b)
{
  move(a.first, b.first);
  move(a.second, b.second);
}


#endif
