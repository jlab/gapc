
#if defined(__APPLE__) && defined(__MACH__)
	#define _DARWIN_C_SOURCE
#endif

#include <sys/mman.h>
#include <cstdlib>

int main()
{
      void *ret = mmap(0, 10 * 1024 * 1024, PROT_READ | PROT_WRITE, MAP_PRIVATE
#if defined(__APPLE__) && defined(__MACH__)
          | MAP_ANON
#else
          | MAP_ANONYMOUS
#endif
          , -1, 0);
      // FIXME report error
      if (ret == MAP_FAILED)
        std::abort();
      return 0;
}
