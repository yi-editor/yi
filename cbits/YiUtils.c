#include <string.h>

#include "YiUtils.h"

/*
 * A non-macro version of getyx(3), to make writing a Haskell binding
 * easier.  Called in Yi/Curses.hsc
 */
void nomacro_getyx(WINDOW *win, int *y, int *x)
{
    getyx(win, *y, *x);
}

/* shift contents of a MutableByteArray#  -- fast! */
void memcpy_shift(char *buf, int src_off, int dst_off, size_t sz ) 
{ 
 // memcpy(buf+dst_off, buf+src_off, sz); 
    memmove(buf+dst_off, buf+src_off, sz);
}

