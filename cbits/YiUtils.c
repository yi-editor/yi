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

/* find index of first occurence of b2 in b1, starting at off in b1,
   with b1sz and b2sz */
int strstr_n(char *b1, char *b2, int off, size_t b1sz, size_t b2sz)
{
    char *p;
    char c = b1[b1sz];  /* insert nulls into end of buffer temporarily */
    char d = b2[b2sz];
    b1[b1sz] = '\0';
    b2[b2sz] = '\0';

    p = strstr(b1+off, b2);

    b1[b1sz] = c;
    b2[b2sz] = d;

    return (p == NULL) ? (-1) : (int)(p - b1);
}

