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
unsigned long strstr_n(char *b1, char *b2, int off, size_t b1sz, size_t b2sz)
{
    char *p;
    b1[b1sz] = b2[b2sz] = '\0'; /* insert nulls into end of buffer */
    p = strstr(b1+off, b2);
    return (p == NULL) ? (-1) : (unsigned long)(p - b1);
}

/* count the number of occurences of \n in buffer from start to end */
unsigned long countlns(char *b1, int start, int end) 
{
    char *p = b1 + start;
    char *q = b1 + end;
    unsigned long c = 1;    /* there's always 1 line */
    while (p++ < q) 
        if (*p == '\n') c++;
    return c;
}

