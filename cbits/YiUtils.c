
#include "YiUtils.h"

/*
 * A non-macro version of getyx(3), to make writing a Haskell binding
 * easier.  Called in Yi/Curses.hsc
 */
void nomacro_getyx(WINDOW *win, int *y, int *x)
{
    getyx(win, *y, *x);
}

/* count the number of occurences of \n in buffer from start to end */
unsigned long countlns(char *b1, int start, int end) 
{
    char *p = b1 + start;
    char *q = b1 + end;
    unsigned long c = 1;    /* there's always 1 line */
    while (p < q) 
        if (*p++ == '\n') c++;
    return c;
}

/* return the index of the first point of line @n@, indexed from 1 */
unsigned long gotoln(char *b, int start, int end, int n)
{
    char *p = b + start;
    char *q = b + end;
    unsigned long c = 1;    /* there's always 1 line */

    if (n >= 0) {
        while (p < q && c < n) 
            if (*p++ == '\n') c++;
    } else {
        int n_ = 0 - n;
        while (p > q && c < n_) 
            if (*p-- == '\n') c++;
    }
    return (p - (b + start));
}

