
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

/* return offset from start in buf that regex matches */
int regexec_baoff(const regex_t *preg, char *buf, int start, int sz,
                  size_t nmatch, regmatch_t pmatch[], int eflags)
{
    int i;
    char *buf_ = buf + start;
    buf[sz] = '\0';         /* just in case */

    i = regexec(preg, buf_, nmatch, pmatch, eflags);

    return i;
}
