
#include "YiUtils.h"

/*
 * A non-macro version of getyx(3), to make writing a Haskell binding
 * easier.  Called in Yi/Curses.hsc
 */
void nomacro_getyx(WINDOW *win, int *y, int *x)
{
    getyx(win, *y, *x);
}

/* 1 + the number of occurences of \n in buffer from start to end
 * i.e. the index of the current line, starting from 1
 */
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

/* return width of all tabs in current line, minus the number of tabs
 * (so that we can just say: number of chars + tabwidths 
 * So this function tells you how many tabs given a string.
 */
unsigned long tabwidths(char *b, int start, int end, int tabwidth)
{
    char *p = b + start;
    char *q = b + end;
    unsigned long c   = 0;
    unsigned long col = 0;
    unsigned long w   = 0;
    while (p < q && *p != '\n') {
        if (*p++ == '\t') {
            w = tabwidth - (col % tabwidth); /* width of tab */
            c   += w - 1; /* minus 1 for tab char itself */
            col += w;
        } else {
            col++;
        }
    }
    return c;
}

/*
 * return the string length required to make a screen length of `max',
 * including tab expansion. assume screenwidth % tabwidth == 0 for now
 * this is kind of the inverse of tabwidths, telling you how much of a
 * string, given a desired tab width
 */
unsigned long screenlen(char *b, int start, int end, int tabwidth, int max)
{
    char *p = b + start;
    char *q = b + end;
    unsigned long i;          /* count of chars */
    unsigned long col = 0;    /* screen column  */
    unsigned long w   = 0;
    for (i = 0; p < q && col < max; i++, col += w) {
        if (*p++ == '\t')
            w = tabwidth - (col % tabwidth);
        else 
            w = 1;
    }
    return i;
}

