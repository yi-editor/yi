
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
unsigned long countLines(char *b1, int start, int end) 
{
    char *p = b1 + start;
    char *q = b1 + end;
    unsigned long c = 1;    /* there's always 1 line */
    while (p < q) 
        if (*p++ == '\n') c++;
    return c;
}

/* 
 * return the offset of the first character of the line @n@ from the
 current point, for example, from the start of the buffer:
 *
 * sequence [ findStartOfLineN p 0 end i | i <- [ 0 .. numline ] ]
 *      ==
 * 0 : (init . map (+1) . (findIndices (== '\n') p))
 */
unsigned long findStartOfLineN(char *b, int start, int end, int n)
{
    char *p = b + start;
    char *q = b + end;
    unsigned long c = 0; /* current line */

    if (n > 0) {
        while (p < q && c < n) 
            if (*p++ == '\n') c++;

    } else { /* go backwards */
        int n_ = - n;
        while (p > q)  {
            if (*(p-1) == '\n') c++;
            if (c >= n_) break;
            p--;
        }
    }
    return (p - (b + start));

}

/* Length of tabs in the string from start to end, after expansion
 *
 * where: let s = "\t\n.\t\n..\t\n...\t\n....\t\n.....\t\n......\t\n.......\t\n........\t\n"
 * then : expandedLengthOfStr == [7,6,5,4,3,2,1,0,7] for these lines
 */
unsigned long expandedLengthOfStr(char *b, int start, int end, int tabwidth)
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
 *
 * let s = "\t\n.\t\n..\t\n...\t\n....\t\n.....\t\n......\t\n.......\t\n........\t\n"
 * then [1,2,3,4,5,6,7,8,8] is the lengths required for these lines, for max == 8
 */
unsigned long strlenWithExpandedLengthN(char *b, int start, int end, int tabwidth, int max)
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
