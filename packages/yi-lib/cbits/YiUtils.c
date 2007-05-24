
#include "YiUtils.h"

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


