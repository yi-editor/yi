#include "nomacro.h"

/*
 * A non-macro version of getyx(3), to make writing a Haskell binding
 * easier.  Called in HEmacs/Curses.hsc
 */
void nomacro_getyx(WINDOW *win, int *y, int *x)
{
    getyx(win, *y, *x);
}

