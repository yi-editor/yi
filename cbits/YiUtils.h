#ifndef YIUTILS_H
#define YIUTILS_H

#include <string.h>

#include "YiCurses.h"

extern void nomacro_getyx(WINDOW *win, int *y, int *x);

extern unsigned long countlns(char *b1, int start, int end);
extern unsigned long gotoln(char *b, int start, int end, int n);

extern unsigned long tabwidths(char *b, int start, int end, int tabwidth);
extern unsigned long screenlen(char *b, int start, int end, int tabwidth, int max);

#endif
