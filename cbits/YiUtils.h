#ifndef YIUTILS_H
#define YIUTILS_H

#include <string.h>

#include "YiCurses.h"

extern void nomacro_getyx(WINDOW *win, int *y, int *x);

extern unsigned long countLines(char *b1, int start, int end);
extern unsigned long findStartOfLineN(char *b, int start, int end, int n);

extern unsigned long expandedLengthOfStr(char *b, int start, int end, int tabwidth);
extern unsigned long strlenWithExpandedLengthN(char *b, int start, int end, int tabwidth, int max);

#endif
