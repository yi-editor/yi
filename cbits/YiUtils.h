#ifndef YIUTILS_H
#define YIUTILS_H

#include "YiCurses.h"

extern void nomacro_getyx(WINDOW *win, int *y, int *x);
extern void memcpy_shift(char *buf, int src_off, int dst_off, size_t sz);
extern int strstr_n(char *b1, char *b2, int off, size_t b1sz, size_t b2sz);

#endif
