#ifndef YIUTILS_H
#define YIUTILS_H

#include "YiCurses.h"

extern void nomacro_getyx(WINDOW *win, int *y, int *x);
extern void memcpy_shift(char *buf, int src_off, int dst_off, size_t sz);

#endif
