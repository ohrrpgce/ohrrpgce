// Minimal stub for the termcap API (part of curses), which can be linked to
// remove a dependency on libncurses/libtinfo by FB programs.
// This does just enough to make hInit() in libfb succeed so INKEY works.
//
// Placed in the public domain.

#include <stdio.h>
#include <string.h>
#define NCURSES_STATIC  // Building or linking to a static library (needed on Windows only)
#include <termcap.h>

char PC;
char * UP;
char * BC;
NCURSES_OSPEED ospeed;

NCURSES_EXPORT(char *) tgetstr (const char *id, char **area) {
    return 0;
}
NCURSES_EXPORT(char *) tgoto (const char *cap, int col, int row) {
    return 0;
}
NCURSES_EXPORT(int) tgetent (char *bp, const char *name) {
    return 1;
}
NCURSES_EXPORT(int) tgetflag (const char *id) {
    if (memcmp(id, "am", 2) == 0)
        return 1;
    return 0;
}
NCURSES_EXPORT(int) tgetnum (const char *id) {
    return 0;
}
NCURSES_EXPORT(int) tputs (const char *str, int affcnt, int (*putc)(int)) {
    puts(str);
    return 0;
}
