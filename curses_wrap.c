// Because translating C headers to FB is hopelessly nonportable...

#include <curses.h>

WINDOW *get_stdscr() {
  return stdscr;
}

void set_ESCDELAY(int val) {
  ESCDELAY = val;
}
