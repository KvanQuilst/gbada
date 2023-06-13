#include <locale.h>
#include <ncurses.h>
#include <panel.h>

#define SCR_MAX_H 144
#define SCR_MAX_W 160

int rowSize, colSize;

int init_screen(void)
{
  setlocale(LC_ALL, "");

  initscr();
  if (stdscr == NULL) {
    fprintf(stderr, "curses: unable to initialize screen\n");
    return -1;
  }

  getmaxyx(stdscr, rowSize, colSize);
  if (rowSize < SCR_MAX_H || colSize < SCR_MAX_W) {
    endwin();
    fprintf(stderr, "curses: terminal is too small!\n");
    return -1;
  }

  curs_set(0);
  noecho();
  nonl();
  wnoutrefresh(stdscr);

  start_color();
  use_default_colors();

  return 0;
}

void kill_screen(void)
{
  if (stdscr) endwin();
}
