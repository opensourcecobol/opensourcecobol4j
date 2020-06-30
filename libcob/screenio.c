/*
 * Copyright (C) 2001-2009 Keisuke Nishida
 * Copyright (C) 2007-2009 Roger While
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	_WIN32
/* Later pdcurses versions require define before the include for DLL build */
#define	PDC_DLL_BUILD	1
#include <io.h>
#endif

#ifdef HAVE_NCURSES_H
#include <ncurses.h>
#define COB_GEN_SCREENIO
#elif defined(HAVE_NCURSES_NCURSES_H)
#include <ncurses/ncurses.h>
#define COB_GEN_SCREENIO
#elif defined(HAVE_PDCURSES_H)
#include <pdcurses.h>
#define COB_GEN_SCREENIO
#elif defined(HAVE_CURSES_H)
#include <curses.h>
#define COB_GEN_SCREENIO
#endif

/* if LINES unsuported */
#ifndef A_OVERLINE
#ifdef A_TOP
#define A_OVERLINE A_TOP
#else 
#define A_OVERLINE 0x00000000
#endif /* A_TOP */
#endif /* A_OVERLINE */

#ifndef A_LEFTLINE
#ifdef A_LEFT
#define A_LEFTLINE A_LEFT
#else 
#define A_LEFTLINE 0x00000000
#endif /* A_LEFT */
#endif /* A_LEFTLINE */

#ifndef A_RIGHTLINE
#ifdef A_RIGHT
#define A_RIGHTLINE A_RIGHT
#else 
#define A_RIGHTLINE 0x00000000
#endif /* A_RIGHT */
#endif /* A_RIGHTLINE */

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

/* Global variables */

int	cob_screen_initialized = 0;
int	cob_screen_mode = 0;

#define COB_CHECK_BEFORE 1
#define COB_CHECK_CURRENT 0


#ifdef COB_GEN_SCREENIO

struct cob_inp_struct {
	cob_screen	*scr;
	size_t		up_index;
	size_t		down_index;
	int		this_y;
	int		this_x;
};

#define	COB_INP_SIZE	1920 * sizeof(struct cob_inp_struct)

/* Local variables */

static struct cob_inp_struct	*cob_base_inp = NULL;
static size_t			curr_index = 0;
static size_t			totl_index = 0;
static size_t			cob_has_color = 0;
static size_t			cob_extended_status = 0;
static size_t			cob_use_esc = 0;
static int			global_return;
static int			cob_current_y = 0;
static int			cob_current_x = 0;
static int			cob_max_y = 0;
static int			cob_max_x = 0;
static short			fore_color;
static short			back_color;
static int 			insert_mode = 0;  /* insert toggle, 0=off, 1=on */ 

/* Local functions */

static void
cob_convert_key (int *keyp, const unsigned int field_accept)
{
	/* Map key to KEY_xxx value */
	switch (*keyp) {
	case '\n':
	case '\r':
	case '\004':
	case '\032':
		*keyp = KEY_ENTER;
		break;
	case '\t':
		*keyp = KEY_STAB;
		break;
	case '\b':
	case 0177:
		*keyp = KEY_BACKSPACE;
		break;

#ifdef	KEY_A1
	/* A1, A3, C1, C3 must be present */
	case KEY_A1:
		*keyp = KEY_HOME;
		break;
	case KEY_A3:
		*keyp = KEY_PPAGE;
		break;
	case KEY_C1:
		*keyp = KEY_END;
		break;
	case KEY_C3:
		*keyp = KEY_NPAGE;
		break;
	/* Any or all of A2, B1-3, C2 MAY be present */
	/* Note B2 ignored */
#ifdef	KEY_A2
	case KEY_A2:
		*keyp = KEY_UP;
		break;
#endif
#ifdef	KEY_B1
	case KEY_B1:
		*keyp = KEY_LEFT;
		break;
#endif
#ifdef	KEY_B3
	case KEY_B3:
		*keyp = KEY_RIGHT;
		break;
#endif
#ifdef	KEY_C2
	case KEY_C2:
		*keyp = KEY_DOWN;
		break;
#endif

#if	defined(__PDCURSES__) && defined(PADSLASH)
	case PADSLASH:
		*keyp = '/';
		break;
	case PADSTAR:
		*keyp = '*';
		break;
	case PADMINUS:
		*keyp = '-';
		break;
	case PADPLUS:
		*keyp = '+';
		break;
	case PADENTER:
		*keyp = KEY_ENTER;
		break;
#ifdef	PAD0
	case PAD0:
		*keyp = KEY_IC;
		break;
	case PADSTOP:
		*keyp = KEY_DC;
		break;
#endif	/* PAD0 */
#endif	/* __PDCURSES__ */
#endif	/* KEY_A1 */
	default:
		break;
	}

	/* Check if key should be ignored */
	switch (*keyp) {
	/* 2012/08/30 removed for extended Accept usage. 
	case KEY_STAB:
		if (field_accept) {
			*keyp = 0;
		}
		break; */
	case '\033':
		if (!cob_extended_status || !cob_use_esc) {
			*keyp = 0;
		}
		break;
	case KEY_PPAGE:
	case KEY_NPAGE:
	case KEY_PRINT:
		if (!cob_extended_status) {
			*keyp = 0;
		}
		break;
	case KEY_UP:
	case KEY_DOWN:
		if (field_accept && !cob_extended_status) {
			*keyp = 0;
		}
		break;
	default:
		break;
	}
}

static void
get_line_column (cob_field *fline, cob_field *fcol, int *line, int *col)
{
	int	l;
	int	c;
	int	p;

	if (fline == NULL) {
		*line = 0;
		*col = 0;
		return;
	}

	p = cob_get_int (fline);

	if (fcol == NULL) {
		if (fline->size == 4) {
			l = p / 100;
			c = p % 100;
		} else {
			l = p / 1000;
			c = p % 1000;
		}
	} else {
		l = p;
		c = cob_get_int (fcol);
	}
	if (l > 0) {
		l--;
	}
	if (c > 0) {
		c--;
	}
	*line = l;
	*col = c;
}

static void
cob_screen_attr (cob_field *fgc, cob_field *bgc, const int attr)
{
	size_t		i;
	int		styles = 0;
	int		line;
	int		column;
	short		fgcolor;
	short		bgcolor;
	short		fgdef;
	short		bgdef;

	attrset (A_NORMAL);
	if (attr & COB_SCREEN_REVERSE) {
		styles |= A_REVERSE;
	}
	if (attr & COB_SCREEN_HIGHLIGHT) {
		styles |= A_BOLD;
	}
	if (attr & COB_SCREEN_BLINK) {
		styles |= A_BLINK;
	}
	if (attr & COB_SCREEN_UNDERLINE) {
		styles |= A_UNDERLINE;
	}
	if (attr & COB_SCREEN_OVERLINE) {
		styles |= A_OVERLINE;
	}
	if (attr & COB_SCREEN_RIGHTLINE) {
		styles |= A_RIGHTLINE;
	}
	if (attr & COB_SCREEN_LEFTLINE) {
		styles |= A_LEFTLINE;
	}

	if (styles) {
		attron (styles);
	}
	if (cob_has_color) {
		fgcolor = fore_color;
		bgcolor = back_color;
		if (fgc) {
			switch (cob_get_int (fgc)) {
			case COB_SCREEN_BLACK:
				fgcolor = COLOR_BLACK;
				break;
			case COB_SCREEN_BLUE:
				fgcolor = COLOR_BLUE;
				break;
			case COB_SCREEN_GREEN:
				fgcolor = COLOR_GREEN;
				break;
			case COB_SCREEN_CYAN:
				fgcolor = COLOR_CYAN;
				break;
			case COB_SCREEN_RED:
				fgcolor = COLOR_RED;
				break;
			case COB_SCREEN_MAGENTA:
				fgcolor = COLOR_MAGENTA;
				break;
			case COB_SCREEN_YELLOW:
				fgcolor = COLOR_YELLOW;
				break;
			case COB_SCREEN_WHITE:
				fgcolor = COLOR_WHITE;
				break;
			default:
				break;
			}
		}
		if (bgc) {
			switch (cob_get_int (bgc)) {
			case COB_SCREEN_BLACK:
				bgcolor = COLOR_BLACK;
				break;
			case COB_SCREEN_BLUE:
				bgcolor = COLOR_BLUE;
				break;
			case COB_SCREEN_GREEN:
				bgcolor = COLOR_GREEN;
				break;
			case COB_SCREEN_CYAN:
				bgcolor = COLOR_CYAN;
				break;
			case COB_SCREEN_RED:
				bgcolor = COLOR_RED;
				break;
			case COB_SCREEN_MAGENTA:
				bgcolor = COLOR_MAGENTA;
				break;
			case COB_SCREEN_YELLOW:
				bgcolor = COLOR_YELLOW;
				break;
			case COB_SCREEN_WHITE:
				bgcolor = COLOR_WHITE;
				break;
			default:
				break;
			}
		}
		for (i = 0; i < (size_t)COLOR_PAIRS; i++) {
			pair_content ((short)i, &fgdef, &bgdef);
			if (fgdef == fgcolor && bgdef == bgcolor) {
				break;
			}
			if (fgdef == 0 && bgdef == 0) {
				init_pair ((short)i, fgcolor, bgcolor);
				break;
			}
		}
		if (i != (size_t)COLOR_PAIRS) {
#ifdef	HAVE_COLOR_SET
			color_set (COLOR_PAIR((short)i), (void *)0);
#else
			attrset (COLOR_PAIR(i));
#endif
			bkgdset (COLOR_PAIR(i));
		} else {
			attrset (A_NORMAL);
		}
	}
	if (attr & COB_SCREEN_BLANK_SCREEN) {
		getyx (stdscr, line, column);
		clear ();
		move (line, column);
	}
	if (attr & COB_SCREEN_BLANK_LINE) {
		getyx (stdscr, line, column);
		move (line, 0);
		clrtoeol ();
		move (line, column);
	}
	if (attr & COB_SCREEN_ERASE_EOL) {
		clrtoeol ();
	}
	if (attr & COB_SCREEN_ERASE_EOS) {
		clrtobot ();
	}
	if (attr & COB_SCREEN_BELL) {
		beep ();
	}
}

static void COB_NOINLINE
cob_screen_init (void)
{
	char	*s;

	if (!cob_screen_initialized) {
		s = getenv ("COB_SCREEN_EXCEPTIONS");
		if (s) {
			if (*s == 'Y' || *s == 'y') {
				cob_extended_status = 1;
				s = getenv ("COB_SCREEN_ESC");
				if (s) {
					if (*s == 'Y' || *s == 'y') {
							cob_use_esc = 1;
					}
				}
			}
		}
		/* Get default insert mode, if 'Y' set to on */ 
		s = getenv ("COB_INSERT_MODE");
                if (s) {
                       if (*s == 'Y' || *s == 'y') {
                               insert_mode = 1; 
                       }
                } 
		fflush (stdout);
		fflush (stderr);
		if (!initscr ()) {
			cob_runtime_error ("Failed to initialize curses");
			cob_stop_run (1);
		}
		cbreak ();
		keypad (stdscr, 1);
		nl ();
		noecho ();
		if (has_colors ()) {
			start_color ();
			pair_content ((short)0, &fore_color, &back_color);
			if (COLOR_PAIRS) {
#ifdef	HAVE_LIBPDCURSES
			size_t i;
			/* pdcurses sets ALL pairs to default fg/bg */
			/* IMHO a bug. */
			for (i = 1; i < (size_t)COLOR_PAIRS; ++i) {
				init_pair ((short)i, 0, 0);
			}
#endif
				cob_has_color = 1;
			}
		}
		attrset (A_NORMAL);
		getmaxyx (stdscr, cob_max_y, cob_max_x);
		cob_screen_initialized = 1;
	}
}

void
cob_screen_terminate (void)
{
	if (cob_screen_initialized) {
		cob_screen_initialized = 0;
		endwin ();
	}
}

static void COB_NOINLINE
cob_check_pos_status (int fret)
{
	cob_field	*f;
	int		sline;
	int		scolumn;
	char		datbuf[8];

	if (fret) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
	}
	if (cob_current_module->crt_status) {
		if (COB_FIELD_IS_NUMERIC (cob_current_module->crt_status)) {
			cob_set_int (cob_current_module->crt_status, fret);
		} else {
			sprintf(datbuf, "%4.4d", fret);
			memcpy (cob_current_module->crt_status->data, datbuf, 4);
		}
	}
	if (cob_current_module->cursor_pos) {
		getyx (stdscr, sline, scolumn);
		f = cob_current_module->cursor_pos;
		if (COB_FIELD_IS_NUMERIC (f) &&
		    COB_FIELD_TYPE (f) != COB_TYPE_NUMERIC_DISPLAY) {
			sline *= 1000;
			sline += scolumn;
			cob_set_int (f, sline);
		} else {
			if (f->size < 6) {
				sline *= 100;
				sline += scolumn;
				sprintf(datbuf, "%4.4d", sline);
				memcpy (f->data, datbuf, 4);
			} else {
				sline *= 1000;
				sline += scolumn;
				sprintf(datbuf, "%6.6d", sline);
				memcpy (f->data, datbuf, 6);
			}
		}
	}
}

static void
cob_screen_puts (cob_screen *s, cob_field *f)
{
	unsigned char	*p;
	size_t		size;
	int		y;
	int		x;
	int		line;
	int		column;

	getyx (stdscr, y, x);
	if (!s->line) {
		line = y;
	} else {
		line = cob_get_int (s->line) - 1;
		if (line < 0) {
			line = y;
		}
	}
	if (!s->column) {
		column = x;
	} else {
		column = cob_get_int (s->column) - 1;
		if (column < 0) {
			column = x;
		}
	}
	if (s->attr & COB_SCREEN_LINE_PLUS) {
		line = y + line + 1;
	} else if (s->attr & COB_SCREEN_LINE_MINUS) {
		line = y - line + 1;
	}
	if (s->attr & COB_SCREEN_COLUMN_PLUS) {
		column = x + column + 1;
	} else if (s->attr & COB_SCREEN_COLUMN_MINUS) {
		column = x - column + 1;
	}
	move (line, column);
	cob_current_y = line;
	cob_current_x = column;
	cob_screen_attr (s->foreg, s->backg, s->attr);
	if (s->attr & COB_SCREEN_INPUT) {
		p = f->data;
		for (size = 0; size < f->size; size++, p++) {
			if (s->attr & COB_SCREEN_SECURE) {
				addch ('*');
			} else if (*p <= ' ') {
				addch ('_');
			} else {
				addch (*p);
			}
		}
	} else {
		addnstr ((char *)f->data, (int)f->size);
	}
	refresh ();
}

static int
cob_is_sjis_multibyte (unsigned char c)
{
#ifdef PDC_SJIS_SUPPORT
	if ((c >= 0x81 && c <= 0x9f) || (c >= 0xe0 && c <= 0xfc)) {
		return 1;
	}
#endif
	return 0;
}

static int
cob_is_sjis_multibyte_at_cursor (unsigned char *data, int current, int checktype)
{
	int flag_multibyte = FALSE;
#ifdef PDC_SJIS_SUPPORT
	int i;

	if (checktype == COB_CHECK_BEFORE) {
		current--;
	}

	for (i = 0; i <= current; i++) {
		if (cob_is_sjis_multibyte(data[i])) {
			i++;
			flag_multibyte = TRUE;
		} else {
			flag_multibyte = FALSE;
		}
	}
#endif
	return flag_multibyte;
}

static void
cob_screen_get_all (void)
{
	struct cob_inp_struct	*sptr;
	cob_screen		*s;
	unsigned char		*p;
	int			keyp;
	int			sline;
	int			scolumn;
	int			cline;
	int			ccolumn;
	int			rightpos;
	int			ateof;
	int			gotbacksp;
	int			ungetched;

	sptr = cob_base_inp;
	s = sptr->scr;
	sline = sptr->this_y;
	scolumn = sptr->this_x;
	move (sline, scolumn);
	cob_screen_attr (s->foreg, s->backg, s->attr);
/* RXW
	p = s->field->data;
	for (count = 0; count < s->field->size; count++) {
		if (s->attr & COB_SCREEN_UPDATE) {
			keyp = *p++;
			addch (keyp);
		} else if (s->attr & COB_SCREEN_PROMPT) {
			addch ('_');
		} else {
			addch (' ');
		}
	}
	move (sline, scolumn);
*/
	ateof = 0;
	gotbacksp = 0;
	ungetched = 0;
	rightpos = scolumn + s->field->size - 1;
	p = s->field->data;

	for (; ;) {
#ifdef PDC_SJIS_SUPPORT
		if (PDC_get_current_byte_type() != PDC_CHAR_SJIS_HIGH) {
			refresh ();
		}
#else
		refresh ();
#endif
		errno = 0;
		keyp = getch ();

		if (keyp == ERR) {
			global_return = 8001;
			goto screen_return;
		}
		if (keyp > KEY_F0 && keyp < KEY_F(65)) {
			global_return = 1000 + keyp - KEY_F0;
			goto screen_return;
		}

		cob_convert_key (&keyp, 0);
		if (keyp <= 0) {
			(void)flushinp ();
			beep ();
			continue;
		}

		getyx (stdscr, cline, ccolumn);

		switch (keyp) {
		case KEY_ENTER:
			goto screen_return;
		case KEY_PPAGE:
			global_return = 2001;
			goto screen_return;
		case KEY_NPAGE:
			global_return = 2002;
			goto screen_return;
		case KEY_PRINT:
			global_return = 2006;
			goto screen_return;
		case '\033':
			global_return = 2005;
			goto screen_return;
		case KEY_STAB:
			if (curr_index < totl_index - 1) {
				curr_index++;
			} else {
				curr_index = 0;
			}
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			gotbacksp = 0;
			rightpos = scolumn + s->field->size - 1;
			p = s->field->data;
			move (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr);
			continue;
		case KEY_BTAB:
			if (curr_index > 0) {
				curr_index--;
			} else {
				curr_index = totl_index - 1;
			}
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			gotbacksp = 0;
			rightpos = scolumn + s->field->size - 1;
			if (ungetched) {
				ungetched = 0;
				if (cob_is_sjis_multibyte_at_cursor (s->field->data, rightpos - scolumn, COB_CHECK_CURRENT)) {
					p = s->field->data + rightpos - 1;
					move (sline, rightpos - 1);
				} else {
					p = s->field->data + rightpos;
					move (sline, rightpos);
				}
			} else {
				p = s->field->data;
				move (sline, scolumn);
			}
			cob_screen_attr (s->foreg, s->backg, s->attr);
			continue;
		case KEY_UP:
			curr_index = sptr->up_index;
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			gotbacksp = 0;
			rightpos = scolumn + s->field->size - 1;
			p = s->field->data;
			move (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr);
			continue;
		case KEY_DOWN:
			curr_index = sptr->down_index;
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			gotbacksp = 0;
			rightpos = scolumn + s->field->size - 1;
			p = s->field->data;
			move (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr);
			continue;
		case KEY_HOME:
			curr_index = 0;
			sptr = cob_base_inp;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			gotbacksp = 0;
			rightpos = scolumn + s->field->size - 1;
			p = s->field->data;
			move (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr);
			continue;
		case KEY_END:
			curr_index = totl_index - 1;
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			gotbacksp = 0;
			rightpos = scolumn + s->field->size - 1;
			p = s->field->data;
			move (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr);
			continue;
		case KEY_BACKSPACE:
			if (ccolumn > scolumn) {
				p = s->field->data + ccolumn - scolumn;
				if (gotbacksp || ccolumn != rightpos) {
					if (!gotbacksp && ccolumn == rightpos - 1 && cob_is_sjis_multibyte(*p)) {
						ateof = 0;
					} else {
						if (cob_is_sjis_multibyte_at_cursor (s->field->data, ccolumn - scolumn, COB_CHECK_BEFORE)) {
							ccolumn -= 2;
							p -= 2;
						} else {
							ccolumn--;
							p--;
						}
					}
				} else {
					ateof = 0;
				}
				gotbacksp = 1;
				if (cob_is_sjis_multibyte (*p) && ccolumn < rightpos) {
					move (cline, ccolumn);
					if (s->attr & COB_SCREEN_SECURE) {
						addch ('*');
						addch ('*');
					} else {
						addch (0x81);
						addch (0x40);
					}
					move (cline, ccolumn);
					*p = 0x81;
					*(p + 1) = 0x40;
					continue;
				} else {
					move(cline, ccolumn);
					if (s->attr & COB_SCREEN_SECURE) {
						addch ('*');
					} else {
						addch (' ');
					}
					move (cline, ccolumn);
					*p = ' ';
					continue;
				}
			}
			continue;
		case KEY_LEFT:
			gotbacksp = 0;
			if (ccolumn > scolumn) {
				if (cob_is_sjis_multibyte_at_cursor (s->field->data, ccolumn - scolumn, COB_CHECK_BEFORE)) {
					ccolumn--;
					if (ccolumn == scolumn) {
						ungetched = 1;
						ungetch (KEY_BTAB);
					}
				}
				ccolumn--;
				move (cline, ccolumn);
				p = s->field->data + ccolumn - scolumn;
			} else {
				ungetched = 1;
				ungetch (KEY_BTAB);
			}
			continue;
		case KEY_RIGHT:
			gotbacksp = 0;
			if (ccolumn < rightpos) {
				if (cob_is_sjis_multibyte(*p)) {
					if ((ccolumn + 1) == rightpos) {
						ungetch('\t'); //goto scolumn
					} else {
						ccolumn++;
					}
				}
				ccolumn++;
				move (cline, ccolumn);
				p = s->field->data + ccolumn - scolumn;
			} else {
				ungetch ('\t');
			}
			continue;
		default:
			break;
		}

		if (keyp > 037 && keyp < (int)A_CHARTEXT) {
			if (COB_FIELD_IS_NUMERIC (s->field)) {
				if (keyp < '0' || keyp > '9') {
					beep ();
					continue;
				}
			}
			gotbacksp = 0;
#ifdef PDC_SJIS_SUPPORT
			if (PDC_get_current_byte_type () == PDC_CHAR_ANSII) {
				if (cob_is_sjis_multibyte_at_cursor (s->field->data, ccolumn - scolumn, COB_CHECK_CURRENT)) {
					if (ccolumn < rightpos) {
						move (cline, ccolumn + 1);
						if (s->attr & COB_SCREEN_SECURE) {
							addch ('*');
						} else {
							addch (' ');
						}
						move (cline, ccolumn);
						*(p + 1) = ' ';
					}
				}
			} else if (PDC_get_current_byte_type () == PDC_CHAR_SJIS_HIGH) {
				if (!cob_is_sjis_multibyte_at_cursor(s->field->data, ccolumn - scolumn, COB_CHECK_CURRENT)) {
					if (ccolumn < rightpos - 1) {
						if (cob_is_sjis_multibyte(*(p + 1))) {
							move (cline, ccolumn + 2);
							if (s->attr & COB_SCREEN_SECURE) {
								addch ('*');
							} else {
								addch (' ');
							}
							move (cline, ccolumn);
							*(p + 2) = ' ';
						}
					}
				}
			}
#endif
			*p = keyp;
			if (s->attr & COB_SCREEN_SECURE) {
				addch ('*');
			} else {
				addch ((unsigned int)keyp);
			}
			if (ccolumn == rightpos) {
#ifdef PDC_SJIS_SUPPORT
				if (PDC_get_current_byte_type () == PDC_CHAR_SJIS_LOW) {
					ccolumn--;
					p--;
				}
				PDC_clear_current_buffer ();
#endif
				if (s->attr & COB_SCREEN_AUTO) {
					if (curr_index == totl_index - 1) {
						goto screen_return;
					} else {
						ungetch (011);
					}
				}
				move (cline, ccolumn);
				if (ateof) {
					beep ();
				} else {
					ateof = 1;
				}
			} else {
				p++;
			}
			continue;
		}
		gotbacksp = 0;
		(void)flushinp ();
		beep ();
	}
screen_return:
	refresh ();
}

static int
compare_yx (const void *m1, const void *m2)
{
	const struct cob_inp_struct	*s1;
	const struct cob_inp_struct	*s2;

	s1 = m1;
	s2 = m2;
	if (s1->this_y < s2->this_y) {
		return -1;
	}
	if (s1->this_y > s2->this_y) {
		return 1;
	}
	if (s1->this_x < s2->this_x) {
		return -1;
	}
	if (s1->this_x > s2->this_x) {
		return 1;
	}
	return 0;
}

static void
cob_prep_input (cob_screen *s)
{
	struct cob_inp_struct	*sptr;
	int			n;

	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
		for (s = s->child; s; s = s->next) {
			cob_prep_input (s);
		}
		break;
	case COB_SCREEN_TYPE_FIELD:
		cob_screen_puts (s, s->field);
		if (s->attr & COB_SCREEN_INPUT) {
			sptr = cob_base_inp + totl_index;
			sptr->scr = s;
			sptr->this_y = cob_current_y;
			sptr->this_x = cob_current_x;
			totl_index++;
		}
		break;
	case COB_SCREEN_TYPE_VALUE:
		cob_screen_puts (s, s->value);
		if (s->occurs) {
			for (n = 1; n < s->occurs; ++n) {
				cob_screen_puts (s, s->value);
			}
		}
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
		cob_screen_attr (s->foreg, s->backg, s->attr);
		break;
	}
}

/* Global functions */

void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column)
{
	int	n;

	if (!cob_screen_initialized) {
		cob_screen_init ();
	}

	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
		for (s = s->child; s; s = s->next) {
			cob_screen_display (s, line, column);
		}
		break;
	case COB_SCREEN_TYPE_FIELD:
		cob_screen_puts (s, s->field);
		break;
	case COB_SCREEN_TYPE_VALUE:
		cob_screen_puts (s, s->value);
		if (s->occurs) {
			for (n = 1; n < s->occurs; ++n) {
				cob_screen_puts (s, s->value);
			}
		}
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
		cob_screen_attr (s->foreg, s->backg, s->attr);
		break;
	}
	refresh ();
}

void
cob_screen_accept (cob_screen *s, cob_field *line, cob_field *column)
{
	struct cob_inp_struct	*sptr;
	struct cob_inp_struct	*sptr2;
	size_t			idx;
	size_t			n;
	size_t			posu;
	size_t			posd;
	size_t			prevy;
	size_t			firsty;
	int			starty;

	if (!cob_screen_initialized) {
		cob_screen_init ();
	}
	if (!cob_base_inp) {
		cob_base_inp = cob_malloc (COB_INP_SIZE);
	} else {
		memset (cob_base_inp, 0, COB_INP_SIZE);
	}
	cob_exception_code = 0;
	cob_current_y = 0;
	cob_current_x = 0;
	totl_index = 0;
	move (0, 0);
	cob_prep_input (s);
	/* No input fields is an error */
	if (!totl_index) {
		cob_check_pos_status (8000);
		return;
	}
	qsort (cob_base_inp, totl_index, sizeof(struct cob_inp_struct), compare_yx);
	sptr = cob_base_inp;
	starty = sptr->this_y;
	posu = 0;
	posd = 0;
	prevy = 0;
	firsty = 0;
	/* Set up array for Cursor UP/DOWN */
	for (n = 0; n < totl_index; n++) {
		sptr = cob_base_inp + n;
		if (sptr->this_y > starty) {
			if (!firsty) {
				firsty = n;
			}
			starty = sptr->this_y;
			sptr2 = cob_base_inp + posd;
			for (idx = posd; idx < n; idx++, sptr2++) {
				sptr2->down_index = n;
			}
			posu = prevy;
			prevy = n;
			posd = n;
		}
		sptr->up_index = posu;
	}
	sptr = cob_base_inp;
	for (n = 0; n < firsty; n++, sptr++) {
		sptr->up_index = posd;
	}
	curr_index = 0;
	global_return = 0;
	cob_screen_get_all ();
	cob_check_pos_status (global_return);
}

void
cob_field_display (cob_field *f, cob_field *line, cob_field *column,
		   cob_field *fgc, cob_field *bgc, cob_field *scroll,
		   const int attr)
{
	int sline;
	int scolumn;

	if (!cob_screen_initialized) {
		cob_screen_init ();
	}

	if (scroll) {
		sline = cob_get_int (scroll);
		if (attr & COB_SCREEN_SCROLL_DOWN) {
			sline = -sline;
		}
		scrollok (stdscr, 1);
		scrl (sline);
		scrollok (stdscr, 0);
		refresh ();
	}
	get_line_column (line, column, &sline, &scolumn);
	move (sline, scolumn);
	cob_screen_attr (fgc, bgc, attr);
	addnstr ((char *)f->data, (int)f->size);
	refresh ();
}

void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *scroll,
		  const int attr)
{
	unsigned char	*p;
	unsigned char	*p2; 
	size_t		count;
	int		keyp;
	int		fret;
	int		sline;
	int		scolumn;
	int		cline;
	int		ccolumn;
	int		rightpos;
	int		ateof;
	int		move_char;
	int		prompt_char; 

	/* Initialize the screen. */ 
	if (!cob_screen_initialized) {
		cob_screen_init ();
	}
		
	if (scroll) {
		keyp = cob_get_int (scroll);
		if (attr & COB_SCREEN_SCROLL_DOWN) {
			keyp = -keyp;
		}
		scrollok (stdscr, 1);
		scrl (keyp);
		scrollok (stdscr, 0);
		refresh ();
	}
	cob_exception_code = 0;
	/* Start line and column. */ 
	get_line_column (line, column, &sline, &scolumn);
	move (sline, scolumn);
	cob_screen_attr (fgc, bgc, attr);
	/* Prompt characters. */ 
	p = f->data;
	for (count = 0; count < f->size; count++) {
		if (attr & COB_SCREEN_SECURE) {
			addch ('*');
		} else if (attr & COB_SCREEN_UPDATE) {
			fret = *p++;
			addch ((unsigned int)fret);
		} else if (COB_FIELD_IS_NUMERIC (f)) {
			addch ('0'); 
		} else if (attr & COB_SCREEN_PROMPT) {
			addch ('_');
		} else {
			addch (' ');
		}
	}
	move (sline, scolumn);
	/* Initialize field. */ 
	if (!(attr & COB_SCREEN_UPDATE)) {
		if (COB_FIELD_IS_NUMERIC (f)) {
			cob_move (&cob_zero, f);
		} else {
			memset (f->data, ' ', f->size);
		}
	}

	fret = 0;
	ateof = 0;
	rightpos = scolumn + f->size - 1; 
	p = f->data;
	/* Get characters from keyboard, processing each one. */ 
	for (; ;) {
		/* Get current line, column. */
		getyx (stdscr, cline, ccolumn); 
		/* Trailing prompts. */ 
		if (COB_FIELD_IS_NUMERIC (f)) {
		  prompt_char = '0';
		} else if (attr & COB_SCREEN_PROMPT) {
		  prompt_char = '_';
		} else {
		  prompt_char = ' ';
		}
		for (count = rightpos; count > scolumn - 1; count--) {
		    /* Get character */ 
		    p2 = f->data + count - scolumn;
		    move_char = *p2; 
		    /* Field prompts. */ 
		    if (COB_FIELD_IS_NUMERIC (f)) {
		      /* Numeric prompt zeros. */ 
		      if (move_char == '0') {
			move (cline, count);
			addch (prompt_char); 
		      } else {
			/* Switch to remove prompts from within field. */
			if (attr & COB_SCREEN_SECURE) {
			  prompt_char = '*';
			} else {
			  prompt_char = '0'; 
			}  
		      }
		    } else {
		      /* Alpha prompts. */ 
		      if (move_char == ' ') {
			move (cline, count);
			addch (prompt_char); 
		      } else {
			/* Switch to remove prompts from within field. */
			if (attr & COB_SCREEN_SECURE) {
			  prompt_char = '*';
			} else {
			  prompt_char = ' '; 
			} 
		      }
		    }
		}
		/* Cursor to current column. */ 
		move (cline, ccolumn);
		/* Refresh screen. */ 
		refresh ();
		errno = 0;
		/* Get a character. */
		keyp = getch ();
		/* Key error. */ 
		if (keyp == ERR) {
			fret = 8001;
			goto field_return;
		}
		/* Function keys F1 through F64 */
		if (keyp > KEY_F0 && keyp < KEY_F(65)) {
			fret = 1000 + keyp - KEY_F0;
			goto field_return;
		}

		cob_convert_key (&keyp, 1U);
		if (keyp <= 0) {
			(void)flushinp ();
			beep ();
			continue;
		}
		
		switch (keyp) {
		case KEY_ENTER:
			goto field_return;
		case KEY_PPAGE:
			/* Page up. */ 
			fret = 2001;
			goto field_return;
		case KEY_NPAGE:
			/* Page down. */ 
			fret = 2002;
			goto field_return;
		case KEY_UP:
			/* Up arrow. */ 
			fret = 2003;
			goto field_return;
		case KEY_DOWN:
			/* Down arrow. */ 
			fret = 2004;
			goto field_return;
		case KEY_PRINT:
			/* Print key. */ 
			/* pdcurses not returning this ? */
			fret = 2006;
			goto field_return;
		case 033:
			/* Escape key. */ 
			fret = 2005;
			goto field_return;
		case KEY_STAB:
			/* Tab key. */
			fret = 2007;
			goto field_return;
		case KEY_BTAB:
			/* Back tab key. */ 
			fret = 2008;
			goto field_return; 
		default:
			break;
		}

		getyx (stdscr, cline, ccolumn);
		switch (keyp) {
		case KEY_IC: 
			/* Insert key toggle.  If off turn on, if on turn off. */ 
			if (insert_mode == 0) {
				insert_mode = 1;     /* on */
				/* to do, needs vertical bar cursor */
				/* this doesn't seem to work */ 
				count = curs_set(1); 
			} else {
				insert_mode = 0;     /* off */ 
				/* to do, needs square cursor */ 
				/* this doesn't seem to work */ 
				count = curs_set(2); 
			}
			continue;
		case KEY_DC:
			/* Delete key. */ 
			/* Delete character, move remainder left. */ 
			for (count = ccolumn; count < rightpos; count++) {
				/* Get character one position to right. */
				p2 = f->data + count - scolumn + 1;
				move_char = *p2;
				/* Move the character left. */ 
				p2 = f->data + count - scolumn;
				*p2 = move_char; 
				/* Update screen with moved character. */ 
				move (cline, count); 
				if (attr & COB_SCREEN_SECURE) {
					addch ('*');
				} else {
					addch (move_char); 
				} 
			}
			/* Put space as the right most character. */ 
			p2 = f->data + f->size - 1;  
			if (COB_FIELD_IS_NUMERIC (f)) {
				*p2 = '0';
			} else { 
				*p2 = ' ';
			}
			/* Put cursor back to original position. */ 
			move (cline, ccolumn); 
			continue;
		case KEY_BACKSPACE:
			/* Backspace key. */ 
			if (ccolumn > scolumn) {
			    /* Shift remainder left with cursor. */
			    for (count = ccolumn; count < rightpos + 1; count++) {
				/* Get character. */
				p2 = f->data + count - scolumn ;
				move_char = *p2;
				/* Move the character left. */ 
				p2 = f->data + count - scolumn - 1;
				*p2 = move_char; 
				/* Update screen with moved character. */ 
				move (cline, count - 1); 
				if (attr & COB_SCREEN_SECURE) {
				    addch ('*');
				} else {
				    addch (move_char); 
				} 
			    }
			    /* Put space as the right most character. */ 
			    p2 = f->data + f->size - 1;  
			    if (COB_FIELD_IS_NUMERIC (f)) {
			      *p2 = '0';
			    } else {
			      *p2 = ' ';
			    }
			    /* Move cursor left one from current. */ 
			    ccolumn--;
			    move (cline, ccolumn); 
			    p--; 
			}    
			ateof = 0; 
			continue; 
		case KEY_HOME:
			/* Home key, move to start of field. */
			move (sline, scolumn);
			p = f->data;
			ateof = 0;
			continue;
		case KEY_END:
			/* End key. */ 
			/* Prepare for empty field. */
			ccolumn = scolumn; 
			move_char = ' '; 
			/* Find non blank from right. */ 
			for (count = rightpos; count >= scolumn; count--) { 
				/* Get character */ 
				p2 = f->data + count - scolumn;
				move_char = *p2; 
				/* Non blank stop. */ 
				if (move_char != ' ') {
					ccolumn = count; 
					count = scolumn;
				}
			} 
			/* Cursor to first blank after. */ 
			if (move_char != ' ' && ccolumn != rightpos) {
				ccolumn++; 
			} 
			move (cline, ccolumn); 
			p = f->data + ccolumn - scolumn; 
			ateof = 0;
			continue;
		case KEY_LEFT:
			/* Left arrow. */ 
			if (ccolumn > scolumn) {
				ccolumn--;
				move (cline, ccolumn);
				p = f->data + ccolumn - scolumn;
				continue;
			}
			continue;
		case KEY_RIGHT:
			/* Right arrow. */ 
			if (ccolumn < rightpos) {
				ccolumn++;
				move (cline, ccolumn);
				p = f->data + ccolumn - scolumn;
				continue;
			}
			continue;
		default:
			break;
		}
		
		/* Printable character. */ 
		if (keyp > 037 && keyp < (int)A_CHARTEXT) {
			/* Numeric field check. */
			if (COB_FIELD_IS_NUMERIC (f)) {
				if (keyp < '0' || keyp > '9') {
					beep ();
					continue;
				}
			}
			/* Insert character. */ 
			if (insert_mode == 1) {
				/* Move remainder to the right. */ 
				for (count = rightpos; count > ccolumn - 1; count--) { 
					/* Get character */ 
					p2 = f->data + count - scolumn - 1;
					move_char = *p2; 
					/* Move character one right. */ 
					p2 = f->data + count - scolumn;
					*p2 = move_char;
					/* Update screen with moved character. */ 
					if (count > scolumn) { 
						move (cline, count); 
						if (move_char != ' ') {
							if (attr & COB_SCREEN_SECURE) {
								addch ('*');
							} else {
								addch (move_char); 
							}
						}  
					}  
				}
				move (cline, ccolumn); 
			}
			*p = (unsigned char)keyp;
			/* Display character or '*' if secure. */ 
			if (attr & COB_SCREEN_SECURE) {
				addch ('*');
			} else {
				addch ((unsigned int)keyp);
			}
			if (ccolumn == rightpos) {
				/* Auto-skip at end of field. */
				if (attr & COB_SCREEN_AUTO) {
					break;
				}
				move (cline, ccolumn);
				if (ateof) {
					beep ();
				} else {
					ateof = 1;
				}
			} else {
				p++;
			}
			continue;
		}
		beep ();
	}
field_return:
	refresh ();
	cob_check_pos_status (fret);
}

void
cob_screen_line_col (cob_field *f, const int l_or_c)
{
	if (!cob_screen_initialized) {
		cob_screen_init ();
	}
	if (!l_or_c) {
		cob_set_int (f, (int)LINES);
	} else {
		cob_set_int (f, (int)COLS);
	}
}

void
cob_screen_set_mode (const size_t smode)
{
	if (!smode) {
		refresh ();
		def_prog_mode ();
		endwin ();
	} else {
		reset_prog_mode ();
		refresh ();
	}
}


static void
cob_screen_setline_allclear ()
{
	int	line, column;
	chtype	current_ch;

	for (line = 0; line < cob_max_y; line++) {
		for (column = 0; column < cob_max_x; column++) {
			current_ch = mvinch (line, column);
			if (current_ch & (A_UNDERLINE | A_OVERLINE | A_LEFTLINE | A_RIGHTLINE)) {
				attrset (current_ch);
				attroff (A_UNDERLINE | A_OVERLINE | A_LEFTLINE | A_RIGHTLINE);
				addch (current_ch & A_CHARTEXT);
			}
		}
	}
}


static void
cob_screen_setline_cell (int line, int column, chtype LINE)
{
	chtype	current_ch;

	current_ch = mvinch (line, column);
	attrset (current_ch | LINE);
	addch (current_ch & A_CHARTEXT);
}

static void
cob_screen_setline_vertical (int start_line, int length, int column, chtype LINE)
{
	int	i;

	for (i = 0; i < length; i++) {
		cob_screen_setline_cell (start_line + i, column, LINE);
	}
}

static void
cob_screen_setline_horizontal (int start_column, int length, int line, chtype LINE)
{
	int	i;

	for (i = 0; i < length; i++) {
		cob_screen_setline_cell (line, start_column + i, LINE);
	}
}

int
CBL_OC_KEISEN (unsigned char * cmd, unsigned char * line, unsigned char * col, unsigned char * lng1, unsigned char * lng2, unsigned char * color, unsigned char * prn)
{
	int	k_cmd;
	int	k_line;
	int	k_col;
	int	k_lng1;
	int	k_lng2;
	int	k_color;
	int	k_prn;
	COB_UNUSED(k_color);
	COB_UNUSED(k_prn);

	COB_CHK_PARMS (CBL_OC_ATTRIBUTE, 5);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[1]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[2]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[3]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[4]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[5]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[6]) {
		return -1;
	}

	k_cmd = cob_get_int (cob_current_module->cob_procedure_parameters[0]);
	k_line = cob_get_int (cob_current_module->cob_procedure_parameters[1]) - 1;
	k_col = cob_get_int (cob_current_module->cob_procedure_parameters[2]) - 1;
	k_lng1 = cob_get_int (cob_current_module->cob_procedure_parameters[3]);
	k_lng2 = cob_get_int (cob_current_module->cob_procedure_parameters[4]);
	k_color = cob_get_int (cob_current_module->cob_procedure_parameters[5]);
	k_prn = cob_get_int (cob_current_module->cob_procedure_parameters[6]);

	if (!cob_screen_initialized) {
		cob_screen_init ();
	}

	switch (k_cmd) {
	case 0:					/* clear */
		cob_screen_setline_allclear ();
		break;
	case 1:					/* under line */
		cob_screen_setline_horizontal (k_col, k_lng1, k_line, A_UNDERLINE);
		break;
	case 2:					/* over line  */
		cob_screen_setline_horizontal (k_col, k_lng1, k_line, A_OVERLINE);
		break;
	case 3:					/* vertical line left */
		cob_screen_setline_vertical (k_line, k_lng1, k_col, A_LEFTLINE);
		break;
	case 4:					/* vertical line right */
		cob_screen_setline_vertical (k_line, k_lng1, k_col, A_RIGHTLINE);
		break;
	case 5:					/* box */
		//horizon line
		cob_screen_setline_horizontal (k_col, k_lng1, k_line, A_OVERLINE);
		cob_screen_setline_horizontal (k_col, k_lng1, k_line + k_lng2 - 1, A_UNDERLINE);

		//vertical line
		cob_screen_setline_vertical (k_line, k_lng2, k_col, A_LEFTLINE);
		cob_screen_setline_vertical (k_line, k_lng2, k_col + k_lng1 - 1, A_RIGHTLINE);
		break;
	default:
		break;
	}

	refresh ();
	return 0;
}

int
CBL_OC_ATTRIBUTE (unsigned char *line, unsigned char *col, unsigned char *lng, unsigned char *fcolor, unsigned char *bcolor, unsigned char *attr, unsigned char *flg)
{
	int		k_line;
	int		k_col;
	int		k_lng;
	cob_field	*k_fcolor;
	cob_field	*k_bcolor;
	int		k_attr;
	int		k_flg;

	int		i;
	chtype		current_ch;

	COB_CHK_PARMS (CBL_OC_ATTRIBUTE, 5);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[1]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[2]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[3]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[4]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[5]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[6]) {
		return -1;
	}

	k_line = cob_get_int (cob_current_module->cob_procedure_parameters[0]) - 1;
	k_col = cob_get_int (cob_current_module->cob_procedure_parameters[1]) - 1;
	k_lng = cob_get_int (cob_current_module->cob_procedure_parameters[2]);
	k_fcolor = cob_current_module->cob_procedure_parameters[3];
	k_bcolor = cob_current_module->cob_procedure_parameters[4];
	k_attr = cob_get_int (cob_current_module->cob_procedure_parameters[5]);
	k_flg = cob_get_int (cob_current_module->cob_procedure_parameters[6]);

	if (!cob_screen_initialized) {
		cob_screen_init ();
	}

	cob_screen_attr (k_fcolor, k_bcolor, k_attr);

	for (i = 0; i < k_lng; i++) {
		current_ch = mvinch (k_line, k_col + i);

		if (k_flg == 0) {
			attron (current_ch);
		}
		addch (current_ch & A_CHARTEXT);
	}

	refresh ();

	return 0;
}

#else

void
cob_screen_terminate (void)
{
}

void
cob_field_display (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *scroll,
		  const int attr)
{
}

void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *scroll,
		  const int attr)
{
}

void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column)
{
}

void
cob_screen_accept (cob_screen *s, cob_field *line, cob_field *column)
{
}

void
cob_screen_line_col (cob_field *f, const int l_or_c)
{
}

void
cob_screen_set_mode (const size_t smode)
{
}

int
CBL_OC_KEISEN (unsigned char * cmd, unsigned char * line, unsigned char * col, unsigned char * lng1, unsigned char * lng2, unsigned char * color, unsigned char * prn)
{
}

int
CBL_OC_ATTRIBUTE (unsigned char *line, unsigned char *col, unsigned char *lng, unsigned char *fcolor, unsigned char *bcolor, unsigned char *attr, unsigned char *flg)
{
}

#endif
