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
#include <stdarg.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

static unsigned char	*term_buff;
static const int	bin_digits[] = { 1, 3, 5, 8, 10, 13, 15, 17, 20 };

/*
 * DISPLAY
 */

static void
display_numeric (cob_field *f, FILE *fp)
{
	int		i;
	int		digits;
	int		scale;
	int		size;
	cob_field_attr	attr;
	cob_field	temp;
	unsigned char	data[128];

	if (f->size == 0) {
		return;
	}
	digits = COB_FIELD_DIGITS (f);
	scale = COB_FIELD_SCALE (f);
	size = digits + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0);
	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, digits, scale, 0, NULL);
	temp.size = size;
	temp.data = data;
	temp.attr = &attr;
	if (COB_FIELD_HAVE_SIGN (f)) {
		attr.flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE;
		if (COB_FIELD_SIGN_LEADING (f)
		    || COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_BINARY) {
			attr.flags |= COB_FLAG_SIGN_LEADING;
		}
	}

	cob_move (f, &temp);
	for (i = 0; i < size; ++i) {
		putc (data[i], fp);
	}
}

static void
pretty_display_numeric (cob_field *f, FILE *fp)
{
	unsigned char	*p;
	int		i;
	int		digits;
	int		scale;
	int		size;
	cob_field_attr	attr;
	cob_field	temp;
	unsigned char	pic[64];
	unsigned char	data[256];

	if (f->size == 0) {
		return;
	}
/* RXW
	if (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_BINARY) {
		digits = bin_digits[f->size];
	} else {
*/
		digits = COB_FIELD_DIGITS (f);
/* RXW
	}
*/
	scale = COB_FIELD_SCALE (f);
	size = (digits + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0)
		+ (scale > 0 ? 1 : 0));
	p = pic;
	temp.size = size;
	temp.data = data;
	temp.attr = &attr;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_EDITED, digits, scale, 0, (char *)pic);
	memset (pic, 0, sizeof (pic));
	memset (data, 0, sizeof (data));
	if (COB_FIELD_HAVE_SIGN (f)) {
		*p++ = '+';
		i = 1;
		memcpy (p, (unsigned char *)&i, sizeof(int));
		p += sizeof(int);
	}
	if (scale > 0) {
		*p++ = '9';
		i = digits - scale;
		memcpy (p, (unsigned char *)&i, sizeof(int));
		p += sizeof(int);
		*p++ = cob_current_module->decimal_point;
		i = 1;
		memcpy (p, (unsigned char *)&i, sizeof(int));
		p += sizeof(int);
		*p++ = '9';
		i = scale;
		memcpy (p, (unsigned char *)&i, sizeof(int));
		p += sizeof(int);
	} else {
		*p++ = '9';
		i = digits;
		memcpy (p, (unsigned char *)&i, sizeof(int));
		p += sizeof(int);
	}

	cob_move (f, &temp);
	for (i = 0; i < size; ++i) {
		putc (data[i], fp);
	}
}

static void
display_alnum (cob_field *f, FILE *fp)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		putc (f->data[i], fp);
	}
}

static void
display (cob_field *f, FILE *fp)
{
	unsigned char	*p;
	int		n;
	cob_field	temp;
	cob_field_attr	attr;

	if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_DOUBLE) {
		double f1doub;

		memcpy ((char *)&f1doub, f->data, sizeof (double));
		fprintf (fp, "%-.18lf", f1doub);
	} else if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_FLOAT) {
		float f1float;

		memcpy ((char *)&f1float, f->data, sizeof (float));
		fprintf (fp, "%-.18lf", (double)f1float);
	} else if (COB_FIELD_IS_POINTER (f)) {
		fprintf (fp, "0x");
#ifdef WORDS_BIGENDIAN
		p = f->data;
		for (n = 0; n < sizeof(void *); ++n, ++p) {
#else
		p = f->data + sizeof(void *) - 1;
		for (n = sizeof(void *) - 1; n >= 0; --n, --p) {
#endif
			fprintf (fp, "%x%x", *p >> 4, *p & 0xF);
		}
	} else if (COB_FIELD_REAL_BINARY(f) ||
		   (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_BINARY
		    && !cob_current_module->flag_pretty_display)) {
		attr = *f->attr;
		temp = *f;
		attr.digits = bin_digits[f->size];
		temp.attr = &attr;
		display_numeric (&temp, fp);
	} else if (COB_FIELD_IS_NUMERIC (f)) {
		if (cob_current_module->flag_pretty_display) {
			pretty_display_numeric (f, fp);
		} else {
			display_numeric (f, fp);
		}
	} else {
		display_alnum (f, fp);
	}
}

void
cob_display (const int outorerr, const int newline, const int varcnt, ...)
{
	FILE		*fp;
	cob_field	*f;
	int		i;
	va_list		args;

	if (!outorerr && !cob_screen_initialized) {
		fp = stdout;
	} else {
		fp = stderr;
	}
	va_start (args, varcnt);
	for (i = 0; i < varcnt; ++i) {
		f = va_arg (args, cob_field *);
		display (f, fp);
	}
	va_end (args);
	if (newline) {
		putc ('\n', fp);
		fflush (fp);
	}
}

/*
 * ACCEPT
 */

void
cob_accept (cob_field *f)
{
/* RXW
	size_t		size;
*/
	cob_field_attr	attr;
	cob_field	temp;

	if (cob_screen_initialized) {
		cob_field_accept (f, NULL, NULL, NULL, NULL, NULL, 0);
		return;
	}
	temp.data = term_buff;
	temp.attr = &attr;
	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
	/* read a line */
	if (fgets ((char *)term_buff, COB_MEDIUM_BUFF, stdin) == NULL) {
		temp.size = 1;
		term_buff[0] = ' ';
		term_buff[1] = 0;
	} else {
		temp.size = strlen ((char *)term_buff) - 1;
	}
	if (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_DISPLAY) {
		if (temp.size > f->size) {
			temp.size = f->size;
		}
	}
	cob_hankaku_move (&temp, f);
/* RXW
	if (isatty (fileno (stdin))) {
		temp.size = strlen ((char *)term_buff) - 1;
		cob_move (&temp, f);
	} else {
		size = strlen ((char *)term_buff) - 1;
		if (size > f->size) {
			size = f->size;
		}
		memcpy (f->data, term_buff, size);
		memset (f->data + size, ' ', f->size - size);
	}
*/
}

void
cob_init_termio (void)
{
	term_buff = cob_malloc (COB_MEDIUM_BUFF);
}
