/*
 * Copyright (C) 2002-2009 Keisuke Nishida
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
#include <ctype.h>
#include <math.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

static size_t		lastsize = 0;
static unsigned char	*lastdata = NULL;

static const int	cob_exp10[10] = {
	1,
	10,
	100,
	1000,
	10000,
	100000,
	1000000,
	10000000,
	100000000,
	1000000000
};

static const long long	cob_exp10LL[19] = {
	1LL,
	10LL,
	100LL,
	1000LL,
	10000LL,
	100000LL,
	1000000LL,
	10000000LL,
	100000000LL,
	1000000000LL,
	10000000000LL,
	100000000000LL,
	1000000000000LL,
	10000000000000LL,
	100000000000000LL,
	1000000000000000LL,
	10000000000000000LL,
	100000000000000000LL,
	1000000000000000000LL
};

static COB_INLINE int
cob_min_int (const int x, const int y)
{
	if (x < y) {
		return x;
	}
	return y;
}

static COB_INLINE int
cob_max_int (const int x, const int y)
{
	if (x > y) {
		return x;
	}
	return y;
}

static COB_INLINE void
own_byte_memcpy (unsigned char *s1, const unsigned char *s2, size_t size)
{
	do {
		*s1++ = *s2++;
	} while (--size);
}

static void
store_common_region (cob_field *f, const unsigned char *data,
		     const size_t size, const int scale)
{
	const unsigned char	*p;
	unsigned char		*q;
	size_t			csize;
	size_t			cinc;
	int			lf1 = -scale;
	int			lf2 = -COB_FIELD_SCALE (f);
	int			hf1 = (int) size + lf1;
	int			hf2 = (int) COB_FIELD_SIZE (f) + lf2;
	int			lcf;
	int			gcf;

	lcf = cob_max_int (lf1, lf2);
	gcf = cob_min_int (hf1, hf2);
	memset (COB_FIELD_DATA (f), '0', COB_FIELD_SIZE (f));
	if (gcf > lcf) {
		csize = (size_t)(gcf - lcf);
		p = data + hf1 - gcf;
		q = COB_FIELD_DATA (f) + hf2 - gcf;
		for (cinc = 0; cinc < csize; ++cinc, ++p, ++q) {
			if (unlikely(*p == ' ')) {
				*q = (unsigned char)'0';
			} else {
				*q = *p;
			}
		}
	}
}

static long long
cob_binary_mget_int64 (const cob_field * const f)
{
	long long	n = 0;
	size_t		fsiz = 8 - f->size;

/* Experimental code - not activated */
#if 0
	unsigned char	*s;

	if ((COB_FIELD_BINARY_SWAP (f) && !COB_FIELD_HAVE_SIGN (f)) ||
	    (!COB_FIELD_BINARY_SWAP (f) && COB_FIELD_HAVE_SIGN (f))) {
		s = (unsigned char *)&n + fsiz;
	} else {
		s = (unsigned char *)&n;
	}
	own_byte_memcpy (s, f->data, f->size);
	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
	}
	if (COB_FIELD_HAVE_SIGN (f)) {
		n >>= 8 * fsiz;	/* shift with sign */
	}
#endif
#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		if (COB_FIELD_HAVE_SIGN (f)) {
			own_byte_memcpy ((unsigned char *)&n, f->data, f->size);
			n = COB_BSWAP_64 (n);
			n >>= 8 * fsiz;	/* shift with sign */
		} else {
			own_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
			n = COB_BSWAP_64 (n);
		}
	} else {
		if (COB_FIELD_HAVE_SIGN (f)) {
			own_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
			n >>= 8 * fsiz;	/* shift with sign */
		} else {
			own_byte_memcpy ((unsigned char *)&n, f->data, f->size);
		}
	}
#else	/* WORDS_BIGENDIAN */
	if (COB_FIELD_HAVE_SIGN (f)) {
		own_byte_memcpy ((unsigned char *)&n, f->data, f->size);
		n >>= 8 * fsiz;	/* shift with sign */
	} else {
		own_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
	}
#endif	/* WORDS_BIGENDIAN */
	return n;
}

static void
cob_binary_mset_int64 (cob_field *f, long long n)
{
#ifndef WORDS_BIGENDIAN
	unsigned char	*s;

	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
		s = ((unsigned char *)&n) + 8 - f->size;
	} else {
		s = (unsigned char *)&n;
	}
	own_byte_memcpy (f->data, s, f->size);
#else	/* WORDS_BIGENDIAN */
	own_byte_memcpy (f->data, ((unsigned char *)&n) + 8 - f->size, f->size);
#endif	/* WORDS_BIGENDIAN */
}

/*
 * Display
 */

static void
cob_move_alphanum_to_display (cob_field *f1, cob_field *f2)
{
	unsigned char	*p;
	unsigned char	*s1;
	unsigned char	*s2;
	unsigned char	*e1;
	unsigned char	*e2;
	int		sign, count, size;
	unsigned char	c;

	/* initialize */
	s1 = f1->data;
	e1 = s1 + f1->size;
	s2 = COB_FIELD_DATA (f2);
	e2 = s2 + COB_FIELD_SIZE (f2);
	memset (f2->data, '0', f2->size);

	/* skip white spaces */
	for (; s1 < e1; ++s1) {
		if (!isspace (*s1)) {
			break;
		}
	}

	/* check for sign */
	sign = 0;
	if (s1 != e1) {
		if (*s1 == '+' || *s1 == '-') {
			sign = (*s1++ == '+') ? 1 : -1;
		}
	}

	/* count the number of digits before decimal point */
	count = 0;
	for (p = s1; p < e1 && *p != cob_current_module->decimal_point; ++p) {
		if (isdigit (*p)) {
			++count;
		}
	}

	/* find the start position */
	size = (int) COB_FIELD_SIZE (f2) - COB_FIELD_SCALE(f2);
	if (count < size) {
		s2 += size - count;
	} else {
		while (count-- > size) {
			while (!isdigit (*s1++)) {
				;
			}
		}
	}

	/* move */
	count = 0;
	for (; s1 < e1 && s2 < e2; ++s1) {
		c = *s1;
		if (isdigit (c)) {
			*s2++ = c;
		} else if (c == cob_current_module->decimal_point) {
			if (count++ > 0) {
				goto error;
			}
		} else if (!(isspace (c) || c == cob_current_module->numeric_separator)) {
			goto error;
		}
	}

	cob_put_sign (f2, sign);
	return;

error:
	memset (f2->data, '0', f2->size);
	cob_put_sign (f2, 0);
}

static void
cob_move_display_to_display (cob_field *f1, cob_field *f2)
{
	int	sign;

	sign = cob_get_sign (f1);
	store_common_region (f2, COB_FIELD_DATA (f1), COB_FIELD_SIZE (f1),
			     COB_FIELD_SCALE (f1));

	cob_put_sign (f1, sign);
	cob_put_sign (f2, sign);
}

static void
cob_move_display_to_alphanum (cob_field *f1, cob_field *f2)
{
	unsigned char	*data1;
	unsigned char	*data2;
	size_t		size1;
	size_t		size2;
	int		sign;
	int		diff;
	int		zero_size;

	data1 = COB_FIELD_DATA (f1);
	size1 = COB_FIELD_SIZE (f1);
	sign = cob_get_sign (f1);
	data2 = f2->data;
	size2 = f2->size;
	if (size1 >= size2) {
		memcpy (data2, data1, size2);
	} else {
		diff = (int)(size2 - size1);
		zero_size = 0;
		/* move */
		memcpy (data2, data1, size1);
		/* implied 0 ('P's) */
		if (COB_FIELD_SCALE(f1) < 0) {
			zero_size = cob_min_int ((int)-COB_FIELD_SCALE(f1), diff);
			memset (data2 + size1, '0', (size_t)zero_size);
		}
		/* padding */
		if (diff - zero_size > 0) {
			memset (data2 + size1 + zero_size, ' ', (size_t)(diff - zero_size));
		}
	}

	cob_put_sign (f1, sign);
}

static void
cob_move_alphanum_to_alphanum (cob_field *f1, cob_field *f2)
{
	unsigned char	*data1;
	unsigned char	*data2;
	size_t		size1;
	size_t		size2;

	data1 = f1->data;
	size1 = f1->size;
	data2 = f2->data;
	size2 = f2->size;
	if (size1 >= size2) {
		/* move string with truncation */
		if (COB_FIELD_JUSTIFIED (f2)) {
			memcpy (data2, data1 + size1 - size2, size2);
		} else {
			memcpy (data2, data1, size2);
		}
	} else {
		/* move string with padding */
		if (COB_FIELD_JUSTIFIED (f2)) {
			memset (data2, ' ', size2 - size1);
			memcpy (data2 + size2 - size1, data1, size1);
		} else {
			memcpy (data2, data1, size1);
			memset (data2 + size1, ' ', size2 - size1);
		}
	}
}

static void
cob_move_alphanum_to_national (cob_field *f1, cob_field *f2)
{
	/* I18N_UTF8: Use UTF-8 3bytes/char SPC for padding. */
	unsigned char	*data1;
	unsigned char	*data2;
	size_t		size1;
	size_t		size2;
#ifndef	I18N_UTF8
	int		i;
#endif /*I18N_UTF8*/
	int		len;

	data1 = f1->data;
	size1 = f1->size;
	data2 = f2->data;
	size2 = f2->size;
	len = size2 - size1;
	if (size1 >= size2) {
		/* move string with truncation */
		if (COB_FIELD_JUSTIFIED (f2)) {
			memcpy (data2, data1+size1-size2, size2);
		} else {
			memcpy (data2, data1, size2);
		}
	} else {
		/* move string with padding */
		memset (data2, ' ', size2);
		if (COB_FIELD_JUSTIFIED (f2)) {
#ifndef	I18N_UTF8
			for (i = 0; i < len; i += COB_ZENCSIZ) {
				if (len-i >= COB_ZENCSIZ) {
					memcpy (data2+i, COB_ZENBLK, COB_ZENCSIZ);
				}
			}
#endif /*I18N_UTF8*/
			memcpy (data2+len, data1, size1);
		} else {
#ifndef	I18N_UTF8
			for (i = 0; i < len; i += COB_ZENCSIZ) {
				if (len-i >= COB_ZENCSIZ) {
					memcpy (data2+size1+i, COB_ZENBLK, COB_ZENCSIZ);
				}
			}
#endif /*I18N_UTF8*/
			memcpy (data2, data1, size1);
		}
	}
}

/*
 * Packed decimal
 */

static void
cob_move_display_to_packed (cob_field *f1, cob_field *f2)
{
	unsigned char	*data1;
	unsigned char	*data2;
	unsigned char	*p;
	size_t		digits1;
	size_t		digits2;
	size_t		i;
	size_t		offset;
	int		sign;
	int		scale1;
	int		scale2;
	int		flag_null;
	unsigned char	n;

	sign = cob_get_sign (f1);
	data1 = COB_FIELD_DATA (f1);
	digits1 = COB_FIELD_DIGITS (f1);
	scale1 = COB_FIELD_SCALE (f1);
	data2 = f2->data;
	digits2 = COB_FIELD_DIGITS (f2);
	scale2 = COB_FIELD_SCALE (f2);

	/* null check */
	flag_null = 1;
	for (i = 0; i < digits1; i++) {
		if (f1->data[i] != 0) flag_null = 0;
	}
	if (flag_null) {
		memset (f2->data, 0, f2->size);
		return;
	}

	/* pack string */
	memset (f2->data, 0, f2->size);
	offset = 1 - (digits2 % 2);
	p = data1 + (digits1 - scale1) - (digits2 - scale2);
	for (i = offset; i < digits2 + offset; ++i, ++p) {
		if (*p == ' ') {
			n = 0;
		} else {
			n = (data1 <= p && p < data1 + digits1) ? cob_d2i (*p) : 0;
		}
		if (i % 2 == 0) {
			data2[i / 2] = n << 4;
		} else {
			data2[i / 2] |= n;
		}
	}

	cob_put_sign (f1, sign);
	p = f2->data + f2->size - 1;
	if (!COB_FIELD_HAVE_SIGN (f2)) {
		*p = (*p & 0xf0) | 0x0f;
	} else if (sign < 0) {
		*p = (*p & 0xf0) | 0x0d;
	} else {
		*p = (*p & 0xf0) | 0x0c;
	}
}

static void
cob_move_packed_to_display (cob_field *f1, cob_field *f2)
{
	unsigned char	*data;
	size_t		i;
	size_t		offset;
	int		sign;
	unsigned char	buff[64];

	/* unpack string */
	data = f1->data;
	sign = cob_get_sign (f1);
	offset = 1 - (COB_FIELD_DIGITS(f1) % 2);
	for (i = offset; i < COB_FIELD_DIGITS(f1) + offset; ++i) {
		if (i % 2 == 0) {
			buff[i - offset] = cob_i2d (data[i / 2] >> 4);
		} else {
			buff[i - offset] = cob_i2d (data[i / 2] & 0x0f);
		}
	}

	/* store */
	store_common_region (f2, buff, COB_FIELD_DIGITS (f1), COB_FIELD_SCALE (f1));

	cob_put_sign (f2, sign);
}

/*
 * Floating point
 */

static void
cob_move_display_to_fp (cob_field *f1, cob_field *f2)
{
	double		val;
	size_t		size;
	int		sign = cob_get_sign (f1);
	size_t		size1 = COB_FIELD_SIZE (f1);
	char		*data1;
	char		buff2[64];

	memset ((ucharptr)buff2, 0, sizeof (buff2));
	size = size1 - COB_FIELD_SCALE(f1);
	if (sign < 0) {
		buff2[0] = '-';
		data1 = &buff2[1];
	} else {
		data1 = buff2;
	}
	if (COB_FIELD_SCALE(f1) <= 0) {
		snprintf (data1, 63, "%*.*s.0", (int)size, (int)size, f1->data);
	} else {
		snprintf (data1, 63, "%*.*s.%*.*s", (int)size, (int)size, f1->data,
			 COB_FIELD_SCALE(f1), COB_FIELD_SCALE(f1), f1->data + size);
	}
	sscanf (buff2, "%lf", &val);
	if (COB_FIELD_TYPE (f2) == COB_TYPE_NUMERIC_FLOAT) {
		float	flval = (float) val;

		memcpy (f2->data, (ucharptr)&flval, sizeof(float));
	} else {
		memcpy (f2->data, (ucharptr)&val, sizeof(double));
	}
}

static void
cob_move_fp_to_display (cob_field *f1, cob_field *f2)
{
	double		val;
	double		frac;
	double		intgr;
	int		sign;
	int		decs;
	long long	res;
	char		*x, *y;
	char		buff[64];
	char		buff2[64];
	COB_UNUSED(frac);

	memset ((ucharptr)buff, 0, sizeof (buff));
	memset ((ucharptr)buff2, 0, sizeof (buff2));
	if (COB_FIELD_TYPE (f1) == COB_TYPE_NUMERIC_FLOAT) {
		float	flval;

		memcpy ((ucharptr)&flval, f1->data, sizeof (float));
		val = flval;
	} else {
		memcpy ((ucharptr)&val, f1->data, sizeof (double));
	}
	sign = 1;
	if (val < 0) {
		sign = -1;
		val = -val;
	}
	frac = modf (val, &intgr);
	res = (long long) intgr;
	decs = 0;
	for (; res; res /= 10) {
		++decs;
	}
	snprintf (buff2, 63, "%-18.*lf", 18 - decs, val);
	y = buff;
	for (x = buff2; *x; ++x) {
		if (*x == '.') {
			continue;
		}
		if (*x == ' ') {
			continue;
		}
		*y++ = *x;
	}

	store_common_region (f2, (ucharptr)buff, strlen (buff), 18 - decs);
	cob_put_sign (f2, sign);
}

/*
 * Binary integer
 */

static void
cob_move_display_to_binary (cob_field *f1, cob_field *f2)
{
	unsigned char	*data1;
	size_t		i, size;
	size_t		size1;
	long long	val = 0;
	int		sign;
	int		flag_null;

	size1 = COB_FIELD_SIZE (f1);
	data1 = COB_FIELD_DATA (f1);
	sign = cob_get_sign (f1);

	/* null check */
	flag_null = 1;
	for (i = 0; i < size1; i++) {
		if (f1->data[i] != 0) flag_null = 0;
	}
	if (flag_null) {
		memset (f2->data, 0, f2->size);
		return;
	}

	/* get value */
	size = size1 - COB_FIELD_SCALE(f1) + COB_FIELD_SCALE(f2);
	for (i = 0; i < size; ++i) {
		if (i < size1) {
			val = val * 10 + cob_d2i (data1[i]);
		} else {
			val = val * 10;
		}
	}
	if (sign < 0 && COB_FIELD_HAVE_SIGN (f2)) {
		val = -val;
	}
	if (cob_current_module->flag_binary_truncate &&
	    !COB_FIELD_REAL_BINARY(f2)) {
		val %= cob_exp10LL[(int)COB_FIELD_DIGITS(f2)];
	}

	/* store */
	cob_binary_mset_int64 (f2, val);

	cob_put_sign (f1, sign);
}

static void
cob_move_binary_to_display (cob_field *f1, cob_field *f2)
{
	int			i, sign;
	unsigned long long	val;
	long long		val2;
	char			buff[64];	/* long long is at most 20 digits */

	sign = 1;
	/* get value */
	if (COB_FIELD_HAVE_SIGN (f1)) {
		val2 = cob_binary_mget_int64 (f1);
		if (val2 < 0) {
			sign = -1;
			val = -val2;
		} else {
			val = val2;
		}
	} else {
		val = cob_binary_mget_int64 (f1);
	}

	/* convert to string */
	i = 20;
	while (val > 0) {
		buff[--i] = (char) cob_i2d (val % 10);
		val /= 10;
	}

	/* store */
	store_common_region (f2, (ucharptr)buff + i, (size_t)(20 - i),
		COB_FIELD_SCALE(f1));

	cob_put_sign (f2, sign);
}

/*
 * Edited
 */

static void
cob_move_display_to_edited (cob_field *f1, cob_field *f2)
{
	const char	*p;
	unsigned char	*min, *max, *src, *dst, *end;
	unsigned char	*decimal_point;
	int		sign;
	int		neg;
	int		count = 0;
	int		count_sign = 1;
	int		count_curr = 1;
	int		trailing_sign = 0;
	int		trailing_curr = 0;
	int		is_zero = 1;
	int		suppress_zero = 1;
	int		sign_first = 0;
	int		p_is_left = 0;
	int		repeat;
	int		n;
	unsigned char	pad = ' ';
	unsigned char	x;
	unsigned char	c;
	unsigned char	sign_symbol = 0;
	unsigned char	curr_symbol = 0;

	decimal_point = NULL;
	sign = cob_get_sign (f1);
	neg = (sign < 0) ? 1 : 0;
	/* count the number of digit places before decimal point */
	for (p = COB_FIELD_PIC (f2); *p; p += 5) {
		c = p[0];
		memcpy ((unsigned char *)&repeat, p + 1, sizeof(int));
		if (c == '9' || c == 'Z' || c == '*') {
			count += repeat;
			count_sign = 0;
			count_curr = 0;
		} else if (count_curr && c == cob_current_module->currency_symbol) {
			count += repeat;
		} else if (count_sign && (c == '+' || c == '-')) {
			count += repeat;
		} else if (c == 'P') {
			if (count == 0) {
				p_is_left = 1;
				break;
			} else {
				count += repeat;
				count_sign = 0;
				count_curr = 0;
			}
		} else if (c == 'V' || c == cob_current_module->decimal_point) {
			break;
		}
	}

	min = COB_FIELD_DATA (f1);
	max = min + COB_FIELD_SIZE (f1);
	src = max - COB_FIELD_SCALE(f1) - count;
	dst = f2->data;
	end = f2->data + f2->size;
	for (p = COB_FIELD_PIC (f2); *p;) {
		c = *p++;	/* PIC char */
		memcpy ((unsigned char *)&n, p, sizeof(int));	/* PIC char count */
		p += sizeof(int);
		for (; n > 0; n--, ++dst) {
			switch (c) {
			case '0':
			case '/':
				*dst = c;
				break;

			case 'B':
				*dst = suppress_zero ? pad : 'B';
				break;

			case 'P':
				if (p_is_left) {
					++src;
					--dst;
				}
				break;

			case '9':
				*dst = (min <= src && src < max) ? *src++ : (src++, '0');
				if (*dst != '0') {
					is_zero = suppress_zero = 0;
				}
				suppress_zero = 0;
				trailing_sign = 1;
				trailing_curr = 1;
				break;

			case 'V':
				--dst;
				decimal_point = dst;
				break;

			case '.':
			case ',':
				if (c == cob_current_module->decimal_point) {
					*dst = cob_current_module->decimal_point;
					decimal_point = dst;
				} else {
					*dst = suppress_zero ? pad : c;
				}
				break;

			case 'C':
			case 'D':
				end = dst;
				memcpy (dst++, neg ? (c == 'C' ? "CR" : "DB") : "  ", 2);
				break;

			case 'Z':
			case '*':
				x = (min <= src && src < max) ? *src++ : (src++, '0');
				if (x != '0') {
					is_zero = suppress_zero = 0;
				}
				pad = (c == '*') ? '*' : ' ';
				*dst = suppress_zero ? pad : x;
				trailing_sign = 1;
				trailing_curr = 1;
				break;

			case '+':
			case '-':
				x = (min <= src && src < max) ? *src++ : (src++, '0');
				if (x != '0') {
					is_zero = suppress_zero = 0;
				}
				if (trailing_sign) {
					*dst = neg ? '-' : (c == '+') ? '+' : ' ';
					--end;
				} else if (dst == f2->data || suppress_zero) {
					*dst = pad;
					sign_symbol = neg ? '-' : (c == '+') ? '+' : ' ';
					if (!curr_symbol) {
						++sign_first;
					}
				} else {
					*dst = x;
				}
				break;

			default:
				if (c == cob_current_module->currency_symbol) {
					x = (min <= src && src < max) ? *src++ : (src++, '0');
					if (x != '0') {
						is_zero = suppress_zero = 0;
					}
					if (trailing_curr) {
						*dst = cob_current_module->currency_symbol;
						--end;
					} else if (dst == f2->data || suppress_zero) {
						*dst = pad;
						curr_symbol = cob_current_module->currency_symbol;
					} else {
						*dst = x;
					}
					break;
				}

				*dst = '?';	/* invalid PIC */
			}
		}
	}

	if (suppress_zero || (is_zero && COB_FIELD_BLANK_ZERO (f2))) {
		/* all digits are zeros */
		if (pad == ' ' || COB_FIELD_BLANK_ZERO (f2)) {
			memset (f2->data, ' ', f2->size);
		} else {
			for (dst = f2->data; dst < f2->data + f2->size; ++dst) {
				if (*dst != cob_current_module->decimal_point) {
					*dst = pad;
				}
			}
		}
	} else {
		/* put zero after the decimal point if necessary */
		if (decimal_point) {
			for (dst = decimal_point + 1; dst < end; ++dst) {
				if (!isdigit (*dst) && !strchr (",+-/B", *dst)) {
					*dst = '0';
				}
			}
		}

		/* put sign or currency symbol at the beginning */
		if (sign_symbol || curr_symbol) {
			for (dst = end - 1; dst > f2->data; --dst) {
				if (*dst == ' ') {
					break;
				}
			}
			if (sign_symbol && curr_symbol) {
				if (sign_first) {
					*dst = curr_symbol;
					--dst;
					if (dst >= f2->data) {
						*dst = sign_symbol;
					}
				} else {
					*dst = sign_symbol;
					--dst;
					if (dst >= f2->data) {
						*dst = curr_symbol;
					}
				}
			} else if (sign_symbol) {
				*dst = sign_symbol;
			} else {
				*dst = curr_symbol;
			}
		}

		/* replace all 'B's by pad */
		count = 0;
		for (dst = f2->data; dst < end; ++dst) {
			if (*dst == 'B') {
				if (count == 0) {
					*dst = pad;
				} else {
					*dst = ' ';
				}
			} else {
				++count;
			}
		}
	}

	cob_put_sign (f1, sign);
}

static void
cob_move_edited_to_display (cob_field *f1, cob_field *f2)
{
	unsigned char	*p;
	const char	*p1;
	size_t		i;
	int		sign = 0;
	int		scale = 0;
	int		count = 0;
	int		have_point = 0;
	int		cp;
	int		n;
	unsigned char	c;
	unsigned char	buff[64];

	p = buff;
	/* de-edit */
	for (i = 0; i < f1->size; ++i) {
		cp = f1->data[i];
		switch (cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			*p++ = cp;
			if (have_point) {
				++scale;
			}
			break;
		case '.':
		case ',':
			if (cp == cob_current_module->decimal_point) {
				have_point = 1;
			}
			break;
		case '-':
		case 'C':
			sign = -1;
			break;
		}
	}
	/* count the number of digit places after decimal point in case of 'V', 'P' */
	if (scale == 0) {
		for (p1 = COB_FIELD_PIC (f1); *p1; p1 += 5) {
			c = p1[0];
			memcpy ((unsigned char *)&n, p1 + 1, sizeof(int));
			if (c == '9'  || c == '0' || c == 'Z' || c == '*') {
				if (have_point) {
					scale += n;
				} else {
					count += n;
				}
			} else if (c == 'P') {
				if (count == 0) {
					have_point = 1;
					scale += n;
				} else {
					scale -= n;
				}
			} else if (c == 'V') {
				have_point = 1;
			}
		}
	}

	/* store */
	store_common_region (f2, buff, (size_t)(p - buff), scale);

	cob_put_sign (f2, sign);
}

static void
cob_move_alphanum_to_edited (cob_field *f1, cob_field *f2)
{
	const char	*p;
	unsigned char	*max, *src, *dst;
	int		sign = cob_get_sign (f1);
	int		n;
	unsigned char	c;

	src = COB_FIELD_DATA (f1);
	max = src + COB_FIELD_SIZE (f1);
	dst = f2->data;
	for (p = COB_FIELD_PIC (f2); *p;) {
		c = *p++;	/* PIC char */
		memcpy ((unsigned char *)&n, p, sizeof(int));	/* PIC char count */
		p += sizeof(int);
		for (; n > 0; --n) {
			switch (c) {
			case 'A':
			case 'X':
			case '9':
				*dst++ = (src < max) ? *src++ : ' ';
				break;
			case '0':
			case '/':
				*dst++ = c;
				break;
			case 'B':
				*dst++ = ' ';
				break;
			default:
				*dst++ = '?';	/* invalid PIC */
			}
		}
	}
	cob_put_sign (f1, sign);
}

static void
cob_move_alphanum_to_national_edited (cob_field *f1, cob_field *f2)
{
	const char	*p;
	unsigned char	*max, *src, *dst;
	int		n;
	unsigned char	c;

	src = COB_FIELD_DATA (f1);
	max = src + COB_FIELD_SIZE (f1);
	dst = f2->data;

	/* move string with padding */
	memset (dst, ' ', f2->size);

	for (p = COB_FIELD_PIC (f2); *p;) {
		c = *p++;	/* PIC char */
		memcpy ((unsigned char *)&n, p, sizeof (int));	/* PIC char count */
		p += sizeof (int);
		for (; n > 0; --n) {
			switch (c) {
			case 'N':
			/* I18N_UTF8: NATIONAL uses 3bytes/char for BMP. */
				if (src < max) {
					*dst++ = *src++;
					*dst++ = *src++;
#ifdef	I18N_UTF8
					*dst++ = *src++;
#else /*!I18N_UTF8*/
				} else {
					memcpy (dst, COB_ZENBLK, COB_ZENCSIZ);
					dst += COB_ZENCSIZ;
#endif /*I18N_UTF8*/
				}
				break;
			case '/':
				memcpy (dst, COB_ZENSLAS, COB_ZENCSIZ);
				dst += COB_ZENCSIZ;
				break;
			case 'B':
				memcpy (dst, COB_ZENSPC, COB_ZENCSIZ);
				dst += COB_ZENCSIZ;
				break;
			case '0':
				memcpy (dst, COB_ZENZERO, COB_ZENCSIZ);
				dst += COB_ZENCSIZ;
				break;
			default:
				*dst++ = '?';	/* invalid PIC */
			}
		}
	}
}

char *
han2zen (char *str, int size, int *retsize)
{
#ifdef	I18N_UTF8
	char *p = (char *)cob_national ((unsigned char *)str, size);
	*retsize = strlen (p);
	return p;
#else /*!I18N_UTF8*/
	char		*buf, *p, *ptr;
	unsigned char	c, d = '\0';
	int		i;

	p = str;
	for (i = size-1; *p != '\0' && i >= 0; i--);

	if (i >= 0) {
		size = i;
	}

	buf = (char *)calloc (size*2+1, sizeof (char));

	for (i = 0, ptr = str, p = buf; i < size; ptr++, i++) {
		c = (unsigned char) *ptr;
		switch (c) {
		case    0x20: strcpy(p, "\x81\x40");p+=2;break;
		case    0x21: strcpy(p, "\x81\x49");p+=2;break;
		case    0x22: strcpy(p, "\x81\x68");p+=2;break;
		case    0x23: strcpy(p, "\x81\x94");p+=2;break;
		case    0x24: strcpy(p, "\x81\x90");p+=2;break;
		case    0x25: strcpy(p, "\x81\x93");p+=2;break;
		case    0x26: strcpy(p, "\x81\x95");p+=2;break;
		case    0x27: strcpy(p, "\x81\x66");p+=2;break;
		case    0x28: strcpy(p, "\x81\x69");p+=2;break;
		case    0x29: strcpy(p, "\x81\x6A");p+=2;break;
		case    0x2A: strcpy(p, "\x81\x96");p+=2;break;
		case    0x2B: strcpy(p, "\x81\x7B");p+=2;break;
		case    0x2C: strcpy(p, "\x81\x43");p+=2;break;
		case    0x2D: strcpy(p, "\x81\x7C");p+=2;break;
		case    0x2E: strcpy(p, "\x81\x44");p+=2;break;
		case    0x2F: strcpy(p, "\x81\x5E");p+=2;break;
		case    0x30: strcpy(p, "\x82\x4F");p+=2;break;
		case    0x31: strcpy(p, "\x82\x50");p+=2;break;
		case    0x32: strcpy(p, "\x82\x51");p+=2;break;
		case    0x33: strcpy(p, "\x82\x52");p+=2;break;
		case    0x34: strcpy(p, "\x82\x53");p+=2;break;
		case    0x35: strcpy(p, "\x82\x54");p+=2;break;
		case    0x36: strcpy(p, "\x82\x55");p+=2;break;
		case    0x37: strcpy(p, "\x82\x56");p+=2;break;
		case    0x38: strcpy(p, "\x82\x57");p+=2;break;
		case    0x39: strcpy(p, "\x82\x58");p+=2;break;
		case    0x3A: strcpy(p, "\x81\x46");p+=2;break;
		case    0x3B: strcpy(p, "\x81\x47");p+=2;break;
		case    0x3C: strcpy(p, "\x81\x83");p+=2;break;
		case    0x3D: strcpy(p, "\x81\x81");p+=2;break;
		case    0x3E: strcpy(p, "\x81\x84");p+=2;break;
		case    0x3F: strcpy(p, "\x81\x48");p+=2;break;
		case    0x40: strcpy(p, "\x81\x97");p+=2;break;
		case    0x41: strcpy(p, "\x82\x60");p+=2;break;
		case    0x42: strcpy(p, "\x82\x61");p+=2;break;
		case    0x43: strcpy(p, "\x82\x62");p+=2;break;
		case    0x44: strcpy(p, "\x82\x63");p+=2;break;
		case    0x45: strcpy(p, "\x82\x64");p+=2;break;
		case    0x46: strcpy(p, "\x82\x65");p+=2;break;
		case    0x47: strcpy(p, "\x82\x66");p+=2;break;
		case    0x48: strcpy(p, "\x82\x67");p+=2;break;
		case    0x49: strcpy(p, "\x82\x68");p+=2;break;
		case    0x4A: strcpy(p, "\x82\x69");p+=2;break;
		case    0x4B: strcpy(p, "\x82\x6A");p+=2;break;
		case    0x4C: strcpy(p, "\x82\x6B");p+=2;break;
		case    0x4D: strcpy(p, "\x82\x6C");p+=2;break;
		case    0x4E: strcpy(p, "\x82\x6D");p+=2;break;
		case    0x4F: strcpy(p, "\x82\x6E");p+=2;break;
		case    0x50: strcpy(p, "\x82\x6F");p+=2;break;
		case    0x51: strcpy(p, "\x82\x70");p+=2;break;
		case    0x52: strcpy(p, "\x82\x71");p+=2;break;
		case    0x53: strcpy(p, "\x82\x72");p+=2;break;
		case    0x54: strcpy(p, "\x82\x73");p+=2;break;
		case    0x55: strcpy(p, "\x82\x74");p+=2;break;
		case    0x56: strcpy(p, "\x82\x75");p+=2;break;
		case    0x57: strcpy(p, "\x82\x76");p+=2;break;
		case    0x58: strcpy(p, "\x82\x77");p+=2;break;
		case    0x59: strcpy(p, "\x82\x78");p+=2;break;
		case    0x5A: strcpy(p, "\x82\x79");p+=2;break;
		case    0x5B: strcpy(p, "\x81\x6D");p+=2;break;
		case    0x5C: strcpy(p, "\x81\x8F");p+=2;break;
		case    0x5D: strcpy(p, "\x81\x6E");p+=2;break;
		case    0x5E: strcpy(p, "\x81\x4F");p+=2;break;
		case    0x5F: strcpy(p, "\x81\x51");p+=2;break;
		case    0x60: strcpy(p, "\x81\x65");p+=2;break;
		case    0x61: strcpy(p, "\x82\x81");p+=2;break;
		case    0x62: strcpy(p, "\x82\x82");p+=2;break;
		case    0x63: strcpy(p, "\x82\x83");p+=2;break;
		case    0x64: strcpy(p, "\x82\x84");p+=2;break;
		case    0x65: strcpy(p, "\x82\x85");p+=2;break;
		case    0x66: strcpy(p, "\x82\x86");p+=2;break;
		case    0x67: strcpy(p, "\x82\x87");p+=2;break;
		case    0x68: strcpy(p, "\x82\x88");p+=2;break;
		case    0x69: strcpy(p, "\x82\x89");p+=2;break;
		case    0x6A: strcpy(p, "\x82\x8A");p+=2;break;
		case    0x6B: strcpy(p, "\x82\x8B");p+=2;break;
		case    0x6C: strcpy(p, "\x82\x8C");p+=2;break;
		case    0x6D: strcpy(p, "\x82\x8D");p+=2;break;
		case    0x6E: strcpy(p, "\x82\x8E");p+=2;break;
		case    0x6F: strcpy(p, "\x82\x8F");p+=2;break;
		case    0x70: strcpy(p, "\x82\x90");p+=2;break;
		case    0x71: strcpy(p, "\x82\x91");p+=2;break;
		case    0x72: strcpy(p, "\x82\x92");p+=2;break;
		case    0x73: strcpy(p, "\x82\x93");p+=2;break;
		case    0x74: strcpy(p, "\x82\x94");p+=2;break;
		case    0x75: strcpy(p, "\x82\x95");p+=2;break;
		case    0x76: strcpy(p, "\x82\x96");p+=2;break;
		case    0x77: strcpy(p, "\x82\x97");p+=2;break;
		case    0x78: strcpy(p, "\x82\x98");p+=2;break;
		case    0x79: strcpy(p, "\x82\x99");p+=2;break;
		case    0x7A: strcpy(p, "\x82\x9A");p+=2;break;
		case    0x7B: strcpy(p, "\x81\x6F");p+=2;break;
		case    0x7C: strcpy(p, "\x81\x62");p+=2;break;
		case    0x7D: strcpy(p, "\x81\x70");p+=2;break;
		case    0x7E: strcpy(p, "\x81\x60");p+=2;break;
		case    0xA1: strcpy(p, "\x81\x42");p+=2;break;
		case    0xA2: strcpy(p, "\x81\x75");p+=2;break;
		case    0xA3: strcpy(p, "\x81\x76");p+=2;break;
		case    0xA4: strcpy(p, "\x81\x41");p+=2;break;
		case    0xA5: strcpy(p, "\x81\x45");p+=2;break;
		case    0xA6: strcpy(p, "\x83\x92");p+=2;break;
		case    0xA7: strcpy(p, "\x83\x40");p+=2;break;
		case    0xA8: strcpy(p, "\x83\x42");p+=2;break;
		case    0xA9: strcpy(p, "\x83\x44");p+=2;break;
		case    0xAA: strcpy(p, "\x83\x46");p+=2;break;
		case    0xAB: strcpy(p, "\x83\x48");p+=2;break;
		case    0xAC: strcpy(p, "\x83\x83");p+=2;break;
		case    0xAD: strcpy(p, "\x83\x85");p+=2;break;
		case    0xAE: strcpy(p, "\x83\x87");p+=2;break;
		case    0xAF: strcpy(p, "\x83\x62");p+=2;break;
		case    0xB0: strcpy(p, "\x81\x5B");p+=2;break;
		case    0xB1: strcpy(p, "\x83\x41");p+=2;break;
		case    0xB2: strcpy(p, "\x83\x43");p+=2;break;
		case    0xB3: strcpy(p, "\x83\x45");p+=2;break;
		case    0xB4: strcpy(p, "\x83\x47");p+=2;break;
		case    0xB5: strcpy(p, "\x83\x49");p+=2;break;
		case    0xB6: strcpy(p, "\x83\x4A");p+=2;break;
		case    0xB7: strcpy(p, "\x83\x4C");p+=2;break;
		case    0xB8: strcpy(p, "\x83\x4E");p+=2;break;
		case    0xB9: strcpy(p, "\x83\x50");p+=2;break;
		case    0xBA: strcpy(p, "\x83\x52");p+=2;break;
		case    0xBB: strcpy(p, "\x83\x54");p+=2;break;
		case    0xBC: strcpy(p, "\x83\x56");p+=2;break;
		case    0xBD: strcpy(p, "\x83\x58");p+=2;break;
		case    0xBE: strcpy(p, "\x83\x5A");p+=2;break;
		case    0xBF: strcpy(p, "\x83\x5C");p+=2;break;
		case    0xC0: strcpy(p, "\x83\x5E");p+=2;break;
		case    0xC1: strcpy(p, "\x83\x60");p+=2;break;
		case    0xC2: strcpy(p, "\x83\x63");p+=2;break;
		case    0xC3: strcpy(p, "\x83\x65");p+=2;break;
		case    0xC4: strcpy(p, "\x83\x67");p+=2;break;
		case    0xC5: strcpy(p, "\x83\x69");p+=2;break;
		case    0xC6: strcpy(p, "\x83\x6A");p+=2;break;
		case    0xC7: strcpy(p, "\x83\x6B");p+=2;break;
		case    0xC8: strcpy(p, "\x83\x6C");p+=2;break;
		case    0xC9: strcpy(p, "\x83\x6D");p+=2;break;
		case    0xCA: strcpy(p, "\x83\x6E");p+=2;break;
		case    0xCB: strcpy(p, "\x83\x71");p+=2;break;
		case    0xCC: strcpy(p, "\x83\x74");p+=2;break;
		case    0xCD: strcpy(p, "\x83\x77");p+=2;break;
		case    0xCE: strcpy(p, "\x83\x7A");p+=2;break;
		case    0xCF: strcpy(p, "\x83\x7D");p+=2;break;
		case    0xD0: strcpy(p, "\x83\x7E");p+=2;break;
		case    0xD1: strcpy(p, "\x83\x80");p+=2;break;
		case    0xD2: strcpy(p, "\x83\x81");p+=2;break;
		case    0xD3: strcpy(p, "\x83\x82");p+=2;break;
		case    0xD4: strcpy(p, "\x83\x84");p+=2;break;
		case    0xD5: strcpy(p, "\x83\x86");p+=2;break;
		case    0xD6: strcpy(p, "\x83\x88");p+=2;break;
		case    0xD7: strcpy(p, "\x83\x89");p+=2;break;
		case    0xD8: strcpy(p, "\x83\x8A");p+=2;break;
		case    0xD9: strcpy(p, "\x83\x8B");p+=2;break;
		case    0xDA: strcpy(p, "\x83\x8C");p+=2;break;
		case    0xDB: strcpy(p, "\x83\x8D");p+=2;break;
		case    0xDC: strcpy(p, "\x83\x8F");p+=2;break;
		case    0xDD: strcpy(p, "\x83\x93");p+=2;break;
		case    0xDE:
			/* Dakuten */
			if ((d >= 0xB6 && d <= 0xC4) ||
			    (d >= 0xCA && d <= 0xCE)) {
				p--;
				(*p)++;
				p++;
			} else if (d == 0xB3) {
				p--;
				*p = 0x94;
				p++;
			} else {
				strcpy (p, "\x81\x4A");
				p+=2;
			}
			break;
		case    0xDF:
			/* Han-dakuten */
			if (d >= 0xCA && d <= 0xCE) {
				p--;
				(*p)+=2;
				p++;
			} else {
				strcpy (p, "\x81\x4B");
				p+=2;
			}
			break;
		case     0:
		case    255:
			*p = *ptr;
			p++;
			*p = *ptr;
			p++;
			if (i+1 < size) {
				if (*(ptr+1) == *ptr) {
					ptr++;
					i++;
				}
			}
			break;
		default:
			if (0 < c && c < 0x20) {
				strcpy (p, COB_SJSPC);
				p += 2;
			} else {
				*p = *ptr;
				p++;
				ptr++;
				*p = *ptr;
				p++;
				i++;
			}
			break;
		}
		d = c;
	}
	*p = '\0';
	*retsize = p-buf;
	return buf;
#endif /*I18N_UTF8*/
}

static char *
judge_hankakujpn_exist (cob_field *src, cob_field *dst, int *size)
{
	char	*tmp_zenjpn_word = NULL;

	if (src->size <= 0) {
		return NULL;
	}
	if (strlen ((char*)src->data) > 0) {
		tmp_zenjpn_word = han2zen ((char*)src->data, src->size, size);
	}
	return tmp_zenjpn_word;
}

/*
 * MOVE dispatcher
 */

static void
indirect_move (void (*func) (cob_field *src, cob_field *dst),
	       cob_field *src, cob_field *dst, size_t size, int scale)
{
	cob_field	temp;
	cob_field_attr	attr;
	unsigned char	data[64];

	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, size, scale,
			COB_FLAG_HAVE_SIGN, NULL);
	temp.size = size;
	temp.data = data;
	temp.attr = &attr;
	func (src, &temp);
	cob_move (&temp, dst);
}

static void
cob_move_all (cob_field *src, cob_field *dst)
{
	size_t			i;
	size_t			digcount;
	cob_field		temp;
	cob_field_attr		attr;
	char			*pTmp = NULL;
	cob_field		tmpSrc;
	int			size;
	int			x_to_n = 0;
#ifdef	I18N_UTF8
	int			j;
#endif /*I18N_UTF8*/

	if ((!(COB_FIELD_TYPE (src) == COB_TYPE_NATIONAL ||
	       COB_FIELD_TYPE (src) == COB_TYPE_NATIONAL_EDITED)) &&
	    (COB_FIELD_TYPE (dst) == COB_TYPE_NATIONAL ||
	     COB_FIELD_TYPE (dst) == COB_TYPE_NATIONAL_EDITED)) {
#ifdef	I18N_UTF8
		if (src == &cob_blank) {
			pTmp = cob_malloc (dst->size);
			for (j = 0; j < dst->size; j+=COB_ZENCSIZ) {
				memcpy (&(pTmp[j]), COB_ZENBLK, COB_ZENCSIZ);
			}
			size = dst->size;
		} else {
			pTmp = judge_hankakujpn_exist (src, dst, &size);
		}
#else /*!I18N_UTF8*/
		pTmp = judge_hankakujpn_exist (src, dst, &size);
#endif /*I18N_UTF8*/
		if (pTmp != NULL) {
			tmpSrc.data = (unsigned char *)pTmp;
			tmpSrc.size = size;
		} else {
			tmpSrc.size = 0;
		}
		x_to_n = 1;
	}

	if (x_to_n == 1) {
		COB_ATTR_INIT (COB_TYPE_NATIONAL, 0, 0, 0, NULL);
	} else {
		COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
	}
	if (COB_FIELD_IS_NUMERIC(dst)) {
		digcount = 18;
		attr.type = COB_TYPE_NUMERIC_DISPLAY;
		attr.digits = 18;
/*
		if (COB_FIELD_TYPE(dst) & COB_TYPE_NUMERIC_EDITED) {
			digcount = dst->size;
		} else {
			digcount = COB_FIELD_DIGITS(dst);
		}
*/
	} else {
		digcount = dst->size;
	}
	if (digcount > lastsize) {
		free (lastdata);
		lastdata = cob_malloc (digcount);
		lastsize = digcount;
	}
	temp.size = digcount;
	temp.data = lastdata;
	temp.attr = &attr;
	if ( x_to_n == 1 && tmpSrc.size > 1) {
		for (i = 0; i < digcount; ++i) {
			lastdata[i] = tmpSrc.data[i % tmpSrc.size];
		}
	} else {
		if (likely (src->size == 1)) {
			memset (lastdata, src->data[0], digcount);
		} else {
			for (i = 0; i < digcount; ++i) {
				lastdata[i] = src->data[i % src->size];
			}
#ifdef	I18N_UTF8
			/* I18N_UTF8: termination of multi octet
			   charactrer sequence is pending. */
#else /*!I18N_UTF8*/
			if ((0x81 <= lastdata[i-1] && lastdata[i-1] <= 0x9F) ||
			    (0xE0 <= lastdata[i-1] && lastdata[i-1] <= 0xFC)) {
				lastdata[i-1] = ' ';
			}
#endif /*I18N_UTF8*/
		}
	}
	cob_move (&temp, dst);
	if (pTmp != NULL) {
		free (pTmp);
	}
}

void
cob_move (cob_field *src, cob_field *dst)
{
	char		*pTmp = NULL;
	int		size;
	cob_field	srcfeild;
	cob_field	*src1;
#ifdef	I18N_UTF8
	cob_field_attr	attr;
	int		j;
#endif /*I18N_UTF8*/

	if (src == &cob_quote) {
		src1 = &cob_quote;
	} else if (src == &cob_zen_quote) {
		src1 = &cob_zen_quote;
	} else if (src == &cob_space) {
		src1 = &cob_space;
	} else if (src == &cob_zen_space) {
		src1 = &cob_zen_space;
	} else if (src == &cob_blank) {
		src1 = &cob_blank;
	} else if (src == &cob_zen_blank) {
		src1 = &cob_zen_blank;
	} else if (src == &cob_zero) {
		src1 = &cob_zero;
	} else if (src == &cob_zen_zero) {
		src1 = &cob_zen_zero;
	} else {
		memcpy (&srcfeild, src, sizeof (cob_field));
		src1 = &srcfeild;
	}

	if (COB_FIELD_TYPE (src1) == COB_TYPE_ALPHANUMERIC_ALL ||
	    COB_FIELD_TYPE (src1) == COB_TYPE_NATIONAL_ALL) {
		cob_move_all (src1, dst);
		return;
	}
	if (dst->size == 0) {
		return;
	}

	if (COB_FIELD_TYPE (src1) != COB_TYPE_GROUP) {
		if ((COB_FIELD_TYPE (src1) == COB_TYPE_NUMERIC_DISPLAY ||
		     COB_FIELD_TYPE (src1) == COB_TYPE_ALPHANUMERIC ||
		     COB_FIELD_TYPE (src1) == COB_TYPE_ALPHANUMERIC_EDITED) &&
		    (COB_FIELD_TYPE (dst) == COB_TYPE_NATIONAL ||
		     COB_FIELD_TYPE (dst) == COB_TYPE_NATIONAL_EDITED)) {
#ifdef	I18N_UTF8
			if (src1 == &cob_blank) {
				pTmp = cob_malloc (dst->size);
				for (j = 0; j < dst->size; j+=COB_ZENCSIZ) {
					memcpy (&(pTmp[j]), COB_ZENBLK, COB_ZENCSIZ);
				}
				size = dst->size;
			} else {
				pTmp = judge_hankakujpn_exist (src1, dst, &size);
			}
			COB_ATTR_INIT (COB_TYPE_NATIONAL, 0, 0, 0, NULL);
			src1->attr = &attr;
#else /*!I18N_UTF8*/
			pTmp = judge_hankakujpn_exist (src1, dst, &size);
#endif /*I18N_UTF8*/
			if (pTmp != NULL) {
				src1->data = (unsigned char *)pTmp;
				src1->size = size;
			}
			if (src1->size == 0) {
				src1 = &cob_zen_space;
			}
		}
	}
	if (src1->size == 0) {
		src1 = &cob_space;
	}

	/* non-elementary move */
	if (COB_FIELD_TYPE (src1) == COB_TYPE_GROUP || COB_FIELD_TYPE (dst) == COB_TYPE_GROUP) {
		cob_move_alphanum_to_alphanum (src1, dst);
		if (pTmp != NULL) {
			free (pTmp);
		}
		return;
	}

	/* elementary move */
	switch (COB_FIELD_TYPE (src1)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			cob_move_display_to_fp (src1, dst);
			break;
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_display_to_display (src1, dst);
			break;
		case COB_TYPE_NUMERIC_PACKED:
			cob_move_display_to_packed (src1, dst);
			break;
		case COB_TYPE_NUMERIC_BINARY:
			cob_move_display_to_binary (src1, dst);
			break;
		case COB_TYPE_NUMERIC_EDITED:
			cob_move_display_to_edited (src1, dst);
			break;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			if (COB_FIELD_SCALE (src1) < 0 ||
			    COB_FIELD_SCALE (src1) > COB_FIELD_DIGITS (src1)) {
				/* expand P's */
				indirect_move (cob_move_display_to_display, src1, dst,
					       (size_t)cob_max_int ((int)COB_FIELD_DIGITS (src1), (int)COB_FIELD_SCALE (src1)),
					       cob_max_int (0, (int)COB_FIELD_SCALE (src1)));
				break;
			} else {
				cob_move_alphanum_to_edited (src1, dst);
				break;
			}
		case COB_TYPE_NATIONAL:
			cob_move_alphanum_to_national (src1, dst);
			break;
		case COB_TYPE_NATIONAL_EDITED:
			cob_move_alphanum_to_national_edited (src1, dst);
			break;
		default:
			cob_move_display_to_alphanum (src1, dst);
			break;
		}
		break;
	case COB_TYPE_NUMERIC_PACKED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_packed_to_display (src1, dst);
			break;
		default:
			indirect_move (cob_move_packed_to_display, src1, dst,
				       COB_FIELD_DIGITS (src1), COB_FIELD_SCALE (src1));
			break;
		}
		break;
	case COB_TYPE_NUMERIC_BINARY:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_binary_to_display (src1, dst);
			break;
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_binary_to_display, src1, dst,
				       20, COB_FIELD_SCALE (src1));
			break;
		default:
			indirect_move (cob_move_binary_to_display, src1, dst,
				       COB_FIELD_DIGITS (src1), COB_FIELD_SCALE (src1));
			break;
		}
		break;
	case COB_TYPE_NUMERIC_EDITED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_edited_to_display (src1, dst);
			break;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_edited_to_display, src1, dst, 36, 18);
			break;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			cob_move_alphanum_to_edited (src1, dst);
			break;
		case COB_TYPE_NATIONAL:
			cob_move_alphanum_to_national (src1, dst);
			break;
		case COB_TYPE_NATIONAL_EDITED:
			cob_move_alphanum_to_national_edited (src1, dst);
			break;
		default:
			cob_move_alphanum_to_alphanum (src1, dst);
			break;
		}
		break;
	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
		indirect_move (cob_move_fp_to_display, src1, dst, 40, 20);
		break;
	default:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_alphanum_to_display (src1, dst);
			break;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_alphanum_to_display, src1, dst, 36, 18);
			break;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			cob_move_alphanum_to_edited (src1, dst);
			break;
		case COB_TYPE_NATIONAL_EDITED:
			cob_move_alphanum_to_national_edited (src1, dst);
			break;
		case COB_TYPE_NATIONAL:
			cob_move_alphanum_to_national (src1, dst);
			break;
		default:
			cob_move_alphanum_to_alphanum (src1, dst);
			break;
		}
		break;
	}
	if (pTmp != NULL) {
		free (pTmp);
	}
	return;
}

static void
cob_hankaku_move_all (cob_field *src, cob_field *dst)
{
	size_t			i;
	size_t			digcount;
	cob_field		temp;
	cob_field_attr		attr;

	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
	if (COB_FIELD_IS_NUMERIC (dst)) {
		digcount = 18;
		attr.type = COB_TYPE_NUMERIC_DISPLAY;
		attr.digits = 18;
	} else {
		digcount = dst->size;
	}
	if (digcount > lastsize) {
		free (lastdata);
		lastdata = cob_malloc (digcount);
		lastsize = digcount;
	}
	temp.size = digcount;
	temp.data = lastdata;
	temp.attr = &attr;
	if (likely(src->size == 1)) {
		memset (lastdata, src->data[0], digcount);
	} else {
		for (i = 0; i < digcount; ++i) {
			lastdata[i] = src->data[i % src->size];
		}
	}

	cob_hankaku_move (&temp, dst);
}

void
cob_hankaku_move (cob_field *src, cob_field *dst)
{
	if (COB_FIELD_TYPE (src) == COB_TYPE_ALPHANUMERIC_ALL ||
	    COB_FIELD_TYPE (src) == COB_TYPE_NATIONAL_ALL) {
		cob_hankaku_move_all (src, dst);
		return;
	}
	if (dst->size == 0) {
		return;
	}
	if (src->size == 0) {
		src = &cob_space;
	}

	/* non-elementary move */
	if (COB_FIELD_TYPE (src) == COB_TYPE_GROUP || COB_FIELD_TYPE (dst) == COB_TYPE_GROUP) {
		cob_move_alphanum_to_alphanum (src, dst);
		return;
	}

	/* elementary move */
	switch (COB_FIELD_TYPE (src)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			cob_move_display_to_fp (src, dst);
			return;
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_display_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
			cob_move_display_to_packed (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
			cob_move_display_to_binary (src, dst);
			return;
		case COB_TYPE_NUMERIC_EDITED:
			cob_move_display_to_edited (src, dst);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			if (COB_FIELD_SCALE(src) < 0 ||
			    COB_FIELD_SCALE(src) > COB_FIELD_DIGITS(src)) {
				/* expand P's */
				indirect_move (cob_move_display_to_display, src, dst,
					      (size_t)cob_max_int ((int)COB_FIELD_DIGITS(src), (int)COB_FIELD_SCALE(src)),
					      cob_max_int (0, (int)COB_FIELD_SCALE(src)));
				return;
			} else {
				cob_move_alphanum_to_edited (src, dst);
				return;
			}
		default:
			cob_move_display_to_alphanum (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_PACKED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_packed_to_display (src, dst);
			return;
		default:
			indirect_move (cob_move_packed_to_display, src, dst,
				      COB_FIELD_DIGITS(src), COB_FIELD_SCALE(src));
			return;
		}

	case COB_TYPE_NUMERIC_BINARY:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_binary_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_binary_to_display, src, dst,
				      20, COB_FIELD_SCALE(src));
			return;
		default:
			indirect_move (cob_move_binary_to_display, src, dst,
				      COB_FIELD_DIGITS(src), COB_FIELD_SCALE(src));
			return;
		}

	case COB_TYPE_NUMERIC_EDITED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_edited_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_edited_to_display, src, dst, 36, 18);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			cob_move_alphanum_to_edited (src, dst);
			return;
		default:
			cob_move_alphanum_to_alphanum (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
		indirect_move (cob_move_fp_to_display, src, dst, 40, 20);
		return;

	default:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_alphanum_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_alphanum_to_display, src, dst, 36, 18);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			cob_move_alphanum_to_edited (src, dst);
			return;
		case COB_TYPE_NATIONAL_EDITED:
		case COB_TYPE_NATIONAL:
			cob_move_alphanum_to_national (src, dst);
			return;
		default:
			cob_move_alphanum_to_alphanum (src, dst);
			return;
		}
	}
}

/*
 * Convenience functions
 */

static int
cob_packed_get_int (cob_field *f1)
{
	unsigned char	*data;
	size_t		i;
	size_t		offset;
	int		val = 0;
	int		sign;

	data = f1->data;
	sign = cob_get_sign (f1);
	offset = 1 - (COB_FIELD_DIGITS(f1) % 2);
	for (i = offset; i < COB_FIELD_DIGITS(f1) - COB_FIELD_SCALE(f1) + offset; ++i) {
		val *= 10;
		if (i % 2 == 0) {
			val += data[i / 2] >> 4;
		} else {
			val += data[i / 2] & 0x0f;
		}
	}
	if (sign < 0) {
		val = -val;
	}
	return val;
}

static long long
cob_packed_get_long_long (cob_field *f1)
{
	unsigned char	*data;
	size_t		i;
	size_t		offset;
	long long	val = 0;
	int		sign;

	data = f1->data;
	sign = cob_get_sign (f1);
	offset = 1 - (COB_FIELD_DIGITS(f1) % 2);
	for (i = offset; i < COB_FIELD_DIGITS(f1) - COB_FIELD_SCALE(f1) + offset; ++i) {
		val *= 10;
		if (i % 2 == 0) {
			val += data[i / 2] >> 4;
		} else {
			val += data[i / 2] & 0x0f;
		}
	}
	if (sign < 0) {
		val = -val;
	}
	return val;
}

static int
cob_display_get_int (cob_field *f)
{
	unsigned char	*data;
	size_t		size;
	size_t		i;
	int		val = 0;
	int		sign;

	size = COB_FIELD_SIZE (f);
	data = COB_FIELD_DATA (f);
	sign = cob_get_sign (f);
	/* skip preceding zeros */
	for (i = 0; i < size; ++i) {
		if (cob_d2i (data[i]) != 0) {
			break;
		}
	}

	/* get value */
	if (COB_FIELD_SCALE(f) < 0) {
		for (; i < size; ++i) {
			val = val * 10 + cob_d2i (data[i]);
		}
		val *= cob_exp10[(int)-COB_FIELD_SCALE(f)];
	} else {
		size -= COB_FIELD_SCALE(f);
		for (; i < size; ++i) {
			val = val * 10 + cob_d2i (data[i]);
		}
	}
	if (sign < 0) {
		val = -val;
	}

	cob_put_sign (f, sign);
	return val;
}

static long long
cob_display_get_long_long (cob_field *f)
{
	unsigned char	*data;
	size_t		size;
	size_t		i;
	long long	val = 0;
	int		sign;

	size = COB_FIELD_SIZE (f);
	data = COB_FIELD_DATA (f);
	sign = cob_get_sign (f);
	/* skip preceding zeros */
	for (i = 0; i < size; ++i) {
		if (cob_d2i (data[i]) != 0) {
			break;
		}
	}

	/* get value */
	if (COB_FIELD_SCALE(f) < 0) {
		for (; i < size; ++i) {
			val = val * 10 + cob_d2i (data[i]);
		}
		val *= cob_exp10LL[(int)-COB_FIELD_SCALE(f)];
	} else {
		size -= COB_FIELD_SCALE(f);
		for (; i < size; ++i) {
			val = val * 10 + cob_d2i (data[i]);
		}
	}
	if (sign < 0) {
		val = -val;
	}

	cob_put_sign (f, sign);
	return val;
}

void
cob_set_int (cob_field *f, const int n)
{
	cob_field	temp;
	cob_field_attr	attr;

	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN, NULL);
	temp.size = 4;
	temp.data = (unsigned char *)&n;
	temp.attr = &attr;
	cob_move (&temp, f);
}

int
cob_get_int (cob_field *f)
{
	int		n;
	cob_field	temp;
	cob_field_attr	attr;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_display_get_int (f);
	case COB_TYPE_NUMERIC_BINARY:
		return (int)cob_binary_mget_int64 (f);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_packed_get_int (f);
	default:
		COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0,
				COB_FLAG_HAVE_SIGN, NULL);
		temp.size = 4;
		temp.data = (unsigned char *)&n;
		temp.attr = &attr;
		cob_move (f, &temp);
		return n;
	}
}

long long
cob_get_long_long (cob_field *f)
{
	long long	n;
	cob_field	temp;
	cob_field_attr	attr;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_display_get_long_long (f);
	case COB_TYPE_NUMERIC_BINARY:
		return cob_binary_mget_int64 (f);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_packed_get_long_long (f);
	default:
		COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 18, 0,
				COB_FLAG_HAVE_SIGN, NULL);
		temp.size = 8;
		temp.data = (unsigned char *)&n;
		temp.attr = &attr;
		cob_move (f, &temp);
		return n;
	}
}

void
cob_init_move (void)
{
	lastdata = cob_malloc (COB_SMALL_BUFF);
	lastsize = COB_SMALL_BUFF;
}

/* I18N_UTF8: utf-8 version of memset for mbchar. */
#ifdef	I18N_UTF8
int
cob_la_memset (cob_field *f, int n)
{
	int		rt = 0;
	unsigned char	buff[3];
	int		i, rep = f->size / sizeof (buff);
	unsigned char	*p = f->data;

	memset (f->data, 0, f->size);
	rt = ascii_to_utf8 (n, buff);
	for (i = 0; i < rep; i++) {
		memcpy (p, buff, sizeof (buff));
		p += sizeof (buff);
	}
	return rt;
}

#else /*!I18N_UTF8*/

int
cob_la_anstojis (int n)
{
	int	i;

	switch (n) {
	case '0':
		i = 0x4f82;
		break;
	case ' ':
		i = 0x4081;
		break;
	case '"':
		i = 0x8d81;
		break;
	case 255:
		i = 0xffff;
		break;
	default:
		i = n;
		break;
	}
	return i;
}

int
cob_la_memset (cob_field *f, int n)
{
	unsigned char	*data;
	size_t		size;
	char		buff[3];
	int		i;

	n = cob_la_anstojis (n);
	memcpy (buff, &n, 2);
	data = f->data;
	size = f->size / 2;
	for (i = 0; i < size; i++) {
		memcpy (&data[i*2], buff, 2);
	}
	return n;
}
#endif /*I18N_UTF8*/
