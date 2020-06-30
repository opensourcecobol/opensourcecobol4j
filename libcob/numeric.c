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
#include <ctype.h>
#include <math.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#define	COB_LIB_INCLUDE
#include "libcob.h"
#include "coblocal.h"

#define DECIMAL_NAN	-128
#define DECIMAL_CHECK(d1,d2) \
  if (unlikely(d1->scale == DECIMAL_NAN || d2->scale == DECIMAL_NAN)) { \
      d1->scale = DECIMAL_NAN; \
      return; \
    }

#define	COB_MAX_BINARY	36

/* Local variables */


static const unsigned char packed_bytes[] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
	0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
	0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
	0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99
};

static unsigned char	*num_buff_ptr;
static cob_decimal	cob_d1;
static cob_decimal	cob_d2;
static cob_decimal	cob_d3;
static cob_decimal	cob_d4;
static mpz_t		cob_mexp;
static mpz_t		cob_mpzt;
static mpz_t		cob_mpze10[COB_MAX_BINARY];
static unsigned char	packed_value[20];

#ifdef	COB_EXPERIMENTAL

#if GMP_NAIL_BITS != 0
#error NAILS not supported
#endif

#define COB_MAX_LL	9223372036854775807LL

static void
mpz_set_ull (mpz_ptr dest, const unsigned long long val)
{
	size_t			size;

	size = (val != 0);
	dest->_mp_d[0] = val & GMP_NUMB_MASK;
#if	GMP_LIMB_BITS < 64
	if (val > GMP_NUMB_MAX) {
		dest->_mp_d[1] = val >> GMP_NUMB_BITS;
		size = 2;
	}
#endif
	dest->_mp_size = size;
}

static void
mpz_set_sll (mpz_ptr dest, const signed long long val)
{
	unsigned long long	vtmp;
	size_t			size;

	vtmp = (unsigned long long)(val >= 0 ? val : -val);
	size = (vtmp != 0);
	dest->_mp_d[0] = vtmp & GMP_NUMB_MASK;
#if	GMP_LIMB_BITS < 64
	if (vtmp > GMP_NUMB_MAX) {
		dest->_mp_d[1] = vtmp >> GMP_NUMB_BITS;
		size = 2;
	}
#endif
	dest->_mp_size = (val >= 0) ? size : -size;
}

static unsigned long long
mpz_get_ull (const mpz_ptr src)
{
	size_t			size;

	size = mpz_size (src);
	if (!size) {
		return 0;
	}
#if	GMP_LIMB_BITS > 32
	return (unsigned long long)src->_mp_d[0];
#else
	if (size < 2) {
		return (unsigned long long)src->_mp_d[0];
	}
	return (unsigned long long)src->_mp_d[0] |
		((unsigned long long)src->_mp_d[1] << GMP_NUMB_BITS);
#endif
}

static signed long long
mpz_get_sll (const mpz_ptr src)
{
	int			size;
	unsigned long long	vtmp;

	size = src->_mp_size;
	if (!size) {
		return 0;
	}
	vtmp = (unsigned long long)src->_mp_d[0];
#if	GMP_LIMB_BITS < 64
	if (mpz_size (src) > 1) {
		vtmp |= (unsigned long long)src->_mp_d[1] << GMP_NUMB_BITS;
	}
#endif
	if (size > 0) {
		return (signed long long) vtmp & COB_MAX_LL;
	}
	return ~(((signed long long) vtmp - 1LL) & COB_MAX_LL);
}

#endif	/* COB_EXPERIMENTAL */

static COB_INLINE void
num_byte_memcpy (unsigned char *s1, const unsigned char *s2, size_t size)
{
	do {
		*s1++ = *s2++;
	} while (--size);
}

static long long
cob_binary_get_int64 (const cob_field * const f)
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
	num_byte_memcpy (s, f->data, f->size);
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
			num_byte_memcpy ((unsigned char *)&n, f->data, f->size);
			n = COB_BSWAP_64 (n);
			n >>= 8 * fsiz;	/* shift with sign */
		} else {
			num_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
			n = COB_BSWAP_64 (n);
		}
	} else {
		if (COB_FIELD_HAVE_SIGN (f)) {
			num_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
			n >>= 8 * fsiz;	/* shift with sign */
		} else {
			num_byte_memcpy ((unsigned char *)&n, f->data, f->size);
		}
	}
#else	/* WORDS_BIGENDIAN */
	if (COB_FIELD_HAVE_SIGN (f)) {
		num_byte_memcpy ((unsigned char *)&n, f->data, f->size);
		n >>= 8 * fsiz;	/* shift with sign */
	} else {
		num_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
	}
#endif	/* WORDS_BIGENDIAN */
	return n;
}

static unsigned long long
cob_binary_get_uint64 (const cob_field * const f)
{
	unsigned long long	n = 0;
	size_t			fsiz = 8 - f->size;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		num_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
		n = COB_BSWAP_64 (n);
	} else {
		num_byte_memcpy ((unsigned char *)&n, f->data, f->size);
	}
#else	/* WORDS_BIGENDIAN */
	num_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
#endif	/* WORDS_BIGENDIAN */
	return n;
}

static void
cob_binary_set_uint64 (cob_field *f, unsigned long long n)
{
#ifndef WORDS_BIGENDIAN
	unsigned char	*s;

	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
		s = ((unsigned char *)&n) + 8 - f->size;
	} else {
		s = (unsigned char *)&n;
	}
	num_byte_memcpy (f->data, s, f->size);
#else	/* WORDS_BIGENDIAN */
	num_byte_memcpy (f->data, ((unsigned char *)&n) + 8 - f->size, f->size);
#endif	/* WORDS_BIGENDIAN */
}

static void
cob_binary_set_int64 (cob_field *f, long long n)
{
#ifndef WORDS_BIGENDIAN
	unsigned char	*s;

	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
		s = ((unsigned char *)&n) + 8 - f->size;
	} else {
		s = (unsigned char *)&n;
	}
	num_byte_memcpy (f->data, s, f->size);
#else	/* WORDS_BIGENDIAN */
	num_byte_memcpy (f->data, ((unsigned char *)&n) + 8 - f->size, f->size);
#endif	/* WORDS_BIGENDIAN */
}

/*
 * Decimal number
 */

void
cob_decimal_init (cob_decimal *d)
{
	mpz_init2 (d->value, 256);
	d->scale = 0;
}

/* Not used - comment out
void
cob_decimal_print (cob_decimal *d)
{
	mpz_out_str (stdout, 10, d->value);
	if (d->scale) {
		fprintf (stdout, " * 10^%d", -d->scale);
	}
	fputs ("\n", stdout);
}
end comment out */

/* d->value *= 10^n, d->scale += n */
static void
shift_decimal (cob_decimal *d, const int n)
{
	if (n == 0) {
		return;
	}
	if (n > 0) {
		mpz_ui_pow_ui (cob_mexp, 10, n);
		mpz_mul (d->value, d->value, cob_mexp);
	} else {
		mpz_ui_pow_ui (cob_mexp, 10, -n);
		mpz_tdiv_q (d->value, d->value, cob_mexp);
	}
	d->scale += n;
}

static void
align_decimal (cob_decimal *d1, cob_decimal *d2)
{
	if (d1->scale < d2->scale) {
		shift_decimal (d1, d2->scale - d1->scale);
	} else if (d1->scale > d2->scale) {
		shift_decimal (d2, d1->scale - d2->scale);
	}
}

/*
 * Decimal set/get
 */

static void
cob_decimal_set (cob_decimal *dst, const cob_decimal *src)
{
	mpz_set (dst->value, src->value);
	dst->scale = src->scale;
}

/* double */

static void
cob_decimal_set_double (cob_decimal *d, const double v)
{
	mpz_set_d (d->value, v * 1.0e9);
	d->scale = 9;
}

static double
cob_decimal_get_double (cob_decimal *d)
{
	double	v;
	int	n;

	v = mpz_get_d (d->value);
	n = d->scale;
	for (; n > 0; n--) {
		v /= 10;
	}
	for (; n < 0; n++) {
		v *= 10;
	}
	return v;
}

/* DISPLAY */

static void
cob_decimal_set_display (cob_decimal *d, cob_field *f)
{
	unsigned char	*data;
	size_t		size;
	int		sign;
	unsigned int	n;

	data = COB_FIELD_DATA (f);
	size = COB_FIELD_SIZE (f);
	if (unlikely(*data == 255)) {
		mpz_ui_pow_ui (d->value, 10, size);
		d->scale = COB_FIELD_SCALE(f);
		return;
	}
	if (unlikely(*data == 0)) {
		mpz_ui_pow_ui (d->value, 10, size);
		mpz_neg (d->value, d->value);
		d->scale = COB_FIELD_SCALE(f);
		return;
	}
	sign = cob_get_sign (f);
	/* skip leading zeros */
	while (size > 1 && *data == '0') {
		size--;
		data++;
	}

	/* set value */
	if (size < 10) {
		n = 0;
		while (size--) {
			n = n * 10 + cob_d2i (*data++);
		}
		mpz_set_ui (d->value, n);
	} else {
		memcpy (num_buff_ptr, data, size);
		num_buff_ptr[size] = 0;
		mpz_set_str (d->value, (char *)num_buff_ptr, 10);
	}

	/* set sign and scale */
	if (sign < 0) {
		mpz_neg (d->value, d->value);
	}
	d->scale = COB_FIELD_SCALE(f);
	cob_put_sign (f, sign);
}

static int
cob_decimal_get_display (cob_decimal *d, cob_field *f, const int opt)
{
	unsigned char	*data;
	size_t		size;
	int		diff;
	int		sign;

	/* build string */
	sign = mpz_sgn (d->value);
	mpz_abs (d->value, d->value);
	mpz_get_str ((char *)num_buff_ptr, 10, d->value);
	size = strlen ((char *)num_buff_ptr);

	/* store number */
	data = COB_FIELD_DATA (f);
	diff = (int)(COB_FIELD_SIZE (f) - size);
	if (unlikely(diff < 0)) {
		/* overflow */
		cob_set_exception (COB_EC_SIZE_OVERFLOW);

		/* if the statement has ON SIZE ERROR or NOT ON SIZE ERROR,
		   then throw an exception */
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			return cob_exception_code;
		}

		/* othersize, truncate digits */
		memcpy (data, num_buff_ptr - diff, COB_FIELD_SIZE (f));
	} else {
		/* no overflow */
		memset (data, '0', (size_t)diff);
		memcpy (data + diff, num_buff_ptr, size);
	}

	cob_put_sign (f, sign);
	return 0;
}

/* BINARY */

static void
cob_decimal_set_binary (cob_decimal *d, cob_field *f)
{
#ifdef	COB_LI_IS_LL
	if (COB_FIELD_HAVE_SIGN (f)) {
		mpz_set_si (d->value, cob_binary_get_int64 (f));
	} else {
		mpz_set_ui (d->value, cob_binary_get_uint64 (f));
	}
#elif	defined(COB_EXPERIMENTAL)
	if (COB_FIELD_HAVE_SIGN (f)) {
		mpz_set_sll (d->value, cob_binary_get_int64 (f));
	} else {
		mpz_set_ull (d->value, cob_binary_get_uint64 (f));
	}
#else
	size_t			negative = 0;
	unsigned long long	uval;
	long long		val;

	if (f->size <= 4) {
		if (COB_FIELD_HAVE_SIGN (f)) {
			mpz_set_si (d->value, (int)cob_binary_get_int64 (f));
		} else {
			mpz_set_ui (d->value, (unsigned int) cob_binary_get_uint64 (f));
		}
	} else {
		if (COB_FIELD_HAVE_SIGN (f)) {
			val = cob_binary_get_int64 (f);
			if (val < 0) {
				negative = 1;
				val = -val;
			}
			mpz_set_ui (d->value, (unsigned int)((val & 0x7FFFFFFF00000000LL)>> 32));
			mpz_mul_2exp (d->value, d->value, 32);
			mpz_add_ui (d->value, d->value, (unsigned int)(val & 0xffffffff));
			if (negative) {
				mpz_neg (d->value, d->value);
			}
		} else {
			uval = cob_binary_get_uint64 (f);
			mpz_set_ui (d->value, (unsigned int)(uval >> 32));
			mpz_mul_2exp (d->value, d->value, 32);
			mpz_add_ui (d->value, d->value, (unsigned int)(uval & 0xffffffff));
		}
	}
#endif
	d->scale = COB_FIELD_SCALE(f);
}

static int
cob_decimal_get_binary (cob_decimal *d, cob_field *f, const int opt)
{
	size_t			overflow;
	size_t			digits;
	size_t			sign;
	size_t			bitnum;
#if	!defined(COB_EXPERIMENTAL) && !defined(COB_LI_IS_LL)
	long long		llval;
	unsigned long long	ullval;
	unsigned int		lo;
#endif

	if (unlikely(mpz_size (d->value) == 0)) {
		memset (f->data, 0, f->size);
		return 0;
	}
	overflow = 0;
	digits = COB_FIELD_DIGITS(f);
	if (COB_FIELD_HAVE_SIGN (f)) {
		sign = 1;
	} else {
		sign = 0;
		if (mpz_sgn (d->value) < 0) {
			mpz_abs (d->value, d->value);
		}
	}
	bitnum = (f->size * 8) - sign;
	if (unlikely(mpz_sizeinbase (d->value, 2) > bitnum)) {
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			goto overflow;
		}
		overflow = 1;
		/* TRUNC_ON_OVERFLOW is only set for binary_truncate */
		if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
			mpz_tdiv_r (d->value, d->value, cob_mpze10[digits]);
/*
		}
*/
/* RXW */
		} else {
			mpz_fdiv_r_2exp (d->value, d->value, (f->size * 8));
		}
	} else if (opt && cob_current_module->flag_binary_truncate) {
		if (mpz_cmpabs (d->value, cob_mpze10[digits]) >= 0) {
			/* overflow */
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				goto overflow;
			}
			overflow = 1;
			/* TRUNC_ON_OVERFLOW is only set for binary_truncate */
			if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
				mpz_tdiv_r (d->value, d->value, cob_mpze10[digits]);
/*
			}
*/
/* RXW */
			} else {
				mpz_fdiv_r_2exp (d->value, d->value, (f->size * 8));
			}
		}
	}
#ifdef	COB_LI_IS_LL
	if (!sign || overflow) {
		cob_binary_set_uint64 (f, mpz_get_ui (d->value));
	} else {
		cob_binary_set_int64 (f, mpz_get_si (d->value));
	}
#elif	defined(COB_EXPERIMENTAL)
	if (!sign || overflow) {
		cob_binary_set_uint64 (f, mpz_get_ull (d->value));
	} else {
		cob_binary_set_int64 (f, mpz_get_sll (d->value));
	}
#else
	if (f->size <= 4) {
		if (!sign || overflow) {
			cob_binary_set_uint64 (f, (unsigned long long)mpz_get_ui (d->value));
		} else {
			cob_binary_set_int64 (f, (long long)mpz_get_si (d->value));
		}
	} else {
		mpz_fdiv_r_2exp (cob_mpzt, d->value, 32);
		mpz_fdiv_q_2exp (d->value, d->value, 32);
		lo = mpz_get_ui (cob_mpzt);

		if (!sign || overflow) {
			ullval = mpz_get_ui (d->value);
			ullval = (ullval << 32) | lo;
			cob_binary_set_uint64 (f, ullval);
		} else {
			llval = mpz_get_si (d->value);
			llval = (llval << 32) | lo;
			cob_binary_set_int64 (f, llval);
		}
	}
#endif
	if (!overflow) {
		return 0;
	}

overflow:
	cob_set_exception (COB_EC_SIZE_OVERFLOW);
	return cob_exception_code;
}

/* PACKED-DECIMAL */

static int
cob_packed_get_sign (const cob_field *f)
{
	unsigned char *p;

	if (!COB_FIELD_HAVE_SIGN (f)) {
		return 0;
	}
	p = f->data + f->size - 1;
	return ((*p & 0x0f) == 0x0d) ? -1 : 1;
}

static void
cob_complement_packed (cob_field *f)
{
	unsigned char	*p;
	int		ndigs;
	int		tval;
	int		carry = 0;
	unsigned int	msn;
	int		emptydig;

	ndigs = COB_FIELD_DIGITS(f);
	msn = 1;
	emptydig = 1 - (COB_FIELD_DIGITS(f) % 2);

	p = f->data + ((ndigs + emptydig) / 2) - (1 - msn);
	while (ndigs--) {
		if (!msn) {
			tval = *p & 0x0f;
		} else {
			tval = (*p & 0xf0) >> 4;
		}
		tval += carry;
		if (tval > 0) {
			carry = 1;
			tval= 10 - tval;
		} else {
			carry = 0;
		}
		if (!msn) {
			*p = (*p & 0xf0) | tval;
			msn = 1;
		} else {
			*p = (*p & 0x0f) | (tval << 4);
			msn = 0;
			p--;
		}
	}
}

static void
cob_add_packed (cob_field *f, int ival)
{
	unsigned char	*p;
	int		sign;
	int		ndigs;
	int		tval;
	int		carry = 0;
	unsigned int	msn;
	unsigned int	subtr = 0;
	unsigned int	zeroes = 0;
	unsigned int	origdigs;
	int		emptydig;
	long long val;

	val = ival;

	ndigs = COB_FIELD_SCALE(f);
	while(ndigs > 0) {
		--ndigs;
		val *= 10;
	}

	ndigs = COB_FIELD_DIGITS(f);
	if (ndigs <= 0) {
		return;
	}
	sign = cob_packed_get_sign (f);
	msn = 1;
	emptydig = 1 - (COB_FIELD_DIGITS(f) % 2);

	/* -x +v = -(x - v), -x -v = -(x + v) */
	if (sign < 0) {
		val = -val;
	}
	if (val < 0) {
		val = -val;
		subtr = 1;
	}
	p = f->data + ((ndigs + emptydig) / 2) - (1 - msn);
	origdigs = ndigs;
	while (ndigs--) {
		if (!msn) {
			tval = *p & 0x0f;
		} else {
			tval = (*p & 0xf0) >> 4;
		}
		if (val) {
			carry += (val % 10);
			val /= 10;
		}
		if (subtr) {
			tval -= carry;
			if (tval < 0) {
				tval += 10;
				carry = 1;
			} else {
				carry = 0;
			}
		} else {
			tval += carry;
			if (tval > 9) {
				tval %= 10;
				carry = 1;
			} else {
				carry = 0;
			}
		}
		if (tval == 0) {
			zeroes++;
		}
		if (!msn) {
			*p = (*p & 0xf0) | tval;
			msn = 1;
		} else {
			*p = (*p & 0x0f) | (tval << 4);
			msn = 0;
			p--;
		}
	}
	if (sign) {
		p = f->data + f->size - 1;
		if (origdigs == zeroes) {
			*p = (*p & 0xf0) | 0x0c;
		} else if (subtr && carry) {
			cob_complement_packed (f);
			sign = -sign;
			if (sign < 0) {
				*p = (*p & 0xf0) | 0x0d;
			} else {
				*p = (*p & 0xf0) | 0x0c;
			}
		}
	} else if (subtr && carry) {
		cob_complement_packed (f);
	}
}

static void
cob_decimal_set_packed (cob_decimal *d, cob_field *f)
{
	unsigned char	*p;
	int		digits;
	int		sign;
	unsigned int	val;
	unsigned int	valseen;

	p = f->data;
	/* Fixme */
	digits = COB_FIELD_DIGITS (f);
	sign = cob_packed_get_sign (f);

	if (digits % 2 == 0) {
		val = *p & 0x0f;
		digits--;
		p++;
	} else {
		val = 0;
	}

	if (COB_FIELD_DIGITS(f) < 10) {
		while (digits > 1) {
			if (val) {
				val *= 100;
			}
			if (*p) {
				val += ((*p >> 4) * 10) + (*p & 0x0f);
			}
			digits -= 2;
			p++;
		}
		if (val) {
			val *= 10;
		}
		val += *p >> 4;
		mpz_set_ui (d->value, val);
	} else {
		valseen = 0;
		mpz_set_ui (d->value, val);
		if (val) {
			valseen = 1;
		}
		while (digits > 1) {
			if (valseen) {
				mpz_mul_ui (d->value, d->value, 100);
			}
			if (*p) {
				mpz_add_ui (d->value, d->value,
					(*p >> 4) * 10 + (*p & 0x0f));
				valseen = 1;
			}
			digits -= 2;
			p++;
		}
		if (valseen) {
			mpz_mul_ui (d->value, d->value, 10);
		}
		mpz_add_ui (d->value, d->value, (*p >> 4));
	}

	if (sign < 0) {
		mpz_neg (d->value, d->value);
	}
	d->scale = COB_FIELD_SCALE(f);
}

static int
cob_decimal_get_packed (cob_decimal *d, cob_field *f, const int opt)
{
	unsigned char	*data;
	unsigned char	*p;
	unsigned char	*q;
	size_t		size;
	size_t		n;
	size_t		i;
	int		diff;
	int		sign;
	int		digits;
	unsigned char	x;

	/* build string */
	sign = mpz_sgn (d->value);
	mpz_abs (d->value, d->value);
	mpz_get_str ((char *)num_buff_ptr, 10, d->value);
	size = strlen ((char *)num_buff_ptr);

	/* store number */
	data = f->data;
	/* Fixme */
	digits = COB_FIELD_DIGITS(f);
	q = num_buff_ptr;
	diff = (int)(digits - size);
	if (diff < 0) {
		/* overflow */
		cob_set_exception (COB_EC_SIZE_OVERFLOW);

		/* if the statement has ON SIZE ERROR or NOT ON SIZE ERROR,
		   then throw an exception */
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			return cob_exception_code;
		}
		q += size - digits;
		size = digits;
	}
	memset (data, 0, f->size);
	p = data + (digits / 2) - (size / 2);
	diff = 1 - (int)(size % 2);
	for (i = diff, n = 0; i < size + diff; i++, n++) {
		x = cob_d2i (q[n]);
		if (i % 2 == 0) {
			*p = x << 4;
		} else {
			*p++ |= x;
		}
	}

	p = f->data + f->size - 1;
	if (!COB_FIELD_HAVE_SIGN (f)) {
		*p = (*p & 0xf0) | 0x0f;
	} else if (sign < 0) {
		*p = (*p & 0xf0) | 0x0d;
	} else {
		*p = (*p & 0xf0) | 0x0c;
	}

	return 0;
}

void
cob_set_packed_zero (cob_field *f)
{
	memset (f->data, 0, f->size);
	if (!COB_FIELD_HAVE_SIGN (f)) {
		*(f->data + f->size - 1) = 0x0f;
	} else {
		*(f->data + f->size - 1) = 0x0c;
	}
}

void
cob_set_packed_int (cob_field *f, const int val)
{
	unsigned char	*p;
	size_t		sign = 0;
	int		n;

	if (val < 0) {
		n = -val;
		sign = 1;
	} else {
		n = val;
	}
	memset (f->data, 0, f->size);
	p = f->data + f->size - 1;
	*p = (n % 10) << 4;
	if (!COB_FIELD_HAVE_SIGN (f)) {
		*p |= 0x0f;
	} else if (sign) {
		*p |= 0x0d;
	} else {
		*p |= 0x0c;
	}
	n /= 10;
	p--;
	for (; n && p >= f->data; n /= 100, p--) {
		*p = packed_bytes[n % 100];
	}
	/* Fixme */
	if ((COB_FIELD_DIGITS(f) % 2) == 0) {
		*(f->data) &= 0x0f;
	}
}

/* General field */

void
cob_decimal_set_field (cob_decimal *d, cob_field *f)
{
	double	dval;
	float	fval;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
		cob_decimal_set_binary (d, f);
		break;
	case COB_TYPE_NUMERIC_PACKED:
		cob_decimal_set_packed (d, f);
		break;
	case COB_TYPE_NUMERIC_FLOAT:
		memcpy ((ucharptr)&fval, f->data, sizeof(float));
		cob_decimal_set_double (d, (double)fval);
		break;
	case COB_TYPE_NUMERIC_DOUBLE:
		memcpy ((ucharptr)&dval, f->data, sizeof(double));
		cob_decimal_set_double (d, dval);
		break;
	default:
		cob_decimal_set_display (d, f);
		break;
	}
}

int
cob_decimal_get_field (cob_decimal *d, cob_field *f, const int opt)
{
	cob_field	temp;
	cob_field_attr	attr;
	double		val;
	float		fval;
	int		sign;
	unsigned char	data[64];

	if (unlikely(d->scale == DECIMAL_NAN)) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		return cob_exception_code;
	}

	/* work copy */
	if (d != &cob_d1) {
		cob_decimal_set (&cob_d1, d);
		d = &cob_d1;
	}

	/* rounding */
	if (opt & COB_STORE_ROUND) {
		if (COB_FIELD_SCALE(f) < d->scale) {
			sign = mpz_sgn (d->value);
			if (sign != 0) {
				shift_decimal (d, COB_FIELD_SCALE(f) - d->scale + 1);
				if (sign > 0) {
					mpz_add_ui (d->value, d->value, 5);
				} else {
					mpz_sub_ui (d->value, d->value, 5);
				}
			}
		}
	}

	/* append or truncate decimal digits */
	shift_decimal (d, COB_FIELD_SCALE(f) - d->scale);

	/* store number */
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
		return cob_decimal_get_binary (d, f, opt);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_decimal_get_packed (d, f, opt);
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_decimal_get_display (d, f, opt);
	case COB_TYPE_NUMERIC_FLOAT:
		fval = (float) cob_decimal_get_double (d);
		memcpy (f->data, (ucharptr)&fval, sizeof (float));
		return 0;
	case COB_TYPE_NUMERIC_DOUBLE:
		val = cob_decimal_get_double (d);
		memcpy (f->data, (ucharptr)&val, sizeof (double));
		return 0;
	default:
		COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, COB_FIELD_DIGITS(f),
				COB_FIELD_SCALE(f), COB_FLAG_HAVE_SIGN, NULL);
		temp.size = COB_FIELD_DIGITS(f);
		temp.data = data;
		temp.attr = &attr;
		if (cob_decimal_get_display (d, &temp, opt) == 0) {
			cob_move (&temp, f);
		}
		return cob_exception_code;
	}
}

/*
 * Decimal arithmetic
 */

void
cob_decimal_add (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	align_decimal (d1, d2);
	mpz_add (d1->value, d1->value, d2->value);
}

void
cob_decimal_sub (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	align_decimal (d1, d2);
	mpz_sub (d1->value, d1->value, d2->value);
}

void
cob_decimal_mul (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	d1->scale += d2->scale;
	mpz_mul (d1->value, d1->value, d2->value);
}

void
cob_decimal_div (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);

	/* check for division by zero */
	if (unlikely(mpz_sgn (d2->value) == 0)) {
		d1->scale = DECIMAL_NAN;
		cob_set_exception (COB_EC_SIZE_ZERO_DIVIDE);
		if (cob_error_on_exit_flag) {
			cob_runtime_error ("Detected division by zero.");
			cob_stop_run (1);
		}
		return;
	}
	if (unlikely(mpz_sgn (d1->value) == 0)) {
		d1->scale = 0;
		return;
	}
	d1->scale -= d2->scale;
	shift_decimal (d1, 37 + ((d1->scale < 0) ? -d1->scale : 0));
	mpz_tdiv_q (d1->value, d1->value, d2->value);
}

void
cob_decimal_pow (cob_decimal *d1, cob_decimal *d2)
{
	unsigned int	n;

	DECIMAL_CHECK (d1, d2);

	if (d2->scale == 0 && mpz_fits_ulong_p (d2->value)) {
		n = mpz_get_ui (d2->value);
		mpz_pow_ui (d1->value, d1->value, n);
		d1->scale *= n;
	} else {
		cob_decimal_set_double (d1, pow (cob_decimal_get_double (d1),
						 cob_decimal_get_double (d2)));
	}
}

int
cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2)
{
	align_decimal (d1, d2);
	return mpz_cmp (d1->value, d2->value);
}

/*
 * Optimized arithmetic for DISPLAY
 */

static int
display_add_int (unsigned char *data, const size_t size, long long n)
{
	unsigned char	*sp;
	size_t		carry = 0;
	int		i;
	int		is;

	sp = data + size;
	while (n > 0) {
		i = n % 10;
		n /= 10;

		/* check for overflow */
		if (unlikely(--sp < data)) {
			if (!cob_current_module->flag_binary_truncate) {
				return 0;
			}
			return 1;
		}

		/* perform addition */
		is = (*sp & 0x0F) + i + carry;
		if (is > 9) {
			carry = 1;
			*sp = '0' + (is % 10);
		} else {
			carry = 0;
			*sp = '0' + is;
		}
	}
	if (carry == 0) {
		return 0;
	}

	/* carry up */
	while (--sp >= data) {
		if ((*sp += 1) <= '9') {
			return 0;
		}
		*sp = '0';
	}
	if (!cob_current_module->flag_binary_truncate) {
		return 0;
	}
	return 1;
}

static int
display_sub_int (unsigned char *data, const size_t size, long long n)
{
	unsigned char	*sp;
	size_t		carry = 0;
	int		i;

	sp = data + size;
	while (n > 0) {
		i = n % 10;
		n /= 10;

		/* check for overflow */
		if (unlikely(--sp < data)) {
			return 1;
		}

		/* perform subtraction */
		if ((*sp -= i + carry) < '0') {
			carry = 1;
			*sp += 10;
		} else {
			carry = 0;
		}
	}
	if (carry == 0) {
		return 0;
	}

	/* carry up */
	while (--sp >= data) {
		if ((*sp -= 1) >= '0') {
			return 0;
		}
		*sp = '9';
	}
	return 1;
}

static int
cob_display_add_int (cob_field *f, int in)
{
	unsigned char	*data;
	size_t		osize;
	size_t		i;
	size_t		size;
	int		scale;
	int		sign;
	unsigned char	tfield[64];
	long long n;

	n = in;
	data = COB_FIELD_DATA (f);
	size = COB_FIELD_SIZE (f);
	scale = COB_FIELD_SCALE (f);
	sign = cob_get_sign (f);
	osize = size;
	memcpy (tfield, data, osize);
	/* -x + n = -(x - n) */
	if (sign < 0) {
		n = -n;
	}
	while(scale > 0) {
		--scale;
		n *= 10;
	}

	if (unlikely(scale < 0)) {
		/* PIC 9(n)P(m) */
		if (-scale < 10) {
			/* Fix optimizer bug */
			while (scale) {
				++scale;
				n /= 10;
			}
		} else {
			n = 0;
		}
	} else {
		/* PIC 9(n)V9(m) */
		size -= scale;
		/* Following can never be true as size is unsigned ?? */
		/* Comment out
		if (size < 0) {
			cob_put_sign (f, sign);
			goto overflow;
		}
		*/
	}

	if (n > 0) {
		/* add n to the field */
		if (display_add_int (data, size, n) != 0) {
			/* if there was an overflow, recover the last value */
			memcpy (data, tfield, osize);
			goto overflow;
		}
	} else if (n < 0) {
		/* subtract n from the field */
		if (display_sub_int (data, size, -n) != 0) {
			for (i = 0; i < size; i++) {
				data[i] = cob_i2d (9 - cob_d2i (data[i]));
			}
			display_add_int (data, size, 1);
			sign = -sign;
		}
	}

	cob_put_sign (f, sign);
	return 0;

overflow:
	cob_put_sign (f, sign);
	cob_set_exception (COB_EC_SIZE_OVERFLOW);
	return cob_exception_code;
}

/*
 * Convenience functions
 */

int
cob_add (cob_field *f1, cob_field *f2, const int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_add (&cob_d1, &cob_d2);
	return cob_decimal_get_field (&cob_d1, f1, opt);
}

int
cob_sub (cob_field *f1, cob_field *f2, const int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_sub (&cob_d1, &cob_d2);
	return cob_decimal_get_field (&cob_d1, f1, opt);
}

int
cob_add_int (cob_field *f, const int n)
{
	if (unlikely(n == 0)) {
		return 0;
	}
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_display_add_int (f, n);
	case COB_TYPE_NUMERIC_PACKED:
		cob_add_packed (f, n);
		return 0;
	default:
		/* not optimized */
		cob_decimal_set_field (&cob_d1, f);
		mpz_set_si (cob_d2.value, n);
		cob_d2.scale = 0;
		if (cob_d1.scale) {
			mpz_ui_pow_ui (cob_mexp, 10, (unsigned int)cob_d1.scale);
			mpz_mul (cob_d2.value, cob_d2.value, cob_mexp);
			cob_d2.scale = cob_d1.scale;
		}
		mpz_add (cob_d1.value, cob_d1.value, cob_d2.value);
		return cob_decimal_get_field (&cob_d1, f, 0);
	}
}

int
cob_sub_int (cob_field *f, const int n)
{
	if (unlikely(n == 0)) {
		return 0;
	}
	return cob_add_int (f, -n);
}

int
cob_div_quotient (cob_field *dividend, cob_field *divisor,
		  cob_field *quotient, const int opt)
{
	int	ret;

	cob_decimal_set_field (&cob_d1, dividend);
	cob_decimal_set_field (&cob_d2, divisor);
	cob_decimal_set (&cob_d3, &cob_d1);

	/* compute quotient */
	cob_decimal_div (&cob_d1, &cob_d2);
	if (cob_d1.scale == DECIMAL_NAN) {
		cob_d3.scale = DECIMAL_NAN;
		return cob_exception_code;
	}

	/* set quotient */
	cob_decimal_set (&cob_d4, &cob_d1);
	ret = cob_decimal_get_field (&cob_d1, quotient, opt);

	/* truncate digits from the quotient */
	shift_decimal (&cob_d4, COB_FIELD_SCALE(quotient) - cob_d4.scale);

	/* compute remainder */
	cob_decimal_mul (&cob_d4, &cob_d2);
	cob_decimal_sub (&cob_d3, &cob_d4);

	return ret;
}

int
cob_div_remainder (cob_field *fld_remainder, const int opt)
{
	return cob_decimal_get_field (&cob_d3, fld_remainder, opt);
}

int
cob_cmp_int (cob_field *f1, const int n)
{
	cob_decimal_set_field (&cob_d1, f1);
	mpz_set_si (cob_d2.value, n);
	cob_d2.scale = 0;
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

int
cob_cmp_uint (cob_field *f1, const unsigned int n)
{
	cob_decimal_set_field (&cob_d1, f1);
	mpz_set_ui (cob_d2.value, n);
	cob_d2.scale = 0;
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

int
cob_numeric_cmp (cob_field *f1, cob_field *f2)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

int
cob_cmp_packed (cob_field *f, int n)
{
	static int		lastval = 0;

	unsigned char		*p;
	size_t			size;
	size_t			inc = 0;
	int			sign;
	unsigned char		val1[20];

	sign = cob_packed_get_sign (f);
	/* Field positive, value negative */
	if (sign >= 0 && n < 0) {
		return 1;
	}
	/* Field negative, value positive */
	if (sign < 0 && n >= 0) {
		return -1;
	}
	/* Both positive or both negative */
	p = f->data;
	for (size = 0; size < 20; size++) {
		if (size < 20 - f->size) {
			val1[size] = 0;
		} else {
			val1[size] = p[inc++];
		}
	}
	val1[19] &= 0xf0;
	if ((COB_FIELD_DIGITS(f) % 2) == 0) {
		val1[20 - f->size] &= 0x0f;
	}
	if (n != lastval) {
		lastval = n;
		if (n < 0) {
			n = -n;
		}
		memset (&packed_value[14], 0, 6);
		if (n) {
			p = &packed_value[19];
			*p =  (n % 10) << 4;
			p--;
			n /= 10;
			for (; n;) {
				size = n % 100;
				*p = (unsigned char)((size % 10) | ((size / 10) << 4));
				n /= 100;
				p--;
			}
		}
	}
	for (size = 0; size < 20; size++) {
		if (val1[size] != packed_value[size]) {
			if (sign < 0) {
				return packed_value[size] - val1[size];
			} else {
				return val1[size] - packed_value[size];
			}
		}
	}
	return 0;
}

void
cob_init_numeric (void)
{
	size_t	i;

	cob_decimal_init (&cob_d1);
	cob_decimal_init (&cob_d2);
	cob_decimal_init (&cob_d3);
	cob_decimal_init (&cob_d4);
	mpz_init2 (cob_mpzt, 256);
	mpz_init2 (cob_mexp, 512);
	for (i = 0; i < COB_MAX_BINARY; i++) {
		mpz_init (cob_mpze10[i]);
		mpz_ui_pow_ui (cob_mpze10[i], 10, i);
	}
	num_buff_ptr = cob_malloc (2048);
	memset (packed_value, 0, sizeof(packed_value));
}

/* Numeric Display compares */

int
cob_cmp_numdisp (const unsigned char *data, const size_t size, const int n)
{
	const unsigned char	*p;
	size_t			inc;
	int			val = 0;

	p = data;
	for (inc = 0; inc < size; inc++, p++) {
		val = (val * 10) + (*p - (unsigned char)'0');
	}
	return (val < n) ? -1 : (val > n);
}

int
cob_cmp_long_numdisp (const unsigned char *data, const size_t size, const int n)
{
	const unsigned char	*p;
	long long		val = 0;
	size_t			inc;

	p = data;
	for (inc = 0; inc < size; inc++, p++) {
		val = (val * 10) + (*p - (unsigned char)'0');
	}
	return (val < n) ? -1 : (val > n);
}

#ifdef	COB_EBCDIC_MACHINE
static void
cob_get_ascii_sign (const unsigned char *p, int *val)
{
	switch (*p) {
	case 'p':
		return;
	case 'q':
		*val += 1;
		return;
	case 'r':
		*val += 2;
		return;
	case 's':
		*val += 3;
		return;
	case 't':
		*val += 4;
		return;
	case 'u':
		*val += 5;
		return;
	case 'v':
		*val += 6;
		return;
	case 'w':
		*val += 7;
		return;
	case 'x':
		*val += 8;
		return;
	case 'y':
		*val += 9;
		return;
	}
}

static void
cob_get_long_ascii_sign (const unsigned char *p, long long *val)
{
	switch (*p) {
	case 'p':
		return;
	case 'q':
		*val += 1;
		return;
	case 'r':
		*val += 2;
		return;
	case 's':
		*val += 3;
		return;
	case 't':
		*val += 4;
		return;
	case 'u':
		*val += 5;
		return;
	case 'v':
		*val += 6;
		return;
	case 'w':
		*val += 7;
		return;
	case 'x':
		*val += 8;
		return;
	case 'y':
		*val += 9;
		return;
	}
}
#endif

static int
cob_get_ebcdic_sign (const unsigned char *p, int *val)
{
	switch (*p) {
	case '{':
		return 0;
	case 'A':
		*val += 1;
		return 0;
	case 'B':
		*val += 2;
		return 0;
	case 'C':
		*val += 3;
		return 0;
	case 'D':
		*val += 4;
		return 0;
	case 'E':
		*val += 5;
		return 0;
	case 'F':
		*val += 6;
		return 0;
	case 'G':
		*val += 7;
		return 0;
	case 'H':
		*val += 8;
		return 0;
	case 'I':
		*val += 9;
		return 0;
	case '}':
		return 1;
	case 'J':
		*val += 1;
		return 1;
	case 'K':
		*val += 2;
		return 1;
	case 'L':
		*val += 3;
		return 1;
	case 'M':
		*val += 4;
		return 1;
	case 'N':
		*val += 5;
		return 1;
	case 'O':
		*val += 6;
		return 1;
	case 'P':
		*val += 7;
		return 1;
	case 'Q':
		*val += 8;
		return 1;
	case 'R':
		*val += 9;
		return 1;
	}
	return 0;
}

static int
cob_get_long_ebcdic_sign (const unsigned char *p, long long *val)
{
	switch (*p) {
	case '{':
		return 0;
	case 'A':
		*val += 1;
		return 0;
	case 'B':
		*val += 2;
		return 0;
	case 'C':
		*val += 3;
		return 0;
	case 'D':
		*val += 4;
		return 0;
	case 'E':
		*val += 5;
		return 0;
	case 'F':
		*val += 6;
		return 0;
	case 'G':
		*val += 7;
		return 0;
	case 'H':
		*val += 8;
		return 0;
	case 'I':
		*val += 9;
		return 0;
	case '}':
		return 1;
	case 'J':
		*val += 1;
		return 1;
	case 'K':
		*val += 2;
		return 1;
	case 'L':
		*val += 3;
		return 1;
	case 'M':
		*val += 4;
		return 1;
	case 'N':
		*val += 5;
		return 1;
	case 'O':
		*val += 6;
		return 1;
	case 'P':
		*val += 7;
		return 1;
	case 'Q':
		*val += 8;
		return 1;
	case 'R':
		*val += 9;
		return 1;
	}
	return 0;
}

int
cob_cmp_sign_numdisp (const unsigned char *data, const size_t size, const int n)
{
	const unsigned char	*p;
	int			val = 0;
	size_t			inc;

	p = data;
	for (inc = 0; inc < size - 1; inc++, p++) {
		val = (val * 10) + (*p - (unsigned char)'0');
	}
	val *= 10;
	if (*p >= '0' && *p <= '9') {
		val += (*p - (unsigned char)'0');
	} else {
		if (unlikely(cob_current_module->display_sign)) {
			if (cob_get_ebcdic_sign (p, &val)) {
				val = -val;
			}
		} else {
#ifdef	COB_EBCDIC_MACHINE
			cob_get_ascii_sign (p, &val);
#else
			val += (*p - (unsigned char)'p');
#endif
			val = -val;
		}
	}
	return (val < n) ? -1 : (val > n);
}

int
cob_cmp_long_sign_numdisp (const unsigned char *data, const size_t size, const int n)
{
	const unsigned char	*p;
	long long		val = 0;
	size_t			inc;

	p = data;
	for (inc = 0; inc < size - 1; inc++, p++) {
		val = (val * 10) + (*p - (unsigned char)'0');
	}
	val *= 10;
	if (*p >= '0' && *p <= '9') {
		val += (*p - (unsigned char)'0');
	} else {
		if (unlikely(cob_current_module->display_sign)) {
			if (cob_get_long_ebcdic_sign (p, &val)) {
				val = -val;
			}
		} else {
#ifdef	COB_EBCDIC_MACHINE
			cob_get_long_ascii_sign (p, &val);
#else
			val += (*p - (unsigned char)'p');
#endif
			val = -val;
		}
	}
	return (val < n) ? -1 : (val > n);
}
