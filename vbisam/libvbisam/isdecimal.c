/*
 * Copyright (C) 2003 Trevor van Bremen
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

#include	"isinternal.h"

#define	ACCSIZE	(DECSIZE + 1)

struct decacc {
	short dec_exp;
	short dec_pos;
	short dec_ndgts;
	char dec_dgts[ACCSIZE];
};

/* Local functions */

static void
comp100 (unsigned char *cp, int count)
{
	int base;

	base = 100;
	cp += count;
	while (count--) {
		cp--;
		*cp = base - *cp;
		if (*cp > 99) {
			*cp -= 100;
			base = 100;
		} else {
			base = 99;
		}
	}
}

static int
round100 (unsigned char *cp, int len)
{
	int carry = 1;
	int count = len;

	cp += len;
	while (len-- > 0) {
		cp--;
		*cp = *cp + carry;
		if (*cp > 99) {
			*cp -= 100;
			carry = 1;
		} else {
			carry = 0;
		}
	}

	if (carry) {
		for (len = count; --len;) {
			cp[len - 1] = cp[len];
		}
		cp[0] = 1;
		return 1;
	}
	return 0;
}

static int
deccvfix (long i, dec_t *dp)
{
	int j;
	char buffer[DECSIZE];

	if (i < 0) {
		dp->dec_pos = 0;
		i = -i;
	} else {
		dp->dec_pos = 1;
	}

	dp->dec_exp = 0;
	j = 0;

	while (i != 0) {
		if ((buffer[j] = i % 100) || (j != 0)) {
			j++;
		}
		dp->dec_exp++;
		i /= 100;
	}
	dp->dec_ndgts = j;

	while (j != 0) {
		dp->dec_dgts[i++] = buffer[--j];
	}

	return 0;
}

static void
dectofix (dec_t *dp, long *ip)
{
	long		i = 0;
	unsigned char	*digits = dp->dec_dgts;
	int		expon = dp->dec_exp;
	int		valid = dp->dec_ndgts;

	while (expon-- > 0) {
		i = i * 100;
		if (valid-- > 0) {
			i += *digits++;
		}
	}
	*ip = (dp->dec_pos) ? i : -i;
}

static int
deccvreal (double dbl, dec_t *dp, int ndigits)
{
	unsigned char	*str;
	unsigned char	*dgt;
	int		decpt;
	int		sign;

	str = (unsigned char *)ecvt (dbl, ndigits, &decpt, &sign);

	dp->dec_pos = sign ? 0 : 1;
	dp->dec_exp = (decpt + (decpt > 0 ? 1 : 0)) / 2;

	dgt = dp->dec_dgts;

	if (decpt & 1) {
		*dgt++ = *str++ - '0';
		ndigits--;
	}

	while (ndigits-- > 0) {
		*dgt++ = (*str++ - '0') * 10;
		if (ndigits-- > 0) {
			dgt[-1] += *str++ - '0';
		}
	}

	while (--dgt >= dp->dec_dgts && *dgt == 0) ;

	dp->dec_ndgts = 1 + dgt - dp->dec_dgts;

	return 0;
}

static int
dectoreal (dec_t *dp, double *dblp, int valid)
{
	unsigned char	*digits = dp->dec_dgts;
	double		dbl;

	if (valid > dp->dec_ndgts) {
		valid = dp->dec_ndgts;
	}

	dbl = 0.0;

	while (valid-- > 0) {
		dbl = (dbl + digits[valid]) / 100.0;
	}

	if (dp->dec_pos == 0) {
		dbl = -dbl;
	}

	if (dp->dec_exp > 0) {
		int i = dp->dec_exp;

		while (i--) {
			dbl *= 100.0;
		}
	} else if (dp->dec_exp < 0) {
		int i = dp->dec_exp;

		while (i++) {
			dbl /= 100.0;
		}
	}

	*dblp = dbl;

	return 0;
}

static char *
decefcvt (dec_t *np, int dg, int *pt, int *sg, int fl)
{
	static char	*ds = NULL;
	int i, j, k, nd;
	dec_t rd;

	if (!ds) {
		ds = calloc (1, 160);
	}
	ds[0] = 0;
	if (np->dec_pos == -1) {
		return ds;
	}
	*sg = np->dec_pos ^ 1;
	*pt = np->dec_exp * 2;
	nd = np->dec_ndgts;
	if (nd && np->dec_dgts[0] < 10) {
		*pt -= 1;
	}
	k = dg;
	if (fl) {
		k += *pt;
	}
	if (k < 0) {
		return ds;
	}
	i = 0;
	if (nd && np->dec_dgts[0] < 10) {
		i = 1;
	}
	rd.dec_pos = np->dec_pos;
	rd.dec_ndgts = 1;
	rd.dec_exp = np->dec_exp - (k + i) / 2;
	if ((k + i) & 1) {
		rd.dec_dgts[0] = 5;
	} else {
		rd.dec_dgts[0] = 50;
	}
	if (nd == 0) {
		rd.dec_ndgts = 0;
		rd.dec_dgts[0] = 0;
	}
	if (decadd (np, &rd, &rd)) {
		return ds;
	}
	i = 0;
	*pt = rd.dec_exp * 2;
	if (nd && rd.dec_dgts[0] < 10) {
		*pt -= 1;
		i = 1;
	}
	if (fl) {
		dg += *pt;
	}
	j = 0;
	while (j < dg && j < 151) {
		if (i / 2 < rd.dec_ndgts) {
			k = rd.dec_dgts[i / 2];
		} else {
			k = 0;
		}
		if (i & 1) {
			k %= 10;
		} else {
			k /= 10;
		}
		ds[j] = k + '0';
		i += 1;
		j += 1;
	}
	ds[j] = 0;
	return ds;
}

static int
dec_round (struct decacc *s, int c)
{
	int i, j;

	if (c > 0) {
		i = ACCSIZE;
		while (--i) {
			s->dec_dgts[i] = s->dec_dgts[i - 1];
		}
		s->dec_dgts[0] = c;
		s->dec_exp += 1;
		s->dec_ndgts += 1;
	} else {
		i = 0;
		j = 0;
		while (s->dec_dgts[j] == 0 && j < s->dec_ndgts) {
			j += 1;
		}
		if (j == s->dec_ndgts) {
			s->dec_exp = 0;
			s->dec_pos = 1;
		} else if (j) {
			s->dec_exp -= j;
			while (j < s->dec_ndgts) {
				s->dec_dgts[i++] = s->dec_dgts[j++];
			}
			while (i < s->dec_ndgts) {
				s->dec_dgts[i++] = 0;
			}
		}
	}
	i = DECSIZE;
	if (s->dec_pos) {
		j = 1;
	} else {
		j = -1;
	}
	if (s->dec_dgts[DECSIZE] > 49) {
		while (i--) {
			j += s->dec_dgts[i];
			if (j > 99) {
				s->dec_dgts[i] = j - 100;
				j = 1;
			} else if (j < 0) {
				s->dec_dgts[i] = j + 100;
				j = -1;
			} else {
				s->dec_dgts[i] = j;
				break;
			}
		}
	}
	i = s->dec_ndgts;
	if (i > DECSIZE) {
		i = DECSIZE;
	}
	while (i--) {
		if (s->dec_dgts[i]) {
			break;
		}
	}
	s->dec_ndgts = i + 1;
	if (s->dec_exp > 0x3f) {
		s->dec_exp = 0x3f;
		return -1200;
	}
	if (s->dec_exp < -0x40) {
		s->dec_exp = -0x40;
		return -1201;
	}
	return 0;
}

/* Global functions */

void
deccopy (dec_t *src, dec_t *dst)
{
	memcpy (dst, src, sizeof (dec_t));
}

int
deccvint (int i, dec_t *dp)
{
	if ((unsigned)i == (unsigned)VAL_DECPOSNULL (int)) {
		dp->dec_pos = -1;
		dp->dec_exp = 0;
		dp->dec_ndgts = 0;
		return 0;
	}
	return deccvfix ((long)i, dp);
}

int
dectoint (dec_t *dp, int *ip)
{
	long lp;

	if (dp->dec_pos == DECPOSNULL) {
		*ip = VAL_DECPOSNULL (int);
	} else {
		dectofix (dp, &lp);
		*ip = lp;
	}
	return 0;
}

int
deccvlong (long i, dec_t *dp)
{
	if (i == VAL_DECPOSNULL (long)) {
		dp->dec_pos = -1;
		dp->dec_exp = 0;
		dp->dec_ndgts = 0;
		return 0;
	}

	return deccvfix ((long)i, dp);
}

int
dectolong (dec_t *dp, long *ip)
{
	if (dp->dec_pos == DECPOSNULL) {
		*ip = VAL_DECPOSNULL (long);
	} else {
		dectofix (dp, ip);
	}
	return 0;
}

int
deccvdbl (double dbl, dec_t *dp)
{
	return deccvreal (dbl, dp, 16);
}

int
dectodbl (dec_t *dp, double *dblp)
{
	return dectoreal (dp, dblp, 16);
}

int
deccvflt (float flt, dec_t *dp)
{
	return deccvreal (flt, dp, 8);
}

int
dectoflt (dec_t *dp, float *fltp)
{
	double dbl;
	int status;

	status = dectoreal (dp, &dbl, 8);
	*fltp = dbl;
	return status;
}

void
stdecimal (dec_t *dp, unsigned char *cp, int len)
{
	unsigned char *bp;
	unsigned char buffer[DECSIZE];
	unsigned char header;
	int count;

	if (dp->dec_pos == -1) {
		memset (cp, 0, len);
		return;
	}

	header = 0xC0 + dp->dec_exp;
	len--;

	if ((count = dp->dec_ndgts)) {
		memcpy (buffer, dp->dec_dgts, count);

		if (len < count && buffer[len] >= 50) {
			header += round100 (buffer, len);
		}

		if (dp->dec_pos == 0) {
			header = ~header;
			comp100 (buffer, len < count ? len : count);
		}
	}

	*cp = header;

	bp = buffer;
	while (len-- > 0) {
		*++cp = (count-- > 0) ? *bp++ : 0;
	}

	return;
}

int
lddecimal (unsigned char *cp, int len, dec_t *dp)
{
	unsigned char buffer[DECSIZE + 1];
	unsigned char *digits;

	if (*cp == 0) {
		dp->dec_pos = -1;
		dp->dec_exp = 0;
		dp->dec_ndgts = 0;
		return 0;
	}

	if (--len > DECSIZE) {
		len = DECSIZE;
	}

	memcpy (buffer, cp + 1, len);

	if (*cp & 0x80) {
		dp->dec_pos = 1;
		dp->dec_exp = *cp - 0xC0;
	} else {
		comp100 (buffer, len);
		dp->dec_pos = 0;
		dp->dec_exp = 0xFF - (*cp) - 0xC0;
	}

	cp = buffer + len;

	while (len > 0 && *--cp == 0) {
		len--;
	}
	dp->dec_ndgts = len;

	digits = dp->dec_dgts;
	cp = buffer;
	while (len-- > 0) {
		*digits++ = *cp++;
	}

	return 0;
}

int
decsub (dec_t *x, dec_t *y, dec_t *r)
{
	int i;

	if (x->dec_pos == -1 || y->dec_pos == -1) {
		r->dec_pos = -1;
		r->dec_ndgts = 0;
		r->dec_exp = 0;
		return 0;
	}
	y->dec_pos ^= 1;
	i = decadd (x, y, r);
	if (y != r) {
		y->dec_pos ^= 1;
	}
	return i;
}

int
decadd (dec_t *x, dec_t *y, dec_t *r)
{
	int		a, c, i, j;
	struct decacc	*z, zv;
	dec_t		*t;

	if (x->dec_pos == -1 || y->dec_pos == -1) {
		r->dec_pos = -1;
		r->dec_ndgts = 0;
		r->dec_exp = 0;
		return 0;
	}
	z = &zv;
	memset (z, 0, sizeof (struct decacc));
	i = x->dec_pos;
	j = y->dec_pos;
	x->dec_pos = 1;
	y->dec_pos = 1;
	if (deccmp (x, y) < 0) {
		t = x;
		x = y;
		y = t;
		c = i;
		i = j;
		j = c;
	}
	x->dec_pos = i;
	y->dec_pos = j;

	memcpy (z, x, sizeof (dec_t));
	a = x->dec_exp - y->dec_exp;

	if (a > DECSIZE) {
		memcpy (r, x, sizeof (dec_t));
		return 0;
	}
	i = y->dec_ndgts + a;
	if (i > ACCSIZE) {
		i = ACCSIZE;
	}
	if (i > z->dec_ndgts) {
		z->dec_ndgts = i;
	}
	if ((j = i - a) < 0) {
		j = 0;
	}
	c = 0;
	while (i--) {
		if (j) {
			j -= 1;
			if (x->dec_pos == y->dec_pos) {
				c += y->dec_dgts[j];
			} else {
				c -= y->dec_dgts[j];
			}
		}
		c += z->dec_dgts[i];
		if (c < 0) {
			z->dec_dgts[i] = c + 100;
			c = -1;
		} else if (c < 100) {
			z->dec_dgts[i] = c;
			c = 0;
		} else {
			z->dec_dgts[i] = c - 100;
			c = 1;
		}
	}
	i = dec_round (z, c);
	memcpy (r, z, sizeof (dec_t));
	return i;
}

int
decmul (dec_t *x, dec_t *y, dec_t *r)
{
	struct decacc	*p, pv;
	int		i, j, k = 0;

	if (x->dec_pos == -1 || y->dec_pos == -1) {
		r->dec_pos = -1;
		r->dec_ndgts = 0;
		r->dec_exp = 0;
		return 0;
	}
	p = &pv;
	memset (p, 0, sizeof (struct decacc));
	for (i = x->dec_ndgts; i >= 0; i--) {
		k = 0;
		for (j = y->dec_ndgts - 1; j >= 0; j--) {
			if (i + j < ACCSIZE) {
				k += p->dec_dgts[i + j] + x->dec_dgts[i] * y->dec_dgts[j];
				p->dec_dgts[i + j] = k % 100;
				k /= 100;
			}
			if (i) {
				p->dec_dgts[i - 1] = k;
			}
		}
	}
	p->dec_pos = x->dec_pos ^ y->dec_pos ^ 1;
	p->dec_exp = x->dec_exp + y->dec_exp - 1;
	p->dec_ndgts = x->dec_ndgts + y->dec_ndgts;
	if (k) {
		k = dec_round (p, k);
	} else {
		p->dec_ndgts -= 1;
	}
	memcpy (r, p, sizeof (dec_t));
	return k;
}

int
decdiv (dec_t *x, dec_t *y, dec_t *r)
{
	int		j, c, n, i, m, s, t, u = 0;
	struct decacc	*q, qv, *a, av;

	if (x->dec_pos == -1 || y->dec_pos == -1) {
		r->dec_pos = -1;
		r->dec_ndgts = 0;
		r->dec_exp = 0;
		return 0;
	}

	if (y->dec_ndgts == 0) {
		r->dec_pos = 1;
		r->dec_ndgts = 0;
		r->dec_exp = 0;
		return -1202;
	}

	q = &qv;
	a = &av;
	memset (q, 0, sizeof (struct decacc));
	q->dec_exp = x->dec_exp - y->dec_exp + 1;
	q->dec_pos = x->dec_pos ^ y->dec_pos ^ 1;
	q->dec_ndgts = ACCSIZE;
	memcpy (a, x, sizeof (dec_t));
	a->dec_exp = a->dec_pos = a->dec_dgts[ACCSIZE - 1] = 0;

	m = -1;
	for (n = 0; n < ACCSIZE; n += 1) {
		if (n == 0 || a->dec_dgts[n - 1] == 0) {
			i = n;
		} else {
			i = n - 1;
		}
		if (n != 1 || u != 0) {
			m += 1;
		} else {
			q->dec_exp -= 1;
		}
		t = a->dec_dgts[i] * 100;
		if (i < ACCSIZE - 1) {
			t += a->dec_dgts[i + 1];
		}
		t += 1;
		s = y->dec_dgts[0] * 100;
		if (y->dec_ndgts > 1) {
			s += y->dec_dgts[1];
		}
		if (i == n) {
			u = t / s;
		} else {
			u = ((long)t) * 100 / s;
		}
		c = 0;
		if (u) {
			if (u > 99) {
				u = 99;
			}
			j = y->dec_ndgts;
			if (i + j > ACCSIZE) {
				j = ACCSIZE - i;
				c = -(y->dec_dgts[j] * u / 100);
			}
			while (j + n > i) {
				j -= 1;
				c += a->dec_dgts[n + j];
				if (j >= 0) {
					c -= y->dec_dgts[j] * u;
				}
				if (c < 0) {
					a->dec_dgts[n + j] = (c + 10000) % 100;
					c = (c + 1) / 100 - 1;
				} else if (c > 99) {
					a->dec_dgts[n + j] = c % 100;
					c /= 100;
				} else {
					a->dec_dgts[n + j] = c;
					c = 0;
				}
			}
			if (c < 0) {
				c = 0;
				j = y->dec_ndgts;
				if (i + j > ACCSIZE) {
					j = ACCSIZE - i;
				}
				u -= 1;
				while (j + n > i) {
					j -= 1;
					c += a->dec_dgts[n + j];
					if (j >= 0) {
						c += y->dec_dgts[j];
					}
					if (c > 99) {
						a->dec_dgts[n + j] = c - 100;
						c = 1;
					} else {
						a->dec_dgts[n + j] = c;
						c = 0;
					}
				}
			}
		}
		q->dec_dgts[m] = u;
	}
	if (s > 99) {
		s = s / 100;
	}
	q->dec_dgts[DECSIZE] = a->dec_dgts[DECSIZE] * 100 / s;
	i = dec_round (q, 0);
	memcpy (r, q, sizeof (dec_t));
	return i;
}

int
deccmp (dec_t *x, dec_t *y)
{
	int i, s;

	if (x->dec_pos == -1 || y->dec_pos == -1) {
		return -2;
	}
	s = x->dec_pos - y->dec_pos;
	if (s == 0) {
		s = x->dec_exp - y->dec_exp;
		if (s == 0) {
			for (i = 0; i < DECSIZE; i += 1) {
				if (i < x->dec_ndgts) {
					s += x->dec_dgts[i];
				}
				if (i < y->dec_ndgts) {
					s -= y->dec_dgts[i];
				}
				if (s) {
					break;
				}
			}
		}
	}
	if (s > 0) {
		return 1;
	}
	if (s < 0) {
		return -1;
	}
	return 0;
}

int
dectoasc (dec_t *np, char *cp, int ln, int dg)
{
	int i, j, m, t, pt, sg;
	char *v;

	memset (cp, ' ', ln);
	if (np->dec_pos == DECPOSNULL) {
		return 0;
	}
	if (dg <= 0) {
		i = np->dec_ndgts;
		dg = i + i;
		if (dg > 0 && np->dec_dgts[0] < 10) {
			dg--;
		}
		if ((dg > 1 && np->dec_dgts[i - 1] % 10) == 0) {
			dg--;
		}
		if (dg <= 0) {
			dg = 1;
		}
		i = np->dec_pos ^ 1;
		if (dg > ln - i - 1) {
			dg = ln - i - 1;
		}
		v = dececvt (np, dg, &pt, &sg);
		if (pt < 0 && dg + sg - pt + 1 >= ln) {
			goto cv_float;
		}
		if (pt < 0) {
			dg -= pt;
		}
	}
	v = decfcvt (np, dg, &pt, &sg);
	i = strlen (v);
	if (pt != i) {
		i++;
	}
	i += sg;
	if (i > ln) {
		i -= ln;
		if (i <= dg) {
			v = decfcvt (np, dg - i, &pt, &sg);
		}
	}
	i = j = 0;
	if (i < ln && sg) {
		cp[i++] = '-';
	}
	if (i < ln && pt <= 0) {
		cp[i++] = '0';
	}
	m = pt;
	while (m > 0 && v[j] != '\0' && i < ln) {
		cp[i++] = v[j++];
		m--;
	}
	if (i < ln) {
		cp[i++] = '.';
	}
	while (m < 0 && i < ln) {
		cp[i++] = '0';
		m++;
	}
	while (v[j] != '\0' && i < ln) {
		cp[i++] = v[j++];
	}
	if (pt <= ln - sg) {
		return 0;
	}

      cv_float:
	while (ln) {
		memset (cp, ' ', ln);
		m = ln;
		i = 0;
		i = pt - 1;
		if (i < 0) {
			i = -i;
		}
		do {
			if (m > 0) {
				cp[--m] = i % 10 + '0';
			}
			i /= 10;
		} while (m && i);
		if (m && pt <= 0) {
			cp[--m] = '-';
		}
		if (m) {
			cp[--m] = 'e';
		}
		dg = m - 1;
		i = 0;
		if (sg && m) {
			dg -= 1;
			cp[i++] = '-';
		}
		if (i >= m) {
			if (np->dec_exp < -1) {
				memset (cp, ' ', ln);
				cp[0] = '0';
			} else {
				memset (cp, '*', ln);
			}
			return 0;
		}
		if (dg <= 0) {
			dg = 1;
		}
		t = pt;
		v = dececvt (np, dg, &pt, &sg);
		if (t != pt) {
			continue;
		}
		j = 0;
		cp[i++] = v[j++];
		if (i < m) {
			cp[i++] = '.';
		}
		while (i < m) {
			cp[i++] = v[j++];
		}
		while (m && (cp[--m] == '0' || cp[m] == '\0')) {
			for (i = m; i < ln - 1; i++) {
				cp[i] = cp[i + 1];
			}
			cp[i] = ' ';
		}
		if (m && cp[m] == '.') {
			for (i = m; i < ln - 1; i++) {
				cp[i] = cp[i + 1];
			}
			cp[i] = ' ';
		}
		ln = 0;
	}
	return 0;
}

int
deccvasc (char *cp, int ln, dec_t *rp)
{
	int		c;
	int		ps, i, j, xs, xv, ms;
	struct decacc	*np, nv;

	np = &nv;
	memset (np, 0, sizeof nv);
	ps = i = j = xs = xv = ms = 0;
	rp->dec_pos = np->dec_pos = DECPOSNULL;
	np->dec_pos = 1;
	while (i < ln && cp[i] == ' ') {
		i += 1;
	}
	if (i == ln) {
		return 0;
	}
	if (cp[i] == '-') {
		i += 1;
		ms = 0;
	} else {
		if (cp[i] == '+') {
			i++;
		}
		ms = 1;
	}
	while (i < ln) {
		c = cp[i++];
		if (c >= '0' && c <= '9') {
			if (ps) {
				ps--;
			}
			c = c - '0';
			if ((j || c) && j < ACCSIZE * 2) {
				if (j & 1) {
					np->dec_dgts[j / 2] += c;
				} else {
					np->dec_dgts[j / 2] = c * 10;
				}
				j += 1;
			}
		} else if (c == '.') {
			if (ps) {
				return -1213;
			} else {
				ps -= 1;
			}
		} else {
			break;
		}
	}
	if (i < ln && (c == 'e' || c == 'E')) {
		c = cp[i++];
		if (c == '+') {
			xs = 1;
			c = cp[i++];
		} else if (c == '-') {
			xs = -1;
			c = cp[i++];
		}
		while (i <= ln) {
			if (c < '0' || c > '9') {
				break;
			}
			if ((xv = xv * 10 + c - '0') >= 1000) {
				return -1216;
			}
			c = cp[i++];
		}
	}
	if (i < ln) {
		if (cp[i] != ' ') {
			return -1213;
		}
	}
	if (xs == -1) {
		xv = -xv;
	}
	xv += j + 1;
	if (ps) {
		xv += ps + 1;
	}
	np->dec_ndgts = (j + 1) / 2;
	i = xv;
	if (i < 0) {
		i--;
	}
	np->dec_exp = i / 2;
	if ((xv & 1) == 0) {
		if ((j & 1) == 0) {
			np->dec_ndgts++;
		}
		j = 0;
		for (i = 0; i < ACCSIZE; i++) {
			xs = np->dec_dgts[i];
			np->dec_dgts[i] = xs / 10 + j;
			j = (xs % 10) * 10;
		}
	}
	i = dec_round (np, 0);
	np->dec_pos = ms;
	memcpy (rp, np, sizeof (dec_t));
	return i;
}

char *
dececvt (dec_t *np, int dg, int *pt, int *sg)
{
	return decefcvt (np, dg, pt, sg, 0);
}

char *
decfcvt (dec_t *np, int dg, int *pt, int *sg)
{
	return decefcvt (np, dg, pt, sg, 1);
}
