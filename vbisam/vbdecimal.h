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
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#ifndef	_VBDECIMAL_H

/* DEFINES */
#define DECSIZE		16
#define DECUNKNOWN	-2
#define DECPOSNULL	(-1)	/* if dec_pos == DECPOSNULL then value is 
					TRUE NULL (less than anything) */


/** STRUCTURES */

/* the structure of an UNPACKED decimal */
struct decimal {
	short		dec_exp;		/* the exponent */
	short		dec_pos;		/* is the value "positive", flag */
	short		dec_ndgts;		/* the number of valid digits in dec_dgts */
	unsigned char	dec_dgts[DECSIZE];	/* the digits, base 100 */
};

typedef struct decimal dec_t;

/* PSEUDO FUNCTIONS */
/* declen, sig = # of significant digits, rd # digits to right of decimal,
           returns # bytes required to hold such */

#define DECLEN(sig,rd)		(((sig) + ((rd)&1) + 3) / 2)
#define DECLENGTH(len)		DECLEN(PRECTOT(len), PRECDEC(len))
#define DECPREC(size)		((size - 1) << 9) + 2)
#define PRECTOT(len)		(((len) >> 8 ) & 0xff)
#define PRECDEC(len)		((len) & 0xff)
#define PRECMAKE(len,dlen)	(((len) << 8 ) + (dlen))

/*
** value of an integer that generates a decimal flagged DECPOSNULL
**     an int of 2 bytes produces 0x8000
**     an int of 4 bytes produces 0x80000000
*/

#define VAL_DECPOSNULL(type)	(1L << ((sizeof (type) * 8) - 1))

/* FUNCTION DECLARATIONS */
extern int	deccvasc (char *cp, int ln, dec_t *rp);
extern int	dectoasc (dec_t *np, char *cp, int ln, int dg);
extern int	deccvint (int i, dec_t *dp);
extern int	dectoint (dec_t *dp, int *ip);
extern int	deccvlong (long i, dec_t *dp);
extern int	dectolong (dec_t *dp, long *ip);
extern int	deccvflt (float flt, dec_t *dp);
extern int	dectoflt (dec_t *dp, float *fltp);
extern int	deccvdbl (double dbl, dec_t *dp);
extern int	dectodbl (dec_t *dp, double *dblp);
extern int	decadd (dec_t *x, dec_t *y, dec_t *r);
extern int	decsub (dec_t *x, dec_t *y, dec_t *r);
extern int	decmul (dec_t *x, dec_t *y, dec_t *r);
extern int	decdiv (dec_t *x, dec_t *y, dec_t *r);
extern int	deccmp (dec_t *x, dec_t *y);
extern void	deccopy (dec_t *src, dec_t *dst);
extern char	*dececvt (dec_t *np, int dg, int *pt, int *sg);
extern char	*decfcvt (dec_t *np, int dg, int *pt, int *sg);

#endif
