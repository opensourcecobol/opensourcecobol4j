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

#ifndef	VBISAM_INCL	/* avoid multiple include problems */
#define	VBISAM_INCL

#include <sys/types.h>

#define	VBISAM_USE_LONG_LONG	/* Note hack for now */

#ifdef	VBISAM_USE_LONG_LONG
#define	vbisam_off_t	long long
#else
#define	vbisam_off_t	int
#endif

#include	<vbdecimal.h>

#ifdef	_MSC_VER
#ifdef VBISAM_LIB
#define DLL_EXPIMP __declspec(dllexport)
#else
#define DLL_EXPIMP __declspec(dllimport)
#endif
#else
#define DLL_EXPIMP
#endif

#define	CHARTYPE	0
#define	INTTYPE		1
#define	LONGTYPE	2
#define	DOUBLETYPE	3
#define	FLOATTYPE	4
#define	QUADTYPE	5

#define	CHARSIZE	1
#define	INTSIZE		2
#define	LONGSIZE	4
#define	FLOATSIZE	(sizeof (float))
#define	DOUBLESIZE	(sizeof (double))

#define	ISDESC		0x80

#define	BYTEMASK	0xFF	/* Mask for one byte */
#define	BYTESHFT	8	/* Shift for one byte */

#define	ISFIRST		0	/* Position to first row */
#define	ISLAST		1	/* Position to last row */
#define	ISNEXT		2	/* Position to next row */
#define	ISPREV		3	/* Position to previous row */
#define	ISCURR		4	/* Position to current row */
#define	ISEQUAL		5	/* Position to equal value */
#define	ISGREAT		6	/* Position to greater value */
#define	ISGTEQ		7	/* Position to >= value */

/* isread () locking modes */
#define	ISLOCK		0x100	/* Row lock */
#define	ISSKIPLOCK	0x200	/* Skip row even if locked */
#define	ISWAIT		0x400	/* Wait for row lock */
#define	ISLCKW		ISLOCK | ISWAIT

/* isstart () lock modes */
#define	ISKEEPLOCK	0x800	/* Keep rec lock in autolk mode */

/* isopen (), isbuild () lock modes */
#define	ISAUTOLOCK	0x200	/* Automatic row lock */
#define	ISMANULOCK	0x400	/* Manual row lock */
#define	ISEXCLLOCK	0x800	/* Exclusive isam file lock */

/* isopen (), isbuild () file types */
#define	ISINPUT		0	/* Open for input only */
#define	ISOUTPUT	1	/* Open for output only */
#define	ISINOUT		2	/* Open for input and output */
#define	ISTRANS		4	/* Open for transaction proc */
#define	ISNOLOG		8	/* No loggin for this file */
#define	ISVARLEN	0x10	/* Variable length rows */
#define	ISFIXLEN	0x0	/* (Non-flag) Fixed length rows only */

/* audit trail mode parameters */
#define	AUDSETNAME	0	/* Set new audit trail name */
#define	AUDGETNAME	1	/* Get audit trail name */
#define	AUDSTART	2	/* Start audit trail */
#define	AUDSTOP		3	/* Stop audit trail */
#define	AUDINFO		4	/* Audit trail running */

#define	VB_MAX_KEYLEN	511	/* BUG - FIXME! Maximum number of bytes in a key */
#define	NPARTS		8	/* Maximum number of key parts */

struct	keypart {
	short	kp_start;	/* Starting byte of key part */
	short	kp_leng;	/* Length in bytes */
	short	kp_type;	/* Type of key part (include ISDESC as needed) */
};

struct keydesc {
	short		k_flags;		/* Flags (Compression) */
	short		k_nparts;		/* Number of parts in this key */
	struct keypart	k_part[NPARTS];		/* Each key part */
	short		k_len;			/* Length of entire uncompressed key */
	vbisam_off_t	k_rootnode;		/* Pointer to rootnode */
};

#define	k_start	k_part[0].kp_start
#define	k_leng	k_part[0].kp_leng
#define	k_type	k_part[0].kp_type

/* Possible values for iFlags */
#define	ISNODUPS	0x00	/* No duplicates allowed */
#define	ISDUPS		0x01	/* Duplicates allowed */
#define	DCOMPRESS	0x02	/* Duplicate compression */
#define	LCOMPRESS	0x04	/* Leading compression */
#define	TCOMPRESS	0x08	/* Trailing compression */
#define	COMPRESS	0x0e	/* All compression */
#define	ISCLUSTER	0x00

struct	dictinfo
{
	short		di_nkeys;	/* Number of keys defined (msb set if VARLEN) */
	short		di_recsize;	/* Maximum data row length */
	short		di_idxsize;	/* Number of bytes in an index node */
	vbisam_off_t	di_nrecords;	/* Number of rows in data file */
};

/* Possible error return values */
#define	EDUPL		100	/* Duplicate row */
#define	ENOTOPEN	101	/* File not open */
#define	EBADARG		102	/* Illegal argument */
#define	EBADKEY		103	/* Illegal key desc */
#define	ETOOMANY	104	/* Too many files open */
#define	EBADFILE	105	/* Bad isam file format */
#define	ENOTEXCL	106	/* Non-exclusive access */
#define	ELOCKED		107	/* Row locked */
#define	EKEXISTS	108	/* Key already exists */
#define	EPRIMKEY	109	/* Is primary key */
#define	EENDFILE	110	/* End/begin of file */
#define	ENOREC		111	/* No row found */
#define	ENOCURR		112	/* No current row */
#define	EFLOCKED	113	/* File locked */
#define	EFNAME		114	/* File name too long */
#define	EBADMEM		116	/* Can't alloc memory */
#define	ELOGREAD	118	/* Cannot read log rec */
#define	EBADLOG		119	/* Bad log row */
#define	ELOGOPEN	120	/* Cannot open log file */
#define	ELOGWRIT	121	/* Cannot write log rec */
#define	ENOTRANS	122	/* No transaction */
#define	ENOBEGIN	124	/* No begin work yet */
#define	ENOPRIM		127	/* No primary key */
#define	ENOLOG		128	/* No logging */
#define	ENOFREE		131	/* No free disk space */
#define	EROWSIZE	132	/* Row size too short / long */
#define	EAUDIT		133	/* Audit trail exists */
#define	ENOLOCKS	134	/* No more locks */
#define	EDEADLOK	143	/* Deadlock avoidance */
#define	ENOMANU		153	/* Must be in ISMANULOCK mode */
#define	EINTERUPT	157	/* Interrupted isam call */
#define	EBADFORMAT	171	/* Locking or NODESIZE change */

DLL_EXPIMP extern int	iserrno;	/* Isam error return code */
DLL_EXPIMP extern int	iserrio;	/* NOT used with VBISAM */
DLL_EXPIMP extern int	isreclen;	/* Used for varlen tables */
DLL_EXPIMP extern int	isrecnum;	/* Current row number */

struct	audhead
{
	char	au_type[2];	/* Audit row type aa,dd,rr,ww */
	char	au_time[4];	/* Audit date-time */
	char	au_procid[2];	/* Process id number */
	char	au_userid[2];	/* User id number */
	char	au_recnum[4];	/* Row number - RXW Set to 4 */
	char	au_reclen[2];	/* audit row length beyond header */
};

/* Number of bytes in audit header */
#define	AUDHEADSIZE	14
/* VARLEN num of bytes in audit header */
#define	VAUDHEADSIZE	16

/* Prototypes for file manipulation functions */
extern int	isaddindex (const int ihandle, struct keydesc *pskeydesc);
extern int	isaudit (const int ihandle, char *pcfilename, int imode);
extern int	isbegin (void);
extern int	isbuild (const char *pcfilename, const int imaxrowlength,
			struct keydesc *pskey, int imode);
extern int	ischeck (const char *pcfile);
extern int	iscleanup (void);
extern int	isclose (const int ihandle);
extern int	iscluster (const int ihandle, struct keydesc *pskeydesc);
extern int	iscommit (void);
extern int	isdelcurr (const int ihandle);
extern int	isdelete (const int ihandle, char *pcrow);
extern int	isdelindex (const int ihandle, struct keydesc *pskeydesc);
extern int	isdelrec (const int ihandle, vbisam_off_t trownumber);
extern int	iserase (char *pcfilename);
extern int	isflush (const int ihandle);
extern int	isfullclose (const int ihandle);
extern int	isindexinfo (const int ihandle, void *pskeydesc, const int ikeynumber);
extern int	islock (const int ihandle);
extern int	islogclose (void);
extern int	islogopen (const char *pcfilename);
extern int	isopen (const char *pcfilename, int imode);
extern int	isread (const int ihandle, char *pcrow, int imode);
extern int	isrecover (void);
extern int	isrelcurr (const int ihandle);
extern int	isrelease (const int ihandle);
extern int	isrelrec (const int ihandle, const vbisam_off_t trownumber);
extern int	isrename (char *pcoldname, char *pcnewname);
extern int	isrewcurr (const int ihandle, char *pcrow);
extern int	isrewrec (const int ihandle, const vbisam_off_t trownumber,
			char *pcrow);
extern int	isrewrite (const int ihandle, char *pcrow);
extern int	isrollback (void);
extern int	issetcollate (const int ihandle, const unsigned char *collating_sequence);
extern int	issetunique (const int ihandle, const vbisam_off_t tuniqueid);
extern int	isstart (const int ihandle, struct keydesc *pskeydesc,
			int ilength, char *pcrow, int imode);
extern int	isuniqueid (const int ihandle, vbisam_off_t *ptuniqueid);
extern int	isunlock (const int ihandle);
extern int	iswrcurr (const int ihandle, char *pcrow);
extern int	iswrite (const int ihandle, char *pcrow);

extern char		*isdi_name (const int ihandle);
extern int		isdi_datlen (const int ihandle);
extern int		isdi_curidx (const int ihandle);
extern int		isdi_idxfd (const int ihandle);
extern int		isdi_datfd (const int ihandle);
extern struct keydesc	*isdi_kdsc (const int ihandle);

extern void	ldchar (char *pcsource, int ilength, char *pcdestination);
extern void	stchar (char *pcsource, char *pcdestination, int ilength);
extern int	ldint (void *pclocation);
extern void	stint (int ivalue, void *pclocation);
extern int	ldlong (void *pclocation);
extern void	stlong (int lvalue, void *pclocation);
/* RXW
extern vbisam_off_t	ldquad (void *pclocation);
extern void	stquad (vbisam_off_t tvalue, void *pclocation);
*/
extern double	ldfloat (void *pclocation);
extern void	stfloat (double dsource, void *pcdestination);
extern double	ldfltnull (void *pclocation, short *pinullflag);
extern void	stfltnull (double dsource, void *pcdestination, int inullflag);
extern double	lddbl (void *pclocation);
extern void	stdbl (double dsource, void *pcdestination);
extern double	lddblnull (void *pclocation, short *pinullflag);
extern void	stdblnull (double dsource, void *pcdestination, int inullflag);
extern int	lddecimal (unsigned char *cp, int len, dec_t *dp);
extern void	stdecimal (dec_t *dp, unsigned char *cp, int len);

#endif	/* VBISAM_INCL */
