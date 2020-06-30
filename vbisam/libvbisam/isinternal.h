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

#ifndef	VB_LIBVBISAM_H
#define	VB_LIBVBISAM_H

#include	"config.h"

/* Note : following are pulled in from config.h : */
/* ISAMMODE		- ISAM Mode */
/* HAVE_LFS64		- 64 bit file I/O */
/* VBDEBUG		- Debugging mode */

#ifdef WITH_LFS64
#define _LFS64_LARGEFILE	1
#define _LFS64_STDIO		1
#define _FILE_OFFSET_BITS	64
#define _LARGEFILE64_SOURCE	1
#define __USE_LARGEFILE64	1
#define __USE_FILE_OFFSET64	1
#ifdef _AIX
#define _LARGE_FILES		1
#endif /* _AIX */
#if defined(__hpux__) && !defined(__LP64__)
#define _APP32_64BIT_OFF_T	1
#endif
#endif

#ifdef  HAVE_UNISTD_H
#include	<unistd.h>
#endif
#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<ctype.h>
#ifdef	HAVE_FCNTL_H
#include	<fcntl.h>
#endif
#include	<stdlib.h>
#include	<string.h>
#include	<errno.h>
#include	<time.h>
#include	<limits.h>
#include	<float.h>

#ifdef _WIN32
#define WINDOWS_LEAN_AND_MEAN
#include	<windows.h>
#include	<io.h>
#include	<sys/locking.h>
#define	open(x, y, z)	_open(x, y, z)
#define	read(x, y, z)	_read(x, y, z)
#define	write(x, y, z)	_write(x, y, z)
#ifdef WITH_LFS64
#define	lseek(x, y, z)	_lseeki64(x, y, z)
#else
#define	lseek(x, y, z)	_lseek(x, y, z)
#endif
#define	close(x)	_close(x)
#define	unlink(x)	_unlink(x)
#define fsync		_commit
typedef int		uid_t;
#ifndef	_PID_T_
typedef int		pid_t;
#endif
#ifndef	_MODE_T_
typedef unsigned short	mode_t;
#endif
#ifndef	_SSIZE_T_
typedef int		ssize_t;
#endif
#endif

#ifdef _MSC_VER

#define _CRT_SECURE_NO_DEPRECATE 1
#define inline _inline
#include	<malloc.h>
#pragma warning(disable: 4996)
#define strncasecmp _strnicmp
#define strcasecmp _stricmp
#define __attribute__(x)
#define __i386__

#endif /* _MSC_VER */

#ifdef  __370__
#define inline __inline
#endif

#if	ISAMMODE == 1
#define QUADSIZE	8
#define VB_OFFLEN_7F	0x7fffffffffffffffLL
#define VB_OFFLEN_3F	0x3fffffffffffffffLL
#define VB_OFFLEN_40	0x4000000000000000LL
#else
#define QUADSIZE	4
#define VB_OFFLEN_7F	0x7FFFFFFF
#define VB_OFFLEN_3F	0x3FFFFFFF
#define VB_OFFLEN_40	0x40000000
#endif

#define	MAXSUBS		32	/* Maximum number of indexes per table */
#define	VB_MAX_FILES	128	/* Maximum number of open VBISAM files */

#define	VBISAM_LIB
#include	"vbisam.h"
#include	"byteswap.h"

#if defined(__GNUC__) && defined(linux) && (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 3))
#define VB_HIDDEN	__attribute__ ((visibility("hidden")))
#else
#define VB_HIDDEN
#endif

#ifndef	O_BINARY
#define	O_BINARY	0
#endif

#ifdef	WITH_LFS64
#ifdef	_MSC_VER
#define off_t __int64
#endif
#endif

#ifdef	HAVE_BUILTIN_EXPECT
#define likely(x)	__builtin_expect(!!(x), 1)
#define unlikely(x)	__builtin_expect(!!(x), 0)
#else
#define likely(x)	(x)
#define unlikely(x)	(x)
#endif

typedef	unsigned char *	ucharptr;

#if !defined(__i386__) && !defined(__x86_64__) && !defined(__powerpc__) && !defined(__powerpc64__) && !defined(__ppc__) && !defined(__amd64__)
	#if defined(_MSC_VER)
		#define ALLOW_MISALIGNED
		#define MISALIGNED __unaligned
	#else
		#define MISALIGNED
	#endif
#else
	#define ALLOW_MISALIGNED
	#define MISALIGNED
#endif

/* Inline versions of load/store routines */

#if defined(ALLOW_MISALIGNED) && !defined(WORDS_BIGENDIAN)
#define	inl_ldint(x)	(int)(VB_BSWAP_16(*((unsigned short MISALIGNED *)(x))))
#else
static inline int
inl_ldint (void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
	return (int)VB_BSWAP_16(*(unsigned short MISALIGNED *)pclocation);
#else
	short	ivalue = 0;
	unsigned char	*pctemp = (unsigned char *) &ivalue;
	unsigned char	*pctemp2 = (unsigned char *) pclocation;

	*(pctemp + 0) = *(pctemp2 + 0);
	*(pctemp + 1) = *(pctemp2 + 1);
	return (int)ivalue;
#endif
}
#endif

#if defined(ALLOW_MISALIGNED) && !defined(WORDS_BIGENDIAN)
#define	inl_stint(x,y)	*(unsigned short MISALIGNED *)(y) = VB_BSWAP_16((unsigned short)(x));
#else
static inline void
inl_stint (int ivalue, void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
	*(unsigned short MISALIGNED *)pclocation = VB_BSWAP_16((unsigned short)ivalue);
#else
	unsigned char	*pctemp = (char *) &ivalue;
	unsigned char	*pctemp2 = (unsigned char *) pclocation;

	*(pctemp2 + 0) = *(pctemp + 0 + INTSIZE);
	*(pctemp2 + 1) = *(pctemp + 1 + INTSIZE);
#endif
	return;
}
#endif

#if defined(ALLOW_MISALIGNED) && !defined(WORDS_BIGENDIAN)
#define	inl_ldlong(x)	(int)(VB_BSWAP_32(*((unsigned short MISALIGNED *)(x))))
#else
static inline int
inl_ldlong (void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
	return VB_BSWAP_32(*(unsigned int MISALIGNED *)pclocation);
#else
	int	lvalue;

	memcpy((unsigned char *)&lvalue, (unsigned char *)pclocation, 4);
	return lvalue;
#endif
}
#endif

#if defined(ALLOW_MISALIGNED) && !defined(WORDS_BIGENDIAN)
#define	inl_stlong(x,y)	*(unsigned int MISALIGNED *)(y) = VB_BSWAP_32((unsigned int)(x));
#else
static inline void
inl_stlong (int lvalue, void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
	*(unsigned int MISALIGNED *)pclocation = VB_BSWAP_32((unsigned int)lvalue);
#else
	memcpy((unsigned char *)pclocation, (unsigned char *)&lvalue, 4);
#endif
	return;
}
#endif

static inline off_t
inl_ldquad (void *pclocation)
{

#ifndef	WORDS_BIGENDIAN
#if	ISAMMODE == 1
	return VB_BSWAP_64(*(unsigned long long MISALIGNED *)pclocation);
#else
	return VB_BSWAP_32(*(unsigned int MISALIGNED *)pclocation);
#endif
#else
	off_t	tvalue = 0;

#if	ISAMMODE == 1
	memcpy((unsigned char *)&tvalue, (unsigned char *)pclocation, 8);
#else
	memcpy((unsigned char *)&tvalue, (unsigned char *)pclocation, 4);
#endif
	return tvalue;
#endif
}

static inline void
inl_stquad (off_t tvalue, void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
#if	ISAMMODE == 1
	*(unsigned long long MISALIGNED *)pclocation = VB_BSWAP_64((unsigned long long)tvalue);
#else
	*(unsigned int MISALIGNED *)pclocation = VB_BSWAP_32((unsigned int)tvalue);
#endif
#else
#if	ISAMMODE == 1
	memcpy((unsigned char *)pclocation, (unsigned char *)&tvalue, 8);
#else
	memcpy((unsigned char *)pclocation, (unsigned char *)&tvalue, 4);
#endif
#endif
	return;
}

/* Implementation limits */
/* 64-bit versions have a maximum node length of 4096 bytes */
/* 32-bit versions have a maximum node length of 1024 bytes */

#if	ISAMMODE == 1
#define	MAX_NODE_LENGTH		4096
#else
#define	MAX_NODE_LENGTH		1024
#endif

#define	VB_NODE_MAX		4096
#define MAX_ISREC_LENGTH	32511
#define MAX_RESERVED_LENGTH	32768	/* Greater then MAX_ISREC_LENGTH */
#define MAX_BUFFER_LENGTH	65536

/* Arguments to ivblock */
#define	VBUNLOCK	0	/* Unlock */
#define	VBRDLOCK	1	/* A simple read lock, non-blocking */
#define	VBRDLCKW	2	/* A simple read lock, blocking */
#define	VBWRLOCK	3	/* An exclusive write lock, non-blocking */
#define	VBWRLCKW	4	/* An exclusive write lock, blocking */

/* Values for ivbrcvmode (used to control how isrecover works) */
#define	RECOV_C		0x00	/* Boring old buggy C-ISAM mode */
#define	RECOV_VB	0x01	/* New, improved error detection */
#define	RECOV_LK	0x02	/* Use locks in isrecover (See documentation) */

/* Values for ivbintrans */
#define	VBNOTRANS	0	/* NOT in a transaction at all */
#define	VBBEGIN		1	/* An isbegin has been issued but no more */
#define	VBNEEDFLUSH	2	/* Something BEYOND just isbegin has been issued */
#define	VBCOMMIT	3	/* We're in 'iscommit' mode */
#define	VBROLLBACK	4	/* We're in 'isrollback' mode */
#define	VBRECOVER	5	/* We're in 'isrecover' mode */

VB_HIDDEN extern	int	ivbintrans;
VB_HIDDEN extern	int	ivblogfilehandle;
VB_HIDDEN extern	int	ivbmaxusedhandle;

struct	VBLOCK {
	struct	VBLOCK *psnext;
	int		ihandle;	/* The handle that 'applied' this lock */
	off_t		trownumber;
};

struct	VBKEY {
	struct	VBKEY	*psnext;	/* Next key in this node */
	struct	VBKEY	*psprev;	/* Previous key in this node */
	struct	VBTREE	*psparent;	/* Pointer towards ROOT */
	struct	VBTREE	*pschild;	/* Pointer towards LEAF */
	off_t		trownode;	/* The row / node number */
	off_t		tdupnumber;	/* The duplicate number (1st = 0) */
	unsigned char	iisnew;		/* If this is a new entry (split use) */
	unsigned char	iishigh;	/* Is this a GREATER THAN key? */
	unsigned char	iisdummy;	/* A simple end of node marker */
	unsigned char	spare[5];	/* Spare */
	unsigned char	ckey[1];	/* Placeholder for the key itself */
};

struct	VBTREE {
	struct	VBTREE	*psnext;	/* Used for the free list only! */
	struct	VBTREE	*psparent;	/* The next level up from this node */
	struct	VBKEY	*pskeyfirst;	/* Pointer to the FIRST key in this node */
	struct	VBKEY	*pskeylast;	/* Pointer to the LAST key in this node */
	struct	VBKEY	*pskeycurr;	/* Pointer to the CURRENT key */
	off_t		tnodenumber;	/* The actual node */
	off_t		ttransnumber;	/* Transaction number stamp */
	unsigned int	ilevel;		/* The level number (0 = LEAF) */
	unsigned int	ikeysinnode;	/* # keys in pskeylist */
	unsigned char	iisroot;	/* 1 = This is the ROOT node */
	unsigned char	iistof;		/* 1 = First entry in index */
	unsigned char	iiseof;		/* 1 = Last entry in index */
	unsigned char	spare[5];	/* Spare */
#if	ISAMMODE == 1			/* Highly non-portable.. kind of */
	struct	VBKEY	*pskeylist[512];
#else
	struct	VBKEY	*pskeylist[256];
#endif
};

struct	DICTNODE {			/* Offset	32Val	64Val */
					/* 32IO	64IO */
	char	cvalidation[2];		/* 0x00	0x00	0xfe53	0x5642 */
	char	cheaderrsvd;		/* 0x02 0x02	0x02	Same */
	char	cfooterrsvd;		/* 0x03	0x03	0x02	Same */
	char	crsvdperkey;		/* 0x04	0x04	0x04	0x08 */
	char	crfu1;			/* 0x05 0x05	0x04	Same */
	char	cnodesize[INTSIZE];	/* 0x06	0x06	0x03ff	0x0fff */
	char	cindexcount[INTSIZE];	/* 0x08	0x08	Varies	Same */
	char	crfu2[2];		/* 0x0a	0x0a	0x0704	Same */
	char	cfileversion;		/* 0x0c	0x0c	0x00	Same */
	char	cminrowlength[INTSIZE];	/* 0x0d	0x0d	Varies	Same */
	char	cnodekeydesc[QUADSIZE];	/* 0x0f	0x0f	Normally 2 */
	char	clocalindex;		/* 0x13	0x17	0x00	Same */
	char	crfu3[5];		/* 0x14	0x18	0x00...	Same */
	char	cdatafree[QUADSIZE];	/* 0x19	0x1d	Varies	Same */
	char	cnodefree[QUADSIZE];	/* 0x1d	0x25	Varies	Same */
	char	cdatacount[QUADSIZE];	/* 0x21	0x2d	Varies	Same */
	char	cnodecount[QUADSIZE];	/* 0x25	0x35	Varies	Same */
	char	ctransnumber[QUADSIZE];	/* 0x29	0x3d	Varies	Same */
	char	cuniqueid[QUADSIZE];	/* 0x2d	0x45	Varies	Same */
	char	cnodeaudit[QUADSIZE];	/* 0x31	0x4d	Varies	Same */
	char	clockmethod[INTSIZE];	/* 0x35	0x55	0x0008	Same */
	char	crfu4[QUADSIZE];	/* 0x37	0x57	0x00...	Same */
	char	cmaxrowlength[INTSIZE];	/* 0x3b	0x5f	Varies	Same */
	char	cvarleng0[QUADSIZE];	/* 0x3d	0x61	Varies	Same */
	char	cvarleng1[QUADSIZE];	/* 0x41	0x69	Varies	Same */
	char	cvarleng2[QUADSIZE];	/* 0x45	0x71	Varies	Same */
	char	cvarleng3[QUADSIZE];	/* 0x49	0x79	Varies	Same */
	char	cvarleng4[QUADSIZE];	/* 0x4d	0x81	Varies	Same */
#if	ISAMMODE == 1
	char	cvarleng5[QUADSIZE];	/*	0x89	Varies	Same */
	char	cvarleng6[QUADSIZE];	/*	0x91	Varies	Same */
	char	cvarleng7[QUADSIZE];	/*	0x99	Varies	Same */
	char	cvarleng8[QUADSIZE];	/*	0xa1	Varies	Same */
#endif
	char	crfulocalindex[36];	/* 0x51	0xa9	0x00...	Same */
			/*		   ---- ---- */
			/* Length Total	   0x75	0xcd */
};

struct	DICTINFO {
	int	inkeys;		/* Number of keys */
	int	iactivekey;	/* Which key is the active key */
	int	inodesize;	/* Number of bytes in an index block */
	int	iminrowlength;	/* Minimum data row length */
	int	imaxrowlength;	/* Maximum data row length */
	int	idatahandle;	/* file descriptor of the .dat file */
	int	iindexhandle;	/* file descriptor of the .idx file */
	int	iisopen;	/* 0: Table open, Files open, Buffers OK */
				/* 1: Table closed, Files open, Buffers OK */
				/*	Used to retain locks */
				/* 2: Table closed, Files closed, Buffers OK */
				/*	Basically, just caching */
	int	iopenmode;	/* The type of open which was used */
	int	ivarlenlength;	/* Length of varlen component */
	int	ivarlenslot;	/* The slot number within tvarlennode */
	int	dspare1;
	off_t	trownumber;	/* Which data row is "CURRENT" 0 if none */
	off_t	tdupnumber;	/* Which duplicate number is "CURRENT" (0=First) */
	off_t	trowstart;	/* ONLY set to nonzero by isstart() */
	off_t	ttranslast;	/* Used to see whether to set iindexchanged */
	off_t	tnrows;		/* Number of rows (0 IF EMPTY, 1 IF NOT) */
	off_t	tvarlennode;	/* Node containing 1st varlen data */
	char	*cfilename;
	char	*ppcrowbuffer;	/* tminrowlength buffer for key (re)construction */
	const unsigned char	*collating_sequence; /* Collating sequence */
	unsigned char		iisdisjoint;	/* If set, CURR & NEXT give same result */
	unsigned char		iisdatalocked;	/* If set, islock() is active */
	unsigned char		iisdictlocked;	/* Relates to sdictnode below */
				/* 0x00: Content on file MIGHT be different */
				/* 0x01: Dictionary node is LOCKED */
				/* 0x02: sdictnode needs to be rewritten */
				/* 0x04: do NOT increment Dict.Trans */
				/*       isrollback () is in progress */
				/*      (Thus, suppress some ivbenter/ivbexit) */
	unsigned char		iindexchanged;	/* Various */
				/* 0: Index has NOT changed since last time */
				/* 1: Index has changed, blocks invalid */
				/* 2: Index has changed, blocks are valid */
	unsigned char		itransyet;	/* Relates to isbegin () et al */
				/* 0: FO Trans not yet written */
				/* 1: FO Trans written outside isbegin */
				/* 2: FO Trans written within isbegin */
	unsigned char		iisammode;	/* ISAM mode */
	unsigned char		dspare2[2];	/* spare */
	struct	DICTNODE	sdictnode;	/* Holds dictionary node data */
	struct	keydesc		*pskeydesc[MAXSUBS];	/* Array of key description info */
	struct	VBTREE		*pstree[MAXSUBS]; /* Linked list of index nodes */
	struct	VBKEY		*pskeyfree[MAXSUBS]; /* An array of linked lists of free VBKEYs */
	struct  VBKEY		*pskeycurr[MAXSUBS]; /* An array of 'current' VBKEY pointers */
};

VB_HIDDEN extern	struct	DICTINFO *psvbfile[VB_MAX_FILES + 1];

struct	VBFILE {
	struct VBLOCK	*pslockhead;	/* Ordered linked list of locked row numbers */
	struct VBLOCK	*pslocktail;
	int		ihandle;
	int		irefcount;	/* How many times we are 'open' */
	dev_t		tdevice;
	ino_t		tinode;
#ifdef	_WIN32
	HANDLE		whandle;
	char		*cfilename;
#endif
};

VB_HIDDEN extern	struct	VBFILE svbfile[VB_MAX_FILES * 3];

#define	VBL_BUILD	("BU")
#define	VBL_BEGIN	("BW")
#define	VBL_CREINDEX	("CI")
#define	VBL_CLUSTER	("CL")
#define	VBL_COMMIT	("CW")
#define	VBL_DELETE	("DE")
#define	VBL_DELINDEX	("DI")
#define	VBL_FILEERASE	("ER")
#define	VBL_FILECLOSE	("FC")
#define	VBL_FILEOPEN	("FO")
#define	VBL_INSERT	("IN")
#define	VBL_RENAME	("RE")
#define	VBL_ROLLBACK	("RW")
#define	VBL_SETUNIQUE	("SU")
#define	VBL_UNIQUEID	("UN")
#define	VBL_UPDATE	("UP")

struct	SLOGHDR {
	char	clength[INTSIZE];
	char	coperation[2];
	char	cpid[INTSIZE];
	char	cuid[INTSIZE];
	char	ctime[LONGSIZE];
	char	crfu1[INTSIZE];
	char	clastposn[INTSIZE];
	char	clastlength[INTSIZE];
};

/* isopen.c */
VB_HIDDEN extern int	ivbforceexit (const int ihandle);
VB_HIDDEN extern int	ivbclose2 (const int ihandle);
VB_HIDDEN extern void	ivbclose3 (const int ihandle);

/* isread.c */
VB_HIDDEN extern int	ivbcheckkey (const int ihandle, struct keydesc *pskey,
			     const int imode, int irowlength, const int iisbuild);

/* istrans.c */
VB_HIDDEN extern int	ivbtransbuild (const char *pcfilename, const int iminrowlen, const int imaxrowlen,
			       struct keydesc *pskeydesc, const int imode);
VB_HIDDEN extern int	ivbtranscreateindex (const int ihandle, struct keydesc *pskeydesc);
VB_HIDDEN extern int	ivbtranscluster (void);
VB_HIDDEN extern int	ivbtransdelete (const int ihandle, off_t trownumber, int irowlength);
VB_HIDDEN extern int	ivbtransdeleteindex (const int ihandle, struct keydesc *pskeydesc);
VB_HIDDEN extern int	ivbtranserase (const char *pcfilename);
VB_HIDDEN extern int	ivbtransclose (const int ihandle, const char *pcfilename);
VB_HIDDEN extern int	ivbtransopen (const int ihandle, const char *pcfilename);
VB_HIDDEN extern int	ivbtransinsert (const int ihandle, const off_t trownumber,
				int irowlength, char *pcrow);
VB_HIDDEN extern int	ivbtransrename (char *pcoldname, char *pcnewname);
VB_HIDDEN extern int	ivbtranssetunique (const int ihandle, const off_t tuniqueid);
VB_HIDDEN extern int	ivbtransuniqueid (const int ihandle, const off_t tuniqueid);
VB_HIDDEN extern int	ivbtransupdate (const int ihandle, const off_t trownumber,
				const int ioldrowlen, const int inewrowlen,
				const char *pcrow);

/* iswrite.c */
VB_HIDDEN extern int	ivbwriterow (const int ihandle, char *pcrow,
				const off_t trownumber);

/* vbdataio.c */
VB_HIDDEN extern int	ivbdataread (const int ihandle, char *pcbuffer,
				int *pideletedrow, const off_t trownumber);
VB_HIDDEN extern int	ivbdatawrite (const int ihandle, char *pcbuffer,
				int ideletedrow, const off_t trownumber);

/* vbindexio.c */
VB_HIDDEN extern off_t	tvbnodecountgetnext (const int ihandle);
VB_HIDDEN extern int	ivbnodefree (const int ihandle, const off_t tnodenumber);
VB_HIDDEN extern int	ivbdatafree (const int ihandle, const off_t trownumber);
VB_HIDDEN extern off_t	tvbnodeallocate (const int ihandle);
VB_HIDDEN extern off_t	tvbdataallocate (const int ihandle);
VB_HIDDEN extern int	ivbforcedataallocate (const int ihandle, const off_t trownumber);

/* vbkeysio.c */
VB_HIDDEN extern void	vvbmakekey (const struct keydesc *pskeydesc,
			    char *pcrow_buffer, unsigned char *pckeyvalue);
VB_HIDDEN extern int	ivbkeysearch (const int ihandle, const int imode,
			      const int ikeynumber, int ilength,
			      unsigned char *pckeyvalue, off_t tdupnumber);
VB_HIDDEN extern int	ivbkeylocaterow (const int ihandle, const int ikeynumber, off_t trownumber);
VB_HIDDEN extern int	ivbkeyload (const int ihandle, const int ikeynumber, const int imode,
			    const int isetcurr, struct VBKEY **ppskey);
VB_HIDDEN extern void	vvbkeyvalueset (const int ihigh, struct keydesc *pskeydesc,
				unsigned char *pckeyvalue);
VB_HIDDEN extern int	ivbkeyinsert (const int ihandle, struct VBTREE *pstree,
			      const int ikeynumber, unsigned char *pckeyvalue,
			      off_t trownode, off_t tdupnumber,
			      struct VBTREE *pschild);
VB_HIDDEN extern int	ivbkeydelete (const int ihandle, const int ikeynumber);
VB_HIDDEN extern int	ivbkeycompare (const int ihandle, const int ikeynumber, int ilength,
			       unsigned char *pckey1, unsigned char *pckey2);

/* vblocking.c */
VB_HIDDEN extern int	ivbenter (const int ihandle, const unsigned int imodifying,
				const unsigned int ispecial);
VB_HIDDEN extern int	ivbexit (const int ihandle);
VB_HIDDEN extern int	ivbfileopenlock (const int ihandle, const int imode);
VB_HIDDEN extern int	ivbdatalock (const int ihandle, const int imode,
				const off_t trownumber);

/* vblowlovel.c */
VB_HIDDEN extern int	ivbopen (const char *pcfilename, const int iflags,
				const mode_t tmode);
VB_HIDDEN extern int	ivbclose (const int ihandle);
VB_HIDDEN extern off_t	tvblseek (const int ihandle, off_t toffset, const int iwhence);

VB_HIDDEN extern ssize_t	tvbread (const int ihandle, void *pvbuffer, const size_t tcount);
VB_HIDDEN extern ssize_t	tvbwrite (const int ihandle, const void *pvbuffer, const size_t tcount);

VB_HIDDEN extern int	ivbblockread (const int ihandle, const int iisindex,
				const off_t tblocknumber, char *cbuffer);
VB_HIDDEN extern int	ivbblockwrite (const int ihandle, const int iisindex,
				const off_t tblocknumber, const char *cbuffer);
VB_HIDDEN extern int	ivblock (const int ihandle, const off_t toffset,
				const off_t tlength, const int imode);

/* vbmemio.c */
VB_HIDDEN extern struct	VBLOCK	*psvblockallocate (const int ihandle);
VB_HIDDEN extern void		vvblockfree (struct VBLOCK *pslock);
VB_HIDDEN extern struct	VBTREE	*psvbtreeallocate (const int ihandle);
VB_HIDDEN extern void		vvbtreeallfree (const int ihandle, const int ikeynumber,
					struct VBTREE *pstree);
VB_HIDDEN extern struct	VBKEY	*psvbkeyallocate (const int ihandle, const int ikeynumber);
VB_HIDDEN extern void		vvbkeyallfree (const int ihandle, const int ikeynumber,
				       struct VBTREE *pstree);
VB_HIDDEN extern void		vvbkeyfree (const int ihandle, const int ikeynumber,
				    struct VBKEY *pskey);
VB_HIDDEN extern void		vvbkeyunmalloc (const int ihandle, const int ikeynumber);
VB_HIDDEN extern void		*pvvbmalloc (const size_t size);
VB_HIDDEN extern void		vvbfree (void *mptr);
VB_HIDDEN extern void		vvbunmalloc (void);

/* vbnodememio.c */
VB_HIDDEN extern int	ivbnodeload (const int ihandle, const int ikeynumber,
				struct VBTREE *pstree, const off_t tnodenumber,
				const int iprevlvl);
VB_HIDDEN extern int	ivbnodesave (const int ihandle, const int ikeynumber,
				struct VBTREE *pstree, const off_t tnodenumber,
				const int imode, const int iposn);

#endif	/* VB_LIBVBISAM_H */
