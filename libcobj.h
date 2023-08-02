/*
 * Copyright (C) 2002-2009 Keisuke Nishida
 * Copyright (C) 2007-2009 Roger While
 * Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef COB_LIBCOB_H
#define COB_LIBCOB_H

//#include <gmp.h>

#ifdef __cplusplus
extern "C" {
#endif

/* File version */
#define	COB_FILE_VERSION	0

/* Start conditions */
#define COB_EQ			1 	/* x == y */
#define COB_LT			2 	/* x <  y */
#define COB_LE			3 	/* x <= y */
#define COB_GT			4 	/* x >  y */
#define COB_GE			5 	/* x >= y */
#define COB_NE			6 	/* x != y */

#define COB_ASCENDING		0
#define COB_DESCENDING		1

#define COB_FILE_MODE		0644

/* Organization */

#define COB_ORG_SEQUENTIAL	0
#define COB_ORG_LINE_SEQUENTIAL	1
#define COB_ORG_RELATIVE	2
#define COB_ORG_INDEXED		3
#define COB_ORG_SORT		4
#define COB_ORG_MAX		5

/* Access mode */

#define COB_ACCESS_SEQUENTIAL	1
#define COB_ACCESS_DYNAMIC	2
#define COB_ACCESS_RANDOM	3

/* SELECT features */

#define	COB_SELECT_FILE_STATUS	0x01
#define	COB_SELECT_EXTERNAL	0x02
#define	COB_SELECT_LINAGE	0x04
#define	COB_SELECT_SPLITKEY	0x08

/* Lock mode */

#define COB_LOCK_EXCLUSIVE	1
#define COB_LOCK_MANUAL		2
#define COB_LOCK_AUTOMATIC	4
#define COB_LOCK_MULTIPLE	8
#define COB_LOCK_MASK		0x7

/* Operate type */

#define COB_IO_OPEN		0
#define COB_IO_READ		1
#define COB_IO_WRITE		2
#define COB_IO_CLOSE		3
#define COB_IO_DELETE		4
#define COB_IO_REWRITE		5
#define COB_IO_START		6
#define COB_IO_COMMIT		7
#define COB_IO_ROLLBACK		8
#define COB_IO_UNLOCK		9
#define COB_IO_DELETE_FILE	10

/* Open mode */

#define COB_OPEN_CLOSED		0
#define COB_OPEN_INPUT 		1
#define COB_OPEN_OUTPUT		2
#define COB_OPEN_I_O 		3
#define COB_OPEN_EXTEND		4
#define COB_OPEN_LOCKED		5

/* Close options */

#define COB_CLOSE_NORMAL	0
#define COB_CLOSE_LOCK		1
#define COB_CLOSE_NO_REWIND	2
#define COB_CLOSE_UNIT		3
#define COB_CLOSE_UNIT_REMOVAL	4

/* Write options */

#define COB_WRITE_MASK		0x0000ffff

#define COB_WRITE_LINES		0x00010000
#define COB_WRITE_PAGE		0x00020000
#define COB_WRITE_CHANNEL	0x00040000
#define COB_WRITE_AFTER		0x00100000
#define COB_WRITE_BEFORE	0x00200000
#define COB_WRITE_EOP		0x00400000
#define COB_WRITE_LOCK		0x00800000

/* Read options */

#define COB_READ_NEXT		0x01
#define COB_READ_PREVIOUS	0x02
#define COB_READ_FIRST		0x04
#define COB_READ_LAST		0x08
#define COB_READ_LOCK		0x10
#define COB_READ_NO_LOCK	0x20
#define COB_READ_KEPT_LOCK	0x40
#define COB_READ_WAIT_LOCK	0x80
#define COB_READ_IGNORE_LOCK	0x100

/* I-O status */

#define COB_STATUS_00_SUCCESS			00
#define COB_STATUS_02_SUCCESS_DUPLICATE		02
#define COB_STATUS_04_SUCCESS_INCOMPLETE	04
#define COB_STATUS_05_SUCCESS_OPTIONAL		05
#define COB_STATUS_07_SUCCESS_NO_UNIT		07
#define COB_STATUS_10_END_OF_FILE		10
#define COB_STATUS_14_OUT_OF_KEY_RANGE		14
#define COB_STATUS_21_KEY_INVALID		21
#define COB_STATUS_22_KEY_EXISTS		22
#define COB_STATUS_23_KEY_NOT_EXISTS		23
#define COB_STATUS_30_PERMANENT_ERROR		30
#define COB_STATUS_31_INCONSISTENT_FILENAME	31
#define COB_STATUS_34_BOUNDARY_VIOLATION	34
#define COB_STATUS_35_NOT_EXISTS		35
#define COB_STATUS_37_PERMISSION_DENIED		37
#define COB_STATUS_38_CLOSED_WITH_LOCK		38
#define COB_STATUS_39_CONFLICT_ATTRIBUTE	39
#define COB_STATUS_41_ALREADY_OPEN		41
#define COB_STATUS_42_NOT_OPEN			42
#define COB_STATUS_43_READ_NOT_DONE		43
#define COB_STATUS_44_RECORD_OVERFLOW		44
#define COB_STATUS_46_READ_ERROR		46
#define COB_STATUS_47_INPUT_DENIED		47
#define COB_STATUS_48_OUTPUT_DENIED		48
#define COB_STATUS_49_I_O_DENIED		49
#define COB_STATUS_51_RECORD_LOCKED		51
#define COB_STATUS_52_EOP			52
#define COB_STATUS_57_I_O_LINAGE		57
#define COB_STATUS_61_FILE_SHARING		61
#define COB_STATUS_91_NOT_AVAILABLE		91

/* Number store defines */
#define COB_STORE_ROUND			0x01
#define COB_STORE_KEEP_ON_OVERFLOW	0x02
#define COB_STORE_TRUNC_ON_OVERFLOW	0x04

/* Screen */
#define COB_SCREEN_BLACK	0
#define COB_SCREEN_BLUE		1
#define COB_SCREEN_GREEN	2
#define COB_SCREEN_CYAN		3
#define COB_SCREEN_RED		4
#define COB_SCREEN_MAGENTA	5
#define COB_SCREEN_YELLOW	6
#define COB_SCREEN_WHITE	7

#define COB_SCREEN_LINE_PLUS	0x00000001
#define COB_SCREEN_LINE_MINUS	0x00000002
#define COB_SCREEN_COLUMN_PLUS	0x00000004
#define COB_SCREEN_COLUMN_MINUS	0x00000008

#define COB_SCREEN_AUTO		0x00000010
#define COB_SCREEN_BELL		0x00000020
#define COB_SCREEN_BLANK_LINE	0x00000040
#define COB_SCREEN_BLANK_SCREEN	0x00000080
#define COB_SCREEN_BLINK	0x00000100
#define COB_SCREEN_ERASE_EOL	0x00000200
#define COB_SCREEN_ERASE_EOS	0x00000400
#define COB_SCREEN_FULL		0x00000800
#define COB_SCREEN_HIGHLIGHT	0x00001000
#define COB_SCREEN_LOWLIGHT	0x00002000
#define COB_SCREEN_REQUIRED	0x00004000
#define COB_SCREEN_REVERSE	0x00008000
#define COB_SCREEN_SECURE	0x00010000
#define COB_SCREEN_UNDERLINE	0x00020000
#define COB_SCREEN_OVERLINE	0x00040000
#define COB_SCREEN_PROMPT	0x00080000
#define COB_SCREEN_UPDATE	0x00100000
#define COB_SCREEN_INPUT	0x00200000
#define COB_SCREEN_SCROLL_DOWN	0x00400000
#define COB_SCREEN_RIGHTLINE	0x00800000
#define COB_SCREEN_LEFTLINE	0x01000000

#define COB_SCREEN_TYPE_GROUP		0
#define COB_SCREEN_TYPE_FIELD		1
#define COB_SCREEN_TYPE_VALUE		2
#define COB_SCREEN_TYPE_ATTRIBUTE	3

/* Common */
#include <time.h>

#ifdef _MSC_VER

#ifndef	_CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE	1
#endif

#define inline _inline
#include <malloc.h>
#include <io.h>
#include <fcntl.h>
#pragma warning(disable: 4996)
#define strncasecmp _strnicmp
#define strcasecmp _stricmp
#define snprintf		_snprintf
#define getpid			_getpid

#define __attribute__(x)

#ifdef	S_ISDIR
#undef	S_ISDIR
#endif
#define S_ISDIR(x)		(((x) & _S_IFMT) == _S_IFDIR)


#ifndef	_M_IA64
#ifdef	_WIN64
#define	__x86_64__
#else
#define	__i386__
#endif
#endif

#endif /* _MSC_VER */

#ifdef	__370__

#define inline __inline
#ifndef __timespec_struct
#define __timespec_struct 1
	struct timespec {
		time_t tv_sec;
		long   tv_nsec;
	};
#endif
#endif

#if	defined(_WIN32) || defined(__CYGWIN__)
#ifdef	COB_LIB_EXPIMP
	#define COB_EXPIMP 	__declspec(dllexport) extern
#else
	#define COB_EXPIMP 	__declspec(dllimport) extern
#endif
#else
	#define COB_EXPIMP 	extern
#endif

#if	defined(__370__) || defined(_MSC_VER)
	#define COB_INLINE	__inline
#elif	defined(__INTEL_COMPILER)
	/* icc */
	#define COB_INLINE	inline
#elif	defined(__GNUC__)
	/* gcc */
	#define COB_INLINE	__inline__
#elif defined(COB_HAS_INLINE)
	#define COB_INLINE inline
#else
	#define COB_INLINE
#endif

/* Also OK for icc which defines __GNUC__ */

#if	defined(__GNUC__)
#define	COB_A_NORETURN	__attribute__((noreturn))
#define	COB_A_FORMAT10	__attribute__((format(printf, 1, 0)))
#define	COB_A_FORMAT12	__attribute__((format(printf, 1, 2)))
#define	COB_A_FORMAT23	__attribute__((format(printf, 2, 3)))
#define	COB_A_FORMAT34	__attribute__((format(printf, 3, 4)))
#else
#define	COB_A_NORETURN
#define	COB_A_FORMAT10
#define	COB_A_FORMAT12
#define	COB_A_FORMAT23
#define	COB_A_FORMAT34
#endif

#ifdef	_MSC_VER
#define	DECLNORET	__declspec(noreturn)
#else
#define	DECLNORET
#endif

#if defined(__GNUC__) && (__GNUC__ >= 3)
#define likely(x)	__builtin_expect(!!(x), 1)
#define unlikely(x)	__builtin_expect(!!(x), 0)
#define	COB_A_MALLOC	__attribute__((malloc))
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)
#define	COB_NOINLINE	__attribute__((noinline))
#else
#define	COB_NOINLINE
#endif
#else
#define likely(x)	(x)
#define unlikely(x)	(x)
#define	COB_A_MALLOC
#define	COB_NOINLINE
#endif

/* Prevent unwanted verbosity when using icc */
#ifdef	__INTEL_COMPILER

/* Unreachable code */
#pragma warning ( disable : 111 )
/* Declared but never referenced */
#pragma warning ( disable : 177 )
/* Format conversion */
#pragma warning ( disable : 181 )
/* Enumerated type mixed with other type */
#pragma warning ( disable : 188 )
/* #undefine tested for zero */
#pragma warning ( disable : 193 )
/* Set but not used */
#pragma warning ( disable : 593 )
/* Parameter not referenced */
#pragma warning ( disable : 869 )
/* Operands are evaluated in unspecified order */
#pragma warning ( disable : 981 )
/* Missing return at end of non-void function */
/* Note - occurs because we have a non-returning abort call in cobc */
#pragma warning ( disable : 1011 )
/* Declaration in same source as definition */
#pragma warning ( disable : 1419 )
/* Shadowed variable - 1599 and 1944 are essentially the same */
#pragma warning ( disable : 1599 )
#pragma warning ( disable : 1944 )
/* Possible loss of precision */
#pragma warning ( disable : 2259 )

#endif

#if	' ' == 0x40
#define	COB_EBCDIC_MACHINE
#else
#undef	COB_EBCDIC_MACHINE
#endif

/* Macro to prevent unused parameter warning */

#define	COB_UNUSED(z)	do { (void)(z); } while (0)

/* Buffer size definitions */

#define	COB_MINI_BUFF		256
#define	COB_SMALL_BUFF		1024
#define	COB_NORMAL_BUFF		2048
#define	COB_MEDIUM_BUFF		8192
#define	COB_LARGE_BUFF		16384
#define	COB_MINI_MAX		(COB_MINI_BUFF - 1)
#define	COB_SMALL_MAX		(COB_SMALL_BUFF - 1)
#define	COB_NORMAL_MAX		(COB_NORMAL_BUFF - 1)
#define	COB_MEDIUM_MAX		(COB_MEDIUM_BUFF - 1)
#define	COB_LARGE_MAX		(COB_LARGE_BUFF - 1)

/* Perform stack size */
#define	COB_STACK_SIZE		255

/* Maximum number of parameters */
#define	COB_MAX_FIELD_PARAMS	64

/* Field */

/* field types */

#define COB_TYPE_UNKNOWN		0x00
#define COB_TYPE_GROUP			0x01
#define COB_TYPE_BOOLEAN		0x02

#define COB_TYPE_NUMERIC		0x10
#define COB_TYPE_NUMERIC_DISPLAY	0x10
#define COB_TYPE_NUMERIC_BINARY		0x11
#define COB_TYPE_NUMERIC_PACKED		0x12
#define COB_TYPE_NUMERIC_FLOAT		0x13
#define COB_TYPE_NUMERIC_DOUBLE		0x14
#define COB_TYPE_NUMERIC_EDITED		0x24

#define COB_TYPE_ALPHANUMERIC		0x21
#define COB_TYPE_ALPHANUMERIC_ALL	0x22
#define COB_TYPE_ALPHANUMERIC_EDITED	0x23

#define COB_TYPE_NATIONAL		0x40
#define COB_TYPE_NATIONAL_EDITED	0x41
#define COB_TYPE_NATIONAL_ALL		0x42

/* field flags */

#define COB_FLAG_HAVE_SIGN		0x01
#define COB_FLAG_SIGN_SEPARATE		0x02
#define COB_FLAG_SIGN_LEADING		0x04
#define COB_FLAG_BLANK_ZERO		0x08
#define COB_FLAG_JUSTIFIED		0x10
#define COB_FLAG_BINARY_SWAP		0x20
#define COB_FLAG_REAL_BINARY		0x40
#define COB_FLAG_IS_POINTER		0x80

#define COB_FIELD_HAVE_SIGN(f)		((f)->attr->flags & COB_FLAG_HAVE_SIGN)
#define COB_FIELD_SIGN_SEPARATE(f)	((f)->attr->flags & COB_FLAG_SIGN_SEPARATE)
#define COB_FIELD_SIGN_LEADING(f)	((f)->attr->flags & COB_FLAG_SIGN_LEADING)
#define COB_FIELD_BLANK_ZERO(f)		((f)->attr->flags & COB_FLAG_BLANK_ZERO)
#define COB_FIELD_JUSTIFIED(f)		((f)->attr->flags & COB_FLAG_JUSTIFIED)
#define COB_FIELD_BINARY_SWAP(f)	((f)->attr->flags & COB_FLAG_BINARY_SWAP)
#define COB_FIELD_REAL_BINARY(f)	((f)->attr->flags & COB_FLAG_REAL_BINARY)
#define COB_FIELD_IS_POINTER(f)		((f)->attr->flags & COB_FLAG_IS_POINTER)

#define cob_get_sign(f)	(COB_FIELD_HAVE_SIGN (f) ? cob_real_get_sign (f) : 0)
#define cob_put_sign(f,s) if (COB_FIELD_HAVE_SIGN (f)) cob_real_put_sign (f, s)

#define COB_FIELD_TYPE(f)	((f)->attr->type)
#define COB_FIELD_DIGITS(f)	((f)->attr->digits)
#define COB_FIELD_SCALE(f)	((f)->attr->scale)
#define COB_FIELD_FLAGS(f)	((f)->attr->flags)
#define COB_FIELD_PIC(f)	((f)->attr->pic)

#define COB_FIELD_DATA(f)						  \
  ((f)->data +								  \
   ((COB_FIELD_SIGN_SEPARATE (f) && COB_FIELD_SIGN_LEADING (f)) ? 1 : 0))
#define COB_FIELD_SIZE(f)						\
  ((f)->size - (COB_FIELD_SIGN_SEPARATE (f) ? 1 : 0))

#define COB_FIELD_IS_NUMERIC(f)	(COB_FIELD_TYPE (f) & COB_TYPE_NUMERIC)
#define COB_FIELD_IS_NATIONAL(f)	(COB_FIELD_TYPE (f) & COB_TYPE_NATIONAL)

/* SIGN */

/*
 * positive: 0123456789
 * negative: pqrstuvwxy
 */
#define GET_SIGN_ASCII(x) x -= 0x40
#define PUT_SIGN_ASCII(x) x += 0x40

#define	COB_DISPLAY_SIGN_ASCII	0
#define	COB_DISPLAY_SIGN_EBCDIC	1

/* Fatal error definitions */

#define COB_FERROR_INITIALIZED	0
#define COB_FERROR_CODEGEN	1
#define COB_FERROR_CHAINING	2
#define COB_FERROR_STACK	3

/* Exception identifier enumeration */

#undef	COB_EXCEPTION
#define	COB_EXCEPTION(code,tag,name,critical)	tag,

enum cob_exception_id {
	COB_EC_ZERO,
#include <exception.def>
	COB_EC_MAX
};

#undef	COB_EXCEPTION

/* convert a digit (e.g., '0') into an integer (e.g., 0) */
#define cob_d2i(x)		((x) - '0')

/* convert an integer (e.g., 0) into a digit (e.g., '0') */
#define cob_i2d(x)		((x) + '0')


/* Structure/union declarations */

typedef unsigned char *	ucharptr;

//#include <libcob/byteswap.h>
//#include <libcob/common.h>
//#include <libcob/call.h>
//#include <libcob/fileio.h>
//#include <libcob/move.h>
//#include <libcob/numeric.h>
//#include <libcob/screenio.h>
//#include <libcob/strings.h>
//#include <libcob/termio.h>
//#include <libcob/intrinsic.h>
//#include <libcob/codegen.h>

#ifdef __cplusplus
}
#endif

#endif
