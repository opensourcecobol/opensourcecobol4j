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

#ifndef COB_COMMON_H
#define COB_COMMON_H

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
#include <libcob/exception.def>
	COB_EC_MAX
};

#undef	COB_EXCEPTION

/* Global variables */

COB_EXPIMP int			cob_initialized;
COB_EXPIMP int			cob_exception_code;
COB_EXPIMP int			cob_error_on_exit_flag;

COB_EXPIMP struct cob_module	*cob_current_module;

COB_EXPIMP int			cob_call_params;
COB_EXPIMP int			cob_save_call_params;
COB_EXPIMP int			cob_initial_external;

/* convert a digit (e.g., '0') into an integer (e.g., 0) */
#define cob_d2i(x)		((x) - '0')

/* convert an integer (e.g., 0) into a digit (e.g., '0') */
#define cob_i2d(x)		((x) + '0')


/* Structure/union declarations */

typedef unsigned char *	ucharptr;

/* External */

struct cob_external {
	struct cob_external	*next;
	char			*ext_alloc;
	char			*ename;
	int			esize;
};

/* Field attribute structure */

typedef struct {
	unsigned char	type;		/* Field type */
	unsigned char	digits;		/* Digit count */
	signed char	scale;		/* Field scale */
	unsigned char	flags;		/* Field flags */
	const char	*pic;		/* Pointer to picture string */
} cob_field_attr;

/* Field structure */

typedef struct {
	size_t			size;		/* Field size */
	unsigned char		*data;		/* Pointer to field data */
	const cob_field_attr	*attr;		/* Pointer to attribute */
} cob_field;

/* Module structure */

struct cob_module {
	struct cob_module		*next;
	const unsigned char		*collating_sequence;
	cob_field			*crt_status;
	cob_field			*cursor_pos;
	cob_field			**cob_procedure_parameters;
	const unsigned char		display_sign;
	const unsigned char		decimal_point;
	unsigned char			currency_symbol;
	const unsigned char		numeric_separator;
	const unsigned char		flag_filename_mapping;
	const unsigned char		flag_binary_truncate;
	const unsigned char		flag_pretty_display;
	const unsigned char		spare8;
	char				*program_id;
};

/*******************************/

COB_EXPIMP cob_field		cob_zero;		/* ZERO */
COB_EXPIMP cob_field		cob_space;		/* SPACE */
COB_EXPIMP cob_field		cob_blank;		/* BLANK */
COB_EXPIMP cob_field		cob_high;		/* HIGH+VALUE */
COB_EXPIMP cob_field		cob_low;		/* LOW+VALUE */
COB_EXPIMP cob_field		cob_quote;		/* QUOTE */
COB_EXPIMP cob_field		cob_one;		/* Numeric ONE */

COB_EXPIMP cob_field		cob_zen_zero;		/* zenkaku ZERO */
COB_EXPIMP cob_field		cob_zen_space;		/* zenkaku SPACE */
COB_EXPIMP cob_field		cob_zen_blank;		/* zenkaku BLANK */
COB_EXPIMP cob_field		cob_zen_quote;		/* zenkaku QUOTE */

/* General functions */

COB_EXPIMP void cob_init		(const int, char **);
COB_EXPIMP void cob_module_enter	(struct cob_module *);
COB_EXPIMP void cob_module_leave	(struct cob_module *);

COB_EXPIMP void cobexit		(const int) COB_A_NORETURN;
DECLNORET COB_EXPIMP void cob_stop_run	(const int) COB_A_NORETURN;
DECLNORET COB_EXPIMP void cob_fatal_error	(const unsigned int) COB_A_NORETURN;
COB_EXPIMP void cob_runtime_error	(const char *, ...)  COB_A_FORMAT10;

COB_EXPIMP void *cob_malloc		(const size_t)  COB_A_MALLOC;

COB_EXPIMP struct tm *cob_localtime	(const time_t *);

COB_EXPIMP void cob_verbose_output	(const char *, ...);

COB_EXPIMP int  cob_io_rewrite_assumed	(void);

COB_EXPIMP void cob_check_version		(const char *, const char *,
						 const int);

COB_EXPIMP const char *cob_get_exception_name	(const int);

COB_EXPIMP void cob_accept_arg_number	(cob_field *);
COB_EXPIMP void cob_accept_arg_value	(cob_field *);
COB_EXPIMP void cob_accept_command_line	(cob_field *);

COB_EXPIMP void cob_set_exception		(const int);
COB_EXPIMP void cob_accept_date		(cob_field *);
COB_EXPIMP void cob_accept_date_yyyymmdd	(cob_field *);
COB_EXPIMP void cob_accept_day		(cob_field *);
COB_EXPIMP void cob_accept_day_yyyyddd	(cob_field *);
COB_EXPIMP void cob_accept_day_of_week	(cob_field *);
COB_EXPIMP void cob_accept_environment	(cob_field *);
COB_EXPIMP void cob_accept_time		(cob_field *);
COB_EXPIMP void cob_display_command_line	(cob_field *);
COB_EXPIMP void cob_display_environment	(const cob_field *);
COB_EXPIMP void cob_display_env_value	(const cob_field *);
COB_EXPIMP void cob_display_arg_number	(cob_field *);
COB_EXPIMP void cob_get_environment		(const cob_field *, cob_field *);
COB_EXPIMP void	cob_set_environment		(const cob_field *,
						 const cob_field *);
COB_EXPIMP void cob_chain_setup		(void *, const size_t,
						 const size_t);
COB_EXPIMP void cob_allocate		(unsigned char **, cob_field *,
					 cob_field *);
COB_EXPIMP void cob_free_alloc		(unsigned char **, unsigned char *);
COB_EXPIMP int  cobinit			(void);
COB_EXPIMP int  cobtidy			(void);
COB_EXPIMP void *cobcommandline		(int, int *, char ***,
					 char ***, char **);
COB_EXPIMP char *cobgetenv			(const char *);
COB_EXPIMP int  cobputenv			(char *);

/* System routines */
COB_EXPIMP int CBL_ERROR_PROC	(unsigned char *, unsigned char *);
COB_EXPIMP int CBL_EXIT_PROC	(unsigned char *, unsigned char *);
COB_EXPIMP int SYSTEM		(const unsigned char *);
COB_EXPIMP int CBL_AND		(unsigned char *, unsigned char *, const int);
COB_EXPIMP int CBL_OR		(unsigned char *, unsigned char *, const int);
COB_EXPIMP int CBL_NOR		(unsigned char *, unsigned char *, const int);
COB_EXPIMP int CBL_XOR		(unsigned char *, unsigned char *, const int);
COB_EXPIMP int CBL_IMP		(unsigned char *, unsigned char *, const int);
COB_EXPIMP int CBL_NIMP		(unsigned char *, unsigned char *, const int);
COB_EXPIMP int CBL_EQ		(unsigned char *, unsigned char *, const int);
COB_EXPIMP int CBL_NOT		(unsigned char *, const int);
COB_EXPIMP int CBL_XF4		(unsigned char *, unsigned char *);
COB_EXPIMP int CBL_XF5		(unsigned char *, unsigned char *);
COB_EXPIMP int CBL_X91		(unsigned char *, const unsigned char *,
				 unsigned char *);
COB_EXPIMP int CBL_TOUPPER		(unsigned char *, const int);
COB_EXPIMP int CBL_TOLOWER		(unsigned char *, const int);
COB_EXPIMP int CBL_OC_NANOSLEEP	(unsigned char *);
COB_EXPIMP int cob_acuw_getpid	(void);
COB_EXPIMP int cob_return_args	(unsigned char *);
COB_EXPIMP int cob_parameter_size	(unsigned char *);
COB_EXPIMP int cob_acuw_sleep	(unsigned char *);
COB_EXPIMP int cob_acuw_justify	(unsigned char *, ...);
COB_EXPIMP int cob_acuw_calledby	(unsigned char *);

/* Utilities */

COB_EXPIMP unsigned char	*cob_external_addr	(const char *, const int);
COB_EXPIMP unsigned char	*cob_get_pointer	(const unsigned char *);
COB_EXPIMP void		*cob_get_prog_pointer	(const unsigned char *);
COB_EXPIMP void		cob_set_location	(const char *, const char *,
						 const unsigned int, const char *,
						 const char *, const char *);
COB_EXPIMP void		cob_ready_trace		(void);
COB_EXPIMP void		cob_reset_trace		(void);

COB_EXPIMP void		cob_set_programid	(struct cob_module *,
						 const char *);

/* Switch */

COB_EXPIMP int		cob_get_switch		(const int);
COB_EXPIMP void		cob_set_switch		(const int, const int);

/* Comparison */

COB_EXPIMP int		cob_cmp			(cob_field *, cob_field *);

/* Class check */

COB_EXPIMP int		cob_is_omitted		(const cob_field *);
COB_EXPIMP int		cob_is_numeric		(const cob_field *);
COB_EXPIMP int		cob_is_alpha		(const cob_field *);
COB_EXPIMP int		cob_is_upper		(const cob_field *);
COB_EXPIMP int		cob_is_lower		(const cob_field *);

/* Table sort */

COB_EXPIMP void 	cob_table_sort_init	(const int, const unsigned char *);
COB_EXPIMP void 	cob_table_sort_init_key	(const int, cob_field *,
					 const size_t);
COB_EXPIMP void cob_table_sort			(cob_field *, const int);

/* Run-time error checking */

COB_EXPIMP void cob_check_numeric			(const cob_field *, const char *);
COB_EXPIMP void cob_check_based			(const unsigned char *,
						 const char *);
COB_EXPIMP void cob_check_odo			(const int, const int,
						 const int, const char *);
COB_EXPIMP void cob_check_subscript			(const int, const int,
						 const int, const char *);
COB_EXPIMP void cob_check_ref_mod_national		(int, int, int, const char *);
COB_EXPIMP int  cob_check_env			(const char *, const char *);
COB_EXPIMP void cob_check_ref_mod			(const int, const int,
						 const int, const char *);
COB_EXPIMP void cob_check_mvstrnum			(cob_field *, cob_field *);


/* Comparison functions */
COB_EXPIMP int cob_numeric_cmp			(cob_field *, cob_field *);

#endif /* COB_COMMON_H */
