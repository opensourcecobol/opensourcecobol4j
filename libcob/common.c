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
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef	_WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#include <io.h>
#include <fcntl.h>
#undef	HAVE_SIGNAL_H
#endif

#ifdef	HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"
#include "lib/gettext.h"

#define NAMERMCHECK	"OC_Reference_Check"
#define VALUERMCHECK	"lenient"

struct cob_exception {
	const char	*name;
	const int	code;
	const int	critical;
};

struct cob_alloc_cache {
	struct cob_alloc_cache	*next;
	void			*cob_pointer;
	size_t			size;
};

#define COB_ERRBUF_SIZE	256

/* Local variables */

static int			cob_argc = 0;
static char			**cob_argv = NULL;
static struct cob_alloc_cache	*cob_alloc_base = NULL;

static char			*cob_local_env = NULL;
static int			current_arg = 1;
static unsigned char		*commlnptr = NULL;
static size_t			commlncnt = 0;

static char			*locale_save = NULL;

static size_t			sort_nkeys;
static struct cob_file_key	*sort_keys;
static const unsigned char	*sort_collate;

static const char		*cob_current_program_id = NULL;
static const char		*cob_current_section = NULL;
static const char		*cob_current_paragraph = NULL;
static const char		*cob_source_file = NULL;
static const char		*cob_source_statement = NULL;
static unsigned int		cob_source_line = 0;
static size_t			cob_line_trace = 0;

#ifdef	HAVE_SIGNAL_H
typedef void (*cob_sighandler_t) (int);
static cob_sighandler_t		hupsig = NULL;
static cob_sighandler_t		intsig = NULL;
static cob_sighandler_t		qutsig = NULL;
#endif

#ifdef	COB_PARAM_CHECK
static const char	parm_msg[] = "CALL to %s requires %d parameters";
#endif

#undef	COB_EXCEPTION
#define COB_EXCEPTION(code,tag,name,critical)	name,
static const char		* const cob_exception_tab_name[] = {
	NULL,		/* COB_EC_ZERO */
#include "exception.def"
	NULL		/* COB_EC_MAX */
};

#undef	COB_EXCEPTION
#define COB_EXCEPTION(code,tag,name,critical)	0x##code,
static const int		cob_exception_tab_code[] = {
	0,		/* COB_EC_ZERO */
#include "exception.def"
	0		/* COB_EC_MAX */
};

#undef	COB_EXCEPTION

#define EXCEPTION_TAB_SIZE	sizeof(cob_exception_tab_code) / sizeof(int)

/*
#define EXCEPTION_TAB_SIZE	sizeof(cob_exception_table) / sizeof(struct cob_exception)
*/

static int		cob_switch[8];

static struct tm	*cob_localtm = NULL;

static int		cob_verbose = 0;

static int		cob_io_assume_rewrite = 0;

static int		cob_nibble_c_for_unsigned = 0;

/* Runtime exit handling */
static struct exit_handlerlist {
	struct exit_handlerlist	*next;
	int			(*proc)(void);
} *exit_hdlrs = NULL;

/* Runtime error handling */
static struct handlerlist {
	struct handlerlist	*next;
	int			(*proc)(char *s);
} *hdlrs = NULL;

static char			*runtime_err_str;

static cob_field_attr	all_attr = { COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, NULL };
static cob_field_attr	one_attr = { COB_TYPE_NUMERIC, 1, 0, 0, NULL };

/* Global variables */

struct cob_module	*cob_current_module = NULL;

int			cob_initialized = 0;
int			cob_exception_code = 0;
int			cob_error_on_exit_flag = 0;

int			cob_call_params = 0;
int			cob_save_call_params = 0;
int			cob_initial_external = 0;
int			cob_got_exception = 0;

const char		*cob_orig_statement = NULL;
const char		*cob_orig_program_id = NULL;
const char		*cob_orig_section = NULL;
const char		*cob_orig_paragraph = NULL;
unsigned int		cob_orig_line = 0;

cob_field		cob_zero = { 1, (ucharptr)"0", &all_attr };
cob_field		cob_space = { 1, (ucharptr)" ", &all_attr };
cob_field		cob_blank = { 1, (ucharptr)" ", &all_attr };
cob_field		cob_high = { 1, (ucharptr)"\xff", &all_attr };
cob_field		cob_low = { 1, (ucharptr)"\0", &all_attr };
cob_field		cob_quote = { 1, (ucharptr)"\"", &all_attr };
cob_field		cob_one = { 1, (ucharptr)"1", &one_attr };

cob_field		cob_zen_zero  = { COB_ZENCSIZ, (ucharptr)COB_ZENZERO, &all_attr };
cob_field		cob_zen_space = { COB_ZENCSIZ, (ucharptr)COB_ZENSPC, &all_attr };
cob_field		cob_zen_blank = { COB_ZENCSIZ, (ucharptr)COB_ZENBLK, &all_attr };
cob_field		cob_zen_quote = { COB_ZENCSIZ, (ucharptr)COB_ZENQUOT, &all_attr };

/* Local functions */

#ifdef	HAVE_SIGNAL_H
static void COB_NOINLINE
cob_sig_handler (int sig)
{
#ifdef	SIGSEGV
	if (sig == SIGSEGV) {
		if (cob_source_file) {
			fprintf (stderr, "%s:%d: ", cob_source_file, cob_source_line);
		}
		fprintf (stderr, "Attempt to reference unallocated memory (Signal SIGSEGV)\n");
		fprintf (stderr, "Abnormal termination - File contents may be incorrect\n");
		fflush (stderr);
		exit (SIGSEGV);
	}
#endif
	if (cob_initialized) {
		cob_screen_terminate ();
		cob_exit_fileio ();
		fprintf (stderr, "Abnormal termination - File contents may not be correct\n");
		fflush (stderr);
	}
	switch (sig) {
#ifdef	SIGHUP
	case SIGHUP:
		if ((hupsig != SIG_IGN) && (hupsig != SIG_DFL)) {
			(*hupsig) (SIGHUP);
		}
		break;
#endif
#ifdef	SIGINT
	case SIGINT:
		if ((intsig != SIG_IGN) && (intsig != SIG_DFL)) {
			(*intsig) (SIGINT);
		}
		break;
#endif
#ifdef	SIGQUIT
	case SIGQUIT:
		if ((qutsig != SIG_IGN) && (qutsig != SIG_DFL)) {
			(*qutsig) (SIGQUIT);
		}
		break;
#endif
	}
	exit (sig);
}
#endif

static void
cob_set_signal (void)
{
#ifdef	HAVE_SIGNAL_H
#ifdef	SIGINT
	if ((intsig = signal (SIGINT, cob_sig_handler)) == SIG_IGN) {
		(void)signal (SIGINT, SIG_IGN);
	}
#endif
#ifdef	SIGHUP
	if ((hupsig = signal (SIGHUP, cob_sig_handler)) == SIG_IGN) {
		(void)signal (SIGHUP, SIG_IGN);
	}
#endif
#ifdef	SIGQUIT
	if ((qutsig = signal (SIGQUIT, cob_sig_handler)) == SIG_IGN) {
		(void)signal (SIGQUIT, SIG_IGN);
	}
#endif
	/* Take direct control of segementation violation */
#ifdef	SIGSEGV
	(void)signal (SIGSEGV, cob_sig_handler);
#endif
#endif
}

#ifdef	COB_EBCDIC_MACHINE
static void
cob_get_sign_ascii (unsigned char *p)
{
	switch (*p) {
	case 'p':
		*p = (unsigned char)'0';
		return;
	case 'q':
		*p = (unsigned char)'1';
		return;
	case 'r':
		*p = (unsigned char)'2';
		return;
	case 's':
		*p = (unsigned char)'3';
		return;
	case 't':
		*p = (unsigned char)'4';
		return;
	case 'u':
		*p = (unsigned char)'5';
		return;
	case 'v':
		*p = (unsigned char)'6';
		return;
	case 'w':
		*p = (unsigned char)'7';
		return;
	case 'x':
		*p = (unsigned char)'8';
		return;
	case 'y':
		*p = (unsigned char)'9';
		return;
	}
}
#endif

static int COB_NOINLINE
cob_get_sign_ebcdic (unsigned char *p)
{
	switch (*p) {
	case '{':
		*p = (unsigned char)'0';
		return 1;
	case 'A':
		*p = (unsigned char)'1';
		return 1;
	case 'B':
		*p = (unsigned char)'2';
		return 1;
	case 'C':
		*p = (unsigned char)'3';
		return 1;
	case 'D':
		*p = (unsigned char)'4';
		return 1;
	case 'E':
		*p = (unsigned char)'5';
		return 1;
	case 'F':
		*p = (unsigned char)'6';
		return 1;
	case 'G':
		*p = (unsigned char)'7';
		return 1;
	case 'H':
		*p = (unsigned char)'8';
		return 1;
	case 'I':
		*p = (unsigned char)'9';
		return 1;
	case '}':
		*p = (unsigned char)'0';
		return -1;
	case 'J':
		*p = (unsigned char)'1';
		return -1;
	case 'K':
		*p = (unsigned char)'2';
		return -1;
	case 'L':
		*p = (unsigned char)'3';
		return -1;
	case 'M':
		*p = (unsigned char)'4';
		return -1;
	case 'N':
		*p = (unsigned char)'5';
		return -1;
	case 'O':
		*p = (unsigned char)'6';
		return -1;
	case 'P':
		*p = (unsigned char)'7';
		return -1;
	case 'Q':
		*p = (unsigned char)'8';
		return -1;
	case 'R':
		*p = (unsigned char)'9';
		return -1;
	default:
		/* What to do here */
		*p = (unsigned char)'0';
		return 1;
	}
/* NOT REACHED */
	return 1;
}

#ifdef	COB_EBCDIC_MACHINE
static void COB_NOINLINE
cob_put_sign_ascii (unsigned char *p)
{
	switch (*p) {
	case '0':
		*p = (unsigned char)'p';
		return;
	case '1':
		*p = (unsigned char)'q';
		return;
	case '2':
		*p = (unsigned char)'r';
		return;
	case '3':
		*p = (unsigned char)'s';
		return;
	case '4':
		*p = (unsigned char)'t';
		return;
	case '5':
		*p = (unsigned char)'u';
		return;
	case '6':
		*p = (unsigned char)'v';
		return;
	case '7':
		*p = (unsigned char)'w';
		return;
	case '8':
		*p = (unsigned char)'x';
		return;
	case '9':
		*p = (unsigned char)'y';
		return;
	}
}
#endif

static void COB_NOINLINE
cob_put_sign_ebcdic (unsigned char *p, const int sign)
{
	if (sign < 0) {
		switch (*p) {
		case '0':
			*p = (unsigned char)'}';
			return;
		case '1':
			*p = (unsigned char)'J';
			return;
		case '2':
			*p = (unsigned char)'K';
			return;
		case '3':
			*p = (unsigned char)'L';
			return;
		case '4':
			*p = (unsigned char)'M';
			return;
		case '5':
			*p = (unsigned char)'N';
			return;
		case '6':
			*p = (unsigned char)'O';
			return;
		case '7':
			*p = (unsigned char)'P';
			return;
		case '8':
			*p = (unsigned char)'Q';
			return;
		case '9':
			*p = (unsigned char)'R';
			return;
		default:
			/* What to do here */
			*p = (unsigned char)'}';
			return;
		}
	}
	switch (*p) {
	case '0':
		*p = (unsigned char)'{';
		return;
	case '1':
		*p = (unsigned char)'A';
		return;
	case '2':
		*p = (unsigned char)'B';
		return;
	case '3':
		*p = (unsigned char)'C';
		return;
	case '4':
		*p = (unsigned char)'D';
		return;
	case '5':
		*p = (unsigned char)'E';
		return;
	case '6':
		*p = (unsigned char)'F';
		return;
	case '7':
		*p = (unsigned char)'G';
		return;
	case '8':
		*p = (unsigned char)'H';
		return;
	case '9':
		*p = (unsigned char)'I';
		return;
	default:
		/* What to do here */
		*p = (unsigned char)'{';
		return;
	}
/* NOT REACHED */
}

static int
common_cmpc (const unsigned char *s1, const unsigned int c, const size_t size)
{
	const unsigned char	*s;
	size_t			i;
	int			ret;

	s = cob_current_module->collating_sequence;
	if (unlikely(s)) {
		for (i = 0; i < size; ++i) {
			if ((ret = s[s1[i]] - s[c]) != 0) {
				return ret;
			}
		}
	} else {
		for (i = 0; i < size; ++i) {
			if ((ret = s1[i] - c) != 0) {
				return ret;
			}
		}
	}
	return 0;
}

static int
is_national_padding (const unsigned char *s, const size_t size)
{
	size_t	i;
	int	ret = 1;

	i = 0;
	while (i < size && ret) {
		if (s[i] == ' ') {
			i++;
		} else if (size - i >= COB_ZENCSIZ
			   && !memcmp (&(s[i]), COB_ZENSPC, COB_ZENCSIZ)) {
			i += COB_ZENCSIZ;
		} else {
			ret = 0;
		}
	}
	return ret;
}

static int
alnum_cmps (const unsigned char *s1, const unsigned char *s2, const size_t size,
	    const unsigned char *col)
{
	size_t			i;
	int			ret;

	if (unlikely(col)) {
		for (i = 0; i < size; ++i) {
			if ((ret = col[s1[i]] - col[s2[i]]) != 0) {
				return ret;
			}
		}
	} else {
		for (i = 0; i < size; ++i) {
			if ((ret = s1[i] - s2[i]) != 0) {
				return ret;
			}
		}
	}
	return 0;
}

static int
national_cmps (const unsigned char *s1, const unsigned char *s2, const size_t size,
	       const unsigned char *col)
{
	size_t			i;
	int			ret = 0;
#ifdef	I18N_UTF8
	size_t			n;

	for (i = 0; i < size && !ret; i += n) {
		n = COB_U8BYTE_1(s1[i]);
		if (n != COB_U8BYTE_1(s2[i])) {
			ret = n - COB_U8BYTE_1(s2[i]);
		} else if (size - i < n) {
			ret =  -1;
		} else {
			ret = memcmp (&(s1[i]), &(s2[i]), n);
		}
	}
#else /*!I18N_UTF8*/
	for (i = 0; i < size && !ret; i += 2) {
		ret = ((s1[i] <<8 | s1[i + 1]) > (s2[i] <<8 | s2[i + 1]));
	}
#endif /*I18N_UTF8*/
	return ret;
}

static int
cob_cmp_char (cob_field *f, const unsigned int c)
{
	int	sign;
	int	ret;

	sign = cob_get_sign (f);
	ret = common_cmpc (f->data, c, f->size);
	if (COB_FIELD_TYPE (f) != COB_TYPE_NUMERIC_PACKED) {
		cob_put_sign (f, sign);
	}
	return ret;
}

static int
cob_cmp_all (cob_field *f1, cob_field *f2)
{
	unsigned char		*data;
	const unsigned char	*s;
	size_t			size;
	int			ret;
	int			sign;
	int (*common_cmps)() = (COB_FIELD_IS_NATIONAL (f1))?
		national_cmps : alnum_cmps;

	if ((COB_FIELD_TYPE (f1) == COB_TYPE_ALPHANUMERIC_ALL ||
	     COB_FIELD_TYPE (f1) == COB_TYPE_NATIONAL_ALL) &&
	    f1->size < f2->size) {
		size = f2->size;
		data = f2->data;
		sign = cob_get_sign (f2);
		ret = 0;
		s = cob_current_module->collating_sequence;
		while (size >= f1->size) {
			if ((ret = common_cmps (f1->data, data, f1->size, s)) != 0) {
				goto end;
			}
			size -= f1->size;
			data += f1->size;
		}
		if (size > 0) {
			ret = common_cmps (f1->data, data,  size, s);
		}
	} else {
		size = f1->size;
		data = f1->data;
		sign = cob_get_sign (f1);
		ret = 0;
		s = cob_current_module->collating_sequence;
		while (size >= f2->size) {
			if ((ret = common_cmps (data, f2->data, f2->size, s)) != 0) {
				goto end;
			}
			size -= f2->size;
			data += f2->size;
		}
		if (size > 0) {
			ret = common_cmps (data, f2->data, size, s);
		}
	}

end:
	if (COB_FIELD_TYPE (f1) != COB_TYPE_NUMERIC_PACKED) {
		cob_put_sign (f1, sign);
	}
	return ret;
}

static int
cob_cmp_simple_str (cob_field *f1, cob_field *f2)
{
	const unsigned char	*s;
	const cob_field		*lf, *sf;
	int			ret = 0;
	int (*common_cmps)() = (COB_FIELD_IS_NATIONAL (f1))?
		national_cmps : alnum_cmps;

	if (f1->size < f2->size) {
		lf = f2;
		sf = f1;
	} else {
		lf = f1;
		sf = f2;
	}
	s = cob_current_module->collating_sequence;
	/* compare common substring */
	if ((ret = common_cmps (f1->data, f2->data, sf->size, s)) == 0) {
		/* compare the rest (if any) with spaces */
		if (lf->size > sf->size) {
			ret = (COB_FIELD_IS_NATIONAL (lf))?
				!is_national_padding (&(lf->data[sf->size]),
						      lf->size - sf->size)
				: common_cmpc (&(lf->data[sf->size]), ' ',
					       lf->size - sf->size);
			if (lf == f2) {
				ret = -ret;
			}
		}
	}
	return ret;
}

static int
cob_cmp_alnum (cob_field *f1, cob_field *f2)
{
	int			ret;
	int			sign1;
	int			sign2;

	sign1 = cob_get_sign (f1);
	sign2 = cob_get_sign (f2);

	ret = cob_cmp_simple_str (f1, f2);

	if (COB_FIELD_TYPE (f1) != COB_TYPE_NUMERIC_PACKED) {
		cob_put_sign (f1, sign1);
	}
	if (COB_FIELD_TYPE (f2) != COB_TYPE_NUMERIC_PACKED) {
		cob_put_sign (f2, sign2);
	}
	return ret;
}

static int
sort_compare (const void *data1, const void *data2)
{
	size_t		i;
	int		cmp;
	cob_field	f1;
	cob_field	f2;

	for (i = 0; i < sort_nkeys; ++i) {
		f1 = f2 = *sort_keys[i].field;
		f1.data = (unsigned char *)data1 + sort_keys[i].offset;
		f2.data = (unsigned char *)data2 + sort_keys[i].offset;
		if (COB_FIELD_IS_NUMERIC(&f1)) {
			cmp = cob_numeric_cmp (&f1, &f2);
		} else if (COB_FIELD_IS_NATIONAL(&f1)) {
			cmp = national_cmps (f1.data, f2.data, f1.size, sort_collate);
		} else {
			cmp = alnum_cmps (f1.data, f2.data, f1.size, sort_collate);
		}
		if (cmp != 0) {
			return (sort_keys[i].flag == COB_ASCENDING) ? cmp : -cmp;
		}
	}
	return 0;
}

static struct tm *
job_or_current_localtime (void)
{
	time_t t;
	struct tm *rt = NULL;

	if (cob_localtm) {
		rt = cob_localtm;
	} else {
		t = time (NULL);
		rt = localtime (&t);
	}
	return rt;
}

/*
 * Global functions
 */

struct tm *
cob_localtime (const time_t *t)
{
	struct tm *rt = localtime (t);
	if (cob_localtm) {
		cob_localtm->tm_hour = rt->tm_hour;
		cob_localtm->tm_min  = rt->tm_min;
		cob_localtm->tm_sec  = rt->tm_sec;
		rt = cob_localtm;
	}
	return rt;
}

void
cob_verbose_output (const char *fmt, ...)
{
	va_list	ap;
	if (cob_verbose) {
		fputs ("libcob: ", stdout);
		va_start (ap, fmt);
		vfprintf (stdout, fmt, ap);
		va_end (ap);
		fputs ("\n", stdout);
	}
	return;
}

int
cob_io_rewrite_assumed (void)
{
	return cob_io_assume_rewrite;
}

void *
cob_malloc (const size_t size)
{
	void *mptr;

	mptr = calloc (1, size);
	if (unlikely(!mptr)) {
		cob_runtime_error ("Cannot acquire %d bytes of memory - Aborting", size);
		cob_stop_run (1);
	}
	return mptr;
}

void
cob_set_location (const char *progid, const char *sfile, const unsigned int sline,
		  const char *csect, const char *cpara, const char *cstatement)
{
	cob_current_program_id = progid;
	cob_source_file = sfile;
	cob_source_line = sline;
	cob_current_section = csect;
	cob_current_paragraph = cpara;
	if (cstatement) {
		cob_source_statement = cstatement;
	}
	if (cob_line_trace) {
		fprintf (stderr, "PROGRAM-ID: %s \tLine: %d \tStatement: %s\n",
			(char *)progid, sline, cstatement ? (char *)cstatement : "Unknown");
		fflush (stderr);
	}
}

void
cob_ready_trace (void)
{
	cob_line_trace = 1;
}

void
cob_reset_trace (void)
{
	cob_line_trace = 0;
}

unsigned char *
cob_get_pointer (const unsigned char *srcptr)
{
	unsigned char	*tmptr;

	memcpy (&tmptr, srcptr, sizeof (void *));
	return tmptr;
}

void *
cob_get_prog_pointer (const unsigned char *srcptr)
{
	void	*tmptr;

	memcpy (&tmptr, srcptr, sizeof (void *));
	return tmptr;
}

void
cob_memcpy (cob_field *dst, unsigned char *src, const int size)
{
	cob_field	temp;
	cob_field_attr	attr;

	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
	temp.size = size;
	temp.data = src;
	temp.attr = &attr;
	cob_move (&temp, dst);
}

static void
cob_hankaku_memcpy (cob_field *dst, unsigned char *src, const int size)
{
	cob_field	temp;
	cob_field_attr	attr;

	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
	temp.size = size;
	temp.data = src;
	temp.attr = &attr;
	cob_hankaku_move (&temp, dst);
}

const char *
cob_get_exception_name (const int exception_code)
{
	size_t	n;

	for (n = 0; n < EXCEPTION_TAB_SIZE; ++n) {
		if (exception_code == cob_exception_tab_code[n]) {
			return cob_exception_tab_name[n];
		}
	}
	return NULL;
}

void
cob_set_exception (const int id)
{
	cob_exception_code = cob_exception_tab_code[id];
	if (cob_exception_code) {
		cob_got_exception = 1;
		cob_orig_statement = cob_source_statement;
		cob_orig_line = cob_source_line;
		cob_orig_program_id = cob_current_program_id;
		cob_orig_section = cob_current_section;
		cob_orig_paragraph = cob_current_paragraph;
	}
}

void
cob_init (const int argc, char **argv)
{
	char	*s;
	size_t	i;
	char	buff[32];

	if (!cob_initialized) {
		cob_set_signal ();

		cob_argc = argc;
		cob_argv = argv;

		/* Get emergency buffer */
		runtime_err_str = cob_malloc (COB_ERRBUF_SIZE);

#ifdef	HAVE_SETLOCALE
		setlocale (LC_ALL, "");
		setlocale (LC_NUMERIC, "C");
		s = setlocale (LC_ALL, NULL);
		if (s) {
			locale_save = strdup (s);
		}
#endif
#ifdef	ENABLE_NLS
		bindtextdomain (PACKAGE, LOCALEDIR);
		textdomain (PACKAGE);
#endif

/* Dirty hack until we implement something better */
#if defined(_WIN32) && !defined(_MSC_VER)
		_setmode (_fileno (stdin), _O_BINARY);
		_setmode (_fileno (stdout), _O_BINARY);
		_setmode (_fileno (stderr), _O_BINARY);
#endif

		cob_init_numeric ();
		cob_init_strings ();
		cob_init_move ();
		cob_init_intrinsic ();
		cob_init_fileio ();
		cob_init_termio ();
		cob_init_call ();

		for (i = 0; i < 8; ++i) {
			memset (buff, 0, sizeof (buff));
			snprintf (buff, 31, "COB_SWITCH_%d", (int)(i + 1));
			s = getenv (buff);
			if (s && strcasecmp (s, "ON") == 0) {
				cob_switch[i] = 1;
			} else {
				cob_switch[i] = 0;
			}
		}

		s = getenv ("COB_LINE_TRACE");
		if (s && (*s == 'Y' || *s == 'y')) {
			cob_line_trace = 1;
		}

		s = getenv ("COB_DATE");
		if (s) {
			struct tm tm;
			memset (&tm, 0, sizeof (struct tm));
			tm.tm_isdst = -1;
			if (3 != sscanf (s, "%d/%d/%d", &tm.tm_year, &tm.tm_mon, &tm.tm_mday)) {
				fputs ("Warning: COB_DATE format invalid, ignored.\n", stderr);
			} else {
				tm.tm_year -= 1900;
				tm.tm_mon  -= 1;
				if (0 > mktime (&tm)) {
					fputs ("Warning: COB_DATE value invalid, ignored.\n", stderr);
				} else {
					cob_localtm = cob_malloc (sizeof (struct tm));
					memcpy (cob_localtm, &tm, sizeof (struct tm));
				}
			}
		}

		s = getenv ("COB_VERBOSE");
		if (s && (*s == 'Y' || *s == 'y')) {
			cob_verbose = 1;
		}

		s = getenv ("COB_IO_ASSUME_REWRITE");
		if (s && (*s == 'Y' || *s == 'y')) {
			cob_io_assume_rewrite = 1;
		}

		s = getenv ("COB_NIBBLE_C_UNSIGNED");
		if (s && (*s == 'Y' || *s == 'y')) {
			cob_nibble_c_for_unsigned = 1;
		}

		cob_initialized = 1;
	}
}

void
cob_module_enter (struct cob_module *module)
{
	if (unlikely(!cob_initialized)) {
		fputs ("Warning: cob_init expected in the main program\n", stderr);
		cob_init (0, NULL);
	}

	module->next = cob_current_module;
	cob_current_module = module;
}

void
cob_module_leave (struct cob_module *module)
{
	cob_current_module = cob_current_module->next;
}

void
cob_stop_run (const int status)
{
	struct exit_handlerlist	*h;

	if (exit_hdlrs != NULL) {
		h = exit_hdlrs;
		while (h != NULL) {
			h->proc ();
			h = h->next;
		}
	}
	cob_screen_terminate ();
	cob_exit_fileio ();
	exit (status);
}

void COB_NOINLINE
cob_runtime_error (const char *fmt, ...)
{
	struct handlerlist	*h;
	char			*p;
	va_list			ap;

	if (hdlrs != NULL) {
		h = hdlrs;
		if (runtime_err_str) {
			p = runtime_err_str;
			if (cob_source_file) {
				sprintf (runtime_err_str, "%s:%d: ",
					 cob_source_file, cob_source_line);
				p = runtime_err_str + strlen (runtime_err_str);
			}
			va_start (ap, fmt);
			vsprintf (p, fmt, ap);
			va_end (ap);
		}
		while (h != NULL) {
			if (runtime_err_str) {
				h->proc (runtime_err_str);
			} else {
				h->proc ((char *)"Malloc error");
			}
			h = h->next;
		}
		hdlrs = NULL;
	}

	/* prefix */
	if (cob_source_file) {
		fprintf (stderr, "%s:%d: ", cob_source_file, cob_source_line);
	}
	fputs ("libcob: ", stderr);

	/* body */
	va_start (ap, fmt);
	vfprintf (stderr, fmt, ap);
	va_end (ap);

	/* postfix */
	fputs ("\n", stderr);
	fflush (stderr);
}

void
cob_fatal_error (const unsigned int fatal_error)
{
	switch (fatal_error) {
	case COB_FERROR_INITIALIZED:
		cob_runtime_error ("cob_init() has not been called");
		break;
	case COB_FERROR_CODEGEN:
		cob_runtime_error ("Codegen error - Please report this");
		break;
	case COB_FERROR_CHAINING:
		cob_runtime_error ("ERROR - Recursive call of chained program");
		break;
	case COB_FERROR_STACK:
		cob_runtime_error ("Stack overflow, possible PERFORM depth exceeded");
		break;
	default:
		cob_runtime_error ("Unknown failure : %d", (int)fatal_error);
		break;
	}
	cob_stop_run (1);
}

void
cob_check_version (const char *prog, const char *packver, const int patchlev)
{
	if (strcmp (packver, PACKAGE_VERSION) || patchlev > PATCH_LEVEL) {
		cob_runtime_error ("Error - Version mismatch");
		cob_runtime_error ("%s has version/patch level %s/%d", prog, packver,
				   patchlev);
		cob_runtime_error ("Library has version/patch level %s/%d", PACKAGE_VERSION,
				   PATCH_LEVEL);
		cob_stop_run (1);
	}
}

/*
 * Sign
 */

int
cob_real_get_sign (const cob_field *f)
{
	unsigned char	*p;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		/* locate sign */
		if (unlikely(COB_FIELD_SIGN_LEADING (f))) {
			p = f->data;
		} else {
			p = f->data + f->size - 1;
		}

		/* get sign */
		if (unlikely(COB_FIELD_SIGN_SEPARATE (f))) {
			return (*p == '+') ? 1 : -1;
		} else {
			if (*p >= '0' && *p <= '9') {
				return 1;
			}
			if (*p == ' ') {
				*p = (unsigned char)'0';
				return 1;
			}
			if (unlikely(cob_current_module->display_sign)) {
				return cob_get_sign_ebcdic (p);
			} else {
#ifdef	COB_EBCDIC_MACHINE
				cob_get_sign_ascii (p);
#else
				GET_SIGN_ASCII (*p);
#endif
				return -1;
			}
		}
	case COB_TYPE_NUMERIC_PACKED:
		p = f->data + f->size - 1;
		return ((*p & 0x0f) == 0x0d) ? -1 : 1;
	default:
		return 0;
	}
}

void
cob_real_put_sign (const cob_field *f, const int sign)
{
	unsigned char	*p;
	int		c;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		/* locate sign */
		if (unlikely(COB_FIELD_SIGN_LEADING (f))) {
			p = f->data;
		} else {
			p = f->data + f->size - 1;
		}

		/* put sign */
		if (unlikely(COB_FIELD_SIGN_SEPARATE (f))) {
			c = (sign < 0) ? '-' : '+';
			if (*p != c) {
				*p = c;
			}
		} else if (unlikely(cob_current_module->display_sign)) {
			cob_put_sign_ebcdic (p, sign);
		} else if (sign < 0) {
#ifdef	COB_EBCDIC_MACHINE
			cob_put_sign_ascii (p);
#else
			PUT_SIGN_ASCII (*p);
#endif
		}
		return;
	case COB_TYPE_NUMERIC_PACKED:
		p = f->data + f->size - 1;
		if (sign < 0) {
			*p = (*p & 0xf0) | 0x0d;
		} else {
			*p = (*p & 0xf0) | 0x0c;
		}
		return;
	default:
		return;
	}
}

void
cob_field_to_string (const cob_field *f, char *s)
{
	int	i;

	memcpy (s, f->data, f->size);
	for (i = (int) f->size - 1; i >= 0; i--) {
		if (s[i] != ' ' && s[i] != 0) {
			break;
		}
	}
	s[i + 1] = '\0';
}

/*
 * Switch
 */

int
cob_get_switch (const int n)
{
	return cob_switch[n];
}

void
cob_set_switch (const int n, const int flag)
{
	if (flag == 0) {
		cob_switch[n] = 0;
	} else if (flag == 1) {
		cob_switch[n] = 1;
	}
}

int
cob_cmp (cob_field *f1, cob_field *f2)
{
	cob_field	temp;
	cob_field_attr	attr;
	unsigned char	buff[48];
#ifdef	I18N_UTF8
	cob_field	g1;
	cob_field	g2;
	unsigned char	*p;
#endif /*I18N_UTF8*/

	if (COB_FIELD_TYPE (f1) == COB_TYPE_NATIONAL ||
	    COB_FIELD_TYPE (f1) == COB_TYPE_NATIONAL_ALL ||
	    COB_FIELD_TYPE (f1) == COB_TYPE_NATIONAL_EDITED) {
		if (f2 == &cob_quote) {
			f2 = &cob_zen_quote;
		} else if (f2 == &cob_space) {
			f2 = &cob_zen_space;
#ifdef	I18N_UTF8
			memcpy (&g1, f1, sizeof (cob_field));
			p = g1.data + g1.size - 1;
			while (g1.size > 0) {
				if (*p == ' ') {
					g1.size--;
					p--;
				} else {
					break;
				}
			}
			if (!g1.size) {
				return 0;
			}
#endif /*I18N_UTF8*/

		} else if (f2 == &cob_zero) {
			f2 = &cob_zen_zero;
		}
	}
	if (COB_FIELD_TYPE (f2) == COB_TYPE_NATIONAL ||
	    COB_FIELD_TYPE (f2) == COB_TYPE_NATIONAL_ALL ||
	    COB_FIELD_TYPE (f2) == COB_TYPE_NATIONAL_EDITED) {
		if (f1 == &cob_quote) {
			f1 = &cob_zen_quote;
		} else if (f1 == &cob_space) {
			f1 = &cob_zen_space;
#ifdef	I18N_UTF8
			memcpy (&g2, f2, sizeof (cob_field));
			p = g2.data + g2.size - 1;
			while (g2.size > 0) {
				if (*p == ' ') {
					g2.size--;
					p--;
				} else {
					break;
				}
			}
	  		if (!g2.size) {
				return 0;
			}
#endif /*I18N_UTF8*/
		} else if (f1 == &cob_zero) {
			f1 = &cob_zen_zero;
		}
	}

	if (COB_FIELD_IS_NUMERIC (f1) && COB_FIELD_IS_NUMERIC (f2)) {
		return cob_numeric_cmp (f1, f2);
	}
	if (COB_FIELD_TYPE (f2) == COB_TYPE_ALPHANUMERIC_ALL) {
		if (f2 == &cob_zero && COB_FIELD_IS_NUMERIC (f1)) {
			return cob_cmp_int (f1, 0);
		} else if (f2->size == 1) {
			return cob_cmp_char (f1, f2->data[0]);
		} else {
			return cob_cmp_all (f1, f2);
		}
	} else if (COB_FIELD_TYPE (f1) == COB_TYPE_ALPHANUMERIC_ALL) {
		if (f1 == &cob_zero && COB_FIELD_IS_NUMERIC (f2)) {
			return -cob_cmp_int (f2, 0);
		} else if (f1->size == 1) {
			return -cob_cmp_char (f2, f1->data[0]);
		} else {
			return -cob_cmp_all (f2, f1);
		}
	} else if (COB_FIELD_TYPE (f2) == COB_TYPE_NATIONAL_ALL) {
		if (f2 == &cob_zero && COB_FIELD_IS_NUMERIC (f1)) {
			return cob_cmp_int (f1, 0);
		} else if (f2->size == 1) {
			return cob_cmp_char (f1, f2->data[0]);
		} else {
			return cob_cmp_all (f1, f2);
		}
	} else if (COB_FIELD_TYPE (f1) == COB_TYPE_NATIONAL_ALL) {
		if (f1 == &cob_zero && COB_FIELD_IS_NUMERIC (f2)) {
			return -cob_cmp_int (f2, 0);
		} else if (f1->size == 1) {
			return -cob_cmp_char (f2, f1->data[0]);
		} else {
			return -cob_cmp_all (f2, f1);
		}
	} else if ((COB_FIELD_TYPE (f1) == COB_TYPE_GROUP) || (COB_FIELD_TYPE (f2) == COB_TYPE_GROUP)) {
		return cob_cmp_simple_str (f1, f2);
	} else {
		if (COB_FIELD_IS_NUMERIC (f1)) {
			if (COB_FIELD_TYPE (f1) != COB_TYPE_NUMERIC_DISPLAY) {
				temp.size = COB_FIELD_DIGITS(f1);
				temp.data = buff;
				temp.attr = &attr;
				attr = *f1->attr;
				attr.type = COB_TYPE_NUMERIC_DISPLAY;
				attr.flags &= ~COB_FLAG_HAVE_SIGN;
				cob_move (f1, &temp);
				f1 = &temp;
			} else if (COB_FIELD_SIGN_SEPARATE (f1)) {
				temp.size = COB_FIELD_DIGITS(f1);
				temp.data = buff;
				temp.attr = &attr;
				attr = *f1->attr;
				attr.type = COB_TYPE_NUMERIC_DISPLAY;
				attr.flags = COB_FLAG_HAVE_SIGN;
				cob_move (f1, &temp);
				f1 = &temp;
			}
		}
		if (COB_FIELD_IS_NUMERIC (f2)) {
			if (COB_FIELD_TYPE (f2) != COB_TYPE_NUMERIC_DISPLAY) {
				temp.size = COB_FIELD_DIGITS(f2);
				temp.data = buff;
				temp.attr = &attr;
				attr = *f2->attr;
				attr.type = COB_TYPE_NUMERIC_DISPLAY;
				attr.flags &= ~COB_FLAG_HAVE_SIGN;
				cob_move (f2, &temp);
				f2 = &temp;
			} else if (COB_FIELD_SIGN_SEPARATE (f2)) {
				temp.size = COB_FIELD_DIGITS(f2);
				temp.data = buff;
				temp.attr = &attr;
				attr = *f2->attr;
				attr.type = COB_TYPE_NUMERIC_DISPLAY;
				attr.flags = COB_FLAG_HAVE_SIGN;
				cob_move (f2, &temp);
				f2 = &temp;
			}
		}
		return cob_cmp_alnum (f1, f2);
	}
}

/*
 * Class check
 */

int
cob_is_omitted (const cob_field *f)
{
	return f->data == NULL;
}

int
cob_is_numeric (const cob_field *f)
{
	unsigned char	*data;
	size_t		size;
	size_t		i;
	int		sign;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
		return 1;
	case COB_TYPE_NUMERIC_PACKED:
		/* check digits */
		for (i = 0; i < f->size - 1; ++i) {
			if ((f->data[i] & 0xf0) > 0x90 || (f->data[i] & 0x0f) > 0x09) {
				return 0;
			}
		}
		if ((f->data[i] & 0xf0) > 0x90) {
			return 0;
		}
		/* check sign */
		sign = f->data[i] & 0x0f;
		if (sign == 0x0f) {
			return 1;
		}
		if (COB_FIELD_HAVE_SIGN (f)) {
			if (sign == 0x0c || sign == 0x0d) {
				return 1;
			}
		} else if (cob_nibble_c_for_unsigned) {
			if (sign == 0x0c) {
				return 1;
			}
		}
		return 0;
	case COB_TYPE_NUMERIC_DISPLAY:
		data = COB_FIELD_DATA (f);
		size = COB_FIELD_SIZE (f);
		sign = cob_get_sign (f);
		for (i = 0; i < size; ++i) {
			if (!isdigit (data[i])) {
				cob_put_sign (f, sign);
				return 0;
			}
		}
		cob_put_sign (f, sign);
		return 1;
	default:
		for (i = 0; i < f->size; ++i) {
			if (!isdigit (f->data[i])) {
				return 0;
			}
		}
		return 1;
	}
}

int
cob_is_alpha (const cob_field *f)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		if (!isspace (f->data[i]) && !isalpha (f->data[i])) {
			return 0;
		}
	}
	return 1;
}

int
cob_is_upper (const cob_field *f)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		if (!isspace (f->data[i]) && !isupper (f->data[i])) {
			return 0;
		}
	}
	return 1;
}

int
cob_is_lower (const cob_field *f)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		if (!isspace (f->data[i]) && !islower (f->data[i])) {
			return 0;
		}
	}
	return 1;
}

/*
 * Table sort
 */

void
cob_table_sort_init (const int nkeys, const unsigned char *collating_sequence)
{
	sort_nkeys = 0;
	sort_keys = cob_malloc (nkeys * sizeof (struct cob_file_key));
	if (collating_sequence) {
		sort_collate = collating_sequence;
	} else {
		sort_collate = cob_current_module->collating_sequence;
	}
}

void
cob_table_sort_init_key (const int flag, cob_field *field, const size_t offset)
{
	sort_keys[sort_nkeys].flag = flag;
	sort_keys[sort_nkeys].field = field;
	sort_keys[sort_nkeys].offset = offset;
	sort_nkeys++;
}

void
cob_table_sort (cob_field *f, const int n)
{
	qsort (f->data, (size_t) n, f->size, sort_compare);
	free (sort_keys);
}

/*
 * Run-time error checking
 */

void
cob_check_based (const unsigned char *x, const char *name)
{
	char	msgword[COB_MINI_BUFF];

	if (!x) {
		cb_get_jisword_buff (name, msgword, sizeof (msgword));
		cob_runtime_error ("BASED/LINKAGE item '%s' has NULL address", msgword);
		cob_stop_run (1);
	}
}

void
cob_check_numeric (const cob_field *f, const char *name)
{
	unsigned char	*data;
	char		*p;
	char		*buff;
	size_t		i;
	char		msgword[COB_MINI_BUFF];

	if (!cob_is_numeric (f)) {
		buff = cob_malloc (COB_SMALL_BUFF);
		p = buff;
		data = f->data;
		for (i = 0; i < f->size; ++i) {
			if (isprint (data[i])) {
				*p++ = data[i];
			} else {
				p += sprintf (p, "\\%03o", data[i]);
			}
		}
		*p = '\0';
		cb_get_jisword_buff (name, msgword, sizeof (msgword));
		cob_runtime_error ("'%s' not numeric: '%s'", msgword, buff);
		cob_stop_run (1);
	}
}

void
cob_check_odo (const int i, const int min, const int max, const char *name)
{
	char	msgword[COB_MINI_BUFF];

	/* check the OCCURS DEPENDING ON item */
	if (i < min || max < i) {
		cob_set_exception (COB_EC_BOUND_ODO);
		cb_get_jisword_buff (name, msgword, sizeof (msgword));
		cob_runtime_error ("OCCURS DEPENDING ON '%s' out of bounds: %d", msgword, i);
		cob_stop_run (1);
	}
}

void
cob_check_subscript (const int i, const int min, const int max, const char *name)
{
	char	msgword[COB_MINI_BUFF];

	/* check the subscript */
	if (i < min || max < i) {
		cob_set_exception (COB_EC_BOUND_SUBSCRIPT);
		cb_get_jisword_buff (name, msgword, sizeof (msgword));
		cob_runtime_error ("Subscript of '%s' out of bounds: %d", msgword, i);
		cob_stop_run (1);
	}
}

int
cob_check_env (const char *name, const char *value)
{
	char *s;
	if (name == NULL || value == NULL) {
		return 0;
	}
	s = getenv (name);
	if (s != NULL) {
		if (strcmp (s, value) == 0) {
			return 1;
		}
	}
	return 0;
}

void
cob_check_ref_mod_national (int offset, int length, int size, const char *name)
{
	char	msgword[COB_MINI_BUFF];

	if (cob_check_env (NAMERMCHECK, VALUERMCHECK)) {
		return;
	}

	/* check the offset */
#ifdef	I18N_UTF8
	/* I18N_UTF8: Double bytes arragements on params are deleted. */
#else /*!I18N_UTF8*/
	offset += 1;
	offset = offset / 2;
	length = length / 2;
	size = size / 2;
#endif /*I18N_UTF8*/

	if (offset < 1 || offset > size) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		cb_get_jisword_buff (name, msgword, sizeof (msgword));
		cob_runtime_error ("Offset of '%s' out of bounds: %d", msgword, offset);
		cob_stop_run (1);
	}

	/* check the length */
	if (length < 1 || offset + length - 1 > size) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		cb_get_jisword_buff (name, msgword, sizeof (msgword));
		cob_runtime_error ("Length of '%s' out of bounds: %d", msgword, length);
		cob_stop_run (1);
	}
}

void
cob_check_ref_mod (const int offset, const int length, const int size, const char *name)
{
	char	msgword[COB_MINI_BUFF];

	if (cob_check_env (NAMERMCHECK, VALUERMCHECK)) {
		return;
	}
	/* check the offset */
	if (offset < 1 || offset > size) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		cb_get_jisword_buff (name, msgword, sizeof (msgword));
		cob_runtime_error ("Offset of '%s' out of bounds: %d", msgword, offset);
		cob_stop_run (1);
	}

	/* check the length */
	if (length < 1 || offset + length - 1 > size) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		cb_get_jisword_buff (name, msgword, sizeof (msgword));
		cob_runtime_error ("Length of '%s' out of bounds: %d", msgword, length);
		cob_stop_run (1);
	}
}

void
cob_check_mvstrnum (cob_field *src, cob_field *dst)
{
	size_t	i;

	switch (COB_FIELD_TYPE (src)) {
	case COB_TYPE_ALPHANUMERIC:
	case COB_TYPE_ALPHANUMERIC_ALL:
	case COB_TYPE_ALPHANUMERIC_EDITED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC:
		/*case COB_TYPE_NUMERIC_DISPLAY:*/
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_EDITED:
			for (i=0; i<src->size; i++) {
				if (! isdigit(COB_FIELD_DATA (src) [i])) {
					cob_runtime_error ("Numeric value is expected");
					cob_stop_run (1);
				}
			}
			break;
		}
		break;
    }
}

unsigned char *
cob_external_addr (const char *exname, const int exlength)
{
	static struct cob_external	*basext = NULL;
	struct cob_external		*eptr;
	char				msgword[COB_MINI_BUFF];

	for (eptr = basext; eptr; eptr = eptr->next) {
		if (!strcmp (exname, eptr->ename)) {
			if (exlength > eptr->esize) {
				cb_get_jisword_buff (exname, msgword, sizeof (msgword));
				cob_runtime_error ("EXTERNAL item '%s' has size > %d",
						   msgword, exlength);
				cob_stop_run (1);
			}
			cob_initial_external = 0;
			return (ucharptr)eptr->ext_alloc;
		}
	}
	eptr = cob_malloc (sizeof (struct cob_external));
	eptr->next = basext;
	eptr->esize = exlength;
	eptr->ename = cob_malloc (strlen (exname) + 1);
	strcpy (eptr->ename, exname);
	eptr->ext_alloc = cob_malloc ((size_t)exlength);
	basext = eptr;
	cob_initial_external = 1;
	return (ucharptr)eptr->ext_alloc;
}

/* Extended ACCEPT/DISPLAY */

void
cob_accept_date (cob_field *f)
{
	char	s[8];

	strftime (s, 7, "%y%m%d", job_or_current_localtime ());
	cob_memcpy (f, (ucharptr)s, 6);
}

void
cob_accept_date_yyyymmdd (cob_field *f)
{
	char	s[12];

	strftime (s, 9, "%Y%m%d", job_or_current_localtime ());
	cob_memcpy (f, (ucharptr)s, 8);
}

void
cob_accept_day (cob_field *f)
{
	char	s[8];

	strftime (s, 6, "%y%j", job_or_current_localtime ());
	cob_memcpy (f, (ucharptr)s, 5);
}

void
cob_accept_day_yyyyddd (cob_field *f)
{
	char	s[12];

	strftime (s, 8, "%Y%j", job_or_current_localtime ());
	cob_memcpy (f, (ucharptr)s, 7);
}

void
cob_accept_day_of_week (cob_field *f)
{
	struct tm	*tm;
	unsigned char	s[4];

	tm = job_or_current_localtime ();
	if (tm->tm_wday == 0) {
		s[0] = (unsigned char)'7';
	} else {
		s[0] = (unsigned char)(tm->tm_wday + '0');
	}
	cob_memcpy (f, s, 1);
}

void
cob_accept_time (cob_field *f)
{
#ifdef _WIN32
	SYSTEMTIME	syst;
#else
	time_t		t;
#if defined(HAVE_SYS_TIME_H) && defined(HAVE_GETTIMEOFDAY)
	struct timeval	tmv;
	char		buff2[8];
#endif
#endif
	char		s[12];

#ifdef _WIN32
	GetLocalTime (&syst);
	sprintf (s, "%2.2d%2.2d%2.2d%2.2d", syst.wHour, syst.wMinute,
		syst.wSecond, syst.wMilliseconds / 10);
#else
#if defined(HAVE_SYS_TIME_H) && defined(HAVE_GETTIMEOFDAY)
	gettimeofday (&tmv, NULL);
	t = tmv.tv_sec;
#else
	t = time (NULL);
#endif
	strftime (s, 9, "%H%M%S00", localtime (&t));
#if defined(HAVE_SYS_TIME_H) && defined(HAVE_GETTIMEOFDAY)
	sprintf(buff2, "%2.2ld", tmv.tv_usec / 10000);
	memcpy (&s[6], buff2, 2);
#endif
#endif
	cob_memcpy (f, (ucharptr)s, 8);
}

void
cob_display_command_line (cob_field *f)
{
	if (commlnptr) {
		free (commlnptr);
	}
	commlnptr = cob_malloc (f->size);
	commlncnt = f->size;
	memcpy (commlnptr, f->data, commlncnt);
}

void
cob_accept_command_line (cob_field *f)
{
	char	*buff;
	size_t	i;
	size_t	size;
	size_t	len;

	if (commlncnt) {
		cob_memcpy (f, commlnptr, (int)commlncnt);
		return;
	}
	buff = cob_malloc (COB_MEDIUM_BUFF);
	size = 0;
	for (i = 1; i < (size_t)cob_argc; ++i) {
		len = strlen (cob_argv[i]);
		if (size + len >= COB_MEDIUM_BUFF) {
			/* overflow */
			break;
		}
		memcpy (buff + size, cob_argv[i], len);
		size += len;
		buff[size++] = ' ';
	}
	cob_memcpy (f, (ucharptr)buff, (int)size);
	free (buff);
}

/*
 * Argument number
 */

void
cob_display_arg_number (cob_field *f)
{
	int		n;
	cob_field_attr	attr;
	cob_field	temp;

	temp.size = 4;
	temp.data = (unsigned char *)&n;
	temp.attr = &attr;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0, 0, NULL);
	cob_move (f, &temp);
	if (n < 0 || n >= cob_argc) {
		cob_set_exception (COB_EC_IMP_DISPLAY);
		return;
	}
	current_arg = n;
}

void
cob_accept_arg_number (cob_field *f)
{
	int		n = cob_argc - 1;
	cob_field_attr	attr;
	cob_field	temp;

	temp.size = 4;
	temp.data = (unsigned char *)&n;
	temp.attr = &attr;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0, 0, NULL);
	cob_move (&temp, f);
}

void
cob_accept_arg_value (cob_field *f)
{
	if (current_arg >= cob_argc) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
		return;
	}
	cob_memcpy (f, (ucharptr)cob_argv[current_arg], (int) strlen (cob_argv[current_arg]));
	current_arg++;
}

/*
 * Environment variable
 */

void
cob_display_environment (const cob_field *f)
{
	if (!cob_local_env) {
		cob_local_env = cob_malloc (COB_SMALL_BUFF);
	}
	if (f->size > COB_SMALL_MAX) {
		cob_set_exception (COB_EC_IMP_DISPLAY);
		return;
	}
	cob_field_to_string (f, cob_local_env);
}

void
cob_display_env_value (const cob_field *f)
{
	char	*env2;

	if (!cob_local_env) {
		cob_set_exception (COB_EC_IMP_DISPLAY);
		return;
	}
	if (!*cob_local_env) {
		cob_set_exception (COB_EC_IMP_DISPLAY);
		return;
	}
	env2 = cob_malloc (f->size + 1);
	cob_field_to_string (f, env2);
#ifdef _WIN32
	if (_putenv_s (cob_local_env, env2)) {
#else
	if (setenv (cob_local_env, env2, 1)) {
#endif
		cob_set_exception (COB_EC_IMP_DISPLAY);
	}
	free (env2);
}

void
cob_set_environment (const cob_field *f1, const cob_field *f2)
{
	cob_display_environment (f1);
	cob_display_env_value (f2);
}

void
cob_get_environment (const cob_field *envname, cob_field *envval)
{
	const char	*p;
	char		*buff;

	if (envname->size < COB_SMALL_BUFF) {
		buff = cob_malloc (COB_SMALL_BUFF);
		cob_field_to_string (envname, buff);
		p = getenv (buff);
		if (!p) {
			cob_set_exception (COB_EC_IMP_ACCEPT);
			p = " ";
		}
		cob_memcpy (envval, (ucharptr)p, (int) strlen (p));
		free (buff);
	} else {
		cob_set_exception (COB_EC_IMP_ACCEPT);
		p = " ";
		cob_memcpy (envval, (ucharptr)p, (int) strlen (p));
	}
}

void
cob_accept_environment (cob_field *f)
{
	const char *p = NULL;

	if (cob_local_env) {
		p = getenv (cob_local_env);
	}
	if (!p) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
		p = " ";
	}
	cob_hankaku_memcpy (f, (ucharptr)p, (int) strlen (p));
}

void
cob_chain_setup (void *data, const size_t parm, const size_t size)
{
	size_t	len;

	memset (data, ' ', size);
	if (parm <= (size_t)cob_argc - 1) {
		len = strlen (cob_argv[parm]);
		if (len <= size) {
			memcpy (data, cob_argv[parm], len);
		} else {
			memcpy (data, cob_argv[parm], size);
		}
	} else {
		memset (data, ' ', size);
	}
	cob_call_params = cob_argc - 1;
}

void
cob_allocate (unsigned char **dataptr, cob_field *retptr, cob_field *sizefld)
{
	void			*mptr = NULL;
	struct cob_alloc_cache	*cache_ptr;
	int			fsize;

	cob_exception_code = 0;
	fsize = cob_get_int (sizefld);
	if (fsize > 0) {
		cache_ptr = cob_malloc (sizeof (struct cob_alloc_cache));
		mptr = malloc ((size_t)fsize);
		if (!mptr) {
			cob_set_exception (COB_EC_STORAGE_NOT_AVAIL);
			free (cache_ptr);
		} else {
			memset (mptr, 0, (size_t)fsize);
			cache_ptr->cob_pointer = mptr;
			cache_ptr->size = (size_t)fsize;
			cache_ptr->next = cob_alloc_base;
			cob_alloc_base = cache_ptr;
		}
	}
	if (dataptr) {
		*dataptr = (unsigned char *)mptr;
	}
	if (retptr) {
		*(void **)(retptr->data) = mptr;
	}
}

void
cob_free_alloc (unsigned char **ptr1, unsigned char *ptr2)
{
	struct cob_alloc_cache	*cache_ptr;

	cob_exception_code = 0;
	if (ptr1 && *ptr1) {
		for (cache_ptr = cob_alloc_base; cache_ptr; cache_ptr = cache_ptr->next) {
			if (*(void **)ptr1 == cache_ptr->cob_pointer) {
				cache_ptr->cob_pointer = NULL;
				free (*ptr1);
				*ptr1 = NULL;
				return;
			}
		}
		cob_set_exception (COB_EC_STORAGE_NOT_ALLOC);
		return;
	}
	if (ptr2 && *(void **)ptr2) {
		for (cache_ptr = cob_alloc_base; cache_ptr; cache_ptr = cache_ptr->next) {
			if (*(void **)ptr2 == cache_ptr->cob_pointer) {
				cache_ptr->cob_pointer = NULL;
				free (*(void **)ptr2);
				*(void **)ptr2 = NULL;
				return;
			}
		}
		cob_set_exception (COB_EC_STORAGE_NOT_ALLOC);
		return;
	}
}

char *
cobgetenv (const char *name)
{
	if (name) {
		return getenv (name);
	}
	return NULL;
}

int
cobputenv (char *name)
{
	if (name) {
		return putenv (name);
	}
	return -1;
}

int
cobinit (void)
{
	cob_init (0, NULL);
	return 0;
}

void *
cobcommandline (int flags, int *pargc, char ***pargv, char ***penvp, char **pname)
{
	char		**spenvp;
	char		*spname;
	int		sflags;
	COB_UNUSED(spenvp);
	COB_UNUSED(spname);
	COB_UNUSED(sflags);

	if (!cob_initialized) {
		cob_runtime_error ("'cobcommandline' - Runtime has not been initialized");
		cob_stop_run (1);
	}
	if (pargc && pargv) {
		cob_argc = *pargc;
		cob_argv = *pargv;
	}
	/* Shut up the compiler */
	sflags = flags;
	if (penvp) {
		spenvp = *penvp;
	}
	if (pname) {
		spname = *pname;
	}
	/* What are we supposed to return here? */
	return NULL;
}

void
cobexit (const int status)
{
	cob_stop_run (status);
}

int
cobtidy (void)
{
	struct exit_handlerlist	*h;

	if (exit_hdlrs != NULL) {
		h = exit_hdlrs;
		while (h != NULL) {
			h->proc ();
			h = h->next;
		}
	}
	cob_screen_terminate ();
	cob_exit_fileio ();
	return 0;
}

/* System routines */

int
CBL_EXIT_PROC (unsigned char *x, unsigned char *pptr)
{
	struct exit_handlerlist *hp = NULL;
	struct exit_handlerlist *h = exit_hdlrs;
	int			(**p)(void) = NULL;

	COB_CHK_PARMS (CBL_EXIT_PROC, 2);

	memcpy (&p, &pptr, sizeof (void *));
	if (!p || !*p) {
		return -1;
	}
	/* remove handler anyway */
	while (h != NULL) {
		if (h->proc == *p) {
			if (hp != NULL) {
				hp->next = h->next;
			} else {
				exit_hdlrs = h->next;
			}
			if (hp) {
				free (hp);
			}
			break;
		}
		hp = h;
		h = h->next;
	}
	if   (*x != 0 && *x != 2 && *x != 3) {	/* remove handler */
		return 0;
	}
	h = cob_malloc (sizeof(struct exit_handlerlist));
	h->next = exit_hdlrs;
	h->proc = *p;
	exit_hdlrs = h;
	return 0;
}

int
CBL_ERROR_PROC (unsigned char *x, unsigned char *pptr)
{
	struct handlerlist	*hp = NULL;
	struct handlerlist	*h = hdlrs;
	int			(**p)(char *s) = NULL;

	COB_CHK_PARMS (CBL_ERROR_PROC, 2);

	memcpy (&p, &pptr, sizeof (void *));
	if (!p || !*p) {
		return -1;
	}
	/* remove handler anyway */
	while (h != NULL) {
		if (h->proc == *p) {
			if (hp != NULL) {
				hp->next = h->next;
			} else {
				hdlrs = h->next;
			}
			if (hp) {
				free (hp);
			}
			break;
		}
		hp = h;
		h = h->next;
	}
	if (*x != 0) {	/* remove handler */
		return 0;
	}
	h = cob_malloc (sizeof(struct handlerlist));
	h->next = hdlrs;
	h->proc = *p;
	hdlrs = h;
	return 0;
}

int
SYSTEM (const unsigned char *cmd)
{
	char	*buff;
	int	i;

	COB_CHK_PARMS (SYSTEM, 1);

	if (cob_current_module->cob_procedure_parameters[0]) {
		i = (int)cob_current_module->cob_procedure_parameters[0]->size;
		if (i > COB_MEDIUM_MAX) {
			cob_runtime_error ("Parameter to SYSTEM call is larger than 8192 characters");
			cob_stop_run (1);
		}
		i--;
		for (; i >= 0; i--) {
			if (cmd[i] != ' ' && cmd[i] != 0) {
				break;
			}
		}
		if (i >= 0) {
			buff = cob_malloc ((size_t)(i + 2));
			memcpy (buff, cmd, (size_t)(i + 1));
			if (cob_screen_initialized) {
				cob_screen_set_mode (0);
			}
			i = system (buff);
			free (buff);
			if (cob_screen_initialized) {
				cob_screen_set_mode (1);
			}
			return i;
		}
	}
	return 1;
}

int
CBL_AND (unsigned char *data_1, unsigned char *data_2, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_AND, 3);

	if (length <= 0) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] &= data_1[n];
	}
	return 0;
}

int
CBL_OR (unsigned char *data_1, unsigned char *data_2, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_OR, 3);

	if (length <= 0) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] |= data_1[n];
	}
	return 0;
}

int
CBL_NOR (unsigned char *data_1, unsigned char *data_2, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_NOR, 3);

	if (length <= 0) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] = ~(data_1[n] | data_2[n]);
	}
	return 0;
}

int
CBL_XOR (unsigned char *data_1, unsigned char *data_2, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_XOR, 3);

	if (length <= 0) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] ^= data_1[n];
	}
	return 0;
}

int
CBL_IMP (unsigned char *data_1, unsigned char *data_2, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_IMP, 3);

	if (length <= 0) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] = (~data_1[n]) | data_2[n];
	}
	return 0;
}

int
CBL_NIMP (unsigned char *data_1, unsigned char *data_2, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_NIMP, 3);

	if (length <= 0) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] = data_1[n] & (~data_2[n]);
	}
	return 0;
}

int
CBL_EQ (unsigned char *data_1, unsigned char *data_2, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_EQ, 3);

	if (length <= 0) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] = ~(data_1[n] ^ data_2[n]);
	}
	return 0;
}

int
CBL_NOT (unsigned char *data_1, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_NOT, 2);

	if (length <= 0) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_1[n] = ~data_1[n];
	}
	return 0;
}

int
CBL_XF4 (unsigned char *data_1, unsigned char *data_2)
{
	size_t	n;

	COB_CHK_PARMS (CBL_XF4, 2);

	*data_1 = 0;
	for (n = 0; n < 8; ++n) {
		*data_1 |= (data_2[n] & 1) << (7 - n);
	}
	return 0;
}

int
CBL_XF5 (unsigned char *data_1, unsigned char *data_2)
{
	size_t	n;

	COB_CHK_PARMS (CBL_XF5, 2);

	for (n = 0; n < 8; ++n) {
		data_2[n] = (*data_1 & (1 << (7 - n))) ? 1 : 0;
	}
	return 0;
}

int
CBL_X91 (unsigned char *result, const unsigned char *func, unsigned char *parm)
{
	unsigned char	*p;
	size_t		i;

	switch (*func) {
	case 11:
		/* Set switches */
		p = parm;
		for (i = 0; i < 8; ++i, ++p) {
			if (*p == 0) {
				cob_switch[i] = 0;
			} else if (*p == 1) {
				cob_switch[i] = 1;
			}
		}
		*result = 0;
		break;
	case 12:
		/* Get switches */
		p = parm;
		for (i = 0; i < 8; ++i, ++p) {
			*p = cob_switch[i];
		}
		*result = 0;
		break;
	case 16:
		/* Return number of call parameters */
		*parm = cob_save_call_params;
		*result = 0;
		break;
	default:
		*result = 1;
		break;
	}
	return 0;
}

int
CBL_TOUPPER (unsigned char *data, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_TOUPPER, 2);

	if (length > 0) {
		for (n = 0; n < (size_t)length; ++n) {
			if (islower (data[n])) {
				data[n] = toupper (data[n]);
			}
		}
	}
	return 0;
}

int
CBL_TOLOWER (unsigned char *data, const int length)
{
	size_t	n;

	COB_CHK_PARMS (CBL_TOLOWER, 2);

	if (length > 0) {
		for (n = 0; n < (size_t)length; ++n) {
			if (isupper (data[n])) {
				data[n] = tolower (data[n]);
			}
		}
	}
	return 0;
}

int
CBL_OC_NANOSLEEP (unsigned char *data)
{
	long long	nsecs;
#ifdef	_WIN32
#if 0
	struct timeval	tv;
#else
	unsigned int	msecs;
#endif
#else
	struct timespec	tsec;
#endif

	COB_CHK_PARMS (CBL_OC_NANOSLEEP, 1);

	if (cob_current_module->cob_procedure_parameters[0]) {
		nsecs = cob_get_long_long (cob_current_module->cob_procedure_parameters[0]);
		if (nsecs > 0) {
#ifdef	_WIN32
#if 0
			nsecs /= 1000;
			if (nsecs > 0) {
				tv.tv_sec = (long)(nsecs / 1000000);
				tv.tv_usec = (long)(nsecs % 1000000);
				select (0, (void *)0, (void *)0, (void *)0, &tv);
			}
#else
			msecs = (unsigned int)(nsecs / 1000000);
			if (msecs > 0) {
				Sleep (msecs);
			}
#endif
#else
			tsec.tv_sec = nsecs / 1000000000;
			tsec.tv_nsec = nsecs % 1000000000;
#ifdef __370__
			if(tsec.tv_sec > 0) sleep(tsec.tv_sec);
#else
			nanosleep (&tsec, NULL);
#endif
#endif
		}
	}
	return 0;
}

int
cob_acuw_getpid (void)
{
	return (int)getpid ();
}

int
cob_return_args (unsigned char *data)
{
	COB_CHK_PARMS (C$NARG, 1);

	if (cob_current_module->cob_procedure_parameters[0]) {
		cob_set_int (cob_current_module->cob_procedure_parameters[0], cob_save_call_params);
	}
	return 0;
}

int
cob_parameter_size (unsigned char *data)
{
	int	n;

	COB_CHK_PARMS (C$PARAMSIZE, 1);

	if (cob_current_module->cob_procedure_parameters[0]) {
		n = cob_get_int (cob_current_module->cob_procedure_parameters[0]);
		if (n > 0 && n <= cob_save_call_params) {
			n--;
			if (cob_current_module->next &&
			    cob_current_module->next->cob_procedure_parameters[n]) {
				return cob_current_module->next->cob_procedure_parameters[n]->size;
			}
		}
	}
	return 0;
}

int
cob_acuw_sleep (unsigned char *data)
{
	int	n;

	COB_CHK_PARMS (C$SLEEP, 1);

	if (cob_current_module->cob_procedure_parameters[0]) {
		n = cob_get_int (cob_current_module->cob_procedure_parameters[0]);
		if (n > 0 && n < 3600*24*7) {
#ifdef	_WIN32
			Sleep (n*1000);
#else
			sleep ((unsigned int)n);
#endif
		}
	}
	return 0;
}

int
cob_acuw_justify (unsigned char *data, ...)
{
	unsigned char	*direction;
	size_t		datalen;
	int		n;
	int		shifting = 0;
	size_t		left = 0;
	size_t		right = 0;
	size_t		movelen;
	size_t		centrelen;
	va_list		args;

	COB_CHK_PARMS (C$JUSTIFY, 1);

	datalen = cob_current_module->cob_procedure_parameters[0]->size;
	if (datalen < 2) {
		return 0;
	}
	if (data[0] != ' ' && data[datalen - 1] != ' ') {
		return 0;
	}
	for (n = 0; n < (int)datalen; ++n, ++left) {
		if (data[n] != ' ') {
			break;
		}
	}
	if (n == (int)datalen) {
		return 0;
	}
	left = n;
	for (n = (int)datalen - 1; n >= 0; --n, ++right) {
		if (data[n] != ' ') {
			break;
		}
	}
	movelen = datalen - left - right;
	if (cob_call_params > 1) {
		va_start (args, data);
		direction = va_arg (args, unsigned char *);
		va_end (args);
		if (*direction == 'L') {
			shifting = 1;
		} else if (*direction == 'C') {
			shifting = 2;
		}
	}
	switch (shifting) {
	case 1:
		memmove (data, &data[left], movelen);
		memset (&data[movelen], ' ', datalen - movelen);
		break;
	case 2:
		centrelen = (left + right) / 2;
		memmove (&data[centrelen], &data[left], movelen);
		memset (data, ' ', centrelen);
		if ((left + right) % 2) {
			memset (&data[centrelen + movelen], ' ', centrelen + 1);
		} else {
			memset (&data[centrelen + movelen], ' ', centrelen);
		}
		break;
	default:
		memmove (&data[left + right], &data[left], movelen);
		memset (data, ' ', datalen - movelen);
		break;
	}
	return 0;
}

void
cob_set_programid (struct cob_module *module, const char *program_name)
{
	int	length;
	length = strlen (program_name);
	if (module->program_id != NULL) {
		free (module->program_id);
	}
	module->program_id = cob_malloc ((const size_t) length+1);
	strcpy (module->program_id, program_name);
}

int
cob_acuw_calledby (unsigned char *data)
{
	int		length;
	cob_field	*f1;
	char		*called_program_name;

	COB_CHK_PARMS (C$CALLEDBY, 1);

	if (cob_current_module->cob_procedure_parameters[0]) {
		f1 = cob_current_module->cob_procedure_parameters[0];
		if (cob_current_module->next == NULL) {
			memset (f1->data, ' ', (int)f1->size);
			return 0;
		} else {
			called_program_name = (char *)cob_current_module->next->program_id;
			if (called_program_name == NULL) {
				return -1;
			}
			length = (int)f1->size;
			if (strlen (called_program_name) < length) {
				length = strlen (called_program_name);
			}
			memcpy (f1->data, called_program_name, length);
		}
	}
	return 1;
}

char *
cb_get_jisword_buff (const char *name, char *jbuf, size_t n)
{
	size_t		siz;
	unsigned int	c;
	const char	*cp, *cs, *ce;
	int		flag_quoted = 0;
	char		*p, *rt = NULL;

	cs = &(name[0]);
	ce = &(name[strlen (name) - 1]);

	/* strip quotes */
	if (*cs == '\'' && *ce == '\'') {
		cs++;
		--ce;
		flag_quoted = 1;
	}

	/* decode if encoded */
	if (ce - cs >= 5 && !memcmp (cs, "___", 3) && !memcmp (ce-2, "___", 3)) {
		cs += 3;
		ce -= 3;
		if (!flag_quoted) {
			siz = (ce - cs + 1) / 2 + 1;
		} else {
			siz = (ce - cs + 1) / 2 + 3;
		}
		if (!jbuf) {
			rt = cob_malloc (siz);
		} else {
			if (siz > n) {
				c = siz - n;
				siz -= c;
				ce -= c * 2;
			}
			memset (jbuf, 0, n);
			rt = jbuf;
		}
		if (flag_quoted && siz > 2) {
			rt [0] = rt [siz - 2] = '\'';
			p = &(rt[1]);
		} else {
			p = &(rt[0]);
		}
		for (c = 1, cp = cs; cp <= ce; cp++, p += (c = !c)) {
			if (*cp >= '0' && *cp <= '9') {
				*p |= (*cp - '0') << (c << 2);
			} else if (*cp >= 'A' && *cp <= 'F') {
				*p |= (*cp - 'A' + 10) << (c << 2);
			} else {
				*p = '?';
				cp += c;
				c = 0;
			}
		}
	} else {
		if (!jbuf) {
			rt = strdup (name);
		} else {
			memset (jbuf, 0, n);
			strncpy (jbuf, name, n - 1);
			rt = jbuf;
		}
	}
	return rt;
}

char *
cb_get_jisword (const char *name)
{
	return cb_get_jisword_buff (name, NULL, 0);
}

/* I18N_UTF8: Map half width chars to full width correspondings. */
#ifdef	I18N_UTF8
int
ascii_to_utf8 (int c, unsigned char *p)
{
	int	rt = 0;

	if (c == 0x5C) {			/* Yen sign */
		*p++ = 0xEF;
		*p++ = 0xBF;
		*p++ = 0xA5;
	} else if (c == 0x20) {			/* SPC */
		*p++ = 0xE3;
		*p++ = 0x80;
		*p++ = 0x80;
	} else if (c >= 0x21 && c <= 0x7E) {	/* ASCII */
		*p++ = 0xEF;
		*p++ = 0xBC;
		*p++ = 0x60 + c;
	} else {				/* No char. mapped there */
		*p++ = 0xEF;
		*p++ = 0xBF;
		*p++ = 0xBD;
		rt = 1;
	}
	return rt;
}
#endif /*I18N_UTF8*/

/* I18N_UTF8: convert into NATIONAL charset. */
#ifdef I18N_UTF8
unsigned char *
cob_national (const unsigned char * src_data, int src_size)
{
	static const char* hankana[] = {
		"\xEF\xBD\xB1","\xEF\xBD\xB2","\xEF\xBD\xB3",
		"\xEF\xBD\xB4","\xEF\xBD\xB5","\xEF\xBD\xB6","\xEF\xBD\xB7",
		"\xEF\xBD\xB8","\xEF\xBD\xB9","\xEF\xBD\xBA","\xEF\xBD\xBB",
		"\xEF\xBD\xBC","\xEF\xBD\xBD","\xEF\xBD\xBE","\xEF\xBD\xBF",
		"\xEF\xBE\x80","\xEF\xBE\x81","\xEF\xBE\x82","\xEF\xBE\x83",
		"\xEF\xBE\x84","\xEF\xBE\x85","\xEF\xBE\x86","\xEF\xBE\x87",
		"\xEF\xBE\x88","\xEF\xBE\x89","\xEF\xBE\x8A","\xEF\xBE\x8B",
		"\xEF\xBE\x8C","\xEF\xBE\x8D","\xEF\xBE\x8E","\xEF\xBE\x8F",
		"\xEF\xBE\x90","\xEF\xBE\x91","\xEF\xBE\x92","\xEF\xBE\x93",
		"\xEF\xBE\x94","\xEF\xBE\x95","\xEF\xBE\x96","\xEF\xBE\x97",
		"\xEF\xBE\x98","\xEF\xBE\x99","\xEF\xBE\x9A","\xEF\xBE\x9B",
		"\xEF\xBE\x9C","\xEF\xBE\x9D",
		"\xEF\xBD\xA1","\xEF\xBD\xA2","\xEF\xBD\xA3",
		"\xEF\xBD\xA4","\xEF\xBD\xA5",
		"\xEF\xBD\xA6","\xEF\xBD\xA7",
		"\xEF\xBD\xA8","\xEF\xBD\xA9","\xEF\xBD\xAA","\xEF\xBD\xAB",
		"\xEF\xBD\xAC","\xEF\xBD\xAD","\xEF\xBD\xAE","\xEF\xBD\xAF",
		"\xEF\xBD\xB0",
		(char*)0
	};
	static const char* zenkana[] = {
		"\xE3\x82\xA2","\xE3\x82\xA4","\xE3\x82\xA6",
		"\xE3\x82\xA8","\xE3\x82\xAA","\xE3\x82\xAB","\xE3\x82\xAD",
		"\xE3\x82\xAF","\xE3\x82\xB1","\xE3\x82\xB3","\xE3\x82\xB5",
		"\xE3\x82\xB7","\xE3\x82\xB9","\xE3\x82\xBB","\xE3\x82\xBD",
		"\xE3\x82\xBF","\xE3\x83\x81","\xE3\x83\x84","\xE3\x83\x86",
		"\xE3\x83\x88","\xE3\x83\x8A","\xE3\x83\x8B","\xE3\x83\x8C",
		"\xE3\x83\x8D","\xE3\x83\x8E","\xE3\x83\x8F","\xE3\x83\x92",
		"\xE3\x83\x95","\xE3\x83\x98","\xE3\x83\x9B","\xE3\x83\x9E",
		"\xE3\x83\x9F","\xE3\x83\xA0","\xE3\x83\xA1","\xE3\x83\xA2",
		"\xE3\x83\xA4","\xE3\x83\xA6","\xE3\x83\xA8","\xE3\x83\xA9",
		"\xE3\x83\xAA","\xE3\x83\xAB","\xE3\x83\xAC","\xE3\x83\xAD",
		"\xE3\x83\xAF","\xE3\x83\xB3",
		"\xE3\x80\x82","\xE3\x80\x8C","\xE3\x80\x8D",
		"\xE3\x80\x81","\xE3\x83\xBB",
		"\xE3\x83\xB2","\xE3\x82\xA1",
		"\xE3\x82\xA3","\xE3\x82\xA5","\xE3\x82\xA7","\xE3\x82\xA9",
		"\xE3\x83\xA3","\xE3\x83\xA5","\xE3\x83\xA7","\xE3\x83\x83",
		"\xE3\x83\xBC"
	};
	const unsigned char	*p, *q, *ub = src_data + src_size;
	unsigned char		*dst_data, *p2, *p3;
	size_t			n, dst_size = 0;
	int			i;

	q = src_data;
	for (p = src_data; p < ub; p += n) {
		n = COB_U8BYTE_1(*p);
		if (n == 1) {
			/*ASCII to be expanded*/
			dst_size += 3;
		} else if (n == 3) {
			if (p+2 < ub &&
			    ((*q == 0xEF && *(q+1) == 0xBD && *(q+2) == 0xB3) ||
			     (*q == 0xEF && *(q+1) == 0xBD && (*(q+2) >= 0xB6 && *(q+2) <= 0xBF)) ||
			     (*q == 0xEF && *(q+1) == 0xBE && (*(q+2) >= 0x80 && *(q+2) <= 0x84)) ||
			     (*q == 0xEF && *(q+1) == 0xBE && (*(q+2) >= 0x8A && *(q+2) <= 0x8E))) &&
			    (*p == 0xEF && *(p+1) == 0xBE && (*(p+2) == 0x9E || *(p+2) == 0x9F))) {
				/*Dakuten,Han-dakuten*/
			} else {
				dst_size += 3;
			}
		} else {
			dst_size += n;
		}
		q = p;
	}
	dst_data = cob_malloc (dst_size + 1);
	q = src_data;
	for (p = src_data, p2 = dst_data; p < ub; p += n) {
		n = COB_U8BYTE_1(*p);
		if (n == 1) {
			ascii_to_utf8 (*p, p2);
			p2 += 3;
		} else if (n == 3) {
			p3 = (unsigned char*)0;
			if (*p == 0xEF && (*(p+1) == 0xBD || *(p+1) == 0xBE)) {
				/*Hankaku to Zenkaku*/
				for (i = 0;
				     hankana[i] && memcmp (hankana[i], p, 3);
				     i++)
					;
				if (hankana[i]) {
					p3 = (unsigned char*)zenkana[i];
				}
			}
			if (p3) {
				*p2++ = *p3++;
				*p2++ = *p3++;
				*p2++ = *p3++;
			} else if (*p == 0xEF && *(p+1) == 0xBE && *(p+2) == 0x9E) {
				/* Dakuten */
				if ((*q == 0xEF && *(q+1) == 0xBD && *(q+2) == 0xB3) ||
				    (*q == 0xEF && *(q+1) == 0xBD && (*(q+2) >= 0xB6 && *(q+2) <= 0xBF)) ||
				    (*q == 0xEF && *(q+1) == 0xBE && (*(q+2) >= 0x80 && *(q+2) <= 0x84)) ||
				    (*q == 0xEF && *(q+1) == 0xBE && (*(q+2) >= 0x8A && *(q+2) <= 0x8E))) {
					--p2;
					if (*(p2-2) == 0xE3 && *(p2-1) == 0x82 && *p2 == 0xBF) {
						*(p2-1) = 0x83;
						*p2++   = 0x80;
					} else if (*(p2-2) == 0xE3 && *(p2-1) == 0x82 && *p2 == 0xA6) {
						*(p2-1) = 0x83;
						*p2++   = 0xB4;
					} else {
						(*p2++)++;
					}
				} else {
					*p2++ = 0xE3;
					*p2++ = 0x82;
					*p2++ = 0x9B;
				}
			} else if (*p == 0xEF && *(p+1) == 0xBE && *(p+2) == 0x9F) {
				/* Han-dakuten */
				if (*q == 0xEF && *(q+1) == 0xBE && (*(q+2) >= 0x8A && *(q+2) <= 0x8E)) {
					--p2;
					(*p2++) += 2;
				} else {
					*p2++ = 0xE3;
					*p2++ = 0x82;
					*p2++ = 0x9C;
				}
			} else {
				*p2++ = *p;
				*p2++ = *(p+1);
				*p2++ = *(p+2);
			}
		} else {
			memcpy (p2, p, n);
			p2 += n;
		}
		q = p;
	}
	*p2 = '\0';
	return dst_data;
}
#endif /*I18N_UTF8*/
