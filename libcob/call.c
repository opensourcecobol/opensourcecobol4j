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
 * License along with this library; see the file COPYING.LIB.  If not write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */


#include "config.h"
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

/*	NOTE - The following variable should be uncommented when
	it is known that dlopen(NULL) is borked.
	This is known to be true for some PA-RISC HP-UX 11.11 systems.
	This is fixed with HP patch PHSS_28871. (There are newer but this
	fixes dlopen/dlsym problems)
*/
/* #define COB_BORKED_DLOPEN */
	
#ifdef	USE_LIBDL

#define __USE_GNU	1
#include <dlfcn.h>
#define lt_dlopen(x)	dlopen(x, RTLD_LAZY | RTLD_GLOBAL)
#define lt_dlsym(x, y)	dlsym(x, y)
#define lt_dlclose(x)	dlclose(x)
#define lt_dlerror()	dlerror()
#define lt_dlhandle	void *

#elif	defined(_WIN32)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
/* Prototype */
static char *	lt_dlerror (void);

static HMODULE
lt_dlopen (const char *x)
{
	if (x == NULL) {
		return GetModuleHandle (NULL);
	}
	return LoadLibrary(x);
}
#define lt_dlsym(x, y)	GetProcAddress(x, y)
#define lt_dlclose(x)	FreeLibrary(x)
static char errbuf[64];
static char *
lt_dlerror ()
{
	sprintf(errbuf, "LoadLibrary/GetProcAddress error %d", (int)GetLastError());
	return errbuf;
}
#define	lt_dlinit()
#define lt_dlhandle	HMODULE

#else

#define LT_NON_POSIX_NAMESPACE 1
#include <ltdl.h>

#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

#ifdef	_MSC_VER
#define PATHSEPC ';'
#define PATHSEPS ";"
#else
#define PATHSEPC ':'
#define PATHSEPS ":"
#endif

#define	COB_MAX_COBCALL_PARMS	16
#define	CALL_BUFF_SIZE		256
#define	CALL_FILEBUFF_SIZE	2048

/* Local variables */

static char		**resolve_path = NULL;
static char		*resolve_error = NULL;
static char		*resolve_error_buff = NULL;
static lt_dlhandle	mainhandle = NULL;
static size_t		name_convert = 0;
static size_t		resolve_size = 0;
static size_t		cobjmp_primed = 0;
static void		*call_buffer;
static char		*call_filename_buff;
static char		*call_entry_buff;
static unsigned char	*call_entry2_buff;
static size_t		call_lastsize;

#if	defined (_WIN32) || !defined (RTLD_DEFAULT)
struct struct_handle {
	struct struct_handle	*next;
	lt_dlhandle		preload_handle;
};
static struct struct_handle	*pre_handle = NULL;
#endif

/*
 * Call table
 */

#define HASH_SIZE	131

struct call_hash {
	struct call_hash	*next;
	const char		*name;
	void			*func;
	void			*cancel;
	size_t			flag_is_active;
};

#ifdef	COB_ALT_HASH
static struct call_hash *call_table = NULL;
#else
static struct call_hash **call_table = NULL;
#endif

/*
 * Call List
 */
struct call_stack_list		*call_stack_list_head = NULL;
struct call_stack_list		*current_call_stack_list = NULL;

struct system_table {
	const char		*syst_name;
	void			*syst_call;
};

#undef	COB_SYSTEM_GEN
#define	COB_SYSTEM_GEN(x, y, z)		{ x, (void *)z },
static const struct system_table	system_tab[] = {
#include "system.def"
	{ NULL, NULL }
};
#undef	COB_SYSTEM_GEN

/* Local functions */

static void * COB_NOINLINE
cob_strdup (const void *stptr)
{
	void	*mptr;
	size_t	len;

	len = strlen (stptr) + 1;
	mptr = cob_malloc (len);
	memcpy (mptr, stptr, len);
	return mptr;
}

static void
cob_set_library_path (const char *path)
{
	char		*p;
	size_t		i;

	/* clear the previous path */
	if (resolve_path) {
		free (resolve_path[0]);
		free (resolve_path);
	}

	/* count the number of separators */
	resolve_size = 1;
	for (p = strchr (path, PATHSEPC); p; p = strchr (p + 1, PATHSEPC)) {
		resolve_size++;
	}

	/* build path array */
	p = cob_strdup (path);
	resolve_path = cob_malloc (sizeof (char *) * resolve_size);
	resolve_path[0] = strtok (p, PATHSEPS);
	for (i = 1; i < resolve_size; ++i) {
		resolve_path[i] = strtok (NULL, PATHSEPS);
	}
}

static void *
cob_get_buff (const size_t buffsize)
{
	if (buffsize > call_lastsize) {
		call_lastsize = buffsize;
		free (call_buffer);
		call_buffer = cob_malloc (buffsize);
	}
	return call_buffer;
}

#if	defined (_WIN32) || !defined (RTLD_DEFAULT)
static void
cache_handle (lt_dlhandle libhandle)
{
	struct struct_handle	*newhandle;

	newhandle = cob_malloc (sizeof (struct struct_handle));
	newhandle->preload_handle = libhandle;
	newhandle->next = pre_handle;
	pre_handle = newhandle;
}
#endif

#ifndef	COB_ALT_HASH
static COB_INLINE size_t
hash (const unsigned char *s)
{
	size_t		val = 0;

	while (*s) {
		val += *s++;
	}
	return val % HASH_SIZE;
}
#endif

static void
insert (const char *name, void *func, void *cancel)
{
	struct call_hash	*p;
#ifndef	COB_ALT_HASH
	size_t			val;
#endif

	p = cob_malloc (sizeof (struct call_hash));
	p->name = cob_strdup (name);
	p->func = func;
	p->cancel = cancel;
#ifdef	COB_ALT_HASH
	p->next = call_table;
	call_table = p;
#else
	val = hash ((const unsigned char *)name);
	p->next = call_table[val];
	call_table[val] = p;
#endif
}

static void *
lookup (const char *name)
{
	struct call_hash	*p;

#ifdef	COB_ALT_HASH
	for (p = call_table; p; p = p->next) {
#else
	for (p = call_table[hash ((const unsigned char *)name)]; p; p = p->next) {
#endif
		if (strcmp (name, p->name) == 0) {
			return p->func;
		}
	}
	return NULL;
}

static void
init_call_stack_list (void)
{
	if (!call_stack_list_head) {
		call_stack_list_head = cob_malloc (sizeof (struct call_stack_list));
		memset (call_stack_list_head, 0, sizeof (struct call_stack_list));
	}
	current_call_stack_list = call_stack_list_head;
}

static struct call_stack_list *
cob_create_call_stack_list (char *name)
{
	struct call_stack_list *new_list = cob_malloc (sizeof (struct call_stack_list));
	memset (new_list, 0, sizeof (struct call_stack_list));
	new_list->parent = current_call_stack_list;
	new_list->name = cob_malloc (strlen (name) + 1);
	strcpy (new_list->name, name);
	current_call_stack_list = new_list;
	return new_list;
}

static void
cob_cancel_call_stack_list (struct call_stack_list *p)
{
	if (!p) {
		/*No program*/
		return;
	}
	static cob_field_attr a_2 = {33, 0, 0, 0, NULL};
	cob_field f = {strlen (p->name), (unsigned char *) p->name, &a_2};
	cob_field_cancel (&f);
	if (p->children) {
		cob_cancel_call_stack_list (p->children);
	}
	struct call_stack_list *s = p->sister;
	while (s != NULL) {
		cob_cancel_call_stack_list (s);
		s = s->sister;
	}
}

const char *
cob_resolve_error (void)
{
	const char	*p = resolve_error;

	resolve_error = NULL;
	return p;
}

void
cob_call_error (void)
{
	const char	*s;

	s = cob_resolve_error ();
	if (!s) {
		s = "Unknown error";
	}
	cob_runtime_error ("%s", s);
	cob_stop_run (1);
}

void
cob_set_cancel (const char *name, void *entry, void *cancel)
{
	struct call_hash	*p;

#ifdef	COB_ALT_HASH
	p = call_table;
#else
	p = call_table[hash ((const unsigned char *)name)];
#endif
	for (; p; p = p->next) {
		if (strcmp (name, p->name) == 0) {
			p->cancel = cancel;
			return;
		}
	}
	insert (name, entry, cancel);
}

void *
cob_resolve (const char *name)
{
	unsigned char		*p;
	const unsigned char	*s;
	void			*func;
#if	defined (_WIN32) || !defined (RTLD_DEFAULT)
	struct struct_handle	*chkhandle;
#endif
	lt_dlhandle		handle;
	size_t			i;
	struct stat		st;

/* Checked in generated code
	if (!cob_initialized) {
		fputs ("cob_init() must be called before cob_resolve()", stderr);
		cob_stop_run (1);
	}
*/
	cob_exception_code = 0;

	/* search the cache */
	func = lookup (name);
	if (func) {
		return func;
	}

	/* encode program name */
	p = (unsigned char *)call_entry_buff;
	s = (const unsigned char *)name;
	if (unlikely(isdigit (*s))) {
		p += sprintf ((char *)p, "_%02X", *s++);
	}
	for (; *s; ++s) {
		if (likely(isalnum (*s) || *s == '_')) {
			*p++ = *s;
		} else if (*s == '-') {
			*p++ = '_';
			*p++ = '_';
		} else {
			p += sprintf ((char *)p, "_%02X", *s);
		}
	}
	*p = 0;

	/* search the main program */
	if (mainhandle != NULL) {
		if ((func = lt_dlsym (mainhandle, call_entry_buff)) != NULL) {
			insert (name, func, NULL);
			resolve_error = NULL;
			return func;
		}
	}

	/* Search preloaded modules */
#if	defined (_WIN32) || !defined (RTLD_DEFAULT)
	for (chkhandle = pre_handle; chkhandle; chkhandle = chkhandle->next) {
		if ((func = lt_dlsym (chkhandle->preload_handle, call_entry_buff)) != NULL) {
			insert (name, func, NULL);
			resolve_error = NULL;
			return func;
		}
	}
#endif
#if	defined(USE_LIBDL) && defined (RTLD_DEFAULT)
	if ((func = lt_dlsym (RTLD_DEFAULT, call_entry_buff)) != NULL) {
		insert (name, func, NULL);
		resolve_error = NULL;
		return func;
	}
#endif

	s = (const unsigned char *)name;
	if (unlikely(name_convert != 0)) {
		s = (const unsigned char *)name;
		p = call_entry2_buff;
		for (; *s; ++s) {
			if (name_convert == 1 && isupper (*s)) {
				*p++ = tolower (*s);
			} else if (name_convert == 2 && islower (*s)) {
				*p++ = toupper (*s);
			} else {
				*p++ = *s;
			}
		}
		*p = 0;
		s = (const unsigned char *)call_entry2_buff;
	}

	/* search external modules */
	for (i = 0; i < resolve_size; ++i) {
		call_filename_buff[CALL_FILEBUFF_SIZE - 1] = 0;
		if (resolve_path[i] == NULL) {
			snprintf (call_filename_buff, CALL_FILEBUFF_SIZE - 1,
				  "%s.%s", s, COB_MODULE_EXT);
		} else {
			snprintf (call_filename_buff, CALL_FILEBUFF_SIZE - 1,
				  "%s/%s.%s", resolve_path[i], s, COB_MODULE_EXT);
		}
		if (stat (call_filename_buff, &st) == 0) {
			if ((handle = lt_dlopen (call_filename_buff)) != NULL) {
#if	defined (_WIN32) || !defined (RTLD_DEFAULT)
				/* Candidate for future calls */
				cache_handle (handle);
#endif
				if ((func = lt_dlsym (handle, call_entry_buff)) != NULL) {
					insert (name, func, NULL);
					resolve_error = NULL;
					return func;
				}
			}
			resolve_error_buff[CALL_BUFF_SIZE - 1] = 0;
			strncpy (resolve_error_buff, lt_dlerror (),
				 CALL_BUFF_SIZE - 1);
			resolve_error = resolve_error_buff;
			cob_set_exception (COB_EC_PROGRAM_NOT_FOUND);
			return NULL;
		}
	}
	resolve_error_buff[CALL_BUFF_SIZE - 1] = 0;
	snprintf (resolve_error_buff, CALL_BUFF_SIZE - 1,
		  "Cannot find module '%s'", name);
	resolve_error = resolve_error_buff;
	cob_set_exception (COB_EC_PROGRAM_NOT_FOUND);
	return NULL;
}

void *
cob_resolve_1 (const char *name)
{
	void	*p;

	p = cob_resolve (name);
	if (unlikely(!p)) {
		cob_call_error ();
	}
	return p;
}

void *
cob_call_resolve (const cob_field *f)
{
	char	*buff;

	buff = cob_get_buff (f->size + 1);
	cob_field_to_string (f, buff);
	return cob_resolve (buff);
}

void *
cob_call_resolve_1 (const cob_field *f)
{
	void	*p;

	p = cob_call_resolve (f);
	if (unlikely(!p)) {
		cob_call_error ();
	}
	return p;
}

void
cobcancel (const char *name)
{
	struct call_hash	*p;
	union {
		int	(*cancel_func)(int, ...);
		void	*cancel_void;
	} unicanc;

	if (unlikely(!name)) {
		cob_runtime_error ("NULL name parameter passed to 'cobcancel'");
		cob_stop_run (1);
	}
#ifdef	COB_ALT_HASH
	for (p = call_table; p; p = p->next) {
#else
	for (p = call_table[hash ((const unsigned char *)name)]; p; p = p->next) {
#endif
		if (strcmp (name, p->name) == 0) {
			if (p->cancel && !p->flag_is_active) {
				unicanc.cancel_void = p->cancel;
				unicanc.cancel_func (-1, NULL, NULL, NULL, NULL,
						     NULL, NULL, NULL, NULL);
			}
		}
	}
}

void
cob_field_cancel (const cob_field *f)
{
	char	*name;

	name = cob_get_buff (f->size + 1);
	cob_field_to_string (f, name);
	cobcancel (name);
}

void
cob_init_call (void)
{
	char				*buff;
	char				*s;
	char				*p;
	const struct system_table	*psyst;
#if	defined (_WIN32) || !defined (RTLD_DEFAULT)
	lt_dlhandle			libhandle;
#endif
	size_t				i;
	struct stat			st;

#ifndef	USE_LIBDL
	lt_dlinit ();
#endif

	/* big enough for anything from libdl/libltdl */
	resolve_error_buff = cob_malloc (CALL_BUFF_SIZE);

#ifndef	COB_ALT_HASH
	call_table = cob_malloc (sizeof (struct call_hash *) * HASH_SIZE);
#endif

	call_filename_buff = cob_malloc (CALL_FILEBUFF_SIZE);
	call_entry_buff = cob_malloc (COB_SMALL_BUFF);
	call_entry2_buff = cob_malloc (COB_SMALL_BUFF);
	s = getenv ("COB_LOAD_CASE");
	if (s != NULL) {
		if (strcasecmp (s, "LOWER") == 0) {
			name_convert = 1;
		} else if (strcasecmp (s, "UPPER") == 0) {
			name_convert = 2;
		}
	}

	buff = cob_malloc (COB_MEDIUM_BUFF);
	s = getenv ("COB_LIBRARY_PATH");
	if (s == NULL) {
		snprintf (buff, COB_MEDIUM_MAX, ".%s%s",
			  PATHSEPS, COB_LIBRARY_PATH);
	} else {
		snprintf (buff, COB_MEDIUM_MAX, "%s%s.%s%s",
			  s, PATHSEPS, PATHSEPS, COB_LIBRARY_PATH);
	}
	cob_set_library_path (buff);

#ifndef	COB_BORKED_DLOPEN
	mainhandle = lt_dlopen (NULL);
#endif

	s = getenv ("COB_PRE_LOAD");
	if (s != NULL) {
		p = cob_strdup (s);
		s = strtok (p, PATHSEPS);
		for (; s; s = strtok (NULL, PATHSEPS)) {
			for (i = 0; i < resolve_size; ++i) {
				buff[COB_MEDIUM_MAX] = 0;
				snprintf (buff, COB_MEDIUM_MAX, "%s/%s.%s",
					  resolve_path[i], s, COB_MODULE_EXT);
				if (stat (buff, &st) == 0) {
#if	defined (_WIN32) || !defined (RTLD_DEFAULT)
					if ((libhandle = lt_dlopen (buff)) != NULL) {
						cache_handle (libhandle);
#else
					if (lt_dlopen (buff) != NULL) {
#endif
						break;
					}
				}
			}
		}
		free (p);
	}
	free (buff);
	call_buffer = cob_malloc (CALL_BUFF_SIZE);
	call_lastsize = CALL_BUFF_SIZE;
	for (psyst = (struct system_table *)&system_tab[0]; psyst->syst_name; ++psyst) {
		insert (psyst->syst_name, psyst->syst_call, NULL);
	}
}

int
cobcall (const char *name, const int argc, void **argv)
{
	int	i;
	union {
		void	*(*funcptr)();
		int	(*funcint)();
		void	*func_void;
	} unifunc;
	void	*pargv[16];

	if (unlikely(!cob_initialized)) {
		cob_runtime_error ("'cobcall' - Runtime has not been initialized");
		cob_stop_run (1);
	}
	if (argc < 0 || argc > 16) {
		cob_runtime_error ("Invalid number of arguments to 'cobcall'");
		cob_stop_run (1);
	}
	if (unlikely(!name)) {
		cob_runtime_error ("NULL name parameter passed to 'cobcall'");
		cob_stop_run (1);
	}
	unifunc.func_void = cob_resolve_1 (name);
	memset (pargv, 0, sizeof(pargv));
	/* Set number of parameters */
	cob_call_params = argc;
	for (i = 0; i < argc; ++i) {
		pargv[i] = argv[i];
	}
	return unifunc.funcint (pargv[0], pargv[1], pargv[2], pargv[3],
				pargv[4], pargv[5], pargv[6], pargv[7],
				pargv[8], pargv[9], pargv[10], pargv[11],
				pargv[12], pargv[13], pargv[14], pargv[15]);
}

int
cobfunc (const char *name, const int argc, void **argv)
{
	int	ret;

	if (unlikely(!cob_initialized)) {
		cob_runtime_error ("'cobfunc' - Runtime has not been initialized");
		cob_stop_run (1);
	}
	ret = cobcall (name, argc, argv);
	cobcancel (name);
	return ret;
}

void *
cobsavenv (struct cobjmp_buf *jbuf)
{
	if (unlikely(!jbuf)) {
		cob_runtime_error ("NULL name parameter passed to 'cobsavenv'");
		cob_stop_run (1);
	}
	if (cobjmp_primed) {
		cob_runtime_error ("Multiple call to 'cobsetjmp'");
		cob_stop_run (1);
	}
	cobjmp_primed = 1;
	return jbuf->cbj_jmp_buf;
}

void *
cobsavenv2 (struct cobjmp_buf *jbuf, const int jsize)
{
	int	jtemp;
	COB_UNUSED(jtemp);

	/* Shut up compiler */
	jtemp = jsize;
	return cobsavenv (jbuf);
}

void
coblongjmp (struct cobjmp_buf *jbuf)
{
	if (unlikely(!jbuf)) {
		cob_runtime_error ("NULL name parameter passed to 'coblongjmp'");
		cob_stop_run (1);
	}
	if (!cobjmp_primed) {
		cob_runtime_error ("Call to 'coblongjmp' with no prior 'cobsetjmp'");
		cob_stop_run (1);
	}
	cobjmp_primed = 0;
	longjmp (jbuf->cbj_jmp_buf, 1);
}

void
cob_push_call_stack_list (char *name)
{
	if (!current_call_stack_list) {
		init_call_stack_list ();
	}

	struct call_stack_list *p = current_call_stack_list->children;
	if (!p) {
		current_call_stack_list->children = cob_create_call_stack_list (name);
		return;
	}
	if (strcmp (p->name, name) == 0) {
		current_call_stack_list = p;
		return;
	}
	if (!p->sister) {
		p->sister = cob_create_call_stack_list (name);
		return;
	}

	p = p->sister;
	for (;;) {
		if (strcmp (p->name, name) == 0) {
			current_call_stack_list = p;
			return;
		}
		if (p->sister == NULL) {
			break;
		}
		p = p->sister;
	}
	current_call_stack_list->sister = cob_create_call_stack_list (name);
	return;
}

void
cob_pop_call_stack_list ()
{
	current_call_stack_list = current_call_stack_list->parent;
}

void
cob_cancel_all ()
{
	if (!current_call_stack_list) {
		cob_runtime_error ("Call to 'cob_cancel_all' current stack is NULL");
		return;
	}
	cob_cancel_call_stack_list (current_call_stack_list->children);
	return;
}
