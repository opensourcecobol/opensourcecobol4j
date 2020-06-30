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

#define _LFS64_LARGEFILE		1
#define _LFS64_STDIO			1
#define _FILE_OFFSET_BITS		64
#define _LARGEFILE64_SOURCE		1
#ifdef _AIX
#define _LARGE_FILES			1
#endif /* _AIX */
#if defined(__hpux__) && !defined(__LP64__)
#define _APP32_64BIT_OFF_T		1
#endif

#ifdef __MINGW32__
#define __USE_MINGW_FSEEK	1
#endif /* __MINGW32__ */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#ifdef	HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifndef _WIN32
#include <dirent.h>
#endif

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>		/* for GetTempPath, GetTempFileName */
#include <direct.h>
#define	fsync	_commit
#define	getcwd	_getcwd
#define	chdir	_chdir
#define	mkdir	_mkdir
#define	rmdir	_rmdir
#endif

#ifndef	O_BINARY
#define	O_BINARY	0
#endif

#ifndef	O_LARGEFILE
#define	O_LARGEFILE	0
#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

#ifdef	WITH_DB
#ifdef	USE_DB41
#include <db.h>
#else
#if HAVE_DB1_DB_H
#include <db1/db.h>
#elif HAVE_DB_185_H
#include <db_185.h>
#elif HAVE_DB3_DB_185_H
#include <db3/db_185.h>
#elif HAVE_DB4_DB_185_H
#include <db4/db_185.h>
#elif HAVE_DB4_1_DB_185_H
#include <db4.1/db_185.h>
#elif HAVE_DB4_2_DB_185_H
#include <db4.2/db_185.h>
#elif HAVE_DB4_3_DB_185_H
#include <db4.3/db_185.h>
#elif HAVE_DB4_4_DB_185_H
#include <db4.4/db_185.h>
#elif HAVE_DB4_5_DB_185_H
#include <db4.5/db_185.h>
#elif HAVE_DB_H
#include <db.h>
#endif
#endif	/* USE_DB41 */

#elif	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM)

#define	WITH_ANY_ISAM
#include <signal.h>
#endif

#ifdef	WITH_CISAM
#include <isam.h>
#endif

#ifdef	WITH_DISAM
#include <disam.h>
#endif

#ifdef	WITH_VBISAM
#include <vbisam.h>
#endif

#if defined(__hpux__) || defined(_AIX) || defined(__sparc)
#define fseek fseeko
#define ftell ftello
#endif

#ifdef	_MSC_VER
#define fseek _fseeki64
#define ftell _ftelli64
#define lseek _lseeki64
#define off_t __int64
#endif

#if !defined(__linux__)
#define SEEK_INIT(f)	fseek ((FILE *)f->file, (off_t)0, SEEK_CUR)
#else
#define SEEK_INIT(f)
#endif


#ifdef _WIN32
#define INITIAL_FLAGS	O_BINARY
#else
#define INITIAL_FLAGS	0
#endif

/* SORT definitions */

#define COBSORTEND        1
#define COBSORTABORT      2
#define COBSORTFILEERR    3
#define COBSORTNOTOPEN    4

struct cobitem {
	struct cobitem		*next;
	size_t			end_of_block;
	int				record_size;
	unsigned char		block_byte;
	unsigned char		unique[sizeof(size_t)];
	unsigned char		item[1];
};

struct memory_struct {
	struct cobitem	*first;
	struct cobitem	*last;
	size_t		count;
};

struct file_struct {
	FILE		*fp;
	size_t		count;	/* count of items in temporary files */
};

struct cobsort {
	void			*pointer;
	struct cobitem		*empty;
	void			*sort_return;
	cob_field		*fnstatus;
	size_t			unique;
	size_t			retrieving;
	size_t			files_used;
	size_t			size;
	size_t			r_size;
	size_t			w_size;
	size_t			memory;
	int			destination_file;
	int			retrieval_queue;
	struct memory_struct	queue[4];
	struct file_struct	file[4];
};

/* End SORT definitions */

#ifdef _WIN32
HANDLE				listdir_handle;
LPWIN32_FIND_DATA	listdir_filedata;
#else
DIR					*listdir_handle;
struct dirent		*listdir_filedata;
#endif

#define        OPENMODESIZE  3
#define        READOPTSSIZE  4
#define        STARTCONDSIZE 2
#define        EXCPTCODESIZE 6
#define        FNSTATUSSIZE  3

cob_file		*cob_error_file;

#ifndef _WIN32
static int		cob_iteration = 0;
static pid_t		cob_process_id = 0;
#endif

static size_t		eop_status = 0;

static int		cob_do_sync = 0;
static int		cob_sort_memory = 128*1024*1024;

#ifdef	USE_DB41
static DB_ENV		*bdb_env = NULL;
static char		*bdb_home;
static char		*bdb_buff;
static const char	**bdb_data_dir = NULL;
static void		*record_lock_object;
static size_t		rlo_size = 0;
static unsigned int	bdb_lock_id;
#endif

static struct file_list {
	struct file_list	*next;
	cob_file		*file;
} *file_cache = NULL;

static char		*cob_file_path = NULL;
static char		*cob_ls_nulls = NULL;
static char		*cob_ls_fixed = NULL;
static char		*file_open_env;
static char		*file_open_name;
static char		*file_open_buff;

#define TIS_DEFINE_USERFH	"OC_USERFH"
#define COB_IO_CREATES		"OC_IO_CREATES"
#define COB_EXTEND_CREATES	"OC_EXTEND_CREATES"

/* Emergence buffer in case of malloc fail */
static char		runtime_buffer[COB_SMALL_BUFF];

#define RETURN_STATUS(x)	do { save_status (f, x, fnstatus); return; } while (0)

static const int	status_exception[] = {
	0,				/* 0x */
	COB_EC_I_O_AT_END,		/* 1x */
	COB_EC_I_O_INVALID_KEY,		/* 2x */
	COB_EC_I_O_PERMANENT_ERROR,	/* 3x */
	COB_EC_I_O_LOGIC_ERROR,		/* 4x */
	COB_EC_I_O_RECORD_OPERATION,	/* 5x */
	COB_EC_I_O_FILE_SHARING,	/* 6x */
	COB_EC_I_O,			/* unused */
	COB_EC_I_O,			/* unused */
	COB_EC_I_O_IMP			/* 9x */
};

static const char	* const prefix[] = { "DD_", "dd_", "" };
#define NUM_PREFIX	sizeof(prefix) / sizeof(char *)

#ifdef	COB_PARAM_CHECK
static const char	parm_msg[] = "CALL to %s requires %d parameters";
#endif

static int dummy_rnxt_del (cob_file *f);
static int dummy_rewrite (cob_file *f, const int opt);
static int dummy_read (cob_file *f, cob_field *key, const int read_opts);
static int dummy_start (cob_file *f, const int cond, cob_field *key);

static int cob_file_open (cob_file *f, char *filename, const int mode,
			  const int sharing);
static int cob_file_close (cob_file *f, const int opt);
static int cob_file_write_opt (cob_file *f, const int opt);

static int sequential_read (cob_file *f, const int read_opts);
static int sequential_write (cob_file *f, const int opt);
static int sequential_rewrite (cob_file *f, const int opt);
static int lineseq_read (cob_file *f, const int read_opts);
static int lineseq_write (cob_file *f, const int opt);
static int relative_start (cob_file *f, const int cond, cob_field *k);
static int relative_read (cob_file *f, cob_field *k, const int read_opts);
static int relative_read_next (cob_file *f, const int read_opts);
static int relative_write (cob_file *f, const int opt);
static int relative_rewrite (cob_file *f, const int opt);
static int relative_delete (cob_file *f);

struct indexfile;

#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM)

static int extract_key (
	  struct indexfile *fh
	, int ix_cob_key
	, const void *pb_rec
	, void *ret_key_value);
static int keycmp (
	  struct indexfile *fh
	, int ix_cob_key
	, const void *pb_rec
	, const void *pb_key);

#endif

#if	defined(WITH_DB) || defined(WITH_ANY_ISAM) || defined(WITH_INDEX_EXTFH)

#ifdef	WITH_DB
#ifdef	USE_DB41
#define DB_PUT(db,flags)	db->put (db, NULL, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, NULL, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->c_get (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, NULL, key, flags)
#define DB_CLOSE(db)		db->close (db, 0)
#define DB_SYNC(db)		db->sync (db, 0)
#define	cob_dbtsize_t		u_int32_t
#else
#define DB_PUT(db,flags)	db->put (db, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->seq (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, key, flags)
#define DB_CLOSE(db)		db->close (db)
#define DB_SYNC(db)		db->sync (db, 0)
#define DB_FIRST		R_FIRST
#define DB_LAST			R_LAST
#define DB_NEXT			R_NEXT
#define DB_PREV			R_PREV
#define	cob_dbtsize_t		size_t
#endif

#define DBT_SET(key,fld)			\
  key.data = fld->data;				\
  key.size = (cob_dbtsize_t) fld->size

struct indexed_file {
	size_t		key_index;
	unsigned char	*last_key;	/* the last key written */
	unsigned char	*temp_key;	/* used for temporary storage */
	DB		**db;		/* database handlers */
	DBT		key;
	DBT		data;
	unsigned char	**last_readkey;	/* the last key read */
	unsigned int	*last_dupno;	/* the last number of duplicates read */
	int		*rewrite_sec_key;
#ifdef	USE_DB41
	DBC		**cursor;
	DB_LOCK		bdb_file_lock;
	char		*filename;	/*needed for record locks*/
	DB_LOCK		bdb_record_lock;
	int		write_cursor_open;
	unsigned int	bdb_lock_id;
	int		record_locked;
	int		filenamelen;
#endif
};
#endif	/* WITH_DB */


static int indexed_open (cob_file *f, char *filename, const int mode,
			 const int sharing);
static int indexed_close (cob_file *f, const int opt);
static int indexed_start (cob_file *f, const int cond, cob_field *key);
static int indexed_read (cob_file *f, cob_field *key, const int read_opts);
static int indexed_read_next (cob_file *f, const int read_opts);
static int indexed_write (cob_file *f, const int opt);
static int indexed_delete (cob_file *f);
static int indexed_rewrite (cob_file *f, const int opt);

#if	!defined(WITH_INDEX_EXTFH) && !defined(WITH_ANY_ISAM)
static int indexed_write_internal (cob_file *f, const int rewrite, const int opt);
static int indexed_delete_internal (cob_file *f, const int rewrite);
#endif	/* WITH_INDEX_EXTFH */

static const struct cob_fileio_funcs indexed_funcs = {
	indexed_open,
	indexed_close,
	indexed_start,
	indexed_read,
	indexed_read_next,
	indexed_write,
	indexed_rewrite,
	indexed_delete
};

#else	/* WITH_DB || WITH_ANY_ISAM || WITH_INDEX_EXTFH */

static int
dummy_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_write_close (cob_file *f, const int opt)
{
	return COB_STATUS_91_NOT_AVAILABLE;
}


static struct cob_fileio_funcs indexed_funcs = {
	dummy_open,
	dummy_write_close,
	dummy_start,
	dummy_read,
	dummy_rnxt_del,
	dummy_write_close,
	dummy_rewrite,
	dummy_rnxt_del
};

#endif	/* WITH_DB || WITH_ANY_ISAM || WITH_INDEX_EXTFH */


static const struct cob_fileio_funcs sequential_funcs = {
	cob_file_open,
	cob_file_close,
	dummy_start,
	dummy_read,
	sequential_read,
	sequential_write,
	sequential_rewrite,
	dummy_rnxt_del
};

static const struct cob_fileio_funcs lineseq_funcs = {
	cob_file_open,
	cob_file_close,
	dummy_start,
	dummy_read,
	lineseq_read,
	lineseq_write,
	dummy_rewrite,
	dummy_rnxt_del
};

static const struct cob_fileio_funcs relative_funcs = {
	cob_file_open,
	cob_file_close,
	relative_start,
	relative_read,
	relative_read_next,
	relative_write,
	relative_rewrite,
	relative_delete
};

static const struct cob_fileio_funcs	*fileio_funcs[COB_ORG_MAX] = {
	&sequential_funcs,
	&lineseq_funcs,
	&relative_funcs,
	&indexed_funcs,
	NULL
};

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
extern void	extfh_cob_init_fileio	(const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					int (*)(cob_file *, const int));
extern void	extfh_cob_exit_fileio	(void);
#endif

#ifdef	WITH_INDEX_EXTFH
extern void extfh_indexed_unlock	(cob_file *);
extern int extfh_indexed_locate		(cob_file *, char *);
extern int extfh_indexed_open		(cob_file *, char *, int, int);
extern int extfh_indexed_close		(cob_file *, int);
extern int extfh_indexed_start		(cob_file *, int, cob_field *);
extern int extfh_indexed_read		(cob_file *, cob_field *, int);
extern int extfh_indexed_read_next	(cob_file *, int);
extern int extfh_indexed_write		(cob_file *, int);
extern int extfh_indexed_delete		(cob_file *);
extern int extfh_indexed_rewrite	(cob_file *, int);
#endif

#ifdef	WITH_SEQRA_EXTFH
extern void extfh_seqra_unlock		(cob_file *);
extern int extfh_seqra_locate		(cob_file *, char *);
extern int extfh_cob_file_open		(cob_file *, char *, int, int);
extern int extfh_cob_file_close		(cob_file *, int);
extern int extfh_sequential_read	(cob_file *, int);
extern int extfh_sequential_write	(cob_file *, int);
extern int extfh_sequential_rewrite	(cob_file *, int);
extern int extfh_relative_start		(cob_file *, int, cob_field *);
extern int extfh_relative_read		(cob_file *, cob_field *, int);
extern int extfh_relative_read_next	(cob_file *, int);
extern int extfh_relative_write		(cob_file *, int);
extern int extfh_relative_rewrite	(cob_file *, int);
extern int extfh_relative_delete	(cob_file *);
#endif

#if	defined(WITH_ANY_ISAM) 
/* Isam File handler packet */

struct indexfile {
	char		*filename;	/* ISAM data file name */
	char		*savekey;	/* Area to save last Prime Key read */
	char		*recwrk;	/* Record work/save area */
	int		isfd;		/* ISAM file number */
	int		recnum;		/* last record number read */
	int		saverecnum;	/* isrecnum of next record to process */
	int		saveerrno;	/* savefileposition errno */
	int		lmode;		/* File lock mode for 'isread' */
	int		curkey;		/* Current active index */
	int		startcond;	/* Previous 'start' condition value */
	int		readdir;	/* read direction: ISPREV or ISNEXT */
	int		nkeys;		/* Actual keys in file */
	int		lenkey;		/* Length of savekey area */
	int		eofpending;	/* end of file pending */
	int		readdone;	/* A 'read' has been succesfully done */
	int		startiscur;	/* The 'start' record is current */
	int		keyhasdups;	/* 'curkey' has dups */
	int		wrkhasrec;	/* 'recwrk' buffer holds the next|prev record */
	struct keydesc	key[1];		/* Table of key information */
					/* keydesc is defined in (d|c|vb)isam.h */
};

/* Translate ISAM status to COBOL status and return */

static int COB_NOINLINE
isretsts (int dfltsts)
{
	switch (iserrno) {
	case 0:
		dfltsts = COB_STATUS_00_SUCCESS;
		break;
	case ENOREC:
		dfltsts = COB_STATUS_23_KEY_NOT_EXISTS;
		break;
	case EENDFILE:
		dfltsts = COB_STATUS_10_END_OF_FILE;
		break;
	case EPERM:
		dfltsts = COB_STATUS_37_PERMISSION_DENIED;
		break;
	case EACCES:
		dfltsts = COB_STATUS_37_PERMISSION_DENIED;
		break;
	case EISDIR:
		dfltsts = COB_STATUS_37_PERMISSION_DENIED;
		break;
	case EDUPL:
		dfltsts = COB_STATUS_22_KEY_EXISTS;
		break;
	case EKEXISTS:
		dfltsts = COB_STATUS_22_KEY_EXISTS;
		break;
	case ENOENT:
		dfltsts = COB_STATUS_35_NOT_EXISTS;
		break;
	case ENOCURR:
		if (dfltsts != COB_STATUS_10_END_OF_FILE) {
			dfltsts = COB_STATUS_21_KEY_INVALID;
		}
		break;
	case ELOCKED:
		dfltsts = COB_STATUS_51_RECORD_LOCKED;
		break;
	case EFLOCKED:
		dfltsts = COB_STATUS_61_FILE_SHARING;
		break;
	}
	return dfltsts;
}

/* Free memory for indexfile packet */

static void COB_NOINLINE
freefh (struct indexfile *fh)
{
	if (fh == NULL) {
		return;
	}
	if (fh->filename) {
		free ((void *)fh->filename);
	}
	if (fh->savekey) {
		free ((void *)fh->savekey);
	}
	if (fh->recwrk) {
		free ((void *)fh->recwrk);
	}
	free ((void *)fh);
}

/*
	Restore ISAM file positioning
*/
static void
restorefileposition (cob_file *f)
{
	struct indexfile	*fh = f->file;
	struct keydesc		k0;

	memset ((void *)&k0, 0, sizeof(k0));
	if (fh->saverecnum >= 0) {			/* Switch back to index */
		isrecnum = fh->saverecnum;
		isstart (fh->isfd, &k0, 0, fh->recwrk, ISEQUAL); /* Switch to recnum mode */
		isread (fh->isfd, fh->recwrk, ISEQUAL);		/* Read by record number */
		isstart (fh->isfd, &fh->key[fh->curkey], fh->key[fh->curkey].k_leng, fh->recwrk, ISEQUAL);
		isread (fh->isfd, fh->recwrk, ISEQUAL);
		while (isrecnum != fh->saverecnum) {		/* Read back into position */
			if (isread (fh->isfd, fh->recwrk, fh->readdir) == -1) {
				break;
			}
		}
		if (isrecnum == fh->saverecnum) {
			if (fh->readdir == ISNEXT) {		/* Back off by one so next read gets this */
				isread (fh->isfd, fh->recwrk, ISPREV);
			} else {
				isread (fh->isfd, fh->recwrk, ISNEXT);
			}
		}
	} else if (fh->readdone && fh->curkey == 0) {
		/* Original BCS/JR patch: extract_key(fh, 0, fh->recwrk, fh->savekey); */
		memcpy (fh->recwrk + fh->key[fh->curkey].k_start, fh->savekey,
			fh->key[fh->curkey].k_leng);
		isstart (fh->isfd, &fh->key[fh->curkey], fh->key[fh->curkey].k_leng, fh->recwrk, ISGTEQ);
	}
}

/* Save ISAM file positioning information for later 'restorefileposition' */

static void
savefileposition (cob_file *f)
{
	struct indexfile	*fh = f->file;

	if (fh->curkey >= 0 && fh->readdir != -1) {	/* Switch back to index */
		if (fh->wrkhasrec != fh->readdir) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			if (isread (fh->isfd, fh->recwrk, fh->readdir) == -1) {	/* Read next record in file */
				fh->saverecnum = -1;
				fh->saveerrno = iserrno;
				if (fh->saveerrno == EENDFILE || fh->saveerrno == ENOREC)  {
					fh->eofpending = fh->readdir;
				}
			} else {
				fh->saverecnum = isrecnum;
				fh->saveerrno = 0;
			}
			memcpy (fh->recwrk, f->record->data, f->record_max);	/* Restore saved record data */
		}
	} else {
		fh->saverecnum = -1;
	}
}
#endif /* WITH_ANY_ISAM */

static void COB_NOINLINE
cob_sync (cob_file *f, const int mode)
{
#ifdef	WITH_DB
	struct indexed_file	*p;
	size_t			i;
#ifdef	USE_DB41
	int			n;
#endif
#elif	defined(WITH_ANY_ISAM) 
	struct indexfile	*fh;
#endif

	if (f->organization == COB_ORG_INDEXED) {
#ifdef	WITH_DB
		p = f->file;
		for (i = 0; i < f->nkeys; i++) {
			if (p->db[i]) {
				DB_SYNC (p->db[i]);
			}
		}
		if (mode == 2) {
			for (i = 0; i < f->nkeys; i++) {
				if (p->db[i]) {
#ifdef	USE_DB41
					fsync (p->db[i]->fd (p->db[i], &n));
#else
					fsync (p->db[i]->fd (p->db[i]));
#endif
				}
			}
		}
#elif	defined(WITH_ANY_ISAM)
		fh = f->file;
		if (fh) {
			isflush (fh->isfd);
		}
#endif
		return;
	}
	if (f->organization != COB_ORG_SORT) {
		fflush ((FILE *)f->file);
		if (mode == 2) {
			fsync (fileno ((FILE *)f->file));
		}
	}
}

static void
cob_cache_file (cob_file *f)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (f == l->file) {
			return;
		}
	}
	l = cob_malloc (sizeof (struct file_list));
	l->file = f;
	l->next = file_cache;
	file_cache = l;
}

static void
save_status (cob_file *f, const int status, cob_field *fnstatus)
{
	cob_error_file = f;
	if (likely(status == 0)) {
		f->file_status[0] = (unsigned char)'0';
		f->file_status[1] = (unsigned char)'0';
		if (fnstatus) {
			fnstatus->data[0] = (unsigned char)'0';
			fnstatus->data[1] = (unsigned char)'0';
		}
		cob_exception_code = 0;
		return;
	}
	if (likely(status != COB_STATUS_52_EOP)) {
		cob_set_exception (status_exception[status / 10]);
	}
	f->file_status[0] = cob_i2d (status / 10);
	f->file_status[1] = cob_i2d (status % 10);
	if (fnstatus) {
		fnstatus->data[0] = f->file_status[0];
		fnstatus->data[1] = f->file_status[1];
	}
}

/* Regular file */

static size_t COB_NOINLINE
file_linage_check (cob_file *f)
{
	struct linage_struct    *lingptr;

	lingptr = (struct linage_struct *)(f->linorkeyptr);
	lingptr->lin_lines = cob_get_int (lingptr->linage);
	if (lingptr->lin_lines < 1) {
		goto linerr;
	}
	if (lingptr->latfoot) {
		lingptr->lin_foot = cob_get_int (lingptr->latfoot);
		if (lingptr->lin_foot < 1 || lingptr->lin_foot > lingptr->lin_lines) {
			goto linerr;
		}
	} else {
		lingptr->lin_foot = 0;
	}
	if (lingptr->lattop) {
		lingptr->lin_top = cob_get_int (lingptr->lattop);
		if (lingptr->lin_top < 0) {
			goto linerr;
		}
	} else {
		lingptr->lin_top = 0;
	}
	if (lingptr->latbot) {
		lingptr->lin_bot = cob_get_int (lingptr->latbot);
		if (lingptr->lin_bot < 0) {
			goto linerr;
		}
	} else {
		lingptr->lin_bot = 0;
	}
	return 0;
linerr:
	cob_set_int (lingptr->linage_ctr, 0);
	return 1;
}

static int
dummy_rnxt_del (cob_file *f)
{
	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_rewrite (cob_file *f, const int opt)
{
	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_read (cob_file *f, cob_field *key, const int read_opts)
{
	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_start (cob_file *f, const int cond, cob_field *key)
{
	return COB_STATUS_91_NOT_AVAILABLE;
}

static int COB_NOINLINE
cob_file_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	FILE			*fp = NULL;
	struct linage_struct    *lingptr;
#ifdef HAVE_FCNTL
	int			ret;
	struct flock		lock;
#endif

	/* open the file */
	switch (mode) {
	case COB_OPEN_INPUT:
#if !defined(_WIN32) || defined(_MSC_VER)
		if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "r");
		else
#endif
			fp = fopen (filename, "rb");
		break;
	case COB_OPEN_OUTPUT:
		if (f->organization == COB_ORG_RELATIVE)
			fp = fopen (filename, "wb+");
#if !defined(_WIN32) || defined(_MSC_VER)
		else if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "w");
#endif
		else
			fp = fopen (filename, "wb");
		break;
	case COB_OPEN_I_O:
#if !defined(_WIN32) || defined(_MSC_VER)
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			fp = fopen (filename, "r+");
			if ((fp == NULL) && (errno == ENOENT) && cob_check_env (COB_IO_CREATES, "yes")) {
				fp = fopen (filename, "w+");
 			}
		} else {
#else
		{
#endif
			fp = fopen (filename, "rb+");
			if ((fp == NULL) && (errno == ENOENT) && cob_check_env (COB_IO_CREATES, "yes")) {
				fp = fopen (filename, "wb+");
			}
		}
		break;
	case COB_OPEN_EXTEND:
#if !defined(_WIN32) || defined(_MSC_VER)
		if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "a+");
		else
#endif
			fp = fopen (filename, "ab+");
		break;
	}
	if (fp == NULL) {
		return errno;
	}

	if (mode == COB_OPEN_EXTEND) {
		fseek (fp, (off_t) 0, SEEK_END);
	}

#ifdef HAVE_FCNTL
	/* lock the file */
	if (memcmp (filename, "/dev/", 5)) {
		memset ((unsigned char *)&lock, 0, sizeof (struct flock));
		lock.l_type = (sharing || mode == COB_OPEN_OUTPUT) ? F_WRLCK : F_RDLCK;
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		if (fcntl (fileno (fp), F_SETLK, &lock) < 0) {
			ret = errno;
			fclose (fp);
			return ret;
		}
	}
#endif

	f->file = fp;
	if (unlikely(f->flag_select_features & COB_SELECT_LINAGE)) {
		if (file_linage_check (f)) {
			return COB_LINAGE_INVALID;
		}
		f->flag_needs_top = 1;
		lingptr = (struct linage_struct *)(f->linorkeyptr);
		cob_set_int (lingptr->linage_ctr, 1);
	}
	return 0;
}

static int COB_NOINLINE
cob_file_close (cob_file *f, const int opt)
{
#ifdef HAVE_FCNTL
	struct flock lock;
#endif

	switch (opt) {
	case COB_CLOSE_NORMAL:
	case COB_CLOSE_LOCK:
	case COB_CLOSE_NO_REWIND:
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->flag_needs_nl && !(f->flag_select_features & COB_SELECT_LINAGE)) {
				f->flag_needs_nl = 0;
				putc ('\n', (FILE *)f->file);
			}
		}
#ifdef HAVE_FCNTL
		/* unlock the file */
		memset ((unsigned char *)&lock, 0, sizeof (struct flock));
		lock.l_type = F_UNLCK;
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		fcntl (fileno ((FILE *)f->file), F_SETLK, &lock);
#endif
		/* close the file */
		fclose ((FILE *)f->file);
		if (opt == COB_CLOSE_NO_REWIND) {
			f->open_mode = COB_OPEN_CLOSED;
			return COB_STATUS_07_SUCCESS_NO_UNIT;
		}
		return COB_STATUS_00_SUCCESS;
	default:
		fflush ((FILE *)f->file);
		return COB_STATUS_07_SUCCESS_NO_UNIT;
	}
}

static int COB_NOINLINE
cob_linage_write_opt (cob_file *f, const int opt)
{
	struct linage_struct    *lingptr;
	int			i, n;

	lingptr = (struct linage_struct *)(f->linorkeyptr);
	if (unlikely(opt & COB_WRITE_PAGE)) {
		i = cob_get_int (lingptr->linage_ctr);
		if (i == 0) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		n = lingptr->lin_lines;
		for (; i < n; i++) {
			putc ('\n', (FILE *)f->file);
		}
		for (i = 0; i < lingptr->lin_bot; i++) {
			putc ('\n', (FILE *)f->file);
		}
		if (file_linage_check (f)) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		for (i = 0; i < lingptr->lin_top; i++) {
			putc ('\n', (FILE *)f->file);
		}
		cob_set_int (lingptr->linage_ctr, 1);
	} else if (opt & COB_WRITE_LINES) {
		n = cob_get_int (lingptr->linage_ctr);
		if (n == 0) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		cob_add_int (lingptr->linage_ctr, opt & COB_WRITE_MASK);
		i = cob_get_int (lingptr->linage_ctr);
		if ((opt & COB_WRITE_EOP) && lingptr->lin_foot) {
			if (i >= lingptr->lin_foot) {
				eop_status = 1;
			}
		}
		if (i > lingptr->lin_lines) {
			if (opt & COB_WRITE_EOP) {
				eop_status = 1;
			}
			for (; n < lingptr->lin_lines; n++) {
				putc ('\n', (FILE *)f->file);
			}
			for (i = 0; i < lingptr->lin_bot; i++) {
				putc ('\n', (FILE *)f->file);
			}
			if (file_linage_check (f)) {
				return COB_STATUS_57_I_O_LINAGE;
			}
			cob_set_int (lingptr->linage_ctr, 1);
			for (i = 0; i < lingptr->lin_top; i++) {
				putc ('\n', (FILE *)f->file);
			}
		} else {
			for (i = (opt & COB_WRITE_MASK) - 1; i > 0; i--)
				putc ('\n', (FILE *)f->file);
		}
	}
	return 0;
}

static int
cob_file_write_opt (cob_file *f, const int opt)
{
	int	i;

	if (unlikely(f->flag_select_features & COB_SELECT_LINAGE)) {
		return cob_linage_write_opt (f, opt);
	}
	if (opt & COB_WRITE_LINES) {
		for (i = opt & COB_WRITE_MASK; i > 0; i--)
			putc ('\n', (FILE *)f->file);
	} else if (opt & COB_WRITE_PAGE) {
		putc ('\f', (FILE *)f->file);
	}
	return 0;
}

/* SEQUENTIAL */

static int
sequential_read (cob_file *f, const int read_opts)
{
	size_t	bytesread;

#if	WITH_VARSEQ == 0 || WITH_VARSEQ == 1 || WITH_VARSEQ == 3
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;
#endif
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	SEEK_INIT (f);

	/* read the record size */
	if (f->record_min != f->record_max) {
#if	WITH_VARSEQ == 2
		if (unlikely(fread (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1)) {
#elif	WITH_VARSEQ == 3
		if (unlikely(fread (recsize.sbuff, 2, 1, (FILE *)f->file) != 1)) {
#else
		if (unlikely(fread (recsize.sbuff, 4, 1, (FILE *)f->file) != 1)) {
#endif
			if (ferror ((FILE *)f->file)) {
				return COB_STATUS_30_PERMANENT_ERROR;
			} else {
				return COB_STATUS_10_END_OF_FILE;
			}
		}
#if	WITH_VARSEQ == 0 || WITH_VARSEQ == 3
#ifdef WORDS_BIGENDIAN
		f->record->size = recsize.sshort[0];
#else
		f->record->size = COB_BSWAP_16 (recsize.sshort[0]);
#endif
#elif	WITH_VARSEQ == 1
#ifdef WORDS_BIGENDIAN
		f->record->size = recsize.sint;
#else
		f->record->size = COB_BSWAP_32 (recsize.sint);
#endif
#endif
	}

	/* read the record */
	bytesread = fread (f->record->data, 1, f->record->size, (FILE *)f->file);
	if (unlikely(bytesread != f->record->size)) {
		if (ferror ((FILE *)f->file)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		} else if (bytesread == 0) {
			return COB_STATUS_10_END_OF_FILE;
		} else {
			return COB_STATUS_04_SUCCESS_INCOMPLETE;
		}
	}
	return COB_STATUS_00_SUCCESS;
}

static int
sequential_write (cob_file *f, const int opt)
{
	int	ret;

#if	WITH_VARSEQ == 0 || WITH_VARSEQ == 1 || WITH_VARSEQ == 3
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;
#endif

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	SEEK_INIT (f);

	/* WRITE AFTER */
	if (opt & COB_WRITE_AFTER) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 1;
	}

	/* write the record size */
	if (f->record_min != f->record_max) {
#if	WITH_VARSEQ == 2
		if (unlikely(fwrite (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1)) {
#else	/* VARSEQ 0, 1, 3 */
#if	WITH_VARSEQ == 1
#ifdef WORDS_BIGENDIAN
		recsize.sint = f->record->size;
#else
		recsize.sint = COB_BSWAP_32 ((unsigned int)f->record->size);
#endif
#else	/* VARSEQ 0, 3 */
		recsize.sint = 0;
#ifdef WORDS_BIGENDIAN
		recsize.sshort[0] = f->record->size;
#else
		recsize.sshort[0] = COB_BSWAP_16 ((unsigned short)f->record->size);
#endif
#endif	/* VARSEQ 0, 3 */
#if	WITH_VARSEQ == 3
		if (unlikely(fwrite (recsize.sbuff, 2, 1, (FILE *)f->file) != 1)) {
#else
		if (unlikely(fwrite (recsize.sbuff, 4, 1, (FILE *)f->file) != 1)) {
#endif	/* VARSEQ 3 */
#endif	/* VARSEQ 0, 1, 3 */
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	/* write the record */
	if (unlikely(fwrite (f->record->data, f->record->size, 1, (FILE *)f->file) != 1)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	/* WRITE BEFORE */
	if (opt & COB_WRITE_BEFORE) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 0;
	}

	return COB_STATUS_00_SUCCESS;
}

static int
sequential_rewrite (cob_file *f, const int opt)
{
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */
	if (fseek ((FILE *)f->file, -(off_t) f->record->size, SEEK_CUR)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (fwrite (f->record->data, f->record->size, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

/*
 * LINE SEQUENTIAL
 */

static int
lineseq_read (cob_file *f, const int read_opts)
{
	unsigned char	*dataptr;
	size_t		i = 0;
	int		n;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	dataptr = f->record->data;
	for (; ;) {
		n = getc ((FILE *)f->file);
		if (unlikely(n == EOF)) {
			if (!i) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				break;
			}
		}
		if (unlikely(n == 0 && cob_ls_nulls != NULL)) {
			n = getc ((FILE *)f->file);
			if (n == EOF) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		} else {
			if (n == '\r') {
				continue;
			}
			if (n == '\n') {
				break;
			}
		}
		if (likely(i < f->record->size)) {
			*dataptr++ = n;
			i++;
		}
	}
	if (i < f->record->size) {
		/* fill the record with spaces */
		memset ((unsigned char *)f->record->data + i, ' ', f->record->size - i);
	}
	if (f->record_size) {
		cob_set_int (f->record_size, (int)i);
	}
	return COB_STATUS_00_SUCCESS;
}

static int
lineseq_write (cob_file *f, const int opt)
{
	unsigned char		*p;
	struct linage_struct    *lingptr;
	size_t			size;
	int			i;
	int			ret;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

/* RXW
	if (opt == 0) {
		opt = COB_WRITE_BEFORE | COB_WRITE_LINES | 1;
	}
*/

	/* determine the size to be written */
	if (unlikely(cob_ls_fixed != NULL)) {
		size = f->record->size;
	} else {
		for (i = (int)f->record->size - 1; i >= 0; i--) {
			if (f->record->data[i] != ' ') {
				break;
			}
		}
		size = i + 1;
	}

	if (unlikely(f->flag_select_features & COB_SELECT_LINAGE)) {
		if (f->flag_needs_top) {
			f->flag_needs_top = 0;
			lingptr = (struct linage_struct *)(f->linorkeyptr);
			for (i = 0; i < lingptr->lin_top; i++) {
				putc ('\n', (FILE *)f->file);
			}
		}
	}
	/* WRITE AFTER */
	if (opt & COB_WRITE_AFTER) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 1;
	}

	/* write to the file */
	if (size) {
		if (unlikely(cob_ls_nulls != NULL)) {
			p = f->record->data;
			for (i = 0; i < (int)size; i++, p++) {
				if (*p < ' ') {
					putc (0, (FILE *)f->file);
				}
				putc ((int)(*p), (FILE *)f->file);
			}
		} else {
			if (unlikely(fwrite (f->record->data, size, 1,
				     (FILE *)f->file) != 1)) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	if (unlikely(f->flag_select_features & COB_SELECT_LINAGE)) {
		putc ('\n', (FILE *)f->file);
	}

	/* WRITE BEFORE */
	if (opt & COB_WRITE_BEFORE) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 0;
	}

	if (f->flag_needs_nl && !unlikely(f->flag_select_features & COB_SELECT_LINAGE)) {
		putc ('\n', (FILE *)f->file);
		f->flag_needs_nl = 0;
	}

	if (unlikely(eop_status)) {
		eop_status = 0;
		cob_exception_code = 0x0502;
		return COB_STATUS_52_EOP;
	}
	return COB_STATUS_00_SUCCESS;
}

/*
 * RELATIVE
 */

static int
relative_start (cob_file *f, const int cond, cob_field *k)
{
	int	kindex;
	size_t	relsize;
	off_t	off;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_start (f, cond, k);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	/* get the index */
	kindex = cob_get_int (k) - 1;
	relsize = f->record_max + sizeof (f->record->size);
	if (cond == COB_LT) {
		kindex--;
	} else if (cond == COB_GT) {
		kindex++;
	}

	/* seek the index */
	for (;;) {
		off = kindex * relsize;
		if (fseek ((FILE *)f->file, off, SEEK_SET) != 0 ||
		    fread (&f->record->size, sizeof (f->record->size),
			   1, (FILE *)f->file) != 1) {
				return COB_STATUS_23_KEY_NOT_EXISTS;
		}

		/* check if a valid record */
		if (f->record->size > 0) {
			cob_set_int (k, kindex + 1);
			fseek ((FILE *)f->file, - (off_t) sizeof (f->record->size), SEEK_CUR);
			return COB_STATUS_00_SUCCESS;
		}

		/* continue */
		switch (cond) {
		case COB_EQ:
			return COB_STATUS_23_KEY_NOT_EXISTS;
		case COB_LT:
		case COB_LE:
			kindex--;
			break;
		case COB_GT:
		case COB_GE:
			kindex++;
			break;
		}
	}
}

static int
relative_read (cob_file *f, cob_field *k, const int read_opts)
{
	int	relnum;
	size_t	relsize;
	off_t	off;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_read (f, k, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	SEEK_INIT (f);

	relnum = cob_get_int (k) - 1;
	relsize = f->record_max + sizeof (f->record->size);
	off = relnum * relsize;
	if (fseek ((FILE *)f->file, off, SEEK_SET) != 0 ||
	    fread (&f->record->size, sizeof (f->record->size),
		   1, (FILE *)f->file) != 1) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (f->record->size == 0) {
		fseek ((FILE *)f->file, - (off_t) sizeof (f->record->size), SEEK_CUR);
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (fread (f->record->data, f->record_max, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_read_next (cob_file *f, const int read_opts)
{
	off_t	off;
	size_t	relsize;
	int	relnum;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_read_next (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	SEEK_INIT (f);

	relsize = f->record_max + sizeof (f->record->size);
	for (;;) {
		if (fread (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1) {
			if (ferror ((FILE *)f->file)) {
				return COB_STATUS_30_PERMANENT_ERROR;
			} else {
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (f->keys[0].field) {
			if (f->flag_first_read) {
				cob_set_int (f->keys[0].field, 1);
				f->flag_first_read = 0;
			} else {
				off = ftell ((FILE *)f->file);
				relnum = (int)((off / relsize) + 1);
				cob_set_int (f->keys[0].field, 0);
				if (cob_add_int (f->keys[0].field, relnum) != 0) {
					fseek ((FILE *)f->file, -(off_t) sizeof (f->record->size),
					       SEEK_CUR);
					return COB_STATUS_14_OUT_OF_KEY_RANGE;
				}
			}
		}

		if (f->record->size > 0) {
			if (fread (f->record->data, f->record_max, 1, (FILE *)f->file) != 1) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			return COB_STATUS_00_SUCCESS;
		}

		fseek ((FILE *)f->file, (off_t) f->record_max, SEEK_CUR);
	}
}

static int
relative_write (cob_file *f, const int opt)
{
	size_t	size;
	size_t	relsize;
	int	i;
	int	kindex;
	off_t	off;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	SEEK_INIT (f);

	relsize = f->record_max + sizeof (f->record->size);
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		kindex = cob_get_int (f->keys[0].field) - 1;
		if (kindex < 0) {
			return COB_STATUS_21_KEY_INVALID;
		}
		off = (off_t) (relsize * kindex);
		if (fseek ((FILE *)f->file, off, SEEK_SET) != 0) {
			return COB_STATUS_21_KEY_INVALID;
		}
	} else {
		off = ftell ((FILE *)f->file);
	}

	if (fread (&size, sizeof (size), 1, (FILE *)f->file) > 0) {
		fseek ((FILE *)f->file, -(off_t) sizeof (size), SEEK_CUR);
		if (size > 0) {
			return COB_STATUS_22_KEY_EXISTS;
		}
	} else {
		fseek ((FILE *)f->file, off, SEEK_SET);
	}

	if (fwrite (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (fwrite (f->record->data, f->record_max, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	/* update RELATIVE KEY */
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->keys[0].field) {
/*
			off = ftell ((FILE *)f->file);
*/
			off += relsize;
			i = (int)(off / relsize);
			cob_set_int (f->keys[0].field, i);
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
relative_rewrite (cob_file *f, const int opt)
{
	size_t	relsize;
	int	relnum;
	off_t	off;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		fseek ((FILE *)f->file, -(off_t) f->record_max, SEEK_CUR);
	} else {
		relsize = f->record_max + sizeof (f->record->size);
		relnum = cob_get_int (f->keys[0].field) - 1;
		off = relnum * relsize;
		if (fseek ((FILE *)f->file, off, SEEK_SET) != 0 ||
		    fread (&f->record->size, sizeof (f->record->size),
			   1, (FILE *)f->file) != 1) {
				return COB_STATUS_23_KEY_NOT_EXISTS;
		}
		SEEK_INIT (f);
	}

	if (fwrite (f->record->data, f->record_max, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_delete (cob_file *f)
{
	size_t	relsize;
	int	relnum;
	off_t	off;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_delete (f);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif	/* WITH_SEQRA_EXTFH */

	relnum = cob_get_int (f->keys[0].field) - 1;
	relsize = f->record_max + sizeof (f->record->size);
	off = relnum * relsize;
	if (fseek ((FILE *)f->file, off, SEEK_SET) != 0 ||
	    fread (&f->record->size, sizeof (f->record->size),
		   1, (FILE *)f->file) != 1) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	fseek ((FILE *)f->file, - (off_t) sizeof (f->record->size), SEEK_CUR);

	f->record->size = 0;
	if (fwrite (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	fseek ((FILE *)f->file, (off_t) f->record_max, SEEK_CUR);
	return COB_STATUS_00_SUCCESS;
}

/*
 * INDEXED
 */

#if	defined(WITH_DB) || defined(WITH_INDEX_EXTFH) || defined(WITH_ANY_ISAM)

#ifdef	USE_DB41
static void
join_environment (void)
{
	int		flags, ret;

	if (bdb_home == NULL) {
		return;
	}
	ret = db_env_create (&bdb_env, 0);
	if (ret) {
		cob_runtime_error ("Can't join BDB environment, env_create: %d %s\n", ret, db_strerror (ret));
		cob_stop_run (1);
	}
	bdb_env->set_errfile (bdb_env, stderr);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgfile (bdb_env, stderr);
#endif
	bdb_env->set_cachesize (bdb_env, 0, 2*1024*1024, 0);
	bdb_env->set_alloc (bdb_env, cob_malloc, realloc, free);
	flags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_CDB;
	ret = bdb_env->open (bdb_env, bdb_home, flags, 0);
	if (ret) {
		cob_runtime_error ("Can't join BDB environment, env_open: %d %s\n", ret, db_strerror (ret));
		bdb_env->close (bdb_env, 0);
		bdb_env = NULL;
		cob_stop_run (1);
	}
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 1))
	bdb_env->get_data_dirs (bdb_env, &bdb_data_dir);
#endif
	bdb_env->lock_id (bdb_env, &bdb_lock_id);
}

static int
lock_record (cob_file *f, char *key, const unsigned int keylen)
{
	struct indexed_file	*p = f->file;
	size_t			len;
	int			ret;
	DBT			dbt;
	
	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename, (size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key, (size_t)keylen);
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
	ret = bdb_env->lock_get (bdb_env, p->bdb_lock_id, DB_LOCK_NOWAIT, 
				&dbt, DB_LOCK_WRITE, &p->bdb_record_lock);
	if (!ret) {
		p->record_locked = 1;
	}
	return ret;
}

static int
test_record_lock (cob_file *f, char *key, const unsigned int keylen)
{
	struct indexed_file	*p = f->file;
	size_t			len;
	int			ret;
	DBT			dbt;
	DB_LOCK			test_lock;
	
	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename, (size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key, (size_t)keylen);
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
	ret = bdb_env->lock_get (bdb_env, p->bdb_lock_id, DB_LOCK_NOWAIT, 
				&dbt, DB_LOCK_WRITE, &test_lock);
	if (!ret) {
		bdb_env->lock_put (bdb_env, &test_lock);
	}
	return ret;
}

static int
unlock_record (cob_file *f)
{
	struct indexed_file	*p = f->file;
	int			ret;
	
	if (p->record_locked == 0) {
		return 0;
	}
	ret = bdb_env->lock_put (bdb_env, &p->bdb_record_lock);
	p->record_locked = 0;
	return ret;
}
#endif	/* USE_DB41 */


/* OPEN the INDEXED file */

static int
indexed_open (cob_file *f, char *filename, const int mode, const int sharing)
{
#ifdef	WITH_INDEX_EXTFH
	return extfh_indexed_open (f, filename, mode, sharing);
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh;
	int			ret = COB_STATUS_00_SUCCESS;
	int			omode = 0;
	int			lmode = 0;
	int			vmode = 0;
	int			dobld = 0;
	int			isfd = -1;
	int			k;
	int			kp;
	struct dictinfo		di;			/* defined in (c|d|vb)isam.h */

#if defined(ISVARLEN)
	if (f->record_min != f->record_max) {
		vmode = ISVARLEN;
		isreclen = f->record_min;
	}
#endif
	if (!f->lock_mode) {
		if (mode != COB_OPEN_INPUT) {
			lmode = ISEXCLLOCK;
		} else {
			lmode = ISMANULOCK;
		}
	} else if ((f->lock_mode & COB_LOCK_EXCLUSIVE)) {
		lmode = ISEXCLLOCK;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) && mode != COB_OPEN_INPUT) {
		lmode = ISAUTOLOCK;
	} else {
		lmode = ISMANULOCK;
	}
	switch (mode) {
	case COB_OPEN_INPUT:
		omode = ISINPUT;
		break;
	case COB_OPEN_OUTPUT:
		lmode = ISEXCLLOCK;
		omode = ISOUTPUT;
		iserrno = 0;
		isfd = isopen (filename, ISINPUT | ISEXCLLOCK | vmode);
		if (iserrno == EFLOCKED) {
#ifdef	WITH_VBISAM
			isfullclose (isfd);
#else
			isclose (isfd);
#endif
			return COB_STATUS_61_FILE_SHARING;
		} else {
			if (isfd >= 0) {
#ifdef	WITH_VBISAM
				isfullclose (isfd);
#else
				isclose (isfd);
#endif
			}
			iserase (filename);
		}
		iserrno = 0;
		dobld = 1;
		break;
	case COB_OPEN_I_O:
		omode = ISINOUT;
		break;
	case COB_OPEN_EXTEND:
		lmode = ISEXCLLOCK;
		omode = ISINOUT;
		break;
	case COB_OPEN_LOCKED:
		lmode = ISEXCLLOCK;
		omode = ISINOUT;
		break;
	}
	fh = cob_malloc (sizeof(struct indexfile) + ((sizeof (struct keydesc)) * (f->nkeys + 1)));
	/* Copy index information */
	for (k = 0; k < f->nkeys; k++) {
		memset (&fh->key[k], 0, sizeof(struct keydesc));
		fh->key[k].k_flags = f->keys[k].flag ? ISDUPS : ISNODUPS;
		/* additional change to BCS/JR patch: put off the simple-key assamption. */
		if (!f->keys[k].count_components) {
			fh->key[k].k_nparts = 1;
			fh->key[k].k_start = f->keys[k].offset;
			fh->key[k].k_leng = f->keys[k].field->size;
			fh->key[k].k_type = CHARTYPE;
		}else{
			fh->key[k].k_nparts = f->keys[k].count_components;
			for (kp = 0; kp < f->keys[k].count_components; kp++) {
				fh->key[k].k_part[kp].kp_start = f->keys[k].component[kp].rb;
				fh->key[k].k_part[kp].kp_leng  = f->keys[k].component[kp].field->size;
				fh->key[k].k_part[kp].kp_type  = CHARTYPE;
			}
		}
		if (fh->lenkey < f->keys[k].field->size) {
			fh->lenkey = fh->key[k].k_leng;
		}
	}
	iserrno = 0;
	fh->lmode = 0;
	if (dobld) {
dobuild:
		isfd = isbuild (filename, f->record_max, &fh->key[0], ISINOUT | ISEXCLLOCK | vmode);
	} else {
		if (lmode == ISAUTOLOCK) {
			fh->lmode = ISLOCK;
			lmode = ISMANULOCK;
		}
		isfd = isopen (filename, omode | lmode | vmode);
		if (isfd == -1) {
			if (f->flag_optional) {
				if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_I_O) {
					dobld = 1;
					ret = COB_STATUS_05_SUCCESS_OPTIONAL;
					goto dobuild;
				}
				f->file = fh;
				f->open_mode = mode;
				fh->isfd = isfd;
				fh->filename = strdup (filename);
				/* Active index is unknown at this time */
				fh->curkey = -1;
				f->flag_end_of_file = 1;
				f->flag_begin_of_file = 1;
				if (f->flag_nonexistent) {
					return COB_STATUS_00_SUCCESS;
				}
				f->flag_nonexistent = 1;
				return COB_STATUS_05_SUCCESS_OPTIONAL;
			} else if (iserrno == ENOENT) {
				if ((mode == COB_OPEN_EXTEND && cob_check_env (COB_EXTEND_CREATES, "yes")) ||
				    (mode == COB_OPEN_I_O && cob_check_env (COB_IO_CREATES, "yes"))) {
					dobld = 1;
					goto dobuild;
				}
			}
		} else {
			memset(&di, 0, sizeof(di));
			isindexinfo (isfd, (void *)&di, 0);
			fh->nkeys = di.di_nkeys & 0x7F; /* Mask off ISVARLEN */
			if (fh->nkeys > f->nkeys) {
				fh = realloc (fh, sizeof(struct indexfile) + ((sizeof (struct keydesc)) * (fh->nkeys + 1)));
			}
			for (k = 0; k < fh->nkeys; k++) {
				memset (&fh->key[k], 0, sizeof(struct keydesc));
				isindexinfo (isfd, &fh->key[k], k+1);
				if (fh->lenkey < fh->key[k].k_leng) {
					fh->lenkey = fh->key[k].k_leng;
				}
				/* Verify that COBOL definition matches the real ISAM file */
				if (f->keys[k].flag) {
					if (!(fh->key[k].k_flags & ISDUPS)) {
						ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
					}
				} else {
					if (fh->key[k].k_flags & ISDUPS) {
						ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
					}
				}
				/* additional change to BCS/JR patch: put off the simple-key assamption. */
				if (fh->key[k].k_nparts == 1
				&& (fh->key[k].k_start != f->keys[k].offset
				||  fh->key[k].k_leng != f->keys[k].field->size)) {
					ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
				}
			}
		}
	}
	if (isfd == -1) {
		ret = isretsts (COB_STATUS_35_NOT_EXISTS);
		freefh (fh);
		return ret;
	}
	if (ret > 9) {
#ifdef	WITH_VBISAM
		isfullclose (isfd);
#else
		isclose (isfd);
#endif
		freefh (fh);
		return ret;
	}
	if (dobld) {
		for (k = 1; k < f->nkeys; k++) {
			iserrno = 0;
			if (isaddindex (isfd, &fh->key[k]) == -1) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
		}
		if (ret > 9) {
#ifdef	WITH_VBISAM
			isfullclose (isfd);
#else
			isclose (isfd);
#endif
			iserase (filename);
			freefh (fh);
			return ret;
		}
	}
	f->file = fh;
	f->open_mode = mode;
	fh->isfd = isfd;
	fh->filename = strdup (filename);
	fh->savekey = cob_malloc (fh->lenkey + 1);
	fh->recwrk = cob_malloc (f->record_max + 1);
	fh->curkey = -1;		/* Active index is unknown at this time */
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	return ret;
#else	/* WITH_INDEX_EXTFH */
	size_t			i, j;
#ifdef	USE_DB41
	int			flags = 0;
	int			lock_mode;
	int			handle_created;
#else
	int			flags = INITIAL_FLAGS;
	BTREEINFO		info;
#endif
	int			ret = 0;
	struct indexed_file	*p;
	size_t			maxsize;

	p = cob_malloc (sizeof (struct indexed_file));
#ifdef	USE_DB41
	if (bdb_env != NULL) {
		if (mode == COB_OPEN_OUTPUT || mode == COB_OPEN_EXTEND ||
		    (f->lock_mode & COB_LOCK_EXCLUSIVE) ||
		    (mode == COB_OPEN_I_O && !f->lock_mode)) {
			lock_mode = DB_LOCK_WRITE;
		} else {
			lock_mode = DB_LOCK_READ;
		}
		p->key.size = (cob_dbtsize_t) strlen (filename);
		p->key.data = filename;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, DB_LOCK_NOWAIT, 
					&p->key, lock_mode, &p->bdb_file_lock);
		if (ret) {
			free (p);
			if (ret == DB_LOCK_NOTGRANTED) {
				ret = COB_STATUS_61_FILE_SHARING;
			}
			return ret;
		}
	}
#endif

	switch (mode) {
	case COB_OPEN_INPUT:
#ifdef	USE_DB41
		flags |= DB_RDONLY;
#else
		flags |= O_RDONLY;
#endif
		break;
	case COB_OPEN_OUTPUT:
#ifdef	USE_DB41
		flags |= DB_CREATE;
#else
		flags |= O_RDWR | O_CREAT | O_TRUNC;
#endif
		break;
	case COB_OPEN_I_O:
	case COB_OPEN_EXTEND:
#ifdef	USE_DB41
		flags |= DB_CREATE;
#else
		flags |= O_RDWR | O_CREAT;
#endif
		break;
	}

	p->db = cob_malloc (sizeof (DB *) * f->nkeys);
#ifdef	USE_DB41
	p->cursor = cob_malloc (sizeof (DBC *) * f->nkeys);
	p->filenamelen = (int) strlen (filename);
#endif
	p->last_readkey = cob_malloc (sizeof (unsigned char *) * 2 * f->nkeys);
	p->last_dupno = cob_malloc (sizeof (unsigned int) * f->nkeys);
	p->rewrite_sec_key = cob_malloc (sizeof (int) * f->nkeys);
	maxsize = 0;
	for (i = 0; i < f->nkeys; i++) {
		if (f->keys[i].field->size > maxsize) {
			maxsize = f->keys[i].field->size;
		}
	}
	for (i = 0; i < f->nkeys; i++) {
		/* file name */
		memset (runtime_buffer, 0, COB_SMALL_BUFF);
		if (i == 0) {
			strncpy (runtime_buffer, filename, COB_SMALL_MAX);
		} else {
			snprintf (runtime_buffer, COB_SMALL_MAX, "%s.%d",
				 filename, (int)i);
		}

		/* btree info */
#ifdef	USE_DB41
		ret = db_create (&p->db[i], bdb_env, 0);
		if (!ret) {
			handle_created = 1;
			if (mode == COB_OPEN_OUTPUT) {
				if (bdb_env) {
					bdb_env->dbremove (bdb_env, NULL, runtime_buffer, NULL, 0);
				} else {
					p->db[i]->remove (p->db[i], runtime_buffer, NULL, 0);
					ret = db_create (&p->db[i], bdb_env, 0);
				}
			}
			if (!ret) {
				if (f->keys[i].flag) {
					p->db[i]->set_flags (p->db[i], DB_DUP);
				}
			}
		} else {
			handle_created = 0;
		}
#else
		memset ((unsigned char *)&info, 0, sizeof (info));
		if (f->keys[i].flag) {
			info.flags = R_DUP;
		}
#endif

		/* open db */
#ifdef	USE_DB41
		if (!ret) {
			ret = p->db[i]->open (p->db[i], NULL, runtime_buffer, NULL,
						DB_BTREE, flags, COB_FILE_MODE);
		}
#else
		p->db[i] = dbopen (runtime_buffer, flags, COB_FILE_MODE, DB_BTREE, &info);
		if (p->db[i] == 0) {
			ret = errno;
		}
#endif
		if (ret) {
			for (j = 0; j < i; j++) {
				DB_CLOSE (p->db[j]);
			}
#ifdef	USE_DB41
			if (handle_created) {
				DB_CLOSE (p->db[i]);
			}
#endif
			free (p->db);
			free (p->last_readkey);
			free (p->last_dupno);
#ifdef	USE_DB41
			free (p->cursor);
			if (bdb_env != NULL) {
				bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			}
#endif
			free (p);
			return ret;

		}

		p->last_readkey[i] = cob_malloc (maxsize);
		p->last_readkey[f->nkeys + i] = cob_malloc (maxsize);
	}

	p->temp_key = cob_malloc (maxsize + sizeof(unsigned int));
	f->file = p;
	p->key_index = 0;
	p->last_key = NULL;

	memset ((unsigned char *)&p->key, 0, sizeof (DBT));
	memset ((unsigned char *)&p->data, 0, sizeof (DBT));
#ifdef	USE_DB41
	p->filename = cob_malloc (strlen (filename) + 1);
	strcpy (p->filename, filename);
	p->write_cursor_open = 0;
	p->record_locked = 0;
	if (bdb_env != NULL) {
		bdb_env->lock_id (bdb_env, &p->bdb_lock_id);
	}

	DBT_SET (p->key, f->keys[0].field);
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	ret = DB_SEQ (p->cursor[0], DB_FIRST);
	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
#else
	ret = DB_SEQ (p->db[p->key_index], R_FIRST);
#endif
	if (!ret) {
		memcpy (p->last_readkey[0], p->key.data, p->key.size);
	} else {
		p->data.data = NULL;
	}

	return 0;
#endif	/* WITH_INDEX_EXTFH */
}

/* Close the INDEXED file */

static int
indexed_close (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH
	return extfh_indexed_close (f, opt);
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh = f->file;

	if (fh == NULL) {
		return COB_STATUS_00_SUCCESS;
	}
	if (fh->isfd >= 0) {
#ifdef	WITH_VBISAM
		isfullclose (fh->isfd);
#else
		isclose (fh->isfd);
#endif
	}
	freefh (fh);
	f->file = NULL;
	return COB_STATUS_00_SUCCESS;
#else	/* WITH_INDEX_EXTFH */
	struct indexed_file	*p = f->file;
	int			i;

	/* close DB's */
#ifdef	USE_DB41
	for (i = 0; i < (int)f->nkeys; i++) {
		if (p->cursor[i]) {
			p->cursor[i]->c_close (p->cursor[i]);
		}
	}
#endif
	for (i = f->nkeys - 1; i >= 0; i--) {
		if (p->db[i]) {
			DB_CLOSE (p->db[i]);
		}
		free (p->last_readkey[i]);
		free (p->last_readkey[f->nkeys + i]);
	}

	if (p->last_key) {
		free (p->last_key);
	}
	free (p->temp_key);
	free (p->db);
	free (p->last_readkey);
	free (p->last_dupno);
	free (p->rewrite_sec_key);
#ifdef	USE_DB41
	free (p->filename);
	free (p->cursor);
	if (bdb_env != NULL) {
		unlock_record (f);
		bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
		bdb_env->lock_id_free (bdb_env, p->bdb_lock_id);
	}
#endif
	free (p);

	return COB_STATUS_00_SUCCESS;
#endif	/* WITH_INDEX_EXTFH */
}

#if	!defined(WITH_INDEX_EXTFH) && !defined(WITH_ANY_ISAM)
static int
indexed_start_internal (cob_file *f, const int cond, cob_field *key, const int read_opts,
			const int test_lock)
{
	int			ret;
	unsigned int		dupno;
	struct indexed_file	*p = f->file;

	/* look up for the key */
	for (p->key_index = 0; p->key_index < f->nkeys; p->key_index++) {
		if (f->keys[p->key_index].field->data == key->data) {
			break;
		}
	}
/* RXW - Removed
	if (unlikely(p->key_index == f->nkeys)) {
		cob_runtime_error ("cob_start_indexed: key not found "
				   "(should have been detected by cobc)");
		return 99;
	}
*/

	/* search */
	DBT_SET (p->key, key);
#ifdef	USE_DB41
	/* the open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);
	ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
#else
	ret = DB_SEQ (p->db[p->key_index], R_CURSOR);
#endif
	switch (cond) {
	case COB_EQ:
		if (ret == 0) {
			ret = memcmp (p->key.data, key->data, key->size);
		}
		break;
	case COB_LT:
		if (ret != 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
#else
			ret = DB_SEQ (p->db[p->key_index], R_LAST);
#endif
		} else {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
#else
			ret = DB_SEQ (p->db[p->key_index], R_PREV);
#endif
		}
		break;
	case COB_LE:
		if (ret != 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
#else
			ret = DB_SEQ (p->db[p->key_index], R_LAST);
#endif
		} else if (memcmp (p->key.data, key->data, key->size) != 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
#else
			ret = DB_SEQ (p->db[p->key_index], R_PREV);
#endif
		} else if (f->keys[p->key_index].flag) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT_NODUP);
#else
			while (!ret && memcmp (p->key.data, key->data, key->size) == 0) {
				ret = DB_SEQ (p->db[p->key_index], R_NEXT);
			}
#endif
			if (ret != 0) {
#ifdef	USE_DB41
				ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
#else
				ret = DB_SEQ (p->db[p->key_index], R_LAST);
#endif
			} else {
#ifdef	USE_DB41
				ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
#else
				ret = DB_SEQ (p->db[p->key_index], R_PREV);
#endif
			}
		}
		break;
	case COB_GT:
		while (ret == 0 && memcmp (p->key.data, key->data, key->size) == 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
#else
			ret = DB_SEQ (p->db[p->key_index], R_NEXT);
#endif
		}
		break;
	case COB_GE:
		/* nothing */
		break;
	}

	if (ret == 0 && p->key_index > 0) {
		/* temporarily save alternate key */
		memcpy (p->temp_key, p->key.data, f->keys[p->key_index].field->size);
		if (f->keys[p->key_index].flag) {
			memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
		}
		p->key.data = p->data.data;
		p->key.size = f->keys[0].field->size;
		ret = DB_GET (p->db[0], 0);
	}

#ifdef	USE_DB41
	if (ret == 0 && test_lock) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			ret = test_record_lock (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = lock_record (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}
#endif

	if (ret == 0) {
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, f->keys[0].field->size);
		} else {
			memcpy (p->last_readkey[p->key_index],
				    p->temp_key, f->keys[p->key_index].field->size);
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, f->keys[0].field->size);
			if (f->keys[p->key_index].flag) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

#ifdef	USE_DB41
	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}
#endif

	return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}
#endif	/* WITH_INDEX_EXTFH */

/* START INDEXED file with positioning */

static int
indexed_start (cob_file *f, const int cond, cob_field *key)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_start (f, cond, key);

#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh = f->file;
	int			k;
	int			mode;
	int			klen;
	int			ret = COB_STATUS_00_SUCCESS;

	f->flag_read_done = 0;
	f->flag_first_read = 0;
	fh->readdone = 0;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	fh->keyhasdups = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	for (k = 0; k < f->nkeys; k++) {
		if (f->keys[k].field->data == key->data) {
			if (fh->key[k].k_flags & ISDUPS) {
				fh->keyhasdups = 1;
			}
			break;
		}
	}
	/* Use size of data field; This may indicate a partial key */
	klen = key->size;
	if (klen < 1 || klen > fh->key[k].k_len) {
		klen = fh->key[k].k_len;	/* Max key length for this index */
	}
	mode = ISGTEQ;
	fh->startiscur = 1;
	switch (cond) {
	case COB_EQ:
		mode = ISEQUAL;
		fh->readdir = ISNEXT;
		break;
	case COB_GE:
		mode = ISGTEQ;
		fh->readdir = ISNEXT;
		break;
	case COB_GT:
		mode = ISGREAT;
		fh->readdir = ISNEXT;
		break;
	case COB_LE:
		fh->readdir = ISPREV;
		mode = ISGTEQ;
		break;
	case COB_LT:
		fh->readdir = ISPREV;
		mode = ISGTEQ;
		break;
	default:
		return COB_STATUS_21_KEY_INVALID;
		break;
	}
	if ((isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, mode)) == -1) {
		ret = isretsts (COB_STATUS_10_END_OF_FILE);
		fh->curkey = -1;
		fh->keyhasdups = 0;
		fh->startcond = -1;
		fh->readdir = -1;
		fh->startiscur = 0;
	} else {
		if (ret == COB_STATUS_00_SUCCESS) {
			fh->startcond = cond;
			extract_key (fh, k, f->record->data, fh->savekey);
			fh->curkey = k;
			f->flag_end_of_file = 0;
			f->flag_begin_of_file = 0;
			f->flag_first_read = 1;
		} else {
			fh->curkey = -1;
			fh->keyhasdups = 0;
			fh->startcond = -1;
			fh->readdir = -1;
		}
	}
	return ret;
#else	/* WITH_INDEX_EXTFH */
	return indexed_start_internal (f, cond, key, 0, 0);
#endif	/* WITH_INDEX_EXTFH */
}

/* Random READ of the INDEXED file  */

static int
indexed_read (cob_file *f, cob_field *key, const int read_opts)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_read (f, key, read_opts);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			k;
	int			ret = COB_STATUS_00_SUCCESS;
	int			lmode;

	fh = f->file;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	for (k = 0; k < f->nkeys; k++) {
		if (f->keys[k].field->data == key->data) {
			break;
		}
	}
	if (fh->curkey != k) {				/* Switch to this index */
		isstart (fh->isfd, &fh->key[k], fh->key[0].k_len, (void *)f->record->data, ISEQUAL);
		fh->curkey = k;
		fh->wrkhasrec = 0;
		if (fh->key[k].k_flags & ISDUPS) {
			fh->keyhasdups = 1;
		} else {
			fh->keyhasdups = 0;
		}
	}
	fh->startcond = -1;
	lmode = 0;
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
		if (f->open_mode != COB_OPEN_INPUT) {
			if (!(read_opts & COB_READ_IGNORE_LOCK)) {
				lmode = ISLOCK;
			}
		}
	}
#ifdef ISSKIPLOCK
	if (read_opts & COB_READ_IGNORE_LOCK) {
		lmode = ISSKIPLOCK;
	}
#endif
	iserrno = 0;
	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	switch (read_opts & 0x0F) {
	case COB_READ_NEXT:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode) == -1) {
			ret = isretsts (COB_STATUS_10_END_OF_FILE);
			f->flag_end_of_file = 1;
		}
		break;
	case COB_READ_PREVIOUS:
		fh->readdir = ISPREV;
		if (isread (fh->isfd, (void *)f->record->data, ISPREV | lmode) == -1) {
			ret = isretsts (COB_STATUS_10_END_OF_FILE);
			f->flag_begin_of_file = 1;
		}
		break;
	case COB_READ_FIRST:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode) == -1) {
			ret = isretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	case COB_READ_LAST:
		fh->readdir = ISPREV;
		if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode) == -1) {
			ret = isretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	default:
		fh->readdir = -1;
		if (isread (fh->isfd, (void *)f->record->data, ISEQUAL | lmode) == -1) {
			ret = isretsts (COB_STATUS_21_KEY_INVALID);
		}
		break;
	}
	if (ret == 0) {
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		fh->readdone = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		extract_key (fh, 0, f->record->data, fh->savekey);
		fh->recnum = isrecnum;
#ifdef	ISVARLEN
		if (f->record_min != f->record_max) {
			f->record->size = isreclen;
		}
#endif
	} else {
		memset (fh->savekey, 0, fh->key[0].k_len);
		fh->recnum = 0;
		fh->readdone = 0;
	}
	return ret;

#else	/* WITH_INDEX_EXTFH */

	struct indexed_file	*p = f->file;
	int			ret;
	int			test_lock = 0;

#ifdef	USE_DB41
	if (bdb_env != NULL) {
		unlock_record (f);
		test_lock = 1;
	}
#endif

	ret = indexed_start_internal (f, COB_EQ, key, read_opts, test_lock);
	if (ret != COB_STATUS_00_SUCCESS) {
		return ret;
	}

	f->record->size = p->data.size;
	memcpy (f->record->data, p->data.data, p->data.size);

	return COB_STATUS_00_SUCCESS;
#endif	/* WITH_INDEX_EXTFH */
}

/* Sequential READ of the INDEXED file */

static int
indexed_read_next (cob_file *f, const int read_opts)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_read_next (f, read_opts);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret;
	int			lmode;
	int			domoveback;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	lmode = 0;

	if (f->flag_nonexistent) {
		if (f->flag_first_read == 0) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
		f->flag_first_read = 0; 
		return COB_STATUS_10_END_OF_FILE;
	}

	if (fh->curkey == -1) {				/* Switch to this index */
		isstart (fh->isfd, &fh->key[0], 0, (void *)f->record->data, ISFIRST);
		fh->curkey = 0;
		fh->readdir = ISNEXT;
		fh->startcond = -1;
		fh->startiscur = 0;
		fh->wrkhasrec = 0;
		fh->keyhasdups = 0;
	}
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) && f->open_mode != COB_OPEN_INPUT) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			lmode = ISLOCK;
		}
	}
#ifdef ISSKIPLOCK
	if (read_opts & COB_READ_IGNORE_LOCK) {
		lmode |= ISSKIPLOCK;
	}
#endif
	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	iserrno = 0;
	switch (read_opts & 0x0F) {
	case COB_READ_NEXT:
		fh->readdir = ISNEXT;
		if (fh->eofpending == ISNEXT) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (isread (fh->isfd, (void *)f->record->data, ISCURR) == -1) {
				ret = isretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_GE:
					domoveback = 0;
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, fh->key[fh->curkey].k_len) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, iserrno == 0 ? ISNEXT : ISFIRST);
					}
					break;
				case COB_LE:
					domoveback = 0;
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, fh->key[fh->curkey].k_len) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, iserrno == 0 ? ISPREV : ISLAST);
					}
					break;
				case COB_LT:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, fh->key[fh->curkey].k_len) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, fh->key[fh->curkey].k_len)<=0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode) == -1) {
					ret = isretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISNEXT) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* now lock 'peek ahead' record */
				if (isread (fh->isfd, (void *)f->record->data,
				    ISCURR | fh->lmode) == -1) {
					ret = isretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISPREV) {
				isread (fh->isfd, (void *)f->record->data, ISNEXT);
				fh->wrkhasrec = 0;
			}
			if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode) == -1) {
				ret = isretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_PREVIOUS:
		fh->readdir = ISPREV;
		if (fh->eofpending == ISPREV) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode) == -1) {
				ret = isretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_LE:
					domoveback = 0;
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, fh->key[fh->curkey].k_len) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_LT:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, fh->key[fh->curkey].k_len) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, fh->key[fh->curkey].k_len) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				case COB_GE:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, fh->key[fh->curkey].k_len) < 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode) == -1) {
					ret = isretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISPREV) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* now lock 'peek ahead' record */
				if (isread (fh->isfd, (void *)f->record->data,
				    ISCURR | fh->lmode) == -1) {
					ret = isretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISNEXT) {
				isread (fh->isfd, (void *)f->record->data, ISPREV);
				fh->wrkhasrec = 0;
			}
			if (isread (fh->isfd, (void *)f->record->data, ISPREV | lmode) == -1) {
				ret = isretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_FIRST:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode) == -1) {
			ret = isretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	case COB_READ_LAST:
		fh->readdir = ISPREV;
		if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode) == -1) {
			ret = isretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	default:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode) == -1) {
			ret = isretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	}
	if (ret == 0) {
		fh->eofpending = 0;
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		fh->readdone = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		extract_key (fh, 0, f->record->data, fh->savekey);
		fh->recnum = isrecnum;
#if defined(ISVARLEN)
		if (f->record_min != f->record_max) {
			f->record->size = isreclen;
		}
#endif
#if defined(WITH_COBSTATUS02) 
		if (fh->keyhasdups) {
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir) == -1) {
				fh->eofpending = fh->readdir;
				fh->wrkhasrec = 0;
				fh->saverecnum = -1;
			} else {
				fh->wrkhasrec = fh->readdir;
				fh->saverecnum = isrecnum;
				if (memcmp (f->record->data + fh->key[fh->curkey].k_start, 
				    fh->recwrk + fh->key[fh->curkey].k_start,
				    fh->key[fh->curkey].k_len) == 0) {
					ret = COB_STATUS_02_SUCCESS_DUPLICATE;
				}
			}
		}
#elif defined(WITH_DISAM)
		if((isstat1 == '0') && (isstat2 == '2')) {
			ret = COB_STATUS_02_SUCCESS_DUPLICATE;
		}
#endif
	} else {
		memset (fh->savekey, 0, fh->key[0].k_len);
		fh->recnum = 0;
		fh->readdone = 0;
		fh->wrkhasrec = 0;
	}
	return ret;
#else	/* WITH_INDEX_EXTFH */
	struct indexed_file	*p = f->file;
	int			ret;
	int			read_nextprev;
	int			nextprev = DB_NEXT;
	int			file_changed = 0;
	unsigned int		dupno;

#ifdef	USE_DB41
	if (bdb_env != NULL) {
		unlock_record (f);
	}
#endif

	if (unlikely(read_opts & COB_READ_PREVIOUS)) {
		if (f->flag_end_of_file) {
			nextprev = DB_LAST;
		} else {
			nextprev = DB_PREV;
		}
	} else if (f->flag_begin_of_file) {
		nextprev = DB_FIRST;
	}
#ifdef	USE_DB41
	/* the open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);
#endif

	if (f->flag_first_read) {
		/* data is read in indexed_open or indexed_start */
		if (p->data.data == NULL || (f->flag_first_read == 2 &&
		    nextprev == DB_PREV)) {
#ifdef	USE_DB41
			p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
			p->cursor[p->key_index] = NULL;
			if (p->key_index != 0) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
			}
#endif
			return COB_STATUS_10_END_OF_FILE;
		}
		/* check if previously read data still exists */
		p->key.size = (cob_dbtsize_t) f->keys[p->key_index].field->size;
		p->key.data = p->last_readkey[p->key_index];
#ifdef	USE_DB41
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET);
#else
		ret = DB_GET (p->db[p->key_index], 0);
#endif
		if (!ret && p->key_index > 0) {
			if (f->keys[p->key_index].flag) {
				memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
				while (ret == 0 &&
				      memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
				      dupno < p->last_dupno[p->key_index]) {
#ifdef	USE_DB41
					ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
#else
					ret = DB_SEQ (p->db[p->key_index], R_NEXT); 
#endif
					memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
				}
				if (ret == 0 &&
				   memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
				   dupno == p->last_dupno[p->key_index]) {
					ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, f->keys[0].field->size);
				} else {
					ret = 1;
				}
			} else {
				ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, f->keys[0].field->size);
			}
			if (!ret) {
				p->key.size = (cob_dbtsize_t) f->keys[0].field->size;
				p->key.data = p->last_readkey[p->key_index + f->nkeys];
				ret = DB_GET (p->db[0], 0);
			}
		}
		file_changed = ret;	
#ifdef	USE_DB41
		if (bdb_env != NULL && !file_changed) {
			if (!(read_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (read_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
#endif
	}
	if (!f->flag_first_read || file_changed) {
		if (nextprev == DB_FIRST || nextprev == DB_LAST) {
			read_nextprev = 1;
		} else {
			p->key.size = (cob_dbtsize_t) f->keys[p->key_index].field->size;
			p->key.data = p->last_readkey[p->key_index];
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE); 
#else
			ret = DB_SEQ (p->db[p->key_index], R_CURSOR); 
#endif
			/* ret != 0 possible, records may be deleted since last read */
			if (ret != 0) {
				if (nextprev == DB_PREV) {
					nextprev = DB_LAST;
					read_nextprev = 1;
				} else {
#ifdef	USE_DB41
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
#endif
					return COB_STATUS_10_END_OF_FILE;
				}
			} else {
				if (memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0) {
					if (p->key_index > 0 && f->keys[p->key_index].flag) {
						memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
						while (ret == 0 &&
						memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
						dupno < p->last_dupno[p->key_index]) {
#ifdef	USE_DB41
							ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
#else
							ret = DB_SEQ (p->db[p->key_index], R_NEXT); 
#endif
							memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
						}
						if (ret != 0) {
							if (nextprev == DB_PREV) {
								nextprev = DB_LAST;
								read_nextprev = 1;
							} else {
#ifdef	USE_DB41
								p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
								p->cursor[p->key_index] = NULL;
								if (p->key_index != 0) {
									p->cursor[0]->c_close (p->cursor[0]);
									p->cursor[0] = NULL;
								}
#endif
								return COB_STATUS_10_END_OF_FILE;
							}
						} else {
							if (memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
								dupno == p->last_dupno[p->key_index]) {
								read_nextprev = 1;
							} else {
								if (nextprev == DB_PREV) {
									read_nextprev = 1;
								} else {
									read_nextprev = 0;
								}
							}
						}
					} else {
						read_nextprev = 1;
					}
				} else {
					if (nextprev == DB_PREV) {
						read_nextprev = 1;
					} else {
						read_nextprev = 0;
					}
				}
			}
		}
		if (read_nextprev) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], nextprev);
#else
			ret = DB_SEQ (p->db[p->key_index], nextprev);
#endif
			if (ret != 0) {
#ifdef	USE_DB41
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
#endif
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (p->key_index > 0) {
			/* temporarily save alternate key */
			memcpy (p->temp_key, p->key.data, p->key.size);
			if (f->keys[p->key_index].flag) {
				memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
			}
			p->key.data = p->data.data;
			p->key.size = f->keys[0].field->size;
			if (DB_GET (p->db[0], 0) != 0) {
#ifdef	USE_DB41
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
#endif
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
#ifdef	USE_DB41
		if (bdb_env != NULL) {
			if (!(read_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (read_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
#endif
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, p->key.size);
		} else {
			memcpy (p->last_readkey[p->key_index], p->temp_key,
				    f->keys[p->key_index].field->size);
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, f->keys[0].field->size);
			if (f->keys[p->key_index].flag) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

#ifdef	USE_DB41
	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}
#endif

	f->record->size = p->data.size;
	memcpy (f->record->data, p->data.data, p->data.size);

	return COB_STATUS_00_SUCCESS;
#endif	/* WITH_INDEX_EXTFH */
}

#if	!defined(WITH_INDEX_EXTFH) && !defined(WITH_ANY_ISAM)
/* get the next number in a set of duplicates */
static unsigned int
get_dupno (cob_file *f, const int i)
{
	int			ret;
	unsigned int		dupno =  0;
	struct indexed_file	*p = f->file;

	DBT_SET (p->key, f->keys[i].field);
	memcpy (p->temp_key, p->key.data, p->key.size);
#ifdef	USE_DB41
	p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], 0);
	ret = DB_SEQ (p->cursor[i], DB_SET_RANGE); 
#else
	ret = DB_SEQ (p->db[i], R_CURSOR); 
#endif
	while (ret == 0 && memcmp (p->key.data, p->temp_key, p->key.size) == 0) {
		memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
#ifdef	USE_DB41
		ret = DB_SEQ (p->cursor[i], DB_NEXT); 
#else
		ret = DB_SEQ (p->db[i], R_NEXT); 
#endif
	}
#ifdef	USE_DB41
	p->cursor[i]->c_close (p->cursor[i]);
	p->cursor[i] = NULL;
#endif
	return ++dupno;
}

static int
check_alt_keys (cob_file *f, const int rewrite)
{
	size_t		i;
	int			ret;
	struct indexed_file	*p = f->file;

	for (i = 1; i < f->nkeys; i++) {
		if (!f->keys[i].flag) {
			DBT_SET (p->key, f->keys[i].field);
			ret = DB_GET (p->db[i], 0);
			if (ret == 0) {
				if (rewrite) {
					if (memcmp (p->data.data, f->keys[0].field->data, f->keys[0].field->size)) {
						return 1;
					}
				} else {
					return 1;
				}
			}
		}
	}
	return 0;
}

static int
indexed_write_internal (cob_file *f, const int rewrite, const int opt)
{
	size_t			i;
	struct indexed_file	*p = f->file;
	int			flags;
	unsigned int		dupno;
#ifdef	USE_DB41
	int			close_cursor;

	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1; 
		close_cursor = 1;
	}
#endif

	/* check duplicate alternate keys */
	if (f->nkeys > 1 && !rewrite) {
		if (check_alt_keys (f, 0)) {
#ifdef	USE_DB41
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0; 
			}
#endif
			return COB_STATUS_22_KEY_EXISTS;
		}
		DBT_SET (p->key, f->keys[0].field);
	}

	/* write data */
#ifdef	USE_DB41
	if (p->cursor[0]->c_get (p->cursor[0], &p->key, &p->data, DB_SET) == 0) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0; 
		}
		return COB_STATUS_22_KEY_EXISTS;
	}
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	p->cursor[0]->c_put (p->cursor[0], &p->key, &p->data, DB_KEYFIRST);
#else
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	if (DB_PUT (p->db[0], R_NOOVERWRITE) != 0) {
		return COB_STATUS_22_KEY_EXISTS;
	}
#endif

	/* write secondary keys */
	p->data = p->key;
	for (i = 1; i < f->nkeys; i++) {
		if (rewrite && ! p->rewrite_sec_key[i]) {
			continue;
		}
		if (f->keys[i].flag) {
			flags =  0;
			dupno = get_dupno(f, i);
			memcpy (p->temp_key, f->keys[0].field->data,
				   f->keys[0].field->size);
			memcpy (p->temp_key + f->keys[0].field->size, &dupno,
				   sizeof(unsigned int));
			p->data.data = p->temp_key;
			p->data.size = f->keys[0].field->size + sizeof(unsigned int);
		} else {
#ifdef	USE_DB41
			flags = DB_NOOVERWRITE;
#else
			flags =  R_NOOVERWRITE;
#endif
		}

		DBT_SET (p->key, f->keys[i].field);
		if (DB_PUT (p->db[i], flags) != 0) {
#ifdef	USE_DB41
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0; 
			}
#endif
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

#ifdef	USE_DB41
	if (opt & COB_WRITE_LOCK) {
		if (bdb_env != NULL) {
			DBT_SET (p->key, f->keys[0].field);
			if (lock_record (f, p->key.data, p->key.size)) {
				if (close_cursor) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
					p->write_cursor_open = 0; 
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}
	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0; 
	}
#endif
	return COB_STATUS_00_SUCCESS;
}
#endif	/* WITH_INDEX_EXTFH */

/* WRITE to the INDEXED file  */

static int
indexed_write (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH
	return extfh_indexed_write (f, opt);
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh = f->file;
	int			ret = COB_STATUS_00_SUCCESS;

	if (f->flag_nonexistent) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
#if defined(ISVARLEN)
	if (f->record_min != f->record_max) {
		isreclen = f->record->size;
	}
#endif
	if (iswrite (fh->isfd, (void *)f->record->data) == -1) {
		ret = isretsts (COB_STATUS_49_I_O_DENIED);
		if (iserrno == EDUPL) {
			if (f->open_mode == COB_OPEN_OUTPUT) {
				ret = COB_STATUS_21_KEY_INVALID;
			}
		}
	} else {
		extract_key (fh, 0, f->record->data, fh->savekey);
	}
	return ret;
#else	/* WITH_INDEX_EXTFH */
	struct indexed_file	*p = f->file;

#ifdef	USE_DB41
	if (bdb_env != NULL) {
		unlock_record (f);
	}
#endif

	/* check record key */
	DBT_SET (p->key, f->keys[0].field);
	if (!p->last_key) {
		p->last_key = cob_malloc (p->key.size);
	} else if (f->access_mode == COB_ACCESS_SEQUENTIAL
		 && memcmp (p->last_key, p->key.data, p->key.size) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	memcpy (p->last_key, p->key.data, p->key.size);

	return indexed_write_internal (f, 0, opt);
#endif	/* WITH_INDEX_EXTFH */
}

#if	!defined(WITH_INDEX_EXTFH) && !defined(WITH_ANY_ISAM)
static int
indexed_delete_internal (cob_file *f, const int rewrite)
{
	size_t			i;
	size_t			offset;
	struct indexed_file	*p = f->file;
	DBT			prim_key;
#ifdef	USE_DB41
	int			ret, flags, close_cursor;

	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1; 
		close_cursor = 1;
	}
	if (bdb_env != NULL) {
		unlock_record (f);
	}
#endif
	/* find the primary key */
#ifdef	USE_DB41
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		DBT_SET (p->key, f->keys[0].field);
	}
	ret = DB_SEQ (p->cursor[0], DB_SET);
	if (ret != 0 && f->access_mode != COB_ACCESS_SEQUENTIAL) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0; 
		}
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (bdb_env != NULL) {
		ret = test_record_lock (f, p->key.data, p->key.size);
		if (ret) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0; 
			}
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
#else
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		DBT_SET (p->key, f->keys[0].field);
		if (DB_GET (p->db[0], 0) != 0) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}
#endif
	prim_key = p->key;

	/* delete the secondary keys */
	offset = (char *) p->data.data - (char *) f->record->data;
	for (i = 1; i < f->nkeys; i++) {
		DBT_SET (p->key, f->keys[i].field);
		p->key.data = (char *)p->key.data + offset;
		/* rewrite: no delete if secondary key is unchanged */
		if (rewrite) {
			p->rewrite_sec_key[i] = memcmp (p->key.data, f->keys[i].field->data, p->key.size);
			if (!p->rewrite_sec_key[i]) {
				continue;
			}
		}
		if (!f->keys[i].flag) {
			DB_DEL (p->db[i], &p->key, 0);
		} else {
			DBT	sec_key = p->key;

#ifdef	USE_DB41
			p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], flags);
			if (DB_SEQ (p->cursor[i], DB_SET_RANGE) == 0) {
#else
			if (DB_SEQ (p->db[i], R_CURSOR) == 0) {
#endif
				while (sec_key.size == p->key.size
				&& memcmp (p->key.data, sec_key.data,
				sec_key.size) == 0) {
					if (memcmp (p->data.data, prim_key.data,
					prim_key.size) == 0) {
#ifdef	USE_DB41
						p->cursor[i]->c_del (p->cursor[i], 0);
#else
						DB_DEL (p->db[i], &p->key, R_CURSOR);
#endif
					}
#ifdef	USE_DB41
					if (DB_SEQ (p->cursor[i], DB_NEXT) != 0) {
#else
					if (DB_SEQ (p->db[i], R_NEXT) != 0) {
#endif
						break;
					}
				}
			}
#ifdef	USE_DB41
			p->cursor[i]->c_close (p->cursor[i]);
			p->cursor[i] = NULL;
#endif
		}
	}

	/* delete the record */
#ifdef	USE_DB41
	p->cursor[0]->c_del (p->cursor[0], 0);
#else
	DB_DEL (p->db[0], &prim_key, 0);
#endif

#ifdef	USE_DB41
	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0; 
	}
#endif
	return COB_STATUS_00_SUCCESS;
}
#endif	/* !WITH_INDEX_EXTFH && !WITH_ANY_ISAM */

/* DELETE record from the INDEXED file  */

static int
indexed_delete (cob_file *f)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_delete (f);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (fh->curkey == -1) {				/* Switch to prime index */
		isstart (fh->isfd, &fh->key[0], fh->key[0].k_leng, (void *)f->record->data, ISEQUAL);
		fh->curkey = 0;
		fh->readdir = ISNEXT;
	} else {
		savefileposition (f);
		if (fh->curkey != 0) {			/* Switch to prime index */
			isstart (fh->isfd, &fh->key[0], fh->key[0].k_leng, (void *)f->record->data, ISEQUAL);
		}
	}
	if (isread (fh->isfd, (void *)f->record->data, ISEQUAL | ISLOCK) == -1) {
		ret = isretsts (COB_STATUS_21_KEY_INVALID);
	} else if (isdelete (fh->isfd, (void *)f->record->data) == -1) {
		ret = isretsts (COB_STATUS_49_I_O_DENIED);
	}
	restorefileposition (f);
	return ret;
#else	/* WITH_INDEX_EXTFH */
	return indexed_delete_internal (f, 0);
#endif	/* WITH_INDEX_EXTFH */
}

/* REWRITE record to the INDEXED file  */

static int
indexed_rewrite (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_rewrite (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			k;
	int			ret;
	int			curisnum;

	COB_UNUSED (opt);

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	&& keycmp (fh, 0, f->record->data, fh->savekey) != 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	if (fh->curkey >= 0) {		/* Index is active */
		/* Save record data */
		memcpy (fh->recwrk, f->record->data, f->record_max);
		savefileposition (f);
		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (fh->curkey != 0) { 		/* Activate Prime index */
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk, ISEQUAL);
		}
		/* Verify record exists */
		if (isread (fh->isfd, fh->recwrk, ISEQUAL) == -1) {
			restorefileposition (f);				
			return COB_STATUS_21_KEY_INVALID;
		}
		curisnum = isrecnum;
		for (k = 1; k < f->nkeys && ret == COB_STATUS_00_SUCCESS; k++) {
			if (fh->key[k].k_flags & ISDUPS) {
				continue;
			}
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[k], fh->key[k].k_leng, (void *)fh->recwrk, ISEQUAL);
			if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL) != -1
			&& isrecnum != curisnum) {
				ret = COB_STATUS_22_KEY_EXISTS;
				break;
			}
		}
		if (ret == COB_STATUS_00_SUCCESS) {
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk, ISEQUAL);
			if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL | ISLOCK) == -1) {
				ret = isretsts (COB_STATUS_49_I_O_DENIED);
			} else if (isrewcurr (fh->isfd, (void *)f->record->data) == -1) {
				ret = isretsts (COB_STATUS_49_I_O_DENIED);
			}
		}
		restorefileposition (f);
		return ret;
	}

	memcpy (fh->recwrk, f->record->data, f->record_max);
	if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL | ISLOCK) == -1) {
		ret = isretsts (COB_STATUS_49_I_O_DENIED);
	} else if (isrewrite (fh->isfd, (void *)f->record->data) == -1) {
		ret = isretsts (COB_STATUS_49_I_O_DENIED);
	}
/* RXW */
	if (!ret) {
		if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
		    !(f->lock_mode & COB_LOCK_MULTIPLE)) {
			isrelease (fh->isfd);
		}
	}
	return ret;
#else	/* WITH_INDEX_EXTFH */
	struct indexed_file *p;
	int			ret;
#ifdef	USE_DB41
	int			flags;

	p = f->file;
	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
	p->write_cursor_open = 1; 
	if (bdb_env != NULL) {
		unlock_record (f);
	}
#endif

	/* check duplicate alternate keys */
	if (check_alt_keys (f, 1)) {
#ifdef	USE_DB41
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0; 
#endif
		return COB_STATUS_22_KEY_EXISTS;
	}

	/* delete the current record */
	ret = indexed_delete_internal (f, 1);

	if (ret != COB_STATUS_00_SUCCESS) {
#ifdef	USE_DB41
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0; 
#endif
		return ret;
	}

	/* write data */
	DBT_SET (p->key, f->keys[0].field);
	ret = indexed_write_internal (f, 1, opt);

#ifdef	USE_DB41
	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
	p->write_cursor_open = 0; 
#endif
	return ret;
#endif	/* WITH_INDEX_EXTFH */
}

#ifdef	USE_DB41
/* 
 * check if a file exists in bdb data dirs
 */
static int
is_absolute (const char *filename)
{
#ifdef _WIN32
	if (filename[0] == '/' || filename[0] == '\\') {
		return 1;
	} else {
		if (isalpha (filename[0]) && filename[1] == ':' &&
		  (filename[2] == '/' || filename[2] == '\\')) {
			return 1;
		} else {
			return 0;
		}
	}
#else
	if (filename[0] == '/') {
		return 1;
	} else {
		return 0;
	}
#endif
}

static int
bdb_nofile (char *filename)
{
	int		i;
	struct	stat	st;

	if (is_absolute (filename)) {
		if (stat (filename, &st) == -1 && errno == ENOENT) {
			return 1;
		} else {
			return 0;
		}
	}

	for (i = 0; bdb_data_dir && bdb_data_dir[i]; ++i) {
		bdb_buff[COB_SMALL_MAX] = 0;
		if (is_absolute (bdb_data_dir[i])) {
			snprintf (bdb_buff, COB_SMALL_MAX, "%s/%s",
				  bdb_data_dir[i], filename);
		} else {
			snprintf (bdb_buff, COB_SMALL_MAX, "%s/%s/%s",
				  bdb_home, bdb_data_dir[i], filename);
		}
		if (stat (bdb_buff, &st) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	if (i == 0) {
		bdb_buff[COB_SMALL_MAX] = 0;
		snprintf (bdb_buff, COB_SMALL_MAX, "%s/%s", bdb_home, filename);
		if (stat (bdb_buff, &st) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	return 1;
}
#endif

#endif	/* WITH_DB */

static void COB_NOINLINE
cob_file_unlock (cob_file *f)
{
#ifdef	WITH_DB
#ifdef	USE_DB41
	struct indexed_file	*p;
#endif
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh;
#endif
#ifdef HAVE_FCNTL
	struct flock		lock;
#endif

	if (f->open_mode != COB_OPEN_CLOSED &&
	    f->open_mode != COB_OPEN_LOCKED) {
		if (f->organization == COB_ORG_SORT) {
			return;
		}
		if (f->organization != COB_ORG_INDEXED) {
#ifndef	WITH_SEQRA_EXTFH
			fflush ((FILE *)f->file);
			fsync (fileno ((FILE *)f->file));
#ifdef HAVE_FCNTL
			if (!(f->lock_mode & COB_LOCK_EXCLUSIVE)) {
				/* unlock the file */
				memset ((unsigned char *)&lock, 0, sizeof (struct flock));
				lock.l_type = F_UNLCK;
				lock.l_whence = SEEK_SET;
				lock.l_start = 0;
				lock.l_len = 0;
				fcntl (fileno ((FILE *)f->file), F_SETLK, &lock);
			}
#endif
#endif	/* WITH_SEQRA_EXTFH */
		} else {
#ifdef	WITH_INDEX_EXTFH
			extfh_indexed_unlock (f);
#else	/* WITH_INDEX_EXTFH */
#ifdef	WITH_DB
#ifdef	USE_DB41
			p = f->file;
			if (bdb_env != NULL) {
				unlock_record (f);
				bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			}
#endif
#endif
#if	defined(WITH_ANY_ISAM)
			fh = f->file;
			isrelease (fh->isfd);
#endif
#endif	/* WITH_INDEX_EXTFH */
		}
	}
}

/*
 * Public interface
 */

void
cob_ex_unlock_file (cob_file *f, cob_field *fnstatus)
{
	cob_file_unlock (f);
	RETURN_STATUS (COB_STATUS_00_SUCCESS);
}

void
cob_unlock_file (cob_file *f, cob_field *fnstatus)
{
	char	openMode[OPENMODESIZE];

	memset (openMode, 0, sizeof (openMode));
	sprintf (openMode, "%02d", f->last_open_mode);
	if (cob_invoke_fun (COB_IO_UNLOCK, (char*)f, NULL, NULL, fnstatus, openMode, NULL, NULL)) {
		return;
	}
	cob_ex_unlock_file (f, fnstatus);
}

void
cob_ex_open (cob_file *f, const int mode, const int sharing, cob_field *fnstatus)
{
	char		*p;
	char		*src;
	char		*dst;
	size_t		i;
	size_t		simple;
	int		was_not_exist = 0;
	struct stat	st;

	f->flag_read_done = 0;

	/* file was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		RETURN_STATUS (COB_STATUS_38_CLOSED_WITH_LOCK);
	}

	/* file is already open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		RETURN_STATUS (COB_STATUS_41_ALREADY_OPEN);
	}

	f->last_open_mode = mode;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;

	if (f->special) {
		if (f->special == 1) {
			if (mode != COB_OPEN_INPUT) {
				RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
			}
			f->file = stdin;
			f->open_mode = mode;
			RETURN_STATUS (COB_STATUS_00_SUCCESS);
		} else {
			if (mode != COB_OPEN_OUTPUT) {
				RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
			}
			f->file = stdout;
			f->open_mode = mode;
			RETURN_STATUS (COB_STATUS_00_SUCCESS);
		}
	}

	/* obtain the file name */
	if(f->assign == NULL) {
		strncpy (file_open_name, f->select_name, COB_SMALL_MAX);
	} else {
		cob_field_to_string (f->assign, file_open_name);
	}

#ifdef	WITH_INDEX_EXTFH
	if (f->organization == COB_ORG_INDEXED) {
		int	ret;

		ret = extfh_indexed_locate (f, file_open_name);
		switch (ret) {
		case COB_NOT_CONFIGURED:
			/* EXTFH requires OC to process the filename */
			break;
		case COB_STATUS_00_SUCCESS:
			/* EXTFH recognized the file */
			goto file_available;
		default:
			/* EXTFH detected an error */
			RETURN_STATUS (ret);
		}
	}
#endif	/* WITH_INDEX_EXTFH */

#ifdef	WITH_SEQRA_EXTFH
	if (f->organization != COB_ORG_INDEXED) {
		int	ret;

		ret = extfh_seqra_locate (f, file_open_name);
		switch (ret) {
		case COB_NOT_CONFIGURED:
			/* EXTFH requires OC to process the filename */
			break;
		case COB_STATUS_00_SUCCESS:
			/* EXTFH recognized the file */
			goto file_available;
		default:
			/* EXTFH detected an error */
			RETURN_STATUS (ret);
		}
	}
#endif	/* WITH_SEQRA_EXTFH */

	if (cob_current_module->flag_filename_mapping) {
		src = file_open_name;
		dst = file_open_buff;
		simple = 1;
		/* expand envoronment variables */
		/* ex. "$TMPDIR/foo" -> "/tmp/foo" */
		while (*src) {
			if (!isalnum (*src) && *src != '_' && *src != '-') {
				simple = 0;
			}
			if (*src == '$') {
				for (i = 1; ; i++) {
					if (!isalnum (src[i]) && src[i] != '_' && *src != '-') {
						break;
					}
				}
				memcpy (file_open_env, src + 1, i - 1);
				file_open_env[i - 1] = 0;
				if ((p = getenv (file_open_env)) != NULL) {
					strcpy (dst, p);
					dst += strlen (p);
				}
				src += i;
			} else {
				*dst++ = *src++;
			}
		}
		*dst = 0;
		cb_get_jisword_buff (file_open_buff, file_open_name, COB_SMALL_BUFF);

		/* resolve by environment variables */
		/* ex. "TMPFILE" -> DD_TMPFILE, dd_TMPFILE, or TMPFILE */
		if (simple) {
			for (i = 0; i < NUM_PREFIX; i++) {
				snprintf (file_open_buff, COB_SMALL_MAX, "%s%s",
					  prefix[i], file_open_name);
				if ((p = getenv (file_open_buff)) != NULL) {
					strncpy (file_open_name, p, COB_SMALL_MAX);
					break;
				}
			}
			if (i == NUM_PREFIX && cob_file_path) {
				snprintf (file_open_buff, COB_SMALL_MAX, "%s/%s",
					  cob_file_path, file_open_name);
				strncpy (file_open_name, file_open_buff,
					 COB_SMALL_MAX);
			}
		}
	}
	/* check if the file exists */
#ifdef	USE_DB41
	if (f->organization == COB_ORG_INDEXED) {
		if ((bdb_env && bdb_nofile (file_open_name)) ||
		     (!bdb_env && stat (file_open_name, &st) == -1 && errno == ENOENT)) {
			was_not_exist = 1;
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0 &&
			    (mode != COB_OPEN_I_O || !cob_check_env (COB_IO_CREATES, "yes")) &&
			    (mode != COB_OPEN_EXTEND || !cob_check_env (COB_EXTEND_CREATES, "yes"))) {
				RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
			}
		}
	} else if (stat (file_open_name, &st) == -1 && errno == ENOENT) {
#else	/* USE_DB41 */

#if	defined(WITH_ANY_ISAM)
	if (f->organization == COB_ORG_INDEXED) {
		strncpy (file_open_buff, file_open_name, COB_SMALL_MAX);
		strcat (file_open_buff, ".idx");
		if (stat (file_open_buff, &st) == -1 && errno == ENOENT) {
			was_not_exist = 1;
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0 &&
			    (mode != COB_OPEN_I_O || !cob_check_env (COB_IO_CREATES, "yes")) &&
			    (mode != COB_OPEN_EXTEND || !cob_check_env (COB_EXTEND_CREATES, "yes"))) {
				RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
			}
		}
		strncpy (file_open_buff, file_open_name, COB_SMALL_MAX);
		strcat (file_open_buff, ".dat");
		if (stat (file_open_buff, &st) == -1 && errno == ENOENT) {
			was_not_exist = 1;
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0 &&
			    (mode != COB_OPEN_I_O || !cob_check_env (COB_IO_CREATES, "yes")) &&
			    (mode != COB_OPEN_EXTEND || !cob_check_env (COB_EXTEND_CREATES, "yes"))) {
				RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
			}
		}
	} else if (stat (file_open_name, &st) == -1 && errno == ENOENT) {
#else	/* WITH_ANY_ISAM */
	if (stat (file_open_name, &st) == -1 && errno == ENOENT) {
#endif	/* WITH_ANY_ISAM */

#endif	/* USE_DB41 */

		was_not_exist = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0 &&
		    (mode != COB_OPEN_I_O || !cob_check_env (COB_IO_CREATES, "yes")) &&
		    (mode != COB_OPEN_EXTEND || !cob_check_env (COB_EXTEND_CREATES, "yes"))) {
			RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
		}
	}

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
file_available:
#endif	/* WITH_INDEX_EXTFH || WITH_SEQRA_EXTFH */

	cob_cache_file (f);

	/* open the file */
#ifdef	WITH_SEQRA_EXTFH
	if (f->organization != COB_ORG_INDEXED) {
		int	ret;

		ret = extfh_cob_file_open (f, file_open_name, mode, sharing);
		switch (ret) {
		case COB_STATUS_00_SUCCESS:
			f->open_mode = mode;
			break;
		case COB_STATUS_35_NOT_EXISTS:
			if (f->flag_optional) {
				f->open_mode = mode;
				f->flag_nonexistent = 1;
				f->flag_end_of_file = 1;
				f->flag_begin_of_file = 1;
				RETURN_STATUS (COB_STATUS_05_SUCCESS_OPTIONAL);
			}
			break;
		}
		RETURN_STATUS (ret);
	}
#endif
#if	defined(WITH_ANY_ISAM)
	if (f->organization == COB_ORG_INDEXED) {
		/* Do this here to avoid mangling of the status in the 'switch' below */
		RETURN_STATUS (fileio_funcs[(int)f->organization]->open (f, file_open_name, mode, sharing));
	}
#endif
	switch (fileio_funcs[(int)f->organization]->open (f, file_open_name, mode, sharing)) {
	case 0:
		f->open_mode = mode;
		if (f->flag_optional && was_not_exist) {
			RETURN_STATUS (COB_STATUS_05_SUCCESS_OPTIONAL);
		} else {
			RETURN_STATUS (COB_STATUS_00_SUCCESS);
		}
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
			RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
		}
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			RETURN_STATUS (COB_STATUS_05_SUCCESS_OPTIONAL);
		} else {
			RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
		}
	case EACCES:
	case EISDIR:
	case EROFS:
		RETURN_STATUS (COB_STATUS_37_PERMISSION_DENIED);
	case EAGAIN:
	case COB_STATUS_61_FILE_SHARING:
		RETURN_STATUS (COB_STATUS_61_FILE_SHARING);
	case COB_STATUS_91_NOT_AVAILABLE:
		RETURN_STATUS (COB_STATUS_91_NOT_AVAILABLE);
	case COB_LINAGE_INVALID:
		RETURN_STATUS (COB_STATUS_57_I_O_LINAGE);
	default:
		RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
	}
}

int
cob_invoke_fun (int operate, char *f, cob_field *key, char *rec,
		cob_field *fnstatus, char *openMode, char *startCond, char *read_opts)
{
	int	iRet = 0;
	char	*s;
	char	funname[256];
	char	ret = '0';
	char	oper[OPENMODESIZE];
	char	excpcode[EXCPTCODESIZE];
	char	*p_excpcode = excpcode;
	char	tmpfnstatus[FNSTATUSSIZE];
	char	*p_tmpfnstatus = tmpfnstatus;
	int		status1 = 0;
	int	(*funcint)();

	sprintf (excpcode, "%05d", 0);
	sprintf (oper, "%02d", operate);
	sprintf (tmpfnstatus, "%02d", 0);
	s = getenv (TIS_DEFINE_USERFH);
	if (s != NULL) {
		strcpy (funname, s);
		funcint = cob_resolve_1 (funname);
		if (funcint) {
			if (fnstatus == NULL) {
				funcint (oper, f, key, rec, &p_tmpfnstatus, openMode,
					startCond, read_opts, &p_excpcode, (char*)&ret);
			} else {
				funcint (oper, f, key, rec, fnstatus->data, openMode,
					startCond, read_opts, &p_excpcode, (char*)&ret);
			}
			if (ret == '1') {
				iRet = 1;
			} else if (ret == '0') {
				iRet = 0;
			}
			cob_exception_code = atoi (p_excpcode);
			//ascii [0]->0x30 [9]->0x39
			if (fnstatus != NULL) {
				status1 = fnstatus->data[0] - 0x30;
			} else {
				status1 = p_tmpfnstatus[0] - 0x30;
			}
			if ((status1 > 0 && status1 <= 9) && cob_exception_code == 0) {
				cob_set_exception (status_exception[status1]);
			}
		}
	}
	return iRet;
}

void
cob_open (cob_file *f, const int mode, const int sharing, cob_field *fnstatus)
{
	char	openMode[OPENMODESIZE];

	memset (openMode, 0, sizeof (openMode));
	sprintf (openMode, "%02d", mode);
	if (cob_invoke_fun (COB_IO_OPEN, (char *)f, NULL, NULL, fnstatus, openMode, NULL, NULL)) {
		f->last_open_mode = atoi (openMode);
		return;
	}
	f->last_open_mode = atoi (openMode);
	cob_ex_open (f, f->last_open_mode, sharing, fnstatus);
}

void
cob_ex_close (cob_file *f, const int opt, cob_field *fnstatus)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->special) {
		f->open_mode = COB_OPEN_CLOSED;
		RETURN_STATUS (COB_STATUS_00_SUCCESS);
	}
	if (f->open_mode == COB_OPEN_CLOSED) {
		RETURN_STATUS (COB_STATUS_42_NOT_OPEN);
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
#ifdef	WITH_SEQRA_EXTFH
		if (f->organization != COB_ORG_INDEXED) {
			ret = extfh_cob_file_close (f, opt);
		} else {
#endif
			ret = fileio_funcs[(int)f->organization]->close (f, opt);
#ifdef	WITH_SEQRA_EXTFH
		}
#endif
	}

	if (ret == COB_STATUS_00_SUCCESS) {
		switch (opt) {
		case COB_CLOSE_LOCK:
			f->open_mode = COB_OPEN_LOCKED;
			break;
		default:
			f->open_mode = COB_OPEN_CLOSED;
			break;
		}
	}

	RETURN_STATUS (ret);
}

void
cob_close (cob_file *f, const int opt, cob_field *fnstatus)
{
	char	openMode[OPENMODESIZE];

	memset (openMode, 0, sizeof (openMode));
	sprintf (openMode, "%02d", f->last_open_mode);
	if (cob_invoke_fun (COB_IO_CLOSE, (char *)f, NULL, NULL, fnstatus, openMode, NULL, NULL)) {
		return;
	}
	cob_ex_close (f, opt, fnstatus);
}

#if 0
void
cob_unlock (cob_file *f)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->open_mode == COB_OPEN_CLOSED) {
		RETURN_STATUS (COB_STATUS_42_NOT_OPEN);
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	RETURN_STATUS (ret);
}
#endif

void
cob_ex_start (cob_file *f, const int cond, cob_field *key, cob_field *fnstatus)
{
	int	ret;

	f->flag_read_done = 0;
	f->flag_first_read = 0;

	if (f->flag_nonexistent) {
		RETURN_STATUS (COB_STATUS_23_KEY_NOT_EXISTS);
	}

	if (f->open_mode == COB_OPEN_CLOSED
	    || f->open_mode == COB_OPEN_OUTPUT
	    || f->open_mode == COB_OPEN_EXTEND
	    || f->access_mode == COB_ACCESS_RANDOM) {
		RETURN_STATUS (COB_STATUS_47_INPUT_DENIED);
	}

	ret = fileio_funcs[(int)f->organization]->start (f, cond, key);
	if (ret == COB_STATUS_00_SUCCESS) {
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		f->flag_first_read = 1;
	}

	RETURN_STATUS (ret);
}

void
cob_start (cob_file *f, const int cond, cob_field *key, cob_field *fnstatus)
{
	char	openMode[OPENMODESIZE];
	char	startCond[STARTCONDSIZE];

	memset (openMode, 0, sizeof (openMode));
	memset (startCond, 0, sizeof (startCond));
	sprintf (openMode, "%02d", f->last_open_mode);
	sprintf (startCond, "%01d", cond);
	if (cob_invoke_fun (COB_IO_START, (char*)f, key, NULL, fnstatus, openMode, startCond, NULL)) {
		return;
	}
	cob_ex_start (f, cond, key, fnstatus);
}

void
cob_ex_read (cob_file *f, cob_field *key, cob_field *fnstatus, int read_opts)
{
	int	ret;

	f->flag_read_done = 0;

	if (unlikely(f->flag_nonexistent)) {
		if (f->flag_first_read == 0) {
			RETURN_STATUS (COB_STATUS_23_KEY_NOT_EXISTS);
		}
		f->flag_first_read = 0;
		RETURN_STATUS (COB_STATUS_10_END_OF_FILE);
	}

	/* sequential read at the end of file is an error */
	if (key == NULL) {
		if (f->flag_end_of_file && !(read_opts & COB_READ_PREVIOUS)) {
			RETURN_STATUS (COB_STATUS_46_READ_ERROR);
		}
		if (f->flag_begin_of_file && (read_opts & COB_READ_PREVIOUS)) {
			RETURN_STATUS (COB_STATUS_46_READ_ERROR);
		}
	}

	if (unlikely(f->open_mode == COB_OPEN_CLOSED
	    || f->open_mode == COB_OPEN_OUTPUT
	    || f->open_mode == COB_OPEN_EXTEND)) {
		RETURN_STATUS (COB_STATUS_47_INPUT_DENIED);
	}

#ifdef	USE_DB41
	if (f->organization == COB_ORG_INDEXED && bdb_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O  ||
		    (f->lock_mode & COB_LOCK_EXCLUSIVE)) {
			read_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
		   !(read_opts & COB_READ_NO_LOCK)) {
			read_opts |= COB_READ_LOCK; 
		}
	} else {
		read_opts &= ~COB_READ_LOCK;
	}
#endif
	if (key) {
		ret = fileio_funcs[(int)f->organization]->read (f, key, read_opts);
	} else {
		ret = fileio_funcs[(int)f->organization]->read_next (f, read_opts);
	}

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (f->record_size && f->organization != COB_ORG_LINE_SEQUENTIAL) {
			cob_set_int (f->record_size, (int) f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		if (read_opts & COB_READ_PREVIOUS) {
			f->flag_begin_of_file = 1;
		} else {
			f->flag_end_of_file = 1;
		}
		break;
	}

	RETURN_STATUS (ret);
}

void
cob_read (cob_file *f, cob_field *key, cob_field *fnstatus, int read_opts)
{
	int	status;
	char	sbuff[3];
	char	openMode[OPENMODESIZE];
	char	readOpts[READOPTSSIZE];

	memset (openMode, 0, sizeof (openMode));
	memset (readOpts, 0, sizeof (readOpts));
	sprintf (openMode, "%02d", f->last_open_mode);
	sprintf (readOpts, "%03d", read_opts);
	if (cob_invoke_fun (COB_IO_READ, (char*)f, key, NULL, fnstatus, openMode, NULL, readOpts)) {
		memset (sbuff, 0, sizeof (sbuff));
		if (fnstatus == NULL) {
			return;
		}
		memcpy (sbuff, fnstatus->data, 2);
		status = atoi (sbuff);
		RETURN_STATUS (status);
	}
	cob_ex_read (f, key, fnstatus, read_opts);
}

void
cob_ex_write (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	int	ret;
	int	tmpsize;

	f->flag_read_done = 0;

	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->open_mode == COB_OPEN_CLOSED
		    || f->open_mode == COB_OPEN_INPUT
		    || f->open_mode == COB_OPEN_I_O) {
			RETURN_STATUS (COB_STATUS_48_OUTPUT_DENIED);
		}
	} else {
		if (f->open_mode == COB_OPEN_CLOSED
		    || f->open_mode == COB_OPEN_INPUT
		    || f->open_mode == COB_OPEN_EXTEND) {
			RETURN_STATUS (COB_STATUS_48_OUTPUT_DENIED);
		}
	}
	tmpsize = f->record->size;
	if (f->record_size) {
		f->record->size = cob_get_int (f->record_size);
	} else {
		f->record->size = rec->size;
	}

	if (f->record->size < f->record_min || f->record_max < f->record->size) {
		RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);
	}

/* RXW
#ifdef	USE_DB41
	if (f->organization != COB_ORG_INDEXED || bdb_env == NULL) {
		opt &= ~COB_WRITE_LOCK;
	}
#endif
*/

	ret = fileio_funcs[(int)f->organization]->write (f, opt);

	if (unlikely(cob_do_sync && ret == 0)) {
		cob_sync (f, cob_do_sync);
	}
	f->record->size = tmpsize;

	RETURN_STATUS (ret);
}

void
cob_write (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	char	openMode[OPENMODESIZE];

	if (f->access_mode == COB_ACCESS_SEQUENTIAL &&
	    f->open_mode == COB_OPEN_I_O &&
	    cob_io_rewrite_assumed()) {
		cob_rewrite (f, rec, opt, fnstatus);
		return;
	}

	memset (openMode, 0, sizeof (openMode));
	sprintf (openMode, "%02d", f->last_open_mode);
	if (cob_invoke_fun (COB_IO_WRITE, (char*)f, NULL, (char*)rec->data, fnstatus, openMode, NULL, NULL)) {
		return;
	}
	cob_ex_write (f, rec, opt, fnstatus);
}

void
cob_ex_rewrite (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	int	ret;
	int	read_done = f->flag_read_done;

	f->flag_read_done = 0;

	if (unlikely(f->open_mode == COB_OPEN_CLOSED ||
	    f->open_mode != COB_OPEN_I_O)) {
		RETURN_STATUS (COB_STATUS_49_I_O_DENIED);
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		RETURN_STATUS (COB_STATUS_43_READ_NOT_DONE);
	}

	if (f->organization == COB_ORG_SEQUENTIAL) {
		if (f->record->size != rec->size) {
			RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);
		}

		if (f->record_size) {
			if (f->record->size != (size_t)cob_get_int (f->record_size)) {
				RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);
			}
		}
	}

/* RXW
#ifdef	USE_DB41
	if (f->organization != COB_ORG_INDEXED || bdb_env == NULL) {
		opt &= ~COB_WRITE_LOCK;
	}
#endif
*/

	ret = fileio_funcs[(int)f->organization]->rewrite (f, opt);

	if (unlikely(cob_do_sync && ret == 0)) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

void
cob_rewrite (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	char	openMode[OPENMODESIZE];

	memset (openMode, 0, sizeof (openMode));
	sprintf (openMode, "%02d", f->last_open_mode);
	if (cob_invoke_fun (COB_IO_REWRITE, (char*)f, NULL, (char*)rec->data, fnstatus, openMode, NULL, NULL)) {
		return;
	}
	cob_ex_rewrite (f, rec, opt, fnstatus);
}

void
cob_ex_delete (cob_file *f, cob_field *fnstatus)
{
	int	ret;
	int	read_done = f->flag_read_done;

	f->flag_read_done = 0;

	if (unlikely(f->open_mode == COB_OPEN_CLOSED ||
	    f->open_mode != COB_OPEN_I_O)) {
		RETURN_STATUS (COB_STATUS_49_I_O_DENIED);
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		RETURN_STATUS (COB_STATUS_43_READ_NOT_DONE);
	}

	ret = fileio_funcs[(int)f->organization]->fdelete (f);

	if (unlikely(cob_do_sync && ret == 0)) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

void
cob_delete (cob_file *f, cob_field *fnstatus)
{
	char	openMode[OPENMODESIZE];

	memset (openMode, 0, sizeof (openMode));
	sprintf (openMode, "%02d", f->last_open_mode);
	if (cob_invoke_fun (COB_IO_DELETE, (char*)f, NULL, NULL, fnstatus, openMode, NULL, NULL)) {
		return;
	}
	cob_ex_delete (f, fnstatus);
}

void
cob_ex_delete_file (cob_file *f, cob_field *fnstatus)
{
	char		*p;
	char		*src;
	char		*dst;
	size_t		i;
	size_t		simple;
	int		ret;

	f->flag_read_done = 0;

	/* file was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		RETURN_STATUS (COB_STATUS_38_CLOSED_WITH_LOCK);
	}

	/* file is already open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		RETURN_STATUS (COB_STATUS_41_ALREADY_OPEN);
	}

	if (f->special) {
		RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
	}

	/* obtain the file name */
	cob_field_to_string (f->assign, file_open_name);

	if (cob_current_module->flag_filename_mapping) {
		src = file_open_name;
		dst = file_open_buff;
		simple = 1;
		/* expand envoronment variables */
		/* ex. "$TMPDIR/foo" -> "/tmp/foo" */
		while (*src) {
			if (!isalnum (*src) && *src != '_' && *src != '-') {
				simple = 0;
			}
			if (*src == '$') {
				for (i = 1; ; i++) {
					if (!isalnum (src[i]) && src[i] != '_' && *src != '-') {
						break;
					}
				}
				memcpy (file_open_env, src + 1, i - 1);
				file_open_env[i - 1] = 0;
				if ((p = getenv (file_open_env)) != NULL) {
					strcpy (dst, p);
					dst += strlen (p);
				}
				src += i;
			} else {
				*dst++ = *src++;
			}
		}
		*dst = 0;
		cb_get_jisword_buff (file_open_buff, file_open_name, COB_SMALL_BUFF);

		/* resolve by environment variables */
		/* ex. "TMPFILE" -> DD_TMPFILE, dd_TMPFILE, or TMPFILE */
		if (simple) {
			for (i = 0; i < NUM_PREFIX; i++) {
				snprintf (file_open_buff, COB_SMALL_MAX, "%s%s",
					  prefix[i], file_open_name);
				if ((p = getenv (file_open_buff)) != NULL) {
					strncpy (file_open_name, p, COB_SMALL_MAX);
					break;
				}
			}
			if (i == NUM_PREFIX && cob_file_path) {
				snprintf (file_open_buff, COB_SMALL_MAX, "%s/%s",
					  cob_file_path, file_open_name);
				strncpy (file_open_name, file_open_buff, COB_SMALL_MAX);
			}
		}
	}

#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM)
	if (f->organization == COB_ORG_INDEXED) {
		strncpy (file_open_buff, file_open_name, COB_SMALL_MAX);
		strcat (file_open_buff, ".idx");
		ret = unlink (file_open_buff);
		if (ret == 0) {
			strncpy (file_open_buff, file_open_name, COB_SMALL_MAX);
			strcat (file_open_buff, ".dat");
			ret = unlink (file_open_buff);
		}
	} else {
#elif	defined(WITH_DB) /* WITH_CISAM || WITH_DISAM || WITH_VBISAM */
	if (f->organization == COB_ORG_INDEXED) {
		RETURN_STATUS (COB_STATUS_91_NOT_AVAILABLE);
	} else {
#else /* WITH_CISAM || WITH_DISAM || WITH_VBISAM */
	{
#endif /* WITH_CISAM || WITH_DISAM || WITH_VBISAM */
		ret = unlink (file_open_name);
	}
	if (ret == 0) {
		RETURN_STATUS (COB_STATUS_00_SUCCESS);
	} else {
		switch (errno) {
		case ENOENT:
			RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
		case EACCES:
		case EISDIR:
		case EROFS:
			RETURN_STATUS (COB_STATUS_37_PERMISSION_DENIED);
		case EAGAIN:
		case COB_STATUS_61_FILE_SHARING:
			RETURN_STATUS (COB_STATUS_61_FILE_SHARING);
		case COB_STATUS_91_NOT_AVAILABLE:
			RETURN_STATUS (COB_STATUS_91_NOT_AVAILABLE);
		case COB_LINAGE_INVALID:
			RETURN_STATUS (COB_STATUS_57_I_O_LINAGE);
		default:
			RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
		}
	}
}

void
cob_delete_file (cob_file *f, cob_field *fnstatus)
{
	char	openMode[OPENMODESIZE];

	memset (openMode, 0, sizeof (openMode));
	sprintf (openMode, "%02d", f->last_open_mode);
	if (cob_invoke_fun (COB_IO_DELETE_FILE, (char*)f, NULL, NULL, fnstatus, openMode, NULL, NULL)) {
		return;
	}
	cob_ex_delete_file (f, fnstatus);
}

void
cob_ex_commit (void)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		cob_file_unlock (l->file);
	}
}

void
cob_commit (void)
{
	if (cob_invoke_fun (COB_IO_COMMIT, NULL, NULL, NULL, NULL, NULL, NULL, NULL)) {
		return;
	}
	cob_ex_commit ();
}

void
cob_ex_rollback (void)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		cob_file_unlock (l->file);
	}
}

void
cob_rollback (void)
{
	if (cob_invoke_fun (COB_IO_ROLLBACK, NULL, NULL, NULL, NULL, NULL, NULL, NULL)) {
		return;
	}
	cob_ex_rollback ();
}

void
cob_default_error_handle (void)
{
	const char	*msg;
	unsigned char	*file_status;
	char		*filename;
	int		status;

	file_status = cob_error_file->file_status;
	status = cob_d2i(file_status[0]) * 10 + cob_d2i(file_status[1]);
	switch (status) {
	case COB_STATUS_10_END_OF_FILE:
		msg = "End of file";
		break;
	case COB_STATUS_14_OUT_OF_KEY_RANGE:
		msg = "Key out of range";
		break;
	case COB_STATUS_21_KEY_INVALID:
		msg = "Key order not ascending";
		break;
	case COB_STATUS_22_KEY_EXISTS:
		msg = "Record key already exists";
		break;
	case COB_STATUS_23_KEY_NOT_EXISTS:
		msg = "Record key does not exist";
		break;
	case COB_STATUS_30_PERMANENT_ERROR:
		msg = "Permanent file error";
		break;
	case COB_STATUS_35_NOT_EXISTS:
		msg = "File does not exist";
		break;
	case COB_STATUS_37_PERMISSION_DENIED:
		msg = "Permission denied";
		break;
	case COB_STATUS_41_ALREADY_OPEN:
		msg = "File already open";
		break;
	case COB_STATUS_42_NOT_OPEN:
		msg = "File not open";
		break;
	case COB_STATUS_43_READ_NOT_DONE:
		msg = "READ must be executed first";
		break;
	case COB_STATUS_44_RECORD_OVERFLOW:
		msg = "Record overflow";
		break;
	case COB_STATUS_46_READ_ERROR:
		msg = "Failed to read";
		break;
	case COB_STATUS_47_INPUT_DENIED:
		msg = "READ/START not allowed";
		break;
	case COB_STATUS_48_OUTPUT_DENIED:
		msg = "WRITE not allowed";
		break;
	case COB_STATUS_49_I_O_DENIED:
		msg = "DELETE/REWRITE not allowed";
		break;
	case COB_STATUS_51_RECORD_LOCKED:
		msg = "Record locked by another file connector";
		break;
	case COB_STATUS_52_EOP:
		msg = "A page overflow condition occurred";
		break;
	case COB_STATUS_57_I_O_LINAGE:
		msg = "LINAGE values invalid";
		break;
	case COB_STATUS_61_FILE_SHARING:
		msg = "File sharing conflict";
		break;
	case COB_STATUS_91_NOT_AVAILABLE:
		msg = "Runtime library is not configured for this operation";
		break;
	default:
		msg = "Unknown file error";
		break;
	}

	filename = cob_malloc (COB_MEDIUM_BUFF);
	cob_field_to_string (cob_error_file->assign, filename);
	cob_runtime_error ("%s (STATUS = %02d) File : '%s'", msg,
				status, filename);
	free (filename);
}

void
cob_init_fileio (void)
{
	char	*s;
	int	n;

	if ((s = getenv ("COB_SYNC")) != NULL) {
		if (*s == 'Y' || *s == 'y') {
			cob_do_sync = 1;
		}
		if (*s == 'P' || *s == 'p') {
			cob_do_sync = 2;
		}
	}
	if ((s = getenv ("COB_SORT_MEMORY")) != NULL) {
		n = atoi (s);
		if (n >= 1024*1024) {
			cob_sort_memory = n;
		}
	}
	cob_file_path = getenv ("COB_FILE_PATH");
	if (cob_file_path) {
		if (!*cob_file_path || *cob_file_path == ' ') {
			cob_file_path = NULL;
		}
	}
	cob_ls_nulls = getenv ("COB_LS_NULLS");
	cob_ls_fixed = getenv ("COB_LS_FIXED");

	file_open_env = cob_malloc (COB_SMALL_BUFF);
	file_open_name = cob_malloc (COB_SMALL_BUFF);
	file_open_buff = cob_malloc (COB_SMALL_BUFF);

#ifdef	USE_DB41
	bdb_home = getenv ("DB_HOME");
	join_environment ();
	record_lock_object = cob_malloc (1024);
	bdb_buff = cob_malloc (COB_SMALL_BUFF);
	rlo_size = 1024;
#endif

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
	extfh_cob_init_fileio (&sequential_funcs, &lineseq_funcs, &relative_funcs, &cob_file_write_opt);
#endif

}

void
cob_exit_fileio (void)
{
	struct file_list	*l;
	char			*str_logic_filename = NULL;
	char			*str_physical_filename = NULL;

	for (l = file_cache; l; l = l->next) {
		if (l->file->open_mode != COB_OPEN_CLOSED &&
		     l->file->open_mode != COB_OPEN_LOCKED) {
			if(l->file->assign == NULL) {
				strncpy (runtime_buffer, l->file->select_name, COB_SMALL_MAX);
			} else {
				cob_field_to_string (l->file->assign, runtime_buffer);
			}
			cob_close (l->file, 0, NULL);
			str_logic_filename = cb_get_jisword (l->file->select_name);
			str_physical_filename = cb_get_jisword (runtime_buffer);
			fprintf (stderr, "WARNING - Implicit CLOSE of %s (\"%s\")\n",
				 str_logic_filename, str_physical_filename);
			if (str_logic_filename) {
				free (str_logic_filename);
				str_logic_filename = NULL;
			}
			if (str_physical_filename) {
				free (str_physical_filename);
				str_physical_filename = NULL;
			}
			fflush (stderr);
		}
	}
#ifdef	USE_DB41
	free (record_lock_object);
	if (bdb_env) {
		bdb_env->lock_id_free (bdb_env, bdb_lock_id);
		bdb_env->close (bdb_env, 0);
	}
#endif
#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
	extfh_cob_exit_fileio ();
#endif
}

/* System routines */

static void * COB_NOINLINE
cob_str_from_fld (const cob_field *f)
{
	void		*mptr;
	unsigned char	*s;
	int		i;
	int		n;
	int		quote_switch;

	if (!f) {
		return cob_malloc (1);
	}
	for (i = (int) f->size - 1; i >= 0; i--) {
		if (f->data[i] != ' ' && f->data[i] != 0) {
			break;
		}
	}
	i++;
	/* i is 0 or > 0 */
	mptr = cob_malloc ((size_t)(i + 1));
	quote_switch = 0;
	s = mptr;
	for (n = 0; n < i; n++) {
		if (f->data[n] == '"') {
			quote_switch = !quote_switch;
			continue;
		}
		s[n] = f->data[n];
		if (quote_switch) {
			continue;
		}
		if (s[n] == ' ' || s[n] == 0) {
			s[n] = 0;
			break;
		}
		
	}
	return mptr;
}

static int COB_NOINLINE
open_cbl_file (unsigned char *file_name, unsigned char *file_access,
	       unsigned char *file_handle, const int file_flags)
{
	char	*fn;
	int	flag = O_BINARY;
	int	fd;

	COB_UNUSED (file_name);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		memset (file_handle, -1, 4);
		return -1;
	}
	flag |= file_flags;
	switch (*file_access & 0x3f) {
		case 1:
			flag |= O_RDONLY;
			break;
		case 2:
			flag |= O_CREAT | O_TRUNC | O_WRONLY;
			break;
		case 3:
			flag |= O_RDWR;
			break;
		default:
			memset (file_handle, -1, 4);
			return -1;
	}
	fn = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
	fd = open (fn, flag, 0660);
	if (fd < 0) {
		free (fn);
		memset (file_handle, -1, 4);
		return 35;
	}
	free (fn);
	memcpy (file_handle, &fd, 4);
	return 0;
}

int
CBL_OPEN_FILE (unsigned char *file_name, unsigned char *file_access,
	       unsigned char *file_lock, unsigned char *file_dev,
	       unsigned char *file_handle)
{
	COB_UNUSED (file_lock);
	COB_UNUSED (file_dev);

	COB_CHK_PARMS (CBL_OPEN_FILE, 5);

	return open_cbl_file (file_name, file_access, file_handle, 0);
}

int
CBL_CREATE_FILE (unsigned char *file_name, unsigned char *file_access,
		 unsigned char *file_lock, unsigned char *file_dev,
		 unsigned char *file_handle)
{
	COB_UNUSED (file_lock);
	COB_UNUSED (file_dev);

	COB_CHK_PARMS (CBL_CREATE_FILE, 5);

	return open_cbl_file (file_name, file_access, file_handle, O_CREAT | O_TRUNC);
}

int
CBL_READ_FILE (unsigned char *file_handle, unsigned char *file_offset,
	       unsigned char *file_len, unsigned char *flags, unsigned char *buf)
{
	long long	off;
	int		fd;
	int		len;
	int		rc;
	struct stat	st;

	COB_CHK_PARMS (CBL_READ_FILE, 5);

	rc = 0;
	memcpy (&fd, file_handle, 4);
	memcpy (&off, file_offset, 8);
	memcpy (&len, file_len, 4);
#ifndef	WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) < 0) {
		return -1;
	}
	if (len > 0) {
		rc = read (fd, buf, (size_t)len);
		if (rc < 0) {
			rc = -1;
		} else if (rc == 0) {
			rc = 10;
		} else {
			rc = 0;
		}
	}
	if ((*flags & 0x80) != 0) {
		if (fstat (fd, &st) < 0) {
			return -1;
		}
		off = st.st_size;
#ifndef	WORDS_BIGENDIAN
		off = COB_BSWAP_64 (off);
#endif
		memcpy (file_offset, &off, 8);
	}
	return rc;
}

int
CBL_WRITE_FILE (unsigned char *file_handle, unsigned char *file_offset,
		unsigned char *file_len, unsigned char *flags, unsigned char *buf)
{
	long long	off;
	int		fd;
	int		len;
	int		rc;

	COB_UNUSED (flags);

	COB_CHK_PARMS (CBL_WRITE_FILE, 5);

	memcpy (&fd, file_handle, 4);
	memcpy (&off, file_offset, 8);
	memcpy (&len, file_len, 4);
#ifndef WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) < 0) {
		return -1;
	}
	rc = write (fd, buf, (size_t)len);
	if (rc < 0) {
		return 30;
	}
	return 0;
}

int
CBL_CLOSE_FILE (unsigned char *file_handle)
{
	int	fd;

	COB_CHK_PARMS (CBL_CLOSE_FILE, 1);

	memcpy (&fd, file_handle, 4);
	return close (fd);
}

int
CBL_FLUSH_FILE (unsigned char *file_handle)
{
	COB_UNUSED (file_handle);

	COB_CHK_PARMS (CBL_FLUSH_FILE, 1);

	return 0;
}

int
CBL_DELETE_FILE (unsigned char *file_name)
{
	char	*fn;
	int	ret;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_DELETE_FILE, 1);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	fn = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
	ret = unlink (fn);
	free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
CBL_COPY_FILE (unsigned char *fname1, unsigned char *fname2)
{
	char	*fn1;
	char	*fn2;
	char	buf[COB_SMALL_BUFF];
	int	flag = O_BINARY;
	int	ret;
	int	i;
	int	fd1, fd2;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_COPY_FILE, 2);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[1]) {
		return -1;
	}
	fn1 = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
	flag |= O_RDONLY;
	fd1 = open (fn1, flag, 0);
	if (fd1 < 0) {
		free (fn1);
		return -1;
	}
	free (fn1);
	fn2 = cob_str_from_fld (cob_current_module->cob_procedure_parameters[1]);
	flag &= ~O_RDONLY;
	flag |= O_CREAT | O_TRUNC | O_WRONLY;
	fd2 = open (fn2, flag, 0660);
	if (fd2 < 0) {
		close (fd1);
		free (fn2);
		return -1;
	}
	free (fn2);
	ret = 0;
	while ((i = read (fd1, buf, sizeof(buf))) > 0) {
		if (write (fd2, buf, (size_t)i) < 0) {
			ret = -1;
			break;
		}
	}
	close (fd1);
	close (fd2);
	return ret;
}

int
CBL_CHECK_FILE_EXIST (unsigned char *file_name, unsigned char *file_info)
{
	char		*fn;
	struct tm	*tm;
	long long	sz;
	struct stat	st;
	short		y;
	char		d, m, hh, mm, ss;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_CHECK_FILE_EXIST, 2);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	fn = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
	if (stat (fn, &st) < 0) {
		free (fn);
		return 35;
	}
	free (fn);
	sz = st.st_size;
	tm = localtime (&st.st_mtime);
	d = (char) tm->tm_mday;
	m = (char) tm->tm_mon + 1;
	y = tm->tm_year + 1900;
	hh = (char) tm->tm_hour;
	mm = (char) tm->tm_min;
	ss = (char) tm->tm_sec;

#ifndef WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
	y = COB_BSWAP_16 (y);
#endif
	memcpy (file_info, &sz, 8);
	file_info[8] = d;
	file_info[9] = m;
	memcpy (file_info+10, &y, 2);
	file_info[12] = hh;
	file_info[13] = mm;
	file_info[14] = ss;
	file_info[15] = 0;
	return 0;
}

int
CBL_RENAME_FILE (unsigned char *fname1, unsigned char *fname2)
{
	char	*fn1;
	char	*fn2;
	int	ret;

	COB_CHK_PARMS (CBL_RENAME_FILE, 2);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	if (!cob_current_module->cob_procedure_parameters[1]) {
		return -1;
	}
	fn1 = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
	fn2 = cob_str_from_fld (cob_current_module->cob_procedure_parameters[1]);
	ret = rename (fn1, fn2);
	free (fn1);
	free (fn2);
	if (ret) {
		return 128;
	}
	return 0;
}

int
CBL_GET_CURRENT_DIR (const int flags, const int dir_length, unsigned char *dir)
{
	char	*dirname;
	int	dir_size;
	int	has_space;

	COB_CHK_PARMS (CBL_GET_CURRENT_DIR, 3);

	if (dir_length < 1) {
		return 128;
	}
	if (flags) {
		return 129;
	}
	memset (dir, ' ', (size_t)dir_length);
	dirname = getcwd (NULL, 0);
	if (dirname == NULL) {
		return 128;
	}
	dir_size = (int) strlen (dirname);
	has_space = 0;
	if (strchr (dirname, ' ')) {
		has_space = 2;
	}
	if (dir_size + has_space > dir_length) {
		free (dirname);
		return 128;
	}
	if (has_space) {
		*dir = '"';
		memcpy (&dir[1], dirname, (size_t)dir_size);
		dir[dir_size + 1] = '"';
	} else {
		memcpy (dir, dirname, (size_t)dir_size);
	}
	free (dirname);
	return 0;
}

int
CBL_CREATE_DIR (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_CHK_PARMS (CBL_CREATE_DIR, 1);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	fn = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
#ifdef	_WIN32
	ret = mkdir (fn);
#else
	ret = mkdir (fn, 0770);
#endif
	free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
CBL_CHANGE_DIR (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_CHK_PARMS (CBL_CHANGE_DIR, 1);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	fn = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
	ret = chdir (fn);
	free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
CBL_DELETE_DIR (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_CHK_PARMS (CBL_DELETE_DIR, 1);

	if (!cob_current_module->cob_procedure_parameters[0]) {
		return -1;
	}
	fn = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
	ret = rmdir (fn);
	free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_acuw_mkdir (unsigned char *dir)
{
	int		ret;

	COB_CHK_PARMS (C$MAKEDIR, 1);

	ret = CBL_CREATE_DIR (dir);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

int
cob_acuw_chdir (unsigned char *dir, unsigned char *status)
{
	int		ret;

	COB_CHK_PARMS (C$CHDIR, 2);

	ret = CBL_CHANGE_DIR (dir);
	if (ret < 0) {
		ret = 128;
	}
	cob_set_int (cob_current_module->cob_procedure_parameters[1], ret);
	return ret;
}

int
cob_acuw_copyfile (unsigned char *fname1, unsigned char *fname2, unsigned char *file_type)
{
	int		ret = 128;

	/* RXW - Type is not yet evaluated */

	COB_CHK_PARMS (C$COPY, 3);

	if (cob_call_params < 3) {
		return 128;
	}
	ret = CBL_COPY_FILE (fname1, fname2);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

int
cob_acuw_file_info (unsigned char *file_name, unsigned char *file_info)
{
	char			*fn;
	struct tm		*tm;
	unsigned long long	sz;
	unsigned int		dt;
	short			y;
	short			d, m, hh, mm, ss;
	struct stat		st;

	COB_CHK_PARMS (C$FILEINFO, 2);

	if (cob_call_params < 2 || !cob_current_module->cob_procedure_parameters[0]) {
		return 128;
	}
	fn = cob_str_from_fld (cob_current_module->cob_procedure_parameters[0]);
	if (stat (fn, &st) < 0) {
		free (fn);
		return 35;
	}
	free (fn);
	sz = st.st_size;
	tm = localtime (&st.st_mtime);
	d = tm->tm_mday;
	m = tm->tm_mon + 1;
	y = tm->tm_year + 1900;
	hh = tm->tm_hour;
	mm = tm->tm_min;
	ss = tm->tm_sec;

#ifndef WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
#endif
	memcpy (file_info, &sz, 8);
	dt = (y * 10000) + (m * 100) + d;
#ifndef WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 8, &dt, 4);
	dt = (hh * 1000000) + (mm * 10000) + (ss * 100);
#ifndef WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 12, &dt, 4);
	return 0;
}

int
cob_acuw_file_delete (unsigned char *file_name, unsigned char *file_type)
{
	int	ret;

	/* RXW - Type is not yet evaluated */
	COB_CHK_PARMS (C$DELETE, 2);

	if (cob_call_params < 2 || !cob_current_module->cob_procedure_parameters[0]) {
		return 128;
	}
	ret = CBL_DELETE_FILE (file_name);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

static int
cob_listdir_open (cob_field *f_dirname, cob_field *f_pattern)
{
	//FIXME: now not use file pattern(ex. *).
#ifdef _WIN32
	char *dirname = cob_str_from_fld (f_dirname);
	char *pattern = cob_str_from_fld (f_pattern);

	LPCTSTR lpFileName = cob_malloc (strlen(dirname) + 1 + strlen (pattern) + 1);

	if (listdir_filedata == NULL) {
		listdir_filedata = cob_malloc (sizeof (LPWIN32_FIND_DATA));
	}

	strcpy (lpFileName, dirname);
	strcat (lpFileName, "\\");
	strcat (lpFileName, pattern);
	listdir_handle = FindFirstFile (lpFileName, listdir_filedata);
	free (dirname);
	free (pattern);
	free (lpFileName);
	if (listdir_handle == INVALID_HANDLE_VALUE) {
		return 0;
	}

#else
	char *dirname = cob_str_from_fld (f_dirname);
	listdir_handle = opendir (dirname);
	free (dirname);
	if (listdir_handle == NULL) {
		return 0;
	}
	
#endif
	//FIXME: now not use handle.
	return 0;
}

static int
cob_listdir_next (cob_field *f_handle, cob_field *f_filename)
{
	//FIXME: now not use handle.
	char *filename;
	int length;

#ifdef _WIN32
	filename = listdir_filedata->cFileName;
#else
	listdir_filedata = readdir (listdir_handle);
	if (listdir_filedata == NULL) {
		filename = NULL;
	}else{
		filename = listdir_filedata->d_name;
	}
#endif
	memset (f_filename->data, ' ', f_filename->size);

	if(filename != NULL){
		length = strlen (filename);
		if (length > f_filename->size) {
			length = f_filename->size;
		}
		memcpy (f_filename->data, filename, length);
	}
#ifdef _WIN32
	if (!FindNextFile (listdir_handle, listdir_filedata)) {
		strcpy (listdir_filedata->cFileName, " ");
	}
#endif
	return 0;
}

static int
cob_listdir_close (cob_field *f_handle)
{
	//FIXME: now not use handle.
#ifdef _WIN32
	FindClose (listdir_handle);
#else
	closedir (listdir_handle);
#endif
	return 0;
}

int
cob_acuw_list_directory (unsigned char *data, ...)
{
	int operation_code = -1;
	int return_code;

	COB_CHK_PARMS (C$LIST-DIRECTORY, 1);

	if (cob_current_module->cob_procedure_parameters[0] == NULL) {
		return -1;
	}

	operation_code = cob_get_int (cob_current_module->cob_procedure_parameters[0]);

	switch (operation_code)
	{
	case 1://LISTDIR-OPEN(value:1)
		return_code = cob_listdir_open (cob_current_module->cob_procedure_parameters[1],
			cob_current_module->cob_procedure_parameters[2]);
		break;
	case 2://LISTDIR-NEXT(value:2)
		return_code = cob_listdir_next (cob_current_module->cob_procedure_parameters[1],
			cob_current_module->cob_procedure_parameters[2]);
		break;
	case 3://LISTDIR-CLOSE(value:3)
		return_code = cob_listdir_close (cob_current_module->cob_procedure_parameters[1]);
		break;
	default:
		//error
		return -1;
	}
	return return_code;
}

/* SORT */

static int
sort_cmps (const unsigned char *s1, const unsigned char *s2, const size_t size,
		const unsigned char *col)
{
	size_t			i;
	int			ret;

	if (unlikely(col)) {
		for (i = 0; i < size; i++) {
			if ((ret = col[s1[i]] - col[s2[i]]) != 0) {
				return ret;
			}
		}
	} else {
		for (i = 0; i < size; i++) {
			if ((ret = s1[i] - s2[i]) != 0) {
				return ret;
			}
		}
	}
	return 0;
}

static COB_INLINE void
unique_copy (unsigned char *s1, unsigned char *s2)
{
	size_t	size = sizeof(size_t);

	do {
		*s1++ = *s2++;
	} while (--size);
}

static int
cob_file_sort_compare (struct cobitem *k1, struct cobitem *k2, void *pointer)
{
	cob_file	*f;
	size_t		i;
	int		cmp;
	size_t		u1;
	size_t		u2;
	cob_field	f1;
	cob_field	f2;

	f = pointer;
	for (i = 0; i < f->nkeys; i++) {
		f1 = f2 = *(f->keys[i].field);
		f1.data = k1->item + f->keys[i].offset;
		f2.data = k2->item + f->keys[i].offset;
		if (COB_FIELD_IS_NUMERIC(&f1)) {
			cmp = cob_numeric_cmp (&f1, &f2);
		} else {
			cmp = sort_cmps (f1.data, f2.data, f1.size, f->sort_collating);
		}
		if (cmp != 0) {
			return (f->keys[i].flag == COB_ASCENDING) ? cmp : -cmp;
		}
	}
	unique_copy ((unsigned char *)&u1, k1->unique);
	unique_copy ((unsigned char *)&u2, k2->unique);
	if (u1 < u2) {
		return -1;
	}
	return 1;
}

static void
cob_free_list (struct cobitem *q)
{
	struct cobitem	*next;

	while (q != NULL) {
		next = q->next;
		free (q);
		q = next;
	}
}

static struct cobitem *
cob_new_item (struct cobsort *hp, size_t size)
{
	struct cobitem *q;

	if (hp->empty != NULL) {
		q = hp->empty;
		hp->empty = q->next;
	} else {
		q = cob_malloc (size);
	}
	return q;
}

static FILE * COB_NOINLINE
cob_tmpfile (void)
{
	FILE		*fp;
	const char	*s;
	int		fd;
	char		*filename;
#ifdef _WIN32
	char		*tmpdir;
#endif

	filename = cob_malloc (COB_MEDIUM_BUFF);

#ifdef _WIN32
	/* get temporary directory */
	tmpdir = cob_malloc (COB_MEDIUM_BUFF);
	if ((s = getenv ("TMPDIR")) != NULL ||
	    (s = getenv ("TMP")) != NULL ||
	    (s = getenv ("TEMP")) != NULL) {
		strncpy (tmpdir, s, COB_MEDIUM_MAX);
	} else {
		GetTempPath (COB_MEDIUM_BUFF, tmpdir);
	}
	/* get temporary file name */
	GetTempFileName (tmpdir, "cob", 0, filename);
	DeleteFile (filename);
	free (tmpdir);
	fd = _open (filename, _O_CREAT | _O_TRUNC | _O_RDWR | _O_BINARY, 0660);
#else
	if ((s = getenv ("TMPDIR")) == NULL &&
	    (s = getenv ("TMP")) == NULL &&
	    (s = getenv ("TEMP")) == NULL) {
		s = "/tmp";
	}
	if (cob_process_id == 0) {
		cob_process_id = getpid ();
	}
	snprintf (filename, COB_MEDIUM_MAX, "%s/cobsort%d_%d",
		  s, cob_process_id, cob_iteration);
	cob_iteration++;
	fd = open (filename, O_CREAT | O_TRUNC | O_RDWR | O_BINARY | O_LARGEFILE, 0660);
#endif
	if (fd < 0) {
		free (filename);
		return NULL;
	}
#ifdef _WIN32
	_unlink (filename);
	fp = _fdopen (fd, "w+b");
	if (!fp) {
		_close (fd);
	}
#else
	unlink (filename);
	fp = fdopen (fd, "w+b");
	if (!fp) {
		close (fd);
	}
#endif
	free (filename);
	return fp;
}

static int COB_NOINLINE
cob_get_temp_file (struct cobsort *hp, const int n)
{
	if (hp->file[n].fp == NULL) {
		hp->file[n].fp = cob_tmpfile ();
		if (hp->file[n].fp == NULL) {
			cob_runtime_error ("SORT is unable to acquire temporary file");
			cob_stop_run (1);
		}
	} else {
		rewind (hp->file[n].fp);
	}
	hp->file[n].count = 0;
	return hp->file[n].fp == NULL;
}

static int
cob_sort_queues (struct cobsort *hp)
{
	struct cobitem	*q;
	int		source = 0;
	int		destination;
	int		move;
	int		n;
	int		end_of_block[2];

	while (hp->queue[source + 1].count != 0) {
		destination = source ^ 2;
		hp->queue[destination].count = hp->queue[destination + 1].count = 0;
		hp->queue[destination].first = hp->queue[destination + 1].first = NULL;
		for (;;) {
			end_of_block[0] = hp->queue[source].count == 0;
			end_of_block[1] = hp->queue[source + 1].count == 0;
			if (end_of_block[0] && end_of_block[1]) {
				break;
			}
			while (!end_of_block[0] || !end_of_block[1]) {
				if (end_of_block[0]) {
					move = 1;
				} else if (end_of_block[1]) {
					move = 0;
				} else {
					n = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = n < 0 ? 0 : 1;
				}
				q = hp->queue[source + move].first;
				if (q->end_of_block) {
					end_of_block[move] = 1;
				}
				hp->queue[source + move].first = q->next;
				if (hp->queue[destination].first == NULL) {
					hp->queue[destination].first = q;
				} else {
					hp->queue[destination].last->next = q;
				}
				hp->queue[destination].last = q;
				hp->queue[source + move].count--;
				hp->queue[destination].count++;
				q->next = NULL;
				q->end_of_block = 0;
			}
			hp->queue[destination].last->end_of_block = 1;
			destination ^= 1;
		}
		source = destination & 2;
	}
	return source;
}

static int
cob_read_item (struct cobsort *hp, const int n)
{
	FILE	*fp = hp->file[n].fp;

	if (getc (fp) != 0) {
		hp->queue[n].first->end_of_block = 1;
	} else {
		hp->queue[n].first->end_of_block = 0;
		if (unlikely(fread (hp->queue[n].first->unique, hp->r_size, 1, fp) != 1)) {
			return 1;
		}
	}
	return 0;
}

static int
cob_write_block (struct cobsort *hp, const int n)
{
	struct cobitem	*q;
	FILE		*fp = hp->file[hp->destination_file].fp;

	for (;;) {
		q = hp->queue[n].first;
		if (q == NULL) {
			break;
		}
		if (unlikely(fwrite (&(q->block_byte), hp->w_size, 1, fp) != 1)) {
			return 1;
		}
		hp->queue[n].first = q->next;
		q->next = hp->empty;
		hp->empty = q;
	}
	hp->queue[n].count = 0;
	hp->file[hp->destination_file].count++;
	if (putc (1, fp) != 1) {
		return 1;
	}
	return 0;
}

static void
cob_copy_check (cob_file *to, cob_file *from)
{
	unsigned char	*toptr;
	unsigned char	*fromptr;
	size_t		tosize;
	size_t		fromsize;

	toptr = to->record->data;
	fromptr = from->record->data;
	tosize = to->record->size;
	fromsize = from->record->size;
	if (unlikely(tosize > fromsize)) {
		memcpy (toptr, fromptr, fromsize);
		memset (toptr + fromsize, ' ', tosize - fromsize);
	} else {
		memcpy (toptr, fromptr, tosize);
	}
}

static int
cob_file_sort_process (struct cobsort *hp)
{
	int	i;
	int	source;
	int	destination;
	int	n;
	int	move;
	int	res;

	hp->retrieving = 1;
	n = cob_sort_queues (hp);
/* RXW - Cannot be true
	if (unlikely(n < 0)) {
		return COBSORTABORT;
	}
*/
	if (likely(!hp->files_used)) {
		hp->retrieval_queue = n;
		return 0;
	}
	if (unlikely(cob_write_block (hp, n))) {
		return COBSORTFILEERR;
	}
	for (i = 0; i < 4; i++) {
		hp->queue[i].first = hp->empty;
		hp->empty = hp->empty->next;
		hp->queue[i].first->next = NULL;
	}
	rewind (hp->file[0].fp);
	rewind (hp->file[1].fp);
	if (unlikely(cob_get_temp_file (hp, 2))) {
		return COBSORTFILEERR;
	}
	if (unlikely(cob_get_temp_file (hp, 3))) {
		return COBSORTFILEERR;
	}
	source = 0;
	while (hp->file[source].count > 1) {
		destination = source ^ 2;
		hp->file[destination].count = 0;
		hp->file[destination + 1].count = 0;
		while (hp->file[source].count > 0) {
			if (unlikely(cob_read_item (hp, source))) {
				return COBSORTFILEERR;
			}
			if (hp->file[source + 1].count > 0) {
				if (unlikely(cob_read_item (hp, source + 1))) {
					return COBSORTFILEERR;
				}
			} else {
				hp->queue[source + 1].first->end_of_block = 1;
			}
			while (!hp->queue[source].first->end_of_block
			       || !hp->queue[source + 1].first->end_of_block) {
				if (hp->queue[source].first->end_of_block) {
					move = 1;
				} else if (hp->queue[source + 1].first->end_of_block) {
					move = 0;
				} else {
					res = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = res < 0 ? 0 : 1;
				}
				if (unlikely(fwrite (
				    &(hp->queue[source + move].first->block_byte),
				    hp->w_size, 1,
				    hp->file[destination].fp) != 1)) {
					return COBSORTFILEERR;
				}
				if (unlikely(cob_read_item (hp, source + move))) {
					return COBSORTFILEERR;
				}
			}
			hp->file[destination].count++;
			if (unlikely(putc (1, hp->file[destination].fp) != 1)) {
				return COBSORTFILEERR;
			}
			hp->file[source].count--;
			hp->file[source + 1].count--;
			destination ^= 1;
		}
		source = destination & 2;
		rewind (hp->file[0].fp);
		rewind (hp->file[1].fp);
		rewind (hp->file[2].fp);
		rewind (hp->file[3].fp);
	}
	hp->retrieval_queue = source;
	if (unlikely(cob_read_item (hp, source))) {
		return COBSORTFILEERR;
	}
	if (unlikely(cob_read_item (hp, source + 1))) {
		return COBSORTFILEERR;
	}
	return 0;
}

static int
cob_file_sort_submit (cob_file *f, const unsigned char *p)
{
	struct cobsort		*hp;
/* RXW - See comment lines below
	size_t			i;
*/
	struct cobitem		*q;
	struct memory_struct	*z;
	int			n;

	hp = f->file;
	if (unlikely(!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely(hp->retrieving)) {
		return COBSORTABORT;
/* RXW - This was a facility to submit new items after retrieval had begun
		for (i = 0; i < 4; i++) {
			if (hp->queue[i].first != NULL) {
				hp->queue[i].last->next = hp->empty;
				hp->empty = hp->queue[i].first;
				hp->queue[i].first = NULL;
			}
		}
		hp->queue[0].count = hp->queue[1].count = 0;
		hp->destination_file = -1;
		hp->retrieving = 0;
		hp->files_used = 0;
*/
	}
	if (hp->queue[0].count + hp->queue[1].count >= hp->memory) {
		if (!hp->files_used) {
			if (unlikely(cob_get_temp_file (hp, 0))) {
				return COBSORTFILEERR;
			}
			if (unlikely(cob_get_temp_file (hp, 1))) {
				return COBSORTFILEERR;
			}
			hp->files_used = 1;
			hp->destination_file = 0;
		}
		n = cob_sort_queues (hp);
/* RXW - Cannot be true
		if (unlikely(n < 0)) {
			return COBSORTABORT;
		}
*/
		if (unlikely(cob_write_block (hp, n))) {
			return COBSORTFILEERR;
		}
		hp->destination_file ^= 1;
	}
	q = cob_new_item (hp, sizeof (struct cobitem) + hp->size);
	if (f->record_size) {
		q->record_size = cob_get_int (f->record_size);
	} else {
		q->record_size = hp->size;
	}
	q->end_of_block = 1;
	unique_copy (q->unique, (unsigned char *)&(hp->unique));
	hp->unique++;
	memcpy (q->item, p, hp->size);
	if (hp->queue[0].count <= hp->queue[1].count) {
		z = &hp->queue[0];
	} else {
		z = &hp->queue[1];
	}
	q->next = z->first;
	z->first = q;
	z->count++;
	return 0;
}

static int
cob_file_sort_retrieve (cob_file *f, unsigned char *p)
{
	struct cobsort		*hp;
	struct cobitem		*next;
	struct memory_struct	*z;
	int			move;
	int			source;
	int			res;

	hp = f->file;
	if (unlikely(!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely(!hp->retrieving)) {
		res = cob_file_sort_process (hp);
		if (res) {
			return res;
		}
	}
	if (unlikely(hp->files_used)) {
		source = hp->retrieval_queue;
		if (hp->queue[source].first->end_of_block) {
			if (hp->queue[source + 1].first->end_of_block) {
				return COBSORTEND;
			}
			move = 1;
		} else if (hp->queue[source + 1].first->end_of_block) {
			move = 0;
		} else {
			res = cob_file_sort_compare (hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
			move = res < 0 ? 0 : 1;
		}
		memcpy (p, hp->queue[source + move].first->item, hp->size);
		if (unlikely(cob_read_item (hp, source + move))) {
			return COBSORTFILEERR;
		}
	} else {
		z = &hp->queue[hp->retrieval_queue];
		if (z->first == NULL) {
			return COBSORTEND;
		}
		memcpy (p, z->first->item, hp->size);
		if (f->record_size) {
			cob_set_int (f->record_size, z->first->record_size);
		}
		next = z->first->next;
		z->first->next = hp->empty;
		hp->empty = z->first;
		z->first = next;
	}
	return 0;
}

void
cob_file_sort_using (cob_file *sort_file, cob_file *data_file)
{
	int		ret;

	cob_open (data_file, COB_OPEN_INPUT, 0, NULL);
	for (;;) {
		cob_read (data_file, NULL, NULL, COB_READ_NEXT);
		if (data_file->file_status[0] != '0') {
			break;
		}
		cob_copy_check (sort_file, data_file);
		ret = cob_file_sort_submit (sort_file, sort_file->record->data);
		if (ret) {
			break;
		}
	}
	cob_close (data_file, COB_CLOSE_NORMAL, NULL);
}

void
cob_file_sort_giving (cob_file *sort_file, const size_t varcnt, ...)
{
	cob_file	**fbase;
	struct cobsort	*hp;
	size_t		i;
	int		ret;
	int		opt;
	va_list		args;
	size_t		cnt_rec = 0;

	fbase = cob_malloc (varcnt * sizeof(cob_file *));
	va_start (args, varcnt);
	for (i = 0; i < varcnt; i++) {
		fbase[i] = va_arg (args, cob_file *);
	}
	va_end (args);
	for (i = 0; i < varcnt; i++) {
		cob_open (fbase[i], COB_OPEN_OUTPUT, 0, NULL);
	}
	for (;;) {
		ret = cob_file_sort_retrieve (sort_file, sort_file->record->data);
		if (ret) {
			if (ret == COBSORTEND) {
				sort_file->file_status[0] = '1';
				sort_file->file_status[1] = '0';
			} else {
				hp = sort_file->file;
				*(int *)(hp->sort_return) = 16;
				sort_file->file_status[0] = '3';
				sort_file->file_status[1] = '0';
			}
			break;
		}
		for (i = 0; i < varcnt; i++) {
			if (fbase[i]->special ||
			    fbase[i]->organization == COB_ORG_LINE_SEQUENTIAL) {
				opt = COB_WRITE_BEFORE | COB_WRITE_LINES | 1;
			} else {
				opt = 0;
			}
			cob_copy_check (fbase[i], sort_file);
			cob_write (fbase[i], fbase[i]->record, opt, NULL);
		}
		cnt_rec++;
	}
	for (i = 0; i < varcnt; i++) {
		cob_close (fbase[i], COB_CLOSE_NORMAL, NULL);
	}
	free (fbase);
	cob_verbose_output ("END OF SORT/MERGE, RECORD= %d.", cnt_rec);
}

void
cob_file_sort_init (cob_file *f, const int nkeys,
		    const unsigned char *collating_sequence,
		    void *sort_return, cob_field *fnstatus)
{
	struct cobsort	*p;

	p = cob_malloc (sizeof (struct cobsort));
	p->fnstatus = fnstatus;
	p->size = f->record_max;
	p->r_size = f->record_max + sizeof(size_t);
	p->w_size = f->record_max + sizeof(size_t) + 1;
	p->pointer = f;
	p->sort_return = sort_return;
	*(int *)sort_return = 0;
	p->memory = (size_t)cob_sort_memory / (p->size + sizeof(struct cobitem));
	f->file = p;
	f->keys = cob_malloc (sizeof (struct cob_file_key) * nkeys);
	f->nkeys = 0;
	if (collating_sequence) {
		f->sort_collating = collating_sequence;
	} else {
		f->sort_collating = cob_current_module->collating_sequence;
	}
	RETURN_STATUS (COB_STATUS_00_SUCCESS);
}

void
cob_file_sort_init_key (cob_file *f, const int flag, cob_field *field,
			size_t offset)
{
	f->keys[f->nkeys].flag = flag;
	f->keys[f->nkeys].field = field;
	f->keys[f->nkeys].offset = offset;
	f->nkeys++;
}

void
cob_file_sort_close (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	size_t		i;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
		cob_free_list (hp->empty);
		for (i = 0; i < 4; i++) {
			cob_free_list (hp->queue[i].first);
			if (hp->file[i].fp != NULL) {
				fclose (hp->file[i].fp);
			}
		}
		free (hp);
	}
	f->file = NULL;
	RETURN_STATUS (COB_STATUS_00_SUCCESS);
}

void
cob_file_release (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_submit (f, f->record->data);
	switch (ret) {
	case 0:
		RETURN_STATUS (COB_STATUS_00_SUCCESS);
		break;
	default:
		if (likely(hp)) {
			*(int *)(hp->sort_return) = 16;
		}
		RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
		break;
	}
}

void
cob_file_return (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_retrieve (f, f->record->data);
	switch (ret) {
	case 0:
		RETURN_STATUS (COB_STATUS_00_SUCCESS);
		break;
	case COBSORTEND:
		RETURN_STATUS (COB_STATUS_10_END_OF_FILE);
		break;
	default:
		if (likely(hp)) {
			*(int *)(hp->sort_return) = 16;
		}
		RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
		break;
	}
}

#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM)

/*
** Using the offset:length of the (component parts) of the ix_cob_key key,
** extract the key-value from the given data record.
*/

static int extract_key (
	  struct indexfile *fh
	, int ix_cob_key	/* ordinal of key to use */
	, const void *pb_rec	/* pointer to data-record */
	, void *ret_key_value)	/* the composited key-value */
{
	int ix;
	struct keydesc *kd_cob = fh->key + ix_cob_key;
	const char *p_rec = pb_rec;
	char *p_val = ret_key_value;
	for (ix = 0; ix < kd_cob->k_nparts; ++ix) {
		memcpy (p_val
			, p_rec + kd_cob->k_part[ix].kp_start
			, kd_cob->k_part[ix].kp_leng);
		p_val += kd_cob->k_part[ix].kp_leng;
	}
	return(0);
}

/*
** Using the offset:length of the (component parts) of the ix_cob_key key,
** extract the key-value from the given data record and compare it
** to the given key-value.
** Returns: -1 ... extracted key is less than given key.
**           0 ... extracted key is equal to given key.
**          +0 ... extracted key is greater than given key.
**
** DEVELOPER TODO:
** Determine max possible size of a key and use local variable instead of malloc() free()
*/

static int keycmp (
	  struct indexfile *fh
	, int ix_cob_key	/* ordinal of key to use */
	, const void *pb_rec	/* pointer to data-record */
	, const void *pb_key)	/* the composited key-value to be compared */
{
	char *pb_key2;
	int cmp;
	pb_key2 = malloc (fh->key[ix_cob_key].k_len);
	extract_key (fh, ix_cob_key, pb_rec, pb_key2);
	cmp = memcmp (pb_key2, pb_key, fh->key[fh->curkey].k_len);
	free (pb_key2);
	return (cmp);
}

#endif
