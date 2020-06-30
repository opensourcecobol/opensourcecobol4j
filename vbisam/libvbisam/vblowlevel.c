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

/* Globals */
struct  DICTINFO	*psvbfile[VB_MAX_FILES + 1];
struct  VBFILE		svbfile[VB_MAX_FILES * 3];

static int		iinitialized = 0;

/* Activate to define LockFileEx locking */
#define	USE_LOCKFILE_EX

int
ivbopen (const char *pcfilename, const int iflags, const mode_t tmode)
{
	int		iloop;
	struct stat	sstat;

	if (!iinitialized) {
		memset ((void *)&svbfile[0], 0, sizeof (svbfile));
		iinitialized = 1;
	}
	if (stat (pcfilename, &sstat)) {
		if (!iflags & O_CREAT) {
			return -1;
		}
	} else {
		for (iloop = 0; iloop < VB_MAX_FILES * 3; iloop++) {
			if (svbfile[iloop].irefcount
			    && svbfile[iloop].tdevice == sstat.st_dev
#ifdef	_WIN32
			    && !strcmp(svbfile[iloop].cfilename, pcfilename)) {
#else
			    && svbfile[iloop].tinode == sstat.st_ino) {
#endif
				svbfile[iloop].irefcount++;
				return iloop;
			}
		}
	}
	for (iloop = 0; iloop < VB_MAX_FILES * 3; iloop++) {
		if (svbfile[iloop].irefcount == 0) {
			svbfile[iloop].ihandle = open (pcfilename, iflags | O_BINARY, tmode);
			if (svbfile[iloop].ihandle == -1) {
				break;
			}
			if ((iflags & O_CREAT) && stat (pcfilename, &sstat)) {
				close (svbfile[iloop].ihandle);
				return -1;
			}
#ifdef	_WIN32
			svbfile[iloop].tinode = 0;
			if (svbfile[iloop].cfilename) {
				free (svbfile[iloop].cfilename);
			}
			svbfile[iloop].whandle = (HANDLE)_get_osfhandle (svbfile[iloop].ihandle);
			if (svbfile[iloop].whandle == INVALID_HANDLE_VALUE) {
				close (svbfile[iloop].ihandle);
				return -1;
			}
			svbfile[iloop].cfilename = strdup (pcfilename);
#else
			svbfile[iloop].tinode = sstat.st_ino;
#endif
			svbfile[iloop].tdevice = sstat.st_dev;
			svbfile[iloop].irefcount++;
			return iloop;
		}
	}
	errno = ENOENT;
	return -1;
}

int
ivbclose (const int ihandle)
{
	if (!svbfile[ihandle].irefcount) {
		errno = ENOENT;
		return -1;
	}
	svbfile[ihandle].irefcount--;
	if (!svbfile[ihandle].irefcount) {
		return close (svbfile[ihandle].ihandle);
	}
	return 0;
}

off_t
tvblseek (const int ihandle, off_t toffset, const int iwhence)
{
	if (unlikely(!svbfile[ihandle].irefcount)) {
		errno = ENOENT;
		return -1;
	}
	return lseek (svbfile[ihandle].ihandle, toffset, iwhence);
}

ssize_t
tvbread (const int ihandle, void *pvbuffer, const size_t tcount)
{
	if (unlikely(!svbfile[ihandle].irefcount)) {
		errno = ENOENT;
		return -1;
	}
	return read (svbfile[ihandle].ihandle, pvbuffer, tcount);
}

ssize_t
tvbwrite (const int ihandle, const void *pvbuffer, const size_t tcount)
{
	if (unlikely(!svbfile[ihandle].irefcount)) {
		errno = ENOENT;
		return -1;
	}
	return write (svbfile[ihandle].ihandle, pvbuffer, tcount);
}

int
ivbblockread (const int ihandle, const int iisindex, const off_t tblocknumber, char *cbuffer)
{
	struct DICTINFO	*psvbptr;
	off_t		toffset;
	ssize_t		tresult;
	int		thandle;

	psvbptr = psvbfile[ihandle];
	toffset = (off_t) ((tblocknumber - 1) * psvbptr->inodesize);
	if (iisindex) {
		thandle = psvbptr->iindexhandle;
	} else {
		thandle = psvbptr->idatahandle;
	}
	if (tvblseek (thandle, toffset, SEEK_SET) != toffset) {
		return EIO;
	}

	tresult = tvbread (thandle, cbuffer, (size_t) psvbptr->inodesize);
	if (!iisindex && tresult == 0) {
		tresult = (ssize_t) psvbptr->inodesize;
		memset (cbuffer, 0, (size_t)psvbptr->inodesize);
	}
	if (tresult != (ssize_t)psvbptr->inodesize) {
		return EIO;
	}
	return 0;
}

int
ivbblockwrite (const int ihandle, const int iisindex, const off_t tblocknumber, const char *cbuffer)
{
	struct DICTINFO	*psvbptr;
	off_t		toffset;
	ssize_t		tresult;
	int		thandle;

	psvbptr = psvbfile[ihandle];
	toffset = (off_t) ((tblocknumber - 1) * psvbptr->inodesize);
	if (iisindex) {
		thandle = psvbptr->iindexhandle;
	} else {
		thandle = psvbptr->idatahandle;
	}
/* RXW
	tresult = tvblseek (thandle, toffset, SEEK_SET);
	if (tresult == (off_t) -1) {
*/
	if (tvblseek (thandle, toffset, SEEK_SET) != toffset) {
		return EIO;
	}

	tresult = tvbwrite (thandle, cbuffer, (size_t) psvbptr->inodesize);
	if (tresult != (ssize_t)psvbptr->inodesize) {
		return EIO;
	}
	return 0;
}

int
ivblock (const int ihandle, const off_t toffset, const off_t tlength, const int imode)
{
#ifdef	_WIN32
#ifdef	USE_LOCKFILE_EX

	OVERLAPPED	toverlapped;
	DWORD		tflags = 0;
	DWORD		winerrno = 0;
	int		bunlock = 0;

	if (!svbfile[ihandle].irefcount) {
		errno = ENOENT;
		return -1;
	}

	switch (imode) {
	case	VBUNLOCK:
		bunlock = 1;
		break;

	case	VBRDLOCK:
		tflags = LOCKFILE_FAIL_IMMEDIATELY;
		break;

	case	VBRDLCKW:
		tflags = 0;
		break;

	case	VBWRLOCK:
		tflags = LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY;
		break;

	case	VBWRLCKW:
		tflags = LOCKFILE_EXCLUSIVE_LOCK;
		break;

	default:
		errno = EBADARG;
		return -1;
	}

	memset (&toverlapped, 0, sizeof(OVERLAPPED));
	toverlapped.Offset = (DWORD)(toffset & 0xffffffff);
	toverlapped.OffsetHigh = (DWORD)(toffset >> 32);
	errno = 0;
	if (!bunlock) {
		if (LockFileEx (svbfile[ihandle].whandle, tflags, 0, (int)(tlength & 0xffffffff),
			(int)(tlength >> 32), &toverlapped)) {
			return 0;
		}
	} else {
		if (UnlockFileEx (svbfile[ihandle].whandle, 0, (int)(tlength & 0xffffffff),
			(int)(tlength >> 32), &toverlapped)) {
			return 0;
		}
		winerrno = GetLastError ();
		if (winerrno == ERROR_NOT_LOCKED) {
			//ALREADY UNLOCKED **SAME FCNTL**
			return 0;
		}
	}

	errno = EBADARG;
	return -1;
#else
/* This will probably not work correctly - 64 bit */
	int		itype, iresult = -1;
	off_t		soffset;
	off_t		tempoffset;

	if (!svbfile[ihandle].irefcount) {
		errno = ENOENT;
		return -1;
	}
	switch (imode) {
	case VBUNLOCK:
		itype = _LK_UNLCK;
		break;

	case VBRDLOCK:
		itype = _LK_NBRLCK;
		break;

	case VBRDLCKW:
		itype = _LK_RLCK;
		break;

	case VBWRLOCK:
		itype = _LK_NBLCK;
		break;

	case VBWRLCKW:
		itype = _LK_LOCK;
		break;

	default:
		errno = EBADARG;
		return -1;
	}
	soffset = lseek (svbfile[ihandle].ihandle, 0, SEEK_CUR);
	if (soffset == -1) {
		errno = 172;
		return -1;
	}
	tempoffset = lseek (svbfile[ihandle].ihandle, toffset, SEEK_SET);
	if (tempoffset == -1) {
		errno = 172;
		return -1;
	}
	errno = 0;
	iresult = _locking (svbfile[ihandle].ihandle, itype, (long)tlength);
	/* Something weird going on here */
	if (errno == 22) {
		errno = 0;
		iresult = 0;
	}
	lseek (svbfile[ihandle].ihandle, soffset, SEEK_SET);
	return iresult;
#endif
#else
	int		icommand, itype, iresult;
	struct flock	sflock;

	if (!svbfile[ihandle].irefcount) {
		errno = ENOENT;
		return -1;
	}
	switch (imode) {
	case VBUNLOCK:
		icommand = F_SETLK;
		itype = F_UNLCK;
		break;

	case VBRDLOCK:
		icommand = F_SETLK;
		itype = F_RDLCK;
		break;

	case VBRDLCKW:
		icommand = F_SETLKW;
		itype = F_RDLCK;
		break;

	case VBWRLOCK:
		icommand = F_SETLK;
		itype = F_WRLCK;
		break;

	case VBWRLCKW:
		icommand = F_SETLKW;
		itype = F_WRLCK;
		break;

	default:
		errno = EBADARG;
		return -1;
	}
	sflock.l_type = itype;
	sflock.l_whence = SEEK_SET;
	sflock.l_start = toffset;
	sflock.l_len = tlength;
	sflock.l_pid = 0;
	do {
		iresult = fcntl (svbfile[ihandle].ihandle, icommand, &sflock);
	} while (iresult && errno == EINTR);

	return iresult;
#endif
}
