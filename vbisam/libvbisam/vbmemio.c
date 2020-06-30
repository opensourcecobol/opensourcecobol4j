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

static struct VBLOCK	*pslockfree = NULL;
static struct VBTREE	*pstreefree = NULL;

/* Globals */

int	iserrno = 0;			/* Value of error is returned here */
int	iserrio = 0;			/* Contains value of last function called */
int	isreclen = 0;			/* Used for varlen tables */
int	isrecnum = 0;			/* Current row number */

#ifdef	VBDEBUG
static void
vb_error (const char *msg)
{
	fprintf (stderr, "%s - Aborting\n", msg);
	fflush (stderr);
	exit (1);
}
#else
#define	vb_error(x)
#endif

void *
pvvbmalloc (const size_t size)
{
	void	*mptr;

	mptr = calloc (1, size);
	if (unlikely(!mptr)) {
		fprintf (stderr, "Cannot allocate %d bytes of memory - Aborting\n", size);
		fflush (stderr);
		exit (1);
	}
	return mptr;
}

void
vvbfree (void *mptr)
{
	if (mptr) {
		free (mptr);
	}
}

struct VBLOCK *
psvblockallocate (const int ihandle)
{
	struct VBLOCK *pslock = pslockfree;

	if (pslockfree != NULL) {
		pslockfree = pslockfree->psnext;
		memset (pslock, 0, sizeof (struct VBLOCK));
	} else {
		pslock = pvvbmalloc (sizeof (struct VBLOCK));
	}
	return pslock;
}

void
vvblockfree (struct VBLOCK *pslock)
{
	pslock->psnext = pslockfree;
	pslockfree = pslock;
}

struct VBTREE *
psvbtreeallocate (const int ihandle)
{
	struct VBTREE *pstree = pstreefree;

	if (pstreefree == NULL) {
		pstree = pvvbmalloc (sizeof (struct VBTREE));
	} else {
		pstreefree = pstreefree->psnext;
		if (pstree->tnodenumber != -1) {
			vb_error ("Treeallocated not free");
		}
		memset (pstree, 0, sizeof (struct VBTREE));
	}
	return pstree;
}

void
vvbtreeallfree (const int ihandle, const int ikeynumber, struct VBTREE *pstree)
{
	if (!pstree) {
		return;
	}
	if (pstree->tnodenumber == -1) {
		vb_error ("Treefreed not free");
	}
	vvbkeyallfree (ihandle, ikeynumber, pstree);
	pstree->psnext = pstreefree;
	pstreefree = pstree;
	pstree->tnodenumber = -1;
}

struct VBKEY *
psvbkeyallocate (const int ihandle, const int ikeynumber)
{
	struct VBKEY	*pskey;
	struct DICTINFO	*psvbptr;
	int		ilength = 0;

	psvbptr = psvbfile[ihandle];
	pskey = psvbptr->pskeyfree[ikeynumber];
	if (pskey == NULL) {
		ilength = psvbptr->pskeydesc[ikeynumber]->k_len;
		pskey = pvvbmalloc (sizeof (struct VBKEY) + ilength);
	} else {
		if (pskey->trownode != -1) {
			vb_error ("Keyallocated not free");
		}
		psvbptr->pskeyfree[ikeynumber] =
		    psvbptr->pskeyfree[ikeynumber]->psnext;
		memset (pskey, 0, (sizeof (struct VBKEY) + ilength));
	}
	return pskey;
}

void
vvbkeyallfree (const int ihandle, const int ikeynumber, struct VBTREE *pstree)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskeycurr;
	struct VBKEY	*pskeynext;

	psvbptr = psvbfile[ihandle];
	pskeycurr = pstree->pskeyfirst;
	while (pskeycurr) {
		if (pskeycurr->trownode == -1) {
			vb_error ("Keyfreed already free");
		}
		pskeynext = pskeycurr->psnext;
		if (pskeycurr->pschild) {
			vvbtreeallfree (ihandle, ikeynumber, pskeycurr->pschild);
		}
		pskeycurr->pschild = NULL;
		pskeycurr->psnext = psvbptr->pskeyfree[ikeynumber];
		psvbptr->pskeyfree[ikeynumber] = pskeycurr;
		pskeycurr->trownode = -1;
		pskeycurr = pskeynext;
	}
	pstree->pskeyfirst = NULL;
	pstree->pskeylast = NULL;
	pstree->pskeycurr = NULL;
	pstree->ikeysinnode = 0;
}

void
vvbkeyfree (const int ihandle, const int ikeynumber, struct VBKEY *pskey)
{
	struct DICTINFO	*psvbptr;

	if (pskey->trownode == -1) {
		vb_error ("Keyfreed already free");
	}
	psvbptr = psvbfile[ihandle];
	if (pskey->pschild) {
		vvbtreeallfree (ihandle, ikeynumber, pskey->pschild);
	}
	pskey->pschild = NULL;
	if (pskey->psnext) {
		pskey->psnext->psprev = pskey->psprev;
	}
	if (pskey->psprev) {
		pskey->psprev->psnext = pskey->psnext;
	}
	pskey->psnext = psvbptr->pskeyfree[ikeynumber];
	psvbptr->pskeyfree[ikeynumber] = pskey;
	pskey->trownode = -1;
}

void
vvbkeyunmalloc (const int ihandle, const int ikeynumber)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskeycurr;
/* RXW
	int		ilength;
*/

	psvbptr = psvbfile[ihandle];
	pskeycurr = psvbptr->pskeyfree[ikeynumber];
/* RXW
	ilength = sizeof (struct VBKEY) + psvbptr->pskeydesc[ikeynumber]->k_len;
*/
	while (pskeycurr) {
		psvbptr->pskeyfree[ikeynumber] =
		    psvbptr->pskeyfree[ikeynumber]->psnext;
		vvbfree (pskeycurr);
		pskeycurr = psvbptr->pskeyfree[ikeynumber];
	}
}
