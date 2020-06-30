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

static char	cvbnodetmp[VB_NODE_MAX];

/* Local functions */

static off_t
tvbdatacountgetnext (const int ihandle)
{
	struct DICTINFO *psvbptr;
	off_t		tvalue;

	psvbptr = psvbfile[ihandle];
	iserrno = EBADARG;
	if (!psvbptr->iisdictlocked) {
		return -1;
	}
	iserrno = 0;

	tvalue = inl_ldquad (psvbptr->sdictnode.cdatacount) + 1;
	inl_stquad (tvalue, psvbptr->sdictnode.cdatacount);
	psvbptr->iisdictlocked |= 0x02;
	return tvalue;
}

/* Global functions */

off_t
tvbnodecountgetnext (const int ihandle)
{
	struct DICTINFO *psvbptr;
	off_t		tvalue;

	psvbptr = psvbfile[ihandle];
	iserrno = EBADARG;
	if (!psvbptr->iisdictlocked) {
		return -1;
	}
	iserrno = 0;

	tvalue = inl_ldquad (psvbptr->sdictnode.cnodecount) + 1;
	inl_stquad (tvalue, psvbptr->sdictnode.cnodecount);
	psvbptr->iisdictlocked |= 0x02;
	return tvalue;
}

int
ivbnodefree (const int ihandle, const off_t tnodenumber)
{
	struct DICTINFO *psvbptr;
	off_t		theadnode;
	int		ilengthused, iresult;
	char		cvbnodetmp2[VB_NODE_MAX];

	/* Sanity check - Is ihandle a currently open table? */
	iserrno = ENOTOPEN;
	psvbptr = psvbfile[ihandle];
	if (!psvbptr) {
		return -1;
	}
	iserrno = EBADARG;
	if (!psvbptr->iisdictlocked) {
		return -1;
	}
	iserrno = 0;

	memset (cvbnodetmp2, 0, (size_t) psvbptr->inodesize);
	inl_stint (INTSIZE, cvbnodetmp2);

	theadnode = inl_ldquad (psvbptr->sdictnode.cnodefree);
	/* If the list is empty, node tnodenumber becomes the whole list */
	if (theadnode == 0) {
		inl_stint (INTSIZE + QUADSIZE, cvbnodetmp2);
		inl_stquad ((off_t)0, cvbnodetmp2 + INTSIZE);
		cvbnodetmp2[psvbptr->inodesize - 2] = 0x7f;
		cvbnodetmp2[psvbptr->inodesize - 3] = -2;
		iresult = ivbblockwrite (ihandle, 1, tnodenumber, cvbnodetmp2);
		if (iresult) {
			return iresult;
		}
		inl_stquad (tnodenumber, psvbptr->sdictnode.cnodefree);
		psvbptr->iisdictlocked |= 0x02;
		return 0;
	}

	/* Read in the head of the current free list */
	iresult = ivbblockread (ihandle, 1, theadnode, cvbnodetmp);
	if (iresult) {
		return iresult;
	}
/* C-ISAM is not 100% C-ISAM compatible */
#if	ISAMMODE == 1
	if (cvbnodetmp[psvbptr->inodesize - 2] != 0x7f) {
		return EBADFILE;
	}
#endif
	if (cvbnodetmp[psvbptr->inodesize - 3] != -2) {
		return EBADFILE;
	}
	ilengthused = inl_ldint (cvbnodetmp);
	if (ilengthused >= psvbptr->inodesize - (QUADSIZE + 3)) {
		/* If there was no space left, tnodenumber becomes the head */
		cvbnodetmp2[psvbptr->inodesize - 2] = 0x7f;
		cvbnodetmp2[psvbptr->inodesize - 3] = -2;
		inl_stint (INTSIZE + QUADSIZE, cvbnodetmp2);
		inl_stquad (theadnode, &cvbnodetmp2[INTSIZE]);
		iresult = ivbblockwrite (ihandle, 1, tnodenumber, cvbnodetmp2);
		if (!iresult) {
			inl_stquad (tnodenumber, psvbptr->sdictnode.cnodefree);
			psvbptr->iisdictlocked |= 0x02;
		}
		return iresult;
	}

	/* If we got here, there's space left in the theadnode to store it */
	cvbnodetmp2[psvbptr->inodesize - 2] = 0x7f;
	cvbnodetmp2[psvbptr->inodesize - 3] = -2;
	iresult = ivbblockwrite (ihandle, 1, tnodenumber, cvbnodetmp2);
	if (iresult) {
		return iresult;
	}
	inl_stquad (tnodenumber, &cvbnodetmp[ilengthused]);
	ilengthused += QUADSIZE;
	inl_stint (ilengthused, cvbnodetmp);
	iresult = ivbblockwrite (ihandle, 1, theadnode, cvbnodetmp);

	return iresult;
}

int
ivbdatafree (const int ihandle, const off_t trownumber)
{
	struct DICTINFO *psvbptr;
	off_t		theadnode, tnodenumber;
	int		ilengthused, iresult;

	/* Sanity check - Is ihandle a currently open table? */
	iserrno = ENOTOPEN;
	psvbptr = psvbfile[ihandle];
	if (!psvbptr) {
		return -1;
	}
	iserrno = EBADARG;
	if (!psvbptr->iisdictlocked) {
		return -1;
	}
	iserrno = 0;

	if (inl_ldquad (psvbptr->sdictnode.cdatacount) == trownumber) {
		inl_stquad (trownumber - 1, psvbptr->sdictnode.cdatacount);
		psvbptr->iisdictlocked |= 0x02;
		return 0;
	}

	theadnode = inl_ldquad (psvbptr->sdictnode.cdatafree);
	if (theadnode != 0) {
		iresult = ivbblockread (ihandle, 1, theadnode, cvbnodetmp);
		if (iresult) {
			return iresult;
		}
/* C-ISAM is not 100% C-ISAM compatible */
#if	ISAMMODE == 1
		if (cvbnodetmp[psvbptr->inodesize - 2] != 0x7f) {
			return EBADFILE;
		}
#endif
		if (cvbnodetmp[psvbptr->inodesize - 3] != -1) {
			return EBADFILE;
		}
		ilengthused = inl_ldint (cvbnodetmp);
		if (ilengthused < psvbptr->inodesize - (QUADSIZE + 3)) {
			/* We need to add trownumber to the current node */
			inl_stquad ((off_t) trownumber, cvbnodetmp + ilengthused);
			ilengthused += QUADSIZE;
			inl_stint (ilengthused, cvbnodetmp);
			iresult = ivbblockwrite (ihandle, 1, theadnode, cvbnodetmp);
			return iresult;
		}
	}
	/* We need to allocate a new row-free node! */
	/* We append any existing nodes using the next pointer from the new node */
	tnodenumber = tvbnodeallocate (ihandle);
	if (tnodenumber == (off_t)-1) {
		return iserrno;
	}
	memset (cvbnodetmp, 0, VB_NODE_MAX);
	cvbnodetmp[psvbptr->inodesize - 2] = 0x7f;
	cvbnodetmp[psvbptr->inodesize - 3] = -1;
	inl_stint (INTSIZE + (2 * QUADSIZE), cvbnodetmp);
	inl_stquad (theadnode, &cvbnodetmp[INTSIZE]);
	inl_stquad (trownumber, &cvbnodetmp[INTSIZE + QUADSIZE]);
	iresult = ivbblockwrite (ihandle, 1, tnodenumber, cvbnodetmp);
	if (iresult) {
		return iresult;
	}
	inl_stquad (tnodenumber, psvbptr->sdictnode.cdatafree);
	psvbptr->iisdictlocked |= 0x02;
	return 0;
}

off_t
tvbnodeallocate (const int ihandle)
{
	struct DICTINFO *psvbptr;
	off_t		theadnode, tvalue;
	int		ilengthused;

	/* Sanity check - Is ihandle a currently open table? */
	iserrno = ENOTOPEN;
	psvbptr = psvbfile[ihandle];
	if (!psvbptr) {
		return -1;
	}
	iserrno = EBADARG;
	if (!psvbptr->iisdictlocked) {
		return -1;
	}
	iserrno = 0;

	/* If there's *ANY* nodes in the free list, use them first! */
	theadnode = inl_ldquad (psvbptr->sdictnode.cnodefree);
	if (theadnode != 0) {
		iserrno = ivbblockread (ihandle, 1, theadnode, cvbnodetmp);
		if (iserrno) {
			return -1;
		}
		iserrno = EBADFILE;
/* C-ISAM is not 100% C-ISAM compatible */
#if	ISAMMODE == 1
		if (cvbnodetmp[psvbptr->inodesize - 2] != 0x7f) {
			return -1;
		}
#endif
		if (cvbnodetmp[psvbptr->inodesize - 3] != -2) {
			return -1;
		}
		ilengthused = inl_ldint (cvbnodetmp);
		if (ilengthused > (INTSIZE + QUADSIZE)) {
			tvalue = inl_ldquad (cvbnodetmp + INTSIZE + QUADSIZE);
			memcpy (cvbnodetmp + INTSIZE + QUADSIZE,
				cvbnodetmp + INTSIZE + QUADSIZE + QUADSIZE,
				(size_t)(ilengthused - (INTSIZE + QUADSIZE + QUADSIZE)));
			ilengthused -= QUADSIZE;
			memset (cvbnodetmp + ilengthused, 0, QUADSIZE);
			inl_stint (ilengthused, cvbnodetmp);
			iserrno = ivbblockwrite (ihandle, 1, theadnode, cvbnodetmp);
			if (iserrno) {
				return -1;
			}
			return tvalue;
		}
		/* If it's last entry in the node, use the node itself! */
		tvalue = inl_ldquad (cvbnodetmp + INTSIZE);
		inl_stquad (tvalue, psvbptr->sdictnode.cnodefree);
		psvbptr->iisdictlocked |= 0x02;
		return theadnode;
	}
	/* If we get here, we need to allocate a NEW node. */
	/* Since we already hold a dictionary lock, we don't need another */
	tvalue = tvbnodecountgetnext (ihandle);
	return tvalue;
}

off_t
tvbdataallocate (const int ihandle)
{
	struct DICTINFO *psvbptr;
	off_t		theadnode, tnextnode, tvalue;
	int		ilengthused, iresult;

	psvbptr = psvbfile[ihandle];
	iserrno = EBADARG;
	if (!psvbptr->iisdictlocked) {
		return -1;
	}
	iserrno = 0;

	/* If there's *ANY* rows in the free list, use them first! */
	theadnode = inl_ldquad (psvbptr->sdictnode.cdatafree);
	while (theadnode != 0) {
		iserrno = ivbblockread (ihandle, 1, theadnode, cvbnodetmp);
		if (iserrno) {
			return -1;
		}
		iserrno = EBADFILE;
/* C-ISAM is not 100% C-ISAM compatible */
#if	ISAMMODE == 1
		if (cvbnodetmp[psvbptr->inodesize - 2] != 0x7f) {
			return -1;
		}
#endif
		if (cvbnodetmp[psvbptr->inodesize - 3] != -1) {
			return -1;
		}
		ilengthused = inl_ldint (cvbnodetmp);
		if (ilengthused > INTSIZE + QUADSIZE) {
			psvbptr->iisdictlocked |= 0x02;
			ilengthused -= QUADSIZE;
			inl_stint (ilengthused, cvbnodetmp);
			tvalue = inl_ldquad (&cvbnodetmp[ilengthused]);
			inl_stquad ((off_t)0, &cvbnodetmp[ilengthused]);
			if (ilengthused > INTSIZE + QUADSIZE) {
				iserrno = ivbblockwrite (ihandle, 1, theadnode, cvbnodetmp);
				if (iserrno) {
					return -1;
				}
				return tvalue;
			}
			/* If we're using the last entry in the node, advance */
			tnextnode = inl_ldquad (&cvbnodetmp[INTSIZE]);
			iresult = ivbnodefree (ihandle, theadnode);
			if (iresult) {
				return -1;
			}
			inl_stquad (tnextnode, psvbptr->sdictnode.cdatafree);
			return tvalue;
		}
		/* Ummmm, this is an INTEGRITY ERROR of sorts! */
		/* However, let's fix it anyway! */
		tnextnode = inl_ldquad (&cvbnodetmp[INTSIZE]);
		iresult = ivbnodefree (ihandle, theadnode);
		if (iresult) {
			return -1;
		}
		inl_stquad (tnextnode, psvbptr->sdictnode.cdatafree);
		psvbptr->iisdictlocked |= 0x02;
		theadnode = tnextnode;
	}
	/* If we get here, we need to allocate a NEW row number. */
	/* Since we already hold a dictionary lock, we don't need another */
	tvalue = tvbdatacountgetnext (ihandle);
	return tvalue;
}

int
ivbforcedataallocate (const int ihandle, const off_t trownumber)
{
	struct DICTINFO *psvbptr;
	off_t		theadnode, tprevnode, tnextnode;
	int		iloop, ilengthused;

	/* Sanity check - Is ihandle a currently open table? */
	iserrno = ENOTOPEN;
	psvbptr = psvbfile[ihandle];
	if (!psvbptr) {
		return -1;
	}
	iserrno = EBADARG;
	if (!psvbptr->iisdictlocked) {
		return -1;
	}
	iserrno = 0;

	/* Test 1: Is it already beyond EOF (the SIMPLE test) */
	theadnode = inl_ldquad (psvbptr->sdictnode.cdatacount);
	if (theadnode < trownumber) {
		psvbptr->iisdictlocked |= 0x02;
		inl_stquad (trownumber, psvbptr->sdictnode.cdatacount);
		theadnode++;
		while (theadnode < trownumber) {
			if (theadnode != 0) {
				ivbdatafree (ihandle, theadnode);
			}
			theadnode++;
		}
		return 0;
	}
	/* <SIGH> It SHOULD be *SOMEWHERE* in the data free list! */
	tprevnode = 0;
	theadnode = inl_ldquad (psvbptr->sdictnode.cdatafree);
	while (theadnode != 0) {
		iserrno = ivbblockread (ihandle, 1, theadnode, cvbnodetmp);
		if (iserrno) {
			return -1;
		}
		iserrno = EBADFILE;
/* C-ISAM is not 100% C-ISAM compatible */
#if	ISAMMODE == 1
		if (cvbnodetmp[psvbptr->inodesize - 2] != 0x7f) {
			return -1;
		}
#endif
		if (cvbnodetmp[psvbptr->inodesize - 3] != -1) {
			return -1;
		}
		ilengthused = inl_ldint (cvbnodetmp);
		for (iloop = INTSIZE + QUADSIZE; iloop < ilengthused; iloop += QUADSIZE) {
			if (inl_ldquad (&cvbnodetmp[iloop]) == trownumber) {	/* Extract it */
				memcpy (&(cvbnodetmp[iloop]), &(cvbnodetmp[iloop + QUADSIZE]),
					(size_t)(ilengthused - iloop));
				ilengthused -= QUADSIZE;
				if (ilengthused > INTSIZE + QUADSIZE) {
					inl_stquad ((off_t)0, &cvbnodetmp[ilengthused]);
					inl_stint (ilengthused, cvbnodetmp);
					return ivbblockwrite (ihandle, 1, theadnode, cvbnodetmp);
				} else {	/* It was the last one in the node! */
					tnextnode = inl_ldquad (&cvbnodetmp[INTSIZE]);
					if (tprevnode) {
						iserrno =
						    ivbblockread (ihandle, 1, tprevnode,
								  cvbnodetmp);
						if (iserrno) {
							return -1;
						}
						inl_stquad (tnextnode, &cvbnodetmp[INTSIZE]);
						return ivbblockwrite
							(ihandle, 1, tprevnode, cvbnodetmp);
					} else {
						psvbptr->iisdictlocked |= 0x02;
						inl_stquad (tnextnode,
							    psvbptr->sdictnode.cdatafree);
					}
					return ivbnodefree (ihandle, theadnode);
				}
			}
		}
		tprevnode = theadnode;
		theadnode = inl_ldquad (&cvbnodetmp[INTSIZE]);
	}
	/* If we get here, we've got a MAJOR integrity error in that the */
	/* nominated row number was simply *NOT FREE* */
	iserrno = EBADFILE;
	return -1;
}
