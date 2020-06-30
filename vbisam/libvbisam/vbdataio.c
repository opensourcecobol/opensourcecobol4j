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

struct SVARLEN {
	char	crfu[INTSIZE];		/* Always 0x0000 */
	char	cconst[INTSIZE];	/* Always 0x7e26 */
	char	cfreenext[QUADSIZE];	/* Pointer to next in group with space */
	char	cfreeprev[QUADSIZE];	/* Pointer to prev in group with space */
	char	cfreethis[INTSIZE];	/* Free space in THIS node */
	char	cfreeoffset[INTSIZE];	/* Position in node of free space */
	char	cfreecont[QUADSIZE];	/* Continuation node (Only on FULL node) */
	char	cflag;			/* Unknown, set to 0x00 */
#if	ISAMMODE == 1
	char	cusedcount[INTSIZE];	/* Number of slots in use */
#else
	char	cusedcount;		/* Number of slots in use */
#endif
	char	cgroup;			/* Used as a reference in dictionary */
};

#if	ISAMMODE == 1
static const int igroupsize[] = {
	QUADSIZE, 16, 32, 64, 128, 256, 512, 1024, 2048, MAX_NODE_LENGTH
};
#else
static const int igroupsize[] = {
	QUADSIZE, 8, 32, 128, 512, MAX_NODE_LENGTH
};
#endif

static char		cnode[VB_NODE_MAX];
static struct SVARLEN	*psvarlenheader = (struct SVARLEN *)cnode;

/* Local functions */

/* Locate space for, and fill in the tail content. */
/* Write out the tail node, and return the tail node number. */
/* Fill in the slot number used in the tail node */

static off_t
ttailnode (const int ihandle, char *pcbuffer, const int ilength, int *pislotnumber)
{
	struct DICTINFO	*psvbptr;
	char		*pcnodeptr;
	struct SVARLEN	*pshdr;
	struct SVARLEN	*psnphdr;
	off_t		tnodenumber = 0, tnodenext, tnodeprev;
	int		ifreethis, ifreeoffset, igroup, islotnumber;
	int		inodesize;
	int		n;
	char		clclnode[VB_NODE_MAX];
	char		cnextprev[VB_NODE_MAX];

	psvbptr = psvbfile[ihandle];
	pshdr = (struct SVARLEN *)clclnode;
	psnphdr = (struct SVARLEN *)cnextprev;
	inodesize = psvbptr->inodesize;
	/* Determine which group to START with */
	for (igroup = 0; igroupsize[igroup] < ilength + INTSIZE + INTSIZE; igroup++) {
		;	/* Do nothing! */
	}
	if (igroup) {
		igroup--;
	}
	while (!tnodenumber) {
		tnodenumber =
		    inl_ldquad (psvbptr->sdictnode.cvarleng0 + (igroup * QUADSIZE));
		while (tnodenumber) {
			if (ivbblockread (ihandle, 1, tnodenumber, clclnode)) {
				return -1;
			}
			if (inl_ldint (pshdr->cfreethis) < (ilength + INTSIZE + INTSIZE)) {
				tnodenumber = inl_ldquad (pshdr->cfreenext);
			} else {
				break;
			}
		}
		if (tnodenumber) {
			break;
		}
		if (igroupsize[igroup] >= MAX_NODE_LENGTH) {
			break;
		}
		igroup++;
	}
	if (!tnodenumber) {
		tnodenumber = tvbnodecountgetnext (ihandle);
		if (tnodenumber == -1) {
			return tnodenumber;
		}
		memset (clclnode, 0, VB_NODE_MAX);
		inl_stint (0x7e26, pshdr->cconst);
		inl_stint (inodesize -
			   (sizeof (struct SVARLEN) + 3 + INTSIZE + INTSIZE), pshdr->cfreethis);
		inl_stint (sizeof (struct SVARLEN), pshdr->cfreeoffset);
		pshdr->cgroup = -1;
		clclnode[inodesize - 3] = 0x7c;
		*pislotnumber = 0;
#if	ISAMMODE == 1
		inl_stint (1, pshdr->cusedcount);
#else
		pshdr->cusedcount = 1;
#endif
	} else {
		*pislotnumber = -1;
		pcnodeptr = clclnode + inodesize - (3 + INTSIZE + INTSIZE);
#if	ISAMMODE == 1
		n = inl_ldint (pshdr->cusedcount);
#else
		n = pshdr->cusedcount;
#endif
		for (islotnumber = 0; islotnumber < n; islotnumber++) {
			if (inl_ldint (pcnodeptr)) {
				pcnodeptr -= (INTSIZE * 2);
				continue;
			} else {
				*pislotnumber = islotnumber;
				break;
			}
		}
		if (*pislotnumber == -1) {
#if	ISAMMODE == 1
			*pislotnumber = inl_ldint (pshdr->cusedcount);
			inl_stint (*(pislotnumber) + 1, pshdr->cusedcount);
#else
			*pislotnumber = pshdr->cusedcount;
			pshdr->cusedcount++;
#endif
			n = inl_ldint (pshdr->cfreethis) - (INTSIZE * 2);
			inl_stint (n, pshdr->cfreethis);
		}
	}
	ifreethis = inl_ldint (pshdr->cfreethis);
	ifreeoffset = inl_ldint (pshdr->cfreeoffset);
	pcnodeptr = clclnode + inodesize - (3 + INTSIZE + INTSIZE +
			(*pislotnumber * INTSIZE * 2));
	inl_stint (ilength, pcnodeptr);
	inl_stint (ifreeoffset, pcnodeptr + INTSIZE);
	memcpy (clclnode + ifreeoffset, pcbuffer, (size_t)ilength);
	ifreethis -= ilength;
	inl_stint (ifreethis, pshdr->cfreethis);
	ifreeoffset += ilength;
	inl_stint (ifreeoffset, pshdr->cfreeoffset);
	/* Determine which 'group' the node belongs in now */
	for (igroup = 0; igroupsize[igroup] < ifreethis; igroup++) {
		;	/* Do nothing! */
	}
	if (igroup) {
		igroup--;
	}
	if (igroup != pshdr->cgroup) {
		tnodenext = inl_ldquad (pshdr->cfreenext);
		tnodeprev = inl_ldquad (pshdr->cfreeprev);
		if (tnodeprev) {
			if (ivbblockread (ihandle, 1, tnodeprev, cnextprev)) {
				return -1;
			}
			inl_stquad (tnodenext, psnphdr->cfreenext);
			if (ivbblockwrite (ihandle, 1, tnodeprev, cnextprev)) {
				return -1;
			}
		}
		if (tnodenext) {
			if (ivbblockread (ihandle, 1, tnodenext, cnextprev)) {
				return -1;
			}
			inl_stquad (tnodeprev, psnphdr->cfreeprev);
			if (ivbblockwrite (ihandle, 1, tnodenext, cnextprev)) {
				return -1;
			}
		}
		inl_stquad ((off_t)0, pshdr->cfreeprev);
		tnodenext =
		    inl_ldquad (psvbptr->sdictnode.cvarleng0 + (igroup * QUADSIZE));
		inl_stquad (tnodenext, pshdr->cfreenext);
		if (tnodenext) {
			if (ivbblockread (ihandle, 1, tnodenext, cnextprev)) {
				return -1;
			}
			inl_stquad (tnodenumber, psnphdr->cfreeprev);
			if (ivbblockwrite (ihandle, 1, tnodenext, cnextprev)) {
				return -1;
			}
		}
		if (pshdr->cgroup >= 0) {
			inl_stquad (tnodenext,
				    psvbptr->sdictnode.cvarleng0 +
				    (pshdr->cgroup * QUADSIZE));
		}
		inl_stquad (tnodenumber,
			    psvbptr->sdictnode.cvarleng0 + (igroup * QUADSIZE));
		psvbptr->iisdictlocked |= 0x02;
		pshdr->cgroup = igroup;
	}
	if (ivbblockwrite (ihandle, 1, tnodenumber, clclnode)) {
		return -1;
	}
	return tnodenumber;
}

static int
ivbvarlenread (const int ihandle, char *pcbuffer, off_t tnodenumber, int islotnumber, int ilength)
{
	char	*pcnodeptr;
	int	iresult, islotlength, islotoffset;
	int	inodesize;

	inodesize = psvbfile[ihandle]->inodesize;
	while (1) {
		iresult = ivbblockread (ihandle, 1, tnodenumber, cnode);
		if (iresult) {
			return -1;
		}
		if (inl_ldint (psvarlenheader->cconst) != 0x7e26) {
			iserrno = EBADFILE;
			return -1;
		}
		pcnodeptr = cnode + inodesize - 3;
		if (*pcnodeptr != 0x7c) {
			iserrno = EBADFILE;
			return -1;
		}
		pcnodeptr -= ((islotnumber + 1) * INTSIZE * 2);
		islotlength = inl_ldint (pcnodeptr);
		islotoffset = inl_ldint (pcnodeptr + INTSIZE);
		if (islotlength <= ilength) {
			if (inl_ldquad (psvarlenheader->cfreecont) != 0) {
				iserrno = EBADFILE;
				return -1;
			}
			memcpy (pcbuffer, cnode + islotoffset, (size_t)ilength);
			return 0;
		}
		ilength -= islotlength;
#if	ISAMMODE == 1
		islotnumber = inl_ldint (psvarlenheader->cfreecont);
		islotnumber >>= 6;
		*(psvarlenheader->cfreecont + 1) &= (unsigned char)0x3f;
#else
		islotnumber = *(psvarlenheader->cfreecont);
#endif
		*(psvarlenheader->cfreecont) = 0;
		tnodenumber = inl_ldquad (psvarlenheader->cfreecont);
	}
	return 0;
}

/* MUST populate psvbfile [ihandle]->tvarlennode */
/* MUST populate psvbfile [ihandle]->ivarlenslot */
/* MUST populate psvbfile [ihandle]->ivarlenlength */
static int
ivbvarlenwrite (const int ihandle, char *pcbuffer, int ilength)
{
	struct DICTINFO	*psvbptr;
	off_t		tnewnode, tnodenumber = 0;
	int		islotnumber;
	int		inodesize;
	int		n;

	psvbptr = psvbfile[ihandle];
	psvbptr->ivarlenlength = ilength;
	psvbptr->tvarlennode = 0;
	inodesize = psvbptr->inodesize;
	/* Write out 'FULL' nodes first */
	while (ilength > 0) {
		if (ilength >
		    (inodesize - (int)(3 + INTSIZE + INTSIZE + sizeof (struct SVARLEN)))) {
			tnewnode = tvbnodecountgetnext (ihandle);
			if (tnewnode == -1) {
				return -1;
			}
			if (tnodenumber) {
				inl_stquad (tnewnode, psvarlenheader->cfreecont);
				if (ivbblockwrite (ihandle, 1, tnodenumber, cnode)) {
					return -1;
				}
			} else {
				psvbptr->tvarlennode = tnewnode;
				psvbptr->ivarlenslot = 0;
			}
			inl_stint (0, psvarlenheader->crfu);
			inl_stint (0x7e26, psvarlenheader->cconst);
			inl_stquad ((off_t)0, psvarlenheader->cfreenext);
			inl_stquad ((off_t)0, psvarlenheader->cfreeprev);
			inl_stint (0, psvarlenheader->cfreethis);
			inl_stint (inodesize - (3 + INTSIZE + INTSIZE),
				   psvarlenheader->cfreeoffset);
			psvarlenheader->cflag = 0x01;
#if	ISAMMODE == 1
			inl_stint (1, psvarlenheader->cusedcount);
#else
			psvarlenheader->cusedcount = 1;
#endif
			psvarlenheader->cgroup = 0x00;
			memcpy (&(psvarlenheader->cgroup) + 1, pcbuffer,
				inodesize - (3 + INTSIZE + INTSIZE +
								sizeof (struct SVARLEN)));
			pcbuffer +=
			    inodesize - (3 + INTSIZE + INTSIZE +
							    sizeof (struct SVARLEN));
			ilength -=
			    inodesize - (3 + INTSIZE + INTSIZE +
							    sizeof (struct SVARLEN));
			/* Length */
			inl_stint (inodesize -
				   (3 + INTSIZE + INTSIZE + sizeof (struct SVARLEN)),
				   cnode + inodesize - (3 + INTSIZE + INTSIZE));
			/* Offset */
			inl_stint (&(psvarlenheader->cgroup) + 1 - cnode,
				   cnode + inodesize - (3 + INTSIZE));
			*(cnode + inodesize - 3) = 0x7c;
			*(cnode + inodesize - 2) = 0x0;
			*(cnode + inodesize - 1) = 0x0;
			tnodenumber = tnewnode;
			continue;
		}
		if (!psvbptr->tvarlennode) {
			psvbptr->tvarlennode = tnodenumber;
			psvbptr->ivarlenslot = 0;
		}
		/* If tnodenumber is != 0, we still need to write it out! */
		if (tnodenumber && !ilength) {
			return ivbblockwrite (ihandle, 1, tnodenumber, cnode);
		}
		/* Now, to deal with the 'tail' */
		tnewnode = ttailnode (ihandle, pcbuffer, ilength, &islotnumber);
		if (tnewnode == -1) {
			return -1;
		}
		if (tnodenumber) {
			inl_stquad (tnewnode, psvarlenheader->cfreecont);
#if	ISAMMODE == 1
			n = (islotnumber << 6) + inl_ldint (psvarlenheader->cfreecont);
			inl_stint (n, psvarlenheader->cfreecont);
#else
			*psvarlenheader->cfreecont = islotnumber;
#endif
			if (ivbblockwrite (ihandle, 1, tnodenumber, cnode)) {
				return -1;
			}
			if (!psvbptr->tvarlennode) {
				psvbptr->tvarlennode = tnodenumber;
				psvbptr->ivarlenslot = 0;
			}
		}
		if (!psvbptr->tvarlennode) {
			psvbptr->tvarlennode = tnewnode;
			psvbptr->ivarlenslot = islotnumber;
		}
		return 0;
	}
	return -1;
}

/* MUST update the group number (if applicable) */
/* MUST update the dictionary node (if applicable) */
static int
ivbvarlendelete (const int ihandle, off_t tnodenumber, int islotnumber, int ilength)
{
	struct DICTINFO	*psvbptr;
	off_t		tnodenext, tnodeprev;
	int		ifreethis, ifreeoffset, igroup,
			iisanyused, iloop, imovelength,
			ioffset, ithislength, ithisoffset, iusedcount;
	int		inodesize;

	psvbptr = psvbfile[ihandle];
	inodesize = psvbptr->inodesize;
	while (ilength > 0) {
		if (ivbblockread (ihandle, 1, tnodenumber, cnode)) {
			return -1;
		}
		ithislength = inl_ldint (cnode + inodesize -
			      (3 + INTSIZE + INTSIZE + (islotnumber * 2 * INTSIZE)));
		ilength -= ithislength;
		inl_stint (0, cnode + inodesize -
			   (3 + INTSIZE + INTSIZE + (islotnumber * 2 * INTSIZE)));
		ithisoffset = inl_ldint (cnode + inodesize -
			       (3 + INTSIZE + (islotnumber * 2 * INTSIZE)));
		inl_stint (0, cnode + inodesize -
			   (3 + INTSIZE + (islotnumber * 2 * INTSIZE)));
#if	ISAMMODE == 1
		iusedcount = inl_ldint (psvarlenheader->cusedcount);
#else
		iusedcount = psvarlenheader->cusedcount;
#endif
		iisanyused = 0;
		for (iloop = 0; iloop < iusedcount; iloop++) {
			if (inl_ldint (cnode + inodesize -
			     (3 + INTSIZE + INTSIZE + (iloop * 2 * INTSIZE)))) {
				iisanyused = 1;
				break;
			}
		}
		if (!iisanyused) {
			tnodenext = inl_ldquad (psvarlenheader->cfreenext);
			tnodeprev = inl_ldquad (psvarlenheader->cfreeprev);
			igroup = psvarlenheader->cgroup;
			if (inl_ldquad (psvbptr->sdictnode.cvarleng0 + (igroup * QUADSIZE)) ==
			    tnodenumber) {
				inl_stquad (tnodenext,
					    psvbptr->sdictnode.cvarleng0 +
					    (igroup * QUADSIZE));
			}
			ivbnodefree (ihandle, tnodenumber);
			tnodenumber = inl_ldquad (psvarlenheader->cfreecont);
			if (tnodenext) {
				if (ivbblockread (ihandle, 1, tnodenext, cnode)) {
					return -1;
				}
				inl_stquad (tnodeprev, psvarlenheader->cfreeprev);
				if (ivbblockwrite (ihandle, 1, tnodenext, cnode)) {
					return -1;
				}
			}
			if (tnodeprev) {
				if (ivbblockread (ihandle, 1, tnodeprev, cnode)) {
					return -1;
				}
				inl_stquad (tnodenext, psvarlenheader->cfreenext);
				if (ivbblockwrite (ihandle, 1, tnodeprev, cnode)) {
					return -1;
				}
			}
			continue;
		}
		ifreethis = inl_ldint (psvarlenheader->cfreethis);
		ifreeoffset = inl_ldint (psvarlenheader->cfreeoffset);
		if (islotnumber == iusedcount) {
			iusedcount--;
#if	ISAMMODE == 1
			inl_stint (iusedcount - 1, psvarlenheader->cusedcount);
#else
			psvarlenheader->cusedcount = iusedcount;
#endif
			ifreethis += (INTSIZE * 2);
		}
		imovelength = inodesize - (ithisoffset + ithislength);
		imovelength -= (3 + (iusedcount * INTSIZE * 2));
		memmove (cnode + ithisoffset, cnode + ithisoffset + ithislength, (size_t)imovelength);
		ifreeoffset -= ithislength;
		ifreethis += ithislength;
		inl_stint (ifreethis, psvarlenheader->cfreethis);
		inl_stint (ifreeoffset, psvarlenheader->cfreeoffset);
		memset (cnode + ifreeoffset, 0, (size_t)ifreethis);
		for (iloop = 0; iloop < iusedcount; iloop++) {
			ioffset =
			    inl_ldint (cnode + inodesize -
				       (3 + INTSIZE + (iloop * 2 * INTSIZE)));
			if (ioffset > ithisoffset) {
				inl_stint (ioffset - ithislength,
					   cnode + inodesize -
					   (3 + INTSIZE + (iloop * 2 * INTSIZE)));
			}
		}
		if (ivbblockwrite (ihandle, 1, tnodenumber, cnode)) {
			return -1;
		}
		tnodenumber = inl_ldquad (psvarlenheader->cfreecont);
	}
	psvbptr->tvarlennode = 0;
	return 0;
}

/* Global functions */

 /* Comments:
 *	This function is *NOT* concerned with whether the row is deleted or not
 *	However, it *DOES* set *(pideletedrow) accordingly.
 *	The receiving buffer (pcbuffer) is only guaranteed to be long enough to
 *	hold the MINIMUM row length (exclusive of the 1 byte deleted flag) and
 *	thus we need to jump through hoops to avoid overwriting stuff beyond it.
 */
int
ivbdataread (const int ihandle, char *pcbuffer, int *pideletedrow, const off_t trownumber)
{
	struct DICTINFO *psvbptr;
	off_t		tblocknumber, toffset, tsofar;
	int		irowlength;
	int		n;
	char		cfooter[VB_NODE_MAX];
	char		cvbnodetmp[VB_NODE_MAX];
	char		pcreadbuffer[MAX_RESERVED_LENGTH];

	/* Sanity check - Is ihandle a currently open table? */
	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		return ENOTOPEN;
	}
	if (!psvbfile[ihandle]) {
		return ENOTOPEN;
	}
	if (trownumber < 1) {
		return EBADARG;
	}
	psvbptr = psvbfile[ihandle];

	irowlength = psvbptr->iminrowlength;
	irowlength++;
	if (psvbptr->iopenmode & ISVARLEN) {
		irowlength += INTSIZE + QUADSIZE;
	} else {
/* RXW */
		toffset = irowlength * (trownumber - 1);
		if (tvblseek (psvbptr->idatahandle, toffset, SEEK_SET) != toffset) {
			return EBADFILE;
		}
		memset (pcreadbuffer, 0, irowlength);
		if (tvbread (psvbptr->idatahandle, pcreadbuffer, (size_t)irowlength) < 0) {
			return EBADFILE;
		}
		memcpy (pcbuffer, pcreadbuffer, (size_t)psvbptr->iminrowlength);
		if (pcreadbuffer[psvbptr->iminrowlength] == 0) {
			*pideletedrow = 1;
		} else {
			*pideletedrow = 0;
		}
		isreclen = psvbptr->iminrowlength;
		return 0;
	}
	toffset = irowlength * (trownumber - 1);
	tblocknumber = (toffset / psvbptr->inodesize);
	toffset -= (tblocknumber * psvbptr->inodesize);
	if (ivbblockread (ihandle, 0, tblocknumber + 1, cvbnodetmp)) {
		return EBADFILE;
	}
	/* Read in the *MINIMUM* rowlength and store it into pcbuffer */
	tsofar = 0;
	while (tsofar < psvbptr->iminrowlength) {
		if ((psvbptr->iminrowlength - tsofar) < (psvbptr->inodesize - toffset)) {
			memcpy (pcbuffer + tsofar, cvbnodetmp + toffset,
				(size_t)(psvbptr->iminrowlength - tsofar));
			toffset += psvbptr->iminrowlength - tsofar;
			tsofar = psvbptr->iminrowlength;
			break;
		}
		memcpy (pcbuffer + tsofar, cvbnodetmp + toffset, (size_t)(psvbptr->inodesize - toffset));
		tblocknumber++;
		tsofar += psvbptr->inodesize - toffset;
		toffset = 0;
		if (ivbblockread (ihandle, 0, tblocknumber + 1, cvbnodetmp)) {
			return EBADFILE;
		}
	}
	pcbuffer += tsofar;
	/* OK, now for the footer.  Either 1 byte or 1 + INTSIZE + QUADSIZE. */
	while (tsofar < irowlength) {
		if ((irowlength - tsofar) <= (psvbptr->inodesize - toffset)) {
			memcpy (cfooter + tsofar - psvbptr->iminrowlength, cvbnodetmp + toffset,
				(size_t)(irowlength - tsofar));
			break;
		}
		memcpy (cfooter + tsofar - psvbptr->iminrowlength, cvbnodetmp + toffset,
			(size_t)(psvbptr->inodesize - toffset));
		tblocknumber++;
		tsofar += psvbptr->inodesize - toffset;
		toffset = 0;
		if (ivbblockread (ihandle, 0, tblocknumber + 1, cvbnodetmp)) {
			return EBADFILE;
		}
	}
	isreclen = psvbptr->iminrowlength;
	*pideletedrow = 0;
	if (cfooter[0] == 0x00) {
		*pideletedrow = 1;
	} else {
		if (psvbptr->iopenmode & ISVARLEN) {
			psvbptr->ivarlenlength = inl_ldint (cfooter + 1);
/* VBISAM in 64 bit mode (4k Nodes) uses a ten bit slot number */
/* VBISAM in 32 bit mode (1k Nodes) uses an eight bit slot number */
#if	ISAMMODE == 1
			n = inl_ldint (cfooter + 1 + INTSIZE);
			psvbptr->ivarlenslot =  n >> 6;
			*(cfooter + 1 + INTSIZE + 1) &= (unsigned char)0x3f;
#else
			psvbptr->ivarlenslot = *(cfooter + 1 + INTSIZE);
#endif
			*(cfooter + 1 + INTSIZE) = 0;
			psvbptr->tvarlennode = inl_ldquad (cfooter + 1 + INTSIZE);
			if (psvbptr->ivarlenlength) {
				if (ivbvarlenread (ihandle, (char *)pcbuffer,
				     psvbptr->tvarlennode,
				     psvbptr->ivarlenslot, psvbptr->ivarlenlength)) {
					return iserrno;
				}
			}
			isreclen += psvbptr->ivarlenlength;
		}
	}
	return 0;
}

int
ivbdatawrite (const int ihandle, char *pcbuffer, int ideletedrow, const off_t trownumber)
{
	struct DICTINFO *psvbptr;
	char		*pctemp;
	off_t		tblocknumber, toffset, tsofar;
	int		irowlength;
	int		n;
	char		cvbnodetmp[VB_NODE_MAX];
	char		pcwritebuffer[MAX_RESERVED_LENGTH];

	/* Sanity check - Is ihandle a currently open table? */
	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		return ENOTOPEN;
	}
	if (!psvbfile[ihandle]) {
		return ENOTOPEN;
	}
	if (trownumber < 1) {
		return EBADARG;
	}
	psvbptr = psvbfile[ihandle];

	irowlength = psvbptr->iminrowlength;
	toffset = irowlength + 1;
	if (psvbptr->iopenmode & ISVARLEN) {
		toffset += INTSIZE + QUADSIZE;
	} else {
/* RXW */
		toffset *= (trownumber - 1);
		if (tvblseek (psvbptr->idatahandle, toffset, SEEK_SET) != toffset) {
			return EBADFILE;
		}
		memcpy (pcwritebuffer, pcbuffer, (size_t)irowlength);
		*(pcwritebuffer + irowlength) = ideletedrow ? 0x00 : 0x0a;
		irowlength++;
		if (tvbwrite (psvbptr->idatahandle, pcwritebuffer, (size_t)irowlength) != (ssize_t)irowlength) {
			return EBADFILE;
		}
		return 0;
	}
	toffset *= (trownumber - 1);
	if (psvbptr->iopenmode & ISVARLEN) {
		if (psvbptr->tvarlennode) {
			if (ivbvarlendelete (ihandle, psvbptr->tvarlennode, psvbptr->ivarlenslot,
			     psvbptr->ivarlenlength)) {
				return -69;
			}
		}
		if (isreclen == psvbptr->iminrowlength || ideletedrow) {
			psvbptr->tvarlennode = 0;
			psvbptr->ivarlenlength = 0;
			psvbptr->ivarlenslot = 0;
		} else {
			if (ivbvarlenwrite (ihandle, pcbuffer + psvbptr->iminrowlength,
			    isreclen - psvbptr->iminrowlength)) {
				return iserrno;
			}
		}
	}
	memcpy (pcwritebuffer, pcbuffer, (size_t)irowlength);
	*(pcwritebuffer + irowlength) = ideletedrow ? 0x00 : 0x0a;
	irowlength++;
	if (psvbptr->iopenmode & ISVARLEN) {
		inl_stint (psvbptr->ivarlenlength, pcwritebuffer + irowlength);
		pctemp = pcwritebuffer + irowlength + INTSIZE;
		inl_stquad (psvbptr->tvarlennode, pctemp);
#if	ISAMMODE == 1
		n = (psvbptr->ivarlenslot << 6) + inl_ldint (pctemp);
		inl_stint (n, pctemp);
#else
		*pctemp = psvbptr->ivarlenslot;
#endif
		irowlength += INTSIZE + QUADSIZE;
	}

	tblocknumber = (toffset / psvbptr->inodesize);
	toffset -= (tblocknumber * psvbptr->inodesize);
	tsofar = 0;
	while (tsofar < irowlength) {
		memset (cvbnodetmp, 0, VB_NODE_MAX);
		ivbblockread (ihandle, 0, tblocknumber + 1, cvbnodetmp);	/* Can fail!! */
		if ((irowlength - tsofar) <= (psvbptr->inodesize - toffset)) {
			memcpy (cvbnodetmp + toffset, pcwritebuffer + tsofar,
				(size_t)(irowlength - tsofar));
			if (ivbblockwrite (ihandle, 0, tblocknumber + 1, cvbnodetmp)) {
				return EBADFILE;
			}
			break;
		}
		memcpy (cvbnodetmp + toffset, pcbuffer + tsofar, (size_t)(psvbptr->inodesize - toffset));
		if (ivbblockwrite (ihandle, 0, tblocknumber + 1, cvbnodetmp)) {
			return EBADFILE;
		}
		tblocknumber++;
		tsofar += psvbptr->inodesize - toffset;
		toffset = 0;
	}
	return 0;
}
