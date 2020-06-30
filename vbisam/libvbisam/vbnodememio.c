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

#define		TCC	(' ')	/* Trailing Compression Character */

static char	cvbnodetmp[VB_NODE_MAX];

static int
iquicknodesave (const int ihandle, struct VBTREE *pstree, const off_t tnodenumber,
		struct keydesc *pskeydesc, const int imode, const int iposn)
{
	int	idupslength = 0;
	int	ikeylength = pskeydesc->k_len;
	int	ilength, iposition, iresult;

	/* Sanity checks */
	if (!pstree || !pstree->pskeylist[iposn]) {
		return -1;
	}
	if (imode == 1 && !pstree->pskeylist[iposn]->iisnew) {
		return -1;
	}
	/* Read in the node (hopefully from the cache too!) */
	iresult = ivbblockread (ihandle, 1, tnodenumber, cvbnodetmp);
	if (iresult) {
		return iresult;
	}
	if (pskeydesc->k_flags & ISDUPS) {
		idupslength = QUADSIZE;
	}
	ilength = inl_ldint (cvbnodetmp);
	/* Is there enough free space in the node for an insertion? */
	if (imode == 1
	    && ilength + 3 + ikeylength + idupslength + QUADSIZE >=
	    psvbfile[ihandle]->inodesize) {
		return -1;
	}
	inl_stint (ilength + (imode * (ikeylength + idupslength + QUADSIZE)), cvbnodetmp);
	/* Calculate position for insertion / deletion of key */
#if	ISAMMODE == 1
	iposition = INTSIZE + QUADSIZE + (iposn * (ikeylength + idupslength + QUADSIZE));
#else
	iposition = INTSIZE + (iposn * (ikeylength + idupslength + QUADSIZE));
#endif
	if (imode == 1) {
		memmove (cvbnodetmp + iposition + ikeylength + idupslength + QUADSIZE,
			 cvbnodetmp + iposition, (size_t)(ilength - iposition));
		memcpy (cvbnodetmp + iposition, pstree->pskeylist[iposn]->ckey, (size_t)ikeylength);
		if (pskeydesc->k_flags & ISDUPS) {
			inl_stquad (pstree->pskeylist[iposn]->tdupnumber,
				    cvbnodetmp + iposition + ikeylength);
		}
		inl_stquad (pstree->pskeylist[iposn]->trownode,
			    cvbnodetmp + iposition + ikeylength + idupslength);
	} else {
		if (ilength - (iposition + ikeylength + idupslength + QUADSIZE) > 0) {
			memmove (cvbnodetmp + iposition,
				 cvbnodetmp + iposition + ikeylength + idupslength + QUADSIZE,
				 (size_t)(ilength - (iposition + ikeylength + idupslength + QUADSIZE)));
		}
		memset (cvbnodetmp + ilength - (ikeylength + QUADSIZE), 0,
			(size_t)(ikeylength + idupslength + QUADSIZE));
	}
	iresult = ivbblockwrite (ihandle, 1, tnodenumber, cvbnodetmp);
	if (iresult) {
		return iresult;
	}
	return 0;
}

static int
inewroot (const int ihandle, const int ikeynumber, struct VBTREE *pstree,
	  struct VBTREE *psnewtree, struct VBTREE *psroottree,
	  struct VBKEY *psrootkey[], off_t tnewnode1, off_t tnewnode2)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey;
	int		iresult;

	psvbptr = psvbfile[ihandle];
	/* Fill in the content for the new root node */
	psrootkey[0]->psnext = psrootkey[1];
	psrootkey[1]->psnext = psrootkey[2];
	psrootkey[2]->psprev = psrootkey[1];
	psrootkey[1]->psprev = psrootkey[0];
	psrootkey[0]->psparent = psrootkey[1]->psparent = psrootkey[2]->psparent = psroottree;
	psrootkey[0]->pschild = pstree;
	psrootkey[1]->pschild = psnewtree;
	psrootkey[0]->trownode = tnewnode2;
	psrootkey[1]->trownode = tnewnode1;
	psrootkey[1]->iishigh = 1;
	psrootkey[2]->iisdummy = 1;
	memcpy (psrootkey[0]->ckey, pstree->pskeylast->psprev->ckey,
		(size_t)psvbptr->pskeydesc[ikeynumber]->k_len);
	psrootkey[0]->tdupnumber = pstree->pskeylast->psprev->tdupnumber;
	vvbkeyvalueset (1, psvbptr->pskeydesc[ikeynumber], psrootkey[1]->ckey);
	/*
	 * psroottree is the new ROOT node
	 * psnewtree is the new LEAF node
	 * pstree is the original node (saved in a new place)
	 */
	psroottree->pskeyfirst = psrootkey[0];
	psroottree->pskeycurr = psrootkey[0];
	psroottree->pskeylast = psrootkey[2];
	psroottree->tnodenumber = pstree->tnodenumber;
	psroottree->ilevel = pstree->ilevel + 1;
	psroottree->iisroot = 1;
	psroottree->iistof = 1;
	psroottree->iiseof = 1;
	pstree->psparent = psroottree;
	pstree->tnodenumber = tnewnode2;
	psnewtree->psparent = psroottree;
	psnewtree->tnodenumber = tnewnode1;
	psnewtree->ilevel = pstree->ilevel;
	psnewtree->pskeycurr = psnewtree->pskeyfirst;
	psvbptr->pstree[ikeynumber] = psroottree;
	for (pskey = pstree->pskeyfirst; pskey; pskey = pskey->psnext) {
		if (pskey->pschild) {
			pskey->pschild->psparent = pstree;
		}
	}
	for (pskey = psnewtree->pskeyfirst; pskey; pskey = pskey->psnext) {
		if (pskey->pschild) {
			pskey->pschild->psparent = psnewtree;
		}
	}
	iresult = ivbnodesave (ihandle, ikeynumber, psnewtree, psnewtree->tnodenumber, 0, 0);
	if (iresult) {
		return iresult;
	}
	iresult = ivbnodesave (ihandle, ikeynumber, pstree, pstree->tnodenumber, 0, 0);
	if (iresult) {
		return iresult;
	}
	pstree->iisroot = 0;
	psnewtree->iisroot = 0;
	pstree->iistof = 1;
	psnewtree->iistof = 0;
	pstree->iiseof = 0;
	psnewtree->iiseof = 1;
	return ivbnodesave (ihandle, ikeynumber, psroottree, psroottree->tnodenumber, 0, 0);
}

static int
inodesplit (const int ihandle, const int ikeynumber, struct VBTREE *pstree,
	    struct VBKEY *pskeyhalfway)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey, *pskeytemp, *psholdkeycurr, *psnewkey;
	struct VBKEY	*psrootkey[3];
	struct VBTREE	*psnewtree, *psroottree = NULL;
	off_t		tnewnode1, tnewnode2 = 0;
	int		iresult;

	psnewtree = psvbtreeallocate (ihandle);
	if (!psnewtree) {
		return errno;
	}
	psnewtree->psparent = pstree;
	psnewkey = psvbkeyallocate (ihandle, ikeynumber);
	if (!psnewkey) {
		return errno;
	}
	psvbptr = psvbfile[ihandle];
	psrootkey[0] = NULL;
	psrootkey[1] = NULL;
	psrootkey[2] = NULL;
	if (pstree->iisroot) {
		psroottree = psvbtreeallocate (ihandle);
		if (!psroottree) {
			return errno;
		}
		psrootkey[0] = psvbkeyallocate (ihandle, ikeynumber);
		if (!psrootkey[0]) {
			return errno;
		}
		psrootkey[1] = psvbkeyallocate (ihandle, ikeynumber);
		if (!psrootkey[1]) {
			return errno;
		}
		psrootkey[2] = psvbkeyallocate (ihandle, ikeynumber);
		if (!psrootkey[2]) {
			return errno;
		}
		tnewnode2 = tvbnodeallocate (ihandle);
		if (tnewnode2 == -1) {
			return iserrno;
		}
	}
	tnewnode1 = tvbnodeallocate (ihandle);
	if (tnewnode1 == -1) {
		return iserrno;
	}

	if (pstree->iisroot) {
		psnewtree->pskeylast = pstree->pskeylast;
		pskey = pskeyhalfway->psnext;
		psnewkey->psprev = pskey->psprev;
		psnewkey->psparent = pskey->psparent;
		psnewkey->iisdummy = 1;
		pskey->psprev->psnext = psnewkey;
		pskey->psprev = NULL;
		pstree->pskeylast = psnewkey;
		pstree->pskeycurr = pstree->pskeyfirst;
		psnewtree->pskeyfirst = psnewtree->pskeycurr = pskey;
		psnewtree->ilevel = pstree->ilevel;
		psnewtree->psparent = pstree->psparent;
		psnewtree->iiseof = pstree->iiseof;
		pstree->iiseof = 0;
		for (pskeytemp = pskey; pskeytemp; pskeytemp = pskeytemp->psnext) {
			pskeytemp->psparent = psnewtree;
		}
		return inewroot (ihandle, ikeynumber, pstree, psnewtree, psroottree, psrootkey,
			 tnewnode1, tnewnode2);
	} else {
		psnewtree->pskeyfirst = psnewtree->pskeycurr = pstree->pskeyfirst;
		psnewtree->pskeylast = psnewkey;
		pstree->pskeyfirst = pstree->pskeycurr = pskeyhalfway->psnext;
		pskeyhalfway->psnext->psprev = NULL;
		pskeyhalfway->psnext = psnewkey;
		psnewkey->psprev = pskeyhalfway;
		psnewkey->psnext = NULL;
		psnewkey->psparent = psnewtree;	/* Doubtful */
		psnewkey->iisdummy = 1;
		for (pskey = psnewtree->pskeyfirst; pskey; pskey = pskey->psnext) {
			pskey->psparent = psnewtree;
			if (pskey->pschild) {
				pskey->pschild->psparent = psnewtree;
			}
		}
		psnewtree->ilevel = pstree->ilevel;
		psnewtree->psparent = pstree->psparent;
		/*
		 * psnewtree is the new LEAF node but is stored in the OLD node
		 * pstree is the original node and contains the HIGH half
		 */
		psnewtree->tnodenumber = tnewnode1;
		iresult = ivbnodesave (ihandle, ikeynumber, psnewtree, psnewtree->tnodenumber, 0, 0);
		if (iresult) {
			return iresult;
		}
		psnewtree->iistof = pstree->iistof;
		iresult = ivbnodesave (ihandle, ikeynumber, pstree, pstree->tnodenumber, 0, 0);
		if (iresult) {
			return iresult;
		}
		pstree->iistof = 0;
		psholdkeycurr = psvbptr->pskeycurr[ikeynumber];
		psvbptr->pskeycurr[ikeynumber] =
		    psnewkey->psparent->psparent->pskeycurr;
		iresult =
		    ivbkeyinsert (ihandle, pskeyhalfway->psparent->psparent, ikeynumber,
				  pskeyhalfway->ckey, tnewnode1, pskeyhalfway->tdupnumber,
				  psnewtree);
		psvbptr->pskeycurr[ikeynumber] = psholdkeycurr;
		if (iresult) {
			return iresult;
		}
		psholdkeycurr->psparent->pskeycurr = psholdkeycurr;
	}
	return 0;
}

/* Global functions */

int
ivbnodeload (const int ihandle, const int ikeynumber, struct VBTREE *pstree,
	     const off_t tnodenumber, const int iprevlvl)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey, *pskeynext;
	struct keydesc	*pskeydesc;
	char		*pcnodeptr;
#if	ISAMMODE == 1
	off_t		ttransnumber;
#endif
	int		icountlc = 0;	/* Leading compression */
	int		icounttc = 0;	/* Trailing compression */
	int		idups = 0, inodelen, iresult;
	unsigned char	cprevkey[VB_MAX_KEYLEN];
	unsigned char	chighkey[VB_MAX_KEYLEN];

	psvbptr = psvbfile[ihandle];
	pskeydesc = psvbptr->pskeydesc[ikeynumber];
	vvbkeyvalueset (0, pskeydesc, cprevkey);
	vvbkeyvalueset (1, pskeydesc, chighkey);
	iresult = ivbblockread (ihandle, 1, tnodenumber, cvbnodetmp);
	if (iresult) {
		return iresult;
	}
	if (iprevlvl != -1) {
		if (*(cvbnodetmp + psvbptr->inodesize - 2) != iprevlvl - 1) {
			return EBADFILE;
		}
	}
	pstree->tnodenumber = tnodenumber;
	pstree->ilevel = *(cvbnodetmp + psvbptr->inodesize - 2);
	inodelen = inl_ldint (cvbnodetmp);
#if	ISAMMODE == 1
	pcnodeptr = cvbnodetmp + INTSIZE + QUADSIZE;
	ttransnumber = inl_ldquad (cvbnodetmp + INTSIZE);
	if (ttransnumber == pstree->ttransnumber) {
		return 0;
	}
#else
	pcnodeptr = cvbnodetmp + INTSIZE;
#endif
	for (pskey = pstree->pskeyfirst; pskey; pskey = pskeynext) {
		if (pskey->pschild) {
			vvbtreeallfree (ihandle, ikeynumber, pskey->pschild);
		}
		pskey->pschild = NULL;
		pskeynext = pskey->psnext;
		vvbkeyfree (ihandle, ikeynumber, pskey);
	}
	pstree->pskeyfirst = pstree->pskeycurr = pstree->pskeylast = NULL;
	pstree->ikeysinnode = 0;
	while (pcnodeptr - cvbnodetmp < inodelen) {
		pskey = psvbkeyallocate (ihandle, ikeynumber);
		if (!pskey) {
			return errno;
		}
		if (!idups) {
			if (pskeydesc->k_flags & LCOMPRESS) {
#if	ISAMMODE == 1
				icountlc = inl_ldint (pcnodeptr);
				pcnodeptr += INTSIZE;
#else
				icountlc = *(pcnodeptr);
				pcnodeptr++;
#endif
			}
			if (pskeydesc->k_flags & TCOMPRESS) {
#if	ISAMMODE == 1
				icounttc = inl_ldint (pcnodeptr);
				pcnodeptr += INTSIZE;
#else
				icounttc = *(pcnodeptr);
				pcnodeptr++;
#endif
			}
			memcpy (cprevkey + icountlc, pcnodeptr,
				(size_t)(pskeydesc->k_len - (icountlc + icounttc)));
			memset (cprevkey + pskeydesc->k_len - icounttc, TCC, (size_t)icounttc);
			pcnodeptr += pskeydesc->k_len - (icountlc + icounttc);
		}
		if (pskeydesc->k_flags & ISDUPS) {
			pskey->tdupnumber = inl_ldquad (pcnodeptr);
			pcnodeptr += QUADSIZE;
		} else {
			pskey->tdupnumber = 0;
		}
		if (pskeydesc->k_flags & DCOMPRESS) {
			if (*pcnodeptr & 0x80) {
				idups = 1;
			} else {
				idups = 0;
			}
			*pcnodeptr &= ~0x80;
		}
		pskey->trownode = inl_ldquad (pcnodeptr);
		pcnodeptr += QUADSIZE;
		pskey->psparent = pstree;
		if (pstree->pskeyfirst) {
			pstree->pskeylast->psnext = pskey;
		} else {
			pstree->pskeyfirst = pstree->pskeycurr = pskey;
		}
		pstree->pskeylist[pstree->ikeysinnode] = pskey;
		pstree->ikeysinnode++;
		pskey->psprev = pstree->pskeylast;
		pstree->pskeylast = pskey;
		memcpy (pskey->ckey, cprevkey, (size_t)pskeydesc->k_len);
	}
	if (pstree->ilevel) {
		pstree->pskeylast->iishigh = 1;
		memcpy (pstree->pskeylast->ckey, chighkey, (size_t)pskeydesc->k_len);
	}
	pskey = psvbkeyallocate (ihandle, ikeynumber);
	if (!pskey) {
		return errno;
	}
	pskey->psparent = pstree;
	pskey->iisdummy = 1;
	if (pstree->pskeyfirst) {
		pstree->pskeylast->psnext = pskey;
	} else {
		pstree->pskeyfirst = pstree->pskeycurr = pskey;
	}
	pskey->psprev = pstree->pskeylast;
	pstree->pskeylast = pskey;
	pstree->pskeylist[pstree->ikeysinnode] = pskey;
	pstree->ikeysinnode++;
	return 0;
}

int
ivbnodesave (const int ihandle, const int ikeynumber, struct VBTREE *pstree,
	     const off_t tnodenumber, const int imode, const int iposn)
{
	struct DICTINFO	*psvbptr;
	unsigned char	*pckeyendptr, *pcnodeptr, *pcnodehalfway;
	unsigned char	*pcnodeend, *pcprevkey = NULL;
	struct VBKEY	*pskey, *pskeyhalfway = NULL;
	struct keydesc	*pskeydesc;
	int		icountlc = 0,	/* Leading compression */
			icounttc = 0,	/* Trailing compression */
			ilasttc = 0, ikeylen, imaxtc, iresult;

	psvbptr = psvbfile[ihandle];
	pskeydesc = psvbptr->pskeydesc[ikeynumber];
	/*
	 * If it's an INSERT into a node or a DELETE from a node
	 *      *AND*
	 * there's no compression
	 *      *THEN*
	 * We can *TRY* to do a quick and dirty insertion / deletion instead!
	 * However, it's still possible that we have insufficient free space
	 * so we *MAY* still need to continue.
	 */
	if (pstree->ilevel) {
		if (pstree->pskeylast->psprev) {
			pstree->pskeylast->psprev->iishigh = 1;
		}
	}
	if (imode && !(pskeydesc->k_flags & (DCOMPRESS | TCOMPRESS | LCOMPRESS))) {
		if (iquicknodesave (ihandle, pstree, tnodenumber, pskeydesc, imode, iposn) == 0) {
			return 0;
		}
	}
	pcnodehalfway = (ucharptr)cvbnodetmp + (psvbptr->inodesize / 2);
	pcnodeend = (ucharptr)cvbnodetmp + psvbptr->inodesize - 2;
	memset (cvbnodetmp, 0, VB_NODE_MAX);
#if	ISAMMODE == 1
	inl_stquad (inl_ldquad (psvbptr->sdictnode.ctransnumber) + 1,
		    cvbnodetmp + INTSIZE);
	pcnodeptr = (ucharptr)cvbnodetmp + INTSIZE + QUADSIZE;
#else
	pcnodeptr = (ucharptr)cvbnodetmp + INTSIZE;
#endif
	*pcnodeend = pstree->ilevel;
	pstree->ikeysinnode = 0;
	for (pskey = pstree->pskeyfirst; pskey && !pskey->iisdummy;
	     pskey = pskey->psnext) {
		pstree->pskeylist[pstree->ikeysinnode] = pskey;
		pstree->ikeysinnode++;
		if (!pskeyhalfway) {
			if (pcnodeptr >= pcnodehalfway) {
				pskeyhalfway = pskey->psprev;
			}
		}
		ikeylen = pskeydesc->k_len;
		if (pskeydesc->k_flags & TCOMPRESS) {
			ilasttc = icounttc;
			icounttc = 0;
			pckeyendptr = pskey->ckey + ikeylen - 1;
			while (*pckeyendptr-- == TCC && pckeyendptr != pskey->ckey) {
				icounttc++;
			}
#if	ISAMMODE == 1
			ikeylen += INTSIZE - icounttc;
#else
			ikeylen += 1 - icounttc;
#endif
		}
		if (pskeydesc->k_flags & LCOMPRESS) {
			icountlc = 0;
			if (pskey != pstree->pskeyfirst) {
				imaxtc = pskeydesc->k_len - (icounttc >
							     ilasttc ? icounttc : ilasttc);
				for (; pskey->ckey[icountlc] == pcprevkey[icountlc]
				     && icountlc < imaxtc; icountlc++) ;
			}
#if	ISAMMODE == 1
			ikeylen += INTSIZE - icountlc;
#else
			ikeylen += 1 - icountlc;
#endif
			if (pskey->iishigh && pskeydesc->k_flags && LCOMPRESS) {
				icountlc = pskeydesc->k_len;
				icounttc = 0;
#if	ISAMMODE == 1
				if (pskeydesc->k_flags && TCOMPRESS) {
					ikeylen = INTSIZE * 2;
				} else {
					ikeylen = INTSIZE;
				}
#else
				if (pskeydesc->k_flags && TCOMPRESS) {
					ikeylen = 2;
				} else {
					ikeylen = 1;
				}
#endif
				if (pskeydesc->k_flags & DCOMPRESS) {
					ikeylen = 0;
				}
			}
		}
		if (pskeydesc->k_flags & ISDUPS) {
			ikeylen += QUADSIZE;
			/* If the key is a duplicate and it's not first in node */
			if (pskey->iishigh || (pskey != pstree->pskeyfirst
				&& !memcmp (pskey->ckey, pcprevkey, (size_t)pskeydesc->k_len))) {
				if (pskeydesc->k_flags & DCOMPRESS) {
					ikeylen = QUADSIZE;
				}
			}
		}
		ikeylen += QUADSIZE;
		/* Split? */
		if (pcnodeptr + ikeylen >= pcnodeend - 1) {
			if (pstree->pskeylast->psprev->iisnew) {
				pskeyhalfway = pstree->pskeylast->psprev->psprev;
			}
			if (pstree->pskeylast->psprev->iishigh
			    && pstree->pskeylast->psprev->psprev->iisnew) {
				pskeyhalfway = pstree->pskeylast->psprev->psprev->psprev;
			}
			iresult = inodesplit (ihandle, ikeynumber, pstree, pskeyhalfway);
			return iresult;
		}
		if (((ikeylen == (QUADSIZE * 2))
		     && ((pskeydesc->k_flags & (DCOMPRESS | ISDUPS)) == (DCOMPRESS | ISDUPS)))
		    || (pskeydesc->k_flags & DCOMPRESS && !(pskeydesc->k_flags & ISDUPS)
			&& ikeylen == QUADSIZE)) {
			*(pcnodeptr - QUADSIZE) |= 0x80;
		} else {
			if (pskeydesc->k_flags & LCOMPRESS) {
#if	ISAMMODE == 1
				inl_stint (icountlc, pcnodeptr);
				pcnodeptr += INTSIZE;
#else
				*pcnodeptr++ = icountlc;
#endif
			}
			if (pskeydesc->k_flags & TCOMPRESS) {
#if	ISAMMODE == 1
				inl_stint (icounttc, pcnodeptr);
				pcnodeptr += INTSIZE;
#else
				*pcnodeptr++ = icounttc;
#endif
			}
			if (icountlc != pskeydesc->k_len) {
				pcprevkey = pskey->ckey + icountlc;
				imaxtc = pskeydesc->k_len - (icountlc + icounttc);
				while (imaxtc--) {
					*pcnodeptr++ = *pcprevkey++;
				}
			}
			pcprevkey = pskey->ckey;
		}
		if (pskeydesc->k_flags & ISDUPS) {
			inl_stquad (pskey->tdupnumber, pcnodeptr);
			pcnodeptr += QUADSIZE;
		}
		inl_stquad (pskey->trownode, pcnodeptr);
		pcnodeptr += QUADSIZE;
	}
	if (pskey && pskey->iisdummy) {
		pstree->pskeylist[pstree->ikeysinnode] = pskey;
		pstree->ikeysinnode++;
	}
	inl_stint ((int)((ucharptr)pcnodeptr - (ucharptr)cvbnodetmp), cvbnodetmp);
	return ivbblockwrite (ihandle, 1, tnodenumber, cvbnodetmp);
}
