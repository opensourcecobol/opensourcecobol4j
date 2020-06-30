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

static char	*gpsdatarow;		/*  Buffer to hold rows read */
static char	*gpsdatamap[2];		/*  Bitmap of 'used' data rows */
static char	*gpsindexmap[2];	/*  Bitmap of 'used' index nodes */
static char	*cvbnodetmp;
static off_t	gtlastuseddata;		/*  Last row USED in data file */
static off_t	gtdatasize;		/*  # Rows in data file */
static off_t	gtindexsize;		/*  # Nodes in index file */
static int	girebuilddatafree;	/*  If set, we need to rebuild data free list */
static int	girebuildindexfree;	/*  If set, we need to rebuild index free list */
static int	girebuildkey[MAXSUBS];	/*  For any are SET, we need to rebuild that key */

static int
ibittestandset (char *psmap, off_t tbit)
{
	tbit--;
	switch (tbit % 8) {
	case 0:
		if (psmap[tbit / 8] & 0x80) {
			return 1;
		}
		psmap[tbit / 8] |= 0x80;
		break;
	case 1:
		if (psmap[tbit / 8] & 0x40) {
			return 1;
		}
		psmap[tbit / 8] |= 0x40;
		break;
	case 2:
		if (psmap[tbit / 8] & 0x20) {
			return 1;
		}
		psmap[tbit / 8] |= 0x20;
		break;
	case 3:
		if (psmap[tbit / 8] & 0x10) {
			return 1;
		}
		psmap[tbit / 8] |= 0x10;
		break;
	case 4:
		if (psmap[tbit / 8] & 0x08) {
			return 1;
		}
		psmap[tbit / 8] |= 0x08;
		break;
	case 5:
		if (psmap[tbit / 8] & 0x04) {
			return 1;
		}
		psmap[tbit / 8] |= 0x04;
		break;
	case 6:
		if (psmap[tbit / 8] & 0x02) {
			return 1;
		}
		psmap[tbit / 8] |= 0x02;
		break;
	case 7:
		if (psmap[tbit / 8] & 0x01) {
			return 1;
		}
		psmap[tbit / 8] |= 0x01;
		break;
	}
	return 0;
}

static int
ibittestandreset (char *psmap, off_t tbit)
{
	tbit--;
	switch (tbit % 8) {
	case 0:
		if (!(psmap[tbit / 8] & 0x80)) {
			return 1;
		}
		psmap[tbit / 8] ^= 0x80;
		break;
	case 1:
		if (!(psmap[tbit / 8] & 0x40)) {
			return 1;
		}
		psmap[tbit / 8] ^= 0x40;
		break;
	case 2:
		if (!(psmap[tbit / 8] & 0x20)) {
			return 1;
		}
		psmap[tbit / 8] ^= 0x20;
		break;
	case 3:
		if (!(psmap[tbit / 8] & 0x10)) {
			return 1;
		}
		psmap[tbit / 8] ^= 0x10;
		break;
	case 4:
		if (!(psmap[tbit / 8] & 0x08)) {
			return 1;
		}
		psmap[tbit / 8] ^= 0x08;
		break;
	case 5:
		if (!(psmap[tbit / 8] & 0x04)) {
			return 1;
		}
		psmap[tbit / 8] ^= 0x04;
		break;
	case 6:
		if (!(psmap[tbit / 8] & 0x02)) {
			return 1;
		}
		psmap[tbit / 8] ^= 0x02;
		break;
	case 7:
		if (!(psmap[tbit / 8] & 0x01)) {
			return 1;
		}
		psmap[tbit / 8] ^= 0x01;
		break;
	}
	return 0;
}

static int
ipreamble (ihandle)
{
	struct DICTINFO	*psvbptr;
	struct stat	sstat;

	psvbptr = psvbfile[ihandle];
	printf ("Table node size: %d bytes\n", psvbptr->inodesize);
	girebuilddatafree = 0;
	girebuildindexfree = 0;
	gtlastuseddata = 0;
	gpsdatarow = pvvbmalloc ((size_t)psvbptr->imaxrowlength);
	if (!gpsdatarow) {
		printf ("Unable to allocate data row buffer!\n");
		return -1;
	}
	if (fstat (svbfile[psvbptr->idatahandle].ihandle, &sstat)) {
		printf ("Unable to get data status!\n");
		return -1;
	}
	if (psvbptr->iopenmode & ISVARLEN) {
		gtdatasize =
		    (off_t) (sstat.st_size + psvbptr->iminrowlength + INTSIZE +
			     QUADSIZE) / (psvbptr->iminrowlength + INTSIZE +
					  QUADSIZE + 1);
	} else {
		gtdatasize = (off_t) (sstat.st_size +
			     psvbptr->iminrowlength) / (psvbptr->iminrowlength + 1);
	}
	gpsdatamap[0] = pvvbmalloc ((size_t)((gtdatasize + 7) / 8));
	if (gpsdatamap[0] == NULL) {
		printf ("Unable to allocate node map!\n");
		return -1;
	}
	gpsdatamap[1] = pvvbmalloc ((size_t)((gtdatasize + 7) / 8));
	if (gpsdatamap[1] == NULL) {
		printf ("Unable to allocate node map!\n");
		return -1;
	}

	if (fstat (svbfile[psvbptr->iindexhandle].ihandle, &sstat)) {
		printf ("Unable to get index status!\n");
		return -1;
	}
	gtindexsize = (off_t)(sstat.st_size + psvbptr->inodesize -
		     1) / psvbptr->inodesize;
	gpsindexmap[0] = pvvbmalloc ((size_t)((gtindexsize + 7) / 8));
	if (gpsindexmap[0] == NULL) {
		printf ("Unable to allocate node map!\n");
		return -1;
	}
	gpsindexmap[1] = pvvbmalloc ((size_t)((gtindexsize + 7) / 8));
	if (gpsindexmap[1] == NULL) {
		printf ("Unable to allocate node map!\n");
		return -1;
	}
	cvbnodetmp = pvvbmalloc (VB_NODE_MAX);
	if (cvbnodetmp == NULL) {
		printf ("Unable to allocate buffer!\n");
		return -1;
	}
	switch (gtindexsize % 8) {
	case 1:
		*(gpsindexmap[0] + ((gtindexsize - 1) / 8)) = 0x7f;
		break;
	case 2:
		*(gpsindexmap[0] + ((gtindexsize - 1) / 8)) = 0x3f;
		break;
	case 3:
		*(gpsindexmap[0] + ((gtindexsize - 1) / 8)) = 0x1f;
		break;
	case 4:
		*(gpsindexmap[0] + ((gtindexsize - 1) / 8)) = 0x0f;
		break;
	case 5:
		*(gpsindexmap[0] + ((gtindexsize - 1) / 8)) = 0x07;
		break;
	case 6:
		*(gpsindexmap[0] + ((gtindexsize - 1) / 8)) = 0x03;
		break;
	case 7:
		*(gpsindexmap[0] + ((gtindexsize - 1) / 8)) = 0x01;
		break;
	}
	ibittestandset (gpsindexmap[0], 1);	/*  Dictionary node! */

	return 0;
}

static int
idatacheck (int ihandle)
{
	off_t	tloop;
	int	ideleted;

	/*  Mark the entries used by *LIVE* data rows */
	for (tloop = 1; tloop <= gtdatasize; tloop++) {
		if (ivbdataread (ihandle, gpsdatarow, &ideleted, tloop)) {
			continue;	/*  A data file read error! Leave it as free! */
		}
		if (!ideleted) {
			gtlastuseddata = tloop;
			/*  MAYBE we could add index verification here */
			/*  That'd be in SUPER THOROUGH mode only! */
			ibittestandset (gpsdatamap[0], tloop);
		}
		/*  BUG - We also need to set gpsindexmap [1] for those node(s) */
		/*  BUG - that were at least partially consumed by VARLEN data! */
	}
	return 0;
}

static int
idatafreecheck (int ihandle)
{
	struct DICTINFO	*psvbptr;
	off_t		tfreehead, tfreerow, tholdhead, tloop;
	int		iloop, iresult;

	psvbptr = psvbfile[ihandle];
	girebuilddatafree = 1;
	/*  Mark the entries used by the free data list */
	tholdhead = inl_ldquad (psvbptr->sdictnode.cdatafree);
	inl_stquad (0, psvbptr->sdictnode.cdatafree);
	tfreehead = tholdhead;
	memcpy (gpsdatamap[1], gpsdatamap[0], (int)((gtdatasize + 7) / 8));
	memcpy (gpsindexmap[1], gpsindexmap[0], (int)((gtindexsize + 7) / 8));
	while (tfreehead) {
		/*  If the freelist node is > index.EOF, it must be bullshit! */
		if (tfreehead > gtindexsize) {
			return 0;
		}
		iresult = ivbblockread (ihandle, 1, tfreehead, cvbnodetmp);
		if (iresult) {
			return 0;
		}
		/*
		 * If the node has the WRONG signature, then we've got
		 * a corrupt data free list.  We'll rebuild it later!
		 */
/* C-ISAM is not 100% C-ISAM compatible */
#if	ISAMMODE == 1
		if (cvbnodetmp[psvbptr->inodesize - 2] != 0x7f) {
			return 0;
		}
#endif
		if (cvbnodetmp[psvbptr->inodesize - 3] != -1) {
			return 0;
		}
		if (inl_ldint (cvbnodetmp) > (psvbptr->inodesize - 3)) {
			return 0;
		}
		/*
		 * If the node is already 'used' then we have a corrupt
		 * data free list (circular reference).
		 * We'll rebuild the free list later
		 */
		if (ibittestandset (gpsindexmap[1], tfreehead)) {
			return 0;
		}
		for (iloop = INTSIZE + QUADSIZE; iloop < inl_ldint (cvbnodetmp); iloop += QUADSIZE) {
			tfreerow = inl_ldquad (cvbnodetmp + iloop);
			/*
			 * If the row is NOT deleted, then the free
			 * list is screwed so we ignore it and rebuild it
			 * later.
			 */
			if (ibittestandset (gpsdatamap[1], tfreerow)) {
				return 0;
			}
		}
		tfreehead = inl_ldquad (cvbnodetmp + INTSIZE);
	}
	/*  Set the few bits between the last row used and EOF to 'used' */
	for (tloop = gtlastuseddata + 1; tloop <= ((gtdatasize + 7) / 8) * 8; tloop++) {
		ibittestandset (gpsdatamap[1], tloop);
	}
	for (tloop = 0; tloop < (gtdatasize + 7) / 8; tloop++) {
		if (gpsdatamap[1][tloop] != -1) {
			return 0;
		}
	}
	/*  Seems the data file is 'intact' so we'll keep the allocation lists! */
	memcpy (gpsdatamap[0], gpsdatamap[1], (int)((gtdatasize + 7) / 8));
	memcpy (gpsindexmap[0], gpsindexmap[1], (int)((gtindexsize + 7) / 8));
	inl_stquad (tholdhead, psvbptr->sdictnode.cdatafree);
	girebuilddatafree = 0;

	return 0;
}

static int
iindexfreecheck (int ihandle)
{
	struct DICTINFO	*psvbptr;
	off_t		tfreehead, tfreenode, tholdhead;
	int		iloop, iresult;

	psvbptr = psvbfile[ihandle];
	/*  Mark the entries used by the free data list */
	girebuildindexfree = 1;
	tfreehead = inl_ldquad (psvbptr->sdictnode.cnodefree);
	tholdhead = tfreehead;
	inl_stquad (0, psvbptr->sdictnode.cnodefree);
	memcpy (gpsindexmap[1], gpsindexmap[0], (int)((gtindexsize + 7) / 8));
	while (tfreehead) {
		/*  If the freelist node is > index.EOF, it must be bullshit! */
		if (tfreehead > inl_ldquad (psvbptr->sdictnode.cnodecount)) {
			return 0;
		}
		if (tfreehead > gtindexsize) {
			return 0;
		}
		iresult = ivbblockread (ihandle, 1, tfreehead, cvbnodetmp);
		if (iresult) {
			return 0;
		}
		/*
		 * If the node has the WRONG signature, then we've got
		 * a corrupt data free list.  We'll rebuild it later!
		 */
/* C-ISAM is not 100% C-ISAM compatible */
#if	ISAMMODE == 1
		if (cvbnodetmp[psvbptr->inodesize - 2] != 0x7f) {
			return 0;
		}
#endif
		if (cvbnodetmp[psvbptr->inodesize - 3] != -2) {
			return 0;
		}
		if (inl_ldint (cvbnodetmp) > (psvbptr->inodesize - 3)) {
			return 0;
		}
		/*
		 * If the node is already 'used' then we have a corrupt
		 * index free list (circular reference).
		 * We'll rebuild the free list later
		 */
		if (ibittestandset (gpsindexmap[1], tfreehead)) {
			return 0;
		}
		for (iloop = INTSIZE + QUADSIZE; iloop < inl_ldint (cvbnodetmp); iloop += QUADSIZE) {
			tfreenode = inl_ldquad (cvbnodetmp + iloop);
			/*
			 * If the row is NOT deleted, then the free
			 * list is screwed so we ignore it and rebuild it
			 * later.
			 */
			if (ibittestandset (gpsindexmap[1], tfreenode)) {
				return 0;
			}
		}
		tfreehead = inl_ldquad (cvbnodetmp + INTSIZE);
	}
	/*  Seems the index free list is 'intact' so we'll keep the allocation lists! */
	memcpy (gpsindexmap[0], gpsindexmap[1], (int)((gtindexsize + 7) / 8));
	inl_stquad (tholdhead, psvbptr->sdictnode.cnodefree);
	girebuildindexfree = 0;

	return 0;
}

static int
icheckkeydesc (int ihandle)
{
	struct DICTINFO	*psvbptr;
	off_t		tnode;

	psvbptr = psvbfile[ihandle];
	tnode = inl_ldquad (psvbptr->sdictnode.cnodekeydesc);
	while (tnode) {
		if (tnode > gtindexsize) {
			return 1;
		}
		if (ibittestandset (gpsindexmap[0], tnode)) {
			return 1;
		}
		if (ivbblockread (ihandle, 1, tnode, cvbnodetmp)) {
			return 1;
		}
		if (cvbnodetmp[psvbptr->inodesize - 3] != -1) {
			return 1;
		}
		if (cvbnodetmp[psvbptr->inodesize - 2] != 0x7e) {
			return 1;
		}
		if (cvbnodetmp[psvbptr->inodesize - 1] != 0) {
			return 1;
		}
		tnode = inl_ldquad (cvbnodetmp + INTSIZE);
	}
	return 0;
}

static int
ichecktree (int ihandle, int ikey, off_t tnode, int ilevel)
{
	int		iloop;
	struct VBTREE	stree;

	memset (&stree, 0, sizeof (stree));
	if (tnode > gtindexsize) {
		return 1;
	}
	if (ibittestandset (gpsindexmap[1], tnode)) {
		return 1;
	}
	stree.ttransnumber = -1;
	if (ivbnodeload (ihandle, ikey, &stree, tnode, ilevel)) {
		return 1;
	}
	for (iloop = 0; iloop < stree.ikeysinnode; iloop++) {
		if (stree.pskeylist[iloop]->iisdummy) {
			continue;
		}
		if (iloop > 0
		    && ivbkeycompare (ihandle, ikey, 0, stree.pskeylist[iloop - 1]->ckey,
				      stree.pskeylist[iloop]->ckey) > 0) {
			printf ("Index is out of order!\n");
			vvbkeyallfree (ihandle, ikey, &stree);
			return 1;
		}
		if (iloop > 0
		    && ivbkeycompare (ihandle, ikey, 0, stree.pskeylist[iloop - 1]->ckey,
				      stree.pskeylist[iloop]->ckey) == 0
		    && stree.pskeylist[iloop - 1]->tdupnumber >=
		    stree.pskeylist[iloop]->tdupnumber) {
			printf ("Index is out of order!\n");
			vvbkeyallfree (ihandle, ikey, &stree);
			return 1;
		}
		if (stree.ilevel) {
			if (ichecktree (ihandle, ikey, stree.pskeylist[iloop]->trownode,
					stree.ilevel)) {
				vvbkeyallfree (ihandle, ikey, &stree);
				return 1;
			}
		} else {
			if (ibittestandreset (gpsdatamap[1], stree.pskeylist[iloop]->trownode)) {
				vvbkeyallfree (ihandle, ikey, &stree);
				printf ("Bad data row pointer!\n");
				return 1;
			}
		}
	}
	vvbkeyallfree (ihandle, ikey, &stree);
	return 0;
}

static int
icheckkey (int ihandle, int ikey)
{
	struct DICTINFO	*psvbptr;
	off_t		tloop;

	psvbptr = psvbfile[ihandle];
	memcpy (gpsdatamap[1], gpsdatamap[0], (int)((gtdatasize + 7) / 8));
	memcpy (gpsindexmap[1], gpsindexmap[0], (int)((gtindexsize + 7) / 8));
	if (ivbblockread (ihandle, 1, psvbptr->pskeydesc[ikey]->k_rootnode, cvbnodetmp)) {
		return 1;
	}
	if (ichecktree (ihandle, ikey, psvbptr->pskeydesc[ikey]->k_rootnode,
			cvbnodetmp[psvbptr->inodesize - 2] + 1)) {
		return 1;
	}
	for (tloop = 0; tloop < (gtdatasize + 7) / 8; tloop++) {
		if (gpsdatamap[1][tloop]) {
			return 1;
		}
	}
	memcpy (gpsindexmap[0], gpsindexmap[1], (int)((gtindexsize + 7) / 8));
	return 0;
}

static int
iindexcheck (int ihandle)
{
	struct DICTINFO	*psvbptr;
	int		ikey, iloop, ipart;

	for (iloop = 0; iloop < MAXSUBS; iloop++) {
		girebuildkey[iloop] = 0;
	}
	/*  If the keydesc node(s) are bad, we QUIT this table altogether */
	if (icheckkeydesc (ihandle)) {
		printf ("Corrupted Key Descriptor node(s)! Can't continue!\n");
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	for (ikey = 0; ikey < psvbptr->inkeys; ikey++) {
		printf ("Index %d: ", ikey + 1);
		printf ("%s ",
			psvbptr->pskeydesc[ikey]->
			k_flags & ISDUPS ? "ISDUPS" : "ISNODUPS");
		printf ("%s",
			psvbptr->pskeydesc[ikey]->
			k_flags & DCOMPRESS ? "DCOMPRESS " : "");
		printf ("%s",
			psvbptr->pskeydesc[ikey]->
			k_flags & LCOMPRESS ? "LCOMPRESS " : "");
		printf ("%s\n",
			psvbptr->pskeydesc[ikey]->
			k_flags & TCOMPRESS ? "TCOMPRESS" : "");
		for (ipart = 0; ipart < psvbptr->pskeydesc[ikey]->k_nparts; ipart++) {
			printf (" Part %d: ", ipart + 1);
			printf ("%d,",
				psvbptr->pskeydesc[ikey]->k_part[ipart].kp_start);
			printf ("%d,",
				psvbptr->pskeydesc[ikey]->k_part[ipart].kp_leng);
			switch (psvbptr->pskeydesc[ikey]->k_part[ipart].
				kp_type & ~ISDESC) {
			case CHARTYPE:
				printf ("CHARTYPE");
				break;
			case INTTYPE:
				printf ("INTTYPE");
				break;
			case LONGTYPE:
				printf ("LONGTYPE");
				break;
			case DOUBLETYPE:
				printf ("DOUBLETYPE");
				break;
			case FLOATTYPE:
				printf ("FLOATTYPE");
				break;
			case QUADTYPE:
				printf ("QUADTYPE");
				break;
			default:
				printf ("UNKNOWN TYPE");
				break;
			}
			if (psvbptr->pskeydesc[ikey]->k_part[ipart].kp_type & ISDESC) {
				printf (" ISDESC\n");
			} else {
				printf ("\n");
			}
		}
		/*  If the index is screwed, write out an EMPTY root node and */
		/*  flag the index for a complete reconstruction later on */
		if (icheckkey (ihandle, ikey)) {
			memset (cvbnodetmp, 0, VB_NODE_MAX);
			inl_stint (2, cvbnodetmp);
			ivbblockwrite (ihandle, 1,
				       psvbptr->pskeydesc[ikey]->k_rootnode,
				       cvbnodetmp);
			ibittestandset (gpsindexmap[0],
					psvbptr->pskeydesc[ikey]->k_rootnode);
			vvbtreeallfree (ihandle, ikey, psvbptr->pstree[ikey]);
			psvbptr->pstree[ikey] = NULL;
			girebuildkey[ikey] = 1;
		}
	}
	return 0;
}

static void
vrebuildindexfree (int ihandle)
{
	struct DICTINFO	*psvbptr;
	off_t		tloop;

	psvbptr = psvbfile[ihandle];
	printf ("Rebuilding index free list\n");
	if (gtindexsize > inl_ldquad (psvbptr->sdictnode.cnodecount)) {
		inl_stquad (gtindexsize, psvbptr->sdictnode.cnodecount);
	}
	for (tloop = 1; tloop <= gtindexsize; tloop++) {
		if (ibittestandset (gpsindexmap[0], tloop)) {
			continue;
		}
		iserrno = ivbnodefree (ihandle, tloop);
		if (iserrno) {
			printf ("Error %d rebuilding index free list!\n", iserrno);
			return;
		}
	}
	return;
}

static void
vrebuilddatafree (int ihandle)
{
	off_t tloop;

	printf ("Rebuilding data free list\n");
	for (tloop = 1; tloop <= gtlastuseddata; tloop++) {
		if (ibittestandset (gpsdatamap[0], tloop)) {
			continue;
		}
		iserrno = ivbdatafree (ihandle, tloop + 1);
		if (iserrno) {
			printf ("Error %d rebuilding data free list!\n", iserrno);
			return;
		}
	}
	return;
}

static void
vaddkeyforrow (int ihandle, int ikey, off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*psKey;
	off_t		tdupnumber;
	int		iresult;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	psvbptr = psvbfile[ihandle];
	if (psvbptr->pskeydesc[ikey]->k_nparts == 0) {
		return;
	}
	vvbmakekey (psvbptr->pskeydesc[ikey], gpsdatarow, ckeyvalue);
	iresult = ivbkeysearch (ihandle, ISGREAT, ikey, 0, ckeyvalue, 0);
	tdupnumber = 0;
	if (iresult >= 0 && !ivbkeyload (ihandle, ikey, ISPREV, 0, &psKey)
	    && !memcmp (psKey->ckey, ckeyvalue,
			psvbptr->pskeydesc[ikey]->k_len)) {
		iserrno = EDUPL;
		if (psvbptr->pskeydesc[ikey]->k_flags & ISDUPS) {
			tdupnumber = psKey->tdupnumber + 1;
		} else {
			printf ("Error! Duplicate entry in key %d\n", ikey);
			return;
		}
		iresult = ivbkeysearch (ihandle, ISGTEQ, ikey, 0, ckeyvalue, tdupnumber);
	}

	ivbkeyinsert (ihandle, NULL, ikey, ckeyvalue, trownumber, tdupnumber,
		      NULL);
	return;
}

static void
vrebuildkeys (int ihandle)
{
	off_t	trownumber;
	int	ideleted, ikey;

	/*  Mark the entries used by *LIVE* data rows */
	for (trownumber = 1; trownumber <= gtdatasize; trownumber++) {
		if (ivbdataread (ihandle, gpsdatarow, &ideleted, trownumber)) {
			continue;	/*  A data file read error! Leave it as free! */
		}
		if (!ideleted) {
			for (ikey = 0; ikey < MAXSUBS; ikey++) {
				if (girebuildkey[ikey]) {
					vaddkeyforrow (ihandle, ikey, trownumber);
				}
			}
		}
	}
	return;
}

static void
ipostamble (int ihandle)
{
	off_t	tloop;
	int	ikeycounttorebuild = 0, iloop;

	for (tloop = 0; tloop < (gtindexsize + 7) / 8; tloop++) {
		if (gpsindexmap[0][tloop] != -1) {
			girebuildindexfree = 1;
		}
	}

	if (girebuildindexfree) {
		vrebuildindexfree (ihandle);
	}
	if (girebuilddatafree) {
		vrebuilddatafree (ihandle);
	}
	for (iloop = 0; iloop < MAXSUBS; iloop++)
		if (girebuildkey[iloop]) {
			if (!ikeycounttorebuild) {
				printf ("Rebuilding keys: ");
			}
			printf ("%d ", iloop + 1);
			ikeycounttorebuild++;
		}
	if (ikeycounttorebuild) {
		vrebuildkeys (ihandle);
		printf ("\n");
	}
	inl_stquad (gtlastuseddata, psvbfile[ihandle]->sdictnode.cdatacount);
	/*  Other stuff here */
	vvbfree (gpsdatarow);
	vvbfree (cvbnodetmp);

	vvbfree (gpsdatamap[0]);
	vvbfree (gpsdatamap[1]);
	vvbfree (gpsindexmap[0]);
	vvbfree (gpsindexmap[1]);
}

static void
vprocess (int ihandle)
{
	if (ipreamble (ihandle)) {
		return;
	}
	if (idatacheck (ihandle)) {
		return;
	}
	if (iindexcheck (ihandle)) {
		return;
	}
	if (idatafreecheck (ihandle)) {
		return;
	}
	if (iindexfreecheck (ihandle)) {
		return;
	}
	ipostamble (ihandle);
}

int
ischeck (const char *pcfile)
{
	int ihandle;

	ihandle = isopen (pcfile, ISINOUT + ISEXCLLOCK);
	if (ihandle < 0 && iserrno == EROWSIZE) {
		printf ("Recovery of ISVARLEN files not possible\n");
		printf ("File %s contains variable length rows!\n", pcfile);
		isfullclose (ihandle);
		return -1;
	}
	/* if (ihandle < 0 && iserrno == EROWSIZE) */
	/* ihandle = isopen (ppcargv [iloop], ISINOUT+ISVARLEN+ISEXCLLOCK); */
	if (ihandle < 0) {
		printf ("Error %d opening %s\n", iserrno, pcfile);
		return -1;
	}

	printf ("Processing: %s\n", pcfile);
	ivbenter (ihandle, 1, 0);
	vprocess (ihandle);
	psvbfile[ihandle]->iisdictlocked |= 0x06;
	ivbexit (ihandle);

	isfullclose (ihandle);
	return 0;
}
