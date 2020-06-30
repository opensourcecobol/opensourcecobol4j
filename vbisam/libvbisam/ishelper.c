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

/* Global functions */

int
iscluster (const int ihandle, struct keydesc *pskeydesc)
{
	/* BUG Write iscluster() and don't forget to call ivbtranscluster */
	if (ihandle < 0 || ihandle > ivbmaxusedhandle) {
		iserrno = EBADARG;
		return -1;
	}
	return 0;
}

int
iserase (char *pcfilename)
{
	int	ihandle;
	char	cbuffer[1024];

	for (ihandle = 0; ihandle <= ivbmaxusedhandle; ihandle++) {
		if (psvbfile[ihandle] != NULL) {
			if (!strcmp (psvbfile[ihandle]->cfilename, pcfilename)) {
				isclose (ihandle);
				ivbclose3 (ihandle);
				break;
			}
		}
	}
	sprintf (cbuffer, "%s.idx", pcfilename);
	unlink (cbuffer);
	sprintf (cbuffer, "%s.dat", pcfilename);
	unlink (cbuffer);
	return ivbtranserase (pcfilename);
}

int
isflush (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (ihandle < 0 || ihandle > ivbmaxusedhandle) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	if (psvbptr->iindexhandle >= 0) {
		fsync (psvbptr->iindexhandle);
	}
	if (psvbptr->idatahandle >= 0) {
		fsync (psvbptr->idatahandle);
	}
	return 0;
}

int
islock (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (ihandle < 0 || ihandle > ivbmaxusedhandle) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	return ivbdatalock (ihandle, VBWRLOCK, (off_t)0);
}

int
isrelcurr (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (ihandle < 0 || ihandle > ivbmaxusedhandle) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	if (ivbintrans != VBNOTRANS) {
		return 0;
	}
	if (!psvbptr->trownumber) {
		iserrno = ENOREC;
		return -1;
	}
	iserrno = ivbdatalock (ihandle, VBUNLOCK, psvbptr->trownumber);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
isrelease (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (ihandle < 0 || ihandle > ivbmaxusedhandle) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	if (ivbintrans != VBNOTRANS) {
		return 0;
	}
	ivbdatalock (ihandle, VBUNLOCK, (off_t)0);	/* Ignore the return */
	return 0;
}

int
isrelrec (const int ihandle, const vbisam_off_t trownumber)
{
	struct DICTINFO	*psvbptr;

	if (ihandle < 0 || ihandle > ivbmaxusedhandle) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	iserrno = ivbdatalock (ihandle, VBUNLOCK, trownumber);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
isrename (char *pcoldname, char *pcnewname)
{
	int	iresult;
	char	cbuffer[2][1024];

	sprintf (cbuffer[0], "%s.idx", pcoldname);
	sprintf (cbuffer[1], "%s.idx", pcnewname);
	iresult = rename (cbuffer[0], cbuffer[1]);
	if (iresult == -1) {
		goto renameexit;
	}
	sprintf (cbuffer[0], "%s.dat", pcoldname);
	sprintf (cbuffer[1], "%s.dat", pcnewname);
	iresult = rename (cbuffer[0], cbuffer[1]);
	if (iresult == -1) {
		sprintf (cbuffer[0], "%s.idx", pcoldname);
		sprintf (cbuffer[1], "%s.idx", pcnewname);
		rename (cbuffer[1], cbuffer[0]);
		goto renameexit;
	}
	return ivbtransrename (pcoldname, pcnewname);
renameexit:
	iserrno = errno;
	return -1;
}

int
issetunique (const int ihandle, const vbisam_off_t tuniqueid)
{
	struct DICTINFO *psvbptr;
	off_t		tvalue;
	int		iresult, iresult2;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	iserrno = 0;
	if (!psvbptr->iisdictlocked) {
		iserrno = EBADARG;
		return -1;
	}
	tvalue = inl_ldquad (psvbptr->sdictnode.cuniqueid);
	if (tuniqueid > tvalue) {
		inl_stquad (tuniqueid, psvbptr->sdictnode.cuniqueid);
		psvbptr->iisdictlocked |= 0x02;
	}

	iresult = ivbtranssetunique (ihandle, tuniqueid);
	psvbptr->iisdictlocked |= 0x02;
	iresult2 = ivbexit (ihandle);
	if (iresult) {
		return -1;
	}
	return iresult2;
}

int
isuniqueid (const int ihandle, vbisam_off_t *ptuniqueid)
{
	struct DICTINFO *psvbptr;
	off_t		tvalue;
	int		iresult;
	int		iresult2;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}

	psvbptr = psvbfile[ihandle];
	iserrno = 0;
	if (!psvbptr->iisdictlocked) {
		iserrno = EBADARG;
		return -1;
	}
	tvalue = inl_ldquad (psvbptr->sdictnode.cuniqueid);
	inl_stquad (tvalue + 1, psvbptr->sdictnode.cuniqueid);
	psvbptr->iisdictlocked |= 0x02;
	iresult = ivbtransuniqueid (ihandle, tvalue);
	iresult2 = ivbexit (ihandle);
	if (iresult) {
		return -1;
	}
	*ptuniqueid = tvalue;
	return iresult2;
}

int
isunlock (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	return ivbdatalock (ihandle, VBUNLOCK, (off_t)0);
}

char *
isdi_name (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return NULL;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return NULL;
	}
	return strdup (psvbptr->cfilename);
}

int
isdi_datlen (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	return psvbptr->imaxrowlength;
}

int
isdi_idxfd (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	return psvbptr->iindexhandle;
}

int
isdi_datfd (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	return psvbptr->idatahandle;
}

int
isdi_curidx (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	return psvbptr->iactivekey;
}

struct keydesc *
isdi_kdsc (const int ihandle)
{
	struct DICTINFO	*psvbptr;
	struct keydesc	*keydptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return NULL;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return NULL;
	}
	keydptr = pvvbmalloc (sizeof(struct keydesc));
	if (keydptr) {
		memcpy (keydptr, psvbptr->pskeydesc[psvbptr->iactivekey],
			sizeof(struct keydesc));
	}
	return keydptr;
}

void
ldchar (char *pcsource, int ilength, char *pcdestination)
{
	char *pcdst;

	memcpy ((void *)pcdestination, (void *)pcsource, (size_t)ilength);
	for (pcdst = pcdestination + ilength - 1; pcdst >= (char *)pcdestination; pcdst--) {
		if (*pcdst != ' ') {
			pcdst++;
			*pcdst = 0;
			return;
		}
	}
	*(++pcdst) = 0;
}

void
stchar (char *pcsource, char *pcdestination, int ilength)
{
	char *pcsrc, *pcdst;
	int icount;

	pcsrc = pcsource;
	pcdst = pcdestination;
	for (icount = ilength; icount && *pcsrc; icount--, pcsrc++, pcdst++) {
		*pcdst = *pcsrc;
	}
	for (; icount; icount--, pcdst++) {
		*pcdst = ' ';
	}
}

int
ldint (void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
	return (int)VB_BSWAP_16 (*(unsigned short *)pclocation);
#else
	short ivalue = 0;
	unsigned char *pctemp = (unsigned char *)&ivalue;
	unsigned char *pctemp2 = (unsigned char *)pclocation;

	*(pctemp + 0) = *(pctemp2 + 0);
	*(pctemp + 1) = *(pctemp2 + 1);
	return (int)ivalue;
#endif
}

void
stint (int ivalue, void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
	*(unsigned short *)pclocation = VB_BSWAP_16 ((unsigned short)ivalue);
#else
	unsigned char *pctemp = (unsigned char *)&ivalue;

	*((unsigned char *)pclocation + 0) = *(pctemp + 0 + INTSIZE);
	*((unsigned char *)pclocation + 1) = *(pctemp + 1 + INTSIZE);
#endif
}

int
ldlong (void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
	return VB_BSWAP_32 (*(unsigned int *)pclocation);
#else
	int lvalue;

	memcpy ((unsigned char *)&lvalue, (unsigned char *)pclocation, 4);
	return lvalue;
#endif
}

void
stlong (int lvalue, void *pclocation)
{
#ifndef	WORDS_BIGENDIAN
	*(unsigned int *)pclocation = VB_BSWAP_32 ((unsigned int)lvalue);
#else
	memcpy ((unsigned char *)pclocation, (unsigned char *)&lvalue, 4);
#endif
}

double
ldfloat (void *pclocation)
{
	float ffloat;
	double ddouble;

	memcpy (&ffloat, pclocation, FLOATSIZE);
	ddouble = ffloat;
	return (double)ddouble;
}

void
stfloat (double dsource, void *pcdestination)
{
	float ffloat;

	ffloat = dsource;
	memcpy (pcdestination, &ffloat, FLOATSIZE);
}

double
ldfltnull (void *pclocation, short *pinullflag)
{
	double dvalue;

	*pinullflag = 0;
	dvalue = ldfloat (pclocation);
	return (double)dvalue;
}

void
stfltnull (double dsource, void *pcdestination, int inullflag)
{
	if (inullflag) {
		dsource = 0;
	}
	stfloat (dsource, pcdestination);
}

double
lddbl (void *pclocation)
{
	double ddouble;

	memcpy (&ddouble, pclocation, DOUBLESIZE);
	return ddouble;
}

void
stdbl (double dsource, void *pcdestination)
{
	memcpy (pcdestination, &dsource, DOUBLESIZE);
}

double
lddblnull (void *pclocation, short *pinullflag)
{
	*pinullflag = 0;
	return (lddbl (pclocation));
}

void
stdblnull (double dsource, void *pcdestination, int inullflag)
{
	if (inullflag) {
		dsource = 0;
	}
	stdbl (dsource, pcdestination);
}
