bin_PROGRAMS = vbcheck vbrecover
vbcheck_SOURCES = vbcheck.c
vbrecover_SOURCES = vbrecover.c

vbcheck_CFLAGS = $(AM_CFLAGS) -I$(top_srcdir) -I$(top_srcdir)/libvbisam
vbrecover_CFLAGS = $(AM_CFLAGS) -I$(top_srcdir) -I$(top_srcdir)/libvbisam
# vbcheck_LDADD = -L$(top_builddir)/libvbisam -lvbisam
# vbrecover_LDADD = -L$(top_builddir)/libvbisam -lvbisam
vbcheck_LDADD = ../libvbisam/libvbisam.la
vbrecover_LDADD = ../libvbisam/libvbisam.la
