/*
 * Copyright (C) 2004-2009 Roger While
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License 
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include	"config.h"
#include	"defaults.h"

#include	<stdio.h>
#include	<string.h>
#include	"libcob.h"

#include	"tarstamp.h"

#ifdef	HAVE_KPATHSEA_GETOPT_H
#include <kpathsea/getopt.h>
#else
#ifdef	HAVE_GETOPT_H
#include <getopt.h>
#else
#include "lib/getopt.h"
#endif
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

static const char short_options[] = "hV";

static const struct option long_options[] = {
	{"help", no_argument, NULL, 'h'},
	{"version", no_argument, NULL, 'V'},
	{NULL, 0, NULL, 0}
};

static void
cobcrun_print_version (void)
{
	int	year;
	int	day;
	char	buff[64];
	char	month[64];

	memset (buff, 0, sizeof(buff));
	memset (month, 0, sizeof(month));
	day = 0;
	year = 0;
	sscanf (__DATE__, "%s %d %d", month, &day, &year);
	if (day && year) {
		sprintf (buff, "%s %2.2d %4.4d %s", month, day, year, __TIME__);
	} else {
		sprintf (buff, "%s %s", __DATE__, __TIME__);
	}
	printf ("cobcrun (%s) %s.%d\n",
		PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts ("Copyright (C) 2004-2009 Roger While");
	printf ("Built    %s\nPackaged %s\n", buff, octardate);
}

static void
cobcrun_print_usage (void)
{
	printf ("Usage: cobcrun PROGRAM [param ...]");
	printf ("\n\n");
	printf ("or   : cobcrun --help");
	printf ("\n");
	printf ("       Display this message");
	printf ("\n\n");
	printf ("or   : cobcrun --version, -V");
	printf ("\n");
	printf ("       Display runtime version");
	printf ("\n\n");
}

static int
process_command_line (int argc, char *argv[])
{
	int			c, idx;

	/* At least one option or module name needed */
	if (argc <= 1) {
		cobcrun_print_usage ();
		return 1;
	}

	/* Translate first command line argument from WIN to UNIX style */
	if (strrchr(argv[1], '/') == argv[1]) {
		argv[1][0] = '-';
	}

	/* Process first command line argument only if not a module */
	if (argv[1][0] != '-') {
		return 99;
	}

	c = getopt_long_only (argc, argv, short_options, long_options, &idx);
	if (c > 0) {
		switch (c) {
		case '?':
			return 1;
		case 'h':
			cobcrun_print_usage ();
			return 0;
		case 'V':
			cobcrun_print_version ();
			return 0;
		}
	}

	return 99;
}

int
main (int argc, char **argv)
{
	int pcl_return;
	
	union {
		int	(*func)();
		void	*func_void;
	} unifunc;
	
#ifdef	HAVE_SETLOCALE
	setlocale (LC_ALL, "");
#endif

	pcl_return = process_command_line (argc, argv);

	if (pcl_return != 99) {
		return pcl_return;
	}

	if (strlen (argv[1]) > 31) {
		fprintf (stderr, "Invalid PROGRAM name\n");
		return 1;
	}
	cob_init (argc - 1, &argv[1]);
	unifunc.func_void = cob_resolve (argv[1]);
	if (unifunc.func_void == NULL) {
		cob_call_error ();
	}
	cob_stop_run ( unifunc.func() );
}
