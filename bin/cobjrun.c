/*
 * Copyright (C) 2002-2009 Keisuke Nishida
 * Copyright (C) 2007-2009 Roger While
 * Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
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
#include	<stdlib.h>

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

static int flag_help = 0;
static int flag_version = 0;
static char *program_id;

struct path_list {
	char *path;
	struct path_list *next;
};

static const char* default_include_path = "./";

static struct path_list* include_paths = NULL;

static const char short_options[] = "hVi";

static const struct option long_options[] = {
	{"help", no_argument, NULL, 'h'},
	{"version", no_argument, NULL, 'V'},
	{"include", required_argument, NULL, 'i'},
	{NULL, 0, NULL, 0}
};


static int cobjrun_run_module();

static void
cobjrun_print_version (void)
{
	int	year;
	int	day;
	const size_t buff_size = 64;
	char	buff[buff_size];
	char	month[buff_size];

	memset (buff, 0, sizeof(buff));
	memset (month, 0, sizeof(month));
	day = 0;
	year = 0;
	sscanf (__DATE__, "%s %d %d", month, &day, &year);
	if (day && year) {
		snprintf (buff, buff_size, "%s %2.2d %4.4d %s", month, day, year, __TIME__);
	} else {
		snprintf (buff, buff_size, "%s %s", __DATE__, __TIME__);
	}
	printf ("cobjrun (%s) %s\n",
		PACKAGE_NAME, PACKAGE_VERSION);
	puts ("Copyright (C) 2022-2022 TOKYO SYSTEM HOUSE CO.,LTD.");
}

static void
cobjrun_print_usage (void)
{
	printf ("Usage: cobjrun [--include=PATH] PROGRAM-ID\n");
	printf (" --include=PATH   Specify the directory path containing jar files\n");
	printf (" --help           Display this message\n");
	printf (" --version        Display runtime version\n");
}

static void
add_include_path(char* path)
{
	struct path_list *new_path = malloc(sizeof(struct path_list));
	new_path->path = strdup(path);
	new_path->next = include_paths;
	include_paths = new_path;
}

static void
init_include_paths()
{
	add_include_path((char*)default_include_path);
}

static void
free_include_paths() {
	for(;include_paths; include_paths = include_paths->next) {
		free(include_paths->path);
	}
}

static int
process_command_line (int argc, char *argv[])
{
	int			c, idx;

	/* At least one option or module name needed */
	if (argc <= 1) {
		flag_help = 1;
		return 1;
	}

	while ((c = getopt_long_only (argc, argv, short_options, long_options, &idx)) >= 0) {
		switch (c) {
		case '?':
			return 0;
		case 'h':
			flag_help = 1;
			break;
		case 'V':
			flag_version = 1;
			break;
		case 'i':
			if(!optarg) {
				return 0;
			}
			add_include_path(strdup(optarg));
			break;
		}
	}

	if(optind < argc) {
		program_id = argv[optind];
	} else {
		return 0;
	}

	return 1;
}

static int cobjrun_run_module() {
	char buff[8192];
	char* buff_ptr = buff;
	struct path_list* list;

	buff[0] = 0;

	const char* head = "java -cp \"$CLASSPATH:";
	strcat(buff_ptr, "java -cp \"$CLASSPATH:");
	buff_ptr += strlen(head);

	const char* ext = "/*:";
	for(list=include_paths; list; list=list->next) {
		strcat(buff_ptr, list->path);
		buff_ptr += strlen(list->path);
		strcat(buff_ptr, ext);
		buff_ptr += strlen(ext);
	}

	sprintf(buff_ptr, "\" %s", program_id);
	return system(buff);
}

int
main (int argc, char **argv)
{
	int return_code;
	int pcl_return_code;
	
#ifdef	HAVE_SETLOCALE
	setlocale (LC_ALL, "");
#endif

	init_include_paths();
	pcl_return_code = process_command_line (argc, argv);
	if(!pcl_return_code) {
		fprintf(stderr, "Failed to parse arguments.\n");
		fprintf(stderr, "Run cobjrun --help.\n");
		return 1;
	}

	if(flag_help) {
		free_include_paths();
		cobjrun_print_usage();
		return 0;
	}

	if(flag_version) {
		free_include_paths();
		cobjrun_print_version ();
		return 0;
	}

	if (strlen (argv[1]) > 31) {
		fprintf (stderr, "Invalid PROGRAM name\n");
		return 1;
	}

	return_code = cobjrun_run_module();
	free_include_paths();
	return return_code;
}
