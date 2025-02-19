/*
 * Copyright (C) 2001-2009 Keisuke Nishida
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

changequote(>>>>>,<<<<<)

%option 8bit
%option caseless
%option noyywrap
%option never-interactive
%option prefix="pp"

%{

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "cobj.h"
#include "ppparse.h"

enum {
	CB_COMPILE_STATUS_NONE,
	CB_COMPILE_STATUS_TRUE,
	CB_COMPILE_STATUS_FALSE,
	CB_COMPILE_STATUS_FALSE_END,
	CB_COMPILE_STATUS_TRUE_ELSE,
	CB_COMPILE_STATUS_FALSE_ELSE,
	CB_COMPILE_STATUS_ERROR
};

static char	*plexbuff1 = NULL;
static char	*plexbuff2 = NULL;
static size_t	newline_count = 0;
static size_t	within_comment = 0;
static size_t	inside_bracket = 0;
static size_t	consecutive_quotation = 0;
static int	quotation_mark = 0;
static int	last_line_1 = -1;
static int	last_line_2 = -1;

/* for COBOL68 style COPY in data description entry */
static int	suppress_echo = 0;
static int	current_division = PP_OUT_OF_DIVISION;
static int	omit_data_entry_name = 0;
static int	omit_data_redef_name = 0;

#define MAX_DEPTH 10
static int	cb_compile_status = CB_COMPILE_STATUS_NONE;
static int	cb_compile_status_list[MAX_DEPTH];
static int	compile_directive_depth = -1;


static struct cb_replace_list	*current_replace_list = NULL;

static struct cb_replace_list	*current_copy_replace_list = NULL;

static struct cb_joining_ext	*current_joining_ext = NULL;

static struct cb_text_list	*text_queue1 = NULL; /* for COPY REPLACING */
static struct cb_text_list	*text_queue2 = NULL; /* for REPLACE */

static struct copy_info {
	struct copy_info	*next;
	char			*file;
	int			line;
	int			replacing;
	int			joining;
	int			quotation_mark;
	YY_BUFFER_STATE		buffer;
} *copy_stack = NULL;

#define YY_INPUT(buf, result, max_size) result = ppinput (buf, max_size);

static int ppinput (char *buf, int max_size);
static void ppecho (const char *text);

#ifdef	I18N_UTF8
static void convert_ucs_hyphen_minus (char *namebuf);
#endif /*I18N_UTF8*/

static void ppecho_dataname (char *datanamebuf);

static void switch_to_buffer (const int lineno, const char *filename,
			      YY_BUFFER_STATE buffer);

%}
ifdef(M4.I18N_UTF8,>>>>>
ZENSPC		[\xE3][\x80][\x80]
U00_7F		[\x00-\x7F]
U80_7FF		[\xC2-\xDF][\x80-\xBF]
U800_FFF	[\xE0][\xA0-\xBF][\x80-\xBF]
U1000_CFFF	[\xE1-\xEC][\x80-\xBF][\x80-\xBF]
U1000_EX_ZENSPC	[\xE1-\xE2][\x80-\xBF][\x80-\xBF]|[\xE3][\x80-\xBF][\x81-\xBF]|[\xE3][\x81-\xBF][\x80-\xBF]|[\xE4-\xEC][\x80-\xBF][\x80-\xBF]
UD000_D7FF	[\xED][\x80-\x9F][\x80-\xBF]
UE000_FFFF	[\xEE-\xEF][\x80-\xBF][\x80-\xBF]
U10000_3FFFF	[\xF0][\x90-\xBF][\x80-\xBF][\x80-\xBF]
U30000_FFFFF	[\xF1-\xF3][\x80-\xBF][\x80-\xBF][\x80-\xBF]
U100000_10FFFF	[\xF4][\x80-\x8F][\x80-\xBF][\x80-\xBF]
UTF8_EXT	{U80_7FF}|{U800_FFF}|{U1000_EX_ZENSPC}|{UD000_D7FF}|{UE000_FFFF}|{U10000_3FFFF}|{U30000_FFFFF}|{U100000_10FFFF}
UTF8		{U00_7F}|{UTF8_EXT}
WORD		({UTF8_EXT}|[_0-9A-Z-])+
<<<<<,>>>>>
ZENSPC		[\x81][\x40]
SJIS		[\x81-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC]
SJIS_EX_ZENSPC	[\x82-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC]|[\x81][\x40-\x7E\x80-\xFC]{-}[\x40]
JPNWORD		[\xA0-\xDF]|{SJIS_EX_ZENSPC}
WORD		([_0-9A-Z-]|{JPNWORD})+
<<<<<)
NUMRIC_LITERAL	[+-]?[0-9,.]*[0-9]
ALNUM_LITERAL	\"[^\"\n]*\"|\'[^\'\n]*\'

%x PROCESS_STATE COPY_STATE PSEUDO_STATE DATANAME_JOIN_STATE

%%

%{
%}

<*>"*>".* {
	ppecho (" ");
}

"IDENTIFICATION"({ZENSPC}|[ ])+"DIVISION"({ZENSPC}|[ ])*"."	{
	identification_division_line_number = cb_source_line;
	ppecho ("IDENTIFICATION DIVISION.");
}
"ID"({ZENSPC}|[ ])+"DIVISION"({ZENSPC}|[ ])*"."			{ ppecho ("ID DIVISION."); }
"FUNCTION"({ZENSPC}|[ ])+"DIVISION"({ZENSPC}|[ ])*"."		{ ppecho ("FUNCTION DIVISION."); }
"PROGRAM-ID"					{ ppecho (yytext); return PROGRAM_ID; }
"FUNCTION-ID"					{ ppecho (yytext); return FUNCTION_ID; }
"ENVIRONMENT"({ZENSPC}|[ ])+"DIVISION"	{ ppecho ("ENVIRONMENT DIVISION"); return ENVIRONMENT_DIVISION; }
"DATA"({ZENSPC}|[ ])+"DIVISION"		{
	position_in_source_code = POSITION_AFTER_WORKING_STORAGE;
	ppecho ("DATA DIVISION");
	return DATA_DIVISION;
}
"PROCEDURE"({ZENSPC}|[ ])+"DIVISION"	{
	if(copy_stack->next == NULL) {
		procedure_division_line_number = cb_source_line;
		position_in_source_code = POSITION_AFTER_PROCEDURE_DIVISION;
	}
	ppecho ("PROCEDURE DIVISION");
	return PROCEDURE_DIVISION;
}
"END"({ZENSPC}|[ ])+"PROGRAM"		{ ppecho ("END PROGRAM"); return END_PROGRAM; }
"END"({ZENSPC}|[ ])+"FUNCTION"		{ ppecho ("END FUNCTION"); return END_FUNCTION; }

<*>^"*".* |
<*>^"/".* {
	ppecho (" ");
	if (cb_source_format != CB_FORMAT_FIXED) {
		ppecho (yytext);
	}
}

"PROCESS"		{ BEGIN PROCESS_STATE; }

<PROCESS_STATE>{
  \n			{ BEGIN INITIAL; unput ('\n'); }
  .*			{ cb_warning (_("PROCESS statement is ignored")); }
}

"COPY"			{ BEGIN COPY_STATE; return COPY; }
"INCLUDE"		{ BEGIN COPY_STATE; return COPY; }
"REPLACE"		{ BEGIN COPY_STATE; return REPLACE; }

<COPY_STATE>{
  [,;]?\n		{ ECHO; cb_source_line++; }
  [,;]?({ZENSPC}|[ ])+	{ /* ignore */ }
  "."			{ BEGIN INITIAL; return '.'; }
  "=="			{ BEGIN PSEUDO_STATE; return EQEQ; }
  "("			{ return '('; }
  ")"			{ return ')'; }
  "BY"			{ return BY; }
  "IN"			{ return IN; }
  "OF"			{ return OF; }
  "OFF"			{ return OFF; }
  "SUPPRESS"		{ return SUPPRESS; }
  "PRINTING"		{ return PRINTING; }
  "REPLACING"		{ return REPLACING; }
  "LEADING"		{ return LEADING; }
  "TRAILING"		{ return TRAILING; }
  "JOINING"		{ return JOINING; }
  "AS"			{ return AS; }
  "PREFIX"		{ return PREFIX; }
  "SUFFIX"		{ return SUFFIX; }
  "PREFIXING"		{ return PREFIXING; }
  "SUFFIXING"		{ return SUFFIXING; }
  {WORD}		{
#ifdef	I18N_UTF8
			  convert_ucs_hyphen_minus (yytext);
#endif /*I18N_UTF8*/
			  pplval.s = strdup (yytext); return TOKEN; }
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .			{ pplval.s = strdup (yytext); return TOKEN; }
}

<PSEUDO_STATE>{
  [,;]?\n		{ ECHO; cb_source_line++; }
  [,;]?({ZENSPC}|[ ])+	{ pplval.s = strdup (" "); return TOKEN; }
  "=="			{ BEGIN COPY_STATE; return EQEQ; }
  {WORD}		{
#ifdef	I18N_UTF8
			  convert_ucs_hyphen_minus (yytext);
#endif /*I18N_UTF8*/
			  pplval.s = strdup (yytext); return TOKEN; }
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .			{ pplval.s = strdup (yytext); return TOKEN; }
}

<DATANAME_JOIN_STATE>{
  "COPY"		{ suppress_echo = 0; BEGIN COPY_STATE; return COPY; }
  "INCLUDE"		{ suppress_echo = 0; BEGIN COPY_STATE; return COPY; }
  "REPLACE"		{ suppress_echo = 0; BEGIN COPY_STATE; return REPLACE; }
  [,;]?\n		{ ECHO; cb_source_line++; }
  [,;]?({ZENSPC}|[ ])+	{ ppecho (" "); }
  {WORD}		{
	BEGIN INITIAL;
	if (!strcasecmp (yytext, "FILLER")) {
		ppecho (yytext);
	} else {
		ppecho_dataname (yytext);
	}
	suppress_echo = 0;
	if (cb_cobol68_copy_in_data_description) {
		pplval.s = strdup (yytext);
		return TOKEN;
	}
  }
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .			{ suppress_echo = 0; BEGIN INITIAL; ppecho (yytext); }
}

"REDEFINES" {
	suppress_echo = (omit_data_redef_name) ? 1 : 0;
	ppecho (yytext);
	BEGIN DATANAME_JOIN_STATE;
	if (cb_cobol68_copy_in_data_description) {
		return REDEFINES;
	}
}

"AUTHOR" |
"DATE-WRITTEN" |
"DATE-MODIFIED" |
"DATE-COMPILED" |
"INSTALLATION" |
"REMARKS" |
"SECURITY" {
	/* these words are treated as comments */
	if (cb_verify (cb_author_paragraph, yytext)) {
		/* skip comments until the end of line */
		int c;

		within_comment = 1;
		while ((c = input ()) != EOF) {
			if (c == '\n') {
				break;
			}
		}
		unput (c);
	}
}

"EJECT"\.? |
"SKIP1"\.? |
"SKIP2"\.? |
"SKIP3"\.? {
	/* these words are comments in IBM COBOL */
	if (cb_verify (cb_eject_statement, yytext)) {
		/* do nothing for now */
	} else {
		/* ECHO; */ /* comment should be suppressed, shouldn't it? */
	}
}

[,;]?\n			{ ppecho ("\n"); cb_source_line++; }

[;]?({ZENSPC}|[ ])+	{ ppecho (" "); }

[,]?({ZENSPC}|[ ])+ {
	if (inside_bracket) {
		ppecho (", ");
	} else {
		ppecho (" ");
	}
}

"(" {
	inside_bracket++;
	ppecho ("(");
}

")" {
	if (inside_bracket) {
		inside_bracket--;
	}
	ppecho (")");
}

"VALUE"({ZENSPC}|[ \n])+[0-9]+		|
"VALUES"(({ZENSPC}|[ \n])+[0-9]+)+	|
"IS"({ZENSPC}|[ \n])+[0-9]+		|
"ARE"({ZENSPC}|[ \n])+[0-9]+		|
"ALL"({ZENSPC}|[ \n])+[0-9]+		|
"THROUGH"({ZENSPC}|[ \n])+[0-9]+	|
"THRU"({ZENSPC}|[ \n])+[0-9]+		|
"FALSE"({ZENSPC}|[ \n])+[0-9]+		|
"CONSTANT"({ZENSPC}|[ \n])+[0-9]+	|
"CONSTANT"({ZENSPC}|[ \n])+"GLOBAL"({ZENSPC}|[ \n])+[0-9]+			|
"CONSTANT"({ZENSPC}|[ \n])+"IS"({ZENSPC}|[ \n])+"GLOBAL"({ZENSPC}|[ \n])+[0-9]+	|
"AS"({ZENSPC}|[ \n])+[0-9]+		|
"PLUS"({ZENSPC}|[ \n])+[0-9]+		|
"FROM"({ZENSPC}|[ \n])+[0-9]+		|
"TO"({ZENSPC}|[ \n])+[0-9]+ {
	/* each numeric is not a level-number */
	char *p, *pcrnt;
	cobc_mbspc2ascii (yytext);
	pcrnt = yytext;
	while (*pcrnt) {
		for (p = pcrnt; *p && *p != ' ' && *p != '\n' ; p++)
			;
		for ( ; *p == ' ' || *p == '\n'; p++) {
			if (*p == '\n') {
				cb_source_line++;
			}
			*p = '\0';
		}
		ppecho (pcrnt);
		if (*p) {
			ppecho (" ");
		}
		pcrnt = p;
	}
}

^({ZENSPC}|[ ])*[0-9]+ {
	char *p = yytext;
	cobc_mbspc2ascii (yytext);
	while (*p == ' ') {
		p++;
	}
	suppress_echo = (omit_data_entry_name) ? 1 : 0;
	ppecho (p);
	if (current_division == PP_DATA_DIVISION) {
		BEGIN DATANAME_JOIN_STATE;
		if (cb_cobol68_copy_in_data_description) {
			return LEVEL_NUMBER;
		}
	}
}

"." {
	ppecho (yytext);
	return yytext[0];
}

{WORD}			{
#ifdef	I18N_UTF8
			  convert_ucs_hyphen_minus (yytext);
#endif /*I18N_UTF8*/
			  ppecho (yytext); }
{NUMRIC_LITERAL} |
{ALNUM_LITERAL} |
.			{ ppecho (yytext); }

<<EOF>> {
	struct copy_info *p;

	p = copy_stack;

	yy_delete_buffer (YY_CURRENT_BUFFER);

	/* Terminate at the end of all input */
	if (p->next == NULL) {
		within_comment = 0;
		newline_count = 0;
		inside_bracket = 0;
		current_replace_list = NULL;
		current_copy_replace_list = NULL;
		current_joining_ext = NULL;
		text_queue1 = text_queue2 = NULL;
		copy_stack = NULL;
		quotation_mark = 0;
		consecutive_quotation = 0;
		last_line_1 = -1;
		last_line_2 = -1;
		yyterminate ();
	}

	/* Close the current file */
	fclose (ppin);

	/* Switch to the last buffer */
	if (p->replacing) {
		pp_set_copy_replace_list (NULL);
	}
	if (p->joining) {
		pp_set_joining_ext (NULL);
	}
	switch_to_buffer (p->line, p->file, p->buffer);
	quotation_mark = p->quotation_mark;

	copy_stack = p->next;
	free (p);
}

%%

void
pp_set_replace_list (struct cb_replace_list *list)
{
	current_replace_list = list;
}

void
pp_set_copy_replace_list (struct cb_replace_list *list)
{
	current_copy_replace_list = list;
}

void
pp_set_joining_ext (struct cb_joining_ext *ext)
{
	current_joining_ext = ext;
}

#ifdef	I18N_UTF8
# define UTF8_FULLWIDTH_HYPHEN_MINUS	"\xEF\xBC\x8D"
# define UTF8_MINUS_SIGN		"\xE2\x88\x92"
#else /*!I18N_UTF8*/
# define SJIS_MINUS_SIGN		"\x81\x7c"
#endif /*I18N_UTF8*/

#ifdef	I18N_UTF8
static void
convert_ucs_hyphen_minus (char *namebuf)
{
	char *p = namebuf;
	while (NULL != (p = strstr (p, UTF8_MINUS_SIGN))) {
		*p = '_';
        memmove(p+1, p+3, strlen(p+3)+1);
        p++;
	}

	p = namebuf;
	while (NULL != (p = strstr (p, UTF8_FULLWIDTH_HYPHEN_MINUS))) {
		*p = '_';
        memmove(p+1, p+3, strlen(p+3)+1);
        p++;
	}
	return;
}
#endif /*I18N_UTF8*/

static void
ppecho_dataname (char *namebuf)
{
	int extlen, namelen, seplen;
	const char *ext;
	const char *sep;

#ifdef	I18N_UTF8
	convert_ucs_hyphen_minus (namebuf);
#endif /*I18N_UTF8*/
	if (!current_joining_ext) {
		ppecho (namebuf);
	} else {
		ext = current_joining_ext->ext;
		if (!plexbuff1) {
			plexbuff1 = cobc_malloc (COB_SMALL_BUFF);
		}
		extlen = strlen (ext);
		namelen = strlen (namebuf);
#ifdef	I18N_UTF8
		if (!utf8_ext_pick ((unsigned char*) ext)) {
			sep = "-";
			seplen = 1;
		} else {
			sep = UTF8_FULLWIDTH_HYPHEN_MINUS;
			seplen = 3;
		}
#else /*!I18N_UTF8*/
		if (!sjis_pick ((unsigned char*) ext)) {
			sep = "-";
			seplen = 1;
		} else {
			sep = SJIS_MINUS_SIGN;
			seplen = 2;
		}
#endif /*I18N_UTF8*/
		if (current_joining_ext->type == joining_as_prefix) {
			memcpy (plexbuff1, ext, extlen);
			memcpy (&(plexbuff1[extlen]), sep, seplen);
			memcpy (&(plexbuff1[extlen+seplen]), &(namebuf[0]), namelen+1);
		} else if (current_joining_ext->type == joining_as_suffix) {
			memcpy (plexbuff1, &(namebuf[0]), namelen);
			memcpy (&(plexbuff1[namelen]), sep, seplen);
			memcpy (&(plexbuff1[namelen+seplen]), ext, extlen+1);
		} else if (current_joining_ext->type == prefixing) {
			memcpy (plexbuff1, ext, extlen);
			memcpy (&(plexbuff1[extlen]), &(namebuf[0]), namelen+1);
		} else if (current_joining_ext->type == suffixing) {
			memcpy (plexbuff1, &(namebuf[0]), namelen);
			memcpy (&(plexbuff1[namelen]), ext, extlen+1);
		}
		ppecho (plexbuff1);
	}
}

static void
switch_to_buffer (const int line, const char *file, YY_BUFFER_STATE buffer)
{
	char	*p;

	cb_source_line = line;
	cb_source_file = strdup (file);
	for (p = cb_source_file; *p; p++) {
		if (*p == '\\') {
			*p = '/';
		}
	}
	yy_switch_to_buffer (buffer);
	fprintf (yyout, "# %d \"%s\"\n", line, cb_source_file);
}

int
ppopen (const char *name, struct cb_joining_ext *joining_ext, struct cb_replace_list *replace_list)
{
	struct copy_info *p;

	for (; newline_count > 0; newline_count--) {
		ungetc ('\n', ppin);
	}

	/* Open the copy file */
	ppin = fopen (name, "rb");
	if (!ppin) {
		if (cb_source_file) {
			cb_error (_("%s: %s"), name, strerror (errno));
		} else {
			perror (name);
		}
		return -1;
	}

	/* Add to dependency list */
	if (cb_depend_file) {
		cb_depend_list = cb_text_list_add (cb_depend_list, name);
	}

	/* Preserve the current buffer */
	p = cobc_malloc (sizeof (struct copy_info));
	p->line = cb_source_line;
	p->file = cb_source_file;
	p->replacing = replace_list ? 1 : 0;
	p->joining = joining_ext ? 1 : 0;
	p->buffer = YY_CURRENT_BUFFER;
	p->quotation_mark = quotation_mark;
	p->next = copy_stack;
	copy_stack = p;

	/* Switch to new buffer */
	if (replace_list) {
		pp_set_copy_replace_list (replace_list);
	}
	if (joining_ext) {
		pp_set_joining_ext (joining_ext);
	}
	switch_to_buffer (1, name, yy_create_buffer (ppin, YY_BUF_SIZE));
	return 0;
}

int
ppcopy (const char *name, const char *lib, struct cb_joining_ext *joining_ext, struct cb_replace_list *replace_list)
{
	struct cb_text_list	*il;
	struct cb_text_list	*el;
	char			*s;
	struct stat		st;

	if (lib) {
		if (!plexbuff1) {
			plexbuff1 = cobc_malloc (COB_SMALL_BUFF);
		}
		snprintf (plexbuff1, COB_SMALL_MAX, "%s/%s", lib, name);
		s = plexbuff1;
	} else {
		s = (char *)name;
	}

	/* Find the file */
	if (stat (s, &st) == 0) {
		return ppopen (s, joining_ext, replace_list);
	}
	if (!plexbuff2) {
		plexbuff2 = cobc_malloc (COB_SMALL_BUFF);
	}
	for (el = cb_extension_list; el; el = el->next) {
		snprintf (plexbuff2, COB_SMALL_MAX, "%s%s", s, el->text);
		if (stat (plexbuff2, &st) == 0) {
			return ppopen (plexbuff2, joining_ext, replace_list);
		}
	}
	if (*s != '/') {
		for (il = cb_include_list; il; il = il->next) {
			for (el = cb_extension_list; el; el = el->next) {
				snprintf (plexbuff2, COB_SMALL_MAX, "%s/%s%s",
					  il->text, name, el->text);
				if (stat (plexbuff2, &st) == 0) {
					return ppopen (plexbuff2, joining_ext, replace_list);
				}
			}
		}
	}
	cb_error (_("%s: %s"), name, strerror (errno));
	return -1;
}

/* Check directives */
/* This is horrible but we have to parse directives directly after the read */
/* as flex buffers up input and it is then too late to use the flex parser */

static void
check_directive (char *buff, int *line_size)
{
	char	*s;
	char	*dirptr;
	size_t	cnt;
	int	n;
	char	sbuff[5][256];

	if (cb_source_format == CB_FORMAT_FIXED) {
		if (*line_size < 8) {
			return;
		}
		if (buff[6] != ' ') {
			return;
		}
		s = &buff[7];
	} else {
		s = buff;
	}
	for (; *s == ' '; s++) {
		;
	}
	dirptr = s;
	if (*s != '>') {
		return;
	}
	s++;
	if (*s != '>') {
		return;
	}
	s++;
	if (*s == 'D') {
		if (cb_flag_debugging_line) {
			memset (dirptr, ' ', 3);
		} else {
			for (cnt = 0; cnt < newline_count; cnt++) {
				buff[cnt] = '\n';
			}
			buff[cnt] = 0;
			strcat (buff, "      *> DEBUG\n");
			*line_size = strlen (buff);
			newline_count = 0;
		}
		return;
	}
	memset (sbuff[0], 0, sizeof (sbuff));
	n = sscanf (s, "%255s %255s %255s %255s %255s",
			sbuff[0], sbuff[1], sbuff[2], sbuff[3], sbuff[4]);
	for (cnt = 0; cnt < newline_count; cnt++) {
		buff[cnt] = '\n';
	}
	buff[cnt] = 0;
	newline_count = 0;
	strcat (buff, "      *> DIRECTIVE\n");
	*line_size = strlen (buff);
	if (n < 2 || strcasecmp (sbuff[0], "SOURCE")) {
		cb_warning (_("Invalid directive - ignored"));
		return;
	}
	switch (n) {
	case 2:
		if (!strcasecmp (sbuff[1], "FIXED")) {
			cb_source_format = CB_FORMAT_FIXED;
			return;
		}
		if (!strcasecmp (sbuff[1], "FREE")) {
			cb_source_format = CB_FORMAT_FREE;
			return;
		}
		if (!strcasecmp (sbuff[1], "FREE_1COL_ASTER")) {
			cb_source_format = CB_FORMAT_FREE_1COL_ASTER;
			cb_source_format1 = 1;
			return;
		}
		break;
	case 3:
		if (strcasecmp (sbuff[1], "FORMAT") &&
		    strcasecmp (sbuff[1], "IS")) {
			break;
		}
		if (!strcasecmp (sbuff[2], "FIXED")) {
			cb_source_format = CB_FORMAT_FIXED;
			return;
		}
		if (!strcasecmp (sbuff[2], "FREE")) {
			cb_source_format = CB_FORMAT_FREE;
			return;
		}
		if (!strcasecmp (sbuff[2], "FREE_1COL_ASTER")) {
			cb_source_format = CB_FORMAT_FREE_1COL_ASTER;
			cb_source_format1 = 1;
			return;
		}
		if (!strcasecmp (sbuff[2], "VARIABLE")){
			cb_source_format = CB_FORMAT_VARIABLE;
		} 
		break;
	default:
		if (strcasecmp (sbuff[1], "FORMAT")) {
			break;
		}
		if (strcasecmp (sbuff[2], "IS")) {
			break;
		}
		if (!strcasecmp (sbuff[3], "FIXED")) {
			cb_source_format = CB_FORMAT_FIXED;
			return;
		}
		if (!strcasecmp (sbuff[3], "FREE")) {
			cb_source_format = CB_FORMAT_FREE;
			return;
		}
		if (!strcasecmp (sbuff[3], "FREE_1COL_ASTER")) {
			cb_source_format = CB_FORMAT_FREE_1COL_ASTER;
			cb_source_format1 = 1;
			return;
		}
		if (!strcasecmp (sbuff[3], "VARIABLE")){
			cb_source_format = CB_FORMAT_VARIABLE;
		} 
		break;
	}
	cb_warning (_("Invalid directive - ignored"));
}

static void
check_dollar_directive (char *buff, int *line_size)
{
	struct cb_constant_list	*l;
	char			*s;
	size_t			cnt;
	int			n;
	char			sbuff[5][256];
	int			isDEFINED, isNOT;
	int			i;
	COB_UNUSED(n);

	if (cb_source_format == CB_FORMAT_FIXED) {
		if (*line_size < 8) {
			return;
		}
		if (buff[6] != '$') {
			return;
		}
		if (*line_size > cb_text_column + 1) {
			strcpy (buff + cb_text_column, "\n");
		}
		s = &buff[6];
	} else {
		if (buff[1] != '$') {
			return;
		}
		s = buff;
	}

	memset (sbuff[0], 0, sizeof (sbuff));
	n = sscanf (s, "%255s %255s %255s %255s %255s",
		sbuff[0], sbuff[1], sbuff[2], sbuff[3], sbuff[4]);
	for (cnt = 0; cnt < newline_count; cnt++) {
		buff[cnt] = '\n';
	}
	buff[cnt] = 0;
	newline_count = 0;
	strcat (buff, "      *> DIRECTIVE\n");
	*line_size = strlen (buff);

	if (strcasecmp (sbuff[0], "$IF") == 0) {
		compile_directive_depth++;
		if (compile_directive_depth >= MAX_DEPTH) {
			compile_directive_depth = -1;
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("$IF is nested more than 10 times"));
			return;
		}
		if (compile_directive_depth < 0) {
			compile_directive_depth = -1;
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("Fatal error in $IF statement"));
			return;
		}
		if (strlen (sbuff[1]) <= 0) {
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("Arguments not enough to $IF statemen"));
			return;
		}

		isDEFINED = 0;
		isNOT = 0;
		cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_FALSE;
		if (strcasecmp (sbuff[2], "NOT") == 0) {
			isNOT = 1;
			cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_TRUE;
		}
		if (strcasecmp (sbuff[2+isNOT], "DEFINED") == 0) {
			isDEFINED = 1;
		} else if (strcasecmp (sbuff[2+isNOT], "=") != 0 ||
				strlen (sbuff[3+isNOT]) <= 0) {
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("Invalid argument $IF statemen"));
			return;
		}
		l = cb_const_list;
		while (l) {
			if (strcasecmp (l->name, sbuff[1]) == 0) {
				if (isDEFINED) {
					if (isNOT) cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_FALSE;
					else cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_TRUE;
					break;
				} else {
					switch (l->type) {
					case CB_CONSTANT_TYPE_ALPANUM:
						if (strlen (sbuff[3+isNOT]) < 3 ||
								sbuff[3+isNOT] != strchr (sbuff[3+isNOT], '\"') ||
								sbuff[3+isNOT] == strrchr (sbuff[3+isNOT], '\"') ||
								strlen (strchr (sbuff[3+isNOT]+1, '\"')) > 1) {
							cb_compile_status = CB_COMPILE_STATUS_ERROR;
							cb_error (_("%s is not a string"), sbuff[3+isNOT]);
							return;
						}
						strcpy (strchr (sbuff[3+isNOT]+1, '\"'), "");
						if (strcasecmp (sbuff[3+isNOT]+1, l->alphavalue) == 0) {
							if (isNOT) cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_FALSE;
							else cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_TRUE;
						}
						break;
					case CB_CONSTANT_TYPE_NUMERIC:
						//TODO
						break;
					default:
						break;
					}
					break;
				}
			}
			l = l->next;
		}
	} else if (strcasecmp (sbuff[0], "$ELSE") == 0) {
		if (compile_directive_depth >= MAX_DEPTH) {
			compile_directive_depth = -1;
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("Fatal error in $ELSE statement"));
			return;
		}
		if (compile_directive_depth < 0) {
			compile_directive_depth = -1;
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("$IF has no defined before the $ELSE"));
			return;
		}
		if (cb_compile_status_list[compile_directive_depth] == CB_COMPILE_STATUS_TRUE_ELSE ||
				cb_compile_status_list[compile_directive_depth] == CB_COMPILE_STATUS_FALSE_ELSE) {
			compile_directive_depth = -1;
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("$ELSE has continued"));
			return;
		}
		if (cb_compile_status_list[compile_directive_depth] == CB_COMPILE_STATUS_FALSE)
			cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_TRUE_ELSE;
		else cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_FALSE_ELSE;
	} else if (strcasecmp (sbuff[0], "$END") == 0) {
		if (compile_directive_depth >= MAX_DEPTH) {
			compile_directive_depth = -1;
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("Fatal error in $END statement"));
			return;
		}
		if (compile_directive_depth < 0) {
			compile_directive_depth = -1;
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("$IF has no defined before the $END"));
			return;
		}
		cb_compile_status_list[compile_directive_depth] = CB_COMPILE_STATUS_NONE;
		compile_directive_depth--;
	} else if (strcasecmp (sbuff[0], "$SET") == 0) {
		if (strcasecmp (sbuff[1], "SOURCEFORMAT(FREE)") == 0) {
			cb_source_format = CB_FORMAT_FREE;
			return;
		} else if (strcasecmp (sbuff[1], "SOURCEFORMAT(FIXED)") == 0) {
			cb_source_format = CB_FORMAT_FIXED;
			return;
		} else if (strcasecmp (sbuff[1], "SOURCEFORMAT(FREE_1COL_ASTER)") == 0) {
			cb_source_format = CB_FORMAT_FREE_1COL_ASTER;
			cb_source_format1 = 1;
			return;
		} else if (strcasecmp (sbuff[1], "SOURCEFORMAT(VARIABLE)") == 0){
			cb_source_format = CB_FORMAT_VARIABLE;
		} else {
			cb_compile_status = CB_COMPILE_STATUS_ERROR;
			cb_error (_("Invalid $SET"));
			return;
		}
	} else {
		cb_compile_status = CB_COMPILE_STATUS_ERROR;
		cb_error (_("Invalid $ statements"));
		return;
	}

	if (compile_directive_depth > -1) {
		for (i = 0; i <= compile_directive_depth; i++) {
			if (cb_compile_status_list[i] != CB_COMPILE_STATUS_TRUE &&
					cb_compile_status_list[i] != CB_COMPILE_STATUS_TRUE_ELSE) {
				cb_compile_status = CB_COMPILE_STATUS_FALSE;
				return;
			}
		}
	}
	if (*buff == '\n') {
		cb_compile_status = CB_COMPILE_STATUS_FALSE_END;
	} else {
		cb_compile_status = CB_COMPILE_STATUS_TRUE;
	}
}

/*
 * Read line
 */

static int
ppinput (char *buff, int max_size)
{

	char	*bp;
	size_t	gotcr;
	size_t	continuation = 0;
	int	ipchar;
	int	i;
	int	n;
	int	coln;
	int buff_len = 0;
	char	*str1 = NULL;
	char	*str2 = NULL;
	int comment_counter = 0;

start:
	/* read a line */
	gotcr = 0;
	ipchar = 0;
	for (n = 0; n < 256 && ipchar != '\n';) {
		ipchar = getc (ppin);
		if (ipchar == EOF) {
			if (n > 0) {
				break;
			}
			if (newline_count == 0) {
				return 0;
			}
			memset (buff, '\n', newline_count);
			buff[newline_count] = 0;
			newline_count = 0;
			return strlen (buff);
		}
		if (n == 0 && cb_source_format != CB_FORMAT_FIXED && cb_source_format1 != 1 
			&& cb_source_format != CB_FORMAT_VARIABLE) {
			if (ipchar != ' ' && ipchar != '\n') {
				buff[n++] = ' ';
			}
		}
		if (gotcr) {
			if (ipchar != '\n') {
				buff[n++] = '\r';
			}
			gotcr = 0;
		}
		if (ipchar == '\r') {
			gotcr = 1;
		} else if (ipchar == '\t') {
			buff[n++] = ' ';
			while (n % cb_tab_width != 0) {
				buff[n++] = ' ';
			}
		} else {
			buff[n++] = ipchar;
		}
	}

	if (buff[n - 1] != '\n') {
		cb_warning (_("Line not terminated by a newline"));
		buff[n++] = '\n';
	}
	buff[n] = 0;
	if (cb_source_format1 == 1 && buff[0] == '*' && buff[1] != '>') {
		strcpy (buff, "\n");
		return strlen (buff);
	}
	str1 = strstr (buff, "*>");
	if (str1 != NULL) {
		if ((str1-buff) >= 7) {
			str2 = strstr (str1, "\"");
			if (str2 == NULL) {
				buff[str1-buff] = '\n';
				buff[str1-buff +1] = '\0';
				n = strlen (buff);
			}
		}
	}
	check_directive (buff, &n);
	check_dollar_directive (buff, &n);
	if (cb_compile_status ==  CB_COMPILE_STATUS_ERROR) {
		return YY_NULL;
	}
	if (cb_compile_status == CB_COMPILE_STATUS_FALSE) {
		newline_count++;
		comment_counter++;
		goto start;
	}
	if (cb_compile_status == CB_COMPILE_STATUS_FALSE_END) {
		cb_compile_status = CB_COMPILE_STATUS_NONE;
		newline_count++;
		comment_counter++;
		goto start;
	}

	/* nothing more to do with free format */
	if (cb_source_format != CB_FORMAT_FIXED && cb_source_format != CB_FORMAT_VARIABLE) {
		return n;
	}

	/* line too short */
	if (n < 8) {
		newline_count++;
		comment_counter++;
		goto start;
	}

	if (cb_flag_mfcomment) {
		if (buff[0] == '*' || buff[0] == '/') {
			/*if(!cb_flag_no_cobol_comment) {
				register_comment(0, buff, comment_counter++);
			}*/
			newline_count++;
			goto start;
		}
	}
	/* check the indicator (column 7) */
	bp = buff + 7;
	switch (buff[6]) {
	case ' ':
		break;
	case '-':
		continuation = 1;
		break;
	case 'd':
	case 'D':
		/* debugging line */
		if (cb_flag_debugging_line) {
			break;
		}
		newline_count++;
		comment_counter++;
		goto start;
	case '*':
	case '/':
		/* comment line */
		/*if(!cb_flag_no_cobol_comment) {
			register_comment(6, buff, comment_counter++);
		}*/
		newline_count++;
		goto start;
	default:
		/* invalid indicator */
		cb_error (_("Invalid indicator '%c' at column 7"), buff[6]);
		return YY_NULL;
	}

	/* skip comments that follow after AUTHORS, etc. */
	if (within_comment) {
		/* Check all of "Area A" */
		switch (n) {
		case 8:
			if (buff[7] == ' ') {
				newline_count++;
				comment_counter++;
				goto start;
			}
			break;
		case 9:
			if (!memcmp (&buff[7], "  ", 2)) {
				newline_count++;
				comment_counter++;
				goto start;
			}
			break;
		case 10:
			if (!memcmp (&buff[7], "   ", 3)) {
				newline_count++;
				comment_counter++;
				goto start;
			}
			break;
		default:
			if (!memcmp (&buff[7], "    ", 4)) {
				newline_count++;
				comment_counter++;
				goto start;
			}
			break;
		}
		within_comment = 0;
	}

#ifdef I18N_UTF8
	unsigned char *p = (unsigned char *)buff;
	if(utf8_ext_pick(p)){
		buff_len = (int) utf8_calc_sjis_size(p, strlen(buff));
	}else{
		buff_len = n;
	}
#else /*!I18N_UTF8*/
	buff_len = n;
#endif /*I18N_UTF8*/

	/* check the text that is longer than cb_text_column */
	if (buff_len > cb_text_column + 1 && cb_source_format != CB_FORMAT_VARIABLE) {

		/* show warning if it is not whitespaces */
		if (cb_warn_column_overflow && last_line_2 < cb_source_line - 1) {
			for (coln = cb_text_column; coln < n; coln++) {
				if (buff[coln] != ' ' && buff[coln] != '\n') {
					cb_warning (_("Source text after column %d"),
						    cb_text_column);
					break;
				}
			}
		}

		/* remove it */
		strcpy (buff + cb_text_column, "\n");
		last_line_2 = cb_source_line;
		n = cb_text_column + 1;
	}

	/* skip blank lines */
	for (i = 7; buff[i] == ' '; i++) ;
	if (buff[i] == '\n') {
		newline_count++;
		comment_counter++;
		goto start;
	}

	if (continuation) {
		/* line continuation */
		for (; *bp == ' '; bp++) ;

		/* validate concatenation */
		if (consecutive_quotation) {
			if (bp[0] == quotation_mark && bp[1] == quotation_mark) {
				bp++;
			} else {
				cb_error (_("Invalid line continuation"));
				return YY_NULL;
			}
			quotation_mark = 0;
			consecutive_quotation = 0;
		} else if (quotation_mark) {
			/* literal concatenation */
			if (*bp == quotation_mark) {
				bp++;
			} else {
				cb_error (_("Invalid line continuation"));
				return YY_NULL;
			}
		}
	} else {
		/* normal line */
		quotation_mark = 0;
		consecutive_quotation = 0;
	}

	/* check if string literal is to be continued */
	for (i = bp - buff; buff[i] != '\n'; i++) {
		if (buff[i] == '\'' || buff[i] == '\"') {
			if (quotation_mark == 0) {
				/* literal start */
				quotation_mark = buff[i];
			} else if (quotation_mark == buff[i]) {
				if (i == cb_text_column - 1) {
					/* consecutive quotation */
					consecutive_quotation = 1;
				} else {
					/* literal end */
					quotation_mark = 0;
				}
			}
		}
	}

	/* truncate trailing spaces, including the newline */
	if (quotation_mark) {
		for (; i < 72;) {
			buff[i++] = ' ';
		}
		buff[i] = 0;
	} else {
		for (i--; buff[i] == ' '; i--) ;
		if (buff[i] == '\'' || buff[i] == '\"') {
			buff[++i] = ' ';
		}
		buff[i + 1] = 0;
	}

	if (continuation) {
		memmove (buff, bp, strlen (bp) + 1);
		newline_count++;
		comment_counter++;
	} else {
		/* insert newlines at the start of the buffer */
		memmove (buff + newline_count, bp, strlen (bp) + 1);
		memset (buff, '\n', newline_count);
		newline_count = 1;
	}

	return strlen (buff);
}

static void
ppecho_0 (const char			*text,
	  struct cb_text_list		**pqueue,
	  struct cb_replace_list	**preplace,
	  void (ppecho_proc)(const char *)) {

	struct cb_replace_list	*r;
	struct cb_text_list	*l;
	struct cb_text_list	*queue;

	if (suppress_echo) {
		/* generate no output */
	} else if (*pqueue == NULL && (text[0] == ' ' || text[0] == '\n')) {
		ppecho_proc (text);
	} else if (!*preplace) {
		for (; *pqueue; *pqueue = (*pqueue)->next) {
			ppecho_proc ((*pqueue)->text);
		}
		ppecho_proc (text);
	} else {
		/* Do replacement */

		*pqueue = cb_text_list_add (*pqueue, text);

		while (*pqueue) {
			for (r = *preplace; r; r = r->next) {
				queue = *pqueue;
				for (l = r->old_text; l; l = l->next) {
					while (l && (l->text[0] == ' ' || l->text[0] == '\n')) {
						l = l->next;
					}
					if (l == NULL) {
						break;
					}
					while (queue && (queue->text[0] == ' ' ||
							 queue->text[0] == '\n')) {
						queue = queue->next;
					}
					if (queue == NULL) {
						return;	/* partial match */
					}
					if (r->replace_type == CB_REPLACE_LEADING) {
						break;
					} else if (r->replace_type == CB_REPLACE_TRAILING) {
						break;
					} else if (r->replace_type == CB_REPLACE_OTHER) {
						if (strcasecmp (l->text, queue->text) != 0) {
							break;
						}
					}
					queue = queue->next;
				}
				if (r->replace_type == CB_REPLACE_LEADING) {
					if (!l || !queue) {
						continue;
					}
					if (strncasecmp (l->text, queue->text, strlen (l->text)) == 0) {
						break;
					}
				} else if (r->replace_type == CB_REPLACE_TRAILING) {
					if (!l || !queue || strlen (queue->text) < strlen (r->old_text->text)) {
						continue;
					}
					if (strcasecmp (queue->text + strlen (queue->text) - strlen (r->old_text->text), r->old_text->text) == 0) {
						break;
					}
				} else if (l == NULL) {
					/* match */
					break;
				}
			}

			/* match */
			if (r && r->replace_type == CB_REPLACE_LEADING) {
				int oldlen = strlen (l->text);
				for (l = *pqueue; l != queue; l = l->next) {
					ppecho_proc (l->text);
				}
				l = r->new_text;
				while (l && (l->text[0] == ' ' || l->text[0] == '\n')) {
					l = l->next;
				}
				if (l) {
					ppecho_proc (l->text);
				}
				ppecho_proc (queue->text + oldlen);
				queue = queue->next;
			} else if (r && r->replace_type == CB_REPLACE_TRAILING) {
				int i;
				int oldlen = strlen (l->text);
				for (l = *pqueue; l != queue; l = l->next) {
					ppecho_proc (l->text);
				}
				for (i = 0; i < strlen (queue->text) - oldlen; i++) {
					fputc (queue->text[i], ppout);
				}
				l = r->new_text;
				while (l && (l->text[0] == ' ' || l->text[0] == '\n')) {
					l = l->next;
				}
				if (l) {
					ppecho_proc (l->text);
				}
				queue = queue->next;
			} else if (r && l == NULL) {
				for (l = r->new_text; l; l = l->next) {
					ppecho_proc (l->text);
				}
			} else {
				/* no match */
				if (!*pqueue) {
					break;
				}
				ppecho_proc ((*pqueue)->text);
				queue = (*pqueue)->next;
			}

			while (*pqueue != queue) {
				if (!*pqueue) break;

				l = (*pqueue)->next;
				free (*pqueue);
				*pqueue = l;
			}
		}
	}
}

static void
ppecho_final (const char *text)
{
	fputs (text, ppout);
}

static void
ppecho_2 (const char *text)
{
	/* process REPLACE */
	ppecho_0 (text, &text_queue2, &current_replace_list, ppecho_final);
}

static void
ppecho_1 (const char *text)
{
	/* process COPY REPLACING */
	ppecho_0 (text, &text_queue1, &current_copy_replace_list, ppecho_2);
}

static void
ppecho (const char *text)
{
	ppecho_1 (text);
}

void pp_set_current_division (int divno)
{
	current_division = divno;
}

void pp_omit_data_entry_name (int on_off)
{
	omit_data_entry_name = on_off;
}

void pp_omit_data_redef_name (int on_off)
{
	omit_data_redef_name = on_off;
}

void register_comment(int comment_mark_index, char* buffer, int delta) {
	struct comment_info* p = malloc(sizeof(struct comment_info));
	p->file = cb_source_file;
	p->line = cb_source_line + delta + 1;

	// Remove the unnecessary part of the comment
	int str_len_original_line = strlen(buffer);
	int comment_end_index;
	if(buffer[str_len_original_line - 1] == '\n') {
		if(str_len_original_line >= 2 && buffer[str_len_original_line - 1] == '\r') {
			comment_end_index = str_len_original_line - 2;
		} else {
			comment_end_index = str_len_original_line - 1;
		}
	} else {
		comment_end_index = str_len_original_line;
	}
	int comment_len = comment_end_index - comment_mark_index - 1;
	p->comment = malloc(comment_len + 1);
	memcpy(p->comment, buffer + comment_mark_index + 1, comment_len);
	p->comment[comment_len] = '\0';

	p->prev = comment_info_list_last;
	p->next = NULL;
	p->is_base_cobol_file = copy_stack->next == NULL;
	p->position_in_source_code = position_in_source_code;
	if(comment_info_list_last != NULL) {
		comment_info_list_last->next = p;
	}
	comment_info_list_last = p;
	if(comment_info_list_head == NULL) {
		comment_info_list_head = p;
	}
}