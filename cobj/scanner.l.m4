/*
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

%{
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cobj.h"
#include "tree.h"
#include "parser.h"

#define SET_LOCATION(x)				\
  (x)->source_file = (unsigned char *)cb_source_file;		\
  (x)->source_line = cb_source_line

struct cb_level_78 {
	struct cb_level_78	*next;
	struct cb_field		*fld78;
};

#define UNGET_BUFFER_SIZE 1
struct cb_unget_buffer {
	int	count;
	int	token[UNGET_BUFFER_SIZE];
	YYSTYPE	lvalue[UNGET_BUFFER_SIZE];
};

/* Local variables */
static struct cb_level_78	*lev78ptr = NULL;
static unsigned char		*plexbuff = NULL;
static size_t			plexsize;
static size_t			in_procedure;
static size_t			cb_force_pid_literal = 0;
static int			last_token_is_dot = 0;
static int			integer_is_label = 0;
static int			inside_bracket = 0;
static int			inside_repository = 0;
struct cb_unget_buffer		*unget_buffer = NULL;

static int read_literal (int mark, enum cb_category);
static int scan_x (char *text);
static int scan_h (char *text);
static int scan_numeric (char *text);
static int scan_picture (char *text);
static void count_lines (char *text);

%}

%s DECIMAL_IS_PERIOD DECIMAL_IS_COMMA
%x PICTURE_STATE FUNCTION_STATE

ifdef(M4.I18N_UTF8,>>>>>
U00_7F		[\x00-\x7F]
U80_7FF		[\xC2-\xDF][\x80-\xBF]
U800_FFF	[\xE0][\xA0-\xBF][\x80-\xBF]
U1000_CFFF	[\xE1-\xEC][\x80-\xBF][\x80-\xBF]
UD000_D7FF	[\xED][\x80-\x9F][\x80-\xBF]
UE000_FFFF	[\xEE-\xEF][\x80-\xBF][\x80-\xBF]
U10000_3FFFF	[\xF0][\x90-\xBF][\x80-\xBF][\x80-\xBF]
U30000_FFFFF	[\xF1-\xF3][\x80-\xBF][\x80-\xBF][\x80-\xBF]
U100000_10FFFF	[\xF4][\x80-\x8F][\x80-\xBF][\x80-\xBF]
UTF8_EXT	{U80_7FF}|{U800_FFF}|{U1000_CFFF}|{UD000_D7FF}|{UE000_FFFF}|{U10000_3FFFF}|{U30000_FFFFF}|{U100000_10FFFF}
UTF8		{U00_7F}|{UTF8_EXT}
<<<<<,>>>>>
JPNWORD [\xA0-\xDF]|([\x81-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC])
<<<<<)

%%
%{
	if (unget_buffer && unget_buffer->count) {
		--unget_buffer->count;
		yylval = unget_buffer->lvalue[unget_buffer->count];
		return unget_buffer->token[unget_buffer->count];
	}

	if (current_program) {
		if (current_program->decimal_point == '.') {
			BEGIN DECIMAL_IS_PERIOD;
		} else {
			BEGIN DECIMAL_IS_COMMA;
		}
	}

	/* We treat integer literals immediately after '.' as labels;
	   that is, they must be level numbers or section names. */
	integer_is_label = 0;
	if (last_token_is_dot) {
		integer_is_label = 1;
		last_token_is_dot = 0;
	}
%}


<*>\n {
	cb_source_line++;
}

^"#".* {
	/* line directive */
	char *endp;

	if (strlen (yytext) > 1 && isdigit (*(yytext + 2))) {
		cb_source_line = strtol (yytext + 2, &endp, 10) - 1;
		cb_source_file = strdup (strchr (endp, '"') + 1);
		strrchr (cb_source_file, '"')[0] = '\0';
	}
}

"PIC" |
"PICTURE" {
	BEGIN PICTURE_STATE;
}

"FUNCTION" {
	if (inside_repository) {
		return FUNCTION;
	}
	BEGIN FUNCTION_STATE;
}

"DIVISION" {
	inside_repository = 0;
	return DIVISION;
}

"PROGRAM-ID" {
	inside_repository = 0;
	cb_force_pid_literal = 1;
	return PROGRAM_ID;
}

"FUNCTION-ID" {
	inside_repository = 0;
	cb_force_pid_literal = 1;
	return FUNCTION_ID;
}

"REPOSITORY" {
	inside_repository = 1;
	return REPOSITORY;
}

[\'\"] {
	/* string literal */
	cb_force_pid_literal = 0;
	return read_literal (yytext[0], CB_CATEGORY_ALPHANUMERIC);
}

N[\'\"] {
	cb_force_pid_literal = 0;
	return read_literal (yytext[1], CB_CATEGORY_NATIONAL);
}

NC[\'\"] {
	cb_force_pid_literal = 0;
	return read_literal (yytext[2], CB_CATEGORY_NATIONAL);
}

ND[\'\"] {
	cb_force_pid_literal = 0;
	return read_literal (yytext[2], CB_CATEGORY_NATIONAL);
}

X\'[^\'\n]*\' |
X\"[^\"\n]*\" {
	/* X string literal */
	if (cb_warn_compat) {
		cb_warning (_("X Binary literal found."));
	}
	cb_force_pid_literal = 0;
	return scan_x (yytext + 2);
}

NX\'[^\'\n]*\' |
NX\"[^\"\n]*\" {
	/* NX string literal */
	if (cb_warn_compat) {
		cb_warning (_("NX Binary literal found."));
	}
	cb_force_pid_literal = 0;
	return scan_x (yytext + 3);
}

"(" {
	inside_bracket++;
	return '(';
}

")" {
	if (inside_bracket > 0) {
		inside_bracket--;
	}
	return ')';
}

[0-9]+ {
	cb_force_pid_literal = 0;
	if (integer_is_label) {
		/* integer label */
		yylval = cb_build_reference (yytext);
		SET_LOCATION (yylval);
		if (in_procedure) {
			/* integer section/paragraph name */
			return WORD;
		} else {
			/* level number in data division */
			int lev = cb_get_level (yylval);
			if (!lev) {
				/* do nothing expecting cb_get_level() had
				 * already given some error message. */
			} else if (lev == 88) {
				return LEVEL88_NUMBER_WORD;
			} else {
				return LEVEL_NUMBER_WORD;
			}
		}
	} else {
		/* numeric literal */
		return scan_numeric (yytext);
	}
}

<*>[ \t]+ {
	/* Ignore */
}

<*>;+ {
	if (inside_bracket) {
		return SEMI_COLON;
	}
	/* Ignore */
}

<DECIMAL_IS_PERIOD>[+-]?[0-9.]*[0-9]+ {
	/* numeric literal */
	return scan_numeric (yytext);
}

<DECIMAL_IS_PERIOD>,+ {
	if (inside_bracket) {
		return COMMA_DELIM;
	}
	/* Ignore */
}

<DECIMAL_IS_COMMA>[+-]?[0-9]+[,]?[0-9]+ {
	/* numeric literal */
	return scan_numeric (yytext);
}

<DECIMAL_IS_COMMA>[+-]?,[0-9]+ {
	/* numeric literal */
	return scan_numeric (yytext);
}

<DECIMAL_IS_COMMA>[+-]?[0-9]+ {
	/* numeric literal */
	return scan_numeric (yytext);
}

<DECIMAL_IS_COMMA>,, {
	unput (',');
}

<DECIMAL_IS_COMMA>, {
	if (inside_bracket) {
		return COMMA_DELIM;
	}
	/* Ignore */
}

H\'[^\'\n]*\' |
H\"[^\"\n]*\" {
	/* H numeric literal */
	cb_force_pid_literal = 0;
	return scan_h (yytext + 2);
}

"END"[ \t\n]+"PROGRAM" {
	cb_force_pid_literal = 1;
	count_lines (yytext);
	return END_PROGRAM;
}

"END"[ \t\n]+"FUNCTION" {
	cb_force_pid_literal = 1;
	count_lines (yytext);
	return END_FUNCTION;
}

"NEXT"[ \t\n]+"SENTENCE" {
	count_lines (yytext);
	return NEXT_SENTENCE;
}

"SCREEN"[ \t\n]+"CONTROL" {
	count_lines (yytext);
	return SCREEN_CONTROL;
}

"EVENT"[ \t\n]+"STATUS" {
	count_lines (yytext);
	return EVENT_STATUS;
}

"BLANK"[ \t\n]+"SCREEN" {
	count_lines (yytext);
	return BLANK_SCREEN;
}

"BLANK"[ \t\n]+"LINE" {
	count_lines (yytext);
	return BLANK_LINE;
}

"CONTROL"[ \t\n]+"IS" {
	count_lines (yytext);
	return CONTROL;
}

"CONTROLS"[ \t\n]+"ARE" {
	count_lines (yytext);
	return CONTROLS;
}

"CONTROL"[ \t\n]+"HEADING" {
	count_lines (yytext);
	return CONTROL_HEADING;
}

"CONTROL"[ \t\n]+"FOOTING" {
	count_lines (yytext);
	return CONTROL_FOOTING;
}

"PAGE"[ \t\n]+"HEADING" {
	count_lines (yytext);
	return PAGE_HEADING;
}

"PAGE"[ \t\n]+"FOOTING" {
	count_lines (yytext);
	return PAGE_FOOTING;
}

"REPORT"[ \t\n]+"HEADING" {
	count_lines (yytext);
	return REPORT_HEADING;
}

"REPORT"[ \t\n]+"FOOTING" {
	count_lines (yytext);
	return REPORT_FOOTING;
}

"LAST"[ \t\n]+"DETAIL" {
	count_lines (yytext);
	return LAST_DETAIL;
}

"LAST"[ \t\n]+"DE" {
	count_lines (yytext);
	return LAST_DETAIL;
}

"LIMIT" {
	/* Ignore */
}

"LIMITS" {
	/* Ignore */
}

"NO"[ \t\n]+"ADVANCING" {
	count_lines (yytext);
	return NO_ADVANCING;
}

"NOT"[ \t\n]+"ON"[ \t\n]+"SIZE"[ \t\n]+"ERROR"[ \t\n] |
"NOT"[ \t\n]+"SIZE"[ \t\n]+"ERROR"[ \t\n] {	
	count_lines (yytext);
	return NOT_SIZE_ERROR;
}

"ON"[ \t\n]+"SIZE"[ \t\n]+"ERROR"[ \t\n] |
"SIZE"[ \t\n]+"ERROR"[ \t\n] {	
	count_lines (yytext);
	return SIZE_ERROR;
}

"NOT"[ \t\n]+"ON"[ \t\n]+"EXCEPTION"[ \t\n] |
"NOT"[ \t\n]+"EXCEPTION"[ \t\n] {	
	count_lines (yytext);
	return NOT_EXCEPTION;
}

"ON"[ \t\n]+"EXCEPTION"[ \t\n] |
"EXCEPTION"[ \t\n] {	
	count_lines (yytext);
	return EXCEPTION;
}

"NOT"[ \t\n]+"ON"[ \t\n]+"OVERFLOW"[ \t\n] |
"NOT"[ \t\n]+"OVERFLOW"[ \t\n] {	
	count_lines (yytext);
	return NOT_OVERFLOW;
}

"NOT"[ \t\n]+"AT"[ \t\n]+"END"[ \t\n] |
"NOT"[ \t\n]+"END"[ \t\n] {	
	count_lines (yytext);
	return NOT_END;
}

"AT"[ \t\n]+"END"[ \t\n] {
	count_lines (yytext);
	return END;
}

"ON"[ \t\n]+"OVERFLOW"[ \t\n] |
"OVERFLOW"[ \t\n] {	
	count_lines (yytext);
	return OVERFLOW;
}

"NOT"[ \t\n]+"AT"[ \t\n]+"END-OF-PAGE"[ \t\n] |
"NOT"[ \t\n]+"AT"[ \t\n]+"EOP"[ \t\n] |
"NOT"[ \t\n]+"END-OF-PAGE"[ \t\n] |
"NOT"[ \t\n]+"EOP"[ \t\n] {	
	count_lines (yytext);
	return NOT_EOP;
}

"AT"[ \t\n]+"END-OF-PAGE"[ \t\n] |
"AT"[ \t\n]+"EOP"[ \t\n] |
"END-OF-PAGE"[ \t\n] |
"EOP"[ \t\n] {	
	count_lines (yytext);
	return EOP;
}

"NOT"[ \t\n]+"INVALID"[ \t\n]+"KEY"[ \t\n] {
	count_lines (yytext);
	return NOT_INVALID_KEY;
}

"NOT"[ \t\n]+"INVALID"[ \t\n] {
	count_lines (yytext);
	return NOT_INVALID_KEY;
}

"INVALID"[ \t\n]+"KEY"[ \t\n] {
	count_lines (yytext);
	return INVALID_KEY;
}

"INVALID"[ \t\n] {
	count_lines (yytext);
	return INVALID_KEY;
}

"UPON"[ \t\n]+"ENVIRONMENT-NAME" {
	count_lines (yytext);
	return UPON_ENVIRONMENT_NAME;
}

"UPON"[ \t\n]+"ENVIRONMENT-VALUE" {
	count_lines (yytext);
	return UPON_ENVIRONMENT_VALUE;
}

"UPON"[ \t\n]+"ARGUMENT-NUMBER" {
	count_lines (yytext);
	return UPON_ARGUMENT_NUMBER;
}

"UPON"[ \t\n]+"COMMAND-LINE" {
	count_lines (yytext);
	return UPON_COMMAND_LINE;
}

"ID"[ ]+"DIVISION"[ ]*"." |
"IDENTIFICATION"[ ]+"DIVISION"[ ]*"." {
	/* Ignore */
}

"SWITCH"[ ]+"1" {
	yylval = cb_build_reference ("SWITCH-1");
	SET_LOCATION (yylval);
	return WORD;
}

"SWITCH"[ ]+"2" {
	yylval = cb_build_reference ("SWITCH-2");
	SET_LOCATION (yylval);
	return WORD;
}

"SWITCH"[ ]+"3" {
	yylval = cb_build_reference ("SWITCH-3");
	SET_LOCATION (yylval);
	return WORD;
}

"SWITCH"[ ]+"4" {
	yylval = cb_build_reference ("SWITCH-4");
	SET_LOCATION (yylval);
	return WORD;
}

"SWITCH"[ ]+"5" {
	yylval = cb_build_reference ("SWITCH-5");
	SET_LOCATION (yylval);
	return WORD;
}

"SWITCH"[ ]+"6" {
	yylval = cb_build_reference ("SWITCH-6");
	SET_LOCATION (yylval);
	return WORD;
}

"SWITCH"[ ]+"7" {
	yylval = cb_build_reference ("SWITCH-7");
	SET_LOCATION (yylval);
	return WORD;
}

"SWITCH"[ ]+"8" {
	yylval = cb_build_reference ("SWITCH-8");
	SET_LOCATION (yylval);
	return WORD;
}

"WHEN"[ \t\n]+"OTHER" {
	count_lines (yytext);
	return WHEN_OTHER;
}

ifdef(M4.I18N_UTF8,>>>>>
([A-Z0-9]|{UTF8_EXT})(([_A-Z0-9-]|{UTF8_EXT})*([A-Z0-9]|{UTF8_EXT})+)? {
<<<<<,>>>>>
([A-Z0-9]|{JPNWORD})(([_A-Z0-9-]|{JPNWORD})*([A-Z0-9]|{JPNWORD}+))? {
<<<<<)
	struct cb_word			*word;
	struct cb_level_78		*p78;
	struct cb_intrinsic_table	*cbp;
	cb_tree				x;
	int				token;

	/* Check word length */
	if (cobc_strlen ((unsigned char*)yytext) > 31) {
		cb_error (_("User defined name must be less than 32 characters"));
	}
        
        yytext = cb_get_hexword (yytext); 
   
	/* Check FUNCTION name without keyword */
	if (in_procedure && functions_are_all) {
		cbp = lookup_intrinsic (yytext, 1);
		if (cbp) {
			yylval = cb_build_reference (yytext);
			SET_LOCATION (yylval);
			if (strcasecmp (yytext, "CONCATENATE") == 0) {
				return CONCATENATE_FUNC;
			}
			if (strcasecmp (yytext, "CURRENT-DATE") == 0) {
				return CURRENT_DATE_FUNC;
			}
			if (strcasecmp (yytext, "UPPER-CASE") == 0) {
				return UPPER_CASE_FUNC;
			}
			if (strcasecmp (yytext, "LOWER-CASE") == 0) {
				return LOWER_CASE_FUNC;
			}
			if (strcasecmp (yytext, "REVERSE") == 0) {
				return REVERSE_FUNC;
			}
			if (strcasecmp (yytext, "SUBSTITUTE") == 0) {
				return SUBSTITUTE_FUNC;
			}
			if (strcasecmp (yytext, "SUBSTITUTE-CASE") == 0) {
				return SUBSTITUTE_CASE_FUNC;
			}
			if (strcasecmp (yytext, "TRIM") == 0) {
				return TRIM_FUNCTION;
			}
			if (strcasecmp (yytext, "WHEN-COMPILED") == 0) {
				return WHEN_COMPILED_FUNC;
			}
			if (strcasecmp (yytext, "NUMVAL-C") == 0) {
				return NUMVALC_FUNC;
			}
			if (strcasecmp (yytext, "LOCALE-DATE") == 0) {
				return LOCALE_DT_FUNC;
			}
			if (strcasecmp (yytext, "LOCALE-TIME") == 0) {
				return LOCALE_DT_FUNC;
			}
			if (strcasecmp (yytext, "LOCALE-TIME-FROM-SECONDS") == 0) {
				return LOCALE_DT_FUNC;
			}
			return FUNCTION_NAME;
		}
	}
	/* Check reserved word */
	token = lookup_reserved_word (yytext);
	if (token != 0) {
		yylval = NULL;
		return token;
	}

	for (p78 = lev78ptr; p78; p78 = p78->next) {
		if (cobc_casecmp (yytext, p78->fld78->name) == 0) {
			if (non_const_word) {
				cb_error (_("CONSTANT (78 level) may not be used here - '%s'"), yytext);
				yylval = cb_error_node;
				return WORD;
			}
			yylval = CB_VALUE (p78->fld78->values);
			return LITERAL;
		}
	}
	/* User word */
	if (cb_force_pid_literal) {
		/* Force PROGRAM-ID / END PROGRAM */
		cb_force_pid_literal = 0;
		if ((cb_c89_identifier_length_check) && (strlen (yytext) > 31)) {
			cb_warning (_("PROGRAM-ID length exceeds C89 function name limit"));
		}
		yylval = cb_build_alphanumeric_literal ((unsigned char *)yytext,
							 strlen (yytext));
		SET_LOCATION (yylval);
		return PROGRAM_NAME;
	}
	/* Check aliases before building reference */
	if ((cb_enable_program_status_register) && (cobc_casecmp (yytext, "PROGRAM-STATUS") == 0)) {
		yylval = cb_build_reference ("RETURN-CODE");
	} else if ((cb_enable_sort_status_register) && (cobc_casecmp (yytext, "SORT-STATUS") == 0)) {
		current_program->flag_sort_status_used = 1;
		yylval = cb_build_reference ("SORT-RETURN");
	} else {
		yylval = cb_build_reference (yytext);
	}
	SET_LOCATION (yylval);

	/*
	 * Distinguish the user defined classnames for parser to help
	 * processing the class conditional expression.
	 */
	if (cb_lookup_class_name (yytext)) {
		return CLASS_NAME;
	}

	/* Special name handling */
	word = CB_REFERENCE (yylval)->word;
	if (word->count > 0) {
		x = CB_VALUE (word->items);
		if (CB_SYSTEM_NAME_P (x)) {
			return MNEMONIC_NAME;
		}
	}
	/* Fix me - with the above rules this cannot happen
	if (yytext[0] == '_' || yytext[0] == '-') {
		cb_error (_("'_' or '-' is invalid as first character of user defined name"));
	}
	if (yytext[wordlen - 1] == '_' || yytext[wordlen - 1] == '-') {
		cb_error (_("'_' or '-' is invalid as last character of user defined name"));
	}
	*/
	return WORD;
}
"<=" {
	yylval = NULL;
	return LE;
}

">=" {
	yylval = NULL;
	return GE;
}

"<>" {
	yylval = NULL;
	return NE;
}

"**" {
	yylval = NULL;
	return '^';
}

"." {
	last_token_is_dot = 1;
	yylval = NULL;
	return '.';
}

. {
	yylval = NULL;
	return yytext[0];
}


<PICTURE_STATE>{
  "IS" {
	/* ignore */
  }
  [^ \t\n;]+ {
	BEGIN INITIAL;
	return scan_picture (yytext);
  }
}

<FUNCTION_STATE>{
  [a-z0-9-]+ {
	BEGIN INITIAL;
	yylval = cb_build_reference (yytext);
	SET_LOCATION (yylval);
	if (strcasecmp (yytext, "CONCATENATE") == 0) {
		return CONCATENATE_FUNC;
	}
	if (strcasecmp (yytext, "CURRENT-DATE") == 0) {
		return CURRENT_DATE_FUNC;
	}
	if (strcasecmp (yytext, "UPPER-CASE") == 0) {
		return UPPER_CASE_FUNC;
	}
	if (strcasecmp (yytext, "LOWER-CASE") == 0) {
		return LOWER_CASE_FUNC;
	}
	if (strcasecmp (yytext, "REVERSE") == 0) {
		return REVERSE_FUNC;
	}
	if (strcasecmp (yytext, "SUBSTITUTE") == 0) {
		return SUBSTITUTE_FUNC;
	}
	if (strcasecmp (yytext, "SUBSTITUTE-CASE") == 0) {
		return SUBSTITUTE_CASE_FUNC;
	}
	if (strcasecmp (yytext, "TRIM") == 0) {
		return TRIM_FUNCTION;
	}
	if (strcasecmp (yytext, "WHEN-COMPILED") == 0) {
		return WHEN_COMPILED_FUNC;
	}
	if (strcasecmp (yytext, "NUMVAL-C") == 0) {
		return NUMVALC_FUNC;
	}
	if (strcasecmp (yytext, "LOCALE-DATE") == 0) {
		return LOCALE_DT_FUNC;
	}
	if (strcasecmp (yytext, "LOCALE-TIME") == 0) {
		return LOCALE_DT_FUNC;
	}
	if (strcasecmp (yytext, "LOCALE-TIME-FROM-SECONDS") == 0) {
		return LOCALE_DT_FUNC;
	}
	return FUNCTION_NAME;
  }
  . {
	yylval = NULL;
	return yytext[0];
  }
}

<<EOF>> {
	last_token_is_dot = 0;
	integer_is_label = 0;
	inside_bracket = 0;
	inside_repository = 0;
	lev78ptr = NULL;
	cb_force_pid_literal = 0;
	yyterminate ();
}

%%

static int
read_literal (int mark, enum cb_category category)
{
	size_t		i = 0;
	int		c;

	if (!plexbuff) {
		plexbuff = cobc_malloc (COB_MINI_BUFF);
		plexsize = COB_MINI_BUFF;
	}

	while ((c = input ()) != EOF) {
#if EOF != 0
		if (unlikely (c == 0)){ 
			cb_error(_("The literal is not properly closed by %c."), mark);
			break;
		}
#endif
		plexbuff[i++] = c;
		if (c == mark && (c = input ()) != mark) {
			i--;
			unput (c);
			break;
		}
		if (i >= plexsize) {
			plexsize *= 2;
			plexbuff = cobc_realloc (plexbuff, plexsize);
		}
	}
	if (!i) {
		cb_warning (_("Alphanumeric literal has zero length"));
		cb_warning (_("A SPACE will be assumed"));
		i = 1;
		plexbuff[0] = ' ';
	}
	plexbuff[i] = 0;

#ifdef	I18N_UTF8
	/* I18N_UTF8: Alnum extended to support also national chars.
	              Literal is granted for national, only when so
		      defined explicitly. */
	if (CB_CATEGORY_NATIONAL == category) {
		yylval = cb_build_national_literal (plexbuff, i);
	} else {
		yylval = cb_build_alphanumeric_literal (plexbuff, i);
	}
#else /*!I18N_UTF8*/
	if (i % 2 == 0) {
		int j;
		for (j = 0; j < i; j += 2) {
			if ((0x81 <= plexbuff[j] && plexbuff[j] <= 0x9F) || 
			    (0xE0 <= plexbuff[j] && plexbuff[j] <= 0xFC)) {
				if (!((0x40 <= plexbuff[j+1] && plexbuff[j+1] <= 0x7E) || 
				      (0x80 <= plexbuff[j+1] && plexbuff[j+1] <= 0xFC))) {
					break;
				}
			} else {
				break;
			}
		}
		if (i != 0 && j == i) {
			yylval = cb_build_national_literal (plexbuff, j);
			SET_LOCATION (yylval);
			return LITERAL;
		}
	}

	yylval = cb_build_alphanumeric_literal (plexbuff, i);
#endif /*I18N_UTF8*/
	SET_LOCATION (yylval);
	return LITERAL;
}

static int
scan_x (char *text)
{
	unsigned char		*src;
	unsigned char		*dst;
	size_t			currlen;
	int			high = 1;
	int			c;

	if (!plexbuff) {
		plexbuff = cobc_malloc (COB_MINI_BUFF);
		plexsize = COB_MINI_BUFF;
	}
	currlen = strlen (text);
	if (currlen > plexsize) {
		plexsize = currlen;
		plexbuff = cobc_realloc (plexbuff, plexsize);
	}
	dst = plexbuff;
	src = (unsigned char *)text;
	while (isalnum (*src)) {
		c = toupper (*src);
		if ('0' <= c && c <= '9') {
			c = c - '0';
		} else if ('A' <= c && c <= 'F') {
			c = c - 'A' + 10;
		} else {
			goto error;
		}

		if (high) {
			*dst = c << 4;
		} else {
			*dst++ += c;
		}

		src++;
		high = 1 - high;
	}

	if (high) {
		yylval = cb_build_alphanumeric_literal (plexbuff, dst - plexbuff);
		SET_LOCATION (yylval);
		return LITERAL;
	}
	/* Fall through */
error:
	cb_error (_("Invalid X literal: %s"), text);
	yylval = cb_error_node;
	return LITERAL;
}

static int
scan_h (char *text)
{
	unsigned char	*p;
	long long	val = 0;
	int		c;
	char		buff[48];

	for (p = (unsigned char *)text; *p != '\'' && *p != '\"'; p++) {
		c = toupper (*p);
		if ('0' <= c && c <= '9') {
			c = c - '0';
		} else if ('A' <= c && c <= 'F') {
			c = c - 'A' + 10;
		} else {
			goto error;
		}

		val = (val << 4) + c;
	}

#ifdef _WIN32
	sprintf (buff, "%I64d", val);
#else
	sprintf (buff, "%lld", val);
#endif
	yylval = cb_build_numeric_literal (0, (unsigned char *)buff, 0);
	SET_LOCATION (yylval);
	return LITERAL;

error:
	cb_error (_("Invalid H literal: %s"), text);
	yylval = cb_error_node;
	return LITERAL;
}

static int
scan_numeric (char *text)
{
	unsigned char	*s;
	int		sign;
	int		scale = 0;

	/* get sign */
	sign = (*text == '+') ? 1 : (*text == '-') ? -1 : 0;
	if (sign) {
		text++;
	}

	/* get decimal point */
	s = (unsigned char *)strchr (text, current_program->decimal_point);
	if (s) {
		scale = strlen ((char *)s) - 1;
		memmove (s, s + 1, (size_t)(scale + 1));
	}
	if (strchr (text, '.')) {
		cb_error (_("Invalid numeric literal"));
	}
	if (strchr (text, ',')) {
		cb_error (_("Invalid numeric literal"));
	}

	yylval = cb_build_numeric_literal (sign, (unsigned char *)text, scale);
	SET_LOCATION (yylval);
	return LITERAL;
}

static int
can_replace (const char *src1, const char *src2, const size_t size,
	      const size_t iteration)
{
	const unsigned char	*p;

	if (strncasecmp (src1, src2, size)) {
		return 0;
	}
	p = (const unsigned char *)src1 + size;
	if (isalnum (*p) || *p == '-' || *p == '_') {
		return 0;
	}
	if (iteration) {
		p = (const unsigned char *)src1 - 1;
		if (isalnum (*p) || *p == '-' || *p == '_') {
			return 0;
		}
	}
	return 1;
}

static int
scan_picture (char *text)
{
	unsigned char		*p;
	struct cb_level_78	*p78;
	size_t			n;
	size_t			i;
	size_t			size;
	size_t			sizep;
	char			buff[COB_SMALL_BUFF];
	char			buff2[COB_SMALL_BUFF];

	/* normalize the input */
	for (p = (unsigned char *)text; *p; p++) {
		/* unput trailing '.' or ',' */
		if (p[1] == 0 && (*p == '.' || *p == ',')) {
			unput (*p);
			*p = 0;
			break;
		}
		/* upcase */
		*p = toupper (*p);
	}

	if (lev78ptr) {
		memset (buff, 0, sizeof (buff));
		memset (buff2, 0, sizeof (buff2));
		strcpy (buff, text);
		for (p78 = lev78ptr; p78; p78 = p78->next) {
			if (p78->fld78->values == cb_error_node) {
				strcpy (buff2, buff);
				continue;
			}
			if (CB_VALUE(p78->fld78->values) == cb_error_node) {
				strcpy (buff2, buff);
				continue;
			}
			p = (unsigned char *)CB_LITERAL(CB_VALUE(p78->fld78->values))->data;
			size = strlen (p78->fld78->name);
			sizep = strlen ((char *)p);
			i = 0;
			for (n = 0; n < strlen (buff); n++) {
				if (can_replace (&buff[n], p78->fld78->name, size, n)) {
					memcpy (&buff2[i], p, sizep);
					n += size - 1;
					i += sizep;
				} else {
					buff2[i++] = buff[n];
				}
			}
			buff2[i] = 0;
			strcpy (buff, buff2);
		}
		yylval = cb_build_picture (buff2);
	} else {
		yylval = cb_build_picture (text);
	}
	return PICTURE;
}

static void
count_lines (char *text)
{
	char *p;

	for (p = text; *p; p++) {
		if (*p == '\n') {
			cb_source_line++;
		}
	}
}

void
cb_reset_in_procedure (void)
{
	in_procedure = 0;
}

void
cb_set_in_procedure (void)
{
	in_procedure = 1;
}

void
cb_reset_78 (void)
{
	lev78ptr = NULL;
}

void
cb_add_78 (struct cb_field *f)
{
	struct cb_level_78	*p78;

	p78 = cobc_malloc (sizeof(struct cb_level_78));
	p78->fld78 = f;
	p78->next = lev78ptr;
	lev78ptr = p78;
}

struct cb_field *
check_level_78 (const char *name)
{
	struct cb_level_78	*p78;

	for (p78 = lev78ptr; p78; p78 = p78->next) {
		if (cobc_casecmp (name, p78->fld78->name) == 0) {
			return p78->fld78;
		}
	}
	return NULL;
}

void cb_unget_token (int tok, YYSTYPE lval)
{
	if (!unget_buffer) {
		unget_buffer = cobc_malloc (sizeof (struct cb_unget_buffer));
	}
	if (unget_buffer->count >= UNGET_BUFFER_SIZE) {
		cb_error (_("Scanner unget buffer overflow"));
	} else {
		unget_buffer->token[unget_buffer->count] = tok;
		unget_buffer->lvalue[unget_buffer->count] = lval;
		unget_buffer->count++;
	}
}
