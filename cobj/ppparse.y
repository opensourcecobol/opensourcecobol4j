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

%expect 4 /* 3 for nested program */

%defines
%name-prefix="pp"

%{
#include "config.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cobj.h"

#define YYDEBUG		1
#define YYERROR_VERBOSE	1
#define pperror cb_error

static char *fix_filename (char *name);
static char *fold_lower (char *name);
static char *fold_upper (char *name);

static struct cb_replace_list *cb_replace_list_add (struct cb_replace_list *replace_list, struct cb_text_list *old_text, struct cb_text_list *new_text);
static void cb_replace_list_set_type (struct cb_replace_list *list, int replace_type);
static struct cb_replace_list *cb_replace_list_add_list (struct cb_replace_list *replace_list, struct cb_replace_list *replace_list_next);
%}

%union {
	char			*s;
	struct cb_text_list	*l;
	struct cb_replace_list	*r;
	struct cb_joining_ext	*jx;
	cb_joining_ext_type_t	jt;
}

%token TOKEN_EOF 0 "end of file"
%token COPY REPLACE SUPPRESS PRINTING REPLACING OFF IN OF BY EQEQ LEADING TRAILING
%token JOINING AS PREFIX SUFFIX
%token PREFIXING SUFFIXING
%token LEVEL_NUMBER REDEFINES /* for COBOL68 style COPY in data description entry */
%token <s> TOKEN
%token PROGRAM_ID FUNCTION_ID ENVIRONMENT_DIVISION DATA_DIVISION PROCEDURE_DIVISION
%token END_PROGRAM END_FUNCTION
%type <s> copy_in _redefines_clause
%type <l> text pseudo_text token_list identifier subscripts
%type <r> copy_replacing replacing_list replacing_tokens
%type <jx> copy_joining
%type <jt> joining_ext_type

%%

/* program structure */

nested_list:
  source_element
| nested_list source_element
;

source_element:
  program_definition
| function_definition
;

program_definition:
  identification_division
  environment_division
  data_division
  procedure_division
  nested_prog
  end_program
;

program_mandatory:
  identification_division
  environment_division
  data_division
  procedure_division
  nested_prog
  end_mandatory
;

function_definition:
  function_division
  environment_division
  data_division
  procedure_division
  end_function
;

nested_prog:
| program_mandatory
| nested_prog program_mandatory
;

identification_division:
  PROGRAM_ID { pp_set_current_division (PP_IDENTIFICATION_DIVISION); }
  statement_list
;

function_division:
  FUNCTION_ID { pp_set_current_division (PP_FUNCTION_DIVISION); }
  statement_list
;
environment_division:
| ENVIRONMENT_DIVISION { pp_set_current_division (PP_ENVIRONMENT_DIVISION); }
  statement_list
;
data_division:
| DATA_DIVISION { pp_set_current_division (PP_DATA_DIVISION); }
  data_statement_list
;
procedure_division:
| PROCEDURE_DIVISION { pp_set_current_division (PP_PROCEDURE_DIVISION); }
  statement_list
;
end_program:
| END_PROGRAM  { pp_set_current_division (PP_OUT_OF_DIVISION); }
  statement_list
;
end_mandatory:
  END_PROGRAM  { pp_set_current_division (PP_OUT_OF_DIVISION); }
  statement_list
;
end_function:
| END_FUNCTION { pp_set_current_division (PP_OUT_OF_DIVISION); }
  statement_list
;

/* end of program structure */

statement_list: | statement_list statement ;
statement: copy_statement | replace_statement | '.';

data_statement_list: | data_statement_list data_statement ;
data_statement: '.' copy_statement | replace_statement | '.'
| data_description
;

data_description:
  data_description_entry recursive_copy_statement data_description
| data_description_entry recursive_copy_statement
| data_description_entry
 {
	pp_omit_data_entry_name (0);
	pp_omit_data_redef_name (0);
  }
;

data_description_entry:
  LEVEL_NUMBER TOKEN _redefines_clause
  {
	pp_omit_data_entry_name (1);
	pp_omit_data_redef_name (($3) ? 1 : 0);
  }
;

_redefines_clause:
  /* nothing */			{ $$ = NULL; }
| REDEFINES TOKEN		{ $$ = $2; }
;

recursive_copy_statement:
  recursive_copy_statement copy_statement '.'
| copy_statement '.'
;

copy_statement:
  COPY TOKEN copy_in copy_suppress copy_joining copy_replacing
  {
	fputc ('\n', ppout);
	$2 = fix_filename ($2);
	if (cb_flag_fold_copy_lower) {
		$2 = fold_lower ($2);
	} else if (cb_flag_fold_copy_upper) {
		$2 = fold_upper ($2);
	}
	if ($3) {
		$3 = fix_filename ($3);
		if (cb_flag_fold_copy_lower) {
			$3 = fold_lower ($3);
		} else if (cb_flag_fold_copy_upper) {
			$3 = fold_upper ($3);
		}
	}
	ppcopy ($2, $3, $5, $6);
  }
;

copy_in:
  /* nothing */			{ $$ = NULL; }
| IN TOKEN			{ $$ = $2; }
| OF TOKEN			{ $$ = $2; }
;

copy_suppress:
| SUPPRESS _printing
;

copy_replacing:
  /* nothing */			{ $$ = NULL; }
| REPLACING replacing_list	{ $$ = $2; }
;

replace_statement:
  REPLACE replacing_list '.'	{ pp_set_replace_list ($2); }
| REPLACE OFF '.'		{ pp_set_replace_list (NULL); }
;

replacing_list:
  replacing_tokens			{ $$ = cb_replace_list_add_list (NULL, $1); }
| replacing_list replacing_tokens	{ $$ = cb_replace_list_add_list ($1, $2); }
;

replacing_tokens:
  text BY text			{ $$ = cb_replace_list_add (NULL, $1, $3); }
| LEADING pseudo_text BY pseudo_text
  {
	$$ = cb_replace_list_add (NULL, $2, $4);
	cb_replace_list_set_type ($$, CB_REPLACE_LEADING);
  }
| TRAILING pseudo_text BY pseudo_text
  {
	$$ = cb_replace_list_add (NULL, $2, $4);
	cb_replace_list_set_type ($$, CB_REPLACE_TRAILING);
  }
;

copy_joining:
/* nothing */			{ $$ = NULL; }
| JOINING TOKEN _as joining_ext_type
  {
	struct cb_joining_ext *p = cobc_malloc (sizeof (struct cb_joining_ext));
	p->ext = $2;
	p->type = $4;
	$$ = p;
  }
| PREFIXING TOKEN
  {
	struct cb_joining_ext *p = cobc_malloc (sizeof (struct cb_joining_ext));
	p->ext  = $2;
	p->type = prefixing;
	$$ = p;
  }
| SUFFIXING TOKEN
  {
	struct cb_joining_ext *p = cobc_malloc (sizeof (struct cb_joining_ext));
	p->ext  = $2;
	p->type = suffixing;
	$$ = p;
  }
;

_as: | AS ;

joining_ext_type:
  PREFIX 			{ $$ = joining_as_prefix; }
| SUFFIX			{ $$ = joining_as_suffix; }
;

text:
  pseudo_text			{ $$ = $1; }
| identifier			{ $$ = $1; }
;

pseudo_text:
  EQEQ EQEQ			{ $$ = NULL; }
| EQEQ token_list EQEQ		{ $$ = $2; }
;

token_list:
  TOKEN				{ $$ = cb_text_list_add (NULL, $1); }
| token_list TOKEN		{ $$ = cb_text_list_add ($1, $2); }
;

identifier:
  TOKEN				{ $$ = cb_text_list_add (NULL, $1); }
| identifier IN TOKEN
  {
	$$ = cb_text_list_add ($1, " ");
	$$ = cb_text_list_add ($$, "IN");
	$$ = cb_text_list_add ($$, " ");
	$$ = cb_text_list_add ($$, $3);
  }
| identifier OF TOKEN
  {
	$$ = cb_text_list_add ($1, " ");
	$$ = cb_text_list_add ($$, "OF");
	$$ = cb_text_list_add ($$, " ");
	$$ = cb_text_list_add ($$, $3);
  }
| identifier '(' subscripts ')'
  {
	struct cb_text_list *l;

	$$ = cb_text_list_add ($1, " ");
	$$ = cb_text_list_add ($$, "(");
	$3 = cb_text_list_add ($3, ")");
	for (l = $$; l->next; l = l->next);
	l->next = $3;
  }
;

subscripts:
  TOKEN				{ $$ = cb_text_list_add (NULL, $1); }
| subscripts TOKEN
  {
	$$ = cb_text_list_add ($1, " ");
	$$ = cb_text_list_add ($$, $2);
  }
;

_printing: | PRINTING ;

%%
static char *
fix_filename (char *name)
{
	/* remove quotation from alphanumeric literals */
	if (name[0] == '\'' || name[0] == '\"') {
		name++;
		name[strlen (name) - 1] = 0;
	}
	return name;
}

static char *
fold_lower (char *name)
{
	unsigned char	*p;

	for (p = (unsigned char *)name; *p; p++) {
		if (isupper (*p)) {
			*p = tolower (*p);
		}
	}
	return name;
}

static char *
fold_upper (char *name)
{
	unsigned char	*p;

	for (p = (unsigned char *)name; *p; p++) {
		if (islower (*p)) {
			*p = toupper (*p);
		}
	}
	return name;
}

static struct cb_replace_list *
cb_replace_list_add (struct cb_replace_list *list,
		     struct cb_text_list *old_text,
		     struct cb_text_list *new_text)
{
	struct cb_replace_list *p;
	struct cb_replace_list *l;

	p = cobc_malloc (sizeof (struct cb_replace_list));
	p->old_text = old_text;
	p->new_text = new_text;
	p->replace_type = CB_REPLACE_OTHER;
	p->next = NULL;
	if (!list) {
		return p;
	} else {
		for (l = list; l->next; l = l->next) ;
		l->next = p;
		return list;
	}
}

static void
cb_replace_list_set_type (struct cb_replace_list *list, int replace_type)
{
	struct cb_replace_list *l;
	if (list) {
		for (l = list; l->next; l = l->next) ;
		l->replace_type = replace_type;
	}
}

static struct cb_replace_list *
cb_replace_list_add_list (struct cb_replace_list *list, struct cb_replace_list *list_next)
{
	struct cb_replace_list *l;

	if (!list) {
		return list_next;
	} else {
		for (l = list; l->next; l = l->next) ;
		l->next = list_next;
		return list;
	}
}
