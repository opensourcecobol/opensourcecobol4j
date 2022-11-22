/*
 * Copyright (C) 2001-2009 Keisuke Nishida
 * Copyright (C) 2007-2009 - Roger While
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

%expect 146

%defines
%verbose

%{
#include "config.h"

#include <stdlib.h>
#include <string.h>

#include "cobj.h"
#include "tree.h"

#define yyerror			cb_error
#define YYDEBUG			1
#define YYERROR_VERBOSE		1

#define PENDING(x)		cb_warning (_("'%s' not implemented"), x)

#define emit_statement(x) \
  current_program->exec_list = cb_cons (x, current_program->exec_list)

#define push_expr(type, node) \
  current_expr = cb_build_list (cb_int (type), node, current_expr)

#define TERM_NONE		0
#define TERM_ACCEPT		1
#define TERM_ADD		2
#define TERM_CALL		3
#define TERM_COMPUTE		4
#define TERM_DELETE		5
#define TERM_DISPLAY		6
#define TERM_DIVIDE		7
#define TERM_EVALUATE		8
#define TERM_IF			9
#define TERM_MULTIPLY		10
#define TERM_PERFORM		11
#define TERM_READ		12
#define TERM_RECEIVE		13
#define TERM_RETURN		14
#define TERM_REWRITE		15
#define TERM_SEARCH		16
#define TERM_START		17
#define TERM_STRING		18
#define TERM_SUBTRACT		19
#define TERM_UNSTRING		20
#define TERM_WRITE		21
#define TERM_MAX		22

/* Global variables */

struct cb_program		*current_program = NULL;
struct cb_statement		*current_statement = NULL;
struct cb_label			*current_section = NULL;
struct cb_label			*current_paragraph = NULL;
size_t				functions_are_all = 0;
int				non_const_word = 0;

/* Local variables */

static struct cb_statement	*main_statement;

static cb_tree			current_expr;
static struct cb_field		*current_field;
static struct cb_field		*description_field;
static struct cb_file		*current_file;
static struct cb_key_component	*key_component_list;
static enum cb_storage		current_storage;

static size_t			check_unreached = 0;
static int			call_mode;
static int			size_mode;

static cb_tree			perform_stack = NULL;
static cb_tree			qualifier = NULL;

static cb_tree			fgc;
static cb_tree			bgc;
static cb_tree			scroll;
static cb_tree			save_tree_1;
static cb_tree			save_tree_2;
static cb_tree			dummy_tree;
static size_t			in_declaratives = 0;
static size_t			current_linage = 0;
static size_t			prog_end = 0;
static size_t			use_global_ind = 0;
static size_t			samearea = 1;
static size_t			organized_seen = 0;
static size_t			inspect_keyword = 0;
static int			next_label_id = 0;
static int			eval_level = 0;
static int			eval_inc = 0;
static int			eval_inc2 = 0;
static int			depth = 0;
static int			dispattrs = 0;
static struct cb_file		*linage_file;
static cb_tree			next_label_list = NULL;
static char			*stack_progid[32];
static int			term_array[TERM_MAX];
static int			eval_check[64][64];

/* Static functions */

static void
BEGIN_STATEMENT (const char *name, const size_t term)
{
	if (cb_warn_unreachable && check_unreached) {
		cb_warning (_("Unreachable statement '%s'"), (char *)name);
	}
	current_statement = cb_build_statement ((char *)name);
	CB_TREE (current_statement)->source_file = (unsigned char *)cb_source_file;
	CB_TREE (current_statement)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_statement));
	if (term) {
		term_array[term]++;
	}
	main_statement = current_statement;
}

static void
BEGIN_IMPLICIT_STATEMENT (cb_tree node)
{
	current_statement = cb_build_statement (NULL);
	CB_TREE (current_statement)->source_file = CB_TREE (node)->source_file;
	CB_TREE (current_statement)->source_line = CB_TREE (node)->source_line;
	main_statement->body = cb_list_add (main_statement->body,
					    CB_TREE (current_statement));
}

static void
emit_entry (const char *name, const int encode, cb_tree using_list)
{
	cb_tree		l;
	cb_tree		label;
	cb_tree		x;
	struct cb_field	*f;
	int		parmnum;
	char		buff[256];

	sprintf (buff, "E$%s", name);
	label = cb_build_label (cb_build_reference (buff), NULL);
	if (encode) {
		CB_LABEL (label)->name = (unsigned char *)(cb_encode_program_id (name));
		CB_LABEL (label)->orig_name = (unsigned char *)name;
	} else {
		CB_LABEL (label)->name = (unsigned char *)name;
		CB_LABEL (label)->orig_name = (unsigned char *)current_program->orig_source_name;
	}
	CB_LABEL (label)->need_begin = 1;
	CB_LABEL (label)->is_entry = 1;
	emit_statement (label);

	parmnum = 1;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (x != cb_error_node && cb_ref (x) != cb_error_node) {
			f = CB_FIELD (cb_ref (x));
			if (f->level != 01 && f->level != 77) {
				cb_error_x (x, _("'%s' not level 01 or 77"), cb_name (x));
			}
			if (!current_program->flag_chained) {
				if (f->storage != CB_STORAGE_LINKAGE) {
					cb_error_x (x, _("'%s' is not in LINKAGE SECTION"), cb_name (x));
				}
				if (f->flag_item_based || f->flag_external) {
					cb_error_x (x, _("'%s' can not be BASED/EXTERNAL"), cb_name (x));
				}
				f->flag_is_pdiv_parm = 1;
			} else {
				if (f->storage != CB_STORAGE_WORKING) {
					cb_error_x (x, _("'%s' is not in WORKING-STORAGE SECTION"), cb_name (x));
				}
				f->flag_chained = 1;
				f->param_num = parmnum;
				parmnum++;
			}
			if (f->redefines) {
				cb_error_x (x, _("'%s' REDEFINES field not allowed here"), cb_name (x));
			}
		}
	}

	/* Check dangling LINKAGE items */
	if (cb_warn_linkage) {
		for (f = current_program->linkage_storage; f; f = f->sister) {
			for (l = using_list; l; l = CB_CHAIN (l)) {
				x = CB_VALUE (l);
				if (x != cb_error_node && cb_ref (x) != cb_error_node) {
					if (f == CB_FIELD (cb_ref (x))) {
						break;
					}
				}
			}
			if (!l && !f->redefines) {
				cb_warning (_("LINKAGE item '%s' is not a PROCEDURE USING parameter"), f->name);
			}
		}
	}

	for (l = current_program->entry_list; l; l = CB_CHAIN (l)) {
		if (strcmp ((const char *)name, (const char *)(CB_LABEL(CB_PURPOSE(l))->name)) == 0) {
			cb_error_x (CB_TREE (current_statement), _("ENTRY '%s' duplicated"), name);
		}
	}

	current_program->entry_list = cb_list_append (current_program->entry_list,
							cb_build_pair (label, using_list));
}

static void
terminator_warning (const size_t termid)
{
	check_unreached = 0;
	if (cb_warn_terminator && term_array[termid]) {
		cb_warning_x (CB_TREE (current_statement),
			_("%s statement not terminated by END-%s"),
			current_statement->name, current_statement->name);
	}
	if (term_array[termid]) {
		term_array[termid]--;
	}
}

static void
terminator_error (void)
{
	check_unreached = 0;
	cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated by END-%s"),
			current_statement->name, current_statement->name);
}

static void
terminator_clear (const size_t termid)
{
	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	}
}

static int
literal_value (cb_tree x)
{
	if (x == cb_space) {
		return ' ';
	} else if (x == cb_zero) {
		return '0';
	} else if (x == cb_quote) {
		return '"';
	} else if (x == cb_null) {
		return 0;
	} else if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
		return cb_get_int (x);
	} else {
		return CB_LITERAL (x)->data[0];
	}
}

static void
setup_use_file (struct cb_file *fileptr)
{
	struct cb_file	*newptr;

	if (fileptr->organization == COB_ORG_SORT) {
		cb_error (_("USE statement invalid for SORT file"));
	}
	if (fileptr->global) {
		newptr = cobc_malloc (sizeof(struct cb_file));
		*newptr = *fileptr;
		newptr->handler = current_section;
		newptr->handler_prog = current_program;
		if (!use_global_ind) {
			current_program->local_file_list =
				cb_list_add (current_program->local_file_list,
					     CB_TREE (newptr));
		} else {
			current_program->global_file_list =
				cb_list_add (current_program->global_file_list,
					     CB_TREE (newptr));
		}
	} else {
		fileptr->handler = current_section;
	}
}

%}

%token TOKEN_EOF 0 "end of file"

%token ACCEPT
%token ACCESS
%token ADD
%token ADDRESS
%token ADVANCING
%token AFTER
%token ALL
%token ALLOCATE
%token ALPHABET
%token ALPHABETIC
%token ALPHABETIC_LOWER		"ALPHABETIC-LOWER"
%token ALPHABETIC_UPPER		"ALPHABETIC-UPPER"
%token ALPHANUMERIC
%token ALPHANUMERIC_EDITED	"ALPHANUMERIC-EDITED"
%token ALSO
%token ALTER
%token ALTERNATE
%token AND
%token ANY
%token APPLY
%token ARE
%token AREA
%token ARGUMENT_NUMBER		"ARGUMENT-NUMBER"
%token ARGUMENT_VALUE		"ARGUMENT-VALUE"
%token AS
%token ASCENDING
%token ASSIGN
%token AT
%token AUTO
%token AUTOMATIC
%token BACKGROUND_COLOR		"BACKGROUND-COLOR"
%token BASED
%token BEFORE
%token BELL
%token BINARY
%token BINARY_C_LONG		"BINARY-C-LONG"
%token BINARY_CHAR		"BINARY-CHAR"
%token BINARY_DOUBLE		"BINARY-DOUBLE"
%token BINARY_LONG		"BINARY-LONG"
%token BINARY_SHORT		"BINARY-SHORT"
%token BLANK
%token BLANK_LINE		"BLANK-LINE"
%token BLANK_SCREEN		"BLANK-SCREEN"
%token BLINK
%token BLOCK
%token BOTTOM
%token BY
%token BYTE_LENGTH		"BYTE-LENGTH"
%token CALL
%token CANCEL
%token CH
%token CHAINING
%token CHARACTER
%token CHARACTERS
%token CLASS
%token CLASS_NAME		/* user defined classname */
%token CLOSE
%token CLOSE_NOFEED		"CLOSE-NOFEED"
%token CODE
%token CODE_SET			"CODE-SET"
%token COLLATING
%token COL
%token COLS
%token COLUMN
%token COLUMNS
%token COMMA
%token COMMAND_LINE		"COMMAND-LINE"
%token COMMA_DELIM		"comma delimiter"
%token COMMIT
%token COMMITMENT_CONTROL	"COMMITMENT-CONTROL"
%token COMMON
%token COMP
%token COMPUTE
%token COMP_1			"COMP-1"
%token COMP_2			"COMP-2"
%token COMP_3			"COMP-3"
%token COMP_4			"COMP-4"
%token COMP_5			"COMP-5"
%token COMP_X			"COMP-X"
%token CONCATENATE_FUNC		"FUNCTION CONCATENATE"
%token CONFIGURATION
%token CONSTANT
%token CONTAINS
%token CONTENT
%token CONTINUE
%token CONTROL
%token CONTROLS
%token CONTROL_FOOTING		"CONTROL FOOTING"
%token CONTROL_HEADING		"CONTROL HEADING"
%token CONVERTING
%token CORE_INDEX		"CORE-INDEX"
%token CORRESPONDING
%token COUNT
%token CRT
%token CURRENCY
%token CURRENT_DATE_FUNC	"FUNCTION CURRENT-DATE"
%token CURSOR
%token CYCLE
%token CYL_OVERFLOW		"CYL-OVERFLOW"
%token DATA
%token DATE
%token DAY
%token DAY_OF_WEEK		"DAY-OF-WEEK"
%token DE
%token DEBUGGING
%token DECIMAL_POINT		"DECIMAL-POINT"
%token DECLARATIVES
%token DEFAULT
%token DELETE
%token DELIMITED
%token DELIMITER
%token DEPENDING
%token DESCENDING
%token DETAIL
%token DISK
%token DISPLAY
%token DIVIDE
%token DIVISION
%token DOWN
%token DUPLICATES
%token DYNAMIC
%token EBCDIC
%token ELSE
%token END
%token END_ACCEPT		"END-ACCEPT"
%token END_ADD			"END-ADD"
%token END_CALL			"END-CALL"
%token END_COMPUTE		"END-COMPUTE"
%token END_DELETE		"END-DELETE"
%token END_DISPLAY		"END-DISPLAY"
%token END_DIVIDE		"END-DIVIDE"
%token END_EVALUATE		"END-EVALUATE"
%token END_FUNCTION		"END FUNCTION"
%token END_IF			"END-IF"
%token END_MULTIPLY		"END-MULTIPLY"
%token END_PERFORM		"END-PERFORM"
%token END_PROGRAM		"END PROGRAM"
%token END_READ			"END-READ"
%token END_RETURN		"END-RETURN"
%token END_REWRITE		"END-REWRITE"
%token END_SEARCH		"END-SEARCH"
%token END_START		"END-START"
%token END_STRING		"END-STRING"
%token END_SUBTRACT		"END-SUBTRACT"
%token END_UNSTRING		"END-UNSTRING"
%token END_WRITE		"END-WRITE"
%token ENTRY
%token ENVIRONMENT
%token ENVIRONMENT_NAME		"ENVIRONMENT-NAME"
%token ENVIRONMENT_VALUE	"ENVIRONMENT-VALUE"
%token EOL
%token EOP
%token EOS
%token EQUAL
%token EQUALS
%token ERASE
%token ERROR
%token ESCAPE
%token EVALUATE
%token EVENT_STATUS		"EVENT-STATUS"
%token EXCEPTION
%token EXCLUSIVE
%token EXIT
%token EXTEND
%token EXTERNAL
%token FD
%token FILE_CONTROL		"FILE-CONTROL"
%token FILE_ID			"FILE-ID"
%token FILLER
%token FINAL
%token FIRST
%token FOOTING
%token FOR
%token FOREGROUND_COLOR		"FOREGROUND-COLOR"
%token FOREVER
%token FORMS_OVERLAY		"FORMS-OVERLAY"
%token FREE
%token FROM
%token FULL
%token FUNCTION
%token FUNCTION_ID		"FUNCTION-ID"
%token FUNCTION_NAME		"FUNCTION"
%token GE
%token GENERATE
%token GIVING
%token GLOBAL
%token GO
%token GOBACK
%token GREATER
%token GROUP
%token HEADING
%token HIGHLIGHT
%token HIGH_VALUE		"HIGH-VALUE"
%token IDENTIFICATION
%token IF
%token IGNORE
%token IGNORING
%token IN
%token INDEX
%token INDEXED
%token INDICATE
%token INITIALIZE
%token INITIALIZED
%token INITIATE
%token INPUT
%token INPUT_OUTPUT		"INPUT-OUTPUT"
%token INSPECT
%token INTO
%token INTRINSIC
%token INVALID
%token INVALID_KEY		"INVALID KEY"
%token IS
%token I_O			"I-O"
%token I_O_CONTROL		"I-O-CONTROL"
%token JUSTIFIED
%token KEY
%token LABEL
%token LAST
%token LAST_DETAIL		"LAST DETAIL"
%token LE
%token LEADING
%token LEFT
%token LENGTH
%token LESS
%token LEVEL_NUMBER_WORD
%token LEVEL88_NUMBER_WORD
%token LIMIT
%token LIMITS
%token LINAGE
%token LINAGE_COUNTER		"LINAGE-COUNTER"
%token LINE
%token LINES
%token LINKAGE
%token LITERAL			"Literal"
%token LOCALE
%token LOCALE_DT_FUNC		"FUNCTION LOCALE"
%token LOCAL_STORAGE		"LOCAL-STORAGE"
%token LOCK
%token LOWER_CASE_FUNC		"FUNCTION LOWER-CASE"
%token LOWLIGHT
%token LOW_VALUE		"LOW-VALUE"
%token MANUAL
%token MEMORY
%token MERGE
%token MINUS
%token MNEMONIC_NAME		"MNEMONIC NAME"
%token MODE
%token MOVE
%token MULTIPLE
%token MULTIPLY
%token NATIONAL
%token NATIONAL_EDITED		"NATIONAL-EDITED"
%token NATIVE
%token NE
%token NEGATIVE
%token NEXT
%token NEXT_SENTENCE		"NEXT SENTENCE"
%token NO
%token NOMINAL
%token NOT
%token NOT_END			"NOT END"
%token NOT_EOP			"NOT EOP"
%token NOT_EXCEPTION		"NOT EXCEPTION"
%token NOT_INVALID_KEY		"NOT INVALID KEY"
%token NOT_OVERFLOW		"NOT OVERFLOW"
%token NOT_SIZE_ERROR		"NOT SIZE ERROR"
%token NO_ADVANCING		"NO ADVANCING"
%token NUMBER
%token NUMBERS
%token NUMERIC
%token NUMERIC_EDITED		"NUMERIC-EDITED"
%token NUMVALC_FUNC		"FUNCTION NUMVALC"
%token OBJECT_COMPUTER		"OBJECT-COMPUTER"
%token OCCURS
%token OF
%token OFF
%token OMITTED
%token ON
%token ONLY
%token OPEN
%token OPTIONAL
%token OR
%token ORDER
%token ORGANIZATION
%token OTHER
%token OUTPUT
%token OVERFLOW
%token OVERLINE
%token PACKED_DECIMAL		"PACKED-DECIMAL"
%token PADDING
%token PAGE
%token PAGE_FOOTING		"PAGE FOOTING"
%token PAGE_HEADING		"PAGE HEADING"
%token PARAGRAPH
%token PERFORM
%token PICTURE
%token PLUS
%token POINTER
%token POSITION
%token POSITIVE
%token PRESENT
%token PREVIOUS
%token PRINTER
%token PRINTING
%token PROCEDURE
%token PROCEDURES
%token PROCEED
%token PROGRAM
%token PROGRAM_ID		"PROGRAM-ID"
%token PROGRAM_NAME		"Program name"
%token PROGRAM_POINTER		"PROGRAM-POINTER"
%token PROMPT
%token QUOTE
%token RANDOM
%token RD
%token READ
%token RECORD
%token RECORDING
%token RECORDS
%token RECURSIVE
%token REDEFINES
%token REEL
%token REFERENCE
%token RELATIVE
%token RELEASE
%token REMAINDER
%token REMOVAL
%token RENAMES
%token REPLACING
%token REPORT
%token REPORTING
%token REPORTS
%token REPORT_FOOTING		"REPORT FOOTING"
%token REPORT_HEADING		"REPORT HEADING"
%token REPOSITORY
%token REQUIRED
%token RESERVE
%token RETURN
%token RETURNING
%token REVERSE_FUNC		"FUNCTION REVERSE"
%token REVERSE_VIDEO		"REVERSE-VIDEO"
%token REWIND
%token REWRITE
%token RIGHT
%token ROLLBACK
%token ROUNDED
%token RUN
%token SAME
%token SCREEN
%token SCREEN_CONTROL		"SCREEN-CONTROL"
%token SCROLL
%token SD
%token SEARCH
%token SECTION
%token SECURE
%token SEGMENT_LIMIT		"SEGMENT-LIMIT"
%token SELECT
%token SEMI_COLON		"semi-colon"
%token SENTENCE
%token SEPARATE
%token SEQUENCE
%token SEQUENTIAL
%token SET
%token SHARING
%token SIGN
%token SIGNED
%token SIGNED_INT		"SIGNED-INT"
%token SIGNED_LONG		"SIGNED-LONG"
%token SIGNED_SHORT		"SIGNED-SHORT"
%token SIZE
%token SIZE_ERROR		"SIZE ERROR"
%token SORT
%token SORT_MERGE		"SORT-MERGE"
%token SOURCE
%token SOURCE_COMPUTER		"SOURCE-COMPUTER"
%token SPACE
%token SPECIAL_NAMES		"SPECIAL-NAMES"
%token STANDARD
%token STANDARD_1		"STANDARD-1"
%token STANDARD_2		"STANDARD-2"
%token START
%token STATUS
%token STOP
%token STRING
%token SUBSTITUTE_FUNC		"FUNCTION SUBSTITUTE"
%token SUBSTITUTE_CASE_FUNC	"FUNCTION SUBSTITUTE-CASE"
%token SUBTRACT
%token SUM
%token SUPPRESS
%token SYMBOLIC
%token SYNCHRONIZED
%token TALLYING
%token TAPE
%token TERMINATE
%token TEST
%token THAN
%token THEN
%token THRU
%token TIME
%token TIMES
%token TO
%token TOK_FALSE		"FALSE"
%token TOK_FILE			"FILE"
%token TOK_INITIAL		"INITIAL"
%token TOK_NULL			"NULL"
%token TOK_TRUE			"TRUE"
%token TOP
%token TRACKS
%token TRAILING
%token TRANSFORM
%token TRIM_FUNCTION		"FUNCTION TRIM"
%token TYPE
%token UNDERLINE
%token UNIT
%token UNLOCK
%token UNSIGNED
%token UNSIGNED_INT		"UNSIGNED-INT"
%token UNSIGNED_LONG		"UNSIGNED-LONG"
%token UNSIGNED_SHORT		"UNSIGNED-SHORT"
%token UNSTRING
%token UNTIL
%token UP
%token UPDATE
%token UPON
%token UPON_ARGUMENT_NUMBER	"UPON ARGUMENT-NUMBER"
%token UPON_COMMAND_LINE	"UPON COMMAND-LINE"
%token UPON_ENVIRONMENT_NAME	"UPON ENVIRONMENT-NAME"
%token UPON_ENVIRONMENT_VALUE	"UPON ENVIRONMENT-VALUE"
%token UPPER_CASE_FUNC		"FUNCTION UPPER-CASE"
%token USAGE
%token USE
%token USING
%token VALUE
%token VARYING
%token WAIT
%token WHEN
%token WHEN_COMPILED_FUNC	"FUNCTION WHEN-COMPILED"
%token WHEN_OTHER		"WHEN OTHER"
%token WITH
%token WORD			"Identifier"
%token WORDS
%token WORKING_STORAGE		"WORKING-STORAGE"
%token WRITE
%token YYYYDDD
%token YYYYMMDD
%token ZERO

%left '+' '-'
%left '*' '/'
%left UNARY_SIGN
%right '^'


%%
/*****************************************************************************
 * COBOL Compilation Unit
 *****************************************************************************/

start:
  {
	perform_stack = NULL;
	current_statement = NULL;
	next_label_id = 0;
	current_linage = 0;
	current_storage = 0;
	eval_level = 0;
	eval_inc = 0;
	eval_inc2 = 0;
	prog_end = 0;
	depth = 0;
	inspect_keyword = 0;
	check_unreached = 0;
	samearea = 1;
	memset ((char *)eval_check, 0, sizeof(eval_check));
	memset ((char *)term_array, 0, sizeof(term_array));
	linage_file = NULL;
	next_label_list = NULL;
	current_program = cb_build_program (NULL, 0);
	cb_build_registers ();
	current_program->flag_main = cb_flag_main;
  }
  /* program_definition */
  nested_list TOKEN_EOF
  {
	current_section = NULL;
	current_paragraph = NULL;
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
	if (depth > 1) {
		cb_error (_("Multiple PROGRAM-ID's without matching END PROGRAM"));
	}
	if (errorcount > 0) {
		YYABORT;
	}
	if (!current_program->entry_list) {
		emit_entry (current_program->program_id, 0, NULL);
	}
  }
;

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
  environment_division	{ cb_validate_program_environment (current_program); }
  data_division		{ cb_validate_program_data (current_program); }
  procedure_division
  nested_prog
  end_program
;

program_mandatory:
  identification_division
  environment_division	{ cb_validate_program_environment (current_program); }
  data_division		{ cb_validate_program_data (current_program); }
  procedure_division
  nested_prog
  end_mandatory
;

function_definition:
  function_division
  environment_division	{ cb_validate_program_environment (current_program); }
  data_division		{ cb_validate_program_data (current_program); }
  procedure_division
  end_function
;

nested_prog:
| program_mandatory
| nested_prog program_mandatory
;

end_program:
| END_PROGRAM program_name '.'
  {
	char			*s;

	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P ($2)) {
		s = (char *)(CB_LITERAL ($2)->data);
	} else {
		s = (char *)(CB_NAME ($2));
	}
	if (depth) {
		depth--;
	}
	if (cb_allow_end_program_with_wrong_name) {
		/* ignore wrong program-id. */
	} else if (strcmp (stack_progid[depth], s)) {
		cb_error (_("END PROGRAM '%s' is different to PROGRAM-ID '%s'"),
			s, stack_progid[depth]);
	}
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
  }
;

end_mandatory:
  END_PROGRAM program_name '.'
  {
	char			*s;

	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P ($2)) {
		s = (char *)(CB_LITERAL ($2)->data);
	} else {
		s = (char *)(CB_NAME ($2));
	}
	if (depth) {
		depth--;
	}
	if (strcmp (stack_progid[depth], s)) {
		cb_error (_("END PROGRAM '%s' is different to PROGRAM-ID '%s'"),
			s, stack_progid[depth]);
	}
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
  }
;

end_function:
  END_FUNCTION program_name '.'
  {
	char			*s;

	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P ($2)) {
		s = (char *)(CB_LITERAL ($2)->data);
	} else {
		s = (char *)(CB_NAME ($2));
	}
	if (depth) {
		depth--;
	}
	if (strcmp (stack_progid[depth], s)) {
		cb_error (_("END FUNCTION '%s' is different to FUNCTION-ID '%s'"),
			s, stack_progid[depth]);
	}
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
  }
;


/*****************************************************************************
 * Identification division
 *****************************************************************************/

identification_division:
  PROGRAM_ID '.' program_name as_literal
  {
	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P ($3)) {
		stack_progid[depth] = (char *)(CB_LITERAL ($3)->data);
	} else {
		stack_progid[depth] = (char *)(CB_NAME ($3));
	}
	if (prog_end) {
		if (!current_program->flag_validated) {
			current_program->flag_validated = 1;
			cb_validate_program_body (current_program);
		}
		perform_stack = NULL;
		current_statement = NULL;
		next_label_id = 0;
		current_linage = 0;
		current_storage = 0;
		eval_level = 0;
		inspect_keyword = 0;
		check_unreached = 0;
		eval_inc = 0;
		eval_inc2 = 0;
		samearea = 1;
		memset ((char *)eval_check, 0, sizeof(eval_check));
		memset ((char *)term_array, 0, sizeof(term_array));
		linage_file = NULL;
		next_label_list = NULL;
		current_program = cb_build_program (current_program, depth);
		cb_build_registers ();
	} else {
		prog_end = 1;
	}
	depth++;
	current_program->program_id = cb_build_program_id ($3, $4);
  }
  program_type '.'
;

function_division:
  FUNCTION_ID '.' program_name as_literal '.'
  {
	cb_error (_("FUNCTION-ID is not yet implemented"));
	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P ($3)) {
		stack_progid[depth] = (char *)(CB_LITERAL ($3)->data);
	} else {
		stack_progid[depth] = (char *)(CB_NAME ($3));
	}
	if (prog_end) {
		if (!current_program->flag_validated) {
			current_program->flag_validated = 1;
			cb_validate_program_body (current_program);
		}
		perform_stack = NULL;
		current_statement = NULL;
		next_label_id = 0;
		current_linage = 0;
		current_storage = 0;
		eval_level = 0;
		inspect_keyword = 0;
		check_unreached = 0;
		eval_inc = 0;
		eval_inc2 = 0;
		samearea = 1;
		memset ((char *)eval_check, 0, sizeof(eval_check));
		memset ((char *)term_array, 0, sizeof(term_array));
		linage_file = NULL;
		next_label_list = NULL;
		current_program = cb_build_program (current_program, depth);
		cb_build_registers ();
	} else {
		prog_end = 1;
	}
	depth++;
	current_program->program_id = cb_build_program_id ($3, $4);
	current_program->prog_type = CB_FUNCTION_TYPE;
	current_program->flag_recursive = 1;
	current_program->flag_initial = 1;
  }
;

program_name:
  PROGRAM_NAME
| LITERAL
;

as_literal:
  /* empty */			{ $$ = NULL; }
| AS LITERAL			{ $$ = $2; }
;

program_type:
| _is program_type_clause _program
;

program_type_clause:
  COMMON
  {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a nested program"));
	}
	current_program->flag_common = 1;
  }
| COMMON _init_or_recurs
  {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a nested program"));
	}
	current_program->flag_common = 1;
  }
| _init_or_recurs
;

_init_or_recurs:
  TOK_INITIAL
  {
	current_program->flag_initial = 1;
  }
| RECURSIVE
  {
	current_program->flag_recursive = 1;
	current_program->flag_initial = 1;
  }
;


/*****************************************************************************
 * Environment division
 *****************************************************************************/

environment_division:
| ENVIRONMENT DIVISION '.'
  configuration_section
  input_output_section
;


/*******************
 * Configuration section
 *******************/

configuration_section:
| CONFIGURATION SECTION '.'
  configuration_list
  {
	if (current_program->nested_level) {
		cb_error (_("CONFIGURATION SECTION not allowed in nested programs"));
	}
  }
;

configuration_list:
| configuration_list configuration_paragraph
;

configuration_paragraph:
  source_computer_paragraph
| object_computer_paragraph
| special_names_paragraph
| repository_paragraph
;


/*
 * SOURCE-COMPUTER paragraph
 */

source_computer_paragraph:
  SOURCE_COMPUTER '.' source_computer_entry
;

source_computer_entry:
| computer_name '.'
| computer_name with_debugging_mode '.'
| with_debugging_mode '.'
;

with_debugging_mode:
  _with DEBUGGING MODE
  {
	cb_verify (cb_debugging_line, "DEBUGGING MODE");
  }
;

computer_name:
  WORD { }
;


/*
 * OBJECT-COMPUTER paragraph
 */

object_computer_paragraph:
  OBJECT_COMPUTER '.' object_computer_entry
;

object_computer_entry:
| computer_name '.'
| computer_name object_clauses_list '.'
| object_clauses_list '.'
;

object_clauses_list:
  object_clauses
| object_clauses_list object_clauses
;

object_clauses:
  object_computer_memory
| object_computer_sequence
| object_computer_segment
;

object_computer_memory:
  MEMORY SIZE _is integer object_char_or_word
  {
	cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
;

object_char_or_word:
  CHARACTERS
| WORDS
;

object_computer_sequence:
  _program coll_sequence _is reference
  {
	current_program->collating_sequence = $4;
  }
;

object_computer_segment:
  SEGMENT_LIMIT _is integer
  {
	/* Ignore */
  }
;


/*
 * REPOSITORY paragraph
 */

repository_paragraph:
  REPOSITORY '.' opt_repository
;

opt_repository:
| repository_list '.'
;

repository_list:
  repository_name
| repository_list repository_name
;

repository_name:
  FUNCTION repository_literal_list INTRINSIC
  {
	current_program->function_spec_list = $2;
  }
| FUNCTION ALL INTRINSIC
  {
	functions_are_all = 1;
  }
;

repository_literal_list:
  LITERAL		{ $$ = cb_list_init ($1); }
| repository_literal_list
  LITERAL		{ $$ = cb_list_add ($1, $2); }
;


/*
 * SPECIAL-NAMES paragraph
 */

special_names_paragraph:
  SPECIAL_NAMES '.' opt_special_names
;

opt_special_names:
| special_name_list /* allow missing terminator dot */
| special_name_list '.'
;

special_name_list:
  special_name
| special_name_list special_name
;

special_name:
  mnemonic_name_clause
| alphabet_name_clause
| symbolic_characters_clause
| locale_clause
| class_name_clause
| currency_sign_clause
| decimal_point_clause
| cursor_clause
| crt_status_clause
| screen_control
| event_status
;


/* Mnemonic name clause */

mnemonic_name_clause:
  WORD _is CRT
  {
	save_tree_1 = lookup_system_name (CB_NAME ($1));
	if (save_tree_1 == cb_error_node) {
		cb_error_x ($1, _("Unknown system-name '%s'"), CB_NAME ($1));
	} else if (CB_SYSTEM_NAME(save_tree_1)->token != CB_DEVICE_CONSOLE) {
		cb_error_x (save_tree_1, _("Invalid CRT clause"));
	}
	/* current_program->flag_screen = 1; */
  }
| WORD _is undefined_word
  {
	save_tree_1 = lookup_system_name (CB_NAME ($1));
	if (save_tree_1 == cb_error_node) {
		cb_error_x ($1, _("Unknown system-name '%s'"), CB_NAME ($1));
	} else {
		cb_define ($3, save_tree_1);
	}
	save_tree_2 = $3;
  }
  special_name_mnemonic_on_off_list
| WORD _is /* omit mnemonic_name */
  {
	save_tree_1 = lookup_system_name (CB_NAME ($1));
	if (save_tree_1 == cb_error_node) {
		cb_error_x ($1, _("Unknown system-name '%s'"), CB_NAME ($1));
	}
	save_tree_2 = NULL;
  }
  special_name_mnemonic_on_off_list_mandatory
| ARGUMENT_NUMBER _is undefined_word
  {
	if (cb_enable_special_names_argument_clause) {
		save_tree_1 = lookup_system_name ("ARGUMENT-NUMBER");
		if (save_tree_1 == cb_error_node) {
			cb_error_x ($1, _("Unknown system-name '%s'"), CB_NAME ($1));
		} else {
			cb_define ($3, save_tree_1);
		}
		save_tree_2 = $3;
	} else {
		cb_error (_("SPECIAL-NAMES with ARGUMENT-NUMBER clause is not yet supported"));
	}
  }
| ARGUMENT_VALUE _is undefined_word
  {
	if (cb_enable_special_names_argument_clause) {
		save_tree_1 = lookup_system_name ("ARGUMENT-VALUE");
		if (save_tree_1 == cb_error_node) {
			cb_error_x ($1, _("Unknown system-name '%s'"), CB_NAME ($1));
		} else {
			cb_define ($3, save_tree_1);
		}
		save_tree_2 = $3;
	} else {
		cb_error (_("SPECIAL-NAMES with ARGUMENT-VALUE clause is not yet supported"));
	}
  }
| ENVIRONMENT_NAME _is undefined_word
  {
	if (cb_enable_special_names_environment_clause) {
		save_tree_1 = lookup_system_name ("ENVIRONMENT-NAME");
		if (save_tree_1 == cb_error_node) {
			cb_error_x ($1, _("Unknown system-name '%s'"), CB_NAME ($1));
		} else {
			cb_define ($3, save_tree_1);
		}
		save_tree_2 = $3;
	} else {
		cb_error (_("SPECIAL-NAMES with ENVIRONMENT-NAME clause is not yet supported"));
	}
  }
| ENVIRONMENT_VALUE _is undefined_word
  {
	if (cb_enable_special_names_environment_clause) {
		save_tree_1 = lookup_system_name ("ENVIRONMENT-VALUE");
		if (save_tree_1 == cb_error_node) {
			cb_error_x ($1, _("Unknown system-name '%s'"), CB_NAME ($1));
		} else {
			cb_define ($3, save_tree_1);
		}
		save_tree_2 = $3;
	} else {
		cb_error (_("SPECIAL-NAMES with ENVIRONMENT-VALUE clause is not yet supported"));
	}
  }
;

special_name_mnemonic_on_off_list:
| special_name_mnemonic_on_off_list special_name_mnemonic_on_off 
;

special_name_mnemonic_on_off_list_mandatory:
  special_name_mnemonic_on_off 
| special_name_mnemonic_on_off_list_mandatory special_name_mnemonic_on_off 
;

special_name_mnemonic_on_off:
  on_or_off _status _is undefined_word
  {
	if (!save_tree_2 && !cb_switch_no_mnemonic) {
		cb_error_x ($4, _("'%s' with no mnemonic name"), CB_NAME ($4));
	} else {
		cb_define_switch_name ($4, save_tree_1, $1, save_tree_2);
	}
  }
;

on_or_off:
  ON				{ $$ = cb_int1; }
| OFF				{ $$ = cb_int0; }
;


/* Alphabet name clause */

alphabet_name_clause:
  ALPHABET undefined_word
  {
	save_tree_1 = $2;
  }
  _is alphabet_definition
  {
	current_program->alphabet_name_list =
		cb_list_add (current_program->alphabet_name_list, $5);
  }
;

alphabet_definition:
  NATIVE	{ $$ = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_NATIVE); }
| STANDARD_1	{ $$ = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_STANDARD_1); }
| STANDARD_2	{ $$ = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_STANDARD_2); }
| EBCDIC	{ $$ = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_EBCDIC); }
| alphabet_literal_list
  {
	$$ = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_CUSTOM);
	CB_ALPHABET_NAME ($$)->custom_list = $1;
  }
;

alphabet_literal_list:
  alphabet_literal		{ $$ = cb_list_init ($1); }
| alphabet_literal_list
  alphabet_literal		{ $$ = cb_list_add ($1, $2); }
;

alphabet_literal:
  alphabet_lits				{ $$ = $1; }
| alphabet_lits THRU alphabet_lits	{ $$ = cb_build_pair ($1, $3); }
| alphabet_lits ALSO
  {
	$$ = cb_list_init ($1);
	save_tree_2 = $$;
  }
  alphabet_also_sequence
  {
	$$ = $3;
  }
;

alphabet_also_sequence:
  alphabet_also_literal
| alphabet_also_sequence ALSO alphabet_also_literal
;

alphabet_lits:
  LITERAL			{ $$ = $1; }
| SPACE				{ $$ = cb_space; }
| ZERO				{ $$ = cb_zero; }
| QUOTE				{ $$ = cb_quote; }
| HIGH_VALUE			{ $$ = cb_norm_high; }
| LOW_VALUE			{ $$ = cb_norm_low; }
;

alphabet_also_literal:
  LITERAL			{ cb_list_add (save_tree_2, $1); }
| SPACE				{ cb_list_add (save_tree_2, cb_space); }
| ZERO				{ cb_list_add (save_tree_2, cb_zero); }
| QUOTE				{ cb_list_add (save_tree_2, cb_quote); }
| HIGH_VALUE			{ cb_list_add (save_tree_2, cb_norm_high); }
| LOW_VALUE			{ cb_list_add (save_tree_2, cb_norm_low); }
;


/* Symbolic characters clause */

symbolic_characters_clause:
  SYMBOLIC _characters symbolic_characters_list
  {
	if ($3) {
		current_program->symbolic_list =
			cb_list_add (current_program->symbolic_list, $3);
	}
	PENDING ("SYMBOLIC CHARACTERS");
  }
;

symbolic_characters_list:
  char_list _is_are integer_list
  {
	if (cb_list_length ($1) != cb_list_length ($3)) {
		cb_error (_("Invalid SYMBOLIC clause"));
		$$ = NULL;
	} else {
		$$ = NULL;
	}
  }
;

char_list:
  undefined_word		{ $$ = cb_list_init ($1); }
| char_list undefined_word	{ $$ = cb_list_add ($1, $2); }
;

integer_list:
  integer			{ $$ = cb_list_init ($1); }
| integer_list integer		{ $$ = cb_list_add ($1, $2); }
;


/* Class name clause */

class_name_clause:
  CLASS undefined_word _is class_item_list
  {
	current_program->class_name_list =
			cb_list_add (current_program->class_name_list,
			cb_build_class_name ($2, $4));
  }
;

class_item_list:
  class_item			{ $$ = cb_list_init ($1); }
| class_item_list class_item	{ $$ = cb_list_add ($1, $2); }
;

class_item:
  basic_value			{ $$ = $1; }
| basic_value THRU basic_value
  {
	/* if (CB_LITERAL ($1)->data[0] < CB_LITERAL ($3)->data[0]) */
	if (literal_value ($1) < literal_value ($3)) {
		$$ = cb_build_pair ($1, $3);
	} else {
		$$ = cb_build_pair ($3, $1);
	}
  }
;

/* LOCALE clause */

locale_clause:
  LOCALE undefined_word _is reference
  {
	cb_tree	l;

	l = cb_build_locale_name ($2, $4);
	if (l != cb_error_node) {
		current_program->locale_list =
			cb_list_add (current_program->locale_list, l);
	}
  }
;

/* CURRENCY SIGN clause */

currency_sign_clause:
  CURRENCY _sign _is LITERAL
  {
	unsigned char *s = CB_LITERAL ($4)->data;

	if (CB_LITERAL ($4)->size != 1) {
		cb_error_x ($4, _("Invalid currency sign '%s'"), s);
	}
	switch (*s) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'a':
	case 'A':
	case 'b':
	case 'B':
	case 'c':
	case 'C':
	case 'd':
	case 'D':
	case 'e':
	case 'E':
	case 'n':
	case 'N':
	case 'p':
	case 'P':
	case 'r':
	case 'R':
	case 's':
	case 'S':
	case 'v':
	case 'V':
	case 'x':
	case 'X':
	case 'z':
	case 'Z':
	case '+':
	case '-':
	case ',':
	case '.':
	case '*':
	case '/':
	case ';':
	case '(':
	case ')':
	case '=':
	case '"':
	case ' ':
		cb_error_x ($4, _("Invalid currency sign '%s'"), s);
		break;
	default:
		break;
	}
	current_program->currency_symbol = s[0];
  }
;


/* DECIMAL-POINT clause */

decimal_point_clause:
  DECIMAL_POINT _is COMMA
  {
	current_program->decimal_point = ',';
	current_program->numeric_separator = '.';
  }
;


/* CURSOR clause */

cursor_clause:
  CURSOR _is reference		{ current_program->cursor_pos = $3; }
;


/* CRT STATUS clause */

crt_status_clause:
  CRT STATUS _is reference	{ current_program->crt_status = $4; }
;


/* SCREEN CONTROL */

screen_control:
  SCREEN_CONTROL _is reference	{  PENDING ("SCREEN CONTROL"); }
;

/* EVENT STATUS */

event_status:
  EVENT_STATUS _is reference	{  PENDING ("EVENT STATUS"); }
;

/*******************
 * INPUT-OUTPUT SECTION
 *******************/

input_output_section:
| INPUT_OUTPUT SECTION '.'
  file_control_paragraph
  i_o_control_paragraph
| FILE_CONTROL '.'
  {
	/* hack for MF compatibility */
	if (cb_relaxed_syntax_check) {
		cb_warning (_("INPUT-OUTPUT SECTION header missing - assumed"));
	} else {
		cb_error (_("INPUT-OUTPUT SECTION header missing"));
	}
  } file_control_sequence
| I_O_CONTROL '.'
  {
	/* hack for MF compatibility */
	if (cb_relaxed_syntax_check) {
		cb_warning (_("INPUT-OUTPUT SECTION header missing - assumed"));
	} else {
		cb_error (_("INPUT-OUTPUT SECTION header missing"));
	}
  } opt_i_o_control
;


/*
 * FILE-CONTROL paragraph
 */

file_control_paragraph:
| FILE_CONTROL '.' file_control_sequence
;

file_control_sequence:
| file_control_sequence file_control_entry
;

file_control_entry:
  SELECT flag_optional undefined_word
  {
	organized_seen = 0;
	if ($3 == cb_error_node) {
		YYERROR;
	}

	/* build new file */
	current_file = build_file ($3);
	current_file->optional = CB_INTEGER ($2)->val;

	/* register the file */
	current_program->file_list =
		cb_cons (CB_TREE (current_file), current_program->file_list);
  }
  select_clause_sequence '.'
  {
	validate_file (current_file, $3);
  }
;

select_clause_sequence:
| select_clause_sequence select_clause
;

select_clause:
  assign_clause
| access_mode_clause
| alternative_record_key_clause
| collating_sequence_clause
| file_status_clause
| lock_mode_clause
| organization_clause
| padding_character_clause
| record_delimiter_clause
| record_key_clause
| relative_key_clause
| reserve_clause
| sharing_clause
| error
| nominal_key_clause
;


/* ASSIGN clause */

assign_clause:
  ASSIGN _to _ext_clause _device assignment_name
  {
	current_file->assign = cb_build_assignment_name (current_file, $5);
  }
| ASSIGN _to _ext_clause DISK
  {
	current_file->fileid_assign = 1;
	current_file->assign = cb_build_assignment_name (current_file, cb_build_reference ("DISK"));
  }
| ASSIGN _to _ext_clause PRINTER
  {
	current_file->fileid_assign = 1;
	current_file->assign = cb_build_assignment_name (current_file, cb_build_reference ("PRINTER"));
  }
;

_device:
| DISK
| PRINTER	{ current_file->organization = COB_ORG_LINE_SEQUENTIAL; }
;

_ext_clause:
| EXTERNAL
  {
	current_file->external_assign = 1;
  }
| DYNAMIC
  {
	current_file->external_assign = 0;
  }
;

assignment_name:
  LITERAL
| DISPLAY
  {
	const char	*s;

	s = "$#@DUMMY@#$";
	$$ = cb_build_alphanumeric_literal ((unsigned char *)s, strlen (s));
  }
| _literal assignment_device_name_list
  {

	if (!$1 || ($1 && CB_TREE_CLASS ($1) == CB_CLASS_NUMERIC)) {
		if ($2) {
			if (CB_CHAIN ($2)) {
				PENDING (_("ASSIGN TO multiple external device names"));
			}
			$$ = CB_VALUE ($2);
		}
	} else {
		if($2) {
			PENDING (_("ASSIGN TO multiple external device names"));
		}
		$$ = $1;
	}
  }
;

assignment_device_name_list:
  qualified_word				{ $$ = cb_list_init ($1); }
| assignment_device_name_list qualified_word 	{ $$ = cb_list_add ($1, $2); }
;

/* ACCESS MODE clause */

access_mode_clause:
  ACCESS _mode _is access_mode
;

access_mode:
  SEQUENTIAL		{ current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
| DYNAMIC		{ current_file->access_mode = COB_ACCESS_DYNAMIC; }
| RANDOM		{ current_file->access_mode = COB_ACCESS_RANDOM; }
;


/* ALTERNATIVE RECORD KEY clause */

alternative_record_key_clause:
  ALTERNATE RECORD _key _is reference flag_duplicates
  {
	struct cb_alt_key *p;
	struct cb_alt_key *l;

	p = cobc_malloc (sizeof (struct cb_alt_key));
	p->key = $5;
	p->duplicates = CB_INTEGER ($6)->val;
	p->component_list = NULL;
	p->next = NULL;

	/* add to the end of list */
	if (current_file->alt_key_list == NULL) {
		current_file->alt_key_list = p;
	} else {
		l = current_file->alt_key_list;
		for (; l->next; l = l->next);
		l->next = p;
	}
  }
| ALTERNATE RECORD _key _is reference key_is_eq split_key_list flag_duplicates
  {
#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM) || defined(WITH_INDEX_EXTFH)
	struct cb_alt_key *p;
	struct cb_alt_key *l;
	cb_tree composite_key;
	struct cb_key_component *comp;

	p = cobc_malloc (sizeof (struct cb_alt_key));
	/* generate field (in w-s) for composite-key */
	if (!$6) {
		/* dialect */
		composite_key = cb_build_field (cb_build_anonymous ());
		comp = cobc_malloc (sizeof (struct cb_key_component));
		comp->next = key_component_list;
		comp->component = $5;
		key_component_list = comp;
	} else {
		/* standard or mf syntax */
		composite_key = cb_build_field ($5);
	}
	if (composite_key == cb_error_node) {
		YYERROR;
	} else {
		composite_key->category = CB_CATEGORY_ALPHANUMERIC;
		((struct cb_field *)composite_key)->count = 1;
		p->key = cb_build_field_reference ((struct cb_field *)composite_key, NULL);
		p->component_list = key_component_list;
		p->duplicates = CB_INTEGER ($8)->val;
		p->next = NULL;

		/* add to the end of list */
		if (current_file->alt_key_list == NULL) {
			current_file->alt_key_list = p;
		} else {
			l = current_file->alt_key_list;
			for (; l->next; l = l->next);
			l->next = p;
		}
	}
#else
	PENDING ("SPLIT KEYS");
#endif
  }
;

split_key_list:
  {
	key_component_list = NULL;
  }
  split_key
| split_key_list split_key
;

split_key:
  reference
  {
	struct cb_key_component *c;
	struct cb_key_component *comp = cobc_malloc (sizeof (struct cb_key_component));
	comp->next = NULL;
	comp->component = $1;
	if (key_component_list == NULL) {
		key_component_list = comp;
	} else {
		for (c = key_component_list; c->next != NULL; c = c->next);
		c->next = comp;
	}
  }
;

key_is_eq:
  /* empty */	{ $$ = NULL; }
| SOURCE _is	{ $$ = cb_int1; }
| '='		{ $$ = cb_int('='); }
;

/* COLLATING SEQUENCE clause */

collating_sequence_clause:
  coll_sequence _is WORD
  {
	PENDING ("COLLATING SEQUENCE");
  }
;


/* FILE STATUS clause */

file_status_clause:
  file_or_sort STATUS _is reference opt_reference
  {
	current_file->file_status = $4;
	if ($5) {
		PENDING ("2nd FILE STATUS");
	}
  }
;

file_or_sort:
  /* empty */
| TOK_FILE
| SORT
;

/* LOCK MODE clause */

lock_mode_clause:
  LOCK _mode _is lock_mode
;

lock_mode:
  MANUAL lock_with	{ current_file->lock_mode = COB_LOCK_MANUAL; } 
| AUTOMATIC lock_with	{ current_file->lock_mode = COB_LOCK_AUTOMATIC; } 
| EXCLUSIVE		{ current_file->lock_mode = COB_LOCK_EXCLUSIVE; }
;

lock_with:
| WITH LOCK ON lock_records
| WITH LOCK ON MULTIPLE lock_records
  {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
| WITH ROLLBACK			{ PENDING ("WITH ROLLBACK"); }
;

lock_records:
  RECORD
| RECORDS
;

/* ORGANIZATION clause */

organization_clause:
  ORGANIZATION _is organization
| organization
;

organization:
  INDEXED
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = COB_ORG_INDEXED;
		organized_seen = 1;
	}
  }
| RECORD _binary SEQUENTIAL
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = COB_ORG_SEQUENTIAL;
		organized_seen = 1;
	}
  }
| SEQUENTIAL
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = cb_default_organization;
		organized_seen = 1;
	}
  }
| RELATIVE
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = COB_ORG_RELATIVE;
		organized_seen = 1;
	}
  }
| LINE SEQUENTIAL
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		organized_seen = 1;
	}
  }
;


/* PADDING CHARACTER clause */

padding_character_clause:
  PADDING _character _is reference_or_literal
  {
	cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
;


/* RECORD DELIMITER clause */

record_delimiter_clause:
  RECORD DELIMITER _is STANDARD_1	{ /* ignored */ }
;


/* RECORD KEY clause */

record_key_clause:
  RECORD _key _is reference
  {
	current_file->key = $4;
  }
| RECORD _key _is reference key_is_eq split_key_list
  {
	/* SPLIT KEY use */
#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM) || defined(WITH_INDEX_EXTFH)

	cb_tree composite_key;	
	struct cb_key_component *comp;

	/* generate field (in w-s) for composite-key */
	if (!$5) {
		/* dialect */
		composite_key = cb_build_field (cb_build_anonymous ());
		comp = cobc_malloc (sizeof (struct cb_key_component));
		comp->next = key_component_list;
		comp->component = $4;
		key_component_list = comp;
	} else {
		/* standard or mf syntax */
		composite_key = cb_build_field ($4);
	}
	if (composite_key == cb_error_node) {
		YYERROR;
	} else {
		composite_key->category = CB_CATEGORY_ALPHANUMERIC;
		((struct cb_field *)composite_key)->count = 1;
		current_file->key = cb_build_field_reference ((struct cb_field *)composite_key, NULL);
		current_file->component_list = key_component_list;
	}
#else
	PENDING ("SPLIT KEYS");
#endif
  }
;


/* RELATIVE KEY clause */

relative_key_clause:
  RELATIVE _key _is reference	{ current_file->key = $4; }
;


/* RESERVE clause */

reserve_clause:
  RESERVE integer _area		{ /* ignored */ }
| RESERVE NO			{ /* ignored */ }
;


/* SHARING clause */

sharing_clause:
  SHARING _with sharing_option	{ current_file->sharing = $3; }
;

sharing_option:
  ALL _other			{ $$ = NULL; PENDING ("SHARING ALL OTHER"); }
| NO _other			{ $$ = cb_int1; }
| READ ONLY			{ $$ = cb_int0; }
;

/* NOMINAL KEY clause */

nominal_key_clause:
  NOMINAL _key _is reference	{ PENDING ("NOMINAL KEY"); }
;

/*
 * I-O-CONTROL paragraph
 */

i_o_control_paragraph:
| I_O_CONTROL '.' opt_i_o_control
;

opt_i_o_control:
| i_o_control_list '.'
| i_o_control_list
| apply_clause_list
;

i_o_control_list:
  i_o_control_clause
| i_o_control_list i_o_control_clause
;

i_o_control_clause:
  same_clause
| multiple_file_tape_clause
;

/* SAME clause */

same_clause:
  SAME same_option _area _for file_name_list
  {
	cb_tree l;

	switch (CB_INTEGER ($2)->val) {
	case 0:
		/* SAME AREA */
		break;
	case 1:
		/* SAME RECORD */
		for (l = $5; l; l = CB_CHAIN (l)) {
			if (CB_VALUE (l) != cb_error_node) {
				CB_FILE (cb_ref (CB_VALUE (l)))->same_clause = samearea;
			}
		}
		samearea++;
		break;
	case 2:
		/* SAME SORT-MERGE */
		break;
	}
  }
;

same_option:
  /* empty */			{ $$ = cb_int0; }
| RECORD			{ $$ = cb_int1; }
| SORT				{ $$ = cb_int2; }
| SORT_MERGE			{ $$ = cb_int2; }
;

/* MULTIPLE FILE TAPE clause */

multiple_file_tape_clause:
  MULTIPLE _file _tape _contains multiple_file_list
  {
	cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
  }
;

multiple_file_list:
  multiple_file
| multiple_file_list multiple_file
;

multiple_file:
  file_name multiple_file_position { }
;

multiple_file_position:
| POSITION integer
;

/* APPLY clause */

apply_clause_list:
  apply_clause_list '.'
| apply_clause
| apply_clause_list apply_clause
;

apply_clause:
  APPLY COMMITMENT_CONTROL _on reference_list
  {
	PENDING ("APPLY COMMITMENT-CONTROL");
  }
| APPLY CYL_OVERFLOW _of LITERAL TRACKS ON reference_list
  {
	PENDING ("APPLY CYL-OVERFLOW");
  }
| APPLY CORE_INDEX TO reference ON reference_list
  {
	PENDING ("APPLY CORE-INDEX");
  }
| APPLY FORMS_OVERLAY TO reference ON reference_list
  {
	PENDING ("APPLY FORMS-OVERLAY");
  }
| APPLY CLOSE_NOFEED ON reference_list
  {
	PENDING ("APPLY CLOSE-NOFEED");
  }
;

/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

data_division:
| DATA DIVISION '.'
  file_section
  working_storage_section
  local_storage_section
  linkage_section
  report_section
  screen_section
;


/*******************
 * FILE SECTION
 *******************/

file_section:
| TOK_FILE SECTION '.'		{ current_storage = CB_STORAGE_FILE; }
  file_description_sequence
| file_type
  {
	/* hack for MF compatibility */
	if (cb_relaxed_syntax_check) {
		cb_warning (_("FILE SECTION header missing - assumed"));
	} else {
		cb_error (_("FILE SECTION header missing"));
	}
	current_storage = CB_STORAGE_FILE;
  }
  file_description_sequence_without_type
;

file_description_sequence:
| file_description_sequence file_description
;

file_description:
  file_type file_description_entry
  record_description_list
  {
	if ($3 && $3 != cb_error_node) {
		finalize_file (current_file, CB_FIELD ($3));
	} else {
		cb_error (_("RECORD description missing or invalid"));
	}
  }
;

file_description_sequence_without_type:
  file_description_entry
  record_description_list
  {
	if ($2 && $2 != cb_error_node) {
		finalize_file (current_file, CB_FIELD ($2));
	} else {
		cb_error (_("RECORD description missing or invalid"));
	}
  }
| file_description_sequence_without_type file_description
;

file_type:
  FD                           { $$ = cb_int0; }
| SD                           { $$ = cb_int1; }
;


/*
 * File description entry
 */

file_description_entry:
  file_name
  {
	if ($1 == cb_error_node) {
		YYERROR;
	}

	current_file = CB_FILE (cb_ref ($1));
	if ($0 == cb_int1) {
		current_file->organization = COB_ORG_SORT;
	}
  }
  file_description_clause_sequence '.'
  {
	/* Shut up bison */
	dummy_tree = $2;
  }
;

file_description_clause_sequence:
| file_description_clause_sequence file_description_clause
;

file_description_clause:
  _is EXTERNAL
  {
	if (current_file->global) {
		cb_error (_("File cannot have both EXTERNAL and GLOBAL clauses"));
	}
	current_file->external = 1;
  }
| _is GLOBAL
  {
	if (current_file->external) {
		cb_error (_("File cannot have both EXTERNAL and GLOBAL clauses"));
	}
	current_file->global = 1;
  }
| block_contains_clause
| record_clause
| label_records_clause
| value_of_clause
| data_records_clause
| linage_clause
| recording_mode_clause
| code_set_clause
| report_clause
| error
;


/* BLOCK CONTAINS clause */

block_contains_clause:
  BLOCK _contains integer opt_to_integer _records_or_characters
  { /* ignored */ }
;

_records_or_characters:	| RECORDS | CHARACTERS ;


/* RECORD clause */

record_clause:
  RECORD _contains integer _characters
  {
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_max = cb_get_int ($3);
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			cb_error (_("RECORD clause invalid"));
		}
	}
  }
| RECORD _contains integer TO integer _characters
  {
	int	error_ind = 0;

	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_min = cb_get_int ($3);
		current_file->record_max = cb_get_int ($5);
		if (current_file->record_min < 0)  {
			current_file->record_min = 0;
			error_ind = 1;
		}
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			error_ind = 1;
		}
		if (current_file->record_max <= current_file->record_min)  {
			error_ind = 1;
		}
		if (error_ind) {
			cb_error (_("RECORD clause invalid"));
		}
	}
  }
| RECORD _is VARYING _in _size opt_from_integer opt_to_integer _characters
  record_depending
  {
	int	error_ind = 0;

	current_file->record_min = $6 ? cb_get_int ($6) : 0;
	current_file->record_max = $7 ? cb_get_int ($7) : 0;
	if ($6 && current_file->record_min < 0)  {
		current_file->record_min = 0;
		error_ind = 1;
	}
	if ($7 && current_file->record_max < 1)  {
		current_file->record_max = 1;
		error_ind = 1;
	}
	if (($6 || $7) && current_file->record_max <= current_file->record_min)  {
		error_ind = 1;
	}
	if (error_ind) {
		cb_error (_("RECORD clause invalid"));
	}
  }
;

record_depending:
| DEPENDING _on reference
  {
	current_file->record_depending = $3;
  }
;

opt_from_integer:
  /* empty */			{ $$ = NULL; }
| _from integer			{ $$ = $2; }
;

opt_to_integer:
  /* empty */			{ $$ = NULL; }
| TO integer			{ $$ = $2; }
;


/* LABEL RECORDS clause */

label_records_clause:
  LABEL records label_option
  {
	cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
;

label_option:
  STANDARD
| OMITTED
;


/* VALUE OF clause */

value_of_clause:
  VALUE OF WORD _is valueof_name
  {
	cb_verify (cb_value_of_clause, "VALUE OF");
  }
| VALUE OF FILE_ID _is valueof_name
  {
	if (!current_file->assign) {
		current_file->assign = cb_build_assignment_name (current_file, $5);
	}
  }
;

valueof_name:
  LITERAL
| qualified_word
;

/* DATA RECORDS clause */

data_records_clause:
  DATA records no_reference_list
  {
	cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
;


/* LINAGE clause */

linage_clause:
  LINAGE _is reference_or_literal _lines
  linage_sequence
  {
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL
	    && current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("LINAGE clause with wrong file type"));
		$$ = cb_error_node;
	} else {
		current_file->linage = $3;
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		if (current_linage == 0) {
			linage_file = current_file;
		}
		current_linage++;
	}
  }
;

linage_sequence:
| linage_sequence linage_lines
;

linage_lines:
  linage_footing
| linage_top
| linage_bottom
;

linage_footing:
  _with FOOTING _at reference_or_literal _lines
  {
	current_file->latfoot = $4;
  }
;

linage_top:
  _at TOP reference_or_literal _lines
  {
	current_file->lattop = $3;
  }
;

linage_bottom:
  _at BOTTOM reference_or_literal
  {
	current_file->latbot = $3;
  }
;


/* RECORDING MODE clause */

recording_mode_clause:
  RECORDING _mode _is WORD	{ /* ignore */ }
;


/* CODE-SET clause */

code_set_clause:
  CODE_SET _is WORD
  {
	if ($3 != cb_error_node) {
		cb_tree x;

		x = cb_ref ($3);
		if (!CB_ALPHABET_NAME_P (x)) {
			cb_error_x ($3, _("Alphabet-name is expected '%s'"), cb_name ($3));
		} else if (CB_ALPHABET_NAME (x)->custom_list) {
			PENDING ("CODE-SET");
		}
	}
  }
;

/* REPORT clause */

report_clause:
  REPORT _is report_name
  {
	cb_warning (_("file descriptor REPORT IS"));
  }
| REPORTS _are report_name
  {
	cb_warning (_("file descriptor REPORTS ARE"));
  }
;


/*******************
 * WORKING-STORAGE SECTION
 *******************/

working_storage_section:
| WORKING_STORAGE SECTION '.'	{ current_storage = CB_STORAGE_WORKING; }
  record_description_list
  {
	if ($5) {
		current_program->working_storage =
			cb_field_add (current_program->working_storage, CB_FIELD ($5));
	}
  }
;

record_description_list:
  /* empty */			{ $$ = NULL; }
| record_description_list_1	{ $$ = $1; }
;

record_description_list_1:
  {
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
  record_description_list_2
  {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	$$ = CB_TREE (description_field);
  }
;

record_description_list_2:
  not_const_word data_description
| record_description_list_2
  not_const_word data_description
| record_description_list_2 '.'
;

data_description:
  constant_entry
| level_number entry_name
  {
	cb_tree x;

	x = cb_build_field_tree ($1, $2, current_field, current_storage, current_file);
	if (x == cb_error_node) {
		YYERROR;
	} else {
		current_field = CB_FIELD (x);
	}
  }
  data_description_clause_sequence _maybe_next_level_number
  {
	if (!qualifier && (current_field->level == 66 || current_field->flag_item_78)) {
		cb_error (_("Item requires a data name"));
	}
	if (current_field->flag_item_78) {
		/* Reset to last non-78 item */
		current_field = cb_validate_78_item (current_field);
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
| level_number_88 entry_name
  {
	cb_tree x;

	x = cb_build_field_tree ($1, $2, current_field, current_storage, current_file);
	if (x == cb_error_node) {
		YYERROR;
	} else {
		current_field = CB_FIELD (x);
	}
  }
  value_cond_clause
  {
	if (!qualifier) {
		cb_error (_("Item requires a data name"));
	}
	cb_validate_88_item (current_field);
	if (!description_field) {
		description_field = current_field;
	}
	
  }
;

level_number:
  LEVEL_NUMBER_WORD
;

level_number_88:
  LEVEL88_NUMBER_WORD
;

/* 
 * Salvage level number of next item may be fell into this ambiguous
 * LITERAL due to missing dot terminator for current item.
 */
_maybe_next_level_number:
  /* empty */
| LITERAL
  {
	if (CB_TREE_CLASS ($1) == CB_CLASS_NUMERIC) {
		cb_tree x = cb_build_reference ((char *)CB_LITERAL($1)->data);
		int lev = cb_get_level (x);
		if (!lev) {
			/* do nothing expecting cb_get_level() had
			 * already given some error message. */
		} else if (lev == 88) {
			cb_unget_token (LEVEL88_NUMBER_WORD, x);
		} else {
			cb_unget_token (LEVEL_NUMBER_WORD, x);		  
		}
	} else {
		/* cause syntax error */
		cb_unget_token (LITERAL, $1);
	}
  }
;

entry_name:
  /* empty */
  {
	$$ = cb_build_filler ();
	qualifier = NULL;
	non_const_word = 0;
  }
| FILLER
  {
	$$ = cb_build_filler ();
	qualifier = NULL;
	non_const_word = 0;
  }
| WORD
  {
	$$ = $1;
	qualifier = $1;
	non_const_word = 0;
  }
;

const_name:
  WORD
  {
	$$ = $1;
	qualifier = $1;
	non_const_word = 0;
  }
;

const_global:
| _is GLOBAL
  {
	current_field->flag_is_global = 1;
	cb_error (_("CONSTANT with GLOBAL clause is not yet supported"));
  }
;

lit_or_length:
  literal			{ $$ = $1; }
| LENGTH _of identifier_1	{ $$ = cb_build_const_length ($3); }
| BYTE_LENGTH _of identifier_1	{ $$ = cb_build_const_length ($3); }
;

constant_entry:
  level_number const_name CONSTANT const_global _as lit_or_length
  {
	cb_tree x;
	int	level;

	level = cb_get_level ($1);
	if (level && level != 1) {
		cb_error (_("CONSTANT item not at 01 level"));
	}
	x = cb_build_constant ($2, $6);
	CB_FIELD (x)->flag_item_78 = 1;
	CB_FIELD (x)->level = 1;
	cb_needs_01 = 1;
	/* Ignore return value */
	cb_validate_78_item (CB_FIELD (x));
  }
;

data_description_clause_sequence:
  /* empty */
  {
	/* required to check redefines */
	$$ = NULL;
  }
| data_description_clause_sequence
  data_description_clause
  {
	/* required to check redefines */
	$$ = cb_true;
  }
;

data_description_clause:
  redefines_clause
| external_clause
| global_clause
| picture_clause
| usage_clause
| sign_clause
| occurs_clause
| justified_clause
| synchronized_clause
| blank_clause
| based_clause
| value_clause
| renames_clause
| any_length_clause
| error
;


/* REDEFINES clause */

redefines_clause:
  REDEFINES identifier_1
  {
	if ($0 != NULL) {
		/* hack for MF compatibility */
		if (cb_relaxed_syntax_check) {
			cb_warning_x ($2, _("REDEFINES clause should follow entry-name"));
		} else {
			cb_error_x ($2, _("REDEFINES clause must follow entry-name"));
		}
	}

	current_field->redefines = cb_resolve_redefines (current_field, $2);
	if (current_field->redefines == NULL) {
		YYERROR;
	}
  }
;


/* EXTERNAL clause */

external_clause:
  _is EXTERNAL as_extname
  {
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error (_("EXTERNAL not allowed here"));
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("EXTERNAL only allowed at 01/77 level"));
	} else if (!qualifier) {
		cb_error (_("EXTERNAL requires a data name"));
	} else if (current_field->flag_is_global) {
		cb_error (_("GLOBAL and EXTERNAL are mutually exclusive"));
	} else if (current_field->flag_item_based) {
		cb_error (_("BASED and EXTERNAL are mutually exclusive"));
	} else if (current_field->redefines) {
		cb_error (_("EXTERNAL and REDEFINES are mutually exclusive"));
	} else {
		current_field->flag_external = 1;
		has_external = 1;
	}
  }
;

as_extname:
  /* empty */			{ current_field->ename = NULL; }
| AS LITERAL
 {
	struct cb_field *x;

	x = CB_FIELD(cb_build_field (cb_build_reference ((char *)(CB_LITERAL ($2)->data))));
	current_field->ename = x->name;
 }
;

/* GLOBAL clause */

global_clause:
  _is GLOBAL
  {
	if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("GLOBAL only allowed at 01/77 level"));
	} else if (!qualifier) {
		cb_error (_("GLOBAL requires a data name"));
	} else if (current_field->flag_external) {
		cb_error (_("GLOBAL and EXTERNAL are mutually exclusive"));
	} else if (current_storage == CB_STORAGE_LOCAL) {
		cb_error (_("GLOBAL not allowed here"));
	} else {
		current_field->flag_is_global = 1;
	}
  }
;


/* PICTURE clause */

picture_clause:
  PICTURE			{ current_field->pic = CB_PICTURE ($1); }
;


/* USAGE clause */

usage_clause:
  usage
| USAGE _is usage
;

usage:
  BINARY			{ current_field->usage = CB_USAGE_BINARY; }
| COMP				{ current_field->usage = CB_USAGE_BINARY; }
| COMP_1			{ current_field->usage = CB_USAGE_FLOAT; }
| COMP_2			{ current_field->usage = CB_USAGE_DOUBLE; }
| COMP_3			{ current_field->usage = CB_USAGE_PACKED; }
| COMP_4			{ current_field->usage = CB_USAGE_BINARY; }
| COMP_5			{ current_field->usage = CB_USAGE_COMP_5; }
| COMP_X			{ current_field->usage = CB_USAGE_COMP_X; }
| DISPLAY			{ current_field->usage = CB_USAGE_DISPLAY; }
| INDEX				{ current_field->usage = CB_USAGE_INDEX; }
| PACKED_DECIMAL		{ current_field->usage = CB_USAGE_PACKED; }
| POINTER
  {
	current_field->usage = CB_USAGE_POINTER;
	current_field->flag_is_pointer = 1;
  }
| PROGRAM_POINTER
  {
	current_field->usage = CB_USAGE_PROGRAM_POINTER;
	current_field->flag_is_pointer = 1;
  }
| SIGNED_SHORT			{ current_field->usage = CB_USAGE_SIGNED_SHORT; }
| SIGNED_INT			{ current_field->usage = CB_USAGE_SIGNED_INT; }
| SIGNED_LONG			{ current_field->usage = CB_USAGE_SIGNED_LONG; }
| UNSIGNED_SHORT		{ current_field->usage = CB_USAGE_UNSIGNED_SHORT; }
| UNSIGNED_INT			{ current_field->usage = CB_USAGE_UNSIGNED_INT; }
| UNSIGNED_LONG			{ current_field->usage = CB_USAGE_UNSIGNED_LONG; }
| BINARY_CHAR SIGNED		{ current_field->usage = CB_USAGE_SIGNED_CHAR; }
| BINARY_CHAR UNSIGNED		{ current_field->usage = CB_USAGE_UNSIGNED_CHAR; }
| BINARY_CHAR			{ current_field->usage = CB_USAGE_SIGNED_CHAR; }
| BINARY_SHORT SIGNED		{ current_field->usage = CB_USAGE_SIGNED_SHORT; }
| BINARY_SHORT UNSIGNED		{ current_field->usage = CB_USAGE_UNSIGNED_SHORT; }
| BINARY_SHORT			{ current_field->usage = CB_USAGE_SIGNED_SHORT; }
| BINARY_LONG SIGNED		{ current_field->usage = CB_USAGE_SIGNED_INT; }
| BINARY_LONG UNSIGNED		{ current_field->usage = CB_USAGE_UNSIGNED_INT; }
| BINARY_LONG			{ current_field->usage = CB_USAGE_SIGNED_INT; }
| BINARY_DOUBLE SIGNED		{ current_field->usage = CB_USAGE_SIGNED_LONG; }
| BINARY_DOUBLE UNSIGNED	{ current_field->usage = CB_USAGE_UNSIGNED_LONG; }
| BINARY_DOUBLE			{ current_field->usage = CB_USAGE_SIGNED_LONG; }
| BINARY_C_LONG SIGNED
  {
	if (sizeof(long) == 4) {
		current_field->usage = CB_USAGE_SIGNED_INT;
	} else {
		current_field->usage = CB_USAGE_SIGNED_LONG;
	}
  }
| BINARY_C_LONG UNSIGNED
  {
	if (sizeof(long) == 4) {
		current_field->usage = CB_USAGE_UNSIGNED_INT;
	} else {
		current_field->usage = CB_USAGE_UNSIGNED_LONG;
	}
  }
| BINARY_C_LONG
  {
	if (sizeof(long) == 4) {
		current_field->usage = CB_USAGE_SIGNED_INT;
	} else {
		current_field->usage = CB_USAGE_SIGNED_LONG;
	}
  }
| NATIONAL			{ PENDING ("USAGE NATIONAL");}
;


/* SIGN clause */

sign_clause:
  _sign_is LEADING flag_separate
  {
	current_field->flag_sign_separate = CB_INTEGER ($3)->val;
	current_field->flag_sign_leading  = 1;
  }
| _sign_is TRAILING flag_separate
  {
	current_field->flag_sign_separate = CB_INTEGER ($3)->val;
	current_field->flag_sign_leading  = 0;
  }
;


/* OCCURS clause */

occurs_key_spec:
| occurs_keys _occurs_indexed
| occurs_indexed _occurs_keys
;

occurs_clause:
  OCCURS integer occurs_to_integer _times
  occurs_depending occurs_key_spec
  {
	if (current_field->occurs_depending && !($3)) {
		cb_verify (cb_odo_without_to, "ODO without TO clause");
	}
	current_field->occurs_min = $3 ? cb_get_int ($2) : 1;
	current_field->occurs_max = $3 ? cb_get_int ($3) : cb_get_int ($2);
	current_field->indexes++;
	if (current_field->indexes > COB_MAX_SUBSCRIPTS) {
		cb_error (_("Maximum OCCURS depth exceeded"));
	}
	current_field->flag_occurs = 1;
  }
;

occurs_to_integer:
  /* empty */			{ $$ = NULL; }
| TO integer			{ $$ = $2; }
;

occurs_depending:
| DEPENDING _on reference
  {
	current_field->occurs_depending = $3;
  }
;

_occurs_keys: | occurs_keys;

occurs_keys:
  occurs_key_list
  {
	if ($1) {
		cb_tree		l;
		struct cb_key	*keys;
		int		i;
		int		nkeys;

		l = $1;
		nkeys = cb_list_length ($1);
		keys = cobc_malloc (sizeof (struct cb_key) * nkeys);

		for (i = 0; i < nkeys; i++) {
			keys[i].dir = CB_PURPOSE_INT (l);
			keys[i].key = CB_VALUE (l);
			l = CB_CHAIN (l);
		}
		current_field->keys = keys;
		current_field->nkeys = nkeys;
	}
  }
;

occurs_key:
  ascending_or_descending _key _is reference_list
  {
	cb_tree l;

	for (l = $4; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = $1;
		if (qualifier && !CB_REFERENCE(CB_VALUE(l))->chain &&
		    strcasecmp (CB_NAME(CB_VALUE(l)), CB_NAME(qualifier))) {
			CB_REFERENCE(CB_VALUE(l))->chain = qualifier;
		}
	}
	$$ = $4;
  }
;

occurs_key_list:
  occurs_key			{ $$ = $1; }
| occurs_key_list occurs_key	{ $$ = cb_list_append ($1, $2); }
;

ascending_or_descending:
  ASCENDING			{ $$ = cb_int (COB_ASCENDING); }
| DESCENDING			{ $$ = cb_int (COB_DESCENDING); }
;

_occurs_indexed: | occurs_indexed;

occurs_indexed:
  INDEXED _by occurs_index_list
  {
	current_field->index_list = $3;
  }
;

occurs_index_list:
  occurs_index			{ $$ = cb_list_init ($1); }
| occurs_index_list
  occurs_index			{ $$ = cb_list_add ($1, $2); }
;

occurs_index:
  WORD
  {
	$$ = cb_build_index ($1, cb_int1, 1, current_field);
  }
;


/* JUSTIFIED clause */

justified_clause:
  JUSTIFIED _right		{ current_field->flag_justified = 1; }
;


/* SYNCHRONIZED clause */

synchronized_clause:
  SYNCHRONIZED left_or_right	{ current_field->flag_synchronized = 1; }
;

left_or_right:
| LEFT
| RIGHT
;


/* BLANK clause */

blank_clause:
  BLANK _when ZERO		{ current_field->flag_blank_zero = 1; }
;


/* BASED clause */

based_clause:
  BASED
  {
	if (current_storage != CB_STORAGE_WORKING &&
	    current_storage != CB_STORAGE_LINKAGE &&
	    current_storage != CB_STORAGE_LOCAL) {
		cb_error (_("BASED not allowed here"));
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("BASED only allowed at 01/77 level"));
	} else if (!qualifier) {
		cb_error (_("BASED requires a data name"));
	} else if (current_field->flag_external) {
		cb_error (_("BASED and EXTERNAL are mutually exclusive"));
	} else if (current_field->redefines) {
		cb_error (_("BASED and REDEFINES are mutually exclusive"));
	} else if (current_field->flag_any_length) {
		cb_error (_("BASED and ANY LENGTH are mutually exclusive"));
	} else {
		current_field->flag_item_based = 1;
	}
  }
;

/* VALUE clause */

value_clause:
VALUE _is literal		{ current_field->values = cb_list_init ($3); }
;

value_cond_clause:
  VALUE _is_are value_item_list	{ current_field->values = $3; }
  _when _set _to false_is
;

value_item_list:
  value_item			{ $$ = cb_list_init ($1); }
| value_item_list value_item	{ $$ = cb_list_add ($1, $2); }
;

value_item:
  literal			{ $$ = $1; }
| literal THRU literal		{ $$ = cb_build_pair ($1, $3); }
;

false_is:
| TOK_FALSE _is literal
  {
	if (current_field->level != 88) {
		cb_error (_("FALSE clause only allowed for 88 level"));
	}
	current_field->false_88 = cb_list_init ($3);
  }
;


/* RENAMES clause */

renames_clause:
  RENAMES qualified_word
  {
	if (cb_ref ($2) != cb_error_node) {
		if (CB_FIELD (cb_ref ($2))->level == 01 ||
		    CB_FIELD (cb_ref ($2))->level > 50) {
			cb_error (_("RENAMES may not reference a level 01 or > 50"));
		} else {
			current_field->redefines = CB_FIELD (cb_ref ($2));
			current_field->pic = current_field->redefines->pic;
		}
	}
  }
| RENAMES qualified_word THRU qualified_word
  {
	if (cb_ref ($2) != cb_error_node && cb_ref ($4) != cb_error_node) {
		if (CB_FIELD (cb_ref ($2))->level == 01 ||
		    CB_FIELD (cb_ref ($2))->level > 50) {
			cb_error (_("RENAMES may not reference a level 01 or > 50"));
		} else if (CB_FIELD (cb_ref ($4))->level == 01 ||
		    CB_FIELD (cb_ref ($4))->level > 50) {
			cb_error (_("RENAMES may not reference a level 01 or > 50"));
		} else {
			current_field->redefines = CB_FIELD (cb_ref ($2));
			current_field->rename_thru = CB_FIELD (cb_ref ($4));
		}
	}
  }
;

/* ANY LENGTH clause */

any_length_clause:
  ANY LENGTH
  {
	if (current_field->flag_item_based) {
		cb_error (_("BASED and ANY LENGTH are mutually exclusive"));
	} else {
		current_field->flag_any_length = 1;
	}
  }
;

/*******************
 * LOCAL-STORAGE SECTION
 *******************/

local_storage_section:
| LOCAL_STORAGE SECTION '.'
  {
	current_storage = CB_STORAGE_LOCAL;
	if (current_program->nested_level) {
		cb_error (_("LOCAL-STORAGE not allowed in nested programs"));
	}
  }
  record_description_list
  {
	if ($5) {
		current_program->local_storage = CB_FIELD ($5);
	}
  }
;


/*******************
 * LINKAGE SECTION
 *******************/

linkage_section:
| LINKAGE SECTION '.'		{ current_storage = CB_STORAGE_LINKAGE; }
  record_description_list
  {
	if ($5) {
		current_program->linkage_storage = CB_FIELD ($5);
	}
  }
;

/*******************
 * REPORT SECTION
 *******************/

report_section:
| REPORT SECTION '.'
  {
	cb_error (_("REPORT SECTION not supported"));
	current_storage = CB_STORAGE_REPORT;
  }
  opt_report_description_list
;

/*******************
 * Optional RD list
 *******************/

opt_report_description_list:
| report_description_list
;

report_description_list:
  report_description_entry
| report_description_list
  report_description_entry
;

/*******************
 * RD report description
 *******************/

report_description_entry:
  RD report_name
  report_description_options '.'
  report_group_description_list 
;

report_description_options:
  /* empty */
  {
	cb_warning (_("Report description using defaults"));
  }
| report_description_options report_description_option
;

report_description_option:
  _is GLOBAL
  {
	cb_error (_("GLOBAL is not allowed with RD"));
  }
| CODE _is id_or_lit
| control_clause
| page_limit_clause
;

/*******************
 * report control breaks
 *******************/
control_clause:
  CONTROL control_field_list
| CONTROLS control_field_list
;

control_field_list:
  _final identifier_list
;

_final:  | FINAL ;
 
identifier_list:
  identifier			{ $$ = cb_list_init ($1); }
| identifier_list identifier	{ $$ = cb_list_add ($1, $2); }
;

/* PAGE clause */

page_limit_clause:
  PAGE _is_are page_line_column
  heading_clause first_detail last_heading last_detail footing_clause
;


heading_clause:
| HEADING _is integer
;

first_detail:
| FIRST DETAIL _is integer
;

last_heading:
| LAST CONTROL_HEADING _is integer
;

last_detail:
| LAST_DETAIL _is integer
;

footing_clause:
| FOOTING _is integer
;

page_line_column:
  integer
| integer line_or_lines integer COLUMNS
| integer line_or_lines
;

line_or_lines:
  LINE
| LINES
;

report_group_description_list: /* empty */
| report_group_description_list report_group_description_entry
; 

report_group_description_entry:
  level_number entry_name   /* might need report_entry_name */
  report_group_options '.'
;

report_group_options: /* empty */
| report_group_options report_group_option
;

report_group_option:
  type_clause { cb_warning (_("looking for Report line TYPE")); }
| next_group_clause
| line_clause
| picture_clause
| usage_clause
| sign_clause
| justified_clause
| column_clause
| blank_clause
| source_clause
| sum_clause_list
| value_clause
| present_when_condition
| group_indicate_clause
| occurs_clause
| varying_clause
;

type_clause:
  TYPE _is type_option
;

type_option:
  REPORT_HEADING
| PAGE_HEADING
| CONTROL_HEADING
| DETAIL
| CONTROL_FOOTING
| PAGE_FOOTING
| REPORT_FOOTING
;

next_group_clause:
  NEXT GROUP _is integer
;

column_clause:
  COLUMN _number _is integer
| COLUMN NUMBERS
| COLUMNS
;

sum_clause_list:
  sum_clause
| sum_clause_list sum_clause
;

sum_clause:
  SUM _of ref_id_exp
;

ref_id_exp:
  reference
/* | identifier */
;

present_when_condition:
  PRESENT WHEN condition
;

varying_clause:
  VARYING identifier FROM x BY x
;

line_clause:
  line_keyword_clause report_line_integer_list
;

line_keyword_clause:
  LINE _numbers _is_are  
| LINES _are
;

report_line_integer_list:
  line_or_plus
| report_line_integer_list line_or_plus
;

line_or_plus:
  PLUS integer
| integer
| NEXT PAGE    /* _on broken here */
;

_numbers:	| NUMBER | NUMBERS ;

source_clause: /* id or exp */
  SOURCE _is identifier flag_rounded
;

group_indicate_clause:
  GROUP _indicate
;

_indicate: | INDICATE ;

report_name:
  WORD 
;

/*******************
 * SCREEN SECTION
 *******************/

screen_section:
| SCREEN SECTION '.'		{ current_storage = CB_STORAGE_SCREEN; }
  {
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
  opt_screen_description_list
  {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	current_program->screen_storage = description_field;
	current_program->flag_screen = 1;
  }
;

opt_screen_description_list:
| screen_description_list
;

screen_description_list:
  screen_description
| screen_description_list
  screen_description
;

screen_description:
  constant_entry
| level_number entry_name
  {
	cb_tree x;

	x = cb_build_field_tree ($1, $2, current_field, current_storage, current_file);
	if (x == cb_error_node) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (current_field->parent) {
		current_field->screen_flag |= current_field->parent->screen_flag;
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
	}
  }
  screen_options '.'
  {
	if (!qualifier && (current_field->level == 88 ||
	    current_field->level == 77 || current_field->level == 66 ||
	    current_field->flag_item_78)) {
		cb_error (_("Item requires a data name"));
	}
	if (current_field->level == 88) {
		cb_validate_88_item (current_field);
	}
	if (current_field->flag_item_78) {
		/* Reset to last non-78 item */
		current_field = cb_validate_78_item (current_field);
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
;

screen_options:
| screen_options screen_option
;

screen_option:
  BLANK_LINE	{ current_field->screen_flag |= COB_SCREEN_BLANK_LINE; }
| BLANK_SCREEN	{ current_field->screen_flag |= COB_SCREEN_BLANK_SCREEN; }
| BELL		{ current_field->screen_flag |= COB_SCREEN_BELL; }
| BLINK		{ current_field->screen_flag |= COB_SCREEN_BLINK; }
| ERASE EOL	{ current_field->screen_flag |= COB_SCREEN_ERASE_EOL; }
| ERASE EOS	{ current_field->screen_flag |= COB_SCREEN_ERASE_EOS; }
| HIGHLIGHT	{ current_field->screen_flag |= COB_SCREEN_HIGHLIGHT; }
| LOWLIGHT	{ current_field->screen_flag |= COB_SCREEN_LOWLIGHT; }
| REVERSE_VIDEO	{ current_field->screen_flag |= COB_SCREEN_REVERSE; }
| UNDERLINE	{ current_field->screen_flag |= COB_SCREEN_UNDERLINE; }
| OVERLINE	{ current_field->screen_flag |= COB_SCREEN_OVERLINE; }
| AUTO		{ current_field->screen_flag |= COB_SCREEN_AUTO; }
| SECURE	{ current_field->screen_flag |= COB_SCREEN_SECURE; }
| REQUIRED	{ current_field->screen_flag |= COB_SCREEN_REQUIRED; }
| FULL		{ current_field->screen_flag |= COB_SCREEN_FULL; }
| PROMPT	{ current_field->screen_flag |= COB_SCREEN_PROMPT; }
| LINE _number _is screen_line_plus_minus num_id_or_lit
  {
	current_field->screen_line = $5;
  }
| COLUMN _number _is screen_col_plus_minus num_id_or_lit
  {
	current_field->screen_column = $5;
  }
| FOREGROUND_COLOR _is num_id_or_lit
  {
	current_field->screen_foreg = $3;
  }
| BACKGROUND_COLOR _is num_id_or_lit
  {
	current_field->screen_backg = $3;
  }
| usage_clause
| blank_clause
| justified_clause
| sign_clause
| value_clause
| picture_clause
| screen_occurs_clause
| USING identifier
  {
	current_field->screen_from = $2;
	current_field->screen_to = $2;
	current_field->screen_flag |= COB_SCREEN_PROMPT;
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
| FROM id_or_lit_or_func
  {
	current_field->screen_from = $2;
  }
| TO identifier
  {
	current_field->screen_to = $2;
	current_field->screen_flag |= COB_SCREEN_PROMPT;
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
;

screen_line_plus_minus:
  /* empty */
  {
	/* Nothing */
  }
| PLUS
  {
	current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  }
| '+'
  {
	current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  }
| MINUS
  {
	current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  }
| '-'
  {
	current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  }
;

screen_col_plus_minus:
  /* empty */
  {
	/* Nothing */
  }
| PLUS
  {
	current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  }
| '+'
  {
	current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  }
| MINUS
  {
	current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  }
| '-'
  {
	current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  }
;


screen_occurs_clause:
  OCCURS integer _times
  {
	current_field->occurs_max = cb_get_int ($2);
	current_field->occurs_min = current_field->occurs_max;
	current_field->indexes++;
	current_field->flag_occurs = 1;
  }
;

/*****************************************************************************
 * PROCEDURE DIVISION
 *****************************************************************************/

procedure_division:
| PROCEDURE DIVISION procedure_using_chaining procedure_returning '.'
  {
	current_section = NULL;
	current_paragraph = NULL;
	cb_define_system_name ("CONSOLE");
	cb_define_system_name ("SYSIN");
	cb_define_system_name ("SYSOUT");
	cb_define_system_name ("SYSERR");
	cb_set_in_procedure ();
  }
  procedure_declaratives
  {
	if (current_program->flag_main && !current_program->flag_chained && $3) {
		cb_error (_("Executable program requested but PROCEDURE/ENTRY has USING clause"));
	}
	emit_entry (current_program->program_id, 0, $3); /* main entry point */
	if (current_program->source_name) {
		emit_entry (current_program->source_name, 1, $3);
	}
  }
  procedure_list
  {
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
  }
;

procedure_using_chaining:
  /* empty */			{ $$ = NULL; }
| USING
  {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
  procedure_param_list		{ $$ = $3; }
| CHAINING
  {
	call_mode = CB_CALL_BY_REFERENCE;
	current_program->flag_chained = 1;
  }
  procedure_param_list		{ $$ = $3; }
;

procedure_param_list:
  procedure_param		{ $$ = $1; }
| procedure_param_list
  procedure_param		{ $$ = cb_list_append ($1, $2); }
;

procedure_param:
  procedure_type size_optional procedure_optional WORD
  {
	$$ = cb_build_pair (cb_int (call_mode), cb_build_identifier ($4));
	CB_SIZES ($$) = size_mode;
  }
;

procedure_type:
  /* empty */
| _by REFERENCE
  {
	call_mode = CB_CALL_BY_REFERENCE;
  }
| _by VALUE
  {
	if (current_program->flag_chained) {
		cb_error (_("BY VALUE not allowed in CHAINED program"));
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
;

size_optional:
  /* empty */
| SIZE _is AUTO
  {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO;
	}
  }
| SIZE _is DEFAULT
  {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_4;
	}
  }
| UNSIGNED SIZE _is integer
  {
	unsigned char *s = CB_LITERAL ($4)->data;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ($4)->size != 1) {
		cb_error_x ($4, _("Invalid value for SIZE"));
	} else {
		size_mode = CB_SIZE_UNSIGNED;
		switch (*s) {
		case '1':
			size_mode |= CB_SIZE_1;
			break;
		case '2':
			size_mode |= CB_SIZE_2;
			break;
		case '4':
			size_mode |= CB_SIZE_4;
			break;
		case '8':
			size_mode |= CB_SIZE_8;
			break;
		default:
			cb_error_x ($4, _("Invalid value for SIZE"));
			break;
		}
	}
  }
| SIZE _is integer
  {
	unsigned char *s = CB_LITERAL ($3)->data;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ($3)->size != 1) {
		cb_error_x ($3, _("Invalid value for SIZE"));
	} else {
		size_mode = 0;
		switch (*s) {
		case '1':
			size_mode = CB_SIZE_1;
			break;
		case '2':
			size_mode = CB_SIZE_2;
			break;
		case '4':
			size_mode = CB_SIZE_4;
			break;
		case '8':
			size_mode = CB_SIZE_8;
			break;
		default:
			cb_error_x ($3, _("Invalid value for SIZE"));
			break;
		}
	}
  }
;

procedure_optional:
  /* empty */
| OPTIONAL
  {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
	}
  }
;

procedure_returning:
  /* empty */
  {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("RETURNING clause is required for a FUNCTION"));
	}
  }
| RETURNING WORD
  {
	if (cb_ref ($2) != cb_error_node) {
		current_program->returning = $2;
		if (cb_field ($2)->storage != CB_STORAGE_LINKAGE) {
			cb_error (_("RETURNING item is not defined in LINKAGE SECTION"));
		}
	}
  }
;

procedure_declaratives:
| DECLARATIVES '.'	{ in_declaratives = 1; }
  procedure_list
  END DECLARATIVES '.'
  {
	in_declaratives = 0;
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		current_paragraph = NULL;
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
		current_section = NULL;
	}
  }
;


/*******************
 * Procedure list
 *******************/

procedure_list:
| procedure_list procedure
;

procedure:
  section_header
| paragraph_header
| invalid_statement
| statements '.'
  {
	if (next_label_list) {
		cb_tree label;
		char name[16];

		sprintf (name, "L$%d", next_label_id);
		label = cb_build_reference (name);
		emit_statement (cb_build_label (label, current_section));
		current_program->label_list =
			cb_list_append (current_program->label_list, next_label_list);
		next_label_list = NULL;
		next_label_id++;
	}
	/* check_unreached = 0; */
  }
| error
  {
	check_unreached = 0;
  }
;


/*******************
 * Section/Paragraph
 *******************/

section_header:
  section_name SECTION opt_segment '.'
  {
	non_const_word = 0;
	check_unreached = 0;
	if ($1 == cb_error_node) {
		YYERROR;
	}

	/* Exit the last section */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}

	/* Begin a new section */
	current_section = CB_LABEL (cb_build_label ($1, NULL));
	current_section->is_section = 1;
	current_paragraph = NULL;
	emit_statement (CB_TREE (current_section));
  }
;

paragraph_header:
  WORD '.'
  {
	cb_tree label;

	non_const_word = 0;
	check_unreached = 0;
	$$ = cb_build_section_name ($1, 1);
	/* if ($1 == cb_error_node) */
	if ($$ == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}

	/* Begin a new paragraph */
	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->is_section = 1;
		emit_statement (CB_TREE (current_section));
	}
	current_paragraph = CB_LABEL (cb_build_label ($$, current_section));
	if (current_section) {
		current_section->children =
			cb_cons (CB_TREE (current_paragraph), current_section->children);
	}
	emit_statement (CB_TREE (current_paragraph));
  }
;

invalid_statement:
  section_name
  {
	non_const_word = 0;
	check_unreached = 0;
	if ($1 != cb_error_node) {
		cb_error_x ($1, _("Unknown statement '%s'"), CB_NAME ($1));
	}
	YYERROR;
  }
;

section_name:
  WORD				{ $$ = cb_build_section_name ($1, 0); }
;

opt_segment:
| LITERAL			{ /* ignore */ }
;


/*******************
 * Statements
 *******************/

statement_list:
  {
	$$ = current_program->exec_list;
	current_program->exec_list = NULL;
  }
  {
	$$ = CB_TREE (current_statement);
	current_statement = NULL;
  }
  statements
  {
	$$ = cb_list_reverse (current_program->exec_list);
	current_program->exec_list = $1;
	current_statement = CB_STATEMENT ($2);
  }
;

statements:
  {
	cb_tree label;

	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->is_section = 1;
		emit_statement (CB_TREE (current_section));
	}
	if (!current_paragraph) {
		label = cb_build_reference ("MAIN PARAGRAPH");
		current_paragraph = CB_LABEL (cb_build_label (label, NULL));
		emit_statement (CB_TREE (current_paragraph));
		current_section->children =
			cb_cons (CB_TREE (current_paragraph), current_section->children);
	}
  }
  /*statement*/
| statements statement
;

statement:
  accept_statement
| add_statement
| allocate_statement
| alter_statement
| call_statement
| cancel_statement
| close_statement
| commit_statement
| compute_statement
| continue_statement
| delete_statement
| delete_file_statement
| display_statement
| divide_statement
| entry_statement
| evaluate_statement
| exit_statement
| free_statement
| generate_statement
| goto_statement
| goback_statement
| if_statement
| initialize_statement
| initiate_statement
| inspect_statement
| merge_statement
| move_statement
| multiply_statement
| open_statement
| perform_statement
| read_statement
| release_statement
| return_statement
| rewrite_statement
| rollback_statement
| search_statement
| set_statement
| sort_statement
| start_statement
| stop_statement
| string_statement
| subtract_statement
| suppress_statement
| terminate_statement
| transform_statement
| unlock_statement
| unstring_statement
| use_statement
| write_statement
| NEXT_SENTENCE
  {
	if (cb_verify (cb_next_sentence_phrase, "NEXT SENTENCE")) {
		cb_tree label;
		char	name[16];

		BEGIN_STATEMENT ("NEXT SENTENCE", 0);
		sprintf (name, "L$%d", next_label_id);
		label = cb_build_reference (name);
		next_label_list = cb_list_add (next_label_list, label);
		emit_statement (cb_build_goto (label, NULL));
	}
	check_unreached = 0;
  }
;


/*
 * ACCEPT statement
 */

accept_statement:
  ACCEPT
  {
	BEGIN_STATEMENT ("ACCEPT", TERM_ACCEPT);
	dispattrs = 0;
	fgc = NULL;
	bgc = NULL;
	scroll = NULL;
  }
  accept_body
  end_accept
;

accept_body:
  identifier opt_at_line_column opt_accp_attr on_accp_exception
  {
	cb_emit_accept ($1, $2, fgc, bgc, scroll, dispattrs);
  }
| identifier FROM ESCAPE KEY
  {
	PENDING ("ACCEPT .. FROM ESCAPE KEY");
  }
| identifier FROM LINES
  {
	cb_emit_accept_line_or_col ($1, 0);
  }
| identifier FROM COLUMNS
  {
	cb_emit_accept_line_or_col ($1, 1);
  }
| identifier FROM DATE
  {
	cb_emit_accept_date ($1);
  }
| identifier FROM DATE YYYYMMDD
  {
	cb_emit_accept_date_yyyymmdd ($1);
  }
| identifier FROM DAY
  {
	cb_emit_accept_day ($1);
  }
| identifier FROM DAY YYYYDDD
  {
	cb_emit_accept_day_yyyyddd ($1);
  }
| identifier FROM DAY_OF_WEEK
  {
	cb_emit_accept_day_of_week ($1);
  }
| identifier FROM TIME
  {
	cb_emit_accept_time ($1);
  }
| identifier FROM COMMAND_LINE
  {
	cb_emit_accept_command_line ($1);
  }
| identifier FROM ENVIRONMENT_VALUE on_accp_exception
  {
	cb_emit_accept_environment ($1);
  }
| identifier FROM ENVIRONMENT simple_value on_accp_exception
  { 
	cb_emit_get_environment ($4, $1);
  }
| identifier FROM ARGUMENT_NUMBER
  {
	cb_emit_accept_arg_number ($1);
  }
| identifier FROM ARGUMENT_VALUE on_accp_exception
  {
	cb_emit_accept_arg_value ($1);
  }
| identifier FROM mnemonic_name
  {
	cb_emit_accept_mnemonic ($1, $3);
  }
| identifier FROM WORD
  {
	cb_emit_accept_name ($1, $3);
  }
;

opt_at_line_column:
  /* empty */			{ $$ = NULL; }
| _at line_number column_number { $$ = cb_build_pair ($2, $3); }
| _at column_number line_number { $$ = cb_build_pair ($3, $2); }
| _at line_number		{ $$ = cb_build_pair ($2, NULL); }
| _at column_number		{ $$ = cb_build_pair (NULL, $2); }
| AT simple_value		{ $$ = $2; }
;

line_number:
  LINE _number id_or_lit	{ $$ = $3; }
;

column_number:
  COLUMN _number id_or_lit	{ $$ = $3; }
| POSITION _number id_or_lit	{ $$ = $3; }
;

opt_accp_attr:
| WITH accp_attrs
;

accp_attrs:
  accp_attr
| accp_attrs accp_attr
;

accp_attr:
  BELL		{ dispattrs |= COB_SCREEN_BELL; }
| BLINK		{ dispattrs |= COB_SCREEN_BLINK; }
| HIGHLIGHT	{ dispattrs |= COB_SCREEN_HIGHLIGHT; }
| LOWLIGHT	{ dispattrs |= COB_SCREEN_LOWLIGHT; }
| REVERSE_VIDEO	{ dispattrs |= COB_SCREEN_REVERSE; }
| UNDERLINE	{ dispattrs |= COB_SCREEN_UNDERLINE; }
| OVERLINE	{ dispattrs |= COB_SCREEN_OVERLINE; }
| FOREGROUND_COLOR _is num_id_or_lit
  {
	fgc = $3;
  }
| BACKGROUND_COLOR _is num_id_or_lit
  {
	bgc = $3;
  }
| SCROLL UP _opt_scroll_lines
  {
	scroll = $3;
  }
| SCROLL DOWN _opt_scroll_lines
  {
	dispattrs |= COB_SCREEN_SCROLL_DOWN;
	scroll = $3;
  }
| AUTO		{ dispattrs |= COB_SCREEN_AUTO; }
| FULL		{ dispattrs |= COB_SCREEN_FULL; }
| REQUIRED	{ dispattrs |= COB_SCREEN_REQUIRED; }
| SECURE	{ dispattrs |= COB_SCREEN_SECURE; }
| UPDATE	{ dispattrs |= COB_SCREEN_UPDATE; }
| PROMPT	{ dispattrs |= COB_SCREEN_PROMPT; }
;

end_accept:
  /* empty */			{ terminator_warning (TERM_ACCEPT); }
| END_ACCEPT			{ terminator_clear (TERM_ACCEPT); }
;


/*
 * ADD statement
 */

add_statement:
  ADD				{ BEGIN_STATEMENT ("ADD", TERM_ADD); }
  add_body
  end_add
;

add_body:
  x_list TO arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($3, '+', cb_build_binary_list ($1, '+'));
  }
| x_list add_to GIVING arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($4, 0, cb_build_binary_list ($1, '+'));
  }
| CORRESPONDING identifier TO identifier flag_rounded on_size_error
  {
	cb_emit_corresponding (cb_build_add, $4, $2, $5);
  }
;

add_to:
| TO x				{ cb_list_add ($0, $2); }
;

end_add:
  /* empty */			{ terminator_warning (TERM_ADD); }
| END_ADD			{ terminator_clear (TERM_ADD); }
;


/*
 * ALLOCATE statement
 */

allocate_statement:
  ALLOCATE			{ BEGIN_STATEMENT ("ALLOCATE", 0); }
  allocate_body
;

allocate_body:
  WORD flag_initialized allocate_returning
  {
	cb_emit_allocate ($1, $3, NULL, $2);
  }
| expr CHARACTERS flag_initialized RETURNING target_x
  {
	cb_emit_allocate (NULL, $5, $1, $3);
  }
;

allocate_returning:
  /* empty */			{ $$ = NULL; }
| RETURNING target_x		{ $$ = $2; }
;


/*
 * ALTER statement
 */

alter_statement:
  ALTER alter_options
  {
	cb_error (_("ALTER statement is obsolete and unsupported"));
  }
;

alter_options:
| alter_options
  procedure_name TO _proceed_to procedure_name
;

_proceed_to:	| PROCEED TO ;


/*
 * CALL statement
 */

call_statement:
  CALL	 			{ BEGIN_STATEMENT ("CALL", TERM_CALL); }
  id_or_lit_or_func call_using call_returning
  call_on_exception call_not_on_exception
  end_call
  {
	cb_emit_call ($3, $4, $5, $6, $7);
  }
;

call_using:
  /* empty */			{ $$ = NULL; }
| USING
  {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
  call_param_list		{ $$ = $3; }
;

call_param_list:
  call_param			{ $$ = $1; }
| call_param_list
  call_param			{ $$ = cb_list_append ($1, $2); }
;

call_param:
  call_type OMITTED
  {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OMITTED only allowed with BY REFERENCE"));
	}
	$$ = cb_build_pair (cb_int (call_mode), cb_null);
  }
| call_type size_optional x
  {
	$$ = cb_build_pair (cb_int (call_mode), $3);
	CB_SIZES ($$) = size_mode;
  }
;

call_type:
  /* empty */
| _by REFERENCE
  {
	call_mode = CB_CALL_BY_REFERENCE;
  }
| _by CONTENT
  {
	if (current_program->flag_chained) {
		cb_error (_("BY CONTENT not allowed in CHAINED program"));
	} else {
		call_mode = CB_CALL_BY_CONTENT;
	}
  }
| _by VALUE
  {
	if (current_program->flag_chained) {
		cb_error (_("BY VALUE not allowed in CHAINED program"));
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
;

call_returning:
  /* empty */			{ $$ = NULL; }
| RETURNING identifier		{ $$ = $2; }
| GIVING identifier		{ $$ = $2; }
;

call_on_exception:
  /* empty */
  {
	$$ = NULL;
  }
| exception_or_overflow
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $3;
  }
;

call_not_on_exception:
  /* empty */
  {
	$$ = NULL;
  }
| not_exception_or_overflow
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $3;
  }
;

end_call:
  /* empty */			{ terminator_warning (TERM_CALL); }
| END_CALL			{ terminator_clear (TERM_CALL); }
;


/*
 * CANCEL statement
 */

cancel_statement:
  CANCEL			{ BEGIN_STATEMENT ("CANCEL", 0); }
  cancel_list
;

cancel_list:
| cancel_list id_or_lit
  {
	cb_emit_cancel ($2);
  }
| ALL
  {
	cb_emit_cancel_all ();
  }
;


/*
 * CLOSE statement
 */

close_statement:
  CLOSE				{ BEGIN_STATEMENT ("CLOSE", 0); }
  close_list
;

close_list:
| close_list
  file_name close_option
  {
	BEGIN_IMPLICIT_STATEMENT ($2);
	if ($2 != cb_error_node) {
		cb_emit_close ($2, $3);
	}
  }
;

close_option:
  /* empty */			{ $$ = cb_int (COB_CLOSE_NORMAL); }
| reel_or_unit			{ $$ = cb_int (COB_CLOSE_UNIT); }
| reel_or_unit _for REMOVAL	{ $$ = cb_int (COB_CLOSE_UNIT_REMOVAL); }
| _with NO REWIND		{ $$ = cb_int (COB_CLOSE_NO_REWIND); }
| _with LOCK			{ $$ = cb_int (COB_CLOSE_LOCK); }
;

reel_or_unit:	REEL | UNIT ;


/*
 * COMPUTE statement
 */

compute_statement:
  COMPUTE			{ BEGIN_STATEMENT ("COMPUTE", TERM_COMPUTE); }
  compute_body
  end_compute
;

compute_body:
  arithmetic_x_list comp_equal expr on_size_error
  {
	cb_emit_arithmetic ($1, 0, $3);
  }
;

end_compute:
  /* empty */			{ terminator_warning (TERM_COMPUTE); }
| END_COMPUTE			{ terminator_clear (TERM_COMPUTE); }
;

comp_equal:	'=' | EQUAL;

/*
 * COMMIT statement
 */

commit_statement:
  COMMIT
  {
	BEGIN_STATEMENT ("COMMIT", 0);
	cb_emit_commit ();
  }
;


/*
 * CONTINUE statement
 */

continue_statement:
  CONTINUE
  {
	BEGIN_STATEMENT ("CONTINUE", 0);
	cb_emit_continue ();
  }
;


/*
 * DELETE statement
 */

delete_statement:
  DELETE			{ BEGIN_STATEMENT ("DELETE", TERM_DELETE); }
  file_name _record opt_invalid_key
  end_delete
  {
	if ($3 != cb_error_node) {
		cb_emit_delete ($3);
	}
  }
;

end_delete:
  /* empty */			{ terminator_warning (TERM_DELETE); }
| END_DELETE			{ terminator_clear (TERM_DELETE); }
;


/*
 * DELETE FILE statement
 */

delete_file_statement:
  DELETE                          { BEGIN_STATEMENT ("DELETE-FILE", 0); }
  TOK_FILE file_name_list
  {
	cb_tree l;
	for (l = $4; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) != cb_error_node) {
			BEGIN_IMPLICIT_STATEMENT (l);
			cb_emit_delete_file (CB_VALUE (l));
		}
	}
  }
;


/*
 * DISPLAY statement
 */

display_statement:
  DISPLAY
  {
	BEGIN_STATEMENT ("DISPLAY", TERM_DISPLAY);
	dispattrs = 0;
	fgc = NULL;
	bgc = NULL;
	scroll = NULL;
  }
  display_body
  end_display
;

display_body:
  id_or_lit UPON_ENVIRONMENT_NAME on_disp_exception
  {
	cb_emit_env_name ($1);
  }
| id_or_lit UPON_ENVIRONMENT_VALUE on_disp_exception
  {
	cb_emit_env_value ($1);
  }
| id_or_lit UPON_ARGUMENT_NUMBER on_disp_exception
  {
	cb_emit_arg_number ($1);
  }
| id_or_lit UPON_COMMAND_LINE on_disp_exception
  {
	cb_emit_command_line ($1);
  }
| x_list opt_at_line_column with_clause on_disp_exception
  {
	cb_emit_display ($1, cb_int0, $3, $2, fgc, bgc, scroll, dispattrs);
  }
| x_list opt_at_line_column UPON mnemonic_name with_clause on_disp_exception
  {
	cb_emit_display_mnemonic ($1, $4, $5, $2, fgc, bgc, scroll, dispattrs);
  }
| x_list opt_at_line_column UPON WORD with_clause on_disp_exception
  {
	cb_tree word = cb_build_display_upon_direct ($4);
	cb_emit_display ($1, word, $5, $2, fgc, bgc, scroll, dispattrs);
  }
| x_list opt_at_line_column UPON PRINTER with_clause on_disp_exception
  {
	cb_emit_display ($1, cb_int0, $5, $2, fgc, bgc, scroll, dispattrs);
  }
| x_list opt_at_line_column UPON CRT with_clause on_disp_exception
  {
	cb_emit_display ($1, cb_int0, $5, $2, fgc, bgc, scroll, dispattrs);
  }
;

with_clause:
  /* empty */			{ $$ = cb_int1; }
| _with NO_ADVANCING		{ $$ = cb_int0; }
| WITH disp_attrs		{ $$ = cb_int1; }
;

disp_attrs:
  disp_attr
| disp_attrs disp_attr
;


disp_attr:
  BELL		{ dispattrs |= COB_SCREEN_BELL; }
| BLINK		{ dispattrs |= COB_SCREEN_BLINK; }
| ERASE EOL	{ dispattrs |= COB_SCREEN_ERASE_EOL; }
| ERASE EOS	{ dispattrs |= COB_SCREEN_ERASE_EOS; }
| HIGHLIGHT	{ dispattrs |= COB_SCREEN_HIGHLIGHT; }
| LOWLIGHT	{ dispattrs |= COB_SCREEN_LOWLIGHT; }
| REVERSE_VIDEO	{ dispattrs |= COB_SCREEN_REVERSE; }
| UNDERLINE	{ dispattrs |= COB_SCREEN_UNDERLINE; }
| OVERLINE	{ dispattrs |= COB_SCREEN_OVERLINE; }
| FOREGROUND_COLOR _is num_id_or_lit
  {
	fgc = $3;
  }
| BACKGROUND_COLOR _is num_id_or_lit
  {
	bgc = $3;
  }
| SCROLL UP _opt_scroll_lines
  {
	scroll = $3;
  }
| SCROLL DOWN _opt_scroll_lines
  {
	dispattrs |= COB_SCREEN_SCROLL_DOWN;
	scroll = $3;
  }
| BLANK_LINE	{ dispattrs |= COB_SCREEN_BLANK_LINE; }
| BLANK_SCREEN	{ dispattrs |= COB_SCREEN_BLANK_SCREEN; }
;

end_display:
  /* empty */			{ terminator_warning (TERM_DISPLAY); }
| END_DISPLAY			{ terminator_clear (TERM_DISPLAY); }
;


/*
 * DIVIDE statement
 */

divide_statement:
  DIVIDE			{ BEGIN_STATEMENT ("DIVIDE", TERM_DIVIDE); }
  divide_body
  end_divide
;

divide_body:
  x INTO arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($3, '/', $1);
  }
| x INTO x GIVING arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($5, 0, cb_build_binary_op ($3, '/', $1));
  }
| x BY x GIVING arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($5, 0, cb_build_binary_op ($1, '/', $3));
  }
| x INTO x GIVING arithmetic_x REMAINDER arithmetic_x on_size_error
  {
	cb_emit_divide ($3, $1, $5, $7);
  }
| x BY x GIVING arithmetic_x REMAINDER arithmetic_x on_size_error
  {
	cb_emit_divide ($1, $3, $5, $7);
  }
;

end_divide:
  /* empty */			{ terminator_warning (TERM_DIVIDE); }
| END_DIVIDE			{ terminator_clear (TERM_DIVIDE); }
;


/*
 * ENTRY statement
 */

entry_statement:
  ENTRY				{ BEGIN_STATEMENT ("ENTRY", 0); }
  LITERAL call_using
  {
	if (current_program->nested_level) {
		cb_error (_("ENTRY is invalid in nested program"));
	} else if (cb_verify (cb_entry_statement, "ENTRY")) {
		if (cobc_check_valid_name ((char *)(CB_LITERAL ($3)->data))) {
			cb_error (_("ENTRY '%s' invalid"), (char *)(CB_LITERAL ($3)->data));
		}
		emit_entry ((char *)(CB_LITERAL ($3)->data), 1, $4);
	}
	check_unreached = 0;
  }
;


/*
 * EVALUATE statement
 */

evaluate_statement:
  EVALUATE
  {
	BEGIN_STATEMENT ("EVALUATE", TERM_EVALUATE);
	eval_level++;
	for (eval_inc = 0; eval_inc < 64; eval_inc++) {
		eval_check[eval_level][eval_inc] = 0;
	}
	eval_inc = 0;
	eval_inc2 = 0;
  }
  evaluate_subject_list evaluate_condition_list
  end_evaluate
  {
	cb_emit_evaluate ($3, $4);
	eval_level--;
  }
;

evaluate_subject_list:
  evaluate_subject		{ $$ = cb_list_init ($1); }
| evaluate_subject_list _also
  evaluate_subject
  {
 	if (!cb_allow_missing_also_clause_in_evaluate && $2 != cb_int1) {
 		cb_error  (_("Invalid expression"));
 	}
 	$$ = cb_list_add ($1, $3);
  }
;

evaluate_subject:
  expr
  {
	$$ = $1;
	if (CB_REFERENCE_P ($1)) {
		eval_check[eval_level][eval_inc++] = 0;
	} else {
		eval_check[eval_level][eval_inc++] = 1;
	}
  }
| TOK_TRUE
  {
	$$ = cb_true;
	eval_check[eval_level][eval_inc++] = 2;
  }
| TOK_FALSE
  {
	$$ = cb_false;
	eval_check[eval_level][eval_inc++] = 3;
  }
;

evaluate_condition_list:
  evaluate_case_list evaluate_other
  {
	$$ = $1;
	if ($2) {
		if (cb_allow_empty_imperative_statement) {
			/*
			 * some compiler implementation allow empty
			 * imperative statements in WHEN phrases, and
			 * treats WHEN OTHER phrase following that
			 * asif the rest part of when_list belonging
			 * to that.
			 */
			cb_tree l, case_item;
			l = $$;
			while (CB_CHAIN (l)) {
				l = CB_CHAIN (l);
			}
			case_item = CB_VALUE (l);
			if (!CB_VALUE (case_item)) {
				 /* warning: duplecates ptr. here */
				CB_VALUE (case_item) = CB_VALUE ($2);
			}
		}
		$$ = cb_list_add ($$, $2);
	}
  }
;

evaluate_case_list:
  evaluate_case			{ $$ = cb_list_init ($1); }
| evaluate_case_list
  evaluate_case			{ $$ = cb_list_add ($1, $2); }
;

evaluate_case:
  evaluate_when_list
  {
	check_unreached = 0;
  }
  statement_list
  {
	if (!cb_allow_empty_imperative_statement && $3 == NULL) {
		cb_error (_("syntax error"));
	}
	$$ = cb_cons ($3, $1);
	eval_inc2 = 0;
  }
;

evaluate_other:
  /* Empty */
  {
	$$ = NULL;
  }
| WHEN_OTHER
  {
	check_unreached = 0;
  }
  statement_list
  {
	if (!cb_allow_empty_imperative_statement && $3 == NULL) {
		cb_error (_("syntax error"));
	}
	$$ = cb_cons ($3, NULL);
	eval_inc2 = 0;
  }
;

evaluate_when_list:
  WHEN evaluate_object_list	{ $$ = cb_list_init ($2); }
| evaluate_when_list
  WHEN evaluate_object_list	{ $$ = cb_list_add ($1, $3); }
;

evaluate_object_list:
  evaluate_object		{ $$ = cb_list_init ($1); }
| evaluate_object_list _also
  evaluate_object
  {
 	if (!cb_allow_missing_also_clause_in_evaluate && $2 != cb_int1) {
 		cb_error  (_("Invalid expression"));
 	}
 	$$ = cb_list_add ($1, $3);
  }
;

evaluate_object:
  partial_expr opt_evaluate_thru_expr
  {
	cb_tree not;
	cb_tree e1;
	cb_tree e2;

	not = cb_int0;
	e2 = $2;
	/* in case the first token is NOT */
	if (CB_PURPOSE_INT ($1) == '!') {
		if (eval_check[eval_level][eval_inc2] < 2) {
			not = cb_int1;
			$1 = CB_CHAIN ($1);
		}
	}

	/* build expr now */
	e1 = cb_build_expr ($1);

	if (e2 == NULL) {
		/* WHEN expr */
		eval_inc2++;
		$$ = cb_build_pair (not, cb_build_pair (e1, NULL));
	} else {
		/* WHEN expr THRU expr */
		$$ = cb_build_pair (not, cb_build_pair (e1, e2));
		eval_inc2++;
	}
  }
| ANY				{ $$ = cb_any; eval_inc2++; }
| TOK_TRUE			{ $$ = cb_true; eval_inc2++; }
| TOK_FALSE			{ $$ = cb_false; eval_inc2++; }
;
opt_evaluate_thru_expr:
  /* empty */			{ $$ = NULL; }
| THRU expr			{ $$ = $2; }
;

end_evaluate:
  /* empty */			{ terminator_warning (TERM_EVALUATE); }
| END_EVALUATE			{ terminator_clear (TERM_EVALUATE); }
;


/*
 * EXIT statement
 */

exit_statement:
  EXIT				{ BEGIN_STATEMENT ("EXIT", 0); }
  exit_body
;

exit_body:
  /* empty */			{ /* nothing */ }
| PROGRAM
  {
	if (in_declaratives && use_global_ind) {
		cb_error (_("EXIT PROGRAM is not allowed within a USE GLOBAL procedure"));
	}
	check_unreached = 1;
	cb_emit_exit (0);
  }
| PERFORM
  {
	struct cb_perform	*p;
	char			name[64];

	if (!perform_stack) {
		cb_error (_("EXIT PERFORM is only valid with inline PERFORM"));
	} else {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->exit_label) {
			sprintf (name, "EXIT PERFORM %d", cb_id);
			p->exit_label = cb_build_reference (name);
			CB_LABEL (cb_build_label (p->exit_label, current_section))->need_begin = 1;
		}
		cb_emit_java_break ();
	}
  }
| PERFORM CYCLE
  {
	struct cb_perform	*p;
	char			name[64];

	if (!perform_stack) {
		cb_error (_("EXIT PERFORM is only valid with inline PERFORM"));
	} else {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->cycle_label) {
			sprintf (name, "EXIT PERFORM CYCLE %d", cb_id);
			p->cycle_label = cb_build_reference (name);
			CB_LABEL (cb_build_label (p->cycle_label, current_section))->need_begin = 1;
		}
		cb_emit_java_continue ();
	}
  }
| SECTION
  {
	cb_tree	plabel;
	char	name[64];

	if (!current_section) {
		cb_error (_("EXIT SECTION is only valid with an active SECTION"));
	} else {
		if (!current_section->exit_label) {
			sprintf (name, "EXIT SECTION %d", cb_id);
			plabel = cb_build_reference (name);
			current_section->exit_label = cb_build_label (plabel, current_section);
			current_section->exit_label_ref = plabel;
			CB_LABEL (current_section->exit_label)->need_begin = 1;
		}
		cb_emit_goto (cb_list_init (current_section->exit_label_ref), NULL);
	}
  }
| PARAGRAPH
  {
	cb_tree	plabel;
	char	name[64];

	if (!current_paragraph) {
		cb_error (_("EXIT PARAGRAPH is only valid with an active PARAGRAPH"));
	} else {
		if (!current_paragraph->exit_label) {
			sprintf (name, "EXIT PARAGRAPH %d", cb_id);
			plabel = cb_build_reference (name);
			current_paragraph->exit_label = cb_build_label (plabel, current_section);
			current_paragraph->exit_label_ref = plabel;
			CB_LABEL (current_paragraph->exit_label)->need_begin = 1;
		}
		cb_emit_goto (cb_list_init (current_paragraph->exit_label_ref), NULL);
	}
  }
;

/*
 * FREE statement
 */

free_statement:
  FREE				{ BEGIN_STATEMENT ("FREE", 0); }
  target_x_list
  {
	cb_emit_free ($3);
  }
;


/*
 * GENERATE statement
 */

generate_statement:
  GENERATE			{ BEGIN_STATEMENT ("GENERATE", 0); }
  identifier
  {
	PENDING("GENERATE");
  }
;


/*
 * GO TO statement
 */

goto_statement:
  GO _to			{ BEGIN_STATEMENT ("GO TO", 0); }
  procedure_name_list goto_depending
  {
	cb_emit_goto ($4, $5);
  }
;

goto_depending:
  /* empty */
  {
	check_unreached = 1;
	$$ = NULL;
  }
| DEPENDING _on identifier
  {
	check_unreached = 0;
	$$ = $3;
  }
;


/*
 * GOBACK statement
 */

goback_statement:
  GOBACK			{ BEGIN_STATEMENT ("GOBACK", 0); }
  {
	check_unreached = 1;
	cb_emit_exit (1);
  }
;


/*
 * IF statement
 */

if_statement:
  IF				{ BEGIN_STATEMENT ("IF", TERM_IF); }
  condition _then
  {
	check_unreached = 0;
  }
  statement_list if_else_sentence
  end_if
  {
	if (!cb_allow_empty_imperative_statement && $6 == NULL) {
		cb_error (_("syntax error"));
	}
	cb_emit_if ($3, $6, $7);
  }
| IF error END_IF
;

if_else_sentence:
  /* empty */
  {
	$$ = NULL;
  }
| ELSE
  {
	check_unreached = 0;
  }
  statement_list
  {
	if (!cb_allow_empty_imperative_statement && $3 == NULL) {
		cb_error (_("syntax error"));
	}
	$$ = $3;
  }
;

end_if:
  /* empty */			{ terminator_warning (TERM_IF); }
| END_IF			{ terminator_clear (TERM_IF); }
;


/*
 * INITIALIZE statement
 */

initialize_statement:
  INITIALIZE			{ BEGIN_STATEMENT ("INITIALIZE", 0); }
  target_x_list initialize_filler initialize_value initialize_replacing initialize_default
  {
	cb_emit_initialize ($3, $4, $5, $6, $7);
  }
;

initialize_filler:
  /* empty */			{ $$ = NULL; }
| _with FILLER			{ $$ = cb_true; }
;

initialize_value:
  /* empty */			{ $$ = NULL; }
| ALL _to VALUE			{ $$ = cb_true; }
| initialize_category _to VALUE	{ $$ = $1; }
;

initialize_replacing:
  /* empty */			{ $$ = NULL; }
| REPLACING
  initialize_replacing_list	{ $$ = $2; }
;

initialize_replacing_list:
  initialize_replacing_item	{ $$ = $1; }
| initialize_replacing_list
  initialize_replacing_item	{ $$ = cb_list_append ($1, $2); }
;

initialize_replacing_item:
  initialize_category _data BY x { $$ = cb_build_pair ($1, $4); }
;

initialize_category:
  ALPHABETIC		{ $$ = cb_int (CB_CATEGORY_ALPHABETIC); }
| ALPHANUMERIC		{ $$ = cb_int (CB_CATEGORY_ALPHANUMERIC); }
| NUMERIC		{ $$ = cb_int (CB_CATEGORY_NUMERIC); }
| ALPHANUMERIC_EDITED	{ $$ = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
| NUMERIC_EDITED	{ $$ = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
| NATIONAL		{ $$ = cb_int (CB_CATEGORY_NATIONAL); }
| NATIONAL_EDITED	{ $$ = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
;

initialize_default:
  /* empty */			{ $$ = NULL; }
| DEFAULT			{ $$ = cb_true; }
;


/*
 * INITIATE statement
 */

initiate_statement:
  INITIATE			{ BEGIN_STATEMENT ("INITIATE", 0); }
  identifier_list
  {
	PENDING("INITIATE");
  }
;

/*
 * INSPECT statement
 */

inspect_statement:
  INSPECT
  {
	BEGIN_STATEMENT ("INSPECT", 0);
	sending_id = 0;
	inspect_keyword = 0;
  }
  send_identifier inspect_list
;

send_identifier:
  identifier			{ save_tree_1 = $1; sending_id = 0; }
| literal			{ save_tree_1 = $1; sending_id = 1; }
| function			{ save_tree_1 = $1; sending_id = 1; }
;

inspect_list:
  inspect_item
| inspect_list inspect_item
;

inspect_item:
  inspect_tallying		{ cb_emit_inspect (save_tree_1, $1, cb_int0, 0); }
| inspect_replacing		{ cb_emit_inspect (save_tree_1, $1, cb_int1, 1); }
| inspect_converting		{ cb_emit_inspect (save_tree_1, $1, cb_int0, 2); }
;

/* INSPECT TALLYING */

inspect_tallying:
  TALLYING			{ cb_init_tarrying (); }
  tallying_list			{ $$ = $3; }
;

tallying_list:
  tallying_item			{ $$ = $1; }
| tallying_list tallying_item	{ $$ = cb_list_append ($1, $2); }
;

tallying_item:
  simple_value FOR		{ $$ = cb_build_tarrying_data ($1); }
| CHARACTERS inspect_region	{ $$ = cb_build_tarrying_characters ($2); }
| ALL				{ $$ = cb_build_tarrying_all (); }
| LEADING			{ $$ = cb_build_tarrying_leading (); }
| TRAILING			{ $$ = cb_build_tarrying_trailing (); }
| simple_value inspect_region	{ $$ = cb_build_tarrying_value ($1, $2); }
;

/* INSPECT REPLACING */

inspect_replacing:
  REPLACING replacing_list	{ $$ = $2; inspect_keyword = 0; }
;

replacing_list:
  replacing_item		{ $$ = $1; }
| replacing_list replacing_item	{ $$ = cb_list_append ($1, $2); }
;

replacing_item:
  CHARACTERS BY simple_value inspect_region
  {
	$$ = cb_build_replacing_characters ($3, $4, save_tree_1);
	inspect_keyword = 0;
  }
| rep_keyword replacing_region		{ $$ = $2; }
;

rep_keyword:
  /* empty */			{ /* Nothing */ }
| ALL				{ inspect_keyword = 1; }
| LEADING			{ inspect_keyword = 2; }
| FIRST				{ inspect_keyword = 3; }
| TRAILING			{ inspect_keyword = 4; }
;

replacing_region:
  simple_value BY simple_all_value inspect_region
  {
	switch (inspect_keyword) {
		case 1:
			$$ = cb_build_replacing_all ($1, $3, $4, save_tree_1);
			break;
		case 2:
			$$ = cb_build_replacing_leading ($1, $3, $4);
			break;
		case 3:
			$$ = cb_build_replacing_first ($1, $3, $4);
			break;
		case 4:
			$$ = cb_build_replacing_trailing ($1, $3, $4);
			break;
		default:
			cb_error (_("INSPECT missing a keyword"));
			$$ = cb_error_node;
			break;
	}
  }
;

/* INSPECT CONVERTING */

inspect_converting:
  CONVERTING simple_value TO simple_all_value inspect_region
  {
	if (cb_validate_inspect (save_tree_1, $2, $4) < 0 ) {
		$$ = cb_error_node;
	} else {
		$$ = cb_build_converting ($2, $4, $5);
	}
  }
;

/* INSPECT BEFORE/AFTER */

inspect_region:
  /* empty */			{ $$ = cb_build_inspect_region_start (); }
| inspect_region
  before_or_after _initial x	{ $$ = cb_build_inspect_region ($1, $2, $4); }
;

_initial:	| TOK_INITIAL ;


/*
 * MERGE statement
 */

merge_statement:
  MERGE				{ BEGIN_STATEMENT ("MERGE", 0); }
  sort_body
;


/*
 * MOVE statement
 */

move_statement:
  MOVE				{ BEGIN_STATEMENT ("MOVE", 0); }
  move_body
;

move_body:
  x TO target_x_list
  {
	cb_emit_move ($1, $3);
  }
| CORRESPONDING x TO target_x_list
  {
	cb_emit_move_corresponding ($2, $4);
  }
;


/*
 * MULTIPLY statement
 */

multiply_statement:
  MULTIPLY			{ BEGIN_STATEMENT ("MULTIPLY", TERM_MULTIPLY); }
  multiply_body
  end_multiply
;

multiply_body:
  x BY arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($3, '*', $1);
  }
| x BY x GIVING arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($5, 0, cb_build_binary_op ($1, '*', $3));
  }
;

end_multiply:
  /* empty */			{ terminator_warning (TERM_MULTIPLY); }
| END_MULTIPLY			{ terminator_clear (TERM_MULTIPLY); }
;


/*
 * OPEN statement
 */

open_statement:
  OPEN				{ BEGIN_STATEMENT ("OPEN", 0); }
  open_list
;

open_list:
| open_list
  open_mode open_sharing file_name_list open_option
  {
	cb_tree l;
	for (l = $4; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) != cb_error_node) {
			BEGIN_IMPLICIT_STATEMENT (l);
			cb_emit_open (CB_VALUE (l), $2, $3);
		}
	}
  }
;

open_mode:
  INPUT				{ $$ = cb_int (COB_OPEN_INPUT); }
| OUTPUT			{ $$ = cb_int (COB_OPEN_OUTPUT); }
| I_O				{ $$ = cb_int (COB_OPEN_I_O); }
| EXTEND			{ $$ = cb_int (COB_OPEN_EXTEND); }
;

open_sharing:
  /* empty */			{ $$ = NULL; }
| SHARING _with sharing_option	{ $$ = $3; }
;

open_option:
  /* empty */			{ $$ = NULL; }
| _with NO REWIND		{ $$ = NULL; }
| _with LOCK			{ PENDING ("OPEN ... WITH LOCK"); }
/*
| _with LOCK			{ $$ = cb_int1; }
*/
;


/*
 * PERFORM statement
 */

perform_statement:
  PERFORM			{ BEGIN_STATEMENT ("PERFORM", TERM_PERFORM); }
  perform_body
;

perform_body:
  perform_procedure perform_option
  {
	cb_emit_perform ($2, $1);
  }
| perform_option
  {
	perform_stack = cb_cons ($1, perform_stack);
	check_unreached = 0;
  }
  statement_list end_perform
  {
	perform_stack = CB_CHAIN (perform_stack);
	cb_emit_perform ($1, $3);
  }
| perform_option END_PERFORM
  {
	cb_emit_perform ($1, NULL);
  }
;

end_perform:
  /* empty */			{ terminator_error (); }
| END_PERFORM			{ terminator_clear (TERM_PERFORM); }
;

perform_procedure:
  procedure_name
  {
	CB_REFERENCE ($1)->length = cb_true; /* return from $1 */
	$$ = cb_build_pair ($1, $1);
  }
| procedure_name THRU procedure_name
  {
	CB_REFERENCE ($3)->length = cb_true; /* return from $3 */
	$$ = cb_build_pair ($1, $3);
  }
;

perform_option:
  /* empty */
  {
	$$ = cb_build_perform_once (NULL);
  }
| FOREVER
  {
	$$ = cb_build_perform_forever (NULL);
  }
| id_or_lit_or_func TIMES
  {
	$$ = cb_build_perform_times ($1);
	current_program->loop_counter++;
  }
| perform_test UNTIL condition
  {
	cb_tree varying;

	varying = cb_list_init (cb_build_perform_varying (NULL, NULL, NULL, $3));
	$$ = cb_build_perform_until ($1, varying);
  }
| perform_test VARYING perform_varying_list
  {
	$$ = cb_build_perform_until ($1, $3);
  }
;

perform_test:
  /* empty */			{ $$ = CB_BEFORE; }
| _with TEST before_or_after	{ $$ = $3; }
;

perform_varying_list:
  perform_varying		{ $$ = cb_list_init ($1); }
| perform_varying_list AFTER
  perform_varying		{ $$ = cb_list_add ($1, $3); }
;

perform_varying:
  identifier FROM x BY x UNTIL condition
  {
	$$ = cb_build_perform_varying ($1, $3, $5, $7);
  }
;


/*
 * READ statements
 */

read_statement:
  READ				{ BEGIN_STATEMENT ("READ", TERM_READ); }
  file_name flag_next _record read_into with_lock read_key read_handler
  end_read
  {
	if ($3 != cb_error_node) {
		if (cb_use_invalidkey_handler_on_status34 &&
		    current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
		    (CB_FILE(cb_ref ($3))->organization != COB_ORG_RELATIVE &&
		     CB_FILE(cb_ref ($3))->organization != COB_ORG_INDEXED)) {
			current_statement->handler_id = COB_EC_I_O_PERMANENT_ERROR;
		}
		if ($7 && (CB_FILE(cb_ref ($3))->lock_mode & COB_LOCK_AUTOMATIC)) {
			cb_error (_("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if ($8 &&
		      (CB_FILE(cb_ref ($3))->organization != COB_ORG_RELATIVE &&
		       CB_FILE(cb_ref ($3))->organization != COB_ORG_INDEXED)) {
			cb_error (_("KEY clause invalid with this file type"));
		} else if (current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
		      (CB_FILE(cb_ref ($3))->organization != COB_ORG_RELATIVE &&
		       CB_FILE(cb_ref ($3))->organization != COB_ORG_INDEXED)) {
			cb_error (_("INVALID KEY clause invalid with this file type"));
		} else {
			cb_emit_read ($3, $4, $6, $8, $7);
		}
	}
  }
;

read_into:
  /* empty */			{ $$ = NULL; }
| INTO identifier		{ $$ = $2; }
;

with_lock:
  /* empty */			{ $$ = NULL; }
| IGNORING LOCK
  {
	$$ = cb_int3;
  }
| _with LOCK
  {
	$$ = cb_int1;
  }
| _with NO LOCK
  {
	$$ = cb_int2;
  }
| _with IGNORE LOCK
  {
	$$ = cb_int3;
  }
| _with WAIT
  {
	$$ = cb_int4;
  }
;

read_key:
  /* empty */			{ $$ = NULL; }
| KEY _is identifier_list
  {
#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM) || defined(WITH_INDEX_EXTFH)
	$$ = $3;
#else
	if (CB_LIST($3)->chain) {
		PENDING ("SPLIT KEYS");
	} else {
		$$ = $3;
	}
#endif
  }
;

read_handler:
| at_end
| invalid_key
;

end_read:
  /* empty */			{ terminator_warning (TERM_READ); }
| END_READ			{ terminator_clear (TERM_READ); }
;


/*
 * RELEASE statement
 */

release_statement:
  RELEASE			{ BEGIN_STATEMENT ("RELEASE", 0); }
  record_name write_from
  {
	if ($3 != cb_error_node) {
		cb_emit_release ($3, $4);
	}
  }
;


/*
 * RETURN statement
 */

return_statement:
  RETURN			{ BEGIN_STATEMENT ("RETURN", TERM_RETURN); }
  file_name _record read_into at_end
  end_return
  {
	if ($3 != cb_error_node) {
		cb_emit_return ($3, $5);
	}
  }
;

end_return:
  /* empty */			{ terminator_warning (TERM_RETURN); }
| END_RETURN			{ terminator_clear (TERM_RETURN); }
;


/*
 * REWRITE statement
 */

rewrite_statement:
  REWRITE			{ BEGIN_STATEMENT ("REWRITE", TERM_REWRITE); }
  record_name write_from write_lock opt_invalid_key
  end_rewrite
  {
	if ($3 != cb_error_node) {
		if (cb_use_invalidkey_handler_on_status34 &&
		    current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
		    (CB_FIELD(cb_ref ($3))->file->organization != COB_ORG_RELATIVE &&
		     CB_FIELD(cb_ref ($3))->file->organization != COB_ORG_INDEXED)) {
			current_statement->handler_id = COB_EC_I_O_PERMANENT_ERROR;
		}
		cb_emit_rewrite ($3, $4, $5);
	}
  }
;

write_lock:
  /* empty */			{ $$ = NULL; }
| _with LOCK
  {
	$$ = cb_int1;
  }
| _with NO LOCK
  {
	$$ = cb_int2;
  }
;

end_rewrite:
  /* empty */			{ terminator_warning (TERM_REWRITE); }
| END_REWRITE			{ terminator_clear (TERM_REWRITE); }
;


/*
 * ROLLBACK statement
 */

rollback_statement:
  ROLLBACK
  {
	BEGIN_STATEMENT ("ROLLBACK", 0);
	cb_emit_rollback ();
  }
;


/*
 * SEARCH statement
 */

search_statement:
  SEARCH			{ BEGIN_STATEMENT ("SEARCH", TERM_SEARCH); }
  search_body
  end_search
;

search_body:
  table_name search_varying search_at_end search_whens
  {
	cb_emit_search ($1, $2, $3, $4);
  }
| ALL table_name search_at_end WHEN expr
  {
	check_unreached = 0;
  }
  statement_list
  {
	cb_emit_search_all ($2, $3, $5, $7);
  }
;

search_varying:
  /* empty */			{ $$ = NULL; }
| VARYING identifier		{ $$ = $2; }
;

search_at_end:
  /* empty */			{ $$ = NULL; }
| _at END
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $4;
  }
;

search_whens:
  search_when			{ $$ = $1; }
| search_when search_whens	{ $$ = $1; CB_IF ($1)->stmt2 = $2; }
;

search_when:
  WHEN condition
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = cb_build_if ($2, $4, NULL);
  }
;

end_search:
  /* empty */			{ terminator_warning (TERM_SEARCH); }
| END_SEARCH			{ terminator_clear (TERM_SEARCH); }
;


/*
 * SET statement
 */

set_statement:
  SET				{ BEGIN_STATEMENT ("SET", 0); }
  set_body
;

set_body:
  set_environment
| set_to
| set_up_down
| set_to_on_off_sequence
| set_to_true_false_sequence
;

/* SET ENVIRONMENT ... TO ... */

set_environment:
  ENVIRONMENT simple_value TO simple_value
  {
	cb_emit_setenv ($2, $4);
  }
;

/* SET name ... TO expr */

set_to:
  target_x_list TO ENTRY alnum_or_id
  {
	cb_emit_set_to ($1, cb_build_ppointer ($4));
  }
| target_x_list TO x
  {
	cb_emit_set_to ($1, $3);
  }
;

/* SET name ... UP/DOWN BY expr */

set_up_down:
  target_x_list up_or_down BY x
  {
	cb_emit_set_up_down ($1, $2, $4);
  }
;

up_or_down:
  UP				{ $$ = cb_int0; }
| DOWN				{ $$ = cb_int1; }
;

/* SET mnemonic-name-1 ... TO ON/OFF */

set_to_on_off_sequence:
  set_to_on_off
| set_to_on_off_sequence set_to_on_off
;

set_to_on_off:
  mnemonic_name_list TO on_or_off
  {
	cb_emit_set_on_off ($1, $3);
  }
;

/* SET condition-name-1 ... TO TRUE/FALSE */

set_to_true_false_sequence:
  set_to_true_false
| set_to_true_false_sequence set_to_true_false
;

set_to_true_false:
  target_x_list TO TOK_TRUE
  {
	cb_emit_set_true ($1);
  }
| target_x_list TO TOK_FALSE
  {
	cb_emit_set_false ($1);
  }
;


/*
 * SORT statement
 */

sort_statement:
  SORT				{ BEGIN_STATEMENT ("SORT", 0); }
  sort_body
;

sort_body:
  qualified_word sort_key_list sort_duplicates sort_collating
  {
	cb_emit_sort_init ($1, $2, $4);
	if (CB_FILE_P (cb_ref ($1)) && $2 == NULL) {
		cb_error (_("File sort requires KEY phrase"));
	}
	/* used in sort_input/sort_output */
	save_tree_1 = $1;
  }
  sort_input sort_output
  {
	cb_emit_sort_finish ($1);
  }
;

sort_key_list:
  /* empty */
  {
	$$ = NULL;
  }
| sort_key_list
  _on ascending_or_descending _key _is opt_key_list
  {
	cb_tree l;

	if (!cb_allow_is_in_sort_key_spec && $5 != NULL) {
		cb_error (_("syntax error"));
		$$ = cb_error_node;
	} else {
		if ($6 == NULL) {
			$6 = cb_list_init (NULL);
		}
		for (l = $6; l; l = CB_CHAIN (l)) {
			CB_PURPOSE (l) = $3;
		}
		$$ = cb_list_append ($1, $6);
	}
  }
;

opt_key_list:
  /* empty */			{ $$ = NULL; }
| opt_key_list qualified_word	{ $$ = cb_list_add ($1, $2); }
;

sort_duplicates:
| with_dups _in_order		{ /* nothing */ }
;

sort_collating:
  /* empty */				{ $$ = cb_null; }
| coll_sequence _is reference		{ $$ = cb_ref ($3); }
;

sort_input:
  /* empty */
  {
	if (CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("File sort requires USING or INPUT PROCEDURE"));
	}
  }
| USING file_name_list
  {
	if (!CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("USING invalid with table SORT"));
	} else {
		cb_emit_sort_using (save_tree_1, $2);
	}
  }
| INPUT PROCEDURE _is perform_procedure
  {
	if (!CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("INPUT PROCEDURE invalid with table SORT"));
	} else {
		cb_emit_sort_input ($4, save_tree_1);
	}
  }
;

sort_output:
  /* empty */
  {
	if (CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("File sort requires GIVING or OUTPUT PROCEDURE"));
	}
  }
| GIVING file_name_list
  {
	if (!CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("GIVING invalid with table SORT"));
	} else {
		cb_emit_sort_giving (save_tree_1, $2);
	}
  }
| OUTPUT PROCEDURE _is perform_procedure
  {
	if (!CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
	} else {
		cb_emit_sort_output ($4, save_tree_1);
	}
  }
;


/*
 * START statement
 */

start_statement:
  START				{ BEGIN_STATEMENT ("START", TERM_START); }
  file_name			{ $$ = cb_int (COB_EQ); }
  start_key opt_invalid_key
  end_start
  {
	if (CB_FILE_P (cb_ref ($3))) {
		if (CB_FILE (cb_ref ($3))->organization != COB_ORG_INDEXED &&
		     CB_FILE (cb_ref ($3))->organization != COB_ORG_RELATIVE) {
			cb_error (_("START not allowed on SEQUENTIAL files"));
			$$ = cb_error_node;
		} else {
			cb_emit_start ($3, $4, $5);
		}
	} else {
		cb_error_x ($3, _("'%s' is not a file name"), CB_NAME ($3));
		$$ = cb_error_node;
	}
  }
;

start_key:
  /* empty */			{ $$ = NULL; }
| KEY _is start_op identifier_list
  {
	$0 = $3;
#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM) || defined(WITH_INDEX_EXTFH)
	$$ = $4;
#else
	if (CB_LIST($4)->chain) {
		PENDING ("SPLIT KEYS");
	} else {
		$$ = $4;
 	}
#endif
  }
;

start_op:
  flag_not eq		{ $$ = cb_int (($1 == cb_int1) ? COB_NE : COB_EQ); }
| flag_not gt		{ $$ = cb_int (($1 == cb_int1) ? COB_LE : COB_GT); }
| flag_not lt		{ $$ = cb_int (($1 == cb_int1) ? COB_GE : COB_LT); }
| flag_not ge		{ $$ = cb_int (($1 == cb_int1) ? COB_LT : COB_GE); }
| flag_not le		{ $$ = cb_int (($1 == cb_int1) ? COB_GT : COB_LE); }
;

end_start:
  /* empty */			{ terminator_warning (TERM_START); }
| END_START			{ terminator_clear (TERM_START); }
;


/*
 * STOP statement
 */

stop_statement:
  STOP RUN			{ BEGIN_STATEMENT ("STOP", 0); }
  stop_returning
  {
	cb_emit_stop_run ($4);
  }
| STOP LITERAL			{ BEGIN_STATEMENT ("STOP", 0); }
  {
	cb_verify (cb_stop_literal_statement, "STOP literal");
  }
;

stop_returning:
  /* empty */		{ $$ = current_program->cb_return_code; }
| RETURNING x		{ $$ = $2; }
| GIVING x		{ $$ = $2; }
;


/*
 * STRING statement
 */

string_statement:
  STRING			{ BEGIN_STATEMENT ("STRING", TERM_STRING); }
  string_item_list INTO identifier opt_with_pointer on_overflow
  end_string
  {
	cb_emit_string ($3, $5, $6);
  }
;

string_item_list:
  string_item			{ $$ = cb_list_init ($1); }
| string_item_list string_item	{ $$ = cb_list_add ($1, $2); }
;

string_item:
  x				{ $$ = $1; }
| DELIMITED _by SIZE		{ $$ = cb_build_pair (cb_int0, NULL); }
| DELIMITED _by x		{ $$ = cb_build_pair ($3, NULL); }
;

opt_with_pointer:
  /* empty */			{ $$ = cb_int0; }
| _with POINTER identifier	{ $$ = $3; }
;

end_string:
  /* empty */			{ terminator_warning (TERM_STRING); }
| END_STRING			{ terminator_clear (TERM_STRING); }
;


/*
 * SUBTRACT statement
 */

subtract_statement:
  SUBTRACT			{ BEGIN_STATEMENT ("SUBTRACT", TERM_SUBTRACT); }
  subtract_body
  end_subtract
;

subtract_body:
  x_list FROM arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($3, '-', cb_build_binary_list ($1, '+'));
  }
| x_list FROM x GIVING arithmetic_x_list on_size_error
  {
	cb_emit_arithmetic ($5, 0, cb_build_binary_list (cb_cons ($3, $1), '-'));
  }
| CORRESPONDING identifier FROM identifier flag_rounded on_size_error
  {
	cb_emit_corresponding (cb_build_sub, $4, $2, $5);
  }
;

end_subtract:
  /* empty */			{ terminator_warning (TERM_SUBTRACT); }
| END_SUBTRACT			{ terminator_clear (TERM_SUBTRACT); }
;


/*
 * SUPPRESS statement
 */

suppress_statement:
  SUPPRESS _printing
  {
	BEGIN_STATEMENT ("SUPPRESS", 0);
	PENDING("SUPPRESS");
  }
;

_printing:
| PRINTING
;

/*
 * TERMINATE statement
 */

terminate_statement:
  TERMINATE			{ BEGIN_STATEMENT ("TERMINATE", 0); }
  identifier_list
  {
	PENDING("TERMINATE");
  }
;


/*
 * TRANSFORM statement
 */

transform_statement:
  TRANSFORM			{ BEGIN_STATEMENT ("TRANSFORM", 0); }
  identifier FROM simple_value TO simple_all_value
  {
	cb_tree		x;

	x = cb_build_converting ($5, $7, cb_build_inspect_region_start ());
	cb_emit_inspect ($3, x, cb_int0, 2);
  }
;


/*
 * UNLOCK statement
 */

unlock_statement:
  UNLOCK			{ BEGIN_STATEMENT ("UNLOCK", 0); }
  file_name opt_record
  {
	if ($3 != cb_error_node) {
		cb_emit_unlock ($3);
	}
  }
;

opt_record:
  /* empty */
| RECORD
| RECORDS
;


/*
 * UNSTRING statement
 */

unstring_statement:
  UNSTRING			{ BEGIN_STATEMENT ("UNSTRING", TERM_UNSTRING); }
  identifier unstring_delimited unstring_into
  opt_with_pointer unstring_tallying on_overflow
  end_unstring
  {
	cb_emit_unstring ($3, $4, $5, $6, $7);
  }
;

unstring_delimited:
  /* empty */			{ $$ = NULL; }
| DELIMITED _by
  unstring_delimited_list	{ $$ = $3; }
;

unstring_delimited_list:
  unstring_delimited_item	{ $$ = cb_list_init ($1); }
| unstring_delimited_list OR
  unstring_delimited_item	{ $$ = cb_list_add ($1, $3); }
;

unstring_delimited_item:
  flag_all simple_value
  {
	$$ = cb_build_unstring_delimited ($1, $2);
  }
;

unstring_into:
  INTO unstring_into_item	{ $$ = cb_list_init ($2); }
| unstring_into
  unstring_into_item		{ $$ = cb_list_add ($1, $2); }
;

unstring_into_item:
  identifier unstring_into_delimiter unstring_into_count
  {
	$$ = cb_build_unstring_into ($1, $2, $3);
  }
;

unstring_into_delimiter:
  /* empty */			{ $$ = NULL; }
| DELIMITER _in identifier	{ $$ = $3; }
;

unstring_into_count:
  /* empty */			{ $$ = NULL; }
| COUNT _in identifier		{ $$ = $3; }
;

unstring_tallying:
  /* empty */			{ $$ = NULL; }
| TALLYING _in identifier	{ $$ = $3; }
;

end_unstring:
  /* empty */			{ terminator_warning (TERM_UNSTRING); }
| END_UNSTRING			{ terminator_clear (TERM_UNSTRING); }
;


/*
 * USE statement
 */

use_statement:
  use_exception
| use_debugging
| use_reporting
;

use_exception:
  USE
  use_global _after _standard exception_or_error _procedure
  _on use_exception_target
  {
	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (!current_section) {
		cb_error (_("SECTION header missing before USE statement"));
	} else {
		current_section->need_begin = 1;
		current_section->need_return = 1;
		CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;
		if (use_global_ind) {
			current_section->is_global = 1;
			current_program->global_list =
				cb_list_add (current_program->global_list,
					     CB_TREE (current_section));
		}
	}
  }
;

use_global:
  /* empty */
  {
	use_global_ind = 0;
  }
| GLOBAL
  {
	use_global_ind = 1;
	current_program->flag_global_use = 1;
  }
;

use_exception_target:
  file_name_list
  {
	cb_tree		l;

	for (l = $1; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) != cb_error_node) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
  }
| INPUT	
  {
	current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  }
| OUTPUT
  {
	current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  }
| I_O	
  {
	current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
	current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  }
| EXTEND
  {
	current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
	current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  }
;

_after:
| AFTER
;

_standard:
| STANDARD
;

exception_or_error:
  EXCEPTION | ERROR
;

exception_or_overflow:
  EXCEPTION | OVERFLOW
;

not_exception_or_overflow:
  NOT_EXCEPTION | NOT_OVERFLOW
;

_procedure:
| PROCEDURE
;

use_debugging:
  USE _for DEBUGGING _on use_debugging_target
  {
	PENDING ("USE FOR DEBUGGING");
  }
;

use_debugging_target:
  procedure_name
| ALL PROCEDURES
;

use_reporting:
  USE use_global BEFORE REPORTING identifier
  {
	PENDING ("USE BEFORE REPORTING");
  }
; 


/*
 * WRITE statement
 */

write_statement:
  WRITE				{ BEGIN_STATEMENT ("WRITE", TERM_WRITE); }
  record_name write_from write_lock write_option write_handler
  end_write
  {
	if ($3 != cb_error_node) {
		if (cb_use_invalidkey_handler_on_status34 &&
		    current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
		    (CB_FIELD(cb_ref ($3))->file->organization != COB_ORG_RELATIVE &&
		     CB_FIELD(cb_ref ($3))->file->organization != COB_ORG_INDEXED)) {
			current_statement->handler_id = COB_EC_I_O_PERMANENT_ERROR;
		}
		cb_emit_write ($3, $4, $6, $5);
	}
  }
;

write_from:
  /* empty */			{ $$ = NULL; }
| FROM id_or_lit		{ $$ = $2; }
;

write_option:
  /* empty */
  {
	$$ = cb_int0;
  }
| before_or_after _advancing num_id_or_lit _line_or_lines
  {
	$$ = cb_build_write_advancing_lines ($1, $3);
  }
| before_or_after _advancing mnemonic_name
  {
	$$ = cb_build_write_advancing_mnemonic ($1, $3);
  }
| before_or_after _advancing PAGE
  {
	$$ = cb_build_write_advancing_page ($1);
  }
;

before_or_after:
  BEFORE			{ $$ = CB_BEFORE; }
| AFTER				{ $$ = CB_AFTER; }
;

write_handler:
| at_eop
| invalid_key
;

end_write:
  /* empty */			{ terminator_warning (TERM_WRITE); }
| END_WRITE			{ terminator_clear (TERM_WRITE); }
;


/*******************
 * Status handlers
 *******************/

/*
 * ON EXCEPTION
 */

on_accp_exception:
  opt_on_exception
  opt_not_on_exception
  {
	current_statement->handler_id = COB_EC_IMP_ACCEPT;
  }
;

on_disp_exception:
  opt_on_exception
  opt_not_on_exception
  {
	current_statement->handler_id = COB_EC_IMP_DISPLAY;
  }
;

opt_on_exception:
| EXCEPTION
  {
	check_unreached = 0;
  }
  statement_list
  {
	current_statement->handler1 = $3;
  }
;

opt_not_on_exception:
| NOT_EXCEPTION
  {
	check_unreached = 0;
  }
  statement_list
  {
	current_statement->handler2 = $3;
  }
;

/*
 * ON SIZE ERROR
 */

on_size_error:
  opt_on_size_error
  opt_not_on_size_error
;

opt_on_size_error:
| SIZE_ERROR
  {
	check_unreached = 0;
	current_statement->handler_id = COB_EC_SIZE;
  }
  statement_list
  {
	current_statement->handler1 = $3;
  }
;

opt_not_on_size_error:
| NOT_SIZE_ERROR
  {
	check_unreached = 0;
	current_statement->handler_id = COB_EC_SIZE;
  }
  statement_list
  {
	current_statement->handler2 = $3;
  }
;


/*
 * ON OVERFLOW
 */

on_overflow:
  opt_on_overflow opt_not_on_overflow
  {
	current_statement->handler_id = COB_EC_OVERFLOW;
  }
;

opt_on_overflow:
| OVERFLOW
  {
	check_unreached = 0;
  }
  statement_list
  {
	current_statement->handler1 = $3;
  }
;

opt_not_on_overflow:
| NOT_OVERFLOW
  {
	check_unreached = 0;
  }
  statement_list
  {
	current_statement->handler2 = $3;
  }
;


/*
 * AT END
 */

at_end:
  at_end_sentence
  {
	current_statement->handler_id = COB_EC_I_O_AT_END;
	current_statement->handler1 = $1;
  }
| not_at_end_sentence
  {
	current_statement->handler_id = COB_EC_I_O_AT_END;
	current_statement->handler2 = $1;
  }
| at_end_sentence not_at_end_sentence
  {
	current_statement->handler_id = COB_EC_I_O_AT_END;
	current_statement->handler1 = $1;
	current_statement->handler2 = $2;
  }
;

at_end_sentence:
  END
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $3;
  }
;

not_at_end_sentence:
  NOT_END
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $3;
  }
;


/*
 * AT EOP
 */

at_eop:
  at_eop_sentence
  {
	current_statement->handler_id = COB_EC_I_O_EOP;
	current_statement->handler1 = $1;
  }
| not_at_eop_sentence
  {
	current_statement->handler_id = COB_EC_I_O_EOP;
	current_statement->handler2 = $1;
  }
| at_eop_sentence not_at_eop_sentence
  {
	current_statement->handler_id = COB_EC_I_O_EOP;
	current_statement->handler1 = $1;
	current_statement->handler2 = $2;
  }
;

at_eop_sentence:
  EOP
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $3;
  }
;

not_at_eop_sentence:
  NOT_EOP
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $3;
  }
;


/*
 * INVALID KEY
 */

opt_invalid_key:
| invalid_key
;

invalid_key:
  invalid_key_sentence
  {
	current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
	current_statement->handler1 = $1;
  }
| not_invalid_key_sentence
  {
	current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
	current_statement->handler2 = $1;
  }
| invalid_key_sentence not_invalid_key_sentence
  {
	current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
	current_statement->handler1 = $1;
	current_statement->handler2 = $2;
  }
;

invalid_key_sentence:
  INVALID_KEY
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $3;
  }
;

not_invalid_key_sentence:
  NOT_INVALID_KEY
  {
	check_unreached = 0;
  }
  statement_list
  {
	$$ = $3;
  }
;


/*****************************************************************************
 * Common Constructs
 *****************************************************************************/

_opt_scroll_lines:
  /* empty */
  {
	$$ = cb_one;
  }
| _by num_id_or_lit _line_or_lines
  {
	$$ = $2;
  }
;


/*******************
 * Expressions
 *******************/

condition:
  expr
  {
	$$ = cb_build_cond ($1);
  }
;

expr:
  partial_expr
  {
	$$ = cb_build_expr ($1);
  }
;

partial_expr:
  {
	current_expr = NULL;
  }
  expr_tokens
  {
	$$ = cb_list_reverse (current_expr);
  }
;

expr_tokens:
  expr_token x		{ push_expr ('x', $2); }
| expr_tokens ')'	{ push_expr (')', NULL); }
/* class condition */
| expr_token OMITTED		{ push_expr ('O', NULL); }
| expr_token NUMERIC		{ push_expr ('9', NULL); }
| expr_token ALPHABETIC		{ push_expr ('A', NULL); }
| expr_token ALPHABETIC_LOWER	{ push_expr ('L', NULL); }
| expr_token ALPHABETIC_UPPER	{ push_expr ('U', NULL); }
| expr_token CLASS_NAME		{ push_expr ('x', $2); }
/* class condition (IS is omitted) */
| expr_tokens OMITTED			{ push_expr ('O', NULL); }
| expr_tokens NUMERIC			{ push_expr ('9', NULL); }
| expr_tokens ALPHABETIC		{ push_expr ('A', NULL); }
| expr_tokens ALPHABETIC_LOWER		{ push_expr ('L', NULL); }
| expr_tokens ALPHABETIC_UPPER		{ push_expr ('U', NULL); }
| expr_tokens CLASS_NAME		{ push_expr ('x', $2); }
/* sign condition */
| expr_token POSITIVE	{ push_expr ('P', NULL); }
| expr_token NEGATIVE	{ push_expr ('N', NULL); }
/* sign condition (IS is omitted) */
| expr_tokens POSITIVE	{ push_expr ('P', NULL); }
| expr_tokens NEGATIVE	{ push_expr ('N', NULL); }
| expr_tokens ZERO	{ push_expr ('x', cb_zero); }
;

expr_token:
  /* empty */
| expr_tokens IS
| expr_token IS	/*abbrev.*/
| expr_token '('	{ push_expr ('(', NULL); }
/* arithmetic operators (unary) */
| expr_token '+'	{ push_expr ('+', NULL); }
| expr_token '-'	{ push_expr ('-', NULL); }
| expr_token '^'	{ push_expr ('^', NULL); }
/* logical operators (unary/complex) */
| expr_token NOT	{ push_expr ('!', NULL); }
| expr_tokens NOT	{ push_expr ('!', NULL); }
/* arithmetic operators (binary) */
| expr_tokens '+'	{ push_expr ('+', NULL); }
| expr_tokens '-'	{ push_expr ('-', NULL); }
| expr_tokens '*'	{ push_expr ('*', NULL); }
| expr_tokens '/'	{ push_expr ('/', NULL); }
| expr_tokens '^'	{ push_expr ('^', NULL); }
/* conditional operators (binary) */
| expr_tokens eq	{ push_expr ('=', NULL); }
| expr_tokens gt	{ push_expr ('>', NULL); }
| expr_tokens lt	{ push_expr ('<', NULL); }
| expr_tokens ge	{ push_expr (']', NULL); }
| expr_tokens le	{ push_expr ('[', NULL); }
| expr_tokens NE	{ push_expr ('~', NULL); }
/* conditional operators (abbrev.) */
| expr_token eq		{ push_expr ('=', NULL); }
| expr_token gt		{ push_expr ('>', NULL); }
| expr_token lt		{ push_expr ('<', NULL); }
| expr_token ge		{ push_expr (']', NULL); }
| expr_token le		{ push_expr ('[', NULL); }
| expr_token NE		{ push_expr ('~', NULL); }
/* logical operators (binary) */
| expr_tokens AND	{ push_expr ('&', NULL); }
| expr_tokens OR	{ push_expr ('|', NULL); }
;

eq:	'=' | EQUAL _to | EQUALS;
gt:	'>' | GREATER _than ;
lt:	'<' | LESS _than ;
ge:	GE | GREATER _than OR EQUAL _to | GREATER _than EQUAL _to ;
le:	LE | LESS _than OR EQUAL _to | LESS _than EQUAL _to ;

/* Arithmetic expression */

exp_list:
  exp				{ $$ = cb_list_init ($1); }
| exp_list e_sep exp		{ $$ = cb_list_add ($1, $3); }
;

e_sep:
| COMMA_DELIM
| SEMI_COLON
;

exp:
  arith_x			{ $$ = $1; }
| exp '+' exp			{ $$ = cb_build_binary_op ($1, '+', $3); }
| exp '-' exp			{ $$ = cb_build_binary_op ($1, '-', $3); }
| exp '*' exp			{ $$ = cb_build_binary_op ($1, '*', $3); }
| exp '/' exp			{ $$ = cb_build_binary_op ($1, '/', $3); }
| '+' exp  %prec UNARY_SIGN	{ $$ = $2; }
| '-' exp  %prec UNARY_SIGN	{ $$ = cb_build_binary_op (cb_zero, '-', $2); }
| exp '^' exp			{ $$ = cb_build_binary_op ($1, '^', $3); }
| '(' exp ')'			{ $$ = $2; }
;


/*******************
 * Names
 *******************/

/* LINAGE-COUNTER */

linage_counter:
  LINAGE_COUNTER
  {
	if (current_linage > 1) {
		cb_error (_("LINAGE-COUNTER must be qualified here"));
		$$ = cb_error_node;
	} else if (current_linage == 0) {
		cb_error (_("Invalid LINAGE-COUNTER usage"));
		$$ = cb_error_node;
	} else {
		$$ = linage_file->linage_ctr;
	}
  }
|  LINAGE_COUNTER in_of WORD
  {
	if (CB_FILE_P (cb_ref ($3))) {
		$$ = CB_FILE (cb_ref ($3))->linage_ctr;
	} else {
		cb_error_x ($3, _("'%s' is not a file name"), CB_NAME ($3));
		$$ = cb_error_node;
	}
  }
;


/* Data name */

arithmetic_x_list:
  arithmetic_x			{ $$ = $1; }
| arithmetic_x_list
  arithmetic_x			{ $$ = cb_list_append ($1, $2); }
;

arithmetic_x:
  x flag_rounded		{ $$ = cb_build_pair ($2, $1); }
;

/* Record name */

record_name:
  qualified_word
  {
	cb_tree x;
	cb_tree r;

	if ((x = cb_build_identifier ($1)) != cb_error_node &&
	    (r = cb_ref (x)) != cb_error_node) {
		if (!CB_FIELD_P(r)) {
			cb_error_x (x, _("Field name expected."));
			x = cb_error_node;
		} else if (!CB_FIELD(r)->file) {
			cb_error_x (x, _("Record name expected."));
			x = cb_error_node;
		}
	}
	$$ = x;
  }
;

/* Table name */

table_name:
  qualified_word
  {
	cb_tree x;

	x = cb_ref ($1);
	if (!CB_FIELD_P (x)) {
		$$ = cb_error_node;
	} else if (!CB_FIELD (x)->index_list) {
		cb_error_x ($1, _("'%s' not indexed"), cb_name ($1));
		cb_error_x (x, _("'%s' defined here"), cb_name (x));
		$$ = cb_error_node;
	} else {
		$$ = $1;
	}
  }
;

/* File name */

file_name_list:
  file_name
  {
	$$ = cb_list_init ($1);
  }
| file_name_list file_name
  {
	cb_tree		l;

	if ($2 != cb_error_node) {
		for (l = $1; l; l = CB_CHAIN (l)) {
			if (!strcasecmp (CB_NAME ($2), CB_NAME (CB_VALUE (l)))) {
				cb_error_x ($2, _("Multiple reference to '%s' "), CB_NAME ($2));
			}
		}
		$$ = cb_list_add ($1, $2);
	}
  }
;

file_name:
  WORD
  {
	if (CB_FILE_P (cb_ref ($1))) {
		$$ = $1;
	} else {
		cb_error_x ($1, _("'%s' is not a file name"), CB_NAME ($1));
		$$ = cb_error_node;
	}
  }
;

/* Mnemonic name */

mnemonic_name_list:
  mnemonic_name			{ $$ = cb_list_init ($1); }
| mnemonic_name_list
  mnemonic_name			{ $$ = cb_list_add ($1, $2); }
;

mnemonic_name:
  MNEMONIC_NAME			{ $$ = $1; }
;

/* Procedure name */

procedure_name_list:
  /* empty */			{ $$ = NULL; }
| procedure_name_list 
  procedure_name		{ $$ = cb_list_add ($1, $2); }
;

procedure_name:
  label
  {
	$$ = $1;
	CB_REFERENCE ($$)->offset = CB_TREE (current_section);
	current_program->label_list = cb_cons ($$, current_program->label_list);
  }
;

label:
  qualified_word
| integer_label
| integer_label in_of integer_label
;

integer_label:
  LITERAL
  {
	$$ = cb_build_reference ((char *)(CB_LITERAL ($1)->data));
	$$->source_file = $1->source_file;
	$$->source_line = $1->source_line;
  }
;

/* Reference */

reference_list:
  reference			{ $$ = cb_list_init ($1); }
| reference_list reference	{ $$ = cb_list_add ($1, $2); }
;

reference:
  qualified_word
  {
	$$ = $1;
	current_program->reference_list = cb_cons ($$, current_program->reference_list);
  }
;

/* No-Reference */

no_reference_list:
  qualified_word			{ $$ = cb_list_init ($1); }
| no_reference_list qualified_word	{ $$ = cb_list_add ($1, $2); }
;

opt_reference:
  /* empty */			{ $$ = NULL; }
| reference			{ $$ = $1; }
;

reference_or_literal:
  reference
| LITERAL
;

/* Undefined word */

undefined_word:
  WORD
  {
	$$ = $1;
	if (CB_REFERENCE ($$)->word->count > 0) {
		redefinition_error ($$);
		$$ = cb_error_node;
	}
  }
;


/*******************
 * Primitive elements
 *******************/

/*
 * Primitive value
 */

target_x_list:
  target_x			{ $$ = cb_list_init ($1); }
| target_x_list target_x	{ $$ = cb_list_add ($1, $2); }
;

target_x:
  identifier
| ADDRESS _of identifier_1	{ $$ = cb_build_address ($3); }
;

x_list:
  x				{ $$ = cb_list_init ($1); }
| x_list x			{ $$ = cb_list_add ($1, $2); }
;

x:
  identifier
| LENGTH _of identifier_1			{ $$ = cb_build_length ($3); }
| LENGTH _of basic_literal			{ $$ = cb_build_length ($3); }
| LENGTH _of function				{ $$ = cb_build_length ($3); }
| ADDRESS _of prog_or_entry alnum_or_id		{ $$ = cb_build_ppointer ($4); }
| ADDRESS _of identifier_1			{ $$ = cb_build_address ($3); }
| literal
| function
| linage_counter
;

arith_x:
  identifier
| LENGTH _of identifier_1			{ $$ = cb_build_length ($3); }
| LENGTH _of basic_literal			{ $$ = cb_build_length ($3); }
| LENGTH _of function				{ $$ = cb_build_length ($3); }
| basic_literal
| function
| linage_counter
;

prog_or_entry:
  PROGRAM
| ENTRY
;

alnum_or_id:
  identifier_1		{ $$ = $1; }
| LITERAL		{ $$ = $1; }
;

simple_value:
  identifier
| basic_literal
;

simple_all_value:
  identifier
| literal
;

/*
numeric_value:
  identifier
| integer
;
*/

id_or_lit:
  identifier
| LITERAL
;

id_or_lit_or_func:
  identifier
| LITERAL
| function
;

num_id_or_lit:
  identifier
| integer
| ZERO                          { $$ = cb_zero; }
;

/*
 * Identifier
 */

identifier:
  identifier_1			{ $$ = cb_build_identifier ($1); }
;

identifier_1:
  qualified_word		{ $$ = $1; }
| qualified_word subref		{ $$ = $1; }
| qualified_word refmod		{ $$ = $1; }
| qualified_word subref refmod	{ $$ = $1; }
;

qualified_word:
  WORD				{ $$ = $1; }
| WORD in_of qualified_word	{ $$ = $1; CB_REFERENCE ($1)->chain = $3; }
;

subref:
  '(' exp_list ')'
  {
	if (cb_ref ($0) != cb_error_node) {
		$$ = $0;
		CB_REFERENCE ($0)->subs = cb_list_reverse ($2);
	}
  }
;

refmod:
  '(' exp ':' ')'
  {
	if (cb_ref ($0) != cb_error_node) {
		CB_REFERENCE ($0)->value = CB_TREE (cb_field ($0));
		if (cb_tree_category ($0) == CB_CATEGORY_NATIONAL ||
		    cb_tree_category ($0) == CB_CATEGORY_NATIONAL_EDITED) {
#ifdef	I18N_UTF8
			/* I18N_UTF8: No wide char support. */
#else /*!I18N_UTF8*/
			$2 = cb_build_binary_op ($2, '*', cb_int2);
			$2 = cb_build_binary_op ($2, '-', cb_int1);
#endif /*I18N_UTF8*/
		} else {
			CB_TREE ($0)->category = CB_CATEGORY_ALPHANUMERIC;
		}
		CB_REFERENCE ($0)->offset = $2;
	}
  }
| '(' exp ':' exp ')'
  {
	if (cb_ref ($0) != cb_error_node) {
		CB_REFERENCE ($0)->value = CB_TREE (cb_field ($0));
		if (cb_tree_category ($0) == CB_CATEGORY_NATIONAL ||
		    cb_tree_category ($0) == CB_CATEGORY_NATIONAL_EDITED) {
#ifdef	I18N_UTF8
			/* I18N_UTF8: No wide char support. */
#else /*!I18N_UTF8*/
			$2 = cb_build_binary_op ($2, '*', cb_int2);
			$2 = cb_build_binary_op ($2, '-', cb_int1);
			$4 = cb_build_binary_op ($4, '*', cb_int2);
#endif /*I18N_UTF8*/
		} else {
			CB_TREE ($0)->category = CB_CATEGORY_ALPHANUMERIC;
		}
		CB_REFERENCE ($0)->offset = $2;
		CB_REFERENCE ($0)->length = $4;
	}
  }
;

/*
 * Literal
 */

integer:
  LITERAL
  {
	if (cb_tree_category ($1) != CB_CATEGORY_NUMERIC) {
		cb_error (_("Integer value expected"));
	} else if (CB_LITERAL ($1)->sign < 0 || CB_LITERAL ($1)->scale) {
		cb_error (_("Integer value expected"));
	}
	$$ = $1;
  }
;

literal:
  basic_literal			{ $$ = $1; }
| ALL basic_value
  {
	$$ = $2;
	if (CB_LITERAL_P ($2)) {
		CB_LITERAL ($2)->all = 1;
	}
  }
;

basic_literal:
  basic_value			{ $$ = $1; }
| basic_literal '&' basic_value	{ $$ = cb_concat_literals ($1, $3); }
;

basic_value:
  LITERAL			{ $$ = $1; }
| SPACE				{ $$ = cb_space; }
| ZERO				{ $$ = cb_zero; }
| QUOTE				{ $$ = cb_quote; }
| HIGH_VALUE			{ $$ = cb_high; }
| LOW_VALUE			{ $$ = cb_low; }
| TOK_NULL			{ $$ = cb_null; }
;

/*
 * Function
 */

function:
  CURRENT_DATE_FUNC func_refmod
  {
	$$ = cb_build_intrinsic ($1, NULL, $2);
  }
| WHEN_COMPILED_FUNC func_refmod
  {
	$$ = cb_build_intrinsic ($1, NULL, $2);
  }
| UPPER_CASE_FUNC '(' exp ')' func_refmod
  {
	$$ = cb_build_intrinsic ($1, cb_list_init ($3), $5);
  }
| LOWER_CASE_FUNC '(' exp ')' func_refmod
  {
	$$ = cb_build_intrinsic ($1, cb_list_init ($3), $5);
  }
| REVERSE_FUNC '(' exp ')' func_refmod
  {
	$$ = cb_build_intrinsic ($1, cb_list_init ($3), $5);
  }
| CONCATENATE_FUNC '(' exp_list ')' func_refmod
  {
	$$ = cb_build_intrinsic ($1, $3, $5);
  }
| SUBSTITUTE_FUNC '(' exp_list ')' func_refmod
  {
	$$ = cb_build_intrinsic ($1, $3, $5);
  }
| SUBSTITUTE_CASE_FUNC '(' exp_list ')' func_refmod
  {
	$$ = cb_build_intrinsic ($1, $3, $5);
  }
| TRIM_FUNCTION '(' trim_args ')' func_refmod
  {
	$$ = cb_build_intrinsic ($1, $3, $5);
  }
| NUMVALC_FUNC '(' numvalc_args ')'
  {
	$$ = cb_build_intrinsic ($1, $3, NULL);
  }
| LOCALE_DT_FUNC '(' locale_dt_args ')' func_refmod
  {
	$$ = cb_build_intrinsic ($1, $3, $5);
  }
| FUNCTION_NAME func_args
  {
	$$ = cb_build_intrinsic ($1, $2, NULL);
  }
;

func_refmod:
  /* empty */			{ $$ = NULL; }
| '(' exp ':' ')'		{ $$ = cb_build_pair ($2, NULL); }
| '(' exp ':' exp ')'		{ $$ = cb_build_pair ($2, $4); }
;

func_args:
  /* empty */			{ $$ = NULL; }
| '(' list_func_args ')'	{ $$ = $2; }
;

list_func_args:
  /* empty */			{ $$ = NULL; }
| exp_list			{ $$ = $1; }
;


trim_args:
  exp
  {
	cb_tree	x;

	x = cb_list_init ($1);
	$$ = cb_list_add (x, cb_int0);
  }
| exp e_sep LEADING
  {
	cb_tree	x;

	x = cb_list_init ($1);
	$$ = cb_list_add (x, cb_int1);
  }
| exp e_sep TRAILING
  {
	cb_tree	x;

	x = cb_list_init ($1);
	$$ = cb_list_add (x, cb_int2);
  }
;

numvalc_args:
  exp
  {
	cb_tree	x;

	x = cb_list_init ($1);
	$$ = cb_list_add (x, cb_null);
  }
| exp e_sep exp
  {
	cb_tree	x;

	x = cb_list_init ($1);
	$$ = cb_list_add (x, $3);
  }
;

locale_dt_args:
  exp
  {
	cb_tree	x;

	x = cb_list_init ($1);
	$$ = cb_list_add (x, cb_null);
  }
| exp e_sep reference
  {
	cb_tree	x;

	x = cb_list_init ($1);
	$$ = cb_list_add (x, cb_ref ($3));
  }
;

/*******************
 * Common rules
 *******************/

not_const_word:
  {
	non_const_word = 1;
  }
;

/*
 * Common flags
 */

flag_all:
  /* empty */			{ $$ = cb_int0; }
| ALL				{ $$ = cb_int1; }
;

flag_duplicates:
  /* empty */			{ $$ = cb_int0; }
| with_dups			{ $$ = cb_int1; }
;

flag_initialized:
  /* empty */			{ $$ = NULL; }
| INITIALIZED			{ $$ = cb_int1; }
;

flag_next:
  /* empty */			{ $$ = cb_int0; }
| NEXT				{ $$ = cb_int1; }
| PREVIOUS			{ $$ = cb_int2; }
;

flag_not:
  /* empty */			{ $$ = cb_int0; }
| NOT				{ $$ = cb_int1; }
;

flag_optional:
  /* empty */			{ $$ = cb_int0; }
| OPTIONAL			{ $$ = cb_int1; }
;

flag_rounded:
  /* empty */			{ $$ = cb_int0; }
| ROUNDED			{ $$ = cb_int1; }
;

flag_separate:
  /* empty */			{ $$ = cb_int0; }
| SEPARATE _character		{ $$ = cb_int1; }
;

/*
 * Prepositions
 */

in_of:		IN | OF ;
records:	RECORD _is | RECORDS _are ;
with_dups:	WITH DUPLICATES | DUPLICATES ;
coll_sequence:	COLLATING SEQUENCE | SEQUENCE ;

_advancing:	| ADVANCING ;
_also:		| ALSO { $$ = cb_int1; } ;
_are:		| ARE ;
_area:		| AREA ;
_as:		| AS ;
_at:		| AT ;
_binary:	| BINARY ;
_by:		| BY ;
_character:	| CHARACTER ;
_characters:	| CHARACTERS ;
_contains:	| CONTAINS ;
_data:		| DATA ;
_file:		| TOK_FILE ;
_for:		| FOR ;
_from:		| FROM ;
_in:		| IN ;
_is:		{ $$ = NULL; } | IS { $$ = cb_int1; } ;
_is_are:	| IS | ARE ;
_key:		| KEY ;
_line_or_lines:	| LINE | LINES ;
_lines:		| LINES ;
_literal:	{ $$ = NULL; } | LITERAL { $$ = $1; } ;
_mode:		| MODE ;
_number:	| NUMBER ;
_of:		| OF ;
_on:		| ON ;
_in_order:	| ORDER | IN ORDER ;
_other:		| OTHER ;
_program:	| PROGRAM ;
_record:	| RECORD ;
_right:		| RIGHT ;
_set:		| SET ;
_sign:		| SIGN ;
_sign_is:	| SIGN | SIGN IS ;
_size:		| SIZE ;
_status:	| STATUS ;
_tape:		| TAPE ;
_than:		| THAN ;
_then:		| THEN ;
_times:		| TIMES ;
_to:		| TO ;
/* _upon:		| UPON ; */
_when:		| WHEN ;
_with:		| WITH ;


%%
