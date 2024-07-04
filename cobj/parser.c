/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 27 "parser.y"

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


#line 355 "parser.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "parser.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_ACCEPT = 3,                     /* ACCEPT  */
  YYSYMBOL_ACCESS = 4,                     /* ACCESS  */
  YYSYMBOL_ADD = 5,                        /* ADD  */
  YYSYMBOL_ADDRESS = 6,                    /* ADDRESS  */
  YYSYMBOL_ADVANCING = 7,                  /* ADVANCING  */
  YYSYMBOL_AFTER = 8,                      /* AFTER  */
  YYSYMBOL_ALL = 9,                        /* ALL  */
  YYSYMBOL_ALLOCATE = 10,                  /* ALLOCATE  */
  YYSYMBOL_ALPHABET = 11,                  /* ALPHABET  */
  YYSYMBOL_ALPHABETIC = 12,                /* ALPHABETIC  */
  YYSYMBOL_ALPHABETIC_LOWER = 13,          /* "ALPHABETIC-LOWER"  */
  YYSYMBOL_ALPHABETIC_UPPER = 14,          /* "ALPHABETIC-UPPER"  */
  YYSYMBOL_ALPHANUMERIC = 15,              /* ALPHANUMERIC  */
  YYSYMBOL_ALPHANUMERIC_EDITED = 16,       /* "ALPHANUMERIC-EDITED"  */
  YYSYMBOL_ALSO = 17,                      /* ALSO  */
  YYSYMBOL_ALTER = 18,                     /* ALTER  */
  YYSYMBOL_ALTERNATE = 19,                 /* ALTERNATE  */
  YYSYMBOL_AND = 20,                       /* AND  */
  YYSYMBOL_ANY = 21,                       /* ANY  */
  YYSYMBOL_APPLY = 22,                     /* APPLY  */
  YYSYMBOL_ARE = 23,                       /* ARE  */
  YYSYMBOL_AREA = 24,                      /* AREA  */
  YYSYMBOL_ARGUMENT_NUMBER = 25,           /* "ARGUMENT-NUMBER"  */
  YYSYMBOL_ARGUMENT_VALUE = 26,            /* "ARGUMENT-VALUE"  */
  YYSYMBOL_AS = 27,                        /* AS  */
  YYSYMBOL_ASCENDING = 28,                 /* ASCENDING  */
  YYSYMBOL_ASSIGN = 29,                    /* ASSIGN  */
  YYSYMBOL_AT = 30,                        /* AT  */
  YYSYMBOL_AUTO = 31,                      /* AUTO  */
  YYSYMBOL_AUTOMATIC = 32,                 /* AUTOMATIC  */
  YYSYMBOL_BACKGROUND_COLOR = 33,          /* "BACKGROUND-COLOR"  */
  YYSYMBOL_BASED = 34,                     /* BASED  */
  YYSYMBOL_BEFORE = 35,                    /* BEFORE  */
  YYSYMBOL_BELL = 36,                      /* BELL  */
  YYSYMBOL_BINARY = 37,                    /* BINARY  */
  YYSYMBOL_BINARY_C_LONG = 38,             /* "BINARY-C-LONG"  */
  YYSYMBOL_BINARY_CHAR = 39,               /* "BINARY-CHAR"  */
  YYSYMBOL_BINARY_DOUBLE = 40,             /* "BINARY-DOUBLE"  */
  YYSYMBOL_BINARY_LONG = 41,               /* "BINARY-LONG"  */
  YYSYMBOL_BINARY_SHORT = 42,              /* "BINARY-SHORT"  */
  YYSYMBOL_BLANK = 43,                     /* BLANK  */
  YYSYMBOL_BLANK_LINE = 44,                /* "BLANK-LINE"  */
  YYSYMBOL_BLANK_SCREEN = 45,              /* "BLANK-SCREEN"  */
  YYSYMBOL_BLINK = 46,                     /* BLINK  */
  YYSYMBOL_BLOCK = 47,                     /* BLOCK  */
  YYSYMBOL_BOTTOM = 48,                    /* BOTTOM  */
  YYSYMBOL_BY = 49,                        /* BY  */
  YYSYMBOL_BYTE_LENGTH = 50,               /* "BYTE-LENGTH"  */
  YYSYMBOL_CALL = 51,                      /* CALL  */
  YYSYMBOL_CANCEL = 52,                    /* CANCEL  */
  YYSYMBOL_CH = 53,                        /* CH  */
  YYSYMBOL_CHAINING = 54,                  /* CHAINING  */
  YYSYMBOL_CHARACTER = 55,                 /* CHARACTER  */
  YYSYMBOL_CHARACTERS = 56,                /* CHARACTERS  */
  YYSYMBOL_CLASS = 57,                     /* CLASS  */
  YYSYMBOL_CLASS_NAME = 58,                /* CLASS_NAME  */
  YYSYMBOL_CLOSE = 59,                     /* CLOSE  */
  YYSYMBOL_CLOSE_NOFEED = 60,              /* "CLOSE-NOFEED"  */
  YYSYMBOL_CODE = 61,                      /* CODE  */
  YYSYMBOL_CODE_SET = 62,                  /* "CODE-SET"  */
  YYSYMBOL_COLLATING = 63,                 /* COLLATING  */
  YYSYMBOL_COL = 64,                       /* COL  */
  YYSYMBOL_COLS = 65,                      /* COLS  */
  YYSYMBOL_COLUMN = 66,                    /* COLUMN  */
  YYSYMBOL_COLUMNS = 67,                   /* COLUMNS  */
  YYSYMBOL_COMMA = 68,                     /* COMMA  */
  YYSYMBOL_COMMAND_LINE = 69,              /* "COMMAND-LINE"  */
  YYSYMBOL_COMMA_DELIM = 70,               /* "comma delimiter"  */
  YYSYMBOL_COMMIT = 71,                    /* COMMIT  */
  YYSYMBOL_COMMITMENT_CONTROL = 72,        /* "COMMITMENT-CONTROL"  */
  YYSYMBOL_COMMON = 73,                    /* COMMON  */
  YYSYMBOL_COMP = 74,                      /* COMP  */
  YYSYMBOL_COMPUTE = 75,                   /* COMPUTE  */
  YYSYMBOL_COMP_1 = 76,                    /* "COMP-1"  */
  YYSYMBOL_COMP_2 = 77,                    /* "COMP-2"  */
  YYSYMBOL_COMP_3 = 78,                    /* "COMP-3"  */
  YYSYMBOL_COMP_4 = 79,                    /* "COMP-4"  */
  YYSYMBOL_COMP_5 = 80,                    /* "COMP-5"  */
  YYSYMBOL_COMP_X = 81,                    /* "COMP-X"  */
  YYSYMBOL_CONCATENATE_FUNC = 82,          /* "FUNCTION CONCATENATE"  */
  YYSYMBOL_CONFIGURATION = 83,             /* CONFIGURATION  */
  YYSYMBOL_CONSTANT = 84,                  /* CONSTANT  */
  YYSYMBOL_CONTAINS = 85,                  /* CONTAINS  */
  YYSYMBOL_CONTENT = 86,                   /* CONTENT  */
  YYSYMBOL_CONTINUE = 87,                  /* CONTINUE  */
  YYSYMBOL_CONTROL = 88,                   /* CONTROL  */
  YYSYMBOL_CONTROLS = 89,                  /* CONTROLS  */
  YYSYMBOL_CONTROL_FOOTING = 90,           /* "CONTROL FOOTING"  */
  YYSYMBOL_CONTROL_HEADING = 91,           /* "CONTROL HEADING"  */
  YYSYMBOL_CONVERTING = 92,                /* CONVERTING  */
  YYSYMBOL_CORE_INDEX = 93,                /* "CORE-INDEX"  */
  YYSYMBOL_CORRESPONDING = 94,             /* CORRESPONDING  */
  YYSYMBOL_COUNT = 95,                     /* COUNT  */
  YYSYMBOL_CRT = 96,                       /* CRT  */
  YYSYMBOL_CURRENCY = 97,                  /* CURRENCY  */
  YYSYMBOL_CURRENT_DATE_FUNC = 98,         /* "FUNCTION CURRENT-DATE"  */
  YYSYMBOL_CURSOR = 99,                    /* CURSOR  */
  YYSYMBOL_CYCLE = 100,                    /* CYCLE  */
  YYSYMBOL_CYL_OVERFLOW = 101,             /* "CYL-OVERFLOW"  */
  YYSYMBOL_DATA = 102,                     /* DATA  */
  YYSYMBOL_DATE = 103,                     /* DATE  */
  YYSYMBOL_DAY = 104,                      /* DAY  */
  YYSYMBOL_DAY_OF_WEEK = 105,              /* "DAY-OF-WEEK"  */
  YYSYMBOL_DE = 106,                       /* DE  */
  YYSYMBOL_DEBUGGING = 107,                /* DEBUGGING  */
  YYSYMBOL_DECIMAL_POINT = 108,            /* "DECIMAL-POINT"  */
  YYSYMBOL_DECLARATIVES = 109,             /* DECLARATIVES  */
  YYSYMBOL_DEFAULT = 110,                  /* DEFAULT  */
  YYSYMBOL_DELETE = 111,                   /* DELETE  */
  YYSYMBOL_DELIMITED = 112,                /* DELIMITED  */
  YYSYMBOL_DELIMITER = 113,                /* DELIMITER  */
  YYSYMBOL_DEPENDING = 114,                /* DEPENDING  */
  YYSYMBOL_DESCENDING = 115,               /* DESCENDING  */
  YYSYMBOL_DETAIL = 116,                   /* DETAIL  */
  YYSYMBOL_DISK = 117,                     /* DISK  */
  YYSYMBOL_DISPLAY = 118,                  /* DISPLAY  */
  YYSYMBOL_DIVIDE = 119,                   /* DIVIDE  */
  YYSYMBOL_DIVISION = 120,                 /* DIVISION  */
  YYSYMBOL_DOWN = 121,                     /* DOWN  */
  YYSYMBOL_DUPLICATES = 122,               /* DUPLICATES  */
  YYSYMBOL_DYNAMIC = 123,                  /* DYNAMIC  */
  YYSYMBOL_EBCDIC = 124,                   /* EBCDIC  */
  YYSYMBOL_ELSE = 125,                     /* ELSE  */
  YYSYMBOL_END = 126,                      /* END  */
  YYSYMBOL_END_ACCEPT = 127,               /* "END-ACCEPT"  */
  YYSYMBOL_END_ADD = 128,                  /* "END-ADD"  */
  YYSYMBOL_END_CALL = 129,                 /* "END-CALL"  */
  YYSYMBOL_END_COMPUTE = 130,              /* "END-COMPUTE"  */
  YYSYMBOL_END_DELETE = 131,               /* "END-DELETE"  */
  YYSYMBOL_END_DISPLAY = 132,              /* "END-DISPLAY"  */
  YYSYMBOL_END_DIVIDE = 133,               /* "END-DIVIDE"  */
  YYSYMBOL_END_EVALUATE = 134,             /* "END-EVALUATE"  */
  YYSYMBOL_END_FUNCTION = 135,             /* "END FUNCTION"  */
  YYSYMBOL_END_IF = 136,                   /* "END-IF"  */
  YYSYMBOL_END_MULTIPLY = 137,             /* "END-MULTIPLY"  */
  YYSYMBOL_END_PERFORM = 138,              /* "END-PERFORM"  */
  YYSYMBOL_END_PROGRAM = 139,              /* "END PROGRAM"  */
  YYSYMBOL_END_READ = 140,                 /* "END-READ"  */
  YYSYMBOL_END_RETURN = 141,               /* "END-RETURN"  */
  YYSYMBOL_END_REWRITE = 142,              /* "END-REWRITE"  */
  YYSYMBOL_END_SEARCH = 143,               /* "END-SEARCH"  */
  YYSYMBOL_END_START = 144,                /* "END-START"  */
  YYSYMBOL_END_STRING = 145,               /* "END-STRING"  */
  YYSYMBOL_END_SUBTRACT = 146,             /* "END-SUBTRACT"  */
  YYSYMBOL_END_UNSTRING = 147,             /* "END-UNSTRING"  */
  YYSYMBOL_END_WRITE = 148,                /* "END-WRITE"  */
  YYSYMBOL_ENTRY = 149,                    /* ENTRY  */
  YYSYMBOL_ENVIRONMENT = 150,              /* ENVIRONMENT  */
  YYSYMBOL_ENVIRONMENT_NAME = 151,         /* "ENVIRONMENT-NAME"  */
  YYSYMBOL_ENVIRONMENT_VALUE = 152,        /* "ENVIRONMENT-VALUE"  */
  YYSYMBOL_EOL = 153,                      /* EOL  */
  YYSYMBOL_EOP = 154,                      /* EOP  */
  YYSYMBOL_EOS = 155,                      /* EOS  */
  YYSYMBOL_EQUAL = 156,                    /* EQUAL  */
  YYSYMBOL_EQUALS = 157,                   /* EQUALS  */
  YYSYMBOL_ERASE = 158,                    /* ERASE  */
  YYSYMBOL_ERROR = 159,                    /* ERROR  */
  YYSYMBOL_ESCAPE = 160,                   /* ESCAPE  */
  YYSYMBOL_EVALUATE = 161,                 /* EVALUATE  */
  YYSYMBOL_EVENT_STATUS = 162,             /* "EVENT-STATUS"  */
  YYSYMBOL_EXCEPTION = 163,                /* EXCEPTION  */
  YYSYMBOL_EXCLUSIVE = 164,                /* EXCLUSIVE  */
  YYSYMBOL_EXIT = 165,                     /* EXIT  */
  YYSYMBOL_EXTEND = 166,                   /* EXTEND  */
  YYSYMBOL_EXTERNAL = 167,                 /* EXTERNAL  */
  YYSYMBOL_FD = 168,                       /* FD  */
  YYSYMBOL_FILE_CONTROL = 169,             /* "FILE-CONTROL"  */
  YYSYMBOL_FILE_ID = 170,                  /* "FILE-ID"  */
  YYSYMBOL_FILLER = 171,                   /* FILLER  */
  YYSYMBOL_FINAL = 172,                    /* FINAL  */
  YYSYMBOL_FIRST = 173,                    /* FIRST  */
  YYSYMBOL_FOOTING = 174,                  /* FOOTING  */
  YYSYMBOL_FOR = 175,                      /* FOR  */
  YYSYMBOL_FOREGROUND_COLOR = 176,         /* "FOREGROUND-COLOR"  */
  YYSYMBOL_FOREVER = 177,                  /* FOREVER  */
  YYSYMBOL_FORMS_OVERLAY = 178,            /* "FORMS-OVERLAY"  */
  YYSYMBOL_FREE = 179,                     /* FREE  */
  YYSYMBOL_FROM = 180,                     /* FROM  */
  YYSYMBOL_FULL = 181,                     /* FULL  */
  YYSYMBOL_FUNCTION = 182,                 /* FUNCTION  */
  YYSYMBOL_FUNCTION_ID = 183,              /* "FUNCTION-ID"  */
  YYSYMBOL_FUNCTION_NAME = 184,            /* "FUNCTION"  */
  YYSYMBOL_GE = 185,                       /* GE  */
  YYSYMBOL_GENERATE = 186,                 /* GENERATE  */
  YYSYMBOL_GIVING = 187,                   /* GIVING  */
  YYSYMBOL_GLOBAL = 188,                   /* GLOBAL  */
  YYSYMBOL_GO = 189,                       /* GO  */
  YYSYMBOL_GOBACK = 190,                   /* GOBACK  */
  YYSYMBOL_GREATER = 191,                  /* GREATER  */
  YYSYMBOL_GROUP = 192,                    /* GROUP  */
  YYSYMBOL_HEADING = 193,                  /* HEADING  */
  YYSYMBOL_HIGHLIGHT = 194,                /* HIGHLIGHT  */
  YYSYMBOL_HIGH_VALUE = 195,               /* "HIGH-VALUE"  */
  YYSYMBOL_IDENTIFICATION = 196,           /* IDENTIFICATION  */
  YYSYMBOL_IF = 197,                       /* IF  */
  YYSYMBOL_IGNORE = 198,                   /* IGNORE  */
  YYSYMBOL_IGNORING = 199,                 /* IGNORING  */
  YYSYMBOL_IN = 200,                       /* IN  */
  YYSYMBOL_INDEX = 201,                    /* INDEX  */
  YYSYMBOL_INDEXED = 202,                  /* INDEXED  */
  YYSYMBOL_INDICATE = 203,                 /* INDICATE  */
  YYSYMBOL_INITIALIZE = 204,               /* INITIALIZE  */
  YYSYMBOL_INITIALIZED = 205,              /* INITIALIZED  */
  YYSYMBOL_INITIATE = 206,                 /* INITIATE  */
  YYSYMBOL_INPUT = 207,                    /* INPUT  */
  YYSYMBOL_INPUT_OUTPUT = 208,             /* "INPUT-OUTPUT"  */
  YYSYMBOL_INSPECT = 209,                  /* INSPECT  */
  YYSYMBOL_INTO = 210,                     /* INTO  */
  YYSYMBOL_INTRINSIC = 211,                /* INTRINSIC  */
  YYSYMBOL_INVALID = 212,                  /* INVALID  */
  YYSYMBOL_INVALID_KEY = 213,              /* "INVALID KEY"  */
  YYSYMBOL_IS = 214,                       /* IS  */
  YYSYMBOL_I_O = 215,                      /* "I-O"  */
  YYSYMBOL_I_O_CONTROL = 216,              /* "I-O-CONTROL"  */
  YYSYMBOL_JUSTIFIED = 217,                /* JUSTIFIED  */
  YYSYMBOL_KEY = 218,                      /* KEY  */
  YYSYMBOL_LABEL = 219,                    /* LABEL  */
  YYSYMBOL_LAST = 220,                     /* LAST  */
  YYSYMBOL_LAST_DETAIL = 221,              /* "LAST DETAIL"  */
  YYSYMBOL_LE = 222,                       /* LE  */
  YYSYMBOL_LEADING = 223,                  /* LEADING  */
  YYSYMBOL_LEFT = 224,                     /* LEFT  */
  YYSYMBOL_LENGTH = 225,                   /* LENGTH  */
  YYSYMBOL_LESS = 226,                     /* LESS  */
  YYSYMBOL_LEVEL_NUMBER_WORD = 227,        /* LEVEL_NUMBER_WORD  */
  YYSYMBOL_LEVEL88_NUMBER_WORD = 228,      /* LEVEL88_NUMBER_WORD  */
  YYSYMBOL_LIMIT = 229,                    /* LIMIT  */
  YYSYMBOL_LIMITS = 230,                   /* LIMITS  */
  YYSYMBOL_LINAGE = 231,                   /* LINAGE  */
  YYSYMBOL_LINAGE_COUNTER = 232,           /* "LINAGE-COUNTER"  */
  YYSYMBOL_LINE = 233,                     /* LINE  */
  YYSYMBOL_LINES = 234,                    /* LINES  */
  YYSYMBOL_LINKAGE = 235,                  /* LINKAGE  */
  YYSYMBOL_LITERAL = 236,                  /* "Literal"  */
  YYSYMBOL_LOCALE = 237,                   /* LOCALE  */
  YYSYMBOL_LOCALE_DT_FUNC = 238,           /* "FUNCTION LOCALE"  */
  YYSYMBOL_LOCAL_STORAGE = 239,            /* "LOCAL-STORAGE"  */
  YYSYMBOL_LOCK = 240,                     /* LOCK  */
  YYSYMBOL_LOWER_CASE_FUNC = 241,          /* "FUNCTION LOWER-CASE"  */
  YYSYMBOL_LOWLIGHT = 242,                 /* LOWLIGHT  */
  YYSYMBOL_LOW_VALUE = 243,                /* "LOW-VALUE"  */
  YYSYMBOL_MANUAL = 244,                   /* MANUAL  */
  YYSYMBOL_MEMORY = 245,                   /* MEMORY  */
  YYSYMBOL_MERGE = 246,                    /* MERGE  */
  YYSYMBOL_MINUS = 247,                    /* MINUS  */
  YYSYMBOL_MNEMONIC_NAME = 248,            /* "MNEMONIC NAME"  */
  YYSYMBOL_MODE = 249,                     /* MODE  */
  YYSYMBOL_MOVE = 250,                     /* MOVE  */
  YYSYMBOL_MULTIPLE = 251,                 /* MULTIPLE  */
  YYSYMBOL_MULTIPLY = 252,                 /* MULTIPLY  */
  YYSYMBOL_NATIONAL = 253,                 /* NATIONAL  */
  YYSYMBOL_NATIONAL_EDITED = 254,          /* "NATIONAL-EDITED"  */
  YYSYMBOL_NATIVE = 255,                   /* NATIVE  */
  YYSYMBOL_NE = 256,                       /* NE  */
  YYSYMBOL_NEGATIVE = 257,                 /* NEGATIVE  */
  YYSYMBOL_NEXT = 258,                     /* NEXT  */
  YYSYMBOL_NEXT_SENTENCE = 259,            /* "NEXT SENTENCE"  */
  YYSYMBOL_NO = 260,                       /* NO  */
  YYSYMBOL_NOMINAL = 261,                  /* NOMINAL  */
  YYSYMBOL_NOT = 262,                      /* NOT  */
  YYSYMBOL_NOT_END = 263,                  /* "NOT END"  */
  YYSYMBOL_NOT_EOP = 264,                  /* "NOT EOP"  */
  YYSYMBOL_NOT_EXCEPTION = 265,            /* "NOT EXCEPTION"  */
  YYSYMBOL_NOT_INVALID_KEY = 266,          /* "NOT INVALID KEY"  */
  YYSYMBOL_NOT_OVERFLOW = 267,             /* "NOT OVERFLOW"  */
  YYSYMBOL_NOT_SIZE_ERROR = 268,           /* "NOT SIZE ERROR"  */
  YYSYMBOL_NO_ADVANCING = 269,             /* "NO ADVANCING"  */
  YYSYMBOL_NUMBER = 270,                   /* NUMBER  */
  YYSYMBOL_NUMBERS = 271,                  /* NUMBERS  */
  YYSYMBOL_NUMERIC = 272,                  /* NUMERIC  */
  YYSYMBOL_NUMERIC_EDITED = 273,           /* "NUMERIC-EDITED"  */
  YYSYMBOL_NUMVALC_FUNC = 274,             /* "FUNCTION NUMVALC"  */
  YYSYMBOL_OBJECT_COMPUTER = 275,          /* "OBJECT-COMPUTER"  */
  YYSYMBOL_OCCURS = 276,                   /* OCCURS  */
  YYSYMBOL_OF = 277,                       /* OF  */
  YYSYMBOL_OFF = 278,                      /* OFF  */
  YYSYMBOL_OMITTED = 279,                  /* OMITTED  */
  YYSYMBOL_ON = 280,                       /* ON  */
  YYSYMBOL_ONLY = 281,                     /* ONLY  */
  YYSYMBOL_OPEN = 282,                     /* OPEN  */
  YYSYMBOL_OPTIONAL = 283,                 /* OPTIONAL  */
  YYSYMBOL_OR = 284,                       /* OR  */
  YYSYMBOL_ORDER = 285,                    /* ORDER  */
  YYSYMBOL_ORGANIZATION = 286,             /* ORGANIZATION  */
  YYSYMBOL_OTHER = 287,                    /* OTHER  */
  YYSYMBOL_OUTPUT = 288,                   /* OUTPUT  */
  YYSYMBOL_OVERFLOW = 289,                 /* OVERFLOW  */
  YYSYMBOL_OVERLINE = 290,                 /* OVERLINE  */
  YYSYMBOL_PACKED_DECIMAL = 291,           /* "PACKED-DECIMAL"  */
  YYSYMBOL_PADDING = 292,                  /* PADDING  */
  YYSYMBOL_PAGE = 293,                     /* PAGE  */
  YYSYMBOL_PAGE_FOOTING = 294,             /* "PAGE FOOTING"  */
  YYSYMBOL_PAGE_HEADING = 295,             /* "PAGE HEADING"  */
  YYSYMBOL_PARAGRAPH = 296,                /* PARAGRAPH  */
  YYSYMBOL_PERFORM = 297,                  /* PERFORM  */
  YYSYMBOL_PICTURE = 298,                  /* PICTURE  */
  YYSYMBOL_PLUS = 299,                     /* PLUS  */
  YYSYMBOL_POINTER = 300,                  /* POINTER  */
  YYSYMBOL_POSITION = 301,                 /* POSITION  */
  YYSYMBOL_POSITIVE = 302,                 /* POSITIVE  */
  YYSYMBOL_PRESENT = 303,                  /* PRESENT  */
  YYSYMBOL_PREVIOUS = 304,                 /* PREVIOUS  */
  YYSYMBOL_PRINTER = 305,                  /* PRINTER  */
  YYSYMBOL_PRINTING = 306,                 /* PRINTING  */
  YYSYMBOL_PROCEDURE = 307,                /* PROCEDURE  */
  YYSYMBOL_PROCEDURES = 308,               /* PROCEDURES  */
  YYSYMBOL_PROCEED = 309,                  /* PROCEED  */
  YYSYMBOL_PROGRAM = 310,                  /* PROGRAM  */
  YYSYMBOL_PROGRAM_ID = 311,               /* "PROGRAM-ID"  */
  YYSYMBOL_PROGRAM_NAME = 312,             /* "Program name"  */
  YYSYMBOL_PROGRAM_POINTER = 313,          /* "PROGRAM-POINTER"  */
  YYSYMBOL_PROMPT = 314,                   /* PROMPT  */
  YYSYMBOL_QUOTE = 315,                    /* QUOTE  */
  YYSYMBOL_RANDOM = 316,                   /* RANDOM  */
  YYSYMBOL_RD = 317,                       /* RD  */
  YYSYMBOL_READ = 318,                     /* READ  */
  YYSYMBOL_RECORD = 319,                   /* RECORD  */
  YYSYMBOL_RECORDING = 320,                /* RECORDING  */
  YYSYMBOL_RECORDS = 321,                  /* RECORDS  */
  YYSYMBOL_RECURSIVE = 322,                /* RECURSIVE  */
  YYSYMBOL_REDEFINES = 323,                /* REDEFINES  */
  YYSYMBOL_REEL = 324,                     /* REEL  */
  YYSYMBOL_REFERENCE = 325,                /* REFERENCE  */
  YYSYMBOL_RELATIVE = 326,                 /* RELATIVE  */
  YYSYMBOL_RELEASE = 327,                  /* RELEASE  */
  YYSYMBOL_REMAINDER = 328,                /* REMAINDER  */
  YYSYMBOL_REMOVAL = 329,                  /* REMOVAL  */
  YYSYMBOL_RENAMES = 330,                  /* RENAMES  */
  YYSYMBOL_REPLACING = 331,                /* REPLACING  */
  YYSYMBOL_REPORT = 332,                   /* REPORT  */
  YYSYMBOL_REPORTING = 333,                /* REPORTING  */
  YYSYMBOL_REPORTS = 334,                  /* REPORTS  */
  YYSYMBOL_REPORT_FOOTING = 335,           /* "REPORT FOOTING"  */
  YYSYMBOL_REPORT_HEADING = 336,           /* "REPORT HEADING"  */
  YYSYMBOL_REPOSITORY = 337,               /* REPOSITORY  */
  YYSYMBOL_REQUIRED = 338,                 /* REQUIRED  */
  YYSYMBOL_RESERVE = 339,                  /* RESERVE  */
  YYSYMBOL_RETURN = 340,                   /* RETURN  */
  YYSYMBOL_RETURNING = 341,                /* RETURNING  */
  YYSYMBOL_REVERSE_FUNC = 342,             /* "FUNCTION REVERSE"  */
  YYSYMBOL_REVERSE_VIDEO = 343,            /* "REVERSE-VIDEO"  */
  YYSYMBOL_REWIND = 344,                   /* REWIND  */
  YYSYMBOL_REWRITE = 345,                  /* REWRITE  */
  YYSYMBOL_RIGHT = 346,                    /* RIGHT  */
  YYSYMBOL_ROLLBACK = 347,                 /* ROLLBACK  */
  YYSYMBOL_ROUNDED = 348,                  /* ROUNDED  */
  YYSYMBOL_RUN = 349,                      /* RUN  */
  YYSYMBOL_SAME = 350,                     /* SAME  */
  YYSYMBOL_SCREEN = 351,                   /* SCREEN  */
  YYSYMBOL_SCREEN_CONTROL = 352,           /* "SCREEN-CONTROL"  */
  YYSYMBOL_SCROLL = 353,                   /* SCROLL  */
  YYSYMBOL_SD = 354,                       /* SD  */
  YYSYMBOL_SEARCH = 355,                   /* SEARCH  */
  YYSYMBOL_SECTION = 356,                  /* SECTION  */
  YYSYMBOL_SECURE = 357,                   /* SECURE  */
  YYSYMBOL_SEGMENT_LIMIT = 358,            /* "SEGMENT-LIMIT"  */
  YYSYMBOL_SELECT = 359,                   /* SELECT  */
  YYSYMBOL_SEMI_COLON = 360,               /* "semi-colon"  */
  YYSYMBOL_SENTENCE = 361,                 /* SENTENCE  */
  YYSYMBOL_SEPARATE = 362,                 /* SEPARATE  */
  YYSYMBOL_SEQUENCE = 363,                 /* SEQUENCE  */
  YYSYMBOL_SEQUENTIAL = 364,               /* SEQUENTIAL  */
  YYSYMBOL_SET = 365,                      /* SET  */
  YYSYMBOL_SHARING = 366,                  /* SHARING  */
  YYSYMBOL_SIGN = 367,                     /* SIGN  */
  YYSYMBOL_SIGNED = 368,                   /* SIGNED  */
  YYSYMBOL_SIGNED_INT = 369,               /* "SIGNED-INT"  */
  YYSYMBOL_SIGNED_LONG = 370,              /* "SIGNED-LONG"  */
  YYSYMBOL_SIGNED_SHORT = 371,             /* "SIGNED-SHORT"  */
  YYSYMBOL_SIZE = 372,                     /* SIZE  */
  YYSYMBOL_SIZE_ERROR = 373,               /* "SIZE ERROR"  */
  YYSYMBOL_SORT = 374,                     /* SORT  */
  YYSYMBOL_SORT_MERGE = 375,               /* "SORT-MERGE"  */
  YYSYMBOL_SOURCE = 376,                   /* SOURCE  */
  YYSYMBOL_SOURCE_COMPUTER = 377,          /* "SOURCE-COMPUTER"  */
  YYSYMBOL_SPACE = 378,                    /* SPACE  */
  YYSYMBOL_SPECIAL_NAMES = 379,            /* "SPECIAL-NAMES"  */
  YYSYMBOL_STANDARD = 380,                 /* STANDARD  */
  YYSYMBOL_STANDARD_1 = 381,               /* "STANDARD-1"  */
  YYSYMBOL_STANDARD_2 = 382,               /* "STANDARD-2"  */
  YYSYMBOL_START = 383,                    /* START  */
  YYSYMBOL_STATUS = 384,                   /* STATUS  */
  YYSYMBOL_STOP = 385,                     /* STOP  */
  YYSYMBOL_STRING = 386,                   /* STRING  */
  YYSYMBOL_SUBSTITUTE_FUNC = 387,          /* "FUNCTION SUBSTITUTE"  */
  YYSYMBOL_SUBSTITUTE_CASE_FUNC = 388,     /* "FUNCTION SUBSTITUTE-CASE"  */
  YYSYMBOL_SUBTRACT = 389,                 /* SUBTRACT  */
  YYSYMBOL_SUM = 390,                      /* SUM  */
  YYSYMBOL_SUPPRESS = 391,                 /* SUPPRESS  */
  YYSYMBOL_SYMBOLIC = 392,                 /* SYMBOLIC  */
  YYSYMBOL_SYNCHRONIZED = 393,             /* SYNCHRONIZED  */
  YYSYMBOL_TALLYING = 394,                 /* TALLYING  */
  YYSYMBOL_TAPE = 395,                     /* TAPE  */
  YYSYMBOL_TERMINATE = 396,                /* TERMINATE  */
  YYSYMBOL_TEST = 397,                     /* TEST  */
  YYSYMBOL_THAN = 398,                     /* THAN  */
  YYSYMBOL_THEN = 399,                     /* THEN  */
  YYSYMBOL_THRU = 400,                     /* THRU  */
  YYSYMBOL_TIME = 401,                     /* TIME  */
  YYSYMBOL_TIMES = 402,                    /* TIMES  */
  YYSYMBOL_TO = 403,                       /* TO  */
  YYSYMBOL_TOK_FALSE = 404,                /* "FALSE"  */
  YYSYMBOL_TOK_FILE = 405,                 /* "FILE"  */
  YYSYMBOL_TOK_INITIAL = 406,              /* "INITIAL"  */
  YYSYMBOL_TOK_NULL = 407,                 /* "NULL"  */
  YYSYMBOL_TOK_TRUE = 408,                 /* "TRUE"  */
  YYSYMBOL_TOP = 409,                      /* TOP  */
  YYSYMBOL_TRACKS = 410,                   /* TRACKS  */
  YYSYMBOL_TRAILING = 411,                 /* TRAILING  */
  YYSYMBOL_TRANSFORM = 412,                /* TRANSFORM  */
  YYSYMBOL_TRIM_FUNCTION = 413,            /* "FUNCTION TRIM"  */
  YYSYMBOL_TYPE = 414,                     /* TYPE  */
  YYSYMBOL_UNDERLINE = 415,                /* UNDERLINE  */
  YYSYMBOL_UNIT = 416,                     /* UNIT  */
  YYSYMBOL_UNLOCK = 417,                   /* UNLOCK  */
  YYSYMBOL_UNSIGNED = 418,                 /* UNSIGNED  */
  YYSYMBOL_UNSIGNED_INT = 419,             /* "UNSIGNED-INT"  */
  YYSYMBOL_UNSIGNED_LONG = 420,            /* "UNSIGNED-LONG"  */
  YYSYMBOL_UNSIGNED_SHORT = 421,           /* "UNSIGNED-SHORT"  */
  YYSYMBOL_UNSTRING = 422,                 /* UNSTRING  */
  YYSYMBOL_UNTIL = 423,                    /* UNTIL  */
  YYSYMBOL_UP = 424,                       /* UP  */
  YYSYMBOL_UPDATE = 425,                   /* UPDATE  */
  YYSYMBOL_UPON = 426,                     /* UPON  */
  YYSYMBOL_UPON_ARGUMENT_NUMBER = 427,     /* "UPON ARGUMENT-NUMBER"  */
  YYSYMBOL_UPON_COMMAND_LINE = 428,        /* "UPON COMMAND-LINE"  */
  YYSYMBOL_UPON_ENVIRONMENT_NAME = 429,    /* "UPON ENVIRONMENT-NAME"  */
  YYSYMBOL_UPON_ENVIRONMENT_VALUE = 430,   /* "UPON ENVIRONMENT-VALUE"  */
  YYSYMBOL_UPPER_CASE_FUNC = 431,          /* "FUNCTION UPPER-CASE"  */
  YYSYMBOL_USAGE = 432,                    /* USAGE  */
  YYSYMBOL_USE = 433,                      /* USE  */
  YYSYMBOL_USING = 434,                    /* USING  */
  YYSYMBOL_VALUE = 435,                    /* VALUE  */
  YYSYMBOL_VARYING = 436,                  /* VARYING  */
  YYSYMBOL_WAIT = 437,                     /* WAIT  */
  YYSYMBOL_WHEN = 438,                     /* WHEN  */
  YYSYMBOL_WHEN_COMPILED_FUNC = 439,       /* "FUNCTION WHEN-COMPILED"  */
  YYSYMBOL_WHEN_OTHER = 440,               /* "WHEN OTHER"  */
  YYSYMBOL_WITH = 441,                     /* WITH  */
  YYSYMBOL_WORD = 442,                     /* "Identifier"  */
  YYSYMBOL_WORDS = 443,                    /* WORDS  */
  YYSYMBOL_WORKING_STORAGE = 444,          /* "WORKING-STORAGE"  */
  YYSYMBOL_WRITE = 445,                    /* WRITE  */
  YYSYMBOL_YYYYDDD = 446,                  /* YYYYDDD  */
  YYSYMBOL_YYYYMMDD = 447,                 /* YYYYMMDD  */
  YYSYMBOL_ZERO = 448,                     /* ZERO  */
  YYSYMBOL_449_ = 449,                     /* '+'  */
  YYSYMBOL_450_ = 450,                     /* '-'  */
  YYSYMBOL_451_ = 451,                     /* '*'  */
  YYSYMBOL_452_ = 452,                     /* '/'  */
  YYSYMBOL_UNARY_SIGN = 453,               /* UNARY_SIGN  */
  YYSYMBOL_454_ = 454,                     /* '^'  */
  YYSYMBOL_455_ = 455,                     /* '.'  */
  YYSYMBOL_456_ = 456,                     /* '='  */
  YYSYMBOL_457_ = 457,                     /* ')'  */
  YYSYMBOL_458_ = 458,                     /* '('  */
  YYSYMBOL_459_ = 459,                     /* '>'  */
  YYSYMBOL_460_ = 460,                     /* '<'  */
  YYSYMBOL_461_ = 461,                     /* ':'  */
  YYSYMBOL_462_ = 462,                     /* '&'  */
  YYSYMBOL_YYACCEPT = 463,                 /* $accept  */
  YYSYMBOL_start = 464,                    /* start  */
  YYSYMBOL_465_1 = 465,                    /* $@1  */
  YYSYMBOL_nested_list = 466,              /* nested_list  */
  YYSYMBOL_source_element = 467,           /* source_element  */
  YYSYMBOL_program_definition = 468,       /* program_definition  */
  YYSYMBOL_469_2 = 469,                    /* $@2  */
  YYSYMBOL_470_3 = 470,                    /* $@3  */
  YYSYMBOL_program_mandatory = 471,        /* program_mandatory  */
  YYSYMBOL_472_4 = 472,                    /* $@4  */
  YYSYMBOL_473_5 = 473,                    /* $@5  */
  YYSYMBOL_function_definition = 474,      /* function_definition  */
  YYSYMBOL_475_6 = 475,                    /* $@6  */
  YYSYMBOL_476_7 = 476,                    /* $@7  */
  YYSYMBOL_nested_prog = 477,              /* nested_prog  */
  YYSYMBOL_end_program = 478,              /* end_program  */
  YYSYMBOL_end_mandatory = 479,            /* end_mandatory  */
  YYSYMBOL_end_function = 480,             /* end_function  */
  YYSYMBOL_identification_division = 481,  /* identification_division  */
  YYSYMBOL_482_8 = 482,                    /* $@8  */
  YYSYMBOL_function_division = 483,        /* function_division  */
  YYSYMBOL_program_name = 484,             /* program_name  */
  YYSYMBOL_as_literal = 485,               /* as_literal  */
  YYSYMBOL_program_type = 486,             /* program_type  */
  YYSYMBOL_program_type_clause = 487,      /* program_type_clause  */
  YYSYMBOL__init_or_recurs = 488,          /* _init_or_recurs  */
  YYSYMBOL_environment_division = 489,     /* environment_division  */
  YYSYMBOL_configuration_section = 490,    /* configuration_section  */
  YYSYMBOL_configuration_list = 491,       /* configuration_list  */
  YYSYMBOL_configuration_paragraph = 492,  /* configuration_paragraph  */
  YYSYMBOL_source_computer_paragraph = 493, /* source_computer_paragraph  */
  YYSYMBOL_source_computer_entry = 494,    /* source_computer_entry  */
  YYSYMBOL_with_debugging_mode = 495,      /* with_debugging_mode  */
  YYSYMBOL_computer_name = 496,            /* computer_name  */
  YYSYMBOL_object_computer_paragraph = 497, /* object_computer_paragraph  */
  YYSYMBOL_object_computer_entry = 498,    /* object_computer_entry  */
  YYSYMBOL_object_clauses_list = 499,      /* object_clauses_list  */
  YYSYMBOL_object_clauses = 500,           /* object_clauses  */
  YYSYMBOL_object_computer_memory = 501,   /* object_computer_memory  */
  YYSYMBOL_object_char_or_word = 502,      /* object_char_or_word  */
  YYSYMBOL_object_computer_sequence = 503, /* object_computer_sequence  */
  YYSYMBOL_object_computer_segment = 504,  /* object_computer_segment  */
  YYSYMBOL_repository_paragraph = 505,     /* repository_paragraph  */
  YYSYMBOL_opt_repository = 506,           /* opt_repository  */
  YYSYMBOL_repository_list = 507,          /* repository_list  */
  YYSYMBOL_repository_name = 508,          /* repository_name  */
  YYSYMBOL_repository_literal_list = 509,  /* repository_literal_list  */
  YYSYMBOL_special_names_paragraph = 510,  /* special_names_paragraph  */
  YYSYMBOL_opt_special_names = 511,        /* opt_special_names  */
  YYSYMBOL_special_name_list = 512,        /* special_name_list  */
  YYSYMBOL_special_name = 513,             /* special_name  */
  YYSYMBOL_mnemonic_name_clause = 514,     /* mnemonic_name_clause  */
  YYSYMBOL_515_9 = 515,                    /* $@9  */
  YYSYMBOL_516_10 = 516,                   /* $@10  */
  YYSYMBOL_special_name_mnemonic_on_off_list = 517, /* special_name_mnemonic_on_off_list  */
  YYSYMBOL_special_name_mnemonic_on_off_list_mandatory = 518, /* special_name_mnemonic_on_off_list_mandatory  */
  YYSYMBOL_special_name_mnemonic_on_off = 519, /* special_name_mnemonic_on_off  */
  YYSYMBOL_on_or_off = 520,                /* on_or_off  */
  YYSYMBOL_alphabet_name_clause = 521,     /* alphabet_name_clause  */
  YYSYMBOL_522_11 = 522,                   /* $@11  */
  YYSYMBOL_alphabet_definition = 523,      /* alphabet_definition  */
  YYSYMBOL_alphabet_literal_list = 524,    /* alphabet_literal_list  */
  YYSYMBOL_alphabet_literal = 525,         /* alphabet_literal  */
  YYSYMBOL_526_12 = 526,                   /* @12  */
  YYSYMBOL_alphabet_also_sequence = 527,   /* alphabet_also_sequence  */
  YYSYMBOL_alphabet_lits = 528,            /* alphabet_lits  */
  YYSYMBOL_alphabet_also_literal = 529,    /* alphabet_also_literal  */
  YYSYMBOL_symbolic_characters_clause = 530, /* symbolic_characters_clause  */
  YYSYMBOL_symbolic_characters_list = 531, /* symbolic_characters_list  */
  YYSYMBOL_char_list = 532,                /* char_list  */
  YYSYMBOL_integer_list = 533,             /* integer_list  */
  YYSYMBOL_class_name_clause = 534,        /* class_name_clause  */
  YYSYMBOL_class_item_list = 535,          /* class_item_list  */
  YYSYMBOL_class_item = 536,               /* class_item  */
  YYSYMBOL_locale_clause = 537,            /* locale_clause  */
  YYSYMBOL_currency_sign_clause = 538,     /* currency_sign_clause  */
  YYSYMBOL_decimal_point_clause = 539,     /* decimal_point_clause  */
  YYSYMBOL_cursor_clause = 540,            /* cursor_clause  */
  YYSYMBOL_crt_status_clause = 541,        /* crt_status_clause  */
  YYSYMBOL_screen_control = 542,           /* screen_control  */
  YYSYMBOL_event_status = 543,             /* event_status  */
  YYSYMBOL_input_output_section = 544,     /* input_output_section  */
  YYSYMBOL_545_13 = 545,                   /* $@13  */
  YYSYMBOL_546_14 = 546,                   /* $@14  */
  YYSYMBOL_file_control_paragraph = 547,   /* file_control_paragraph  */
  YYSYMBOL_file_control_sequence = 548,    /* file_control_sequence  */
  YYSYMBOL_file_control_entry = 549,       /* file_control_entry  */
  YYSYMBOL_550_15 = 550,                   /* $@15  */
  YYSYMBOL_select_clause_sequence = 551,   /* select_clause_sequence  */
  YYSYMBOL_select_clause = 552,            /* select_clause  */
  YYSYMBOL_assign_clause = 553,            /* assign_clause  */
  YYSYMBOL__device = 554,                  /* _device  */
  YYSYMBOL__ext_clause = 555,              /* _ext_clause  */
  YYSYMBOL_assignment_name = 556,          /* assignment_name  */
  YYSYMBOL_assignment_device_name_list = 557, /* assignment_device_name_list  */
  YYSYMBOL_access_mode_clause = 558,       /* access_mode_clause  */
  YYSYMBOL_access_mode = 559,              /* access_mode  */
  YYSYMBOL_alternative_record_key_clause = 560, /* alternative_record_key_clause  */
  YYSYMBOL_split_key_list = 561,           /* split_key_list  */
  YYSYMBOL_562_16 = 562,                   /* $@16  */
  YYSYMBOL_split_key = 563,                /* split_key  */
  YYSYMBOL_key_is_eq = 564,                /* key_is_eq  */
  YYSYMBOL_collating_sequence_clause = 565, /* collating_sequence_clause  */
  YYSYMBOL_file_status_clause = 566,       /* file_status_clause  */
  YYSYMBOL_file_or_sort = 567,             /* file_or_sort  */
  YYSYMBOL_lock_mode_clause = 568,         /* lock_mode_clause  */
  YYSYMBOL_lock_mode = 569,                /* lock_mode  */
  YYSYMBOL_lock_with = 570,                /* lock_with  */
  YYSYMBOL_lock_records = 571,             /* lock_records  */
  YYSYMBOL_organization_clause = 572,      /* organization_clause  */
  YYSYMBOL_organization = 573,             /* organization  */
  YYSYMBOL_padding_character_clause = 574, /* padding_character_clause  */
  YYSYMBOL_record_delimiter_clause = 575,  /* record_delimiter_clause  */
  YYSYMBOL_record_key_clause = 576,        /* record_key_clause  */
  YYSYMBOL_relative_key_clause = 577,      /* relative_key_clause  */
  YYSYMBOL_reserve_clause = 578,           /* reserve_clause  */
  YYSYMBOL_sharing_clause = 579,           /* sharing_clause  */
  YYSYMBOL_sharing_option = 580,           /* sharing_option  */
  YYSYMBOL_nominal_key_clause = 581,       /* nominal_key_clause  */
  YYSYMBOL_i_o_control_paragraph = 582,    /* i_o_control_paragraph  */
  YYSYMBOL_opt_i_o_control = 583,          /* opt_i_o_control  */
  YYSYMBOL_i_o_control_list = 584,         /* i_o_control_list  */
  YYSYMBOL_i_o_control_clause = 585,       /* i_o_control_clause  */
  YYSYMBOL_same_clause = 586,              /* same_clause  */
  YYSYMBOL_same_option = 587,              /* same_option  */
  YYSYMBOL_multiple_file_tape_clause = 588, /* multiple_file_tape_clause  */
  YYSYMBOL_multiple_file_list = 589,       /* multiple_file_list  */
  YYSYMBOL_multiple_file = 590,            /* multiple_file  */
  YYSYMBOL_multiple_file_position = 591,   /* multiple_file_position  */
  YYSYMBOL_apply_clause_list = 592,        /* apply_clause_list  */
  YYSYMBOL_apply_clause = 593,             /* apply_clause  */
  YYSYMBOL_data_division = 594,            /* data_division  */
  YYSYMBOL_file_section = 595,             /* file_section  */
  YYSYMBOL_596_17 = 596,                   /* $@17  */
  YYSYMBOL_597_18 = 597,                   /* $@18  */
  YYSYMBOL_file_description_sequence = 598, /* file_description_sequence  */
  YYSYMBOL_file_description = 599,         /* file_description  */
  YYSYMBOL_file_description_sequence_without_type = 600, /* file_description_sequence_without_type  */
  YYSYMBOL_file_type = 601,                /* file_type  */
  YYSYMBOL_file_description_entry = 602,   /* file_description_entry  */
  YYSYMBOL_603_19 = 603,                   /* @19  */
  YYSYMBOL_file_description_clause_sequence = 604, /* file_description_clause_sequence  */
  YYSYMBOL_file_description_clause = 605,  /* file_description_clause  */
  YYSYMBOL_block_contains_clause = 606,    /* block_contains_clause  */
  YYSYMBOL__records_or_characters = 607,   /* _records_or_characters  */
  YYSYMBOL_record_clause = 608,            /* record_clause  */
  YYSYMBOL_record_depending = 609,         /* record_depending  */
  YYSYMBOL_opt_from_integer = 610,         /* opt_from_integer  */
  YYSYMBOL_opt_to_integer = 611,           /* opt_to_integer  */
  YYSYMBOL_label_records_clause = 612,     /* label_records_clause  */
  YYSYMBOL_label_option = 613,             /* label_option  */
  YYSYMBOL_value_of_clause = 614,          /* value_of_clause  */
  YYSYMBOL_valueof_name = 615,             /* valueof_name  */
  YYSYMBOL_data_records_clause = 616,      /* data_records_clause  */
  YYSYMBOL_linage_clause = 617,            /* linage_clause  */
  YYSYMBOL_linage_sequence = 618,          /* linage_sequence  */
  YYSYMBOL_linage_lines = 619,             /* linage_lines  */
  YYSYMBOL_linage_footing = 620,           /* linage_footing  */
  YYSYMBOL_linage_top = 621,               /* linage_top  */
  YYSYMBOL_linage_bottom = 622,            /* linage_bottom  */
  YYSYMBOL_recording_mode_clause = 623,    /* recording_mode_clause  */
  YYSYMBOL_code_set_clause = 624,          /* code_set_clause  */
  YYSYMBOL_report_clause = 625,            /* report_clause  */
  YYSYMBOL_working_storage_section = 626,  /* working_storage_section  */
  YYSYMBOL_627_20 = 627,                   /* $@20  */
  YYSYMBOL_record_description_list = 628,  /* record_description_list  */
  YYSYMBOL_record_description_list_1 = 629, /* record_description_list_1  */
  YYSYMBOL_630_21 = 630,                   /* $@21  */
  YYSYMBOL_record_description_list_2 = 631, /* record_description_list_2  */
  YYSYMBOL_data_description = 632,         /* data_description  */
  YYSYMBOL_633_22 = 633,                   /* $@22  */
  YYSYMBOL_634_23 = 634,                   /* $@23  */
  YYSYMBOL_level_number = 635,             /* level_number  */
  YYSYMBOL_level_number_88 = 636,          /* level_number_88  */
  YYSYMBOL__maybe_next_level_number = 637, /* _maybe_next_level_number  */
  YYSYMBOL_entry_name = 638,               /* entry_name  */
  YYSYMBOL_const_name = 639,               /* const_name  */
  YYSYMBOL_const_global = 640,             /* const_global  */
  YYSYMBOL_lit_or_length = 641,            /* lit_or_length  */
  YYSYMBOL_constant_entry = 642,           /* constant_entry  */
  YYSYMBOL_data_description_clause_sequence = 643, /* data_description_clause_sequence  */
  YYSYMBOL_data_description_clause = 644,  /* data_description_clause  */
  YYSYMBOL_redefines_clause = 645,         /* redefines_clause  */
  YYSYMBOL_external_clause = 646,          /* external_clause  */
  YYSYMBOL_as_extname = 647,               /* as_extname  */
  YYSYMBOL_global_clause = 648,            /* global_clause  */
  YYSYMBOL_picture_clause = 649,           /* picture_clause  */
  YYSYMBOL_usage_clause = 650,             /* usage_clause  */
  YYSYMBOL_usage = 651,                    /* usage  */
  YYSYMBOL_sign_clause = 652,              /* sign_clause  */
  YYSYMBOL_occurs_key_spec = 653,          /* occurs_key_spec  */
  YYSYMBOL_occurs_clause = 654,            /* occurs_clause  */
  YYSYMBOL_occurs_to_integer = 655,        /* occurs_to_integer  */
  YYSYMBOL_occurs_depending = 656,         /* occurs_depending  */
  YYSYMBOL__occurs_keys = 657,             /* _occurs_keys  */
  YYSYMBOL_occurs_keys = 658,              /* occurs_keys  */
  YYSYMBOL_occurs_key = 659,               /* occurs_key  */
  YYSYMBOL_occurs_key_list = 660,          /* occurs_key_list  */
  YYSYMBOL_ascending_or_descending = 661,  /* ascending_or_descending  */
  YYSYMBOL__occurs_indexed = 662,          /* _occurs_indexed  */
  YYSYMBOL_occurs_indexed = 663,           /* occurs_indexed  */
  YYSYMBOL_occurs_index_list = 664,        /* occurs_index_list  */
  YYSYMBOL_occurs_index = 665,             /* occurs_index  */
  YYSYMBOL_justified_clause = 666,         /* justified_clause  */
  YYSYMBOL_synchronized_clause = 667,      /* synchronized_clause  */
  YYSYMBOL_left_or_right = 668,            /* left_or_right  */
  YYSYMBOL_blank_clause = 669,             /* blank_clause  */
  YYSYMBOL_based_clause = 670,             /* based_clause  */
  YYSYMBOL_value_clause = 671,             /* value_clause  */
  YYSYMBOL_value_cond_clause = 672,        /* value_cond_clause  */
  YYSYMBOL_673_24 = 673,                   /* $@24  */
  YYSYMBOL_value_item_list = 674,          /* value_item_list  */
  YYSYMBOL_value_item = 675,               /* value_item  */
  YYSYMBOL_false_is = 676,                 /* false_is  */
  YYSYMBOL_renames_clause = 677,           /* renames_clause  */
  YYSYMBOL_any_length_clause = 678,        /* any_length_clause  */
  YYSYMBOL_local_storage_section = 679,    /* local_storage_section  */
  YYSYMBOL_680_25 = 680,                   /* $@25  */
  YYSYMBOL_linkage_section = 681,          /* linkage_section  */
  YYSYMBOL_682_26 = 682,                   /* $@26  */
  YYSYMBOL_report_section = 683,           /* report_section  */
  YYSYMBOL_684_27 = 684,                   /* $@27  */
  YYSYMBOL_opt_report_description_list = 685, /* opt_report_description_list  */
  YYSYMBOL_report_description_list = 686,  /* report_description_list  */
  YYSYMBOL_report_description_entry = 687, /* report_description_entry  */
  YYSYMBOL_report_description_options = 688, /* report_description_options  */
  YYSYMBOL_report_description_option = 689, /* report_description_option  */
  YYSYMBOL_control_clause = 690,           /* control_clause  */
  YYSYMBOL_control_field_list = 691,       /* control_field_list  */
  YYSYMBOL__final = 692,                   /* _final  */
  YYSYMBOL_identifier_list = 693,          /* identifier_list  */
  YYSYMBOL_page_limit_clause = 694,        /* page_limit_clause  */
  YYSYMBOL_heading_clause = 695,           /* heading_clause  */
  YYSYMBOL_first_detail = 696,             /* first_detail  */
  YYSYMBOL_last_heading = 697,             /* last_heading  */
  YYSYMBOL_last_detail = 698,              /* last_detail  */
  YYSYMBOL_footing_clause = 699,           /* footing_clause  */
  YYSYMBOL_page_line_column = 700,         /* page_line_column  */
  YYSYMBOL_line_or_lines = 701,            /* line_or_lines  */
  YYSYMBOL_report_group_description_list = 702, /* report_group_description_list  */
  YYSYMBOL_report_group_description_entry = 703, /* report_group_description_entry  */
  YYSYMBOL_report_group_options = 704,     /* report_group_options  */
  YYSYMBOL_report_group_option = 705,      /* report_group_option  */
  YYSYMBOL_type_clause = 706,              /* type_clause  */
  YYSYMBOL_type_option = 707,              /* type_option  */
  YYSYMBOL_next_group_clause = 708,        /* next_group_clause  */
  YYSYMBOL_column_clause = 709,            /* column_clause  */
  YYSYMBOL_sum_clause_list = 710,          /* sum_clause_list  */
  YYSYMBOL_sum_clause = 711,               /* sum_clause  */
  YYSYMBOL_ref_id_exp = 712,               /* ref_id_exp  */
  YYSYMBOL_present_when_condition = 713,   /* present_when_condition  */
  YYSYMBOL_varying_clause = 714,           /* varying_clause  */
  YYSYMBOL_line_clause = 715,              /* line_clause  */
  YYSYMBOL_line_keyword_clause = 716,      /* line_keyword_clause  */
  YYSYMBOL_report_line_integer_list = 717, /* report_line_integer_list  */
  YYSYMBOL_line_or_plus = 718,             /* line_or_plus  */
  YYSYMBOL__numbers = 719,                 /* _numbers  */
  YYSYMBOL_source_clause = 720,            /* source_clause  */
  YYSYMBOL_group_indicate_clause = 721,    /* group_indicate_clause  */
  YYSYMBOL__indicate = 722,                /* _indicate  */
  YYSYMBOL_report_name = 723,              /* report_name  */
  YYSYMBOL_screen_section = 724,           /* screen_section  */
  YYSYMBOL_725_28 = 725,                   /* $@28  */
  YYSYMBOL_procedure_division = 726,       /* procedure_division  */
  YYSYMBOL_727_29 = 727,                   /* $@29  */
  YYSYMBOL_728_30 = 728,                   /* $@30  */
  YYSYMBOL_procedure_using_chaining = 729, /* procedure_using_chaining  */
  YYSYMBOL_730_31 = 730,                   /* $@31  */
  YYSYMBOL_731_32 = 731,                   /* $@32  */
  YYSYMBOL_procedure_param_list = 732,     /* procedure_param_list  */
  YYSYMBOL_procedure_param = 733,          /* procedure_param  */
  YYSYMBOL_procedure_type = 734,           /* procedure_type  */
  YYSYMBOL_size_optional = 735,            /* size_optional  */
  YYSYMBOL_procedure_optional = 736,       /* procedure_optional  */
  YYSYMBOL_procedure_returning = 737,      /* procedure_returning  */
  YYSYMBOL_procedure_declaratives = 738,   /* procedure_declaratives  */
  YYSYMBOL_739_33 = 739,                   /* $@33  */
  YYSYMBOL_procedure_list = 740,           /* procedure_list  */
  YYSYMBOL_procedure = 741,                /* procedure  */
  YYSYMBOL_section_header = 742,           /* section_header  */
  YYSYMBOL_paragraph_header = 743,         /* paragraph_header  */
  YYSYMBOL_invalid_statement = 744,        /* invalid_statement  */
  YYSYMBOL_section_name = 745,             /* section_name  */
  YYSYMBOL_opt_segment = 746,              /* opt_segment  */
  YYSYMBOL_statement_list = 747,           /* statement_list  */
  YYSYMBOL_748_34 = 748,                   /* @34  */
  YYSYMBOL_749_35 = 749,                   /* @35  */
  YYSYMBOL_statements = 750,               /* statements  */
  YYSYMBOL_statement = 751,                /* statement  */
  YYSYMBOL_accept_statement = 752,         /* accept_statement  */
  YYSYMBOL_753_36 = 753,                   /* $@36  */
  YYSYMBOL_accept_body = 754,              /* accept_body  */
  YYSYMBOL_opt_at_line_column = 755,       /* opt_at_line_column  */
  YYSYMBOL_line_number = 756,              /* line_number  */
  YYSYMBOL_column_number = 757,            /* column_number  */
  YYSYMBOL_opt_accp_attr = 758,            /* opt_accp_attr  */
  YYSYMBOL_accp_attrs = 759,               /* accp_attrs  */
  YYSYMBOL_accp_attr = 760,                /* accp_attr  */
  YYSYMBOL_end_accept = 761,               /* end_accept  */
  YYSYMBOL_add_statement = 762,            /* add_statement  */
  YYSYMBOL_763_37 = 763,                   /* $@37  */
  YYSYMBOL_add_body = 764,                 /* add_body  */
  YYSYMBOL_add_to = 765,                   /* add_to  */
  YYSYMBOL_end_add = 766,                  /* end_add  */
  YYSYMBOL_allocate_statement = 767,       /* allocate_statement  */
  YYSYMBOL_768_38 = 768,                   /* $@38  */
  YYSYMBOL_allocate_body = 769,            /* allocate_body  */
  YYSYMBOL_allocate_returning = 770,       /* allocate_returning  */
  YYSYMBOL_alter_statement = 771,          /* alter_statement  */
  YYSYMBOL_alter_options = 772,            /* alter_options  */
  YYSYMBOL__proceed_to = 773,              /* _proceed_to  */
  YYSYMBOL_call_statement = 774,           /* call_statement  */
  YYSYMBOL_775_39 = 775,                   /* $@39  */
  YYSYMBOL_call_using = 776,               /* call_using  */
  YYSYMBOL_777_40 = 777,                   /* $@40  */
  YYSYMBOL_call_param_list = 778,          /* call_param_list  */
  YYSYMBOL_call_param = 779,               /* call_param  */
  YYSYMBOL_call_type = 780,                /* call_type  */
  YYSYMBOL_call_returning = 781,           /* call_returning  */
  YYSYMBOL_call_on_exception = 782,        /* call_on_exception  */
  YYSYMBOL_783_41 = 783,                   /* $@41  */
  YYSYMBOL_call_not_on_exception = 784,    /* call_not_on_exception  */
  YYSYMBOL_785_42 = 785,                   /* $@42  */
  YYSYMBOL_end_call = 786,                 /* end_call  */
  YYSYMBOL_cancel_statement = 787,         /* cancel_statement  */
  YYSYMBOL_788_43 = 788,                   /* $@43  */
  YYSYMBOL_cancel_list = 789,              /* cancel_list  */
  YYSYMBOL_close_statement = 790,          /* close_statement  */
  YYSYMBOL_791_44 = 791,                   /* $@44  */
  YYSYMBOL_close_list = 792,               /* close_list  */
  YYSYMBOL_close_option = 793,             /* close_option  */
  YYSYMBOL_reel_or_unit = 794,             /* reel_or_unit  */
  YYSYMBOL_compute_statement = 795,        /* compute_statement  */
  YYSYMBOL_796_45 = 796,                   /* $@45  */
  YYSYMBOL_compute_body = 797,             /* compute_body  */
  YYSYMBOL_end_compute = 798,              /* end_compute  */
  YYSYMBOL_comp_equal = 799,               /* comp_equal  */
  YYSYMBOL_commit_statement = 800,         /* commit_statement  */
  YYSYMBOL_continue_statement = 801,       /* continue_statement  */
  YYSYMBOL_delete_statement = 802,         /* delete_statement  */
  YYSYMBOL_803_46 = 803,                   /* $@46  */
  YYSYMBOL_end_delete = 804,               /* end_delete  */
  YYSYMBOL_delete_file_statement = 805,    /* delete_file_statement  */
  YYSYMBOL_806_47 = 806,                   /* $@47  */
  YYSYMBOL_display_statement = 807,        /* display_statement  */
  YYSYMBOL_808_48 = 808,                   /* $@48  */
  YYSYMBOL_display_body = 809,             /* display_body  */
  YYSYMBOL_with_clause = 810,              /* with_clause  */
  YYSYMBOL_disp_attrs = 811,               /* disp_attrs  */
  YYSYMBOL_disp_attr = 812,                /* disp_attr  */
  YYSYMBOL_end_display = 813,              /* end_display  */
  YYSYMBOL_divide_statement = 814,         /* divide_statement  */
  YYSYMBOL_815_49 = 815,                   /* $@49  */
  YYSYMBOL_divide_body = 816,              /* divide_body  */
  YYSYMBOL_end_divide = 817,               /* end_divide  */
  YYSYMBOL_entry_statement = 818,          /* entry_statement  */
  YYSYMBOL_819_50 = 819,                   /* $@50  */
  YYSYMBOL_evaluate_statement = 820,       /* evaluate_statement  */
  YYSYMBOL_821_51 = 821,                   /* $@51  */
  YYSYMBOL_evaluate_subject_list = 822,    /* evaluate_subject_list  */
  YYSYMBOL_evaluate_subject = 823,         /* evaluate_subject  */
  YYSYMBOL_evaluate_condition_list = 824,  /* evaluate_condition_list  */
  YYSYMBOL_evaluate_case_list = 825,       /* evaluate_case_list  */
  YYSYMBOL_evaluate_case = 826,            /* evaluate_case  */
  YYSYMBOL_827_52 = 827,                   /* $@52  */
  YYSYMBOL_evaluate_other = 828,           /* evaluate_other  */
  YYSYMBOL_829_53 = 829,                   /* $@53  */
  YYSYMBOL_evaluate_when_list = 830,       /* evaluate_when_list  */
  YYSYMBOL_evaluate_object_list = 831,     /* evaluate_object_list  */
  YYSYMBOL_evaluate_object = 832,          /* evaluate_object  */
  YYSYMBOL_opt_evaluate_thru_expr = 833,   /* opt_evaluate_thru_expr  */
  YYSYMBOL_end_evaluate = 834,             /* end_evaluate  */
  YYSYMBOL_exit_statement = 835,           /* exit_statement  */
  YYSYMBOL_836_54 = 836,                   /* $@54  */
  YYSYMBOL_exit_body = 837,                /* exit_body  */
  YYSYMBOL_free_statement = 838,           /* free_statement  */
  YYSYMBOL_839_55 = 839,                   /* $@55  */
  YYSYMBOL_generate_statement = 840,       /* generate_statement  */
  YYSYMBOL_841_56 = 841,                   /* $@56  */
  YYSYMBOL_goto_statement = 842,           /* goto_statement  */
  YYSYMBOL_843_57 = 843,                   /* $@57  */
  YYSYMBOL_goto_depending = 844,           /* goto_depending  */
  YYSYMBOL_goback_statement = 845,         /* goback_statement  */
  YYSYMBOL_846_58 = 846,                   /* $@58  */
  YYSYMBOL_if_statement = 847,             /* if_statement  */
  YYSYMBOL_848_59 = 848,                   /* $@59  */
  YYSYMBOL_849_60 = 849,                   /* $@60  */
  YYSYMBOL_if_else_sentence = 850,         /* if_else_sentence  */
  YYSYMBOL_851_61 = 851,                   /* $@61  */
  YYSYMBOL_end_if = 852,                   /* end_if  */
  YYSYMBOL_initialize_statement = 853,     /* initialize_statement  */
  YYSYMBOL_854_62 = 854,                   /* $@62  */
  YYSYMBOL_initialize_filler = 855,        /* initialize_filler  */
  YYSYMBOL_initialize_value = 856,         /* initialize_value  */
  YYSYMBOL_initialize_replacing = 857,     /* initialize_replacing  */
  YYSYMBOL_initialize_replacing_list = 858, /* initialize_replacing_list  */
  YYSYMBOL_initialize_replacing_item = 859, /* initialize_replacing_item  */
  YYSYMBOL_initialize_category = 860,      /* initialize_category  */
  YYSYMBOL_initialize_default = 861,       /* initialize_default  */
  YYSYMBOL_initiate_statement = 862,       /* initiate_statement  */
  YYSYMBOL_863_63 = 863,                   /* $@63  */
  YYSYMBOL_inspect_statement = 864,        /* inspect_statement  */
  YYSYMBOL_865_64 = 865,                   /* $@64  */
  YYSYMBOL_send_identifier = 866,          /* send_identifier  */
  YYSYMBOL_inspect_list = 867,             /* inspect_list  */
  YYSYMBOL_inspect_item = 868,             /* inspect_item  */
  YYSYMBOL_inspect_tallying = 869,         /* inspect_tallying  */
  YYSYMBOL_870_65 = 870,                   /* $@65  */
  YYSYMBOL_tallying_list = 871,            /* tallying_list  */
  YYSYMBOL_tallying_item = 872,            /* tallying_item  */
  YYSYMBOL_inspect_replacing = 873,        /* inspect_replacing  */
  YYSYMBOL_replacing_list = 874,           /* replacing_list  */
  YYSYMBOL_replacing_item = 875,           /* replacing_item  */
  YYSYMBOL_rep_keyword = 876,              /* rep_keyword  */
  YYSYMBOL_replacing_region = 877,         /* replacing_region  */
  YYSYMBOL_inspect_converting = 878,       /* inspect_converting  */
  YYSYMBOL_inspect_region = 879,           /* inspect_region  */
  YYSYMBOL__initial = 880,                 /* _initial  */
  YYSYMBOL_merge_statement = 881,          /* merge_statement  */
  YYSYMBOL_882_66 = 882,                   /* $@66  */
  YYSYMBOL_move_statement = 883,           /* move_statement  */
  YYSYMBOL_884_67 = 884,                   /* $@67  */
  YYSYMBOL_move_body = 885,                /* move_body  */
  YYSYMBOL_multiply_statement = 886,       /* multiply_statement  */
  YYSYMBOL_887_68 = 887,                   /* $@68  */
  YYSYMBOL_multiply_body = 888,            /* multiply_body  */
  YYSYMBOL_end_multiply = 889,             /* end_multiply  */
  YYSYMBOL_open_statement = 890,           /* open_statement  */
  YYSYMBOL_891_69 = 891,                   /* $@69  */
  YYSYMBOL_open_list = 892,                /* open_list  */
  YYSYMBOL_open_mode = 893,                /* open_mode  */
  YYSYMBOL_open_sharing = 894,             /* open_sharing  */
  YYSYMBOL_open_option = 895,              /* open_option  */
  YYSYMBOL_perform_statement = 896,        /* perform_statement  */
  YYSYMBOL_897_70 = 897,                   /* $@70  */
  YYSYMBOL_perform_body = 898,             /* perform_body  */
  YYSYMBOL_899_71 = 899,                   /* $@71  */
  YYSYMBOL_end_perform = 900,              /* end_perform  */
  YYSYMBOL_perform_procedure = 901,        /* perform_procedure  */
  YYSYMBOL_perform_option = 902,           /* perform_option  */
  YYSYMBOL_perform_test = 903,             /* perform_test  */
  YYSYMBOL_perform_varying_list = 904,     /* perform_varying_list  */
  YYSYMBOL_perform_varying = 905,          /* perform_varying  */
  YYSYMBOL_read_statement = 906,           /* read_statement  */
  YYSYMBOL_907_72 = 907,                   /* $@72  */
  YYSYMBOL_read_into = 908,                /* read_into  */
  YYSYMBOL_with_lock = 909,                /* with_lock  */
  YYSYMBOL_read_key = 910,                 /* read_key  */
  YYSYMBOL_read_handler = 911,             /* read_handler  */
  YYSYMBOL_end_read = 912,                 /* end_read  */
  YYSYMBOL_release_statement = 913,        /* release_statement  */
  YYSYMBOL_914_73 = 914,                   /* $@73  */
  YYSYMBOL_return_statement = 915,         /* return_statement  */
  YYSYMBOL_916_74 = 916,                   /* $@74  */
  YYSYMBOL_end_return = 917,               /* end_return  */
  YYSYMBOL_rewrite_statement = 918,        /* rewrite_statement  */
  YYSYMBOL_919_75 = 919,                   /* $@75  */
  YYSYMBOL_write_lock = 920,               /* write_lock  */
  YYSYMBOL_end_rewrite = 921,              /* end_rewrite  */
  YYSYMBOL_rollback_statement = 922,       /* rollback_statement  */
  YYSYMBOL_search_statement = 923,         /* search_statement  */
  YYSYMBOL_924_76 = 924,                   /* $@76  */
  YYSYMBOL_search_body = 925,              /* search_body  */
  YYSYMBOL_926_77 = 926,                   /* $@77  */
  YYSYMBOL_search_varying = 927,           /* search_varying  */
  YYSYMBOL_search_at_end = 928,            /* search_at_end  */
  YYSYMBOL_929_78 = 929,                   /* $@78  */
  YYSYMBOL_search_whens = 930,             /* search_whens  */
  YYSYMBOL_search_when = 931,              /* search_when  */
  YYSYMBOL_932_79 = 932,                   /* $@79  */
  YYSYMBOL_end_search = 933,               /* end_search  */
  YYSYMBOL_set_statement = 934,            /* set_statement  */
  YYSYMBOL_935_80 = 935,                   /* $@80  */
  YYSYMBOL_set_body = 936,                 /* set_body  */
  YYSYMBOL_set_environment = 937,          /* set_environment  */
  YYSYMBOL_set_to = 938,                   /* set_to  */
  YYSYMBOL_set_up_down = 939,              /* set_up_down  */
  YYSYMBOL_up_or_down = 940,               /* up_or_down  */
  YYSYMBOL_set_to_on_off_sequence = 941,   /* set_to_on_off_sequence  */
  YYSYMBOL_set_to_on_off = 942,            /* set_to_on_off  */
  YYSYMBOL_set_to_true_false_sequence = 943, /* set_to_true_false_sequence  */
  YYSYMBOL_set_to_true_false = 944,        /* set_to_true_false  */
  YYSYMBOL_sort_statement = 945,           /* sort_statement  */
  YYSYMBOL_946_81 = 946,                   /* $@81  */
  YYSYMBOL_sort_body = 947,                /* sort_body  */
  YYSYMBOL_948_82 = 948,                   /* $@82  */
  YYSYMBOL_sort_key_list = 949,            /* sort_key_list  */
  YYSYMBOL_opt_key_list = 950,             /* opt_key_list  */
  YYSYMBOL_sort_duplicates = 951,          /* sort_duplicates  */
  YYSYMBOL_sort_collating = 952,           /* sort_collating  */
  YYSYMBOL_sort_input = 953,               /* sort_input  */
  YYSYMBOL_sort_output = 954,              /* sort_output  */
  YYSYMBOL_start_statement = 955,          /* start_statement  */
  YYSYMBOL_956_83 = 956,                   /* $@83  */
  YYSYMBOL_957_84 = 957,                   /* @84  */
  YYSYMBOL_start_key = 958,                /* start_key  */
  YYSYMBOL_start_op = 959,                 /* start_op  */
  YYSYMBOL_end_start = 960,                /* end_start  */
  YYSYMBOL_stop_statement = 961,           /* stop_statement  */
  YYSYMBOL_962_85 = 962,                   /* $@85  */
  YYSYMBOL_963_86 = 963,                   /* $@86  */
  YYSYMBOL_stop_returning = 964,           /* stop_returning  */
  YYSYMBOL_string_statement = 965,         /* string_statement  */
  YYSYMBOL_966_87 = 966,                   /* $@87  */
  YYSYMBOL_string_item_list = 967,         /* string_item_list  */
  YYSYMBOL_string_item = 968,              /* string_item  */
  YYSYMBOL_opt_with_pointer = 969,         /* opt_with_pointer  */
  YYSYMBOL_end_string = 970,               /* end_string  */
  YYSYMBOL_subtract_statement = 971,       /* subtract_statement  */
  YYSYMBOL_972_88 = 972,                   /* $@88  */
  YYSYMBOL_subtract_body = 973,            /* subtract_body  */
  YYSYMBOL_end_subtract = 974,             /* end_subtract  */
  YYSYMBOL_suppress_statement = 975,       /* suppress_statement  */
  YYSYMBOL__printing = 976,                /* _printing  */
  YYSYMBOL_terminate_statement = 977,      /* terminate_statement  */
  YYSYMBOL_978_89 = 978,                   /* $@89  */
  YYSYMBOL_transform_statement = 979,      /* transform_statement  */
  YYSYMBOL_980_90 = 980,                   /* $@90  */
  YYSYMBOL_unlock_statement = 981,         /* unlock_statement  */
  YYSYMBOL_982_91 = 982,                   /* $@91  */
  YYSYMBOL_opt_record = 983,               /* opt_record  */
  YYSYMBOL_unstring_statement = 984,       /* unstring_statement  */
  YYSYMBOL_985_92 = 985,                   /* $@92  */
  YYSYMBOL_unstring_delimited = 986,       /* unstring_delimited  */
  YYSYMBOL_unstring_delimited_list = 987,  /* unstring_delimited_list  */
  YYSYMBOL_unstring_delimited_item = 988,  /* unstring_delimited_item  */
  YYSYMBOL_unstring_into = 989,            /* unstring_into  */
  YYSYMBOL_unstring_into_item = 990,       /* unstring_into_item  */
  YYSYMBOL_unstring_into_delimiter = 991,  /* unstring_into_delimiter  */
  YYSYMBOL_unstring_into_count = 992,      /* unstring_into_count  */
  YYSYMBOL_unstring_tallying = 993,        /* unstring_tallying  */
  YYSYMBOL_end_unstring = 994,             /* end_unstring  */
  YYSYMBOL_use_statement = 995,            /* use_statement  */
  YYSYMBOL_use_exception = 996,            /* use_exception  */
  YYSYMBOL_use_global = 997,               /* use_global  */
  YYSYMBOL_use_exception_target = 998,     /* use_exception_target  */
  YYSYMBOL__after = 999,                   /* _after  */
  YYSYMBOL__standard = 1000,               /* _standard  */
  YYSYMBOL_exception_or_error = 1001,      /* exception_or_error  */
  YYSYMBOL_exception_or_overflow = 1002,   /* exception_or_overflow  */
  YYSYMBOL_not_exception_or_overflow = 1003, /* not_exception_or_overflow  */
  YYSYMBOL__procedure = 1004,              /* _procedure  */
  YYSYMBOL_use_debugging = 1005,           /* use_debugging  */
  YYSYMBOL_use_debugging_target = 1006,    /* use_debugging_target  */
  YYSYMBOL_use_reporting = 1007,           /* use_reporting  */
  YYSYMBOL_write_statement = 1008,         /* write_statement  */
  YYSYMBOL_1009_93 = 1009,                 /* $@93  */
  YYSYMBOL_write_from = 1010,              /* write_from  */
  YYSYMBOL_write_option = 1011,            /* write_option  */
  YYSYMBOL_before_or_after = 1012,         /* before_or_after  */
  YYSYMBOL_write_handler = 1013,           /* write_handler  */
  YYSYMBOL_end_write = 1014,               /* end_write  */
  YYSYMBOL_on_accp_exception = 1015,       /* on_accp_exception  */
  YYSYMBOL_on_disp_exception = 1016,       /* on_disp_exception  */
  YYSYMBOL_opt_on_exception = 1017,        /* opt_on_exception  */
  YYSYMBOL_1018_94 = 1018,                 /* $@94  */
  YYSYMBOL_opt_not_on_exception = 1019,    /* opt_not_on_exception  */
  YYSYMBOL_1020_95 = 1020,                 /* $@95  */
  YYSYMBOL_on_size_error = 1021,           /* on_size_error  */
  YYSYMBOL_opt_on_size_error = 1022,       /* opt_on_size_error  */
  YYSYMBOL_1023_96 = 1023,                 /* $@96  */
  YYSYMBOL_opt_not_on_size_error = 1024,   /* opt_not_on_size_error  */
  YYSYMBOL_1025_97 = 1025,                 /* $@97  */
  YYSYMBOL_on_overflow = 1026,             /* on_overflow  */
  YYSYMBOL_opt_on_overflow = 1027,         /* opt_on_overflow  */
  YYSYMBOL_1028_98 = 1028,                 /* $@98  */
  YYSYMBOL_opt_not_on_overflow = 1029,     /* opt_not_on_overflow  */
  YYSYMBOL_1030_99 = 1030,                 /* $@99  */
  YYSYMBOL_at_end = 1031,                  /* at_end  */
  YYSYMBOL_at_end_sentence = 1032,         /* at_end_sentence  */
  YYSYMBOL_1033_100 = 1033,                /* $@100  */
  YYSYMBOL_not_at_end_sentence = 1034,     /* not_at_end_sentence  */
  YYSYMBOL_1035_101 = 1035,                /* $@101  */
  YYSYMBOL_at_eop = 1036,                  /* at_eop  */
  YYSYMBOL_at_eop_sentence = 1037,         /* at_eop_sentence  */
  YYSYMBOL_1038_102 = 1038,                /* $@102  */
  YYSYMBOL_not_at_eop_sentence = 1039,     /* not_at_eop_sentence  */
  YYSYMBOL_1040_103 = 1040,                /* $@103  */
  YYSYMBOL_opt_invalid_key = 1041,         /* opt_invalid_key  */
  YYSYMBOL_invalid_key = 1042,             /* invalid_key  */
  YYSYMBOL_invalid_key_sentence = 1043,    /* invalid_key_sentence  */
  YYSYMBOL_1044_104 = 1044,                /* $@104  */
  YYSYMBOL_not_invalid_key_sentence = 1045, /* not_invalid_key_sentence  */
  YYSYMBOL_1046_105 = 1046,                /* $@105  */
  YYSYMBOL__opt_scroll_lines = 1047,       /* _opt_scroll_lines  */
  YYSYMBOL_condition = 1048,               /* condition  */
  YYSYMBOL_expr = 1049,                    /* expr  */
  YYSYMBOL_partial_expr = 1050,            /* partial_expr  */
  YYSYMBOL_1051_106 = 1051,                /* $@106  */
  YYSYMBOL_expr_tokens = 1052,             /* expr_tokens  */
  YYSYMBOL_expr_token = 1053,              /* expr_token  */
  YYSYMBOL_eq = 1054,                      /* eq  */
  YYSYMBOL_gt = 1055,                      /* gt  */
  YYSYMBOL_lt = 1056,                      /* lt  */
  YYSYMBOL_ge = 1057,                      /* ge  */
  YYSYMBOL_le = 1058,                      /* le  */
  YYSYMBOL_exp_list = 1059,                /* exp_list  */
  YYSYMBOL_e_sep = 1060,                   /* e_sep  */
  YYSYMBOL_exp = 1061,                     /* exp  */
  YYSYMBOL_linage_counter = 1062,          /* linage_counter  */
  YYSYMBOL_arithmetic_x_list = 1063,       /* arithmetic_x_list  */
  YYSYMBOL_arithmetic_x = 1064,            /* arithmetic_x  */
  YYSYMBOL_record_name = 1065,             /* record_name  */
  YYSYMBOL_table_name = 1066,              /* table_name  */
  YYSYMBOL_file_name_list = 1067,          /* file_name_list  */
  YYSYMBOL_file_name = 1068,               /* file_name  */
  YYSYMBOL_mnemonic_name_list = 1069,      /* mnemonic_name_list  */
  YYSYMBOL_mnemonic_name = 1070,           /* mnemonic_name  */
  YYSYMBOL_procedure_name_list = 1071,     /* procedure_name_list  */
  YYSYMBOL_procedure_name = 1072,          /* procedure_name  */
  YYSYMBOL_label = 1073,                   /* label  */
  YYSYMBOL_integer_label = 1074,           /* integer_label  */
  YYSYMBOL_reference_list = 1075,          /* reference_list  */
  YYSYMBOL_reference = 1076,               /* reference  */
  YYSYMBOL_no_reference_list = 1077,       /* no_reference_list  */
  YYSYMBOL_opt_reference = 1078,           /* opt_reference  */
  YYSYMBOL_reference_or_literal = 1079,    /* reference_or_literal  */
  YYSYMBOL_undefined_word = 1080,          /* undefined_word  */
  YYSYMBOL_target_x_list = 1081,           /* target_x_list  */
  YYSYMBOL_target_x = 1082,                /* target_x  */
  YYSYMBOL_x_list = 1083,                  /* x_list  */
  YYSYMBOL_x = 1084,                       /* x  */
  YYSYMBOL_arith_x = 1085,                 /* arith_x  */
  YYSYMBOL_prog_or_entry = 1086,           /* prog_or_entry  */
  YYSYMBOL_alnum_or_id = 1087,             /* alnum_or_id  */
  YYSYMBOL_simple_value = 1088,            /* simple_value  */
  YYSYMBOL_simple_all_value = 1089,        /* simple_all_value  */
  YYSYMBOL_id_or_lit = 1090,               /* id_or_lit  */
  YYSYMBOL_id_or_lit_or_func = 1091,       /* id_or_lit_or_func  */
  YYSYMBOL_num_id_or_lit = 1092,           /* num_id_or_lit  */
  YYSYMBOL_identifier = 1093,              /* identifier  */
  YYSYMBOL_identifier_1 = 1094,            /* identifier_1  */
  YYSYMBOL_qualified_word = 1095,          /* qualified_word  */
  YYSYMBOL_subref = 1096,                  /* subref  */
  YYSYMBOL_refmod = 1097,                  /* refmod  */
  YYSYMBOL_integer = 1098,                 /* integer  */
  YYSYMBOL_literal = 1099,                 /* literal  */
  YYSYMBOL_basic_literal = 1100,           /* basic_literal  */
  YYSYMBOL_basic_value = 1101,             /* basic_value  */
  YYSYMBOL_function = 1102,                /* function  */
  YYSYMBOL_func_refmod = 1103,             /* func_refmod  */
  YYSYMBOL_func_args = 1104,               /* func_args  */
  YYSYMBOL_list_func_args = 1105,          /* list_func_args  */
  YYSYMBOL_trim_args = 1106,               /* trim_args  */
  YYSYMBOL_numvalc_args = 1107,            /* numvalc_args  */
  YYSYMBOL_locale_dt_args = 1108,          /* locale_dt_args  */
  YYSYMBOL_not_const_word = 1109,          /* not_const_word  */
  YYSYMBOL_flag_all = 1110,                /* flag_all  */
  YYSYMBOL_flag_duplicates = 1111,         /* flag_duplicates  */
  YYSYMBOL_flag_initialized = 1112,        /* flag_initialized  */
  YYSYMBOL_flag_next = 1113,               /* flag_next  */
  YYSYMBOL_flag_not = 1114,                /* flag_not  */
  YYSYMBOL_flag_optional = 1115,           /* flag_optional  */
  YYSYMBOL_flag_rounded = 1116,            /* flag_rounded  */
  YYSYMBOL_flag_separate = 1117,           /* flag_separate  */
  YYSYMBOL_in_of = 1118,                   /* in_of  */
  YYSYMBOL_records = 1119,                 /* records  */
  YYSYMBOL_with_dups = 1120,               /* with_dups  */
  YYSYMBOL_coll_sequence = 1121,           /* coll_sequence  */
  YYSYMBOL__advancing = 1122,              /* _advancing  */
  YYSYMBOL__also = 1123,                   /* _also  */
  YYSYMBOL__are = 1124,                    /* _are  */
  YYSYMBOL__area = 1125,                   /* _area  */
  YYSYMBOL__as = 1126,                     /* _as  */
  YYSYMBOL__at = 1127,                     /* _at  */
  YYSYMBOL__binary = 1128,                 /* _binary  */
  YYSYMBOL__by = 1129,                     /* _by  */
  YYSYMBOL__character = 1130,              /* _character  */
  YYSYMBOL__characters = 1131,             /* _characters  */
  YYSYMBOL__contains = 1132,               /* _contains  */
  YYSYMBOL__data = 1133,                   /* _data  */
  YYSYMBOL__file = 1134,                   /* _file  */
  YYSYMBOL__for = 1135,                    /* _for  */
  YYSYMBOL__from = 1136,                   /* _from  */
  YYSYMBOL__in = 1137,                     /* _in  */
  YYSYMBOL__is = 1138,                     /* _is  */
  YYSYMBOL__is_are = 1139,                 /* _is_are  */
  YYSYMBOL__key = 1140,                    /* _key  */
  YYSYMBOL__line_or_lines = 1141,          /* _line_or_lines  */
  YYSYMBOL__lines = 1142,                  /* _lines  */
  YYSYMBOL__literal = 1143,                /* _literal  */
  YYSYMBOL__mode = 1144,                   /* _mode  */
  YYSYMBOL__number = 1145,                 /* _number  */
  YYSYMBOL__of = 1146,                     /* _of  */
  YYSYMBOL__on = 1147,                     /* _on  */
  YYSYMBOL__in_order = 1148,               /* _in_order  */
  YYSYMBOL__other = 1149,                  /* _other  */
  YYSYMBOL__program = 1150,                /* _program  */
  YYSYMBOL__record = 1151,                 /* _record  */
  YYSYMBOL__right = 1152,                  /* _right  */
  YYSYMBOL__set = 1153,                    /* _set  */
  YYSYMBOL__sign = 1154,                   /* _sign  */
  YYSYMBOL__sign_is = 1155,                /* _sign_is  */
  YYSYMBOL__size = 1156,                   /* _size  */
  YYSYMBOL__status = 1157,                 /* _status  */
  YYSYMBOL__tape = 1158,                   /* _tape  */
  YYSYMBOL__than = 1159,                   /* _than  */
  YYSYMBOL__then = 1160,                   /* _then  */
  YYSYMBOL__times = 1161,                  /* _times  */
  YYSYMBOL__to = 1162,                     /* _to  */
  YYSYMBOL__when = 1163,                   /* _when  */
  YYSYMBOL__with = 1164                    /* _with  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   5548

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  463
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  702
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1546
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2284

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   704


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int16 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   462,     2,
     458,   457,   451,   449,     2,   450,   455,   452,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   461,     2,
     460,   456,   459,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,   454,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   453
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   772,   772,   772,   816,   817,   821,   822,   827,   828,
     826,   841,   842,   840,   855,   856,   854,   861,   862,   863,
     866,   867,   895,   921,   953,   952,   996,  1040,  1041,  1045,
    1046,  1049,  1050,  1054,  1061,  1068,  1072,  1076,  1088,  1089,
    1099,  1100,  1109,  1110,  1114,  1115,  1116,  1117,  1126,  1129,
    1130,  1131,  1132,  1136,  1143,  1152,  1155,  1156,  1157,  1158,
    1162,  1163,  1167,  1168,  1169,  1173,  1180,  1181,  1185,  1192,
    1204,  1207,  1208,  1212,  1213,  1217,  1221,  1228,  1229,  1239,
    1242,  1243,  1244,  1248,  1249,  1253,  1254,  1255,  1256,  1257,
    1258,  1259,  1260,  1261,  1262,  1263,  1270,  1281,  1280,  1292,
    1291,  1300,  1314,  1328,  1342,  1358,  1359,  1363,  1364,  1368,
    1379,  1380,  1388,  1387,  1399,  1400,  1401,  1402,  1403,  1411,
    1412,  1417,  1418,  1420,  1419,  1431,  1432,  1436,  1437,  1438,
    1439,  1440,  1441,  1445,  1446,  1447,  1448,  1449,  1450,  1457,
    1468,  1480,  1481,  1485,  1486,  1493,  1502,  1503,  1507,  1508,
    1522,  1537,  1604,  1615,  1622,  1629,  1635,  1642,  1643,  1647,
    1646,  1656,  1655,  1671,  1672,  1675,  1676,  1681,  1680,  1701,
    1702,  1706,  1707,  1708,  1709,  1710,  1711,  1712,  1713,  1714,
    1715,  1716,  1717,  1718,  1719,  1720,  1727,  1731,  1736,  1743,
    1744,  1745,  1748,  1749,  1753,  1760,  1761,  1768,  1788,  1789,
    1795,  1799,  1800,  1801,  1808,  1828,  1871,  1871,  1875,  1879,
    1895,  1896,  1897,  1903,  1913,  1922,  1924,  1925,  1931,  1935,
    1936,  1937,  1940,  1941,  1942,  1946,  1950,  1951,  1957,  1958,
    1962,  1971,  1980,  1989,  1998,  2013,  2023,  2030,  2039,  2077,
    2084,  2085,  2092,  2096,  2097,  2098,  2104,  2111,  2112,  2115,
    2116,  2117,  2118,  2122,  2123,  2127,  2128,  2134,  2159,  2160,
    2161,  2162,  2168,  2175,  2176,  2180,  2183,  2184,  2190,  2191,
    2192,  2196,  2200,  2204,  2208,  2212,  2222,  2223,  2237,  2238,
    2238,  2241,  2240,  2253,  2254,  2258,  2270,  2279,  2283,  2284,
    2294,  2293,  2311,  2312,  2316,  2323,  2330,  2331,  2332,  2333,
    2334,  2335,  2336,  2337,  2338,  2339,  2346,  2350,  2350,  2350,
    2356,  2368,  2393,  2417,  2418,  2425,  2426,  2430,  2431,  2438,
    2445,  2446,  2453,  2457,  2466,  2467,  2473,  2483,  2501,  2502,
    2506,  2507,  2508,  2512,  2519,  2526,  2536,  2543,  2561,  2565,
    2576,  2577,  2577,  2588,  2589,  2593,  2593,  2610,  2611,  2613,
    2617,  2619,  2618,  2644,  2643,  2669,  2673,  2680,  2682,  2704,
    2709,  2715,  2724,  2732,  2733,  2741,  2742,  2743,  2747,  2767,
    2771,  2780,  2781,  2782,  2783,  2784,  2785,  2786,  2787,  2788,
    2789,  2790,  2791,  2792,  2793,  2794,  2801,  2823,  2845,  2846,
    2858,  2878,  2885,  2886,  2890,  2891,  2892,  2893,  2894,  2895,
    2896,  2897,  2898,  2899,  2900,  2901,  2906,  2911,  2912,  2913,
    2914,  2915,  2916,  2917,  2918,  2919,  2920,  2921,  2922,  2923,
    2924,  2925,  2926,  2927,  2928,  2929,  2937,  2945,  2953,  2960,
    2965,  2975,  2976,  2977,  2981,  2998,  2999,  3002,  3003,  3009,
    3009,  3012,  3036,  3052,  3053,  3057,  3058,  3061,  3061,  3064,
    3071,  3072,  3077,  3087,  3094,  3097,  3098,  3099,  3106,  3113,
    3138,  3142,  3142,  3147,  3148,  3152,  3153,  3156,  3157,  3170,
    3182,  3202,  3216,  3218,  3217,  3237,  3238,  3238,  3251,  3253,
    3252,  3264,  3265,  3269,  3270,  3279,  3286,  3289,  3293,  3297,
    3298,  3299,  3306,  3307,  3311,  3314,  3314,  3317,  3318,  3324,
    3329,  3330,  3333,  3334,  3337,  3338,  3341,  3342,  3345,  3346,
    3350,  3351,  3352,  3356,  3357,  3360,  3361,  3365,  3369,  3370,
    3374,  3375,  3376,  3377,  3378,  3379,  3380,  3381,  3382,  3383,
    3384,  3385,  3386,  3387,  3388,  3389,  3393,  3397,  3398,  3399,
    3400,  3401,  3402,  3403,  3407,  3411,  3412,  3413,  3417,  3418,
    3422,  3426,  3431,  3435,  3439,  3443,  3444,  3448,  3449,  3453,
    3454,  3455,  3458,  3458,  3458,  3461,  3465,  3468,  3468,  3471,
    3478,  3479,  3479,  3489,  3491,  3501,  3490,  3528,  3530,  3529,
    3536,  3535,  3544,  3545,  3550,  3557,  3559,  3563,  3573,  3575,
    3583,  3591,  3620,  3651,  3653,  3663,  3668,  3679,  3680,  3680,
    3707,  3708,  3712,  3713,  3714,  3715,  3731,  3743,  3774,  3811,
    3823,  3826,  3827,  3836,  3840,  3836,  3853,  3874,  3878,  3879,
    3880,  3881,  3882,  3883,  3884,  3885,  3886,  3887,  3888,  3889,
    3890,  3891,  3892,  3893,  3894,  3895,  3896,  3897,  3898,  3899,
    3900,  3901,  3902,  3903,  3904,  3905,  3906,  3907,  3908,  3909,
    3910,  3911,  3912,  3913,  3914,  3915,  3916,  3917,  3918,  3919,
    3920,  3921,  3922,  3923,  3924,  3925,  3926,  3927,  3950,  3949,
    3962,  3966,  3970,  3974,  3978,  3982,  3986,  3990,  3994,  3998,
    4002,  4006,  4010,  4014,  4018,  4022,  4026,  4033,  4034,  4035,
    4036,  4037,  4038,  4042,  4046,  4047,  4050,  4051,  4055,  4056,
    4060,  4061,  4062,  4063,  4064,  4065,  4066,  4067,  4071,  4075,
    4079,  4084,  4085,  4086,  4087,  4088,  4089,  4093,  4094,  4103,
    4103,  4109,  4113,  4117,  4123,  4124,  4128,  4129,  4138,  4138,
    4143,  4147,  4154,  4155,  4164,  4170,  4171,  4175,  4175,  4183,
    4183,  4193,  4195,  4194,  4203,  4204,  4209,  4216,  4223,  4225,
    4229,  4237,  4248,  4249,  4250,  4255,  4259,  4258,  4270,  4274,
    4273,  4284,  4285,  4294,  4294,  4298,  4299,  4303,  4315,  4315,
    4319,  4320,  4331,  4332,  4333,  4334,  4335,  4338,  4338,  4346,
    4346,  4352,  4359,  4360,  4363,  4363,  4370,  4383,  4396,  4396,
    4407,  4408,  4417,  4417,  4437,  4436,  4449,  4453,  4457,  4461,
    4465,  4469,  4473,  4478,  4482,  4489,  4490,  4491,  4495,  4496,
    4501,  4502,  4503,  4504,  4505,  4506,  4507,  4508,  4509,  4510,
    4514,  4518,  4522,  4527,  4528,  4532,  4533,  4542,  4542,  4548,
    4552,  4556,  4560,  4564,  4571,  4572,  4581,  4581,  4603,  4602,
    4621,  4622,  4633,  4642,  4647,  4655,  4684,  4685,  4691,  4690,
    4706,  4710,  4709,  4724,  4725,  4730,  4731,  4742,  4771,  4772,
    4773,  4776,  4777,  4781,  4782,  4791,  4791,  4796,  4797,  4805,
    4813,  4821,  4839,  4864,  4864,  4877,  4877,  4890,  4890,  4899,
    4903,  4916,  4916,  4929,  4931,  4929,  4942,  4947,  4951,  4950,
    4964,  4965,  4974,  4974,  4982,  4983,  4987,  4988,  4989,  4993,
    4994,  4999,  5000,  5005,  5009,  5010,  5011,  5012,  5013,  5014,
    5015,  5019,  5020,  5029,  5029,  5042,  5041,  5051,  5052,  5053,
    5057,  5058,  5062,  5063,  5064,  5070,  5070,  5075,  5076,  5080,
    5081,  5082,  5083,  5084,  5085,  5091,  5095,  5096,  5100,  5105,
    5109,  5110,  5111,  5112,  5113,  5117,  5143,  5156,  5157,  5161,
    5161,  5169,  5169,  5179,  5179,  5184,  5188,  5200,  5200,  5206,
    5210,  5217,  5218,  5227,  5227,  5231,  5232,  5246,  5247,  5248,
    5249,  5253,  5254,  5258,  5259,  5260,  5272,  5272,  5277,  5282,
    5281,  5291,  5298,  5299,  5303,  5308,  5317,  5320,  5324,  5329,
    5336,  5343,  5344,  5348,  5349,  5354,  5366,  5366,  5395,  5396,
    5400,  5401,  5405,  5409,  5413,  5417,  5424,  5425,  5431,  5432,
    5433,  5437,  5438,  5447,  5447,  5462,  5462,  5473,  5474,  5483,
    5483,  5500,  5501,  5505,  5512,  5513,  5522,  5535,  5535,  5541,
    5546,  5545,  5556,  5557,  5561,  5563,  5562,  5573,  5574,  5579,
    5578,  5589,  5590,  5599,  5599,  5604,  5605,  5606,  5607,  5608,
    5614,  5623,  5627,  5636,  5643,  5644,  5650,  5651,  5655,  5664,
    5665,  5669,  5673,  5685,  5685,  5691,  5690,  5707,  5710,  5731,
    5732,  5735,  5736,  5740,  5741,  5746,  5751,  5759,  5771,  5776,
    5784,  5800,  5801,  5800,  5821,  5822,  5830,  5831,  5832,  5833,
    5834,  5838,  5839,  5848,  5848,  5853,  5853,  5860,  5861,  5862,
    5871,  5871,  5880,  5881,  5885,  5886,  5887,  5891,  5892,  5896,
    5897,  5906,  5906,  5912,  5916,  5920,  5927,  5928,  5937,  5944,
    5945,  5953,  5953,  5966,  5966,  5982,  5982,  5991,  5993,  5994,
    6003,  6003,  6013,  6014,  6019,  6020,  6025,  6032,  6033,  6038,
    6045,  6046,  6050,  6051,  6055,  6056,  6060,  6061,  6070,  6071,
    6072,  6076,  6100,  6103,  6111,  6121,  6126,  6131,  6136,  6143,
    6144,  6147,  6148,  6152,  6152,  6156,  6156,  6160,  6160,  6163,
    6164,  6168,  6175,  6176,  6180,  6192,  6192,  6209,  6210,  6215,
    6218,  6222,  6226,  6233,  6234,  6237,  6238,  6239,  6243,  6244,
    6257,  6265,  6272,  6274,  6273,  6283,  6285,  6284,  6299,  6303,
    6305,  6304,  6315,  6317,  6316,  6333,  6339,  6341,  6340,  6350,
    6352,  6351,  6367,  6372,  6377,  6387,  6386,  6398,  6397,  6413,
    6418,  6423,  6433,  6432,  6444,  6443,  6458,  6459,  6463,  6468,
    6473,  6483,  6482,  6494,  6493,  6510,  6513,  6525,  6532,  6539,
    6539,  6549,  6550,  6552,  6553,  6554,  6555,  6556,  6557,  6559,
    6560,  6561,  6562,  6563,  6564,  6566,  6567,  6569,  6570,  6571,
    6574,  6576,  6577,  6578,  6580,  6581,  6582,  6584,  6585,  6587,
    6588,  6589,  6590,  6591,  6593,  6594,  6595,  6596,  6597,  6598,
    6600,  6601,  6602,  6603,  6604,  6605,  6607,  6608,  6611,  6611,
    6611,  6612,  6612,  6613,  6613,  6614,  6614,  6614,  6615,  6615,
    6615,  6620,  6621,  6624,  6625,  6626,  6630,  6631,  6632,  6633,
    6634,  6635,  6636,  6637,  6638,  6649,  6661,  6676,  6677,  6682,
    6688,  6710,  6730,  6734,  6750,  6764,  6765,  6770,  6776,  6777,
    6782,  6791,  6792,  6793,  6797,  6808,  6809,  6813,  6823,  6824,
    6828,  6829,  6833,  6834,  6840,  6860,  6861,  6865,  6866,  6870,
    6871,  6875,  6876,  6877,  6878,  6879,  6880,  6881,  6882,  6883,
    6887,  6888,  6889,  6890,  6891,  6892,  6893,  6897,  6898,  6902,
    6903,  6907,  6908,  6912,  6913,  6924,  6925,  6929,  6930,  6931,
    6935,  6936,  6937,  6945,  6949,  6950,  6951,  6952,  6956,  6957,
    6961,  6971,  6989,  7016,  7028,  7029,  7039,  7040,  7044,  7045,
    7046,  7047,  7048,  7049,  7050,  7058,  7062,  7066,  7070,  7074,
    7078,  7082,  7086,  7090,  7094,  7098,  7102,  7109,  7110,  7111,
    7115,  7116,  7120,  7121,  7126,  7133,  7140,  7150,  7157,  7167,
    7174,  7188,  7198,  7199,  7203,  7204,  7208,  7209,  7213,  7214,
    7215,  7219,  7220,  7224,  7225,  7229,  7230,  7234,  7235,  7242,
    7242,  7243,  7243,  7244,  7244,  7245,  7245,  7247,  7247,  7248,
    7248,  7249,  7249,  7250,  7250,  7251,  7251,  7252,  7252,  7253,
    7253,  7254,  7254,  7255,  7255,  7256,  7256,  7257,  7257,  7258,
    7258,  7259,  7259,  7260,  7260,  7261,  7261,  7262,  7262,  7263,
    7263,  7264,  7264,  7264,  7265,  7265,  7266,  7266,  7266,  7267,
    7267,  7268,  7268,  7269,  7269,  7270,  7270,  7271,  7271,  7272,
    7272,  7273,  7273,  7273,  7274,  7274,  7275,  7275,  7276,  7276,
    7277,  7277,  7278,  7278,  7279,  7279,  7280,  7280,  7280,  7281,
    7281,  7282,  7282,  7283,  7283,  7284,  7284,  7285,  7285,  7286,
    7286,  7287,  7287,  7289,  7289,  7290,  7290
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "ACCEPT", "ACCESS",
  "ADD", "ADDRESS", "ADVANCING", "AFTER", "ALL", "ALLOCATE", "ALPHABET",
  "ALPHABETIC", "\"ALPHABETIC-LOWER\"", "\"ALPHABETIC-UPPER\"",
  "ALPHANUMERIC", "\"ALPHANUMERIC-EDITED\"", "ALSO", "ALTER", "ALTERNATE",
  "AND", "ANY", "APPLY", "ARE", "AREA", "\"ARGUMENT-NUMBER\"",
  "\"ARGUMENT-VALUE\"", "AS", "ASCENDING", "ASSIGN", "AT", "AUTO",
  "AUTOMATIC", "\"BACKGROUND-COLOR\"", "BASED", "BEFORE", "BELL", "BINARY",
  "\"BINARY-C-LONG\"", "\"BINARY-CHAR\"", "\"BINARY-DOUBLE\"",
  "\"BINARY-LONG\"", "\"BINARY-SHORT\"", "BLANK", "\"BLANK-LINE\"",
  "\"BLANK-SCREEN\"", "BLINK", "BLOCK", "BOTTOM", "BY", "\"BYTE-LENGTH\"",
  "CALL", "CANCEL", "CH", "CHAINING", "CHARACTER", "CHARACTERS", "CLASS",
  "CLASS_NAME", "CLOSE", "\"CLOSE-NOFEED\"", "CODE", "\"CODE-SET\"",
  "COLLATING", "COL", "COLS", "COLUMN", "COLUMNS", "COMMA",
  "\"COMMAND-LINE\"", "\"comma delimiter\"", "COMMIT",
  "\"COMMITMENT-CONTROL\"", "COMMON", "COMP", "COMPUTE", "\"COMP-1\"",
  "\"COMP-2\"", "\"COMP-3\"", "\"COMP-4\"", "\"COMP-5\"", "\"COMP-X\"",
  "\"FUNCTION CONCATENATE\"", "CONFIGURATION", "CONSTANT", "CONTAINS",
  "CONTENT", "CONTINUE", "CONTROL", "CONTROLS", "\"CONTROL FOOTING\"",
  "\"CONTROL HEADING\"", "CONVERTING", "\"CORE-INDEX\"", "CORRESPONDING",
  "COUNT", "CRT", "CURRENCY", "\"FUNCTION CURRENT-DATE\"", "CURSOR",
  "CYCLE", "\"CYL-OVERFLOW\"", "DATA", "DATE", "DAY", "\"DAY-OF-WEEK\"",
  "DE", "DEBUGGING", "\"DECIMAL-POINT\"", "DECLARATIVES", "DEFAULT",
  "DELETE", "DELIMITED", "DELIMITER", "DEPENDING", "DESCENDING", "DETAIL",
  "DISK", "DISPLAY", "DIVIDE", "DIVISION", "DOWN", "DUPLICATES", "DYNAMIC",
  "EBCDIC", "ELSE", "END", "\"END-ACCEPT\"", "\"END-ADD\"", "\"END-CALL\"",
  "\"END-COMPUTE\"", "\"END-DELETE\"", "\"END-DISPLAY\"", "\"END-DIVIDE\"",
  "\"END-EVALUATE\"", "\"END FUNCTION\"", "\"END-IF\"", "\"END-MULTIPLY\"",
  "\"END-PERFORM\"", "\"END PROGRAM\"", "\"END-READ\"", "\"END-RETURN\"",
  "\"END-REWRITE\"", "\"END-SEARCH\"", "\"END-START\"", "\"END-STRING\"",
  "\"END-SUBTRACT\"", "\"END-UNSTRING\"", "\"END-WRITE\"", "ENTRY",
  "ENVIRONMENT", "\"ENVIRONMENT-NAME\"", "\"ENVIRONMENT-VALUE\"", "EOL",
  "EOP", "EOS", "EQUAL", "EQUALS", "ERASE", "ERROR", "ESCAPE", "EVALUATE",
  "\"EVENT-STATUS\"", "EXCEPTION", "EXCLUSIVE", "EXIT", "EXTEND",
  "EXTERNAL", "FD", "\"FILE-CONTROL\"", "\"FILE-ID\"", "FILLER", "FINAL",
  "FIRST", "FOOTING", "FOR", "\"FOREGROUND-COLOR\"", "FOREVER",
  "\"FORMS-OVERLAY\"", "FREE", "FROM", "FULL", "FUNCTION",
  "\"FUNCTION-ID\"", "\"FUNCTION\"", "GE", "GENERATE", "GIVING", "GLOBAL",
  "GO", "GOBACK", "GREATER", "GROUP", "HEADING", "HIGHLIGHT",
  "\"HIGH-VALUE\"", "IDENTIFICATION", "IF", "IGNORE", "IGNORING", "IN",
  "INDEX", "INDEXED", "INDICATE", "INITIALIZE", "INITIALIZED", "INITIATE",
  "INPUT", "\"INPUT-OUTPUT\"", "INSPECT", "INTO", "INTRINSIC", "INVALID",
  "\"INVALID KEY\"", "IS", "\"I-O\"", "\"I-O-CONTROL\"", "JUSTIFIED",
  "KEY", "LABEL", "LAST", "\"LAST DETAIL\"", "LE", "LEADING", "LEFT",
  "LENGTH", "LESS", "LEVEL_NUMBER_WORD", "LEVEL88_NUMBER_WORD", "LIMIT",
  "LIMITS", "LINAGE", "\"LINAGE-COUNTER\"", "LINE", "LINES", "LINKAGE",
  "\"Literal\"", "LOCALE", "\"FUNCTION LOCALE\"", "\"LOCAL-STORAGE\"",
  "LOCK", "\"FUNCTION LOWER-CASE\"", "LOWLIGHT", "\"LOW-VALUE\"", "MANUAL",
  "MEMORY", "MERGE", "MINUS", "\"MNEMONIC NAME\"", "MODE", "MOVE",
  "MULTIPLE", "MULTIPLY", "NATIONAL", "\"NATIONAL-EDITED\"", "NATIVE",
  "NE", "NEGATIVE", "NEXT", "\"NEXT SENTENCE\"", "NO", "NOMINAL", "NOT",
  "\"NOT END\"", "\"NOT EOP\"", "\"NOT EXCEPTION\"", "\"NOT INVALID KEY\"",
  "\"NOT OVERFLOW\"", "\"NOT SIZE ERROR\"", "\"NO ADVANCING\"", "NUMBER",
  "NUMBERS", "NUMERIC", "\"NUMERIC-EDITED\"", "\"FUNCTION NUMVALC\"",
  "\"OBJECT-COMPUTER\"", "OCCURS", "OF", "OFF", "OMITTED", "ON", "ONLY",
  "OPEN", "OPTIONAL", "OR", "ORDER", "ORGANIZATION", "OTHER", "OUTPUT",
  "OVERFLOW", "OVERLINE", "\"PACKED-DECIMAL\"", "PADDING", "PAGE",
  "\"PAGE FOOTING\"", "\"PAGE HEADING\"", "PARAGRAPH", "PERFORM",
  "PICTURE", "PLUS", "POINTER", "POSITION", "POSITIVE", "PRESENT",
  "PREVIOUS", "PRINTER", "PRINTING", "PROCEDURE", "PROCEDURES", "PROCEED",
  "PROGRAM", "\"PROGRAM-ID\"", "\"Program name\"", "\"PROGRAM-POINTER\"",
  "PROMPT", "QUOTE", "RANDOM", "RD", "READ", "RECORD", "RECORDING",
  "RECORDS", "RECURSIVE", "REDEFINES", "REEL", "REFERENCE", "RELATIVE",
  "RELEASE", "REMAINDER", "REMOVAL", "RENAMES", "REPLACING", "REPORT",
  "REPORTING", "REPORTS", "\"REPORT FOOTING\"", "\"REPORT HEADING\"",
  "REPOSITORY", "REQUIRED", "RESERVE", "RETURN", "RETURNING",
  "\"FUNCTION REVERSE\"", "\"REVERSE-VIDEO\"", "REWIND", "REWRITE",
  "RIGHT", "ROLLBACK", "ROUNDED", "RUN", "SAME", "SCREEN",
  "\"SCREEN-CONTROL\"", "SCROLL", "SD", "SEARCH", "SECTION", "SECURE",
  "\"SEGMENT-LIMIT\"", "SELECT", "\"semi-colon\"", "SENTENCE", "SEPARATE",
  "SEQUENCE", "SEQUENTIAL", "SET", "SHARING", "SIGN", "SIGNED",
  "\"SIGNED-INT\"", "\"SIGNED-LONG\"", "\"SIGNED-SHORT\"", "SIZE",
  "\"SIZE ERROR\"", "SORT", "\"SORT-MERGE\"", "SOURCE",
  "\"SOURCE-COMPUTER\"", "SPACE", "\"SPECIAL-NAMES\"", "STANDARD",
  "\"STANDARD-1\"", "\"STANDARD-2\"", "START", "STATUS", "STOP", "STRING",
  "\"FUNCTION SUBSTITUTE\"", "\"FUNCTION SUBSTITUTE-CASE\"", "SUBTRACT",
  "SUM", "SUPPRESS", "SYMBOLIC", "SYNCHRONIZED", "TALLYING", "TAPE",
  "TERMINATE", "TEST", "THAN", "THEN", "THRU", "TIME", "TIMES", "TO",
  "\"FALSE\"", "\"FILE\"", "\"INITIAL\"", "\"NULL\"", "\"TRUE\"", "TOP",
  "TRACKS", "TRAILING", "TRANSFORM", "\"FUNCTION TRIM\"", "TYPE",
  "UNDERLINE", "UNIT", "UNLOCK", "UNSIGNED", "\"UNSIGNED-INT\"",
  "\"UNSIGNED-LONG\"", "\"UNSIGNED-SHORT\"", "UNSTRING", "UNTIL", "UP",
  "UPDATE", "UPON", "\"UPON ARGUMENT-NUMBER\"", "\"UPON COMMAND-LINE\"",
  "\"UPON ENVIRONMENT-NAME\"", "\"UPON ENVIRONMENT-VALUE\"",
  "\"FUNCTION UPPER-CASE\"", "USAGE", "USE", "USING", "VALUE", "VARYING",
  "WAIT", "WHEN", "\"FUNCTION WHEN-COMPILED\"", "\"WHEN OTHER\"", "WITH",
  "\"Identifier\"", "WORDS", "\"WORKING-STORAGE\"", "WRITE", "YYYYDDD",
  "YYYYMMDD", "ZERO", "'+'", "'-'", "'*'", "'/'", "UNARY_SIGN", "'^'",
  "'.'", "'='", "')'", "'('", "'>'", "'<'", "':'", "'&'", "$accept",
  "start", "$@1", "nested_list", "source_element", "program_definition",
  "$@2", "$@3", "program_mandatory", "$@4", "$@5", "function_definition",
  "$@6", "$@7", "nested_prog", "end_program", "end_mandatory",
  "end_function", "identification_division", "$@8", "function_division",
  "program_name", "as_literal", "program_type", "program_type_clause",
  "_init_or_recurs", "environment_division", "configuration_section",
  "configuration_list", "configuration_paragraph",
  "source_computer_paragraph", "source_computer_entry",
  "with_debugging_mode", "computer_name", "object_computer_paragraph",
  "object_computer_entry", "object_clauses_list", "object_clauses",
  "object_computer_memory", "object_char_or_word",
  "object_computer_sequence", "object_computer_segment",
  "repository_paragraph", "opt_repository", "repository_list",
  "repository_name", "repository_literal_list", "special_names_paragraph",
  "opt_special_names", "special_name_list", "special_name",
  "mnemonic_name_clause", "$@9", "$@10",
  "special_name_mnemonic_on_off_list",
  "special_name_mnemonic_on_off_list_mandatory",
  "special_name_mnemonic_on_off", "on_or_off", "alphabet_name_clause",
  "$@11", "alphabet_definition", "alphabet_literal_list",
  "alphabet_literal", "@12", "alphabet_also_sequence", "alphabet_lits",
  "alphabet_also_literal", "symbolic_characters_clause",
  "symbolic_characters_list", "char_list", "integer_list",
  "class_name_clause", "class_item_list", "class_item", "locale_clause",
  "currency_sign_clause", "decimal_point_clause", "cursor_clause",
  "crt_status_clause", "screen_control", "event_status",
  "input_output_section", "$@13", "$@14", "file_control_paragraph",
  "file_control_sequence", "file_control_entry", "$@15",
  "select_clause_sequence", "select_clause", "assign_clause", "_device",
  "_ext_clause", "assignment_name", "assignment_device_name_list",
  "access_mode_clause", "access_mode", "alternative_record_key_clause",
  "split_key_list", "$@16", "split_key", "key_is_eq",
  "collating_sequence_clause", "file_status_clause", "file_or_sort",
  "lock_mode_clause", "lock_mode", "lock_with", "lock_records",
  "organization_clause", "organization", "padding_character_clause",
  "record_delimiter_clause", "record_key_clause", "relative_key_clause",
  "reserve_clause", "sharing_clause", "sharing_option",
  "nominal_key_clause", "i_o_control_paragraph", "opt_i_o_control",
  "i_o_control_list", "i_o_control_clause", "same_clause", "same_option",
  "multiple_file_tape_clause", "multiple_file_list", "multiple_file",
  "multiple_file_position", "apply_clause_list", "apply_clause",
  "data_division", "file_section", "$@17", "$@18",
  "file_description_sequence", "file_description",
  "file_description_sequence_without_type", "file_type",
  "file_description_entry", "@19", "file_description_clause_sequence",
  "file_description_clause", "block_contains_clause",
  "_records_or_characters", "record_clause", "record_depending",
  "opt_from_integer", "opt_to_integer", "label_records_clause",
  "label_option", "value_of_clause", "valueof_name", "data_records_clause",
  "linage_clause", "linage_sequence", "linage_lines", "linage_footing",
  "linage_top", "linage_bottom", "recording_mode_clause",
  "code_set_clause", "report_clause", "working_storage_section", "$@20",
  "record_description_list", "record_description_list_1", "$@21",
  "record_description_list_2", "data_description", "$@22", "$@23",
  "level_number", "level_number_88", "_maybe_next_level_number",
  "entry_name", "const_name", "const_global", "lit_or_length",
  "constant_entry", "data_description_clause_sequence",
  "data_description_clause", "redefines_clause", "external_clause",
  "as_extname", "global_clause", "picture_clause", "usage_clause", "usage",
  "sign_clause", "occurs_key_spec", "occurs_clause", "occurs_to_integer",
  "occurs_depending", "_occurs_keys", "occurs_keys", "occurs_key",
  "occurs_key_list", "ascending_or_descending", "_occurs_indexed",
  "occurs_indexed", "occurs_index_list", "occurs_index",
  "justified_clause", "synchronized_clause", "left_or_right",
  "blank_clause", "based_clause", "value_clause", "value_cond_clause",
  "$@24", "value_item_list", "value_item", "false_is", "renames_clause",
  "any_length_clause", "local_storage_section", "$@25", "linkage_section",
  "$@26", "report_section", "$@27", "opt_report_description_list",
  "report_description_list", "report_description_entry",
  "report_description_options", "report_description_option",
  "control_clause", "control_field_list", "_final", "identifier_list",
  "page_limit_clause", "heading_clause", "first_detail", "last_heading",
  "last_detail", "footing_clause", "page_line_column", "line_or_lines",
  "report_group_description_list", "report_group_description_entry",
  "report_group_options", "report_group_option", "type_clause",
  "type_option", "next_group_clause", "column_clause", "sum_clause_list",
  "sum_clause", "ref_id_exp", "present_when_condition", "varying_clause",
  "line_clause", "line_keyword_clause", "report_line_integer_list",
  "line_or_plus", "_numbers", "source_clause", "group_indicate_clause",
  "_indicate", "report_name", "screen_section", "$@28",
  "procedure_division", "$@29", "$@30", "procedure_using_chaining", "$@31",
  "$@32", "procedure_param_list", "procedure_param", "procedure_type",
  "size_optional", "procedure_optional", "procedure_returning",
  "procedure_declaratives", "$@33", "procedure_list", "procedure",
  "section_header", "paragraph_header", "invalid_statement",
  "section_name", "opt_segment", "statement_list", "@34", "@35",
  "statements", "statement", "accept_statement", "$@36", "accept_body",
  "opt_at_line_column", "line_number", "column_number", "opt_accp_attr",
  "accp_attrs", "accp_attr", "end_accept", "add_statement", "$@37",
  "add_body", "add_to", "end_add", "allocate_statement", "$@38",
  "allocate_body", "allocate_returning", "alter_statement",
  "alter_options", "_proceed_to", "call_statement", "$@39", "call_using",
  "$@40", "call_param_list", "call_param", "call_type", "call_returning",
  "call_on_exception", "$@41", "call_not_on_exception", "$@42", "end_call",
  "cancel_statement", "$@43", "cancel_list", "close_statement", "$@44",
  "close_list", "close_option", "reel_or_unit", "compute_statement",
  "$@45", "compute_body", "end_compute", "comp_equal", "commit_statement",
  "continue_statement", "delete_statement", "$@46", "end_delete",
  "delete_file_statement", "$@47", "display_statement", "$@48",
  "display_body", "with_clause", "disp_attrs", "disp_attr", "end_display",
  "divide_statement", "$@49", "divide_body", "end_divide",
  "entry_statement", "$@50", "evaluate_statement", "$@51",
  "evaluate_subject_list", "evaluate_subject", "evaluate_condition_list",
  "evaluate_case_list", "evaluate_case", "$@52", "evaluate_other", "$@53",
  "evaluate_when_list", "evaluate_object_list", "evaluate_object",
  "opt_evaluate_thru_expr", "end_evaluate", "exit_statement", "$@54",
  "exit_body", "free_statement", "$@55", "generate_statement", "$@56",
  "goto_statement", "$@57", "goto_depending", "goback_statement", "$@58",
  "if_statement", "$@59", "$@60", "if_else_sentence", "$@61", "end_if",
  "initialize_statement", "$@62", "initialize_filler", "initialize_value",
  "initialize_replacing", "initialize_replacing_list",
  "initialize_replacing_item", "initialize_category", "initialize_default",
  "initiate_statement", "$@63", "inspect_statement", "$@64",
  "send_identifier", "inspect_list", "inspect_item", "inspect_tallying",
  "$@65", "tallying_list", "tallying_item", "inspect_replacing",
  "replacing_list", "replacing_item", "rep_keyword", "replacing_region",
  "inspect_converting", "inspect_region", "_initial", "merge_statement",
  "$@66", "move_statement", "$@67", "move_body", "multiply_statement",
  "$@68", "multiply_body", "end_multiply", "open_statement", "$@69",
  "open_list", "open_mode", "open_sharing", "open_option",
  "perform_statement", "$@70", "perform_body", "$@71", "end_perform",
  "perform_procedure", "perform_option", "perform_test",
  "perform_varying_list", "perform_varying", "read_statement", "$@72",
  "read_into", "with_lock", "read_key", "read_handler", "end_read",
  "release_statement", "$@73", "return_statement", "$@74", "end_return",
  "rewrite_statement", "$@75", "write_lock", "end_rewrite",
  "rollback_statement", "search_statement", "$@76", "search_body", "$@77",
  "search_varying", "search_at_end", "$@78", "search_whens", "search_when",
  "$@79", "end_search", "set_statement", "$@80", "set_body",
  "set_environment", "set_to", "set_up_down", "up_or_down",
  "set_to_on_off_sequence", "set_to_on_off", "set_to_true_false_sequence",
  "set_to_true_false", "sort_statement", "$@81", "sort_body", "$@82",
  "sort_key_list", "opt_key_list", "sort_duplicates", "sort_collating",
  "sort_input", "sort_output", "start_statement", "$@83", "@84",
  "start_key", "start_op", "end_start", "stop_statement", "$@85", "$@86",
  "stop_returning", "string_statement", "$@87", "string_item_list",
  "string_item", "opt_with_pointer", "end_string", "subtract_statement",
  "$@88", "subtract_body", "end_subtract", "suppress_statement",
  "_printing", "terminate_statement", "$@89", "transform_statement",
  "$@90", "unlock_statement", "$@91", "opt_record", "unstring_statement",
  "$@92", "unstring_delimited", "unstring_delimited_list",
  "unstring_delimited_item", "unstring_into", "unstring_into_item",
  "unstring_into_delimiter", "unstring_into_count", "unstring_tallying",
  "end_unstring", "use_statement", "use_exception", "use_global",
  "use_exception_target", "_after", "_standard", "exception_or_error",
  "exception_or_overflow", "not_exception_or_overflow", "_procedure",
  "use_debugging", "use_debugging_target", "use_reporting",
  "write_statement", "$@93", "write_from", "write_option",
  "before_or_after", "write_handler", "end_write", "on_accp_exception",
  "on_disp_exception", "opt_on_exception", "$@94", "opt_not_on_exception",
  "$@95", "on_size_error", "opt_on_size_error", "$@96",
  "opt_not_on_size_error", "$@97", "on_overflow", "opt_on_overflow",
  "$@98", "opt_not_on_overflow", "$@99", "at_end", "at_end_sentence",
  "$@100", "not_at_end_sentence", "$@101", "at_eop", "at_eop_sentence",
  "$@102", "not_at_eop_sentence", "$@103", "opt_invalid_key",
  "invalid_key", "invalid_key_sentence", "$@104",
  "not_invalid_key_sentence", "$@105", "_opt_scroll_lines", "condition",
  "expr", "partial_expr", "$@106", "expr_tokens", "expr_token", "eq", "gt",
  "lt", "ge", "le", "exp_list", "e_sep", "exp", "linage_counter",
  "arithmetic_x_list", "arithmetic_x", "record_name", "table_name",
  "file_name_list", "file_name", "mnemonic_name_list", "mnemonic_name",
  "procedure_name_list", "procedure_name", "label", "integer_label",
  "reference_list", "reference", "no_reference_list", "opt_reference",
  "reference_or_literal", "undefined_word", "target_x_list", "target_x",
  "x_list", "x", "arith_x", "prog_or_entry", "alnum_or_id", "simple_value",
  "simple_all_value", "id_or_lit", "id_or_lit_or_func", "num_id_or_lit",
  "identifier", "identifier_1", "qualified_word", "subref", "refmod",
  "integer", "literal", "basic_literal", "basic_value", "function",
  "func_refmod", "func_args", "list_func_args", "trim_args",
  "numvalc_args", "locale_dt_args", "not_const_word", "flag_all",
  "flag_duplicates", "flag_initialized", "flag_next", "flag_not",
  "flag_optional", "flag_rounded", "flag_separate", "in_of", "records",
  "with_dups", "coll_sequence", "_advancing", "_also", "_are", "_area",
  "_as", "_at", "_binary", "_by", "_character", "_characters", "_contains",
  "_data", "_file", "_for", "_from", "_in", "_is", "_is_are", "_key",
  "_line_or_lines", "_lines", "_literal", "_mode", "_number", "_of", "_on",
  "_in_order", "_other", "_program", "_record", "_right", "_set", "_sign",
  "_sign_is", "_size", "_status", "_tape", "_than", "_then", "_times",
  "_to", "_when", "_with", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-2010)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1546)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -2010,   206,   536, -2010,   153,   306,   195, -2010, -2010, -2010,
     447,   447,   686,   686, -2010, -2010,   419, -2010, -2010, -2010,
   -2010,   952,   952,   472,   938,   938,   852,   640, -2010,  1027,
    1016, -2010, -2010, -2010, -2010,   -76,   785,   766,   697,   860,
     860, -2010,   704,    61,   718,   746,   837,   751, -2010,   460,
    1070,   898,  1082, -2010,   -80, -2010, -2010,   925, -2010, -2010,
   -2010,   784, -2010, -2010, -2010,   894,   799, -2010,    16, -2010,
     495,   447,   686, -2010, -2010, -2010, -2010,   679, -2010,  1109,
     177,   800,   915,  1057,   867, -2010, -2010,   971,   686, -2010,
   -2010, -2010,   861,   865,   868,   881,   882, -2010, -2010, -2010,
   -2010, -2010,   968,   886,  1124,  1094,   939,   711, -2010,   315,
   -2010, -2010, -2010,    45, -2010, -2010,   891,   991,  1113, -2010,
     245,  1001, -2010,    17,    17,   909,   897,   899,   938, -2010,
     308,  1176,    52,  1402,  1078, -2010, -2010,   907, -2010,  1083,
    1092,   962,  1093,   978, -2010,   995, -2010, -2010, -2010,  1371,
   -2010, -2010, -2010, -2010, -2010, -2010,   945,  1048,  1073, -2010,
     867, -2010, -2010, -2010, -2010, -2010,   232, -2010,   234,  -106,
     292, -2010, -2010, -2010, -2010,  1035,  1197, -2010,   579, -2010,
     580, -2010, -2010, -2010, -2010,   216,   254, -2010,   -81, -2010,
   -2010, -2010,   953,   562,  1308,   974,  1197,  1197,   974,  1036,
    1054,  1197,  1197,  1197,  1197,  1197,   974,  1197,  1366,  1197,
   -2010,   900, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,   974,   968,   177,   982, -2010,
     982,   982, -2010,  1189,   982, -2010,  1341, -2010,  1261,   245,
    1001, -2010,   986,  1087,  1100,  1001,    97,  1019,  1060, -2010,
    1197,  1072,  1167, -2010, -2010,  1348,   860,  1197,  1231, -2010,
     608, -2010, -2010,  1105, -2010,  1197,  1258, -2010,   408, -2010,
   -2010, -2010, -2010,  1020,  1222, -2010, -2010,   974,   974,  1197,
    1197, -2010,  1197,   982,  1406,   974,   974,   982,  1197,   982,
   -2010,   974,   -11, -2010, -2010, -2010, -2010,   393,   982, -2010,
   -2010,   982,  1196,  1067,  1198, -2010,   867, -2010,   867, -2010,
   -2010,  1001, -2010,  1026,  1126, -2010, -2010, -2010,  1019, -2010,
   -2010, -2010,   -38,   -31, -2010, -2010,  1341,  1197,   862,   862,
    1197,    15,  1234,  1197,  1462,  1209, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010,   973,   319,  1197,
   -2010,  1049,  1039, -2010,   898,  1231, -2010, -2010, -2010, -2010,
     982, -2010, -2010, -2010, -2010, -2010,  1197, -2010, -2010,   631,
     982,  1254, -2010, -2010, -2010, -2010, -2010,   982, -2010, -2010,
      36, -2010, -2010,   935, -2010, -2010, -2010, -2010,   982, -2010,
     982,  1220,   982,   867, -2010,  1191,   867, -2010, -2010,  1001,
   -2010,  1047, -2010, -2010,  1420, -2010,  1422, -2010, -2010,  1231,
    1065,  1197,  1462,   982,   -65,   -90,  1231,  1075, -2010,  1197,
    1074, -2010,  1074,   -46, -2010, -2010, -2010, -2010, -2010,  1231,
   -2010, -2010, -2010,   520,    81, -2010,   895, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010,   631, -2010,  1112, -2010, -2010, -2010,
   -2010, -2010, -2010,  1231, -2010, -2010,   935, -2010,  1133, -2010,
     944, -2010,   982,   982,   982, -2010,  1231, -2010, -2010, -2010,
    1201, -2010, -2010,   502,  1088,  1117, -2010, -2010, -2010,   982,
   -2010, -2010, -2010, -2010, -2010, -2010,  1288,    86,  1324,  1089,
   -2010, -2010, -2010,  1197,  1197, -2010, -2010,  2849,   686, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010,   834, -2010,    64, -2010,   631,  1231,
   -2010, -2010, -2010,  1197,   935, -2010,  1234,  1210,  1125, -2010,
    1170,  1234,  1321,  1197,  1486,   117,   -43,    18, -2010,  1104,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,  1162,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
    1197,   982, -2010,  1074, -2010,  1201, -2010, -2010,  2581,  1521,
    1361,   317, -2010,  1231,    56, -2010, -2010, -2010,  1231, -2010,
   -2010,  1179, -2010,   -21,   -21,  3099, -2010,  1097, -2010, -2010,
   -2010, -2010,  1200,  3762,  1103, -2010, -2010,   834, -2010, -2010,
     974, -2010,  1197,  1321, -2010,   136, -2010,  1197, -2010,  1197,
     885, -2010,  1197, -2010,  1197,  1195,  1197,  1197, -2010,  1371,
     138,  1197,  1115, -2010, -2010, -2010,  1335, -2010, -2010,  -134,
    -121,   552,   575,   653,  1123, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010,  1219, -2010, -2010,  1231, -2010, -2010,
   -2010, -2010,   982,   982,  1352, -2010, -2010, -2010,   -57, -2010,
   -2010, -2010,  1197,  1197, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010,   975,   163, -2010,   243, -2010,   121, -2010, -2010, -2010,
   -2010,    39,  1366, -2010,    63, -2010, -2010, -2010, -2010,  1458,
   -2010,  1333, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010,  1169, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
    1125, -2010,  2138, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010,   -44, -2010, -2010,  1270, -2010, -2010, -2010, -2010,   840,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010,   883, -2010, -2010,   322,
    1197, -2010, -2010,   284,   558,   982,  1542, -2010, -2010,   -90,
    1199, -2010,   982,   982, -2010,  1296,  1296,  1303, -2010,   982,
   -2010,   123, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010,  1137, -2010, -2010,  1186, -2010,  1132,
    1206, -2010, -2010, -2010, -2010,  3618,   121,  1564, -2010,  1233,
    1233,   631,  1093,  1093, -2010, -2010,  1139, -2010,   121, -2010,
    1207, -2010, -2010, -2010, -2010, -2010,    35,  1428, -2010, -2010,
    1117,  1231,  1157, -2010,  1158,   982,  4190,  1168,   -16,  1718,
    1600, -2010,  4721,   867,  1213,  4796,  4721,  1388,   672,   818,
      62,   982, -2010, -2010,  1490, -2010,    62,   982,  1155,   982,
    4224,  4721, -2010,  2701,   867,   982,   867,   982,    79,    66,
     982,   867, -2010, -2010,  4271,  4293, -2010, -2010,   982,   982,
     867,   982, -2010,  1085,  1522,   982, -2010, -2010, -2010, -2010,
   -2010, -2010,  1611, -2010, -2010, -2010, -2010, -2010,   982,     5,
      92,   581,  1205, -2010,  1205, -2010, -2010, -2010, -2010,   199,
   -2010, -2010, -2010, -2010, -2010,   982,  1197,  1460,  1460,   317,
   -2010, -2010, -2010, -2010,  1461, -2010,  1231,  1246,  5090,  1203,
   -2010,   982, -2010, -2010,  1419, -2010,  1486, -2010, -2010, -2010,
     982,   982,   631,  1123, -2010,   121,   -90,   -90,  1627,  1366,
   -2010, -2010, -2010,  1531,   643, -2010,  1093,  1204,   982,  1208,
    1211,  1093,   393,  1215,  1216,  1217,  1224,  1225,  1226,  1228,
    1230,  1208,  1532, -2010,  2880, -2010, -2010, -2010, -2010,  1473,
   -2010,  1607, -2010, -2010, -2010,  1262, -2010,   393, -2010, -2010,
    1255, -2010, -2010, -2010,    12,   867,  1560,  1289, -2010,  1344,
    1374,   867,   872,  1562,  2451,   947,   956,  1565,   538,  1255,
   -2010, -2010,    38, -2010, -2010, -2010,  1595, -2010, -2010, -2010,
    1093,    62, -2010, -2010, -2010, -2010, -2010,  1298, -2010,    78,
     982, -2010,    30, -2010, -2010, -2010, -2010, -2010,  4721, -2010,
    1306,  1574,  1652,   887, -2010,  1310, -2010,  2988,  1576,   712,
    1315,  1314,  -157,  1320,   823,  1538, -2010,  1374,  1538,   982,
    1578,  1283, -2010,   873, -2010, -2010, -2010, -2010, -2010,  1474,
   -2010,    62, -2010,   -47, -2010,   143, -2010, -2010, -2010,    41,
    1674,  3878, -2010, -2010,   982,  1580,  4473,   982,  1544,   913,
    1615, -2010,  1396,  1353,  1092,  1538,   883,   199, -2010,  1290,
   -2010,   982,   -61, -2010, -2010, -2010,  1197,  1612, -2010, -2010,
   -2010, -2010, -2010, -2010,    12, -2010, -2010,   982, -2010,  1231,
    1508, -2010, -2010, -2010,  1624,  1093,  5090,  5090,  5090,    28,
    1011, -2010, -2010, -2010,  1139, -2010,  5090, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010,  1375, -2010, -2010,  1288,   -90,  1625,
   -2010, -2010,   873,  1154,  1300,   114,   -20,  5090,  1339,  5090,
   -2010,  5090, -2010,  2108,  1301,  5090,  5090,  5090,  5090,  5090,
    5090,  5090,  5090, -2010, -2010, -2010,  4721,  1557, -2010, -2010,
    1405,  1473,  1667,  3447,  1438,  1512, -2010,   363, -2010, -2010,
   -2010,   735, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010,   648,   867, -2010, -2010,   490,  1586,  1586,  1586,  1586,
   -2010, -2010,  4721,  4721, -2010, -2010,   122,  1616,   804, -2010,
    1313,   672, -2010,   982, -2010,     1, -2010, -2010,   699,  1581,
   -2010,   873,    85, -2010,    30, -2010, -2010, -2010, -2010,    82,
    1350,    62, -2010, -2010,  4721, -2010, -2010, -2010, -2010,  1389,
   -2010, -2010, -2010, -2010,   982,   -16, -2010,  1090, -2010, -2010,
    1374,    12, -2010,  1546,   -52,   566, -2010, -2010,   982,   566,
    1354, -2010,  1139, -2010, -2010,    50,   935, -2010, -2010,  3993,
   -2010,  1709,  1541,  4721,  4721, -2010,  4542,   982, -2010,  1582,
   -2010, -2010,  4721,   873, -2010, -2010, -2010,  1674,  1550,   982,
   -2010,   980,    68,   -52, -2010, -2010, -2010,   982, -2010,  1481,
   -2010, -2010, -2010,   -36,   982, -2010,   982,  1571,  1051,   -31,
   -2010,  1092,   617,  2108,  1311,  1311,  1121, -2010, -2010, -2010,
    5090,  5090,  5090,  5090,  5090,  5090,  4905,  1011, -2010,  1125,
   -2010,  1288,  1092, -2010, -2010, -2010,  1586, -2010, -2010,  1319,
    1322, -2010,   873,  1586,  1549, -2010, -2010, -2010, -2010,   888,
    1586,  1499,  1499,  1499,    54,  1537, -2010, -2010,    95, -2010,
      75,   981,   982,  1086,   102,  1316, -2010,  1139, -2010, -2010,
     303,  1317,  1165,   314,  1318,  1184,   124,   137,   534,  1323,
    1193,  4554,   433,  4721,    62, -2010,  1435, -2010, -2010, -2010,
   -2010, -2010,  1125, -2010, -2010,  1379, -2010, -2010,  1379, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010,  1376,   -16, -2010,    40,   982,   982,   588, -2010,
   -2010, -2010,   389,   696,  1408, -2010, -2010,  1651, -2010,  1517,
   -2010,    29,  1111,  1586,  1515, -2010, -2010,  1520, -2010, -2010,
   -2010,  1599,  4554,   442, -2010, -2010, -2010,  3351, -2010,  1387,
   -2010, -2010, -2010, -2010, -2010,   122, -2010, -2010, -2010,  1092,
   -2010, -2010, -2010,  1125, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010,  1457,  1125, -2010,  1390, -2010,  1741, -2010, -2010, -2010,
    1045, -2010,   873,  1102, -2010,   216,   702,   181,    62,    62,
    4554,   473,  1104,   867,  1653, -2010, -2010,  1784, -2010,  1618,
   -2010, -2010, -2010, -2010,  1546, -2010,   982,   499,   648,   844,
    1357,  1670, -2010,  1363,   873,   778, -2010,    95, -2010, -2010,
   -2010,  4721,  1197,   648, -2010, -2010, -2010, -2010,   -82,   982,
    4554,   485,  1399,  1790,   982,   403, -2010, -2010, -2010,  1496,
    1498, -2010, -2010,  1090,   -36, -2010,   681, -2010, -2010, -2010,
   -2010,  1197,  1631, -2010, -2010,  1231, -2010,   982, -2010, -2010,
    1674, -2010,  1605, -2010,   181,  1321,   181, -2010,  1139, -2010,
   -2010,   981,   768,   768,  1311,  1311,  1311, -2010,  1202,  1404,
   -2010,   982, -2010,  1520, -2010, -2010,  1586, -2010, -2010, -2010,
    1197, -2010, -2010,  1197, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010,   -29, -2010, -2010, -2010,   888, -2010, -2010, -2010,    12,
      12,    12, -2010, -2010, -2010, -2010, -2010,  1208,  1344,  5024,
   -2010,   982,  1208,  1208,  5090, -2010,  1208,  1208,  1208,   333,
    1208,  1208, -2010, -2010,  1543,  4554, -2010,    62, -2010, -2010,
     644,   710, -2010, -2010,  1866, -2010,   531,   -12, -2010, -2010,
   -2010, -2010,   989, -2010,  1480, -2010,  1466, -2010, -2010, -2010,
   -2010, -2010, -2010,   256,   256,   256,   256,  1197, -2010, -2010,
   -2010, -2010,  1107,  1197, -2010, -2010, -2010, -2010,   -22, -2010,
    1111, -2010, -2010, -2010, -2010, -2010, -2010,  4721, -2010,  4721,
     122, -2010, -2010, -2010,  3351, -2010,   982,  1687,  1380,   770,
    1704,  1383,   144,   873, -2010, -2010,  1770, -2010, -2010, -2010,
   -2010,  1102, -2010,  1645, -2010,  1197,  1536, -2010, -2010,  1321,
      62, -2010,  4721,   138,   386, -2010, -2010, -2010,   982,  4721,
     596, -2010, -2010, -2010,  1681,  1568, -2010,  1683, -2010,  1587,
   -2010, -2010, -2010, -2010,  1363, -2010, -2010, -2010,  1566,  1688,
    1545,  1533,  1344, -2010,  4721,   144, -2010,  1552, -2010,   873,
   -2010,  1724,  1444, -2010, -2010,  1092, -2010,   801,  1832, -2010,
     998, -2010, -2010, -2010,  1231,  1725,  1620,  1775,  5006, -2010,
    1401, -2010, -2010, -2010,  1197, -2010, -2010, -2010,  1197, -2010,
   -2010, -2010, -2010,   359,   359,    87,    87, -2010, -2010, -2010,
   -2010, -2010,  1408, -2010,  1256, -2010, -2010, -2010,   981, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010,  1125,  1689,  1125,  1690, -2010, -2010,  4721, -2010, -2010,
   -2010, -2010, -2010,  1719, -2010, -2010, -2010, -2010, -2010, -2010,
    1586,  1586,  1586,  1586,   359, -2010, -2010,   359,    87,    87,
   -2010, -2010, -2010,  4554,  1519,  4554,  1523, -2010, -2010, -2010,
   -2010, -2010,  1713, -2010,   770, -2010,  1751, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010,   144,  1090, -2010, -2010,  1090,   142,
     982, -2010,  1197,  4554, -2010, -2010,   893,  3976, -2010,  1805,
    1617,  1637,   532, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010,   982,  1212, -2010, -2010,
   -2010,  1715,  1597,   982,  1408,  4554, -2010,  1790, -2010,  1324,
    1767,  1324,  1545,   436, -2010, -2010,  1717, -2010,  1602, -2010,
   -2010, -2010,   -88, -2010, -2010,  1197,  1776,  1655, -2010,  1023,
   -2010,  1671,  1059,  1462,  1685,  1441,  1197,  1093,  1197,   982,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010,  1483, -2010, -2010, -2010, -2010,   634, -2010, -2010,
   -2010,  1401, -2010,   982,   121, -2010, -2010, -2010, -2010, -2010,
   -2010,   359, -2010, -2010, -2010, -2010, -2010, -2010,  1125, -2010,
    1125, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010,  4721, -2010,  4721, -2010,
   -2010, -2010, -2010, -2010,  1831,  1090,  1090, -2010,  1476,  1577,
     867,   396, -2010, -2010, -2010, -2010,  1539,  4721, -2010,  1197,
     738,  1646, -2010,  1647, -2010, -2010, -2010, -2010, -2010, -2010,
     982, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,   982,  1324, -2010,   982,  1738,
   -2010, -2010, -2010, -2010, -2010,   867, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010,  1099,  1231,  1197,  1197,  1714, -2010,  1197,
   -2010, -2010, -2010, -2010,   317, -2010,  1197, -2010,   982,   982,
     604,  1710, -2010,  1598,  1231,   634, -2010, -2010, -2010,   982,
   -2010,  1099, -2010, -2010, -2010, -2010,  1408,  1408, -2010,  4721,
    1090, -2010,  4721,  1197,   867,   867,  1585, -2010,   982, -2010,
    1472,   982,  1756, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010,   982, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010,  1231,  1231,  1197, -2010,  1231, -2010,  1231, -2010,
    1344, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010,  4721, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
     -16,   867,  1197, -2010, -2010,   982, -2010, -2010, -2010, -2010,
   -2010, -2010,  1231, -2010, -2010, -2010,  1848, -2010,   -16, -2010,
   -2010,  4721, -2010, -2010
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     4,     6,     7,
      38,    38,     0,     0,     3,     5,     0,     8,    14,    28,
      27,    29,    29,     0,   276,   276,     0,     0,    24,    40,
       0,     9,    15,    30,    26,  1489,     0,   157,     0,   573,
     573,  1490,     0,     0,     0,     0,     0,     0,    39,   278,
       0,    17,     0,    25,    33,    37,    36,  1516,    35,    42,
     159,     0,   161,   288,   289,     0,   340,   281,   577,    18,
      20,    38,     0,    16,    34,  1517,    32,    41,   165,   163,
     249,     0,     0,   472,     0,   580,   578,   595,     0,    19,
      10,    11,     0,     0,     0,     0,     0,    43,    44,    45,
      47,    46,   160,     0,   247,     0,  1481,   258,   162,   251,
     253,   255,   256,   252,   269,   279,     0,     0,   475,  1324,
     282,   343,   290,   585,   585,     0,     0,     0,   276,    23,
      56,    71,    49,    80,  1443,   166,   165,     0,   158,     0,
    1509,     0,  1507,     0,  1482,  1533,   259,   260,   261,  1463,
     250,   254,   268,   270,   283,   341,     0,     0,   478,   287,
       0,   286,   344,  1431,   292,  1472,   585,   582,   588,     0,
     585,   596,   574,    21,    12,     0,  1489,    54,  1516,    55,
    1516,    60,    62,    63,    64,     0,     0,    70,     0,    73,
    1546,    48,     0,  1545,     0,     0,  1489,  1489,     0,     0,
    1524,  1489,  1489,  1489,  1489,  1489,     0,  1489,  1475,  1489,
      79,    81,    83,    85,    86,    87,    89,    88,    90,    91,
      92,    93,    94,    95,  1444,     0,   164,   249,     0,  1510,
       0,     0,  1508,     0,     0,  1534,  1477,  1464,  1483,   280,
     343,   473,     0,     0,   570,   343,   346,     0,     0,   583,
    1489,     0,   593,   586,   587,   597,   573,  1489,     0,    57,
    1516,    59,    61,     0,  1456,  1489,     0,    77,     0,    72,
      74,    52,    50,     0,     0,  1344,   112,     0,     0,  1489,
    1489,  1525,  1489,     0,     0,     0,     0,     0,  1489,     0,
    1476,     0,    99,    82,    84,   167,   248,  1388,   275,  1335,
    1337,   271,     0,     0,     0,  1478,     0,  1484,     0,   284,
     342,   343,   476,     0,     0,   277,   285,   349,     0,   355,
     356,   347,   359,   359,   350,   305,  1477,  1489,     0,     0,
    1489,  1477,  1503,  1489,  1461,     0,   291,   293,   296,   297,
     298,   299,   300,   301,   302,   303,   304,     0,     0,  1489,
     594,     0,     0,   575,    17,     0,  1393,    69,    58,  1455,
       0,    76,    75,    78,    51,    53,  1489,   101,   102,     0,
       0,     0,   153,   152,   103,   104,   156,     0,   155,   139,
    1491,   141,    96,     0,    97,   169,  1449,  1450,     0,  1336,
       0,     0,     0,   262,   263,   266,   257,  1322,   474,   343,
     479,     0,   348,   360,   361,   351,     0,   361,   353,     0,
       0,  1489,  1461,     0,     0,     0,     0,     0,  1504,  1489,
       0,  1462,     0,     0,   294,   295,   589,   590,   592,     0,
     584,   598,   600,     0,     0,    68,     0,  1402,  1398,  1403,
    1401,  1399,  1404,  1400,   145,   146,   148,   154,   151,   150,
    1493,  1492,   142,     0,   111,   110,   100,   107,  1531,   105,
       0,  1389,   273,     0,   274,   264,     0,   265,  1323,   477,
     481,   571,   369,   363,     0,   317,   337,  1451,  1452,   326,
    1338,   321,   320,   319,  1343,  1342,  1499,  1475,  1487,     0,
     569,   338,   339,  1489,  1489,   591,   600,     0,     0,    13,
      66,    67,    65,   117,   131,   127,   132,   114,   130,   128,
     115,   116,   129,   113,   118,   119,   121,   147,     0,   140,
     143,   108,  1532,  1489,    98,   184,  1503,     0,  1541,   230,
       0,  1503,  1494,  1489,  1473,  1494,   233,     0,   232,  1545,
     217,   216,   168,   170,   171,   172,   173,   174,   175,     0,
     176,   177,   229,   178,   179,   180,   181,   182,   183,   185,
    1489,   272,   267,     0,   480,   482,   483,   572,     0,  1465,
       0,  1491,   354,     0,   307,  1339,  1500,   328,     0,   310,
    1488,  1529,   336,     0,     0,     0,   606,   610,   601,   602,
     603,   604,   609,     0,     0,   120,   123,     0,   149,   144,
       0,   106,  1489,  1494,  1542,   192,   234,  1489,  1495,  1489,
       0,  1474,  1489,  1470,  1489,     0,  1489,  1489,   241,  1463,
       0,  1489,     0,   486,   484,   385,     0,   459,   394,   427,
     415,   424,   421,   418,  1543,   395,   396,   397,   398,   399,
     400,   401,   402,   403,  1520,   358,   428,     0,   404,   391,
     405,   406,     0,     0,  1527,   408,   409,   407,   455,   411,
     412,   410,  1489,  1489,   352,   370,   371,   372,   373,   374,
     375,   392,   376,   377,   378,   379,   380,   381,   382,   383,
     384,     0,     0,  1466,     0,   364,     0,   318,   309,   308,
     306,   327,  1475,  1530,   315,   324,   323,   325,   322,     0,
     608,   611,   668,   719,   728,   735,   739,   763,   768,   786,
     779,   787,   788,   794,   827,   836,   838,   865,   873,   875,
    1541,   881,     0,   892,   913,   915,   951,   953,   957,   667,
     963,   976,   996,  1013,  1015,  1019,  1026,  1027,  1043,  1063,
    1081,     0,  1100,  1111,  1119,  1121,  1123,  1125,  1130,  1152,
    1175,   605,   617,   618,   619,   620,   621,   622,   623,   624,
     626,   625,   627,   628,   629,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,  1148,  1149,  1150,   666,    22,     0,   122,   109,     0,
    1489,   194,   193,   189,     0,     0,  1469,   233,   228,     0,
       0,   231,     0,     0,   240,  1514,  1514,     0,   242,     0,
     213,  1489,   471,   425,   426,   413,   414,   422,   423,   419,
     420,   416,   417,  1544,     0,  1521,   453,   435,   386,  1384,
     469,  1528,   456,   457,   454,     0,     0,   388,   390,  1447,
    1447,     0,  1507,  1507,   368,   365,  1394,  1396,   461,   463,
     465,  1468,   329,   330,   331,   332,     0,     0,   311,  1486,
     317,     0,     0,   612,     0,     0,     0,  1239,   734,     0,
     765,   770,     0,     0,     0,     0,     0,     0,  1239,   867,
       0,     0,   877,   882,     0,  1239,     0,     0,     0,     0,
       0,     0,   965,   986,     0,     0,     0,     0,     0,     0,
       0,     0,  1095,  1093,     0,     0,  1120,  1118,     0,     0,
       0,     0,  1153,  1159,     0,     0,   137,   133,   138,   136,
     134,   135,   124,   125,   202,   203,   201,   200,     0,   187,
     188,  1501,   222,   221,   222,   218,   246,   235,   236,  1434,
     239,  1515,   243,   244,   245,  1340,  1489,   495,   495,  1491,
     515,   487,   490,   491,     0,   458,     0,  1539,     0,  1385,
    1386,     0,   393,   460,     0,   387,  1473,   429,   430,  1395,
       0,     0,     0,  1543,   464,     0,     0,     0,  1467,  1475,
     316,   599,   607,   717,   687,  1383,  1507,     0,     0,  1417,
    1420,  1507,  1315,     0,     0,     0,     0,     0,     0,     0,
       0,  1417,   726,  1359,   724,  1349,  1351,  1357,  1358,  1436,
     729,     0,  1238,  1260,  1334,     0,  1330,  1332,  1331,  1378,
     741,  1377,  1379,   767,   764,   769,   782,     0,  1317,  1445,
    1518,     0,  1398,   825,   687,     0,  1351,   834,     0,   741,
     844,   843,  1459,   840,   842,   872,   869,   868,   871,   866,
    1507,   874,  1345,  1347,   876,  1328,   886,  1537,  1237,   894,
     914,   497,     0,   917,   918,   919,   952,  1067,     0,   954,
       0,   961,     0,   964,   987,  1334,   977,   986,   979,     0,
     984,     0,  1331,     0,  1438,  1177,  1320,  1518,  1177,     0,
    1041,  1032,  1321,     0,  1327,  1044,  1045,  1046,  1047,  1048,
    1056,  1049,  1059,     0,  1325,     0,  1064,  1082,  1096,  1097,
    1471,     0,  1102,  1104,     0,  1116,     0,  1122,     0,  1127,
    1132,  1160,     0,  1161,  1509,  1177,     0,  1434,   196,   195,
     186,     0,     0,   220,   219,  1454,  1489,     0,   212,   206,
     237,  1435,  1341,   214,     0,   496,   492,     0,   493,     0,
     485,   488,   436,  1540,   437,  1507,     0,     0,     0,  1303,
    1301,  1366,  1306,  1360,  1364,  1365,     0,  1387,   470,   389,
    1448,   367,   366,  1397,  1522,   466,   335,  1499,     0,   313,
     718,   669,  1468,     0,   696,     0,     0,     0,     0,     0,
    1405,  1422,  1416,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1406,   727,   720,     0,     0,  1350,  1437,
     732,  1436,  1240,     0,   737,     0,   742,   752,  1376,   766,
    1375,   772,   783,   780,   785,   784,  1239,  1318,  1446,  1319,
    1519,  1226,   793,   826,   795,   805,  1192,  1192,  1192,  1192,
     835,   828,     0,     0,   837,  1460,  1239,   863,   850,   846,
     848,  1239,   870,     0,  1346,   879,  1538,   884,   896,     0,
     498,     0,   940,   925,   916,   920,   922,   923,   924,  1071,
       0,     0,   962,   958,     0,   970,   967,   969,   968,   971,
     978,   981,   613,  1239,     0,     0,   988,     0,  1439,  1440,
    1518,     0,  1014,   998,  1021,  1034,  1042,  1028,     0,  1034,
       0,  1371,  1372,  1057,  1060,     0,     0,  1326,  1055,     0,
    1054,     0,  1084,     0,     0,  1094,     0,     0,  1103,     0,
    1117,  1112,     0,     0,  1128,  1129,  1126,  1471,     0,     0,
    1162,     0,     0,  1021,   126,   206,   204,   197,   198,     0,
     225,   211,  1453,  1434,     0,   489,   494,   500,   510,   359,
     516,  1509,   431,     0,  1311,  1312,     0,  1304,  1305,  1390,
       0,     0,     0,     0,     0,     0,     0,     0,  1523,  1541,
     334,  1499,  1509,   312,   692,   683,  1192,   673,   680,   674,
     676,   678,     0,  1192,     0,   672,   679,   686,   685,     0,
    1192,  1505,  1505,  1505,   690,   691,  1368,  1367,     0,  1356,
    1303,  1301,     0,     0,  1303,     0,  1352,  1353,  1354,  1316,
    1303,     0,     0,  1303,     0,     0,  1303,  1303,  1303,     0,
       0,  1199,  1445,     0,     0,   730,     0,  1251,  1252,  1253,
    1286,  1254,  1541,  1290,  1295,  1535,  1261,  1298,  1535,  1279,
    1258,  1268,  1250,  1249,  1287,  1257,  1259,  1269,  1270,  1271,
    1272,  1273,  1288,  1242,  1291,  1293,  1274,  1275,  1276,  1277,
    1278,  1245,  1246,  1247,  1248,  1262,  1285,  1256,  1267,  1244,
    1243,  1255,  1264,  1265,  1266,  1263,  1280,  1281,  1282,  1283,
    1284,  1241,     0,     0,  1333,   748,     0,     0,   755,   777,
     778,   771,   773,     0,  1199,  1231,  1233,   790,  1227,  1228,
    1229,     0,  1546,  1192,     0,  1193,   798,  1195,   799,   796,
     797,     0,  1199,  1445,   858,   860,   859,   853,   855,   861,
     864,   839,   851,   847,   845,  1239,   613,   841,  1348,  1509,
     878,  1329,   613,  1541,   904,   905,   907,   909,   910,   906,
     908,   899,  1541,   895,     0,   941,     0,   943,   942,   944,
     935,   936,     0,     0,   921,  1073,  1511,     0,     0,   955,
    1199,  1445,  1545,     0,   982,   614,   989,   990,   993,     0,
     985,  1184,  1183,   992,   998,  1178,     0,     0,  1226,     0,
       0,     0,  1033,     0,     0,     0,  1058,     0,  1062,  1061,
    1052,     0,  1489,  1226,  1099,  1098,  1105,  1106,  1107,     0,
    1199,  1445,     0,  1432,     0,  1107,  1174,  1164,  1163,  1169,
       0,  1171,  1172,  1179,  1434,   199,     0,   208,   209,   238,
     207,  1489,   502,   513,   514,   512,   518,     0,   445,   446,
    1471,   434,   447,   443,   441,  1494,   439,  1361,  1362,  1363,
    1314,  1302,  1307,  1308,  1309,  1310,  1313,  1391,     0,   467,
     333,     0,   684,  1195,   675,   677,  1192,   681,   671,   711,
    1489,   700,   701,  1489,   712,   702,   703,   706,   716,   713,
     704,     0,   714,   705,   715,   697,   698,   670,  1506,     0,
       0,     0,   688,   689,  1370,  1355,  1369,  1417,  1445,     0,
    1421,     0,  1417,  1417,     0,  1414,  1417,  1417,  1417,     0,
    1417,  1417,  1200,   721,  1202,  1199,   733,     0,  1289,  1536,
    1292,  1294,   738,   736,   743,   744,   588,     0,   754,   753,
    1165,  1166,   758,   756,     0,   776,     0,   781,   613,   613,
     791,   789,  1230,   805,   805,   805,   805,  1489,   810,   823,
     824,   811,     0,  1489,   814,   815,   818,   816,     0,   817,
     807,   808,   800,   806,   613,  1196,  1191,     0,   829,     0,
    1239,  1239,   857,   613,   854,   849,     0,   887,     0,     0,
     911,     0,     0,     0,   937,   939,     0,   931,   947,   932,
     933,   926,   927,   947,  1065,  1489,     0,  1512,  1072,  1494,
     956,   959,     0,     0,   973,   983,   980,   616,     0,     0,
    1000,   999,  1215,  1217,  1017,  1212,  1213,  1024,  1022,     0,
    1239,  1035,  1239,  1029,  1037,  1050,  1051,  1053,  1441,  1091,
    1206,     0,  1445,  1113,     0,     0,  1433,  1133,  1134,     0,
    1137,  1140,  1144,  1138,  1170,  1509,  1173,  1185,  1457,   205,
       0,   226,   227,   223,     0,     0,   504,     0,  1526,   438,
       0,   432,   448,   444,  1489,   433,   440,  1392,  1489,   462,
     314,  1190,   682,     0,     0,  1235,  1235,   699,   694,   693,
     695,  1410,  1199,  1418,     0,  1430,  1415,  1408,  1428,  1409,
    1411,  1412,  1425,  1426,  1413,  1407,   613,  1203,  1198,   722,
     731,  1541,     0,  1541,     0,   745,   746,     0,   750,   749,
     751,  1167,  1168,   761,   759,   613,   774,   775,  1232,  1234,
    1192,  1192,  1192,  1192,     0,   812,   813,     0,  1235,  1235,
     809,  1194,   613,  1199,  1317,  1199,  1317,   856,   862,   852,
     880,   888,   890,   897,   900,   901,  1479,   912,   893,   898,
     947,  1373,  1374,   947,     0,   930,   928,   929,   934,  1075,
       0,  1513,  1489,  1199,   972,   966,     0,   615,   994,     0,
       0,  1006,     0,   613,   613,  1018,  1016,  1214,  1025,  1020,
    1023,  1030,   613,  1039,  1038,  1442,     0,     0,  1092,  1083,
    1207,  1109,  1209,     0,  1199,  1199,  1124,  1432,  1136,  1487,
    1142,  1487,  1206,     0,  1222,  1224,  1188,  1186,  1219,  1220,
    1187,  1458,     0,   224,   501,  1489,     0,   506,   511,  1505,
     547,   567,   562,  1461,     0,     0,  1489,  1507,  1489,     0,
     517,   523,   524,   525,   534,   526,   528,   531,   519,   520,
     521,   527,   530,   548,   532,   535,   522,     0,   529,   533,
     452,   449,   450,     0,     0,  1382,   708,  1380,  1381,   707,
     710,     0,   709,   723,  1419,  1201,   613,  1297,  1541,  1300,
    1541,   747,   762,   740,   613,   757,   804,   803,   802,   801,
     820,   819,   822,   821,  1197,   831,     0,   830,     0,   613,
     891,   885,   902,  1480,     0,   946,   938,   947,   949,     0,
       0,  1078,  1074,  1069,   960,   975,     0,     0,  1001,  1489,
    1008,     0,  1002,     0,  1005,  1216,  1218,   613,  1036,   613,
    1085,  1086,  1087,  1088,  1089,  1090,   613,  1110,  1101,  1210,
    1205,  1108,  1115,  1114,  1135,     0,  1487,  1139,     0,  1146,
    1158,  1155,  1157,  1156,  1151,  1154,   613,   613,  1189,  1176,
    1221,  1182,  1181,  1496,     0,  1489,  1489,   508,   546,  1489,
     568,   566,   563,   564,  1491,   556,  1489,  1239,     0,     0,
       0,     0,   549,     0,     0,   554,   557,   560,   451,   442,
     468,  1496,  1204,  1296,  1299,   760,  1199,  1199,   889,     0,
     945,   950,     0,  1489,  1076,     0,     0,  1066,  1068,   974,
       0,     0,  1011,  1009,  1010,  1004,  1003,  1031,  1040,  1208,
     613,  1141,     0,  1145,  1147,  1131,  1223,  1225,  1497,  1498,
    1180,   503,     0,     0,  1489,   499,     0,   555,     0,   552,
    1445,   550,   551,   541,   539,   540,   542,   538,   543,   537,
     536,     0,   561,   559,   558,  1236,   833,   832,   903,   948,
       0,  1079,  1489,  1070,  1239,  1007,  1012,   997,  1211,  1143,
     505,   507,     0,   545,   544,   565,     0,  1077,     0,   995,
     509,     0,  1080,   553
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -2010, -2010, -2010, -2010,  1892, -2010, -2010, -2010,    44, -2010,
   -2010, -2010, -2010, -2010,  1551, -2010, -2010, -2010,  1194, -2010,
   -2010,    47,  1878, -2010, -2010,  1850,   251, -2010, -2010, -2010,
   -2010, -2010,  1723,  1774, -2010, -2010,  1739,   663, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,  1730, -2010, -2010, -2010, -2010,
    1708, -2010, -2010, -2010, -2010, -2010,  -224,   594, -2010, -2010,
   -2010, -2010,  1407, -2010, -2010,  1325,   779, -2010, -2010, -2010,
   -2010, -2010, -2010,  1482, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,  1791, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,   573, -2010,
     567,   783, -2010, -2010, -2010, -2010, -2010,   979,    72, -2010,
    1326, -2010, -2010, -2010, -2010, -2010, -2010,   125, -2010, -2010,
    1707, -2010,  1826, -2010, -2010, -2010, -2010,  1547, -2010, -2010,
    1828,   706, -2010, -2010, -2010, -2010,  1703, -2010,  1894,  1785,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,  1069, -2010,
   -2010, -2010,  1369, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,   656, -2010, -2010, -2010,  1629,
   -2010, -2010,   780, -2010, -2010,  -309, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010,    93,    94,  1110,    98,
   -2010,    99, -2010, -2010, -2010,   299,   316, -2010,   383, -2010,
     320, -2010,   -93,   103, -2010, -2010,   106, -2010,   107, -2010,
   -2010, -2010,  1108, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010,  1412, -2010, -2010, -2010,  1010, -2010,
    -921, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,   -73, -2010,
   -2010, -2010, -2010, -2010, -2010,  -205, -2010, -2010, -2010, -2010,
     166, -2010, -2010,   110, -2010, -2010, -2010, -2010, -2010,  1857,
    1037, -2010,   246, -2010, -2010, -2010, -2010,  1488, -2010, -2010,
   -2010, -2010, -2010, -2010,  -717, -2010, -2010,   168, -2010, -2010,
   -2010, -2010,   932,   574,   584, -2010, -2010,   293, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010,   934, -2010, -2010,   261, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010,  -307, -2010,   229,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
     729, -2010, -2010,   734, -2010, -2010, -2010, -2010,   459,   227,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010,    58,   730, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,   731, -2010, -2010, -2010,   212,
   -2010, -2010,   444, -2010, -2010, -2010, -1563, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2009,   921,
   -2010, -2010,   201, -2010, -2010,   426, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010,   668, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,   703, -2010,   189, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,   905,
   -2010,   904, -2010, -2010,  1106, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,   896,   405, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010,    24, -2010,   407, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010, -2010,  -870, -2010, -1150,
   -2010, -2010, -1259, -1196, -1227, -2010,   360, -2010,  -932, -2010,
   -2010, -2010, -2010,    22, -2010, -2010, -2010, -2010,   -83, -2010,
   -2010,   211, -2010, -2010, -2010, -2010,    20, -2010,  -726, -1730,
   -2010, -2010,   521, -2010,  -974, -1284,  -852, -1212, -2010, -2010,
   -2010, -1210, -1209, -1200, -1186, -1184,   115, -1165,  -476,  -336,
   -1155,  -882,   101,   930, -1036,   -84, -2010, -1107, -2010,  -810,
   -2010,   807,  -228,    -7, -2010, -2010,  -740,   814,  -866, -1014,
    -263,  -793, -2010, -2010,   437,  -936, -1628,  -994,  1159, -1046,
     692,  -615,  -192, -2010,  1064,  -227,  -675,  -704,  -328,  -606,
    -858, -2010, -2010, -2010, -2010, -2010,  1800, -2010, -1096,   816,
   -2010, -2010, -2010, -1656,  1192,    84,  1720,   762,  -452, -2010,
     992,  -408,  1434, -2010,  -666, -2010, -1087,  1071,  -439,   803,
   -2010, -2010,  -729, -2010, -1326,  -175,  -566,  -523,  -136, -1024,
   -2010,   496, -1368,  -845, -1086, -2010,  1236,  1999,  -942, -2010,
   -2010, -2010, -2010, -2010, -2010, -2010,   605, -2010, -2010,  -714,
    1076,  -129
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,     2,     6,     7,     8,    24,    39,    69,   128,
     256,     9,    25,    40,    70,    90,   499,    73,    71,    35,
      11,    21,    27,    42,    57,    58,    17,    37,    77,    97,
      98,   191,   192,   178,    99,   179,   180,   181,   182,   502,
     183,   184,   100,   187,   188,   189,   268,   101,   210,   211,
     212,   213,   459,   383,   524,   456,   457,   458,   214,   366,
     513,   514,   515,   806,   942,   516,   943,   215,   379,   380,
     519,   216,   444,   445,   217,   218,   219,   220,   221,   222,
     223,    48,    78,    80,   104,   102,   135,   385,   460,   543,
     544,   951,   813,  1150,  1357,   545,   947,   546,  1363,  1364,
    1637,  1159,   547,   548,   549,   550,   955,  1153,  1863,   551,
     552,   553,   554,   555,   556,   557,   558,   828,   559,   138,
     108,   109,   110,   111,   149,   112,   393,   394,   467,   113,
     114,    31,    66,   154,    84,   239,   159,   120,   160,   121,
     164,   248,   337,   338,   690,   339,  1393,   880,   574,   340,
     483,   341,   696,   342,   343,   691,   872,   873,   874,   875,
     344,   345,   346,    83,   240,   161,   162,   163,   246,   321,
     472,   474,   322,   323,   664,   405,   406,   569,   864,   324,
     568,   665,   666,   667,   985,   668,   669,   670,   671,   672,
    1651,   673,   977,  1372,  1875,  1652,  1653,  1654,  1655,  1871,
    1656,  2061,  2062,   674,   675,   854,   676,   677,   678,   572,
     993,   868,   869,  1879,   679,   680,   118,   311,   158,   399,
     244,   470,   564,   565,   566,   831,   971,   972,  1166,  1167,
    1080,   973,  1642,  1866,  2027,  2167,  2235,  1367,  1645,  1170,
    1370,  1868,  2048,  2049,  2250,  2050,  2051,  2052,  2053,  2241,
    2054,  2055,  2056,  2057,  2185,  2186,  2174,  2058,  2059,  2171,
     491,   315,   567,    51,   255,   432,    87,   124,   123,   166,
     167,   168,   252,   351,   126,   353,   496,   497,   588,   589,
     590,   591,   592,   884,  1584,  1585,  1817,   593,   752,   753,
     885,  1003,  1204,  1414,  1415,  1410,  1695,  1696,  1201,   754,
     886,  1022,  1227,  1225,   755,   887,  1030,  1445,   756,   888,
    1503,   757,   889,  1237,  1505,  1734,  1735,  1736,  1508,  1742,
    1925,  1923,  2084,  2083,   758,   890,  1044,   759,   891,  1045,
    1511,  1512,   760,   892,  1046,  1243,  1246,   761,   762,   763,
     893,  1751,   764,   894,   765,   895,  1053,  1523,  1770,  1771,
    1254,   766,   896,  1057,  1261,   767,   897,   768,   898,  1062,
    1063,  1267,  1268,  1269,  1546,  1544,  1783,  1270,  1537,  1538,
    1782,  1541,   769,   899,  1069,   770,   900,   771,   901,   772,
    1075,  1550,   773,   903,   774,   905,  1552,  1952,  2099,  2101,
     775,   906,  1278,  1561,  1790,  1954,  1955,  1956,  1958,   776,
     907,   777,   908,  1082,  1284,  1285,  1286,  1573,  1801,  1802,
    1287,  1570,  1571,  1572,  1795,  1288,  1965,  2202,   778,   909,
     779,   910,  1089,   780,   911,  1091,  1293,   781,   912,  1093,
    1299,  1583,  1975,   782,   913,  1096,  1302,  1816,  1097,  1098,
    1099,  1587,  1588,   783,   914,  1597,  1981,  2120,  2212,  2267,
     784,   915,   785,   916,  1986,   786,   917,  1598,  1989,   787,
     788,   918,  1110,  2127,  1319,  1600,  1992,  1833,  1834,  2129,
    1317,   789,   919,  1115,  1116,  1117,  1118,  1331,  1119,  1120,
    1121,  1122,   790,   920,  1086,  1969,  1289,  2208,  1575,  1804,
    2111,  2207,   791,   921,  1332,  1613,  1996,  1999,   792,  1129,
    1128,  1335,   793,   924,  1131,  1132,  1840,  2138,   794,   925,
    1135,  1341,   795,   927,   796,   928,   797,   929,   798,   930,
    1346,   799,   931,  1348,  1847,  1848,  1625,  1850,  2010,  2147,
    2012,  2225,   800,   801,   933,  2154,  1143,  1351,  1629,  1743,
    1924,  1855,   802,  1631,   803,   804,   935,  1312,  1857,  2108,
    2016,  2159,  1672,  1526,  1527,  1774,  1776,  1942,  1723,  1724,
    1906,  1908,  2076,  2001,  2002,  2136,  2140,  2220,  1824,  1825,
    1983,  1826,  1984,  2017,  2018,  2156,  2019,  2157,  1517,  1518,
    1519,  1748,  1520,  1749,  2070,  1077,  1078,  1032,  1033,  1232,
    1233,  1476,  1477,  1478,  1479,  1480,  1179,  1380,  1421,  1023,
    1047,  1247,  1105,  1111,   396,   397,  1123,  1124,  1275,  1100,
    1036,  1037,   298,   299,   479,  1163,   486,   276,  1071,  1072,
    1024,  1049,  1182,  1418,  1705,  1803,  1960,  1055,  1101,  2066,
    1026,  1005,   849,   979,   980,  2068,  1027,   866,   867,  1028,
    1210,  1212,  1425,  1439,  1434,  1431,   247,  1849,  1160,  1230,
    1310,  1997,   225,  1249,   987,   388,   413,  1161,   265,  2022,
    1780,   422,   238,   684,  1205,   615,   169,   612,   291,   306,
    2104,   145,   308,   881,   581,    43,   453,   609,  2230,   577,
    1151,   419,  1699,   233,   230,  1808,   962,   185,  1251,   846,
    1389,   282,   682,   694,   523,   236,  1730,  1277,  1174,   605,
     844,  1524
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     122,   258,   301,   194,   478,   686,   902,  1137,   560,   865,
    1048,   870,   616,   617,   408,  1252,  1327,   990,   991,  1586,
     934,   277,   278,  1496,  1497,   876,   283,   284,   285,   286,
     287,   357,   289,  1498,   292,  1031,   300,   848,   300,   300,
    1079,   446,   300,  1336,  1700,  1701,  1064,  1499,   579,  1500,
    1239,  1356,  1892,  1125,  1539,  1265,  1070,  1274,  1352,   450,
      22,  1528,  1529,  1530,   194,  1274,   165,   105,  1070,   871,
      85,  1441,  1070,   347,  1918,   348,   122,  1630,  1035,   957,
     810,   596,   355,   996,  1070,   382,  1155, -1467,  1109,   165,
     360,   300,  1885,  1025,  1565,   300,  1408,   300,  1377,  1938,
     305,   186,  1025,  1058,   369,   370,   300,   371,  1532,   300,
   -1509,  1274,   688,   377,    89,  1549,   446,  1090,  1092,    92,
    1411,   428,  1281,  -190,   493,  1753, -1471,  2020,   434,  1416,
     861,  1133,  1025,   403,    54,   127,   165,   500,    41,  1580,
     403,  1566,   290,  1534,  1677,  1377,   484,   825,   356,  1070,
      52,  1697,   410,   861,   613,   415,   417,  1593,   420, -1545,
    1114,  1206,   462,  1223,   464,  1313,  1213,   852,   300,  1673,
    1365, -1494,  1377,  1390,   429,   608,  1673,  1320,   300,  1359,
    1411,   983,   475,  1673,   966,   300,  2004,  1620, -1545,   487,
     598,   436,   922,   870,  1377,    14,   461, -1509,   300,   105,
     300,  1114,   495,  1577,  1155,  2161,     3,  1377, -1545,  1648,
    -191,   967,   968, -1545,   481,   695,  1113,  2006, -1545,   253,
    1034,   480,   395,   300,   302,  1273,   520,   304,  1333,    41,
     614,  1228,   521,  1084,   833,   561,   477,  1034,  1314,   562,
    1968,  -190,    55,   879,   489, -1384,  1366,   835,  1238, -1545,
     451,  2277,   861,   878,   356,  1325,  1196,  1197,  1567,   811,
    1623,  1228,    18,   266,  1328,  1711,  1394,  1639,  1714,  2282,
     300,   300,   300,  1719,  1184,  1353,   372,  1114,   618,   263,
     376,   165,   378,  1042,   834,  1647,  1360,   575,  1725,   853,
    1417,   389,   599,   862,   389,  1290,  1649,   836,   570, -1485,
     601,   978,  1085,   812,  1034,   923,  1671,  1042,  1568,   395,
     619,  1274,   468,  1919,  1114,   482,   437,  1595,   583,   584,
    1195,  1155,    91, -1471, -1431, -1431,    56,  1772,  -191,   254,
    1373,  1704,  1198,  1539,  1754,   608,  2107,    41,  1133,   437,
     450,   165, -1471,  1228,  1048,  1564,   687,  1412,   600,  2109,
     426,   692,   297,   435,   297,  1413,  1326,   438,   610,   190,
    2065,  1282,   229,   447,   439, -1471,   354,  1670,  1594,   300,
     449, -1516,  1185,  1377,   269,  1191,  1192,   689,     4,   -31,
     438,  1048,  1334,    55,  1377,   622,   859,   439,  1378,   190,
    2214,   697,   697,   681,  1514,  1886,   494,  2105,   826, -1494,
    2106,   949,  1939,  1169,   404,  1157,   297,  1622,   485,  1322,
     620,   407,  1048,    63,  1756,  1413,   969,  1882,  1737,  1064,
     847,   297,   297,  1920,  1283,  1579,   297,   809,   106,   427,
    1726,   275,   814,  1442,   815,  1378,   440,   819,   437,   820,
    1501,   822,   823,   297,   997,   944,   829,  -190, -1467,  1673,
      86, -1489, -1471,  1605,   297,   389,   827,   389,  1391,   440,
    1048,   850,  1378,  1786,   597,  1551,  1676,    56,   863,  1531,
    1533,  1755,  1184,  1184,  1184, -1471,  1266,    89,   275,   438,
     190, -1469,  1184,  1858,  1378,  1379,   439,   855,   856,   578,
     267,  1042,   297,   190,   177,  1590,  1569,  1378,  1322,   441,
     152,  1581,  1180,  1184,   297,  1184,     5,  1184,   297,  1427,
     297,  1184,  1184,  1184,  1184,  1184,  1184,  1184,  1184,   190,
     297,   297,   441,  1157,   501, -1545,  1535,   107,   442, -1471,
    1536,   451,  1707,   989,  -191, -1471,  1610,   297,  1859,    23,
    1614,  1615,  1632,  1617,  2200,   594,  1329,  1814,  1993,  1621,
    1506,   442,   317,   175,   389,   356,  1902, -1471,   440, -1423,
    1199,  1048,   877,  1870,   307,  1274,   106,  1330,  1539,   443,
    1185,  1185,  1185,  -581,   860,  1156,  2110,  1322,   970,   264,
    1185,  1717,  1747,  2205,  2275,   297,   297,  1262,   492,   950,
     952,  1419,   443,   386,  1718,   356,   871,    16,  1426,    64,
    1778,  1185,  2150,  1185,  1377,  1185,   250,  1428,    12,  1185,
    1185,  1185,  1185,  1185,  1185,  1185,  1185, -1471,    75,   362,
    -725,   441,  1943,   300,  1945,  1822, -1545,   300,    63,  1779,
     300,   300,  1054,  -579,    88,   948,  1796,   300,   945,  1322,
    1157,  -210,  1181,  2151,   363,  1648, -1545,  1737,  1811,  1601,
     442,  2152,   251,  1601,  1000,  1158,   974,  1973,  1548,   498,
    1812,  2169,  1136,  1378,  1193,   107,   176, -1471,  1835,  1658,
     387, -1516,  1844,  1202,  1378,  1669,  1184,  1184,  1184,  1184,
    1184,  1184,  1184,  2145,  2206,  2148,   946,  -581,  1843,  2005,
   -1489,   443, -1467,  1733,  2243,  2244,  1038,  1522,  1322,  1148,
    1374,  1375,  1376, -1545,  1507,  1888,  1889,  1890,  1553, -1467,
    1387,  1554,  1810,  1910,  1555,  1556,    41,  1087, -1483,     4,
    2245,  1102,   953,  1106,  2153,  1106,  1112, -1471,  1087,   623,
    2121,    32,  1649,  1423,  2086,  2087,  2088,  2089,  1728,  1430,
    1432,  1433,  1435,  1106,  1903,  1438,  1440,  -579,  1263,  1172,
     177,  1740,  1381,  1382,  1383,  1384,   300,  1385,  1657, -1545,
   -1429,    13,  1823,  1381,  1382,  1383,  1384,  1659,  1385,  2013,
     150, -1427,  2122,   300,  1185,  1185,  1185,  1185,  1185,  1185,
    1185,  1248,  1554,  1744,  1103,  1555,  1556,  2131,  2132,  1188,
    1248,  1164,  2123,  1909, -1545,  1980,  1274,  2133,  2071,  2071,
    1911,   297,   954,  1706,   300,   300,     5,  2065,   956,  1050,
    1916,  2134,   485,  2135,    64,   959,   960,  1149,  1837,  1650,
    2222,  1248,   965,  1203,   175,   175,   437,   190,   119,  1785,
    1104,     5,  1107,  1248,   174,  1787, -1545,  1127,  2069,  1788,
    1181,  1181,  1181,   262,   190,   297,  1139,     5,  1791,  1891,
    1181,  2071,  2071,   175,  1896,  1897, -1545,  1963,  1899,  1900,
    1901,  1515,  1904,  1905,  1822,    65,  1913,   438,  1322,  1322,
     356,  1181,  1827,  1181,   439,  1181, -1467,  1741,   119,  1181,
    1181,  1181,  1181,  1181,  1181,  1181,  1181,  1839,  2090,    75,
      75,  2091,  2183,  2239,  1378,  1944,   310,  1946,  2246,  2247,
    1322,   316,  1806,   250,  1661,  1662,  1663,  1664,  1665,  1666,
    1668,   195,  2072,  2008,  1516,  2162,  1521,  1112,    75,  1679,
     837,  1680,    19,   262,  1681,   196,   197,    29,  1912,  1948,
    1048,  1522,  1860,  2184,  1682,    45,  1745,   176,   176,  2248,
    2249,  1147,  1368,   839, -1467,   525,   440, -1483,   526,   251,
    1279,  1515,  1557,  1558,    93,  2014,  1746,   198,  1162,  1358,
    2073,  1241,  1048,   527,  2092,  2093,   176,   398,  1103,  2124,
     838,  1559,  1560,   528,    46, -1545,  2163,  2155,  1991,    26,
    2279,  1361,    47,  1381,  1382,  1383,  1384,  1807,  1385,   485,
     485, -1424,  1706,   840,  1914, -1545,   199,   200,    20,   201,
    1861,  1823,  1862,   190,  1516,  1184,   300,   263,   202,   441,
    1184,  2095,   279,  2097,  1515,   307,    94,   272,  1108,   503,
     288,   841,   602,  1557,  1558,  2191,  1979,   607,   932,   504,
     146,  1928,  1929, -1545,   259,   261,  1145,   190,   442,   295,
      30,  2114,  1559,  1560,  1181,  1181,  1181,  1181,  1181,  1181,
    1181,   203,   204,  1295,  1565,   469,    95,  1941,    96,  1509,
    1646,   325,   205,   358,  1683,  2015,  1949,  1516,   437,  1684,
     505,   842,  2142,  2143,  2204,  2130,  1060,   506,   936,   443,
    1061,  1308,  1685,  1038,  1828,   147,   148,   529,    33,  1322,
     504,   367,   368,  1141,  1296,    34,  1214,  1322,  1591,   374,
     375,  1566,  1297,  1185,  1829,   381,   384,   326,  1185,   438,
      36,  1797,  1513,  1038,  1065,  1066,   439,  1962,   530,   937,
    1142,  1235,   327,  1805,  2081,  1592,   938,  1309,  1067,   409,
    1686,   505,  1874,  2115,   416,  1303,    38,   206,   506,  1627,
     424,    44,   857,  1628,  1757,  1322,   529,  1758,  1304,   508,
     507,  1510,    49,  2116,   139,  1759,  1760,  1761,  1798,    53,
    1038,   425,   328,   858,   861,  1635,   140,    50,   468,  2261,
    1962,   300,   300,    59,  1068,  1298,   190,   530,  1687,  1395,
    1396,   411,  1608,   412,   531,  1599,  1609,   141,   440,  2075,
      68,   485,  2179,    61,   452,   142,    10,  2077,   939,  2079,
      10,    60,  1688,   249,   816,   532,    62,   249,  2085,     5,
     508,   817,   509,   454,  2196,   455,  2197,    72,  1567,  1383,
    1384,  1397,  1385,  1398,  1599,  2094,  1689, -1489,  -345,  -345,
     533,  1690,  1344,  1894,  1345,    75,   534,  1007,  1898,    79,
    -940,  1691,  1266,    82,  1542,  1692,   319,   320, -1489,   538,
      81,   441,   207,  1009,  1921,   115,  1922,  1399,  1400,  1401,
    1935,   940,  1936,   535,  2256,  2257,  2125,  2126,  1568,  1762,
     536,   116,   143,   509,    41,  2128,   510,   511,   103,   329,
     442,  -940,   512,   537,  1643,  1644,  1972,  1763,  -940,  1962,
    2265,   330,   208,  1698,  2168,  1006,   117,   437,   861, -1376,
   -1376, -1376, -1376,  1693,  1402,  1764,  1403,   264,   538,   119,
     539,  1038,   125,  1694,  1404,   297,   129,  1861,   540,  1862,
     130,   443,  1420,   131,  2210,  1799,  1424,   134,  -215,  2172,
    2173,   941,  2228,  2229,  1436,  1437,   132,   133,   438,  1010,
     137,   136,   209,   512,   144,   439,   155,   156,   157,   541,
     437,   171,   172,  1765,   173,   293,  1638,  1638,   186,  2192,
    -940,   224,   227,   228,  2193,   231,  2194,  2195,  1452,  1453,
     232,  1007,   229,  1181,  1256,  1257,  1258,  1259,  1181,   331,
     332,   234,  2198, -1375, -1375, -1375, -1375,  1009,  1405,  2190,
     235,   438,   333,  1013,   334,   237,  1014,  1454,   439,   542,
     241,  1766,  1114,  1455,   242,   243,  2258,   257,   271,  2259,
    2217,    41,  2218,   195,   808,   274,   275,   440,  1867,  2219,
     280,   281,   290,  -940,   297,   303,   305,   196,   197,  1015,
    1381,  1382,  1383,  1384,  1457,  1385,   307,  1838,  1458,  2226,
    2227,   312,   300,   313,   349,  1244,  1930,  1931,  1932,  1933,
     350,   314,  -940,  1813,  1767,   300,  1569,   352,  2276,   198,
    1381,  1382,  1383,  1384,  1768,  1385,  1864,   356,   359,   361,
     440,   365,  1386,  1010,   373,   364,   390,   391,   392,   300,
     441,   400,   401,   418,   437,   421,   423,  -940,  2283,  1841,
     448,   430,   466,  -940,   431,   335,  1841,  1016,   199,   200,
     463,   201,   471,  2268,  -362,  1883,   473,   476,  1884,   442,
     202,   488,   518,  1800,  1011,   336,   490,   522,   563,   300,
     573,  1012,   576,   571,   580,   438,  1769,  1013,   604,   603,
    1014,   582,   439,   441,   606,  1381,  1382,  1383,  1384,   608,
    1385,   611,  1017,  1018,   297,   190,   621,  1709,   683,   685,
     443,   693,   700,   203,   204,  1406,   701,   830,   805,   821,
     832,   843,   442,  1015,   205,   845,   851,   882,  1019,   883,
    1381,  1382,  1383,  1384,  -792,  1385,   926,  1004,  1660,   613,
     958,  1041,  1934,   961,   964,   975,  1020,  1056,  1937,   976,
     978,   984,  1073,  1074,  1021,   986,  1407,   297,  1073,  1081,
    1083,   992,   998,   443,   440,  1041,   981,   995,  2237,  1043,
    1029,  1073,  1001,  1002,  1381,  1382,  1383,  1384,  1051,  1385,
    1081,  1138,  1713,  1140,  1059,  2175,  1076,  1638,  1146,  1144,
    1970,  1016,  1165,  1381,  1382,  1383,  1384,  2024,  1385,   206,
    1869,  1716,  1381,  1382,  1383,  1384,  1152,  1385,  1173,  1171,
    1721,  1381,  1382,  1383,  1384,  1189,  1385,   871,  1200,  1877,
    1224,  1186,  1207,  1231,  1880,  1234,  1209,   441,  1472,  1211,
    1183,  1474,  1475,  1215,  1216,  1217,  1017,  1018,  1229,  1447,
    1448,  1449,  1218,  1219,  1220,  1976,  1221,  1450,  1222,  1236,
    1242,  1982,  1248,  1250,  1253,  1272,   442,  1276,  1260,  2063,
    1208,  1294,  1019,  2064,  1895,  1381,  1382,  1383,  1384,  1291,
    1385,  1292, -1378,  2074,  1301,  1305,  1306,  1307,  1311,  1318,
    1020,  1316,  1114,   165,  1343,  1451,  1340,  1347,  1021,  1349,
     468,   297, -1502,  1350,  1362,   319,  1240,   443,  1371,  1392,
    1388,  1409,  1422,  1429,  1443,  1245,  1444,  1502,  1034,  1525,
    1540,  1545,  1563,  1578,   207,  1582,  1596,  1604,  1611,  1612,
    1624,  1636,  1619,  1073,  1641,  1385,  1674,  1678,  1675,  1698,
    1412,  1073,  1280,  1710,  1712,  1715,  1727,  1729,   300,  1732,
    1720,  1722,  1750,  1516,  1773,  1775,  1777,  1781,  1789,  1041,
    1793,  1815,  1818,  1792,   208,  1830,  1831,  2113,  1819,  1846,
    1007,  1832,  1845,  1854,  1865,  1321,  1856,  1650,  1878,  1926,
    1927,  1907,  1951,  1073,  1957,  1953,  1009,  1073,  1959,  1964,
    1967,  1971,  1985,  1452,  1453,  1988,  1339,  1990,  1995,  1280,
    2187,  1823,  1998,  2003,  2000,  2189,  2007,  2009,  2011,  2021,
    2026,  2025,  2028,  2060,   209,  2078,  2080,  2096,  2082,  2100,
    2164,  2098,  1454,  2103,  2117,  2119,  1240,  2118,  1455,  1081,
    2137,  2178,  2146,  2180,  2139,  2158,  2015,  2165,  1183,  1183,
    1183,   300,  -748,  2037,  2170,  -748,  2166,  2176,  1183,  2177,
    2199,  1456,  2201,  2209,  2203,  2224,  2215,  2216,  2234,  1457,
    2251,  2252,  2262,  1458,  1321,  2264,  2266,  2281,    15,  1183,
      28,  1183,  1010,  1183,    74,   433,   193,  1183,  1183,  1183,
    1183,  1183,  1183,  1183,  1183,   165,   273,   260,   270,   294,
    1606,   595,   807,  1459,  1460,  1354,   517,   226,  1634,  1461,
    1355,  1640,  2023,  1154,   296,   151,   818,  2231,  1974,  1462,
     465,   153,   309,    67,  2211,   245,  1463,   402,  -748,   999,
    1369,  1464, -1471,   698,  1039,  1876,  1013,  2253,  2187,  1014,
    1809,  2041,  2042,  2112,  -748,   982,  2043,  2044,  2188,  1465,
    1873,  2045,  1872,  1321,  2046,  2047,   994,   624,  1168,  2182,
    2254,   170,  1917,  1073,   585,  1977,  1255,   300,  1887,  1703,
    2232,  2233,  1015,  1264,  2236,  1915,  1589,   300,  1702,  1940,
    1547,  2238,  1543,  1240,  1784,  2270,  2271,  1947,  1562,  2273,
    1602,  2274,  2102,  1966,  1794,  1574,  2263,  1073,  1300,  1978,
    1820,  1633,  1603,  1994,  1323,  1324,  1126,  1338,  2260,  1618,
    1852,  2144,  1853,  1881,  2149,  1321,  1987,  2213,  2160,  1315,
    1752,  1626,  1504,  1187,  1836,  2280,   318,  1446,  1040,   414,
    -748,  1576,   988,   824,  1271,  2255,    76,  1190,  1280,  2272,
    1016,  -748,   963,  1731,     0,     0,     0,     0,  1038,  1194,
       0,   468,  1183,  1183,  1183,  1183,  1183,  1183,  1183,     0,
       0,     0,     0,     0,     0,     0,  1038,  2278,     0,     0,
       0,  -748,     0,     0,  1321,     0,     0,     0,  -748,     0,
       0,     0,  -748,     0,  -748,  1017,  1018,  -748,     0,  -748,
       0,     0,     0,     0,  1708,  1466,  1467,  1468,  1469,  1470,
     468,  1471,     0,  1472,  1473,     0,  1474,  1475,     0,     0,
       0,  1019,     0,     0,     0,     0,  1073,     0,     0,   904,
    -748,     0,     0,     0,  -883,  -748,     0,  -883,     0,  1020,
    -883,  -883,  -883,     0,     0,     0,     0,  1021,     0,     0,
     297,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2242,     0,     0,     0,     0,   468,     0,     0,
       0,  -748,   389,     0,     0,     0,     0,     0,     0,     0,
    1007, -1471,     0,     0,     0,     0,  -883,     0,  1738,  1739,
       0,     0,     0,     0,     0,     0,  1009,     0,  -748,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -883,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -883,     0,  -748,     0,
       0,     0,     0,     0,  -748,     0,     0,     0,     0,     0,
       0,     0,     0,  -748,  -748,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1321,  1321,     0,     0,     0,     0,
    1073,  1073,     0,  -748,     0,     0,     0,     0,     0,  -748,
       0,     0,     0,     0,  -748,     0,     0,     0,  1821,     0,
       0,     0,  1010,     0,  -883,  -883,  1321,  -748,     0,     0,
       0, -1471,     0,   437,     0,  -748,     0,     0,  -748,     0,
       0,  1842,     0,     0,  -748,     0,  1851,  1851,     0,     0,
       0,     0,  -883,  -883,     0,     0,     0,     0,     0,  -883,
       0,     0,     0,  -883,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   438,     0,  1013,     0,     0,  1014,
       0,   439,  -883,     0,     0,     0,     0,     0,     0,     0,
    -883,     0,     0,  -883,  -883,     0,     0,     0,     0,     0,
    -883,     0,     0,     0,  -883,     0,  -883,     0,     0,  -883,
       0,  -883,  1015,     0,     0,     0,     0,     0,     0,     0,
       0,  1240,  1240,  1240,  -883,  -883,     0,     0,     0,     0,
    -883,  1183,     0,     0,     0,     0,  1183,     0,     0,     0,
    -883,     0,  -883,     0,     0,     0,     0,  -883,     0,  1073,
       0,     0,     0,   440,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -883,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1016,     0,     0,  -883,     0,     0,     0,  1006,     0,     0,
     861,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1950,     0,
    -883,  1202,     0,     0,  1961,  1321,   441,     0,     0,     0,
       0,     0,     0,  1321,     0,  1017,  1018,     0,     0,     0,
       0,     0,  1073,     0,     0,     0,     0,     0,     0,     0,
    1589,     0,     0,     0,     0,   442,  -883, -1467,     0,     0,
       0,  1019,     0,     0,     0,  -883,  -883,     0,     0,     0,
       0,     0,     0,  1007,     0,     0,     0,  1961,     0,  1020,
       0,  1321,     0,     0,     0,  -883,     0,  1021,     0,  1009,
     297,  -883,     0,     0,     0,     0,   443,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -883,
       0,     0,     0,     0,     0,  2067,  2067,  -883,     0,     0,
    -883,  -357,   625,     0,     0,     0,  -883,  -883,  -883,     0,
       0,     0,  -883,     0,  -883,     0,  -883,  -883,  -883,     0,
       0,     0,   626,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   627,     0,     0,   628,   629,
     630,   631,   632,   633,   634,     0,  2067,     0,     0,  2067,
       0,     0,     0,     0,     0,  1010,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   437,     0,     0,     0,
       0,     0,     0,     0,     0,   635,  1961,   636,   637,   638,
     639,   640,   641,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1011,     0,     0,     0,
       0,     0,     0,  1012, -1467,     0,     0,   438,  1081,  1013,
       0,     0,  1014,     0,   439,  2141,     0,     0,     0,   642,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2067,     0,  -357,     0,     0,     0,
    -357,     0,     0,     0,     0,  1015,     0,     0,     0,     0,
       0,  2181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -1489,  -357,
       0,     0, -1467,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2067,  -357,     0,   440,     0,     0, -1489,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   643,  1007,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1016,     0,    41,     0,     0,   644,  1009,
       0,     0,     0,     0, -1526,     0,     0,     0,  -357,  -357,
       0,     0,     0,     0,     0,     0,  -357,   645,     0,     0,
    -357,     0,  1280,     0,     0,     0,     0,     0,     0,   441,
       0,     0,     0,     0,   646,     0,     0,  2221,  1017,  1018,
    2223,     0,     0,     0,     0,     0,     0,     0,     0,  -576,
     586,     0,  -616,     0,  -616,     0,     0,   647,   442,  -616,
       0,     0,     0,     0,  1019,     0,     0,  -616,     0,     0,
    2240,     0,   648,     0,     0,     0,     0,     0,  1094,   649,
       0,   650,  1020,     0,     0,  1010,  1006,     0,  -357,   861,
    1021,     0,  -357,   297,   651,     0,     0,     0,     0,   443,
    -616,  -616,     0,  1081,   652,     0,     0,     0,  -616,     0,
       0,   653,     0,  -357,  2269,     0,     0,     0,     0,     0,
    -616,     0,     0,     0,  -616,     0,     0,     0,     0,     0,
       0,     0,  -357,     0,     0,  -357,  -616,  1095,     0,  1013,
       0,     0,  1014,     0,     0,     0,     0,     0,   654,     0,
     655,   656,   657,     0,     0,     0,     0,  1280,     0,     0,
    -616,     0,  1007,     0,     0,     0,     0,  -616,  -616,     0,
       0,     0,     0,     0,   658,  1015,     0,     0,  1009,     0,
       0,     0,     0,     0,  -576,     0,     0,     0,  -576,     0,
       0,     0, -1526,     0,     0,     0,     0,     0,  -616,     0,
     659,   660,   661,     0,     0,     0,     0,     0,     0,     0,
    -616,     0,     0,   662,  -616,     0,   663,     0,     0,     0,
       0,     0,     0,     0,     0,  -357,     0,     0,  -616,     0,
       0,     0,  -576,     0,     0,  -616,  -357,     0,  -616,  -616,
       0,     0,     0,  1016,     0,     0,  -616,     0,     0,     0,
       0,     0,     0,  -616,     0,  -616,     0,     0,  -616,     0,
       0,     0,     0,     0,  1010,     0,     0,     0,     0,     0,
    1007,     0,     0,     0,     0,   437,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1009,     0,  1017,  1018,
       0,     0,     0,     0,     0,  -616,     0,     0, -1545,  -616,
     586,  -616,  -616,     0,  -616,  1011,     0,     0,  -616,  -616,
       0,     0,  1012,     0,  1019,     0,   438,  -616,  1013,     0,
       0,  1014,     0,   439,  -991,     0,     0,     0,     0,     0,
       0,  -616,  1020,     0,     0,     0,     0,  -991,     0,     0,
    1021,     0,   190,   297,     0,     0,  -616,     0,     0,     0,
    -616,  -616,     0,     0,  1015,     0,     0,     0,  -616,     0,
    -576,     0,     0,     0,     0,  1094,     0,  -616,     0,     0,
    -616,     0,  1010,     0,  -616,     0,  -616,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -616,     0,     0,  -616,
       0,     0,     0,     0,  -616,   440,  -616,     0,     0,     0,
       0,     0,     0,     0,  -616,     0,     0,     0,     0,     0,
    -616,     0,     0,     0,  -616,     0,     0,  -616,  -616,     0,
       0,     0,  1016,  -616,  1039,   699,  1013,     0,     0,  1014,
       0,     0,  -616,     0,  -616,  -616,     0,     0,  -616,     0,
    -616,     0,     0,     0,     0,  -616,     0,     0,  -616,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   441,     0,
    -616,  -616,  1015,     0,  -616,     0,  -616,  1017,  1018,     0,
       0,  -616,     0,     0,     0,     0,     0,     0,  -616,     0,
       0,     0,  -616,  1226,     0,  -616,     0,   442,  -616,  -616,
       0,   587,     0,  1019,  -616,     0,  -616,     0,     0,     0,
       0,     0,     0,  -616,  -616,  -616,     0,     0,  -616,     0,
       0,  1020,     0,     0,     0,     0,     0,     0,     0,  1021,
       0,     0,   297,     0,     0,     0,     0,     0,   443,     0,
    1016,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -616,     0,     0,     0,  -616,
       0,  -616,     0,     0,     0,     0,     0, -1459,  -616,     0,
   -1459,     0,     0, -1459, -1459, -1459,     0,     0,  1265,     0,
       0,     0, -1459,     0,     0,  1017,  1018,     0,     0,     0,
       0,  -616,     0,     0,     0, -1545,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -616,     0,     0,     0,
       0,  1019,     0,     0,     0,     0,     0,     0,     0, -1459,
       0,  -991,     0,     0,     0,     0,     0,  -616,     0,  1020,
       0,     0,     0,     0,  -991,     0,  -616,  1021,     0,   190,
     297,     0,     0, -1459,     0,     0,     0,     0,     0,  -616,
       0,     0,     0,     0,  -616,     0,  -616,     0,     0, -1459,
       0,     0,     0,  1006,  -616,     0,   861,     0,     0,  1481,
    1482,  1483,     0,     0,  -616,     0,     0,     0,     0,     0,
       0,     0,     0,  -616,     0,     0,     0,     0,     0,     0,
       0,     0,  -616,     0,  -616,  -616,     0,     0,  -616,     0,
    -616,     0,     0,     0,     0,  -616,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1484,     0, -1459, -1459,     0,
       0,  -616,     0,     0,     0,     0,  -616,     0,     0,     0,
       0,  -616,     0,     0,     0,     0,     0,     0,     0,  1007,
       0,     0,  -616,     0,     0, -1459, -1459,     0,     0,     0,
       0,   587, -1459,     0,  -616,  1009, -1459,     0,     0,     0,
       0,     0,     0,     0,  -616,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -1459,     0,     0,     0,     0,
       0,     0,     0, -1459,     0,     0, -1459, -1459,     0,     0,
       0,     0,     0, -1459,     0,     0,     0, -1459,     0, -1459,
       0,     0, -1459,     0, -1459,     0,     0,     0,     0,     0,
       0,     0,     0,  1452,  1453,     0,     0, -1459, -1459,     0,
       0,     0,     0, -1459,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -1459,     0, -1459,     0,     0,     0,     0,
   -1459,  1010,  1454,     0,     0,     0,     0,     0,  1455,     0,
       0,     0,   437,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -1459,     0,   628,   629,   630,   631,   632,
     633,  1485,     0,     0,     0,     0, -1459,     0,     0,  1457,
       0,     0,  1011,  1458,     0,     0,     0,     0,     0,  1012,
       0,     0,     0,   438,     0,  1013,     0,     0,  1014,     0,
     439,     0,   635, -1459,   636,   637,   638,   639,   640,   641,
       0,     0,     0,  1486,  1487,     0,     0,     0,     0,  1488,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1489,
       0,  1015,     0,     0,     0,     0,  1490,     0,     0, -1459,
       0,     0,     0,     0,     0,     0,   642,     0, -1459, -1459,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1491,
       0,     0,     0,     0,     0, -1459,     0,     0, -1459, -1459,
       0,     0,   440,     0, -1459,   702,     0,   703,     0,     0,
       0,     0,   704,     0,     0,     0,     0,     0,     0,     0,
     705,     0, -1459,     0,     0,     0,     0,     0,     0,  1016,
   -1459,     0,     0, -1459,     0,     0,     0,     0,     0, -1459,
   -1459, -1459,     0,     0,     0, -1459,     0, -1459,     0, -1459,
   -1459, -1459,     0,   706,   707,     0,     0,     0,     0,   643,
       0,   708,     0,     0,     0,   441,     0,     0,     0,     0,
       0,     0,     0,   709,  1017,  1018,     0,   710,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   711,
       0,     0,     0,     0,   442,     0,     0,     0,     0,     0,
    1019,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   646,     0,   712,     0,     0,     0,     0,  1020,     0,
     713,   714,     0,     0,  1006,     0,  1021,   861,     0,   297,
       0,     0,     0,     0,     0,   443,  1492,  1493,     0,     0,
       0,  1494,     0,  1472,     0,  1495,  1474,  1475,     0,   648,
       0,   715,     0,     0,     0,     0,     0,     0,   650,     0,
       0,     0,     0,   716,     0,     0,     0,   717,     0,     0,
       0,   651,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   718,     0,     0,     0,     0,     0,     0,   719,     0,
       0,   720,   721,     0,     0,     0,     0,     0,     0,   722,
    1007,     0,     0,     0,     0,     0,   723,     0,   724,     0,
       0,   725,     0,     0,     0,     0,  1009,     0,     0,   702,
       0,   703,     0,     0,     0,     0,   704,   655,   656,   657,
    1130,     0,     0,     0,   705,     0,     0,     0,     0,  1006,
       0,     0,   861,     0,     0,     0,     0,     0,   726,     0,
       0,     0,   727,     0,   728,     0,     0,     0,     0,     0,
       0,   729,     0,     0,     0,     0,     0,   706,   707,     0,
       0,     0,     0,     0,     0,   708,     0,   659,   660,   661,
       0,     0,     0,     0,   730,     0,     0,   709,     0,     0,
       0,   710,     0,     0,     0,     0,     0,     0,     0,   731,
       0,     0,  1010,   711,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   437,     0,  1007,     0,     0,     0,     0,
     732,     0,     0,     0,     0,     0,     0,   712,  1337,   733,
       0,  1009,     0,     0,   713,   714,     0,     0,     0,     0,
       0,     0,   734,  1011,     0,     0,     0,   735,     0,   736,
    1012,     0,     0,     0,   438,     0,  1013,   737,     0,  1014,
       0,   439,     0,     0,     0,   715,     0,   738,     0,     0,
       0,     0,     0,     0,     0,     0,   739,   716,     0,     0,
       0,   717,  1607,     0,     0,   740,     0,   741,   742,     0,
       0,   743,  1015,   744,     0,   718,     0,     0,   745,     0,
       0,     0,   719,     0,     0,   720,   721,     0,     0,     0,
       0,     0,     0,   722,   746,     0,     0,  1010,     0,   747,
     723,     0,   724,     0,   748,   725,     0,     0,   437,     0,
       0,     0,     0,   440,     0,   749,  1006,     0,     0,   861,
       0,     0,     0,     0,     0,     0,     0,   750,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   751,  1011,     0,
    1016,     0,   726,     0,     0,  1012,   727,     0,   728,   438,
    1006,  1013,     0,   861,  1014,   729,   439,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   441,     0,   730,     0,
       0,     0,     0,     0,     0,  1017,  1018,  1015,     0,     0,
       0,     0,  1007,   731,     0,     0,     0,  1006,     0,     0,
     861,     0,     0,     0,  1008,   442,     0,     0,  1009,     0,
       0,  1019,     0,     0,   732,     0,     0,     0,     0,  1006,
       0,     0,   861,   733,     0,     0,  1007,     0,   440,  1020,
       0,     0,     0,     0,     0,     0,   734,  1021,  1088,     0,
     297,   735,  1009,   736,     0,     0,   443,     0,     0,     0,
       0,   737,     0,     0,     0,  1016,     0,     0,     0,     0,
       0,   738,     0,     0,     0,     0,     0,     0,     0,     0,
     739,     0,     0,  1007,     0,     0,     0,     0,     0,   740,
       0,   741,   742,     0,     0,   743,     0,   744,     0,  1009,
       0,   441,   745,     0,  1010,  1007,     0,     0,     0,     0,
    1017,  1018,     0,  1130,     0,   437,     0,  1134,   746,     0,
       0,  1009,     0,   747,     0,     0,     0,  1608,   748,     0,
     442,  1609,     0,     0,     0,     0,  1019,     0,  1010,   749,
       0,     0,     0,     0,     0,  1011,     0,     0,     0,   437,
       0,   750,  1012,     0,  1020,     0,   438,     0,  1013,     0,
       0,  1014,  1021,   439,     0,   297,     0,     0,     0,     0,
       0,   443,     0,     0,     0,     0,     0,     0,     0,  1011,
       0,     0,     0,     0,     0,  1010,  1012,     0,     0,     0,
     438,     0,  1013,     0,  1015,  1014,   437,   439,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1010,     0,  1006,
       0,     0,   861,     0,     0,     0,     0,     0,   437,     0,
       0,     0,     0,     0,     0,     0,  1011,     0,  1015,     0,
       0,     0,     0,  1012,     0,   440,     0,   438,     0,  1013,
       0,     0,  1014,     0,   439,     0,     0,     0,  1011,     0,
       0,     0,     0,     0,     0,  1012,     0,     0,     0,   438,
       0,  1013,  1016,     0,  1014,     0,   439,     0,     0,   440,
       0,     0,     0,     0,     0,  1015,     0,     0,  1006,     0,
       0,   861,     0,     0,     0,  1007,     0,     0,     0,     0,
    1006,     0,     0,   861,     0,     0,  1016,  1015,   441,     0,
       0,  1009,     0,     0,     0,     0,     0,  1017,  1018,     0,
       0,     0,     0,     0,     0,     0,   440,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   442,     0,     0,
       0,     0,   441,  1019,     0,     0,     0,     0,   440,     0,
       0,  1017,  1018,  1016,     0,     0,     0,     0,     0,     0,
       0,  1020,     0,     0,  1007,     0,     0,     0,     0,  1021,
       0,   442,   297,     0,     0,  1016,  1007,  1019,   443,     0,
    1009,     0,     0,     0,     0,     0,     0,     0,     0,   441,
       0,     0,  1009,  1342,     0,  1020,     0,  1010,  1017,  1018,
       0,     0,     0,  1021,     0,     0,   297,     0,   437,     0,
       0,   441,   443,     0,     0,     0,     0,     0,   442,     0,
    1017,  1018,     0,     0,  1019,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1011,     0,
     442,     0,  1020,     0,     0,  1012,  1019,     0,     0,   438,
    1021,  1013,     0,   297,  1014,     0,   439,     0,     0,   443,
       0,     0,     0,     0,  1020,     0,  1010,  1006,     0,     0,
     861,     0,  1021,     0,     0,   297,     0,   437,  1010,     0,
       0,   443,     0,     0,     0,     0,     0,  1015,     0,   437,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1011,     0,     0,
       0,     0,     0,     0,  1012,     0,     0,     0,   438,  1011,
    1013,     0,     0,  1014,     0,   439,  1012,     0,   440,     0,
     438,     0,  1013,     0,     0,  1014,     0,   439,     0,     0,
       0,     0,  1006,  1007,     0,   861,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1016,  1015,     0,     0,  1009,
       0,     0,     0,     0,     0,     0,     0,     0,  1015,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   441,     0,     0,     0,     0,     0,   440,     0,     0,
    1017,  1018,     0,     0,     0,     0,     0,     0,     0,   440,
       0,     0,     0,     0,     0,     0,     0,     0,  1007,     0,
     442,     0,     0,     0,  1016,     0,  1019,     0,     0,     0,
       0,     0,     0,     0,  1009,     0,  1016,     0,     0,     0,
       0,     0,     0,     0,  1020,  1010,     0,     0,     0,     0,
       0,     0,  1021,     0,  1616,   297,   437,     0,     0,     0,
     441,   443,     0,     0,     0,     0,     0,  1722,     0,  1017,
    1018,     0,   441,     0,     0,     0,     0,     0,     0,     0,
       0,  1017,  1018,     0,     0,     0,  1011,     0,     0,   442,
       0,     0,     0,  1012,     0,  1019,     0,   438,     0,  1013,
       0,   442,  1014,     0,   439,     0,     0,  1019,     0,     0,
       0,     0,     0,  1020,     0,     0,     0,     0,     0,     0,
    1010,  1021,     0,     0,   297,  1020,     0,  1007,     0,     0,
     443,   437,     0,  1021,     0,  1015,   297,     0,     0,     0,
       0,     0,   443,  1009,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1011,     0,     0,     0,     0,     0,     0,  1012,     0,
       0,     0,  1052,     0,  1013,     0,   440,  1014,     0,   439,
       0,     0,     0,   628,   629,   630,   631,   632,   633,   634,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1016,     0,     0,     0,     0,     0,     0,
    1015,     0,  2029,  2030,     0,     0,     0,     0,     0,     0,
     635,     0,   636,   637,   638,   639,   640,   641,     0,  1010,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   441,
     437,     0,     0,     0,     0,     0,  1007,     0,  1017,  1018,
       0,   440,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1009,     0,   642,     0,     0,     0,   442,     0,
    1175,     0,     0,     0,  1019,     0,     0,  1012,  1016,     0,
       0,   438,     0,  1013,     0,     0,  1014,     0,   439,     0,
       0,     0,  1020,     0,     0,     0,     0,     0,     0,     0,
    1021,     0,     0,   297,     0,     0,     0,     0,     0,   443,
       0,     0,  1007,     0,   441,     0,     0,     0,     0,  1015,
       0,     0,     0,  1017,  1018,     0,     0,     0,  1009,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2031,     0,
       0,     0,     0,   442,     0,     0,     0,   643,  1010,  1019,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   437,
     440,     0,     0,   644,     0,     0,     0,  1020,     0,     0,
       0,     0,     0,     0,     0,  1021,     0,     0,   297,  2032,
    2033,     0,     0,     0,   443,     0,     0,  1016,     0,  1175,
       0,     0,     0,     0,     0,     0,  1012,     0,     0,   646,
     438,     0,  1013,     0,  2034,  1014,     0,   439,     0,     0,
       0,     0,     0,     0,  1010,     0,     0,     0,     0,     0,
       0,     0,   647,   441,     0,   437,     0,     0,     0,     0,
       0,     0,  1017,  1018,     0,     0,     0,   648,  1015,     0,
       0,     0,     0,     0,   649,     0,   650,     0,     0,  2035,
       0,     0,   442,     0,     0,  1175,     0,     0,  1019,   651,
       0,     0,  1012,     0,     0,     0,   438,     0,  1013,     0,
       0,  1014,     0,   439,     0,     0,  1020,     0,     0,   440,
       0,     0,     0,     0,  1021,     0,     0,   297,     0,     0,
       0,     0,     0,   443,  1176,  1177,     0,     0,     0,     0,
       0,     0,  1667,  1178,  1015,     0,  1016,     0,     0,     0,
       0,     0,     0,   654,     0,   655,   656,   657,     0,     0,
       0,     0,  2036,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2037,     0,     0,     0,
       0,     0,   441,     0,     0,   440,     0,     0,     0,     0,
       0,  1017,  1018,     0,     0,     0,     0,     0,     0,     0,
    2038,     0,     0,     0,     0,   659,   660,   661,     0,     0,
       0,   442,  1016,     0,     0,     0,     0,  1019,   662,     0,
       0,   663,  2039,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1020,     0,     0,     0,     0,
       0,  2040,     0,  1021,     0,     0,   297,     0,   441,     0,
       0,     0,   443,  1176,  1177,     0,     0,  1017,  1018,     0,
       0,  1893,  1178,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   442,     0,     0,
       0,     0,     0,  1019,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1020,     0,     0,     0,     0,     0,     0,     0,  1021,
       0,     0,   297,     0,     0,     0,     0,     0,   443,  1176,
    1177,     0,     0,     0,     0,     0,     0,     0,  1178
};

static const yytype_int16 yycheck[] =
{
      84,   176,   230,   132,   412,   571,   720,   928,   460,   684,
     892,   686,   535,   536,   323,  1051,  1123,   862,   863,  1303,
     749,   196,   197,  1233,  1233,   691,   201,   202,   203,   204,
     205,   258,   207,  1233,   209,   887,   228,   652,   230,   231,
     906,   369,   234,  1130,  1412,  1413,   898,  1233,   487,  1233,
    1044,  1147,  1708,   919,  1266,    17,     6,  1071,  1144,    23,
      13,  1257,  1258,  1259,   193,  1079,    49,    22,     6,    30,
      54,  1226,     6,   248,    86,   250,   160,     9,   888,   819,
     603,    17,   257,    48,     6,    96,   122,    48,     9,    49,
     265,   283,   121,   886,     9,   287,  1203,   289,    70,   121,
      85,   182,   895,   896,   279,   280,   298,   282,  1263,   301,
      28,  1125,    56,   288,    70,   114,   444,   910,   911,    72,
      66,   348,    92,   118,   170,    96,    86,  1857,   355,   149,
       9,   924,   925,   171,    73,    88,    49,    56,   214,  1294,
     171,    56,    56,    21,  1403,    70,   236,     9,   236,     6,
      40,  1410,   327,     9,    37,   330,   331,  1307,   333,   107,
     248,  1006,   390,  1021,   392,  1107,  1011,   224,   360,  1396,
    1164,   214,    70,  1197,   349,   218,  1403,  1113,   370,   240,
      66,   856,   409,  1410,    61,   377,  1842,  1342,   240,   416,
     518,   366,   236,   868,    70,     0,   388,   115,   390,    22,
     392,   248,   429,  1289,   122,   293,     0,    70,   260,    28,
     118,    88,    89,   174,   279,   236,   150,  1845,   300,   325,
     236,   413,   306,   415,   231,  1070,   453,   234,   187,   214,
     113,  1024,   456,   908,   368,   463,   411,   236,  1108,   466,
    1803,   236,   322,   180,   419,   402,  1167,   368,   236,   171,
     214,  2260,     9,   692,   236,  1121,   996,   997,   173,   123,
    1347,  1054,    11,     9,   121,  1430,  1202,  1363,  1433,  2278,
     462,   463,   464,  1438,   978,  1145,   283,   248,   260,    63,
     287,    49,   289,   889,   418,  1371,   347,   479,  1443,   346,
     310,   298,   519,    50,   301,  1088,   115,   418,   473,   236,
     524,   458,   908,   167,   236,   349,  1392,   913,   223,   393,
     537,  1325,   396,   325,   248,   380,   195,  1311,   493,   494,
     995,   122,    71,   236,   227,   228,   406,  1523,   236,   435,
    1175,   236,   998,  1545,   305,   218,  1964,   214,  1131,   195,
      23,    49,   325,  1136,  1226,  1281,   573,   233,   523,   207,
      31,   578,   442,   360,   442,   301,   403,   236,   533,   441,
     448,   331,   280,   370,   243,   325,   256,  1391,  1310,   561,
     377,    63,   978,    70,   455,   990,   991,   321,   183,   455,
     236,  1263,   341,   322,    70,   560,   223,   243,   360,   441,
    2120,   583,   584,   568,  1246,   424,   442,  1960,   260,   442,
    1963,   117,   424,   969,   442,   441,   442,  1343,   415,  1113,
     539,   442,  1294,   168,  1521,   301,   293,  1676,  1505,  1271,
     647,   442,   442,   435,   394,  1291,   442,   602,   251,   110,
    1444,   442,   607,  1226,   609,   360,   315,   612,   195,   614,
    1233,   616,   617,   442,   409,   123,   621,   442,   409,  1676,
     434,   436,   435,   403,   442,   462,   318,   464,  1198,   315,
    1342,   653,   360,  1549,   400,  1275,  1402,   406,   225,  1262,
    1263,   442,  1176,  1177,  1178,   435,   438,   433,   442,   236,
     441,   364,  1186,  1633,   360,   457,   243,   662,   663,   403,
     236,  1097,   442,   441,   442,  1305,   411,   360,  1202,   378,
     455,  1294,   978,  1207,   442,  1209,   311,  1211,   442,  1213,
     442,  1215,  1216,  1217,  1218,  1219,  1220,  1221,  1222,   441,
     442,   442,   378,   441,   443,   269,   404,   350,   407,   442,
     408,   214,   457,   861,   442,   448,  1329,   442,  1634,   120,
    1333,  1334,  1352,  1336,  2107,   498,   403,  1583,  1832,  1342,
     187,   407,   455,   245,   561,   236,   223,   325,   315,   457,
     999,  1443,   691,  1650,   175,  1579,   251,   424,  1780,   448,
    1176,  1177,  1178,   341,   411,   376,   434,  1281,   455,   363,
    1186,   457,  1514,   187,  2240,   442,   442,    49,   422,   305,
      32,  1206,   448,   200,   457,   236,    30,   150,  1213,   354,
    1532,  1207,   166,  1209,    70,  1211,   372,  1213,   455,  1215,
    1216,  1217,  1218,  1219,  1220,  1221,  1222,   325,   310,   211,
     187,   378,  1777,   815,  1779,   126,   240,   819,   168,   187,
     822,   823,   895,   341,   139,   810,  1572,   829,   316,  1343,
     441,   442,   978,   207,   236,    28,   260,  1734,  1580,  1315,
     407,   215,   418,  1319,   881,   456,   831,  1812,  1273,   139,
     187,  2029,   925,   360,   992,   350,   358,   435,  1604,  1373,
     277,   363,   187,    30,   360,  1389,  1380,  1381,  1382,  1383,
    1384,  1385,  1386,  2009,   288,  2011,   364,   455,  1620,  1844,
     188,   448,   126,  1503,    90,    91,   888,   441,  1402,   118,
    1176,  1177,  1178,   300,   341,  1699,  1700,  1701,     9,    66,
    1186,    12,  1578,  1727,    15,    16,   214,   909,   329,   183,
     116,   913,   164,   915,   288,   917,   918,   435,   920,   563,
     198,    25,   115,  1209,  1930,  1931,  1932,  1933,  1452,  1215,
    1216,  1217,  1218,   935,   411,  1221,  1222,   455,   210,   976,
     442,   163,   449,   450,   451,   452,   948,   454,  1373,   269,
     457,   455,   263,   449,   450,   451,   452,  1373,   454,  1855,
     455,   457,   240,   965,  1380,  1381,  1382,  1383,  1384,  1385,
    1386,   348,    12,  1512,   913,    15,    16,  1997,  1997,   981,
     348,   966,   260,  1725,   198,   199,  1810,  1997,  1885,  1886,
     156,   442,   244,  1418,   996,   997,   311,   448,   815,   893,
     279,  1997,   819,  1997,   354,   822,   823,   236,  1611,   202,
    2146,   348,   829,   180,   245,   245,   195,   441,   442,  1546,
     914,   311,   916,   348,   128,  1552,   240,   921,  1884,  1553,
    1176,  1177,  1178,   180,   441,   442,   930,   311,  1562,  1707,
    1186,  1938,  1939,   245,  1712,  1713,   260,  1793,  1716,  1717,
    1718,   213,  1720,  1721,   126,   405,   156,   236,  1572,  1573,
     236,  1207,  1598,  1209,   243,  1211,   233,   289,   442,  1215,
    1216,  1217,  1218,  1219,  1220,  1221,  1222,  1613,  1934,   310,
     310,  1937,   258,  2177,   360,  1777,   240,  1779,   294,   295,
    1604,   245,   200,   372,  1380,  1381,  1382,  1383,  1384,  1385,
    1386,    11,  1886,  1849,   266,  2022,   426,  1109,   310,    31,
     368,    33,   236,   260,    36,    25,    26,   455,   284,  1781,
    1812,   441,   251,   299,    46,   169,   240,   358,   358,   335,
     336,   948,  1169,   368,   301,     1,   315,   107,     4,   418,
    1079,   213,   253,   254,   275,   154,   260,    57,   965,  1151,
    1892,  1045,  1844,    19,  1938,  1939,   358,   311,  1097,   437,
     418,   272,   273,    29,   208,   240,  2022,  2013,  1830,    27,
    2264,  1156,   216,   449,   450,   451,   452,   285,   454,   996,
     997,   457,  1607,   418,   284,   260,    96,    97,   312,    99,
     319,   263,   321,   441,   266,  1709,  1198,    63,   108,   378,
    1714,  1943,   198,  1945,   213,   175,   337,   455,   917,   124,
     206,   368,   526,   253,   254,  2071,  1819,   531,   188,   195,
     319,  1748,  1749,   437,   455,   455,   935,   441,   407,   225,
     102,  1973,   272,   273,  1380,  1381,  1382,  1383,  1384,  1385,
    1386,   151,   152,   166,     9,   399,   377,  1774,   379,   324,
    1369,     1,   162,   455,   176,   264,  1783,   266,   195,   181,
     236,   418,  2004,  2005,  2110,  1996,   404,   243,   195,   448,
     408,   258,   194,  1275,   240,   374,   375,   202,   236,  1793,
     195,   277,   278,     8,   207,   455,  1012,  1801,     8,   285,
     286,    56,   215,  1709,   260,   291,   292,    47,  1714,   236,
      83,     9,  1241,  1305,   296,   297,   243,  1792,   233,   236,
      35,  1037,    62,  1575,  1917,    35,   243,   304,   310,   326,
     242,   236,  1655,   240,   331,   423,   120,   237,   243,   159,
     167,   356,   167,   163,    33,  1849,   202,    36,   436,   315,
     255,   416,   455,   260,    60,    44,    45,    46,    56,   455,
    1352,   188,   102,   188,     9,  1357,    72,   307,  1252,  2205,
    1845,  1363,  1364,   455,   356,   288,   441,   233,   290,    25,
      26,   319,   404,   321,   240,  1314,   408,    93,   315,  1906,
     120,  1198,  2037,   356,   380,   101,     2,  1911,   315,  1913,
       6,   455,   314,   166,   319,   261,   455,   170,  1925,   311,
     315,   326,   378,   278,  2096,   280,  2098,   135,   173,   451,
     452,    67,   454,    69,  1353,  1942,   338,   167,   227,   228,
     286,   343,   319,  1709,   321,   310,   292,    82,  1714,   455,
     195,   353,   438,   444,   440,   357,   227,   228,   188,   364,
     356,   378,   352,    98,   265,   455,   267,   103,   104,   105,
     153,   378,   155,   319,  2196,  2197,  1983,  1984,   223,   158,
     326,   356,   178,   378,   214,  1992,   381,   382,   169,   219,
     407,   236,   448,   339,   233,   234,  1809,   176,   243,  1964,
    2211,   231,   392,   270,   271,     6,   239,   195,     9,   427,
     428,   429,   430,   415,   150,   194,   152,   363,   364,   442,
     366,  1503,   341,   425,   160,   442,   455,   319,   374,   321,
     455,   448,  1207,   455,  2117,   223,  1211,   359,   384,   270,
     271,   448,   233,   234,  1219,  1220,   455,   455,   236,   184,
     216,   455,   442,   448,   405,   243,   455,   356,   235,   405,
     195,   442,   455,   242,   455,   455,  1363,  1364,   182,  2076,
     315,   283,   455,   280,  2078,   403,  2080,  2084,   156,   157,
     277,    82,   280,  1709,   427,   428,   429,   430,  1714,   319,
     320,   403,  2099,   427,   428,   429,   430,    98,   234,  2064,
     395,   236,   332,   238,   334,    24,   241,   185,   243,   455,
     455,   290,   248,   191,   356,   332,  2199,   372,   455,  2202,
    2127,   214,  2129,    11,   600,   107,   442,   315,  1645,  2136,
     384,   367,    56,   378,   442,   236,    85,    25,    26,   274,
     449,   450,   451,   452,   222,   454,   175,  1612,   226,  2156,
    2157,   455,  1634,   356,   372,   156,  1753,  1754,  1755,  1756,
     283,   351,   407,  1582,   343,  1647,   411,   109,  2251,    57,
     449,   450,   451,   452,   353,   454,  1641,   236,   363,   211,
     315,   249,   461,   184,    68,   455,   280,   410,   280,  1671,
     378,   455,   356,   249,   195,    23,   277,   442,  2281,  1618,
     236,   442,   301,   448,   455,   435,  1625,   342,    96,    97,
     280,    99,   455,  2220,    84,  1680,    84,   442,  1683,   407,
     108,   436,   400,   411,   225,   455,   442,   384,   317,  1711,
     403,   232,   234,   435,   200,   236,   415,   238,   403,   319,
     241,   442,   243,   378,   364,   449,   450,   451,   452,   218,
     454,    55,   387,   388,   442,   441,   384,   461,    27,   188,
     448,   372,   455,   151,   152,   401,   356,   442,   455,   364,
     225,   438,   407,   274,   162,   346,   214,   109,   413,   236,
     449,   450,   451,   452,   405,   454,   306,   885,   457,    37,
     381,   889,  1757,   287,   281,   448,   431,   895,  1763,   403,
     458,    27,   900,   901,   439,   362,   442,   442,   906,   907,
     908,   462,   174,   448,   315,   913,   400,   400,  2174,     9,
     442,   919,   455,   455,   449,   450,   451,   452,   405,   454,
     928,   929,   457,   931,   236,  2033,   136,  1634,    17,   107,
    1805,   342,   172,   449,   450,   451,   452,  1864,   454,   237,
    1647,   457,   449,   450,   451,   452,   441,   454,   402,   188,
     457,   449,   450,   451,   452,   236,   454,    30,   127,   457,
     128,   458,   458,    56,  1671,   403,   458,   378,   456,   458,
     978,   459,   460,   458,   458,   458,   387,   388,   205,    12,
      13,    14,   458,   458,   458,  1814,   458,    20,   458,   434,
     130,  1820,   348,   319,   132,   100,   407,   399,   133,  1874,
    1008,    49,   413,  1878,  1711,   449,   450,   451,   452,   403,
     454,   137,   402,   457,   138,   400,   402,   397,   180,   436,
     431,   143,   248,    49,   180,    58,   146,   112,   439,   333,
    1814,   442,   442,   380,   122,   227,  1044,   448,   114,   114,
     365,   441,   403,   442,   187,   456,   341,   309,   236,   163,
     134,   438,   171,   403,   352,   366,   210,   403,    49,   218,
     210,   280,   180,  1071,   193,   454,   447,   218,   446,   270,
     233,  1079,  1080,   457,   457,   457,   341,   398,  1970,   403,
     457,   373,   131,   266,   269,   265,   187,   400,   331,  1097,
      49,   138,     8,   403,   392,   438,   126,  1972,   180,     9,
      82,   438,   403,   307,   173,  1113,   308,   202,   404,   329,
     344,   268,   125,  1121,   110,   435,    98,  1125,   435,    49,
     175,   285,   141,   156,   157,   142,  1134,   240,   262,  1137,
    2057,   263,   144,   300,   289,  2063,   284,   113,   394,     7,
     220,   116,    67,   442,   442,   156,   156,   328,   129,   136,
    2025,   328,   185,   102,    49,   218,  1164,   240,   191,  1167,
     145,  2036,    95,  2038,   267,   148,   264,    91,  1176,  1177,
    1178,  2063,     6,   390,   203,     9,   221,   192,  1186,   438,
      49,   214,   406,   344,   307,   147,   240,   240,   174,   222,
     180,   293,   307,   226,  1202,   423,   140,    49,     6,  1207,
      22,  1209,   184,  1211,    54,   354,   132,  1215,  1216,  1217,
    1218,  1219,  1220,  1221,  1222,    49,   193,   178,   188,   211,
    1326,   514,   597,   256,   257,  1146,   444,   136,  1355,   262,
    1147,  1364,  1860,   954,   227,   109,   610,  2164,  1813,   272,
     393,   113,   239,    49,  2119,   160,   279,   318,    82,   880,
    1170,   284,    86,   584,   236,  1656,   238,  2184,  2185,   241,
    1577,  1868,  1868,  1970,    98,   855,  1868,  1868,  2061,   302,
    1654,  1868,  1652,  1281,  1868,  1868,   868,   565,   968,  2052,
    2185,   124,  1736,  1291,   496,  1817,  1054,  2179,  1695,  1415,
    2165,  2166,   274,  1059,  2169,  1734,  1304,  2189,  1414,  1770,
    1271,  2176,  1268,  1311,  1545,  2232,  2233,  1780,  1278,  2236,
    1318,  2238,  1954,  1801,  1570,  1284,  2208,  1325,  1097,  1818,
    1594,  1353,  1319,  1834,  1119,  1121,   920,  1131,  2203,  1337,
    1625,  2007,  1625,  1673,  2012,  1343,  1825,  2120,  2018,  1109,
    1519,  1349,  1235,   979,  1607,  2272,   246,  1231,   889,   329,
     184,  1289,   860,   619,  1062,  2191,    57,   986,  1366,  2234,
     342,   195,   826,  1458,    -1,    -1,    -1,    -1,  2260,   993,
      -1,  2155,  1380,  1381,  1382,  1383,  1384,  1385,  1386,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2278,  2262,    -1,    -1,
      -1,   225,    -1,    -1,  1402,    -1,    -1,    -1,   232,    -1,
      -1,    -1,   236,    -1,   238,   387,   388,   241,    -1,   243,
      -1,    -1,    -1,    -1,  1422,   448,   449,   450,   451,   452,
    2204,   454,    -1,   456,   457,    -1,   459,   460,    -1,    -1,
      -1,   413,    -1,    -1,    -1,    -1,  1444,    -1,    -1,     1,
     274,    -1,    -1,    -1,     6,   279,    -1,     9,    -1,   431,
      12,    13,    14,    -1,    -1,    -1,    -1,   439,    -1,    -1,
     442,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2179,    -1,    -1,    -1,    -1,  2261,    -1,    -1,
      -1,   315,  2189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,   325,    -1,    -1,    -1,    -1,    58,    -1,  1506,  1507,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,   342,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,   372,    -1,
      -1,    -1,    -1,    -1,   378,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   387,   388,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1572,  1573,    -1,    -1,    -1,    -1,
    1578,  1579,    -1,   407,    -1,    -1,    -1,    -1,    -1,   413,
      -1,    -1,    -1,    -1,   418,    -1,    -1,    -1,  1596,    -1,
      -1,    -1,   184,    -1,   156,   157,  1604,   431,    -1,    -1,
      -1,   435,    -1,   195,    -1,   439,    -1,    -1,   442,    -1,
      -1,  1619,    -1,    -1,   448,    -1,  1624,  1625,    -1,    -1,
      -1,    -1,   184,   185,    -1,    -1,    -1,    -1,    -1,   191,
      -1,    -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   236,    -1,   238,    -1,    -1,   241,
      -1,   243,   214,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     222,    -1,    -1,   225,   226,    -1,    -1,    -1,    -1,    -1,
     232,    -1,    -1,    -1,   236,    -1,   238,    -1,    -1,   241,
      -1,   243,   274,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1699,  1700,  1701,   256,   257,    -1,    -1,    -1,    -1,
     262,  1709,    -1,    -1,    -1,    -1,  1714,    -1,    -1,    -1,
     272,    -1,   274,    -1,    -1,    -1,    -1,   279,    -1,  1727,
      -1,    -1,    -1,   315,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     342,    -1,    -1,   315,    -1,    -1,    -1,     6,    -1,    -1,
       9,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1786,    -1,
     342,    30,    -1,    -1,  1792,  1793,   378,    -1,    -1,    -1,
      -1,    -1,    -1,  1801,    -1,   387,   388,    -1,    -1,    -1,
      -1,    -1,  1810,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1818,    -1,    -1,    -1,    -1,   407,   378,    66,    -1,    -1,
      -1,   413,    -1,    -1,    -1,   387,   388,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,  1845,    -1,   431,
      -1,  1849,    -1,    -1,    -1,   407,    -1,   439,    -1,    98,
     442,   413,    -1,    -1,    -1,    -1,   448,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   431,
      -1,    -1,    -1,    -1,    -1,  1883,  1884,   439,    -1,    -1,
     442,     0,     1,    -1,    -1,    -1,   448,   449,   450,    -1,
      -1,    -1,   454,    -1,   456,    -1,   458,   459,   460,    -1,
      -1,    -1,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    34,    -1,    -1,    37,    38,
      39,    40,    41,    42,    43,    -1,  1934,    -1,    -1,  1937,
      -1,    -1,    -1,    -1,    -1,   184,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   195,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,  1964,    76,    77,    78,
      79,    80,    81,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   225,    -1,    -1,    -1,
      -1,    -1,    -1,   232,   233,    -1,    -1,   236,  1996,   238,
      -1,    -1,   241,    -1,   243,  2003,    -1,    -1,    -1,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2022,    -1,   135,    -1,    -1,    -1,
     139,    -1,    -1,    -1,    -1,   274,    -1,    -1,    -1,    -1,
      -1,  2039,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    -1,   301,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2071,   183,    -1,   315,    -1,    -1,   188,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   201,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   342,    -1,   214,    -1,    -1,   217,    98,
      -1,    -1,    -1,    -1,   223,    -1,    -1,    -1,   227,   228,
      -1,    -1,    -1,    -1,    -1,    -1,   235,   236,    -1,    -1,
     239,    -1,  2130,    -1,    -1,    -1,    -1,    -1,    -1,   378,
      -1,    -1,    -1,    -1,   253,    -1,    -1,  2145,   387,   388,
    2148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,
       1,    -1,     3,    -1,     5,    -1,    -1,   276,   407,    10,
      -1,    -1,    -1,    -1,   413,    -1,    -1,    18,    -1,    -1,
    2178,    -1,   291,    -1,    -1,    -1,    -1,    -1,   177,   298,
      -1,   300,   431,    -1,    -1,   184,     6,    -1,   307,     9,
     439,    -1,   311,   442,   313,    -1,    -1,    -1,    -1,   448,
      51,    52,    -1,  2211,   323,    -1,    -1,    -1,    59,    -1,
      -1,   330,    -1,   332,  2222,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    75,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   351,    -1,    -1,   354,    87,   236,    -1,   238,
      -1,    -1,   241,    -1,    -1,    -1,    -1,    -1,   367,    -1,
     369,   370,   371,    -1,    -1,    -1,    -1,  2265,    -1,    -1,
     111,    -1,    82,    -1,    -1,    -1,    -1,   118,   119,    -1,
      -1,    -1,    -1,    -1,   393,   274,    -1,    -1,    98,    -1,
      -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,   139,    -1,
      -1,    -1,   411,    -1,    -1,    -1,    -1,    -1,   149,    -1,
     419,   420,   421,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     161,    -1,    -1,   432,   165,    -1,   435,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   444,    -1,    -1,   179,    -1,
      -1,    -1,   183,    -1,    -1,   186,   455,    -1,   189,   190,
      -1,    -1,    -1,   342,    -1,    -1,   197,    -1,    -1,    -1,
      -1,    -1,    -1,   204,    -1,   206,    -1,    -1,   209,    -1,
      -1,    -1,    -1,    -1,   184,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,   195,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,   387,   388,
      -1,    -1,    -1,    -1,    -1,   246,    -1,    -1,   397,   250,
       1,   252,     3,    -1,     5,   225,    -1,    -1,   259,    10,
      -1,    -1,   232,    -1,   413,    -1,   236,    18,   238,    -1,
      -1,   241,    -1,   243,   423,    -1,    -1,    -1,    -1,    -1,
      -1,   282,   431,    -1,    -1,    -1,    -1,   436,    -1,    -1,
     439,    -1,   441,   442,    -1,    -1,   297,    -1,    -1,    -1,
      51,    52,    -1,    -1,   274,    -1,    -1,    -1,    59,    -1,
     311,    -1,    -1,    -1,    -1,   177,    -1,   318,    -1,    -1,
      71,    -1,   184,    -1,    75,    -1,   327,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,   340,
      -1,    -1,    -1,    -1,   345,   315,   347,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   355,    -1,    -1,    -1,    -1,    -1,
     111,    -1,    -1,    -1,   365,    -1,    -1,   118,   119,    -1,
      -1,    -1,   342,   374,   236,   126,   238,    -1,    -1,   241,
      -1,    -1,   383,    -1,   385,   386,    -1,    -1,   389,    -1,
     391,    -1,    -1,    -1,    -1,   396,    -1,    -1,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   378,    -1,
     161,   412,   274,    -1,   165,    -1,   417,   387,   388,    -1,
      -1,   422,    -1,    -1,    -1,    -1,    -1,    -1,   179,    -1,
      -1,    -1,   433,   403,    -1,   186,    -1,   407,   189,   190,
      -1,   442,    -1,   413,   445,    -1,   197,    -1,    -1,    -1,
      -1,    -1,    -1,   204,   455,   206,    -1,    -1,   209,    -1,
      -1,   431,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,
      -1,    -1,   442,    -1,    -1,    -1,    -1,    -1,   448,    -1,
     342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   246,    -1,    -1,    -1,   250,
      -1,   252,    -1,    -1,    -1,    -1,    -1,     6,   259,    -1,
       9,    -1,    -1,    12,    13,    14,    -1,    -1,    17,    -1,
      -1,    -1,    21,    -1,    -1,   387,   388,    -1,    -1,    -1,
      -1,   282,    -1,    -1,    -1,   397,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   297,    -1,    -1,    -1,
      -1,   413,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,
      -1,   423,    -1,    -1,    -1,    -1,    -1,   318,    -1,   431,
      -1,    -1,    -1,    -1,   436,    -1,   327,   439,    -1,   441,
     442,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,   340,
      -1,    -1,    -1,    -1,   345,    -1,   347,    -1,    -1,    98,
      -1,    -1,    -1,     6,   355,    -1,     9,    -1,    -1,    12,
      13,    14,    -1,    -1,   365,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   383,    -1,   385,   386,    -1,    -1,   389,    -1,
     391,    -1,    -1,    -1,    -1,   396,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    58,    -1,   156,   157,    -1,
      -1,   412,    -1,    -1,    -1,    -1,   417,    -1,    -1,    -1,
      -1,   422,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,   433,    -1,    -1,   184,   185,    -1,    -1,    -1,
      -1,   442,   191,    -1,   445,    98,   195,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   214,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   222,    -1,    -1,   225,   226,    -1,    -1,
      -1,    -1,    -1,   232,    -1,    -1,    -1,   236,    -1,   238,
      -1,    -1,   241,    -1,   243,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    -1,    -1,   256,   257,    -1,
      -1,    -1,    -1,   262,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   272,    -1,   274,    -1,    -1,    -1,    -1,
     279,   184,   185,    -1,    -1,    -1,    -1,    -1,   191,    -1,
      -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    37,    38,    39,    40,    41,
      42,   214,    -1,    -1,    -1,    -1,   315,    -1,    -1,   222,
      -1,    -1,   225,   226,    -1,    -1,    -1,    -1,    -1,   232,
      -1,    -1,    -1,   236,    -1,   238,    -1,    -1,   241,    -1,
     243,    -1,    74,   342,    76,    77,    78,    79,    80,    81,
      -1,    -1,    -1,   256,   257,    -1,    -1,    -1,    -1,   262,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   272,
      -1,   274,    -1,    -1,    -1,    -1,   279,    -1,    -1,   378,
      -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,   387,   388,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,
      -1,    -1,    -1,    -1,    -1,   404,    -1,    -1,   407,   408,
      -1,    -1,   315,    -1,   413,     3,    -1,     5,    -1,    -1,
      -1,    -1,    10,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      18,    -1,   431,    -1,    -1,    -1,    -1,    -1,    -1,   342,
     439,    -1,    -1,   442,    -1,    -1,    -1,    -1,    -1,   448,
     449,   450,    -1,    -1,    -1,   454,    -1,   456,    -1,   458,
     459,   460,    -1,    51,    52,    -1,    -1,    -1,    -1,   201,
      -1,    59,    -1,    -1,    -1,   378,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,   387,   388,    -1,    75,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,   407,    -1,    -1,    -1,    -1,    -1,
     413,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,   111,    -1,    -1,    -1,    -1,   431,    -1,
     118,   119,    -1,    -1,     6,    -1,   439,     9,    -1,   442,
      -1,    -1,    -1,    -1,    -1,   448,   449,   450,    -1,    -1,
      -1,   454,    -1,   456,    -1,   458,   459,   460,    -1,   291,
      -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,
      -1,    -1,    -1,   161,    -1,    -1,    -1,   165,    -1,    -1,
      -1,   313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   179,    -1,    -1,    -1,    -1,    -1,    -1,   186,    -1,
      -1,   189,   190,    -1,    -1,    -1,    -1,    -1,    -1,   197,
      82,    -1,    -1,    -1,    -1,    -1,   204,    -1,   206,    -1,
      -1,   209,    -1,    -1,    -1,    -1,    98,    -1,    -1,     3,
      -1,     5,    -1,    -1,    -1,    -1,    10,   369,   370,   371,
     112,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,     6,
      -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,   246,    -1,
      -1,    -1,   250,    -1,   252,    -1,    -1,    -1,    -1,    -1,
      -1,   259,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    -1,    -1,    -1,    59,    -1,   419,   420,   421,
      -1,    -1,    -1,    -1,   282,    -1,    -1,    71,    -1,    -1,
      -1,    75,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   297,
      -1,    -1,   184,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   195,    -1,    82,    -1,    -1,    -1,    -1,
     318,    -1,    -1,    -1,    -1,    -1,    -1,   111,   210,   327,
      -1,    98,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,   340,   225,    -1,    -1,    -1,   345,    -1,   347,
     232,    -1,    -1,    -1,   236,    -1,   238,   355,    -1,   241,
      -1,   243,    -1,    -1,    -1,   149,    -1,   365,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   374,   161,    -1,    -1,
      -1,   165,   149,    -1,    -1,   383,    -1,   385,   386,    -1,
      -1,   389,   274,   391,    -1,   179,    -1,    -1,   396,    -1,
      -1,    -1,   186,    -1,    -1,   189,   190,    -1,    -1,    -1,
      -1,    -1,    -1,   197,   412,    -1,    -1,   184,    -1,   417,
     204,    -1,   206,    -1,   422,   209,    -1,    -1,   195,    -1,
      -1,    -1,    -1,   315,    -1,   433,     6,    -1,    -1,     9,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   445,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   455,   225,    -1,
     342,    -1,   246,    -1,    -1,   232,   250,    -1,   252,   236,
       6,   238,    -1,     9,   241,   259,   243,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   378,    -1,   282,    -1,
      -1,    -1,    -1,    -1,    -1,   387,   388,   274,    -1,    -1,
      -1,    -1,    82,   297,    -1,    -1,    -1,     6,    -1,    -1,
       9,    -1,    -1,    -1,    94,   407,    -1,    -1,    98,    -1,
      -1,   413,    -1,    -1,   318,    -1,    -1,    -1,    -1,     6,
      -1,    -1,     9,   327,    -1,    -1,    82,    -1,   315,   431,
      -1,    -1,    -1,    -1,    -1,    -1,   340,   439,    94,    -1,
     442,   345,    98,   347,    -1,    -1,   448,    -1,    -1,    -1,
      -1,   355,    -1,    -1,    -1,   342,    -1,    -1,    -1,    -1,
      -1,   365,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     374,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,   383,
      -1,   385,   386,    -1,    -1,   389,    -1,   391,    -1,    98,
      -1,   378,   396,    -1,   184,    82,    -1,    -1,    -1,    -1,
     387,   388,    -1,   112,    -1,   195,    -1,    94,   412,    -1,
      -1,    98,    -1,   417,    -1,    -1,    -1,   404,   422,    -1,
     407,   408,    -1,    -1,    -1,    -1,   413,    -1,   184,   433,
      -1,    -1,    -1,    -1,    -1,   225,    -1,    -1,    -1,   195,
      -1,   445,   232,    -1,   431,    -1,   236,    -1,   238,    -1,
      -1,   241,   439,   243,    -1,   442,    -1,    -1,    -1,    -1,
      -1,   448,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   225,
      -1,    -1,    -1,    -1,    -1,   184,   232,    -1,    -1,    -1,
     236,    -1,   238,    -1,   274,   241,   195,   243,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,    -1,     6,
      -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,   195,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   225,    -1,   274,    -1,
      -1,    -1,    -1,   232,    -1,   315,    -1,   236,    -1,   238,
      -1,    -1,   241,    -1,   243,    -1,    -1,    -1,   225,    -1,
      -1,    -1,    -1,    -1,    -1,   232,    -1,    -1,    -1,   236,
      -1,   238,   342,    -1,   241,    -1,   243,    -1,    -1,   315,
      -1,    -1,    -1,    -1,    -1,   274,    -1,    -1,     6,    -1,
      -1,     9,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
       6,    -1,    -1,     9,    -1,    -1,   342,   274,   378,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,   387,   388,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   315,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,
      -1,    -1,   378,   413,    -1,    -1,    -1,    -1,   315,    -1,
      -1,   387,   388,   342,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   431,    -1,    -1,    82,    -1,    -1,    -1,    -1,   439,
      -1,   407,   442,    -1,    -1,   342,    82,   413,   448,    -1,
      98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   378,
      -1,    -1,    98,   180,    -1,   431,    -1,   184,   387,   388,
      -1,    -1,    -1,   439,    -1,    -1,   442,    -1,   195,    -1,
      -1,   378,   448,    -1,    -1,    -1,    -1,    -1,   407,    -1,
     387,   388,    -1,    -1,   413,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   225,    -1,
     407,    -1,   431,    -1,    -1,   232,   413,    -1,    -1,   236,
     439,   238,    -1,   442,   241,    -1,   243,    -1,    -1,   448,
      -1,    -1,    -1,    -1,   431,    -1,   184,     6,    -1,    -1,
       9,    -1,   439,    -1,    -1,   442,    -1,   195,   184,    -1,
      -1,   448,    -1,    -1,    -1,    -1,    -1,   274,    -1,   195,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   225,    -1,    -1,
      -1,    -1,    -1,    -1,   232,    -1,    -1,    -1,   236,   225,
     238,    -1,    -1,   241,    -1,   243,   232,    -1,   315,    -1,
     236,    -1,   238,    -1,    -1,   241,    -1,   243,    -1,    -1,
      -1,    -1,     6,    82,    -1,     9,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   342,   274,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   274,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   378,    -1,    -1,    -1,    -1,    -1,   315,    -1,    -1,
     387,   388,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   315,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
     407,    -1,    -1,    -1,   342,    -1,   413,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,   342,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   431,   184,    -1,    -1,    -1,    -1,
      -1,    -1,   439,    -1,   372,   442,   195,    -1,    -1,    -1,
     378,   448,    -1,    -1,    -1,    -1,    -1,   373,    -1,   387,
     388,    -1,   378,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   387,   388,    -1,    -1,    -1,   225,    -1,    -1,   407,
      -1,    -1,    -1,   232,    -1,   413,    -1,   236,    -1,   238,
      -1,   407,   241,    -1,   243,    -1,    -1,   413,    -1,    -1,
      -1,    -1,    -1,   431,    -1,    -1,    -1,    -1,    -1,    -1,
     184,   439,    -1,    -1,   442,   431,    -1,    82,    -1,    -1,
     448,   195,    -1,   439,    -1,   274,   442,    -1,    -1,    -1,
      -1,    -1,   448,    98,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   225,    -1,    -1,    -1,    -1,    -1,    -1,   232,    -1,
      -1,    -1,   236,    -1,   238,    -1,   315,   241,    -1,   243,
      -1,    -1,    -1,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   342,    -1,    -1,    -1,    -1,    -1,    -1,
     274,    -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    76,    77,    78,    79,    80,    81,    -1,   184,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   378,
     195,    -1,    -1,    -1,    -1,    -1,    82,    -1,   387,   388,
      -1,   315,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,   118,    -1,    -1,    -1,   407,    -1,
     225,    -1,    -1,    -1,   413,    -1,    -1,   232,   342,    -1,
      -1,   236,    -1,   238,    -1,    -1,   241,    -1,   243,    -1,
      -1,    -1,   431,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     439,    -1,    -1,   442,    -1,    -1,    -1,    -1,    -1,   448,
      -1,    -1,    82,    -1,   378,    -1,    -1,    -1,    -1,   274,
      -1,    -1,    -1,   387,   388,    -1,    -1,    -1,    98,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,
      -1,    -1,    -1,   407,    -1,    -1,    -1,   201,   184,   413,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   195,
     315,    -1,    -1,   217,    -1,    -1,    -1,   431,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   439,    -1,    -1,   442,   233,
     234,    -1,    -1,    -1,   448,    -1,    -1,   342,    -1,   225,
      -1,    -1,    -1,    -1,    -1,    -1,   232,    -1,    -1,   253,
     236,    -1,   238,    -1,   258,   241,    -1,   243,    -1,    -1,
      -1,    -1,    -1,    -1,   184,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   276,   378,    -1,   195,    -1,    -1,    -1,    -1,
      -1,    -1,   387,   388,    -1,    -1,    -1,   291,   274,    -1,
      -1,    -1,    -1,    -1,   298,    -1,   300,    -1,    -1,   303,
      -1,    -1,   407,    -1,    -1,   225,    -1,    -1,   413,   313,
      -1,    -1,   232,    -1,    -1,    -1,   236,    -1,   238,    -1,
      -1,   241,    -1,   243,    -1,    -1,   431,    -1,    -1,   315,
      -1,    -1,    -1,    -1,   439,    -1,    -1,   442,    -1,    -1,
      -1,    -1,    -1,   448,   449,   450,    -1,    -1,    -1,    -1,
      -1,    -1,   457,   458,   274,    -1,   342,    -1,    -1,    -1,
      -1,    -1,    -1,   367,    -1,   369,   370,   371,    -1,    -1,
      -1,    -1,   376,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   390,    -1,    -1,    -1,
      -1,    -1,   378,    -1,    -1,   315,    -1,    -1,    -1,    -1,
      -1,   387,   388,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     414,    -1,    -1,    -1,    -1,   419,   420,   421,    -1,    -1,
      -1,   407,   342,    -1,    -1,    -1,    -1,   413,   432,    -1,
      -1,   435,   436,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   431,    -1,    -1,    -1,    -1,
      -1,   455,    -1,   439,    -1,    -1,   442,    -1,   378,    -1,
      -1,    -1,   448,   449,   450,    -1,    -1,   387,   388,    -1,
      -1,   457,   458,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,
      -1,    -1,    -1,   413,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   431,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,
      -1,    -1,   442,    -1,    -1,    -1,    -1,    -1,   448,   449,
     450,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   458
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   464,   465,     0,   183,   311,   466,   467,   468,   474,
     481,   483,   455,   455,     0,   467,   150,   489,   489,   236,
     312,   484,   484,   120,   469,   475,    27,   485,   485,   455,
     102,   594,   594,   236,   455,   482,    83,   490,   120,   470,
     476,   214,   486,  1138,   356,   169,   208,   216,   544,   455,
     307,   726,   726,   455,    73,   322,   406,   487,   488,   455,
     455,   356,   455,   168,   354,   405,   595,   601,   120,   471,
     477,   481,   135,   480,   488,   310,  1150,   491,   545,   455,
     546,   356,   444,   626,   597,    54,   434,   729,   139,   471,
     478,   489,   484,   275,   337,   377,   379,   492,   493,   497,
     505,   510,   548,   169,   547,    22,   251,   350,   583,   584,
     585,   586,   588,   592,   593,   455,   356,   239,   679,   442,
     600,   602,  1068,   731,   730,   341,   737,   484,   472,   455,
     455,   455,   455,   455,   359,   549,   455,   216,   582,    60,
      72,    93,   101,   178,   405,  1134,   319,   374,   375,   587,
     455,   585,   455,   593,   596,   455,   356,   235,   681,   599,
     601,   628,   629,   630,   603,    49,   732,   733,   734,  1129,
     732,   442,   455,   455,   594,   245,   358,   442,   496,   498,
     499,   500,   501,   503,   504,  1150,   182,   506,   507,   508,
     441,   494,   495,   496,  1164,    11,    25,    26,    57,    96,
      97,    99,   108,   151,   152,   162,   237,   352,   392,   442,
     511,   512,   513,   514,   521,   530,   534,   537,   538,   539,
     540,   541,   542,   543,   283,  1115,   548,   455,   280,   280,
    1147,   403,   277,  1146,   403,   395,  1158,    24,  1125,   598,
     627,   455,   356,   332,   683,   602,   631,  1109,   604,   733,
     372,   418,   735,   325,   435,   727,   473,   372,  1138,   455,
     499,   455,   500,    63,   363,  1121,     9,   236,   509,   455,
     508,   455,   455,   495,   107,   442,  1080,  1138,  1138,  1080,
     384,   367,  1154,  1138,  1138,  1138,  1138,  1138,  1080,  1138,
      56,  1131,  1138,   455,   513,  1080,   583,   442,  1075,  1076,
    1095,  1075,  1076,   236,  1076,    85,  1132,   175,  1135,   599,
     628,   680,   455,   356,   351,   724,   628,   455,  1109,   227,
     228,   632,   635,   636,   642,     1,    47,    62,   102,   219,
     231,   319,   320,   332,   334,   435,   455,   605,   606,   608,
     612,   614,   616,   617,   623,   624,   625,  1138,  1138,   372,
     283,   736,   109,   738,   726,  1138,   236,  1098,   455,   363,
    1138,   211,   211,   236,   455,   249,   522,  1080,  1080,  1138,
    1138,  1138,  1076,    68,  1080,  1080,  1076,  1138,  1076,   531,
     532,  1080,    96,   516,  1080,   550,   200,   277,  1118,  1076,
     280,   410,   280,   589,   590,  1068,  1067,  1068,   628,   682,
     455,   356,   632,   171,   442,   638,   639,   442,   638,  1132,
    1138,   319,   321,  1119,  1119,  1138,  1132,  1138,   249,  1144,
    1138,    23,  1124,   277,   167,   188,    31,   110,  1098,  1138,
     442,   455,   728,   477,  1098,  1076,  1138,   195,   236,   243,
     315,   378,   407,   448,   535,   536,  1101,  1076,   236,  1076,
      23,   214,  1080,  1139,   278,   280,   518,   519,   520,   515,
     551,  1095,  1075,   280,  1075,   590,   301,   591,  1068,   628,
     684,   455,   633,    84,   634,  1098,   442,  1138,  1124,  1077,
    1095,   279,   380,   613,   236,  1076,  1079,  1098,   436,  1138,
     442,   723,   723,   170,   442,  1098,   739,   740,   139,   479,
      56,   443,   502,   124,   195,   236,   243,   255,   315,   378,
     381,   382,   448,   523,   524,   525,   528,   536,   400,   533,
    1098,   519,   384,  1157,   517,     1,     4,    19,    29,   202,
     233,   240,   261,   286,   292,   319,   326,   339,   364,   366,
     374,   405,   455,   552,   553,   558,   560,   565,   566,   567,
     568,   572,   573,   574,   575,   576,   577,   578,   579,   581,
    1121,  1075,  1098,   317,   685,   686,   687,   725,   643,   640,
    1138,   435,   672,   403,   611,  1095,   234,  1142,   403,  1131,
     200,  1137,   442,  1138,  1138,   740,     1,   442,   741,   742,
     743,   744,   745,   750,   484,   525,    17,   400,  1101,  1098,
    1138,   519,  1144,   319,   403,  1162,   364,  1144,   218,  1140,
    1138,    55,  1130,    37,   113,  1128,  1140,  1140,   260,  1098,
    1164,   384,  1138,   723,   687,     1,    21,    34,    37,    38,
      39,    40,    41,    42,    43,    74,    76,    77,    78,    79,
      80,    81,   118,   201,   217,   236,   253,   276,   291,   298,
     300,   313,   323,   330,   367,   369,   370,   371,   393,   419,
     420,   421,   432,   435,   637,   644,   645,   646,   648,   649,
     650,   651,   652,   654,   666,   667,   669,   670,   671,   677,
     678,  1138,  1155,    27,  1126,   188,  1139,  1098,    56,   321,
     607,   618,  1098,   372,  1156,   236,   615,  1095,   615,   126,
     455,   356,     3,     5,    10,    18,    51,    52,    59,    71,
      75,    87,   111,   118,   119,   149,   161,   165,   179,   186,
     189,   190,   197,   204,   206,   209,   246,   250,   252,   259,
     282,   297,   318,   327,   340,   345,   347,   355,   365,   374,
     383,   385,   386,   389,   391,   396,   412,   417,   422,   433,
     445,   455,   751,   752,   762,   767,   771,   774,   787,   790,
     795,   800,   801,   802,   805,   807,   814,   818,   820,   835,
     838,   840,   842,   845,   847,   853,   862,   864,   881,   883,
     886,   890,   896,   906,   913,   915,   918,   922,   923,   934,
     945,   955,   961,   965,   971,   975,   977,   979,   981,   984,
     995,   996,  1005,  1007,  1008,   455,   526,   528,  1080,  1138,
    1140,   123,   167,   555,  1138,  1138,   319,   326,   573,  1138,
    1138,   364,  1138,  1138,  1125,     9,   260,   318,   580,  1138,
     442,   688,   225,   368,   418,   368,   418,   368,   418,   368,
     418,   368,   418,   438,  1163,   346,  1152,  1098,  1094,  1095,
    1095,   214,   224,   346,   668,  1138,  1138,   167,   188,   223,
     411,     9,    50,   225,   641,  1099,  1100,  1101,   674,   675,
    1099,    30,   619,   620,   621,   622,  1127,  1164,  1131,   180,
     610,  1136,   109,   236,   746,   753,   763,   768,   772,   775,
     788,   791,   796,   803,   806,   808,   815,   819,   821,   836,
     839,   841,  1162,   846,     1,   848,   854,   863,   865,   882,
     884,   887,   891,   897,   907,   914,   916,   919,   924,   935,
     946,   956,   236,   349,   966,   972,   306,   976,   978,   980,
     982,   985,   188,   997,  1135,  1009,   195,   236,   243,   315,
     378,   448,   527,   529,   123,   316,   364,   559,  1138,   117,
     305,   554,    32,   164,   244,   569,  1076,  1079,   381,  1076,
    1076,   287,  1149,  1149,   281,  1076,    61,    88,    89,   293,
     455,   689,   690,   694,  1138,   448,   403,   655,   458,  1096,
    1097,   400,   651,  1099,    27,   647,   362,  1117,  1117,  1101,
    1146,  1146,   462,   673,   675,   400,    48,   409,   174,   611,
    1098,   455,   455,   754,  1093,  1094,     6,    82,    94,    98,
     184,   225,   232,   238,   241,   274,   342,   387,   388,   413,
     431,   439,   764,  1062,  1083,  1084,  1093,  1099,  1102,   442,
     769,  1049,  1050,  1051,   236,  1072,  1073,  1074,  1095,   236,
    1091,  1093,  1102,     9,   789,   792,   797,  1063,  1064,  1084,
    1068,   405,   236,   809,  1083,  1090,  1093,   816,  1084,   236,
     404,   408,   822,   823,  1049,   296,   297,   310,   356,   837,
       6,  1081,  1082,  1093,  1093,   843,   136,  1048,  1049,  1081,
     693,  1093,   866,  1093,  1099,  1102,   947,  1095,    94,   885,
    1084,   888,  1084,   892,   177,   236,   898,   901,   902,   903,
    1072,  1091,  1095,  1164,  1068,  1065,  1095,  1068,  1065,     9,
     925,  1066,  1095,   150,   248,   936,   937,   938,   939,   941,
     942,   943,   944,  1069,  1070,  1081,   947,  1068,   963,   962,
     112,   967,   968,  1084,    94,   973,  1083,   693,  1093,  1068,
    1093,     8,    35,   999,   107,  1065,    17,  1076,   118,   236,
     556,  1143,   441,   570,   570,   122,   376,   441,   456,   564,
    1111,  1120,  1076,  1078,  1138,   172,   691,   692,   691,  1139,
     702,   188,  1098,   402,  1161,   225,   449,   450,   458,  1059,
    1061,  1062,  1085,  1093,  1100,  1102,   458,  1097,  1095,   236,
    1130,  1094,  1094,  1101,  1163,  1099,  1079,  1079,  1127,  1131,
     127,   761,    30,   180,   755,  1127,  1146,   458,  1093,   458,
    1103,   458,  1104,  1146,  1118,   458,   458,   458,   458,   458,
     458,   458,   458,  1103,   128,   766,   403,   765,  1084,   205,
    1112,    56,  1052,  1053,   403,  1118,   434,   776,   236,  1090,
    1093,  1068,   130,   798,   156,   456,   799,  1064,   348,  1116,
     319,  1151,  1067,   132,   813,   755,   427,   428,   429,   430,
     133,   817,    49,   210,   776,    17,   438,   824,   825,   826,
     830,  1123,   100,  1146,  1082,  1071,   399,  1160,   855,  1164,
    1093,    92,   331,   394,   867,   868,   869,   873,   878,   949,
    1084,   403,   137,   889,    49,   166,   207,   215,   288,   893,
     902,   138,   899,   423,   436,   400,   402,   397,   258,   304,
    1113,   180,  1010,  1151,  1010,  1066,   143,   933,   436,   927,
    1088,  1093,  1100,   942,   944,  1081,   403,  1070,   121,   403,
     424,   940,   957,   187,   341,   964,  1129,   210,   968,  1093,
     146,   974,   180,   180,   319,   321,   983,   112,   986,   333,
     380,  1000,  1147,  1010,   529,   564,  1111,   557,  1095,   240,
     347,  1138,   122,   561,   562,  1090,   693,   700,  1098,   635,
     703,   114,   656,  1146,  1061,  1061,  1061,    70,   360,   457,
    1060,   449,   450,   451,   452,   454,   461,  1061,   365,  1153,
    1142,  1079,   114,   609,  1088,    25,    26,    67,    69,   103,
     104,   105,   150,   152,   160,   234,   401,   442,  1070,   441,
     758,    66,   233,   301,   756,   757,   149,   310,  1086,  1094,
    1059,  1061,   403,  1061,  1059,  1105,  1094,  1100,  1102,   442,
    1061,  1108,  1061,  1061,  1107,  1061,  1059,  1059,  1061,  1106,
    1061,  1063,  1084,   187,   341,   770,  1112,    12,    13,    14,
      20,    58,   156,   157,   185,   191,   214,   222,   226,   256,
     257,   262,   272,   279,   284,   302,   448,   449,   450,   451,
     452,   454,   456,   457,   459,   460,  1054,  1055,  1056,  1057,
    1058,    12,    13,    14,    58,   214,   256,   257,   262,   272,
     279,   302,   449,   450,   454,   458,  1054,  1055,  1056,  1057,
    1058,  1084,   309,   773,  1074,   777,   187,   341,   781,   324,
     416,   793,   794,  1164,  1049,   213,   266,  1041,  1042,  1043,
    1045,   426,   441,   810,  1164,   163,  1016,  1017,  1016,  1016,
    1016,  1084,  1063,  1084,    21,   404,   408,   831,   832,  1050,
     134,   834,   440,   826,   828,   438,   827,   823,  1094,   114,
     844,  1072,   849,     9,    12,    15,    16,   253,   254,   272,
     273,   856,   860,   171,  1088,     9,    56,   173,   223,   411,
     874,   875,   876,   870,   868,   951,  1120,  1147,   403,  1081,
    1063,  1084,   366,   894,   747,   748,  1048,   904,   905,  1093,
    1072,     8,    35,  1012,  1151,  1090,   210,   908,   920,  1164,
     928,  1127,  1093,   928,   403,   403,   520,   149,   404,   408,
    1084,    49,   218,   958,  1084,  1084,   372,  1084,  1093,   180,
    1063,  1084,  1088,  1129,   210,   989,  1093,   159,   163,  1001,
       9,  1006,  1072,   920,   561,  1095,   280,   563,  1076,  1111,
     563,   193,   695,   233,   234,   701,   638,  1147,    28,   115,
     202,   653,   658,   659,   660,   661,   663,  1094,  1100,  1102,
     457,  1061,  1061,  1061,  1061,  1061,  1061,   457,  1061,  1162,
    1142,  1147,  1015,  1017,   447,   446,  1088,  1015,   218,    31,
      33,    36,    46,   176,   181,   194,   242,   290,   314,   338,
     343,   353,   357,   415,   425,   759,   760,  1015,   270,  1145,
    1145,  1145,   757,   756,   236,  1087,  1094,   457,  1093,   461,
     457,  1060,   457,   457,  1060,   457,   457,   457,   457,  1060,
     457,   457,   373,  1021,  1022,  1063,  1082,   341,  1162,   398,
    1159,  1159,   403,  1072,   778,   779,   780,  1129,  1093,  1093,
     163,   289,   782,  1002,  1135,   240,   260,  1021,  1044,  1046,
     131,   804,  1045,    96,   305,   442,  1070,    33,    36,    44,
      45,    46,   158,   176,   194,   242,   290,   343,   353,   415,
     811,   812,  1016,   269,  1018,   265,  1019,   187,  1021,   187,
    1123,   400,   833,   829,   831,   747,  1147,   747,  1162,   331,
     857,  1162,   403,    49,   875,   877,  1088,     9,    56,   223,
     411,   871,   872,  1088,   952,  1121,   200,   285,  1148,   661,
    1081,  1021,   187,  1164,  1067,   138,   900,   749,     8,   180,
     908,  1093,   126,   263,  1031,  1032,  1034,  1041,   240,   260,
     438,   126,   438,   930,   931,  1088,  1087,  1084,  1138,  1041,
     969,  1164,  1093,  1021,   187,   403,     9,   987,   988,  1110,
     990,  1093,   969,   990,   307,  1004,   308,  1011,  1012,  1111,
     251,   319,   321,   571,  1138,   173,   696,  1098,   704,  1076,
    1129,   662,   663,   659,  1140,   657,   658,   457,   404,   676,
    1076,  1019,  1015,  1138,  1138,   121,   424,   760,  1090,  1090,
    1090,  1103,  1116,   457,  1061,  1076,  1103,  1103,  1061,  1103,
    1103,  1103,   223,   411,  1103,  1103,  1023,   268,  1024,  1021,
    1082,   156,   284,   156,   284,   779,   279,   735,    86,   325,
     435,   265,   267,   784,  1003,   783,   329,   344,   747,   747,
     810,   810,   810,   810,  1138,   153,   155,  1138,   121,   424,
     812,   747,  1020,  1063,  1064,  1063,  1064,   832,  1049,   747,
    1093,   125,   850,   435,   858,   859,   860,   110,   861,   435,
    1089,  1093,  1099,  1088,    49,   879,   872,   175,   879,   948,
    1138,   285,  1140,  1063,   580,   895,  1164,   750,   905,  1084,
     199,   909,  1164,  1033,  1035,   141,   917,  1034,   142,   921,
     240,  1049,   929,  1048,   930,   262,   959,  1114,   144,   960,
     289,  1026,  1027,   300,  1116,  1063,  1089,   284,  1088,   113,
     991,   394,   993,  1147,   154,   264,  1013,  1036,  1037,  1039,
    1042,     7,  1122,   571,  1098,   116,   220,   697,    67,    66,
      67,   192,   233,   234,   258,   303,   376,   390,   414,   436,
     455,   649,   650,   652,   654,   666,   669,   671,   705,   706,
     708,   709,   710,   711,   713,   714,   715,   716,   720,   721,
     442,   664,   665,  1138,  1138,   448,  1092,  1093,  1098,  1092,
    1047,  1129,  1047,  1021,   457,   747,  1025,  1162,   156,  1162,
     156,  1084,   129,   786,   785,   747,  1016,  1016,  1016,  1016,
    1092,  1092,  1047,  1047,   747,  1021,   328,  1021,   328,   851,
     136,   852,   859,   102,  1133,   879,   879,  1089,  1012,   207,
     434,   953,  1076,  1138,  1021,   240,   260,    49,   240,   218,
     910,   198,   240,   260,   437,   747,   747,   926,   747,   932,
     693,  1054,  1055,  1056,  1057,  1058,  1028,   145,   970,   267,
    1029,  1093,  1021,  1021,   988,  1137,    95,   992,  1137,  1026,
     166,   207,   215,   288,   998,  1067,  1038,  1040,   148,  1014,
    1039,   293,  1070,  1092,  1138,    91,   221,   698,   271,  1145,
     203,   722,   270,   271,   719,  1124,   192,   438,  1138,  1146,
    1138,  1093,   711,   258,   299,   717,   718,  1098,   665,  1075,
    1099,  1092,   747,  1162,  1162,   747,  1064,  1064,   747,    49,
     879,   406,   880,   307,  1067,   187,   288,   954,   950,   344,
    1084,  1138,   911,  1031,  1042,   240,   240,   747,   747,   747,
    1030,  1093,  1137,  1093,   147,   994,   747,   747,   233,   234,
    1141,  1098,  1138,  1138,   174,   699,  1138,  1139,  1138,  1048,
    1093,   712,  1076,    90,    91,   116,   294,   295,   335,   336,
     707,   180,   293,  1098,   718,  1141,  1021,  1021,  1084,  1084,
    1138,  1067,   307,  1095,   423,   693,   140,   912,   747,  1093,
    1098,  1098,  1138,  1098,  1098,  1116,  1084,   901,  1138,  1048,
    1098,    49,   901,  1084
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   463,   465,   464,   466,   466,   467,   467,   469,   470,
     468,   472,   473,   471,   475,   476,   474,   477,   477,   477,
     478,   478,   479,   480,   482,   481,   483,   484,   484,   485,
     485,   486,   486,   487,   487,   487,   488,   488,   489,   489,
     490,   490,   491,   491,   492,   492,   492,   492,   493,   494,
     494,   494,   494,   495,   496,   497,   498,   498,   498,   498,
     499,   499,   500,   500,   500,   501,   502,   502,   503,   504,
     505,   506,   506,   507,   507,   508,   508,   509,   509,   510,
     511,   511,   511,   512,   512,   513,   513,   513,   513,   513,
     513,   513,   513,   513,   513,   513,   514,   515,   514,   516,
     514,   514,   514,   514,   514,   517,   517,   518,   518,   519,
     520,   520,   522,   521,   523,   523,   523,   523,   523,   524,
     524,   525,   525,   526,   525,   527,   527,   528,   528,   528,
     528,   528,   528,   529,   529,   529,   529,   529,   529,   530,
     531,   532,   532,   533,   533,   534,   535,   535,   536,   536,
     537,   538,   539,   540,   541,   542,   543,   544,   544,   545,
     544,   546,   544,   547,   547,   548,   548,   550,   549,   551,
     551,   552,   552,   552,   552,   552,   552,   552,   552,   552,
     552,   552,   552,   552,   552,   552,   553,   553,   553,   554,
     554,   554,   555,   555,   555,   556,   556,   556,   557,   557,
     558,   559,   559,   559,   560,   560,   562,   561,   561,   563,
     564,   564,   564,   565,   566,   567,   567,   567,   568,   569,
     569,   569,   570,   570,   570,   570,   571,   571,   572,   572,
     573,   573,   573,   573,   573,   574,   575,   576,   576,   577,
     578,   578,   579,   580,   580,   580,   581,   582,   582,   583,
     583,   583,   583,   584,   584,   585,   585,   586,   587,   587,
     587,   587,   588,   589,   589,   590,   591,   591,   592,   592,
     592,   593,   593,   593,   593,   593,   594,   594,   595,   596,
     595,   597,   595,   598,   598,   599,   600,   600,   601,   601,
     603,   602,   604,   604,   605,   605,   605,   605,   605,   605,
     605,   605,   605,   605,   605,   605,   606,   607,   607,   607,
     608,   608,   608,   609,   609,   610,   610,   611,   611,   612,
     613,   613,   614,   614,   615,   615,   616,   617,   618,   618,
     619,   619,   619,   620,   621,   622,   623,   624,   625,   625,
     626,   627,   626,   628,   628,   630,   629,   631,   631,   631,
     632,   633,   632,   634,   632,   635,   636,   637,   637,   638,
     638,   638,   639,   640,   640,   641,   641,   641,   642,   643,
     643,   644,   644,   644,   644,   644,   644,   644,   644,   644,
     644,   644,   644,   644,   644,   644,   645,   646,   647,   647,
     648,   649,   650,   650,   651,   651,   651,   651,   651,   651,
     651,   651,   651,   651,   651,   651,   651,   651,   651,   651,
     651,   651,   651,   651,   651,   651,   651,   651,   651,   651,
     651,   651,   651,   651,   651,   651,   651,   651,   651,   652,
     652,   653,   653,   653,   654,   655,   655,   656,   656,   657,
     657,   658,   659,   660,   660,   661,   661,   662,   662,   663,
     664,   664,   665,   666,   667,   668,   668,   668,   669,   670,
     671,   673,   672,   674,   674,   675,   675,   676,   676,   677,
     677,   678,   679,   680,   679,   681,   682,   681,   683,   684,
     683,   685,   685,   686,   686,   687,   688,   688,   689,   689,
     689,   689,   690,   690,   691,   692,   692,   693,   693,   694,
     695,   695,   696,   696,   697,   697,   698,   698,   699,   699,
     700,   700,   700,   701,   701,   702,   702,   703,   704,   704,
     705,   705,   705,   705,   705,   705,   705,   705,   705,   705,
     705,   705,   705,   705,   705,   705,   706,   707,   707,   707,
     707,   707,   707,   707,   708,   709,   709,   709,   710,   710,
     711,   712,   713,   714,   715,   716,   716,   717,   717,   718,
     718,   718,   719,   719,   719,   720,   721,   722,   722,   723,
     724,   725,   724,   726,   727,   728,   726,   729,   730,   729,
     731,   729,   732,   732,   733,   734,   734,   734,   735,   735,
     735,   735,   735,   736,   736,   737,   737,   738,   739,   738,
     740,   740,   741,   741,   741,   741,   741,   742,   743,   744,
     745,   746,   746,   748,   749,   747,   750,   750,   751,   751,
     751,   751,   751,   751,   751,   751,   751,   751,   751,   751,
     751,   751,   751,   751,   751,   751,   751,   751,   751,   751,
     751,   751,   751,   751,   751,   751,   751,   751,   751,   751,
     751,   751,   751,   751,   751,   751,   751,   751,   751,   751,
     751,   751,   751,   751,   751,   751,   751,   751,   753,   752,
     754,   754,   754,   754,   754,   754,   754,   754,   754,   754,
     754,   754,   754,   754,   754,   754,   754,   755,   755,   755,
     755,   755,   755,   756,   757,   757,   758,   758,   759,   759,
     760,   760,   760,   760,   760,   760,   760,   760,   760,   760,
     760,   760,   760,   760,   760,   760,   760,   761,   761,   763,
     762,   764,   764,   764,   765,   765,   766,   766,   768,   767,
     769,   769,   770,   770,   771,   772,   772,   773,   773,   775,
     774,   776,   777,   776,   778,   778,   779,   779,   780,   780,
     780,   780,   781,   781,   781,   782,   783,   782,   784,   785,
     784,   786,   786,   788,   787,   789,   789,   789,   791,   790,
     792,   792,   793,   793,   793,   793,   793,   794,   794,   796,
     795,   797,   798,   798,   799,   799,   800,   801,   803,   802,
     804,   804,   806,   805,   808,   807,   809,   809,   809,   809,
     809,   809,   809,   809,   809,   810,   810,   810,   811,   811,
     812,   812,   812,   812,   812,   812,   812,   812,   812,   812,
     812,   812,   812,   812,   812,   813,   813,   815,   814,   816,
     816,   816,   816,   816,   817,   817,   819,   818,   821,   820,
     822,   822,   823,   823,   823,   824,   825,   825,   827,   826,
     828,   829,   828,   830,   830,   831,   831,   832,   832,   832,
     832,   833,   833,   834,   834,   836,   835,   837,   837,   837,
     837,   837,   837,   839,   838,   841,   840,   843,   842,   844,
     844,   846,   845,   848,   849,   847,   847,   850,   851,   850,
     852,   852,   854,   853,   855,   855,   856,   856,   856,   857,
     857,   858,   858,   859,   860,   860,   860,   860,   860,   860,
     860,   861,   861,   863,   862,   865,   864,   866,   866,   866,
     867,   867,   868,   868,   868,   870,   869,   871,   871,   872,
     872,   872,   872,   872,   872,   873,   874,   874,   875,   875,
     876,   876,   876,   876,   876,   877,   878,   879,   879,   880,
     880,   882,   881,   884,   883,   885,   885,   887,   886,   888,
     888,   889,   889,   891,   890,   892,   892,   893,   893,   893,
     893,   894,   894,   895,   895,   895,   897,   896,   898,   899,
     898,   898,   900,   900,   901,   901,   902,   902,   902,   902,
     902,   903,   903,   904,   904,   905,   907,   906,   908,   908,
     909,   909,   909,   909,   909,   909,   910,   910,   911,   911,
     911,   912,   912,   914,   913,   916,   915,   917,   917,   919,
     918,   920,   920,   920,   921,   921,   922,   924,   923,   925,
     926,   925,   927,   927,   928,   929,   928,   930,   930,   932,
     931,   933,   933,   935,   934,   936,   936,   936,   936,   936,
     937,   938,   938,   939,   940,   940,   941,   941,   942,   943,
     943,   944,   944,   946,   945,   948,   947,   949,   949,   950,
     950,   951,   951,   952,   952,   953,   953,   953,   954,   954,
     954,   956,   957,   955,   958,   958,   959,   959,   959,   959,
     959,   960,   960,   962,   961,   963,   961,   964,   964,   964,
     966,   965,   967,   967,   968,   968,   968,   969,   969,   970,
     970,   972,   971,   973,   973,   973,   974,   974,   975,   976,
     976,   978,   977,   980,   979,   982,   981,   983,   983,   983,
     985,   984,   986,   986,   987,   987,   988,   989,   989,   990,
     991,   991,   992,   992,   993,   993,   994,   994,   995,   995,
     995,   996,   997,   997,   998,   998,   998,   998,   998,   999,
     999,  1000,  1000,  1001,  1001,  1002,  1002,  1003,  1003,  1004,
    1004,  1005,  1006,  1006,  1007,  1009,  1008,  1010,  1010,  1011,
    1011,  1011,  1011,  1012,  1012,  1013,  1013,  1013,  1014,  1014,
    1015,  1016,  1017,  1018,  1017,  1019,  1020,  1019,  1021,  1022,
    1023,  1022,  1024,  1025,  1024,  1026,  1027,  1028,  1027,  1029,
    1030,  1029,  1031,  1031,  1031,  1033,  1032,  1035,  1034,  1036,
    1036,  1036,  1038,  1037,  1040,  1039,  1041,  1041,  1042,  1042,
    1042,  1044,  1043,  1046,  1045,  1047,  1047,  1048,  1049,  1051,
    1050,  1052,  1052,  1052,  1052,  1052,  1052,  1052,  1052,  1052,
    1052,  1052,  1052,  1052,  1052,  1052,  1052,  1052,  1052,  1052,
    1053,  1053,  1053,  1053,  1053,  1053,  1053,  1053,  1053,  1053,
    1053,  1053,  1053,  1053,  1053,  1053,  1053,  1053,  1053,  1053,
    1053,  1053,  1053,  1053,  1053,  1053,  1053,  1053,  1054,  1054,
    1054,  1055,  1055,  1056,  1056,  1057,  1057,  1057,  1058,  1058,
    1058,  1059,  1059,  1060,  1060,  1060,  1061,  1061,  1061,  1061,
    1061,  1061,  1061,  1061,  1061,  1062,  1062,  1063,  1063,  1064,
    1065,  1066,  1067,  1067,  1068,  1069,  1069,  1070,  1071,  1071,
    1072,  1073,  1073,  1073,  1074,  1075,  1075,  1076,  1077,  1077,
    1078,  1078,  1079,  1079,  1080,  1081,  1081,  1082,  1082,  1083,
    1083,  1084,  1084,  1084,  1084,  1084,  1084,  1084,  1084,  1084,
    1085,  1085,  1085,  1085,  1085,  1085,  1085,  1086,  1086,  1087,
    1087,  1088,  1088,  1089,  1089,  1090,  1090,  1091,  1091,  1091,
    1092,  1092,  1092,  1093,  1094,  1094,  1094,  1094,  1095,  1095,
    1096,  1097,  1097,  1098,  1099,  1099,  1100,  1100,  1101,  1101,
    1101,  1101,  1101,  1101,  1101,  1102,  1102,  1102,  1102,  1102,
    1102,  1102,  1102,  1102,  1102,  1102,  1102,  1103,  1103,  1103,
    1104,  1104,  1105,  1105,  1106,  1106,  1106,  1107,  1107,  1108,
    1108,  1109,  1110,  1110,  1111,  1111,  1112,  1112,  1113,  1113,
    1113,  1114,  1114,  1115,  1115,  1116,  1116,  1117,  1117,  1118,
    1118,  1119,  1119,  1120,  1120,  1121,  1121,  1122,  1122,  1123,
    1123,  1124,  1124,  1125,  1125,  1126,  1126,  1127,  1127,  1128,
    1128,  1129,  1129,  1130,  1130,  1131,  1131,  1132,  1132,  1133,
    1133,  1134,  1134,  1135,  1135,  1136,  1136,  1137,  1137,  1138,
    1138,  1139,  1139,  1139,  1140,  1140,  1141,  1141,  1141,  1142,
    1142,  1143,  1143,  1144,  1144,  1145,  1145,  1146,  1146,  1147,
    1147,  1148,  1148,  1148,  1149,  1149,  1150,  1150,  1151,  1151,
    1152,  1152,  1153,  1153,  1154,  1154,  1155,  1155,  1155,  1156,
    1156,  1157,  1157,  1158,  1158,  1159,  1159,  1160,  1160,  1161,
    1161,  1162,  1162,  1163,  1163,  1164,  1164
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     3,     1,     2,     1,     1,     0,     0,
       8,     0,     0,     8,     0,     0,     7,     0,     1,     2,
       0,     3,     3,     3,     0,     7,     5,     1,     1,     0,
       2,     0,     3,     1,     2,     1,     1,     1,     0,     5,
       0,     4,     0,     2,     1,     1,     1,     1,     3,     0,
       2,     3,     2,     3,     1,     3,     0,     2,     3,     2,
       1,     2,     1,     1,     1,     5,     1,     1,     4,     3,
       3,     0,     2,     1,     2,     3,     3,     1,     2,     3,
       0,     1,     2,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     0,     5,     0,
       4,     3,     3,     3,     3,     0,     2,     1,     2,     4,
       1,     1,     0,     5,     1,     1,     1,     1,     1,     1,
       2,     1,     3,     0,     4,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       3,     1,     2,     1,     2,     4,     1,     2,     1,     3,
       4,     4,     3,     3,     4,     3,     3,     0,     5,     0,
       4,     0,     4,     0,     3,     0,     2,     0,     6,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     4,     4,     0,
       1,     1,     0,     1,     1,     1,     1,     2,     1,     2,
       4,     1,     1,     1,     6,     8,     0,     2,     2,     1,
       0,     2,     1,     3,     5,     0,     1,     1,     4,     2,
       2,     1,     0,     4,     5,     2,     1,     1,     3,     1,
       1,     3,     1,     1,     2,     4,     4,     5,     7,     4,
       3,     2,     3,     2,     2,     2,     4,     0,     3,     0,
       2,     1,     1,     1,     2,     1,     1,     5,     0,     1,
       1,     1,     5,     1,     2,     2,     0,     2,     2,     1,
       2,     4,     7,     6,     6,     4,     0,     9,     0,     0,
       5,     0,     3,     0,     2,     3,     2,     2,     1,     1,
       0,     4,     0,     2,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     0,     1,     1,
       4,     6,     9,     0,     3,     0,     2,     0,     2,     3,
       1,     1,     5,     5,     1,     1,     3,     5,     0,     2,
       1,     1,     1,     5,     4,     3,     4,     3,     3,     3,
       0,     0,     5,     0,     1,     0,     2,     2,     3,     2,
       1,     0,     5,     0,     4,     1,     1,     0,     1,     0,
       1,     1,     1,     0,     2,     1,     3,     3,     6,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     3,     0,     2,
       2,     1,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     1,     2,     2,     1,     2,
       2,     1,     2,     2,     1,     2,     2,     1,     1,     3,
       3,     0,     2,     2,     6,     0,     2,     0,     3,     0,
       1,     1,     4,     1,     2,     1,     1,     0,     1,     3,
       1,     2,     1,     2,     2,     0,     1,     1,     3,     1,
       3,     0,     8,     1,     2,     1,     3,     0,     3,     2,
       4,     2,     0,     0,     5,     0,     0,     5,     0,     0,
       5,     0,     1,     1,     2,     5,     0,     2,     2,     3,
       1,     1,     2,     2,     2,     0,     1,     1,     2,     8,
       0,     3,     0,     4,     0,     4,     0,     3,     0,     3,
       1,     4,     2,     1,     1,     0,     2,     4,     0,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     4,     4,     2,     1,     1,     2,
       3,     1,     3,     6,     2,     3,     2,     1,     2,     2,
       1,     2,     0,     1,     1,     4,     2,     0,     1,     1,
       0,     0,     4,     0,     0,     0,     9,     0,     0,     3,
       0,     3,     1,     2,     4,     0,     2,     2,     0,     3,
       3,     4,     3,     0,     1,     0,     2,     0,     0,     7,
       0,     2,     1,     1,     1,     2,     1,     4,     2,     1,
       1,     0,     1,     0,     0,     3,     0,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     4,
       4,     4,     3,     3,     3,     4,     3,     4,     3,     3,
       3,     4,     5,     3,     4,     3,     3,     0,     3,     3,
       2,     2,     2,     3,     3,     3,     0,     2,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     3,     3,     3,
       3,     1,     1,     1,     1,     1,     1,     0,     1,     0,
       4,     4,     5,     6,     0,     2,     0,     1,     0,     3,
       3,     5,     0,     2,     2,     0,     5,     0,     2,     0,
       8,     0,     0,     3,     1,     2,     2,     3,     0,     2,
       2,     2,     0,     2,     2,     0,     0,     3,     0,     0,
       3,     0,     1,     0,     3,     0,     2,     1,     0,     3,
       0,     3,     0,     1,     3,     3,     2,     1,     1,     0,
       4,     4,     0,     1,     1,     1,     1,     1,     0,     6,
       0,     1,     0,     4,     0,     4,     3,     3,     3,     3,
       4,     6,     6,     6,     6,     0,     2,     2,     1,     2,
       1,     1,     2,     2,     1,     1,     1,     1,     1,     3,
       3,     3,     3,     1,     1,     0,     1,     0,     4,     4,
       6,     6,     8,     8,     0,     1,     0,     4,     0,     5,
       1,     3,     1,     1,     1,     2,     1,     2,     0,     3,
       0,     0,     3,     2,     3,     1,     3,     2,     1,     1,
       1,     0,     2,     0,     1,     0,     3,     0,     1,     1,
       2,     1,     1,     0,     3,     0,     3,     0,     5,     0,
       3,     0,     2,     0,     0,     8,     3,     0,     0,     3,
       0,     1,     0,     7,     0,     2,     0,     3,     3,     0,
       2,     1,     2,     4,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     0,     3,     0,     4,     1,     1,     1,
       1,     2,     1,     1,     1,     0,     3,     1,     2,     2,
       2,     1,     1,     1,     2,     2,     1,     2,     4,     2,
       0,     1,     1,     1,     1,     4,     5,     0,     4,     0,
       1,     0,     3,     0,     3,     3,     4,     0,     4,     4,
       6,     0,     1,     0,     3,     0,     5,     1,     1,     1,
       1,     0,     3,     0,     3,     2,     0,     3,     2,     0,
       4,     2,     0,     1,     1,     3,     0,     1,     2,     3,
       3,     0,     3,     1,     3,     7,     0,    10,     0,     2,
       0,     2,     2,     3,     3,     2,     0,     3,     0,     1,
       1,     0,     1,     0,     4,     0,     7,     0,     1,     0,
       7,     0,     2,     3,     0,     1,     1,     0,     4,     4,
       0,     7,     0,     2,     0,     0,     4,     1,     2,     0,
       4,     0,     1,     0,     3,     1,     1,     1,     1,     1,
       4,     4,     3,     4,     1,     1,     1,     2,     3,     1,
       2,     3,     3,     0,     3,     0,     7,     0,     6,     0,
       2,     0,     2,     0,     3,     0,     2,     4,     0,     2,
       4,     0,     0,     7,     0,     4,     2,     2,     2,     2,
       2,     0,     1,     0,     4,     0,     3,     0,     2,     2,
       0,     8,     1,     2,     1,     3,     3,     0,     3,     0,
       1,     0,     4,     4,     6,     6,     0,     1,     2,     0,
       1,     0,     3,     0,     7,     0,     4,     0,     1,     1,
       0,     9,     0,     3,     1,     3,     2,     2,     2,     3,
       0,     3,     0,     3,     0,     3,     0,     1,     1,     1,
       1,     8,     0,     1,     1,     1,     1,     1,     1,     0,
       1,     0,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     5,     1,     2,     5,     0,     8,     0,     2,     0,
       4,     3,     3,     1,     1,     0,     1,     1,     0,     1,
       2,     2,     0,     0,     3,     0,     0,     3,     2,     0,
       0,     3,     0,     0,     3,     2,     0,     0,     3,     0,
       0,     3,     1,     1,     2,     0,     3,     0,     3,     1,
       1,     2,     0,     3,     0,     3,     0,     1,     1,     1,
       2,     0,     3,     0,     3,     0,     3,     1,     1,     0,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     1,     2,
       1,     1,     2,     1,     2,     1,     5,     4,     1,     5,
       4,     1,     3,     0,     1,     1,     1,     3,     3,     3,
       3,     2,     2,     3,     3,     1,     3,     1,     2,     2,
       1,     1,     1,     2,     1,     1,     2,     1,     0,     2,
       1,     1,     1,     3,     1,     1,     2,     1,     1,     2,
       0,     1,     1,     1,     1,     1,     2,     1,     3,     1,
       2,     1,     3,     3,     3,     4,     3,     1,     1,     1,
       1,     3,     3,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     3,     1,     3,
       3,     4,     5,     1,     1,     2,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     5,     5,     5,
       5,     5,     5,     5,     4,     5,     2,     0,     4,     5,
       0,     3,     0,     1,     1,     3,     3,     1,     3,     1,
       3,     0,     0,     1,     0,     1,     0,     1,     0,     1,
       1,     0,     1,     0,     1,     0,     1,     0,     2,     1,
       1,     2,     2,     2,     1,     2,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     1,     0,     1,     0,     1,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     2,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     2,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= TOKEN_EOF)
    {
      yychar = TOKEN_EOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* $@1: %empty  */
#line 772 "parser.y"
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
#line 5217 "parser.c"
    break;

  case 3: /* start: $@1 nested_list "end of file"  */
#line 796 "parser.y"
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
#line 5239 "parser.c"
    break;

  case 8: /* $@2: %empty  */
#line 827 "parser.y"
                        { cb_validate_program_environment (current_program); }
#line 5245 "parser.c"
    break;

  case 9: /* $@3: %empty  */
#line 828 "parser.y"
                        { cb_validate_program_data (current_program); }
#line 5251 "parser.c"
    break;

  case 10: /* program_definition: identification_division environment_division $@2 data_division $@3 procedure_division nested_prog end_program  */
#line 831 "parser.y"
              {
	cb_tree file = current_program->file_list;
	for(; file; file = CB_CHAIN(file)) {
		cb_validate_indexed_file_key(CB_FILE(CB_VALUE(file)));
	}
  }
#line 5262 "parser.c"
    break;

  case 11: /* $@4: %empty  */
#line 841 "parser.y"
                        { cb_validate_program_environment (current_program); }
#line 5268 "parser.c"
    break;

  case 12: /* $@5: %empty  */
#line 842 "parser.y"
                        { cb_validate_program_data (current_program); }
#line 5274 "parser.c"
    break;

  case 13: /* program_mandatory: identification_division environment_division $@4 data_division $@5 procedure_division nested_prog end_mandatory  */
#line 845 "parser.y"
                {
	cb_tree file = current_program->file_list;
	for(; file; file = CB_CHAIN(file)) {
		cb_validate_indexed_file_key(CB_FILE(CB_VALUE(file)));
	}
  }
#line 5285 "parser.c"
    break;

  case 14: /* $@6: %empty  */
#line 855 "parser.y"
                        { cb_validate_program_environment (current_program); }
#line 5291 "parser.c"
    break;

  case 15: /* $@7: %empty  */
#line 856 "parser.y"
                        { cb_validate_program_data (current_program); }
#line 5297 "parser.c"
    break;

  case 21: /* end_program: "END PROGRAM" program_name '.'  */
#line 868 "parser.y"
  {
	char			*s;

	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P (yyvsp[-1])) {
		s = (char *)(CB_LITERAL (yyvsp[-1])->data);
	} else {
		s = (char *)(CB_NAME (yyvsp[-1]));
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
#line 5326 "parser.c"
    break;

  case 22: /* end_mandatory: "END PROGRAM" program_name '.'  */
#line 896 "parser.y"
  {
	char			*s;

	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P (yyvsp[-1])) {
		s = (char *)(CB_LITERAL (yyvsp[-1])->data);
	} else {
		s = (char *)(CB_NAME (yyvsp[-1]));
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
#line 5353 "parser.c"
    break;

  case 23: /* end_function: "END FUNCTION" program_name '.'  */
#line 922 "parser.y"
  {
	char			*s;

	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P (yyvsp[-1])) {
		s = (char *)(CB_LITERAL (yyvsp[-1])->data);
	} else {
		s = (char *)(CB_NAME (yyvsp[-1]));
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
#line 5380 "parser.c"
    break;

  case 24: /* $@8: %empty  */
#line 953 "parser.y"
  {
	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P (yyvsp[-1])) {
		stack_progid[depth] = (char *)(CB_LITERAL (yyvsp[-1])->data);
	} else {
		stack_progid[depth] = (char *)(CB_NAME (yyvsp[-1]));
	}
	if(strcmp("MAIN", stack_progid[depth]) == 0) {
		cb_error (_("PROGRAM-ID should not be MAIN"));
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
	current_program->program_id = cb_build_program_id (yyvsp[-1], yyvsp[0]);
  }
#line 5424 "parser.c"
    break;

  case 26: /* function_division: "FUNCTION-ID" '.' program_name as_literal '.'  */
#line 997 "parser.y"
  {
	cb_error (_("FUNCTION-ID is not yet implemented"));
	current_section = NULL;
	current_paragraph = NULL;
	if (CB_LITERAL_P (yyvsp[-2])) {
		stack_progid[depth] = (char *)(CB_LITERAL (yyvsp[-2])->data);
	} else {
		stack_progid[depth] = (char *)(CB_NAME (yyvsp[-2]));
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
	current_program->program_id = cb_build_program_id (yyvsp[-2], yyvsp[-1]);
	current_program->prog_type = CB_FUNCTION_TYPE;
	current_program->flag_recursive = 1;
	current_program->flag_initial = 1;
  }
#line 5469 "parser.c"
    break;

  case 29: /* as_literal: %empty  */
#line 1045 "parser.y"
                                { yyval = NULL; }
#line 5475 "parser.c"
    break;

  case 30: /* as_literal: AS "Literal"  */
#line 1046 "parser.y"
                                { yyval = yyvsp[0]; }
#line 5481 "parser.c"
    break;

  case 33: /* program_type_clause: COMMON  */
#line 1055 "parser.y"
  {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a nested program"));
	}
	current_program->flag_common = 1;
  }
#line 5492 "parser.c"
    break;

  case 34: /* program_type_clause: COMMON _init_or_recurs  */
#line 1062 "parser.y"
  {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a nested program"));
	}
	current_program->flag_common = 1;
  }
#line 5503 "parser.c"
    break;

  case 36: /* _init_or_recurs: "INITIAL"  */
#line 1073 "parser.y"
  {
	current_program->flag_initial = 1;
  }
#line 5511 "parser.c"
    break;

  case 37: /* _init_or_recurs: RECURSIVE  */
#line 1077 "parser.y"
  {
	current_program->flag_recursive = 1;
	current_program->flag_initial = 1;
  }
#line 5520 "parser.c"
    break;

  case 41: /* configuration_section: CONFIGURATION SECTION '.' configuration_list  */
#line 1102 "parser.y"
  {
	if (current_program->nested_level) {
		cb_error (_("CONFIGURATION SECTION not allowed in nested programs"));
	}
  }
#line 5530 "parser.c"
    break;

  case 53: /* with_debugging_mode: _with DEBUGGING MODE  */
#line 1137 "parser.y"
  {
	cb_verify (cb_debugging_line, "DEBUGGING MODE");
  }
#line 5538 "parser.c"
    break;

  case 54: /* computer_name: "Identifier"  */
#line 1143 "parser.y"
       { }
#line 5544 "parser.c"
    break;

  case 65: /* object_computer_memory: MEMORY SIZE _is integer object_char_or_word  */
#line 1174 "parser.y"
  {
	cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
#line 5552 "parser.c"
    break;

  case 68: /* object_computer_sequence: _program coll_sequence _is reference  */
#line 1186 "parser.y"
  {
	current_program->collating_sequence = yyvsp[0];
  }
#line 5560 "parser.c"
    break;

  case 69: /* object_computer_segment: "SEGMENT-LIMIT" _is integer  */
#line 1193 "parser.y"
  {
	/* Ignore */
  }
#line 5568 "parser.c"
    break;

  case 75: /* repository_name: FUNCTION repository_literal_list INTRINSIC  */
#line 1218 "parser.y"
  {
	current_program->function_spec_list = yyvsp[-1];
  }
#line 5576 "parser.c"
    break;

  case 76: /* repository_name: FUNCTION ALL INTRINSIC  */
#line 1222 "parser.y"
  {
	functions_are_all = 1;
  }
#line 5584 "parser.c"
    break;

  case 77: /* repository_literal_list: "Literal"  */
#line 1228 "parser.y"
                        { yyval = cb_list_init (yyvsp[0]); }
#line 5590 "parser.c"
    break;

  case 78: /* repository_literal_list: repository_literal_list "Literal"  */
#line 1230 "parser.y"
                        { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 5596 "parser.c"
    break;

  case 96: /* mnemonic_name_clause: "Identifier" _is CRT  */
#line 1271 "parser.y"
  {
	save_tree_1 = lookup_system_name (CB_NAME (yyvsp[-2]));
	if (save_tree_1 == cb_error_node) {
		cb_error_x (yyvsp[-2], _("Unknown system-name '%s'"), CB_NAME (yyvsp[-2]));
	} else if (CB_SYSTEM_NAME(save_tree_1)->token != CB_DEVICE_CONSOLE) {
		cb_error_x (save_tree_1, _("Invalid CRT clause"));
	}
	/* current_program->flag_screen = 1; */
  }
#line 5610 "parser.c"
    break;

  case 97: /* $@9: %empty  */
#line 1281 "parser.y"
  {
	save_tree_1 = lookup_system_name (CB_NAME (yyvsp[-2]));
	if (save_tree_1 == cb_error_node) {
		cb_error_x (yyvsp[-2], _("Unknown system-name '%s'"), CB_NAME (yyvsp[-2]));
	} else {
		cb_define (yyvsp[0], save_tree_1);
	}
	save_tree_2 = yyvsp[0];
  }
#line 5624 "parser.c"
    break;

  case 99: /* $@10: %empty  */
#line 1292 "parser.y"
  {
	save_tree_1 = lookup_system_name (CB_NAME (yyvsp[-1]));
	if (save_tree_1 == cb_error_node) {
		cb_error_x (yyvsp[-1], _("Unknown system-name '%s'"), CB_NAME (yyvsp[-1]));
	}
	save_tree_2 = NULL;
  }
#line 5636 "parser.c"
    break;

  case 101: /* mnemonic_name_clause: "ARGUMENT-NUMBER" _is undefined_word  */
#line 1301 "parser.y"
  {
	if (cb_enable_special_names_argument_clause) {
		save_tree_1 = lookup_system_name ("ARGUMENT-NUMBER");
		if (save_tree_1 == cb_error_node) {
			cb_error_x (yyvsp[-2], _("Unknown system-name '%s'"), CB_NAME (yyvsp[-2]));
		} else {
			cb_define (yyvsp[0], save_tree_1);
		}
		save_tree_2 = yyvsp[0];
	} else {
		cb_error (_("SPECIAL-NAMES with ARGUMENT-NUMBER clause is not yet supported"));
	}
  }
#line 5654 "parser.c"
    break;

  case 102: /* mnemonic_name_clause: "ARGUMENT-VALUE" _is undefined_word  */
#line 1315 "parser.y"
  {
	if (cb_enable_special_names_argument_clause) {
		save_tree_1 = lookup_system_name ("ARGUMENT-VALUE");
		if (save_tree_1 == cb_error_node) {
			cb_error_x (yyvsp[-2], _("Unknown system-name '%s'"), CB_NAME (yyvsp[-2]));
		} else {
			cb_define (yyvsp[0], save_tree_1);
		}
		save_tree_2 = yyvsp[0];
	} else {
		cb_error (_("SPECIAL-NAMES with ARGUMENT-VALUE clause is not yet supported"));
	}
  }
#line 5672 "parser.c"
    break;

  case 103: /* mnemonic_name_clause: "ENVIRONMENT-NAME" _is undefined_word  */
#line 1329 "parser.y"
  {
	if (cb_enable_special_names_environment_clause) {
		save_tree_1 = lookup_system_name ("ENVIRONMENT-NAME");
		if (save_tree_1 == cb_error_node) {
			cb_error_x (yyvsp[-2], _("Unknown system-name '%s'"), CB_NAME (yyvsp[-2]));
		} else {
			cb_define (yyvsp[0], save_tree_1);
		}
		save_tree_2 = yyvsp[0];
	} else {
		cb_error (_("SPECIAL-NAMES with ENVIRONMENT-NAME clause is not yet supported"));
	}
  }
#line 5690 "parser.c"
    break;

  case 104: /* mnemonic_name_clause: "ENVIRONMENT-VALUE" _is undefined_word  */
#line 1343 "parser.y"
  {
	if (cb_enable_special_names_environment_clause) {
		save_tree_1 = lookup_system_name ("ENVIRONMENT-VALUE");
		if (save_tree_1 == cb_error_node) {
			cb_error_x (yyvsp[-2], _("Unknown system-name '%s'"), CB_NAME (yyvsp[-2]));
		} else {
			cb_define (yyvsp[0], save_tree_1);
		}
		save_tree_2 = yyvsp[0];
	} else {
		cb_error (_("SPECIAL-NAMES with ENVIRONMENT-VALUE clause is not yet supported"));
	}
  }
#line 5708 "parser.c"
    break;

  case 109: /* special_name_mnemonic_on_off: on_or_off _status _is undefined_word  */
#line 1369 "parser.y"
  {
	if (!save_tree_2 && !cb_switch_no_mnemonic) {
		cb_error_x (yyvsp[0], _("'%s' with no mnemonic name"), CB_NAME (yyvsp[0]));
	} else {
		cb_define_switch_name (yyvsp[0], save_tree_1, yyvsp[-3], save_tree_2);
	}
  }
#line 5720 "parser.c"
    break;

  case 110: /* on_or_off: ON  */
#line 1379 "parser.y"
                                { yyval = cb_int1; }
#line 5726 "parser.c"
    break;

  case 111: /* on_or_off: OFF  */
#line 1380 "parser.y"
                                { yyval = cb_int0; }
#line 5732 "parser.c"
    break;

  case 112: /* $@11: %empty  */
#line 1388 "parser.y"
  {
	save_tree_1 = yyvsp[0];
  }
#line 5740 "parser.c"
    break;

  case 113: /* alphabet_name_clause: ALPHABET undefined_word $@11 _is alphabet_definition  */
#line 1392 "parser.y"
  {
	current_program->alphabet_name_list =
		cb_list_add (current_program->alphabet_name_list, yyvsp[0]);
  }
#line 5749 "parser.c"
    break;

  case 114: /* alphabet_definition: NATIVE  */
#line 1399 "parser.y"
                { yyval = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_NATIVE); }
#line 5755 "parser.c"
    break;

  case 115: /* alphabet_definition: "STANDARD-1"  */
#line 1400 "parser.y"
                { yyval = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_STANDARD_1); }
#line 5761 "parser.c"
    break;

  case 116: /* alphabet_definition: "STANDARD-2"  */
#line 1401 "parser.y"
                { yyval = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_STANDARD_2); }
#line 5767 "parser.c"
    break;

  case 117: /* alphabet_definition: EBCDIC  */
#line 1402 "parser.y"
                { yyval = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_EBCDIC); }
#line 5773 "parser.c"
    break;

  case 118: /* alphabet_definition: alphabet_literal_list  */
#line 1404 "parser.y"
  {
	yyval = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_CUSTOM);
	CB_ALPHABET_NAME (yyval)->custom_list = yyvsp[0];
  }
#line 5782 "parser.c"
    break;

  case 119: /* alphabet_literal_list: alphabet_literal  */
#line 1411 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 5788 "parser.c"
    break;

  case 120: /* alphabet_literal_list: alphabet_literal_list alphabet_literal  */
#line 1413 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 5794 "parser.c"
    break;

  case 121: /* alphabet_literal: alphabet_lits  */
#line 1417 "parser.y"
                                        { yyval = yyvsp[0]; }
#line 5800 "parser.c"
    break;

  case 122: /* alphabet_literal: alphabet_lits THRU alphabet_lits  */
#line 1418 "parser.y"
                                        { yyval = cb_build_pair (yyvsp[-2], yyvsp[0]); }
#line 5806 "parser.c"
    break;

  case 123: /* @12: %empty  */
#line 1420 "parser.y"
  {
	yyval = cb_list_init (yyvsp[-1]);
	save_tree_2 = yyval;
  }
#line 5815 "parser.c"
    break;

  case 124: /* alphabet_literal: alphabet_lits ALSO @12 alphabet_also_sequence  */
#line 1425 "parser.y"
  {
	yyval = yyvsp[-1];
  }
#line 5823 "parser.c"
    break;

  case 127: /* alphabet_lits: "Literal"  */
#line 1436 "parser.y"
                                { yyval = yyvsp[0]; }
#line 5829 "parser.c"
    break;

  case 128: /* alphabet_lits: SPACE  */
#line 1437 "parser.y"
                                { yyval = cb_space; }
#line 5835 "parser.c"
    break;

  case 129: /* alphabet_lits: ZERO  */
#line 1438 "parser.y"
                                { yyval = cb_zero; }
#line 5841 "parser.c"
    break;

  case 130: /* alphabet_lits: QUOTE  */
#line 1439 "parser.y"
                                { yyval = cb_quote; }
#line 5847 "parser.c"
    break;

  case 131: /* alphabet_lits: "HIGH-VALUE"  */
#line 1440 "parser.y"
                                { yyval = cb_norm_high; }
#line 5853 "parser.c"
    break;

  case 132: /* alphabet_lits: "LOW-VALUE"  */
#line 1441 "parser.y"
                                { yyval = cb_norm_low; }
#line 5859 "parser.c"
    break;

  case 133: /* alphabet_also_literal: "Literal"  */
#line 1445 "parser.y"
                                { cb_list_add (save_tree_2, yyvsp[0]); }
#line 5865 "parser.c"
    break;

  case 134: /* alphabet_also_literal: SPACE  */
#line 1446 "parser.y"
                                { cb_list_add (save_tree_2, cb_space); }
#line 5871 "parser.c"
    break;

  case 135: /* alphabet_also_literal: ZERO  */
#line 1447 "parser.y"
                                { cb_list_add (save_tree_2, cb_zero); }
#line 5877 "parser.c"
    break;

  case 136: /* alphabet_also_literal: QUOTE  */
#line 1448 "parser.y"
                                { cb_list_add (save_tree_2, cb_quote); }
#line 5883 "parser.c"
    break;

  case 137: /* alphabet_also_literal: "HIGH-VALUE"  */
#line 1449 "parser.y"
                                { cb_list_add (save_tree_2, cb_norm_high); }
#line 5889 "parser.c"
    break;

  case 138: /* alphabet_also_literal: "LOW-VALUE"  */
#line 1450 "parser.y"
                                { cb_list_add (save_tree_2, cb_norm_low); }
#line 5895 "parser.c"
    break;

  case 139: /* symbolic_characters_clause: SYMBOLIC _characters symbolic_characters_list  */
#line 1458 "parser.y"
  {
	if (yyvsp[0]) {
		current_program->symbolic_list =
			cb_list_add (current_program->symbolic_list, yyvsp[0]);
	}
	PENDING ("SYMBOLIC CHARACTERS");
  }
#line 5907 "parser.c"
    break;

  case 140: /* symbolic_characters_list: char_list _is_are integer_list  */
#line 1469 "parser.y"
  {
	if (cb_list_length (yyvsp[-2]) != cb_list_length (yyvsp[0])) {
		cb_error (_("Invalid SYMBOLIC clause"));
		yyval = NULL;
	} else {
		yyval = NULL;
	}
  }
#line 5920 "parser.c"
    break;

  case 141: /* char_list: undefined_word  */
#line 1480 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 5926 "parser.c"
    break;

  case 142: /* char_list: char_list undefined_word  */
#line 1481 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 5932 "parser.c"
    break;

  case 143: /* integer_list: integer  */
#line 1485 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 5938 "parser.c"
    break;

  case 144: /* integer_list: integer_list integer  */
#line 1486 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 5944 "parser.c"
    break;

  case 145: /* class_name_clause: CLASS undefined_word _is class_item_list  */
#line 1494 "parser.y"
  {
	current_program->class_name_list =
			cb_list_add (current_program->class_name_list,
			cb_build_class_name (yyvsp[-2], yyvsp[0]));
  }
#line 5954 "parser.c"
    break;

  case 146: /* class_item_list: class_item  */
#line 1502 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 5960 "parser.c"
    break;

  case 147: /* class_item_list: class_item_list class_item  */
#line 1503 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 5966 "parser.c"
    break;

  case 148: /* class_item: basic_value  */
#line 1507 "parser.y"
                                { yyval = yyvsp[0]; }
#line 5972 "parser.c"
    break;

  case 149: /* class_item: basic_value THRU basic_value  */
#line 1509 "parser.y"
  {
	/* if (CB_LITERAL ($1)->data[0] < CB_LITERAL ($3)->data[0]) */
	if (literal_value (yyvsp[-2]) < literal_value (yyvsp[0])) {
		yyval = cb_build_pair (yyvsp[-2], yyvsp[0]);
	} else {
		yyval = cb_build_pair (yyvsp[0], yyvsp[-2]);
	}
  }
#line 5985 "parser.c"
    break;

  case 150: /* locale_clause: LOCALE undefined_word _is reference  */
#line 1523 "parser.y"
  {
	cb_tree	l;

	l = cb_build_locale_name (yyvsp[-2], yyvsp[0]);
	if (l != cb_error_node) {
		current_program->locale_list =
			cb_list_add (current_program->locale_list, l);
	}
  }
#line 5999 "parser.c"
    break;

  case 151: /* currency_sign_clause: CURRENCY _sign _is "Literal"  */
#line 1538 "parser.y"
  {
	unsigned char *s = CB_LITERAL (yyvsp[0])->data;

	if (CB_LITERAL (yyvsp[0])->size != 1) {
		cb_error_x (yyvsp[0], _("Invalid currency sign '%s'"), s);
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
		cb_error_x (yyvsp[0], _("Invalid currency sign '%s'"), s);
		break;
	default:
		break;
	}
	current_program->currency_symbol = s[0];
  }
#line 6064 "parser.c"
    break;

  case 152: /* decimal_point_clause: "DECIMAL-POINT" _is COMMA  */
#line 1605 "parser.y"
  {
	current_program->decimal_point = ',';
	current_program->numeric_separator = '.';
  }
#line 6073 "parser.c"
    break;

  case 153: /* cursor_clause: CURSOR _is reference  */
#line 1615 "parser.y"
                                { current_program->cursor_pos = yyvsp[0]; }
#line 6079 "parser.c"
    break;

  case 154: /* crt_status_clause: CRT STATUS _is reference  */
#line 1622 "parser.y"
                                { current_program->crt_status = yyvsp[0]; }
#line 6085 "parser.c"
    break;

  case 155: /* screen_control: "SCREEN-CONTROL" _is reference  */
#line 1629 "parser.y"
                                {  PENDING ("SCREEN CONTROL"); }
#line 6091 "parser.c"
    break;

  case 156: /* event_status: "EVENT-STATUS" _is reference  */
#line 1635 "parser.y"
                                {  PENDING ("EVENT STATUS"); }
#line 6097 "parser.c"
    break;

  case 159: /* $@13: %empty  */
#line 1647 "parser.y"
  {
	/* hack for MF compatibility */
	if (cb_relaxed_syntax_check) {
		cb_warning (_("INPUT-OUTPUT SECTION header missing - assumed"));
	} else {
		cb_error (_("INPUT-OUTPUT SECTION header missing"));
	}
  }
#line 6110 "parser.c"
    break;

  case 161: /* $@14: %empty  */
#line 1656 "parser.y"
  {
	/* hack for MF compatibility */
	if (cb_relaxed_syntax_check) {
		cb_warning (_("INPUT-OUTPUT SECTION header missing - assumed"));
	} else {
		cb_error (_("INPUT-OUTPUT SECTION header missing"));
	}
  }
#line 6123 "parser.c"
    break;

  case 167: /* $@15: %empty  */
#line 1681 "parser.y"
  {
	organized_seen = 0;
	if (yyvsp[0] == cb_error_node) {
		YYERROR;
	}

	/* build new file */
	current_file = build_file (yyvsp[0]);
	current_file->optional = CB_INTEGER (yyvsp[-1])->val;

	/* register the file */
	current_program->file_list =
		cb_cons (CB_TREE (current_file), current_program->file_list);
  }
#line 6142 "parser.c"
    break;

  case 168: /* file_control_entry: SELECT flag_optional undefined_word $@15 select_clause_sequence '.'  */
#line 1696 "parser.y"
  {
	validate_file (current_file, yyvsp[-3]);
  }
#line 6150 "parser.c"
    break;

  case 186: /* assign_clause: ASSIGN _to _ext_clause _device assignment_name  */
#line 1728 "parser.y"
  {
	current_file->assign = cb_build_assignment_name (current_file, yyvsp[0]);
  }
#line 6158 "parser.c"
    break;

  case 187: /* assign_clause: ASSIGN _to _ext_clause DISK  */
#line 1732 "parser.y"
  {
	current_file->fileid_assign = 1;
	current_file->assign = cb_build_assignment_name (current_file, cb_build_reference ("DISK"));
  }
#line 6167 "parser.c"
    break;

  case 188: /* assign_clause: ASSIGN _to _ext_clause PRINTER  */
#line 1737 "parser.y"
  {
	current_file->fileid_assign = 1;
	current_file->assign = cb_build_assignment_name (current_file, cb_build_reference ("PRINTER"));
  }
#line 6176 "parser.c"
    break;

  case 191: /* _device: PRINTER  */
#line 1745 "parser.y"
                { current_file->organization = COB_ORG_LINE_SEQUENTIAL; }
#line 6182 "parser.c"
    break;

  case 193: /* _ext_clause: EXTERNAL  */
#line 1750 "parser.y"
  {
	current_file->external_assign = 1;
  }
#line 6190 "parser.c"
    break;

  case 194: /* _ext_clause: DYNAMIC  */
#line 1754 "parser.y"
  {
	current_file->external_assign = 0;
  }
#line 6198 "parser.c"
    break;

  case 196: /* assignment_name: DISPLAY  */
#line 1762 "parser.y"
  {
	const char	*s;

	s = "$#@DUMMY@#$";
	yyval = cb_build_alphanumeric_literal ((unsigned char *)s, strlen (s));
  }
#line 6209 "parser.c"
    break;

  case 197: /* assignment_name: _literal assignment_device_name_list  */
#line 1769 "parser.y"
  {

	if (!yyvsp[-1] || (yyvsp[-1] && CB_TREE_CLASS (yyvsp[-1]) == CB_CLASS_NUMERIC)) {
		if (yyvsp[0]) {
			if (CB_CHAIN (yyvsp[0])) {
				PENDING (_("ASSIGN TO multiple external device names"));
			}
			yyval = CB_VALUE (yyvsp[0]);
		}
	} else {
		if(yyvsp[0]) {
			PENDING (_("ASSIGN TO multiple external device names"));
		}
		yyval = yyvsp[-1];
	}
  }
#line 6230 "parser.c"
    break;

  case 198: /* assignment_device_name_list: qualified_word  */
#line 1788 "parser.y"
                                                { yyval = cb_list_init (yyvsp[0]); }
#line 6236 "parser.c"
    break;

  case 199: /* assignment_device_name_list: assignment_device_name_list qualified_word  */
#line 1789 "parser.y"
                                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 6242 "parser.c"
    break;

  case 201: /* access_mode: SEQUENTIAL  */
#line 1799 "parser.y"
                        { current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
#line 6248 "parser.c"
    break;

  case 202: /* access_mode: DYNAMIC  */
#line 1800 "parser.y"
                        { current_file->access_mode = COB_ACCESS_DYNAMIC; }
#line 6254 "parser.c"
    break;

  case 203: /* access_mode: RANDOM  */
#line 1801 "parser.y"
                        { current_file->access_mode = COB_ACCESS_RANDOM; }
#line 6260 "parser.c"
    break;

  case 204: /* alternative_record_key_clause: ALTERNATE RECORD _key _is reference flag_duplicates  */
#line 1809 "parser.y"
  {
	struct cb_alt_key *p;
	struct cb_alt_key *l;

	p = cobc_malloc (sizeof (struct cb_alt_key));
	p->key = yyvsp[-1];
	p->duplicates = CB_INTEGER (yyvsp[0])->val;
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
#line 6284 "parser.c"
    break;

  case 205: /* alternative_record_key_clause: ALTERNATE RECORD _key _is reference key_is_eq split_key_list flag_duplicates  */
#line 1829 "parser.y"
  {
	struct cb_alt_key *p;
	struct cb_alt_key *l;
	cb_tree composite_key;
	struct cb_key_component *comp;

	p = cobc_malloc (sizeof (struct cb_alt_key));
	/* generate field (in w-s) for composite-key */
	if (!yyvsp[-2]) {
		/* dialect */
		composite_key = cb_build_field (cb_build_anonymous ());
		comp = cobc_malloc (sizeof (struct cb_key_component));
		comp->next = key_component_list;
		comp->component = yyvsp[-3];
		key_component_list = comp;
	} else {
		/* standard or mf syntax */
		composite_key = cb_build_field (yyvsp[-3]);
	}
	if (composite_key == cb_error_node) {
		YYERROR;
	} else {
		composite_key->category = CB_CATEGORY_ALPHANUMERIC;
		((struct cb_field *)composite_key)->count = 1;
		p->key = cb_build_field_reference ((struct cb_field *)composite_key, NULL);
		p->component_list = key_component_list;
		p->duplicates = CB_INTEGER (yyvsp[0])->val;
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
  }
#line 6328 "parser.c"
    break;

  case 206: /* $@16: %empty  */
#line 1871 "parser.y"
  {
	key_component_list = NULL;
  }
#line 6336 "parser.c"
    break;

  case 209: /* split_key: reference  */
#line 1880 "parser.y"
  {
	struct cb_key_component *c;
	struct cb_key_component *comp = cobc_malloc (sizeof (struct cb_key_component));
	comp->next = NULL;
	comp->component = yyvsp[0];
	if (key_component_list == NULL) {
		key_component_list = comp;
	} else {
		for (c = key_component_list; c->next != NULL; c = c->next);
		c->next = comp;
	}
  }
#line 6353 "parser.c"
    break;

  case 210: /* key_is_eq: %empty  */
#line 1895 "parser.y"
                { yyval = NULL; }
#line 6359 "parser.c"
    break;

  case 211: /* key_is_eq: SOURCE _is  */
#line 1896 "parser.y"
                { yyval = cb_int1; }
#line 6365 "parser.c"
    break;

  case 212: /* key_is_eq: '='  */
#line 1897 "parser.y"
                { yyval = cb_int('='); }
#line 6371 "parser.c"
    break;

  case 213: /* collating_sequence_clause: coll_sequence _is "Identifier"  */
#line 1904 "parser.y"
  {
	PENDING ("COLLATING SEQUENCE");
  }
#line 6379 "parser.c"
    break;

  case 214: /* file_status_clause: file_or_sort STATUS _is reference opt_reference  */
#line 1914 "parser.y"
  {
	current_file->file_status = yyvsp[-1];
	if (yyvsp[0]) {
		PENDING ("2nd FILE STATUS");
	}
  }
#line 6390 "parser.c"
    break;

  case 219: /* lock_mode: MANUAL lock_with  */
#line 1935 "parser.y"
                        { current_file->lock_mode = COB_LOCK_MANUAL; }
#line 6396 "parser.c"
    break;

  case 220: /* lock_mode: AUTOMATIC lock_with  */
#line 1936 "parser.y"
                        { current_file->lock_mode = COB_LOCK_AUTOMATIC; }
#line 6402 "parser.c"
    break;

  case 221: /* lock_mode: EXCLUSIVE  */
#line 1937 "parser.y"
                        { current_file->lock_mode = COB_LOCK_EXCLUSIVE; }
#line 6408 "parser.c"
    break;

  case 224: /* lock_with: WITH LOCK ON MULTIPLE lock_records  */
#line 1943 "parser.y"
  {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
#line 6416 "parser.c"
    break;

  case 225: /* lock_with: WITH ROLLBACK  */
#line 1946 "parser.y"
                                { PENDING ("WITH ROLLBACK"); }
#line 6422 "parser.c"
    break;

  case 230: /* organization: INDEXED  */
#line 1963 "parser.y"
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = COB_ORG_INDEXED;
		organized_seen = 1;
	}
  }
#line 6435 "parser.c"
    break;

  case 231: /* organization: RECORD _binary SEQUENTIAL  */
#line 1972 "parser.y"
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = COB_ORG_SEQUENTIAL;
		organized_seen = 1;
	}
  }
#line 6448 "parser.c"
    break;

  case 232: /* organization: SEQUENTIAL  */
#line 1981 "parser.y"
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = cb_default_organization;
		organized_seen = 1;
	}
  }
#line 6461 "parser.c"
    break;

  case 233: /* organization: RELATIVE  */
#line 1990 "parser.y"
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = COB_ORG_RELATIVE;
		organized_seen = 1;
	}
  }
#line 6474 "parser.c"
    break;

  case 234: /* organization: LINE SEQUENTIAL  */
#line 1999 "parser.y"
  {
	if (organized_seen) {
		cb_error (_("Invalid or duplicate ORGANIZED clause"));
	} else {
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		organized_seen = 1;
	}
  }
#line 6487 "parser.c"
    break;

  case 235: /* padding_character_clause: PADDING _character _is reference_or_literal  */
#line 2014 "parser.y"
  {
	cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
#line 6495 "parser.c"
    break;

  case 236: /* record_delimiter_clause: RECORD DELIMITER _is "STANDARD-1"  */
#line 2023 "parser.y"
                                        { /* ignored */ }
#line 6501 "parser.c"
    break;

  case 237: /* record_key_clause: RECORD _key _is reference flag_duplicates  */
#line 2031 "parser.y"
  {

	if(yyvsp[0] == cb_int1) {
		cb_error (_("Record keys with duplicates are not yet supported"));
	}

	current_file->key = yyvsp[-1];
  }
#line 6514 "parser.c"
    break;

  case 238: /* record_key_clause: RECORD _key _is reference key_is_eq split_key_list flag_duplicates  */
#line 2040 "parser.y"
  {
	/* SPLIT KEY use */

	cb_tree composite_key;	
	struct cb_key_component *comp;

	if(yyvsp[0] == cb_int1) {
		cb_error (_("Record keys with duplicates are not yet supported"));
	}

	/* generate field (in w-s) for composite-key */
	if (!yyvsp[-2]) {
		/* dialect */
		composite_key = cb_build_field (cb_build_anonymous ());
		comp = cobc_malloc (sizeof (struct cb_key_component));
		comp->next = key_component_list;
		comp->component = yyvsp[-3];
		key_component_list = comp;
	} else {
		/* standard or mf syntax */
		composite_key = cb_build_field (yyvsp[-3]);
	}
	if (composite_key == cb_error_node) {
		YYERROR;
	} else {
		composite_key->category = CB_CATEGORY_ALPHANUMERIC;
		((struct cb_field *)composite_key)->count = 1;
		current_file->key = cb_build_field_reference ((struct cb_field *)composite_key, NULL);
		current_file->component_list = key_component_list;
	}
  }
#line 6550 "parser.c"
    break;

  case 239: /* relative_key_clause: RELATIVE _key _is reference  */
#line 2077 "parser.y"
                                { current_file->key = yyvsp[0]; }
#line 6556 "parser.c"
    break;

  case 240: /* reserve_clause: RESERVE integer _area  */
#line 2084 "parser.y"
                                { /* ignored */ }
#line 6562 "parser.c"
    break;

  case 241: /* reserve_clause: RESERVE NO  */
#line 2085 "parser.y"
                                { /* ignored */ }
#line 6568 "parser.c"
    break;

  case 242: /* sharing_clause: SHARING _with sharing_option  */
#line 2092 "parser.y"
                                { current_file->sharing = yyvsp[0]; }
#line 6574 "parser.c"
    break;

  case 243: /* sharing_option: ALL _other  */
#line 2096 "parser.y"
                                { yyval = NULL; PENDING ("SHARING ALL OTHER"); }
#line 6580 "parser.c"
    break;

  case 244: /* sharing_option: NO _other  */
#line 2097 "parser.y"
                                { yyval = cb_int1; }
#line 6586 "parser.c"
    break;

  case 245: /* sharing_option: READ ONLY  */
#line 2098 "parser.y"
                                { yyval = cb_int0; }
#line 6592 "parser.c"
    break;

  case 246: /* nominal_key_clause: NOMINAL _key _is reference  */
#line 2104 "parser.y"
                                { PENDING ("NOMINAL KEY"); }
#line 6598 "parser.c"
    break;

  case 257: /* same_clause: SAME same_option _area _for file_name_list  */
#line 2135 "parser.y"
  {
	cb_tree l;

	switch (CB_INTEGER (yyvsp[-3])->val) {
	case 0:
		/* SAME AREA */
		break;
	case 1:
		/* SAME RECORD */
		for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
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
#line 6624 "parser.c"
    break;

  case 258: /* same_option: %empty  */
#line 2159 "parser.y"
                                { yyval = cb_int0; }
#line 6630 "parser.c"
    break;

  case 259: /* same_option: RECORD  */
#line 2160 "parser.y"
                                { yyval = cb_int1; }
#line 6636 "parser.c"
    break;

  case 260: /* same_option: SORT  */
#line 2161 "parser.y"
                                { yyval = cb_int2; }
#line 6642 "parser.c"
    break;

  case 261: /* same_option: "SORT-MERGE"  */
#line 2162 "parser.y"
                                { yyval = cb_int2; }
#line 6648 "parser.c"
    break;

  case 262: /* multiple_file_tape_clause: MULTIPLE _file _tape _contains multiple_file_list  */
#line 2169 "parser.y"
  {
	cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
  }
#line 6656 "parser.c"
    break;

  case 265: /* multiple_file: file_name multiple_file_position  */
#line 2180 "parser.y"
                                   { }
#line 6662 "parser.c"
    break;

  case 271: /* apply_clause: APPLY "COMMITMENT-CONTROL" _on reference_list  */
#line 2197 "parser.y"
  {
	PENDING ("APPLY COMMITMENT-CONTROL");
  }
#line 6670 "parser.c"
    break;

  case 272: /* apply_clause: APPLY "CYL-OVERFLOW" _of "Literal" TRACKS ON reference_list  */
#line 2201 "parser.y"
  {
	PENDING ("APPLY CYL-OVERFLOW");
  }
#line 6678 "parser.c"
    break;

  case 273: /* apply_clause: APPLY "CORE-INDEX" TO reference ON reference_list  */
#line 2205 "parser.y"
  {
	PENDING ("APPLY CORE-INDEX");
  }
#line 6686 "parser.c"
    break;

  case 274: /* apply_clause: APPLY "FORMS-OVERLAY" TO reference ON reference_list  */
#line 2209 "parser.y"
  {
	PENDING ("APPLY FORMS-OVERLAY");
  }
#line 6694 "parser.c"
    break;

  case 275: /* apply_clause: APPLY "CLOSE-NOFEED" ON reference_list  */
#line 2213 "parser.y"
  {
	PENDING ("APPLY CLOSE-NOFEED");
  }
#line 6702 "parser.c"
    break;

  case 279: /* $@17: %empty  */
#line 2238 "parser.y"
                                { current_storage = CB_STORAGE_FILE; }
#line 6708 "parser.c"
    break;

  case 281: /* $@18: %empty  */
#line 2241 "parser.y"
  {
	/* hack for MF compatibility */
	if (cb_relaxed_syntax_check) {
		cb_warning (_("FILE SECTION header missing - assumed"));
	} else {
		cb_error (_("FILE SECTION header missing"));
	}
	current_storage = CB_STORAGE_FILE;
  }
#line 6722 "parser.c"
    break;

  case 285: /* file_description: file_type file_description_entry record_description_list  */
#line 2260 "parser.y"
  {
	if (yyvsp[0] && yyvsp[0] != cb_error_node) {
		finalize_file (current_file, CB_FIELD (yyvsp[0]));
	} else {
		cb_error (_("RECORD description missing or invalid"));
	}
  }
#line 6734 "parser.c"
    break;

  case 286: /* file_description_sequence_without_type: file_description_entry record_description_list  */
#line 2272 "parser.y"
  {
	if (yyvsp[0] && yyvsp[0] != cb_error_node) {
		finalize_file (current_file, CB_FIELD (yyvsp[0]));
	} else {
		cb_error (_("RECORD description missing or invalid"));
	}
  }
#line 6746 "parser.c"
    break;

  case 288: /* file_type: FD  */
#line 2283 "parser.y"
                               { yyval = cb_int0; }
#line 6752 "parser.c"
    break;

  case 289: /* file_type: SD  */
#line 2284 "parser.y"
                               { yyval = cb_int1; }
#line 6758 "parser.c"
    break;

  case 290: /* @19: %empty  */
#line 2294 "parser.y"
  {
	if (yyvsp[0] == cb_error_node) {
		YYERROR;
	}

	current_file = CB_FILE (cb_ref (yyvsp[0]));
	if (yyvsp[-1] == cb_int1) {
		current_file->organization = COB_ORG_SORT;
	}
  }
#line 6773 "parser.c"
    break;

  case 291: /* file_description_entry: file_name @19 file_description_clause_sequence '.'  */
#line 2305 "parser.y"
  {
	/* Shut up bison */
	dummy_tree = yyvsp[-2];
  }
#line 6782 "parser.c"
    break;

  case 294: /* file_description_clause: _is EXTERNAL  */
#line 2317 "parser.y"
  {
	if (current_file->global) {
		cb_error (_("File cannot have both EXTERNAL and GLOBAL clauses"));
	}
	current_file->external = 1;
  }
#line 6793 "parser.c"
    break;

  case 295: /* file_description_clause: _is GLOBAL  */
#line 2324 "parser.y"
  {
	if (current_file->external) {
		cb_error (_("File cannot have both EXTERNAL and GLOBAL clauses"));
	}
	current_file->global = 1;
  }
#line 6804 "parser.c"
    break;

  case 306: /* block_contains_clause: BLOCK _contains integer opt_to_integer _records_or_characters  */
#line 2347 "parser.y"
  { /* ignored */ }
#line 6810 "parser.c"
    break;

  case 310: /* record_clause: RECORD _contains integer _characters  */
#line 2357 "parser.y"
  {
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_max = cb_get_int (yyvsp[-1]);
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			cb_error (_("RECORD clause invalid"));
		}
	}
  }
#line 6826 "parser.c"
    break;

  case 311: /* record_clause: RECORD _contains integer TO integer _characters  */
#line 2369 "parser.y"
  {
	int	error_ind = 0;

	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_min = cb_get_int (yyvsp[-3]);
		current_file->record_max = cb_get_int (yyvsp[-1]);
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
#line 6855 "parser.c"
    break;

  case 312: /* record_clause: RECORD _is VARYING _in _size opt_from_integer opt_to_integer _characters record_depending  */
#line 2395 "parser.y"
  {
	int	error_ind = 0;

	current_file->record_min = yyvsp[-3] ? cb_get_int (yyvsp[-3]) : 0;
	current_file->record_max = yyvsp[-2] ? cb_get_int (yyvsp[-2]) : 0;
	if (yyvsp[-3] && current_file->record_min < 0)  {
		current_file->record_min = 0;
		error_ind = 1;
	}
	if (yyvsp[-2] && current_file->record_max < 1)  {
		current_file->record_max = 1;
		error_ind = 1;
	}
	if ((yyvsp[-3] || yyvsp[-2]) && current_file->record_max <= current_file->record_min)  {
		error_ind = 1;
	}
	if (error_ind) {
		cb_error (_("RECORD clause invalid"));
	}
  }
#line 6880 "parser.c"
    break;

  case 314: /* record_depending: DEPENDING _on reference  */
#line 2419 "parser.y"
  {
	current_file->record_depending = yyvsp[0];
  }
#line 6888 "parser.c"
    break;

  case 315: /* opt_from_integer: %empty  */
#line 2425 "parser.y"
                                { yyval = NULL; }
#line 6894 "parser.c"
    break;

  case 316: /* opt_from_integer: _from integer  */
#line 2426 "parser.y"
                                { yyval = yyvsp[0]; }
#line 6900 "parser.c"
    break;

  case 317: /* opt_to_integer: %empty  */
#line 2430 "parser.y"
                                { yyval = NULL; }
#line 6906 "parser.c"
    break;

  case 318: /* opt_to_integer: TO integer  */
#line 2431 "parser.y"
                                { yyval = yyvsp[0]; }
#line 6912 "parser.c"
    break;

  case 319: /* label_records_clause: LABEL records label_option  */
#line 2439 "parser.y"
  {
	cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
#line 6920 "parser.c"
    break;

  case 322: /* value_of_clause: VALUE OF "Identifier" _is valueof_name  */
#line 2454 "parser.y"
  {
	cb_verify (cb_value_of_clause, "VALUE OF");
  }
#line 6928 "parser.c"
    break;

  case 323: /* value_of_clause: VALUE OF "FILE-ID" _is valueof_name  */
#line 2458 "parser.y"
  {
	if (!current_file->assign) {
		current_file->assign = cb_build_assignment_name (current_file, yyvsp[0]);
	}
  }
#line 6938 "parser.c"
    break;

  case 326: /* data_records_clause: DATA records no_reference_list  */
#line 2474 "parser.y"
  {
	cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
#line 6946 "parser.c"
    break;

  case 327: /* linage_clause: LINAGE _is reference_or_literal _lines linage_sequence  */
#line 2485 "parser.y"
  {
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL
	    && current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("LINAGE clause with wrong file type"));
		yyval = cb_error_node;
	} else {
		current_file->linage = yyvsp[-2];
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		if (current_linage == 0) {
			linage_file = current_file;
		}
		current_linage++;
	}
  }
#line 6965 "parser.c"
    break;

  case 333: /* linage_footing: _with FOOTING _at reference_or_literal _lines  */
#line 2513 "parser.y"
  {
	current_file->latfoot = yyvsp[-1];
  }
#line 6973 "parser.c"
    break;

  case 334: /* linage_top: _at TOP reference_or_literal _lines  */
#line 2520 "parser.y"
  {
	current_file->lattop = yyvsp[-1];
  }
#line 6981 "parser.c"
    break;

  case 335: /* linage_bottom: _at BOTTOM reference_or_literal  */
#line 2527 "parser.y"
  {
	current_file->latbot = yyvsp[0];
  }
#line 6989 "parser.c"
    break;

  case 336: /* recording_mode_clause: RECORDING _mode _is "Identifier"  */
#line 2536 "parser.y"
                                { /* ignore */ }
#line 6995 "parser.c"
    break;

  case 337: /* code_set_clause: "CODE-SET" _is "Identifier"  */
#line 2544 "parser.y"
  {
	if (yyvsp[0] != cb_error_node) {
		cb_tree x;

		x = cb_ref (yyvsp[0]);
		if (!CB_ALPHABET_NAME_P (x)) {
			cb_error_x (yyvsp[0], _("Alphabet-name is expected '%s'"), cb_name (yyvsp[0]));
		} else if (CB_ALPHABET_NAME (x)->custom_list) {
			PENDING ("CODE-SET");
		}
	}
  }
#line 7012 "parser.c"
    break;

  case 338: /* report_clause: REPORT _is report_name  */
#line 2562 "parser.y"
  {
	cb_warning (_("file descriptor REPORT IS"));
  }
#line 7020 "parser.c"
    break;

  case 339: /* report_clause: REPORTS _are report_name  */
#line 2566 "parser.y"
  {
	cb_warning (_("file descriptor REPORTS ARE"));
  }
#line 7028 "parser.c"
    break;

  case 341: /* $@20: %empty  */
#line 2577 "parser.y"
                                { current_storage = CB_STORAGE_WORKING; }
#line 7034 "parser.c"
    break;

  case 342: /* working_storage_section: "WORKING-STORAGE" SECTION '.' $@20 record_description_list  */
#line 2579 "parser.y"
  {
	if (yyvsp[0]) {
		current_program->working_storage =
			cb_field_add (current_program->working_storage, CB_FIELD (yyvsp[0]));
	}
  }
#line 7045 "parser.c"
    break;

  case 343: /* record_description_list: %empty  */
#line 2588 "parser.y"
                                { yyval = NULL; }
#line 7051 "parser.c"
    break;

  case 344: /* record_description_list: record_description_list_1  */
#line 2589 "parser.y"
                                { yyval = yyvsp[0]; }
#line 7057 "parser.c"
    break;

  case 345: /* $@21: %empty  */
#line 2593 "parser.y"
  {
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 7067 "parser.c"
    break;

  case 346: /* record_description_list_1: $@21 record_description_list_2  */
#line 2599 "parser.y"
  {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	yyval = CB_TREE (description_field);
  }
#line 7080 "parser.c"
    break;

  case 351: /* $@22: %empty  */
#line 2619 "parser.y"
  {
	cb_tree x;

	x = cb_build_field_tree (yyvsp[-1], yyvsp[0], current_field, current_storage, current_file, cb_source_file, cb_source_line, prev_field_line_number);
	prev_field_line_number = cb_source_line;
	if (x == cb_error_node) {
		YYERROR;
	} else {
		current_field = CB_FIELD (x);
	}
  }
#line 7096 "parser.c"
    break;

  case 352: /* data_description: level_number entry_name $@22 data_description_clause_sequence _maybe_next_level_number  */
#line 2631 "parser.y"
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
#line 7113 "parser.c"
    break;

  case 353: /* $@23: %empty  */
#line 2644 "parser.y"
  {
	cb_tree x;

	x = cb_build_field_tree (yyvsp[-1], yyvsp[0], current_field, current_storage, current_file, cb_source_file, cb_source_line, prev_field_line_number);
	prev_field_line_number = cb_source_line;
	if (x == cb_error_node) {
		YYERROR;
	} else {
		current_field = CB_FIELD (x);
	}
  }
#line 7129 "parser.c"
    break;

  case 354: /* data_description: level_number_88 entry_name $@23 value_cond_clause  */
#line 2656 "parser.y"
  {
	if (!qualifier) {
		cb_error (_("Item requires a data name"));
	}
	cb_validate_88_item (current_field);
	if (!description_field) {
		description_field = current_field;
	}
	
  }
#line 7144 "parser.c"
    break;

  case 358: /* _maybe_next_level_number: "Literal"  */
#line 2683 "parser.y"
  {
	if (CB_TREE_CLASS (yyvsp[0]) == CB_CLASS_NUMERIC) {
		cb_tree x = cb_build_reference ((char *)CB_LITERAL(yyvsp[0])->data);
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
		cb_unget_token (LITERAL, yyvsp[0]);
	}
  }
#line 7166 "parser.c"
    break;

  case 359: /* entry_name: %empty  */
#line 2704 "parser.y"
  {
	yyval = cb_build_filler ();
	qualifier = NULL;
	non_const_word = 0;
  }
#line 7176 "parser.c"
    break;

  case 360: /* entry_name: FILLER  */
#line 2710 "parser.y"
  {
	yyval = cb_build_filler ();
	qualifier = NULL;
	non_const_word = 0;
  }
#line 7186 "parser.c"
    break;

  case 361: /* entry_name: "Identifier"  */
#line 2716 "parser.y"
  {
	yyval = yyvsp[0];
	qualifier = yyvsp[0];
	non_const_word = 0;
  }
#line 7196 "parser.c"
    break;

  case 362: /* const_name: "Identifier"  */
#line 2725 "parser.y"
  {
	yyval = yyvsp[0];
	qualifier = yyvsp[0];
	non_const_word = 0;
  }
#line 7206 "parser.c"
    break;

  case 364: /* const_global: _is GLOBAL  */
#line 2734 "parser.y"
  {
	current_field->flag_is_global = 1;
	cb_error (_("CONSTANT with GLOBAL clause is not yet supported"));
  }
#line 7215 "parser.c"
    break;

  case 365: /* lit_or_length: literal  */
#line 2741 "parser.y"
                                { yyval = yyvsp[0]; }
#line 7221 "parser.c"
    break;

  case 366: /* lit_or_length: LENGTH _of identifier_1  */
#line 2742 "parser.y"
                                { yyval = cb_build_const_length (yyvsp[0]); }
#line 7227 "parser.c"
    break;

  case 367: /* lit_or_length: "BYTE-LENGTH" _of identifier_1  */
#line 2743 "parser.y"
                                { yyval = cb_build_const_length (yyvsp[0]); }
#line 7233 "parser.c"
    break;

  case 368: /* constant_entry: level_number const_name CONSTANT const_global _as lit_or_length  */
#line 2748 "parser.y"
  {
	cb_tree x;
	int	level;

	level = cb_get_level (yyvsp[-5]);
	if (level && level != 1) {
		cb_error (_("CONSTANT item not at 01 level"));
	}
	x = cb_build_constant (yyvsp[-4], yyvsp[0]);
	CB_FIELD (x)->flag_item_78 = 1;
	CB_FIELD (x)->level = 1;
	cb_needs_01 = 1;
	/* Ignore return value */
	cb_validate_78_item (CB_FIELD (x));
  }
#line 7253 "parser.c"
    break;

  case 369: /* data_description_clause_sequence: %empty  */
#line 2767 "parser.y"
  {
	/* required to check redefines */
	yyval = NULL;
  }
#line 7262 "parser.c"
    break;

  case 370: /* data_description_clause_sequence: data_description_clause_sequence data_description_clause  */
#line 2773 "parser.y"
  {
	/* required to check redefines */
	yyval = cb_true;
  }
#line 7271 "parser.c"
    break;

  case 386: /* redefines_clause: REDEFINES identifier_1  */
#line 2802 "parser.y"
  {
	if (yyvsp[-2] != NULL) {
		/* hack for MF compatibility */
		if (cb_relaxed_syntax_check) {
			cb_warning_x (yyvsp[0], _("REDEFINES clause should follow entry-name"));
		} else {
			cb_error_x (yyvsp[0], _("REDEFINES clause must follow entry-name"));
		}
	}

	current_field->redefines = cb_resolve_redefines (current_field, yyvsp[0]);
	if (current_field->redefines == NULL) {
		YYERROR;
	}
  }
#line 7291 "parser.c"
    break;

  case 387: /* external_clause: _is EXTERNAL as_extname  */
#line 2824 "parser.y"
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
#line 7314 "parser.c"
    break;

  case 388: /* as_extname: %empty  */
#line 2845 "parser.y"
                                { current_field->ename = NULL; }
#line 7320 "parser.c"
    break;

  case 389: /* as_extname: AS "Literal"  */
#line 2847 "parser.y"
 {
	struct cb_field *x;

	x = CB_FIELD(cb_build_field (cb_build_reference ((char *)(CB_LITERAL (yyvsp[0])->data))));
	current_field->ename = x->name;
 }
#line 7331 "parser.c"
    break;

  case 390: /* global_clause: _is GLOBAL  */
#line 2859 "parser.y"
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
#line 7349 "parser.c"
    break;

  case 391: /* picture_clause: PICTURE  */
#line 2878 "parser.y"
                                { current_field->pic = CB_PICTURE (yyvsp[0]); }
#line 7355 "parser.c"
    break;

  case 394: /* usage: BINARY  */
#line 2890 "parser.y"
                                { current_field->usage = CB_USAGE_BINARY; }
#line 7361 "parser.c"
    break;

  case 395: /* usage: COMP  */
#line 2891 "parser.y"
                                { current_field->usage = CB_USAGE_BINARY; }
#line 7367 "parser.c"
    break;

  case 396: /* usage: "COMP-1"  */
#line 2892 "parser.y"
                                { current_field->usage = CB_USAGE_FLOAT; }
#line 7373 "parser.c"
    break;

  case 397: /* usage: "COMP-2"  */
#line 2893 "parser.y"
                                { current_field->usage = CB_USAGE_DOUBLE; }
#line 7379 "parser.c"
    break;

  case 398: /* usage: "COMP-3"  */
#line 2894 "parser.y"
                                { current_field->usage = CB_USAGE_PACKED; }
#line 7385 "parser.c"
    break;

  case 399: /* usage: "COMP-4"  */
#line 2895 "parser.y"
                                { current_field->usage = CB_USAGE_BINARY; }
#line 7391 "parser.c"
    break;

  case 400: /* usage: "COMP-5"  */
#line 2896 "parser.y"
                                { current_field->usage = CB_USAGE_COMP_5; }
#line 7397 "parser.c"
    break;

  case 401: /* usage: "COMP-X"  */
#line 2897 "parser.y"
                                { current_field->usage = CB_USAGE_COMP_X; }
#line 7403 "parser.c"
    break;

  case 402: /* usage: DISPLAY  */
#line 2898 "parser.y"
                                { current_field->usage = CB_USAGE_DISPLAY; }
#line 7409 "parser.c"
    break;

  case 403: /* usage: INDEX  */
#line 2899 "parser.y"
                                { current_field->usage = CB_USAGE_INDEX; }
#line 7415 "parser.c"
    break;

  case 404: /* usage: "PACKED-DECIMAL"  */
#line 2900 "parser.y"
                                { current_field->usage = CB_USAGE_PACKED; }
#line 7421 "parser.c"
    break;

  case 405: /* usage: POINTER  */
#line 2902 "parser.y"
  {
	current_field->usage = CB_USAGE_POINTER;
	current_field->flag_is_pointer = 1;
  }
#line 7430 "parser.c"
    break;

  case 406: /* usage: "PROGRAM-POINTER"  */
#line 2907 "parser.y"
  {
	current_field->usage = CB_USAGE_PROGRAM_POINTER;
	current_field->flag_is_pointer = 1;
  }
#line 7439 "parser.c"
    break;

  case 407: /* usage: "SIGNED-SHORT"  */
#line 2911 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_SHORT; }
#line 7445 "parser.c"
    break;

  case 408: /* usage: "SIGNED-INT"  */
#line 2912 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_INT; }
#line 7451 "parser.c"
    break;

  case 409: /* usage: "SIGNED-LONG"  */
#line 2913 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_LONG; }
#line 7457 "parser.c"
    break;

  case 410: /* usage: "UNSIGNED-SHORT"  */
#line 2914 "parser.y"
                                { current_field->usage = CB_USAGE_UNSIGNED_SHORT; }
#line 7463 "parser.c"
    break;

  case 411: /* usage: "UNSIGNED-INT"  */
#line 2915 "parser.y"
                                { current_field->usage = CB_USAGE_UNSIGNED_INT; }
#line 7469 "parser.c"
    break;

  case 412: /* usage: "UNSIGNED-LONG"  */
#line 2916 "parser.y"
                                { current_field->usage = CB_USAGE_UNSIGNED_LONG; }
#line 7475 "parser.c"
    break;

  case 413: /* usage: "BINARY-CHAR" SIGNED  */
#line 2917 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_CHAR; }
#line 7481 "parser.c"
    break;

  case 414: /* usage: "BINARY-CHAR" UNSIGNED  */
#line 2918 "parser.y"
                                { current_field->usage = CB_USAGE_UNSIGNED_CHAR; }
#line 7487 "parser.c"
    break;

  case 415: /* usage: "BINARY-CHAR"  */
#line 2919 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_CHAR; }
#line 7493 "parser.c"
    break;

  case 416: /* usage: "BINARY-SHORT" SIGNED  */
#line 2920 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_SHORT; }
#line 7499 "parser.c"
    break;

  case 417: /* usage: "BINARY-SHORT" UNSIGNED  */
#line 2921 "parser.y"
                                { current_field->usage = CB_USAGE_UNSIGNED_SHORT; }
#line 7505 "parser.c"
    break;

  case 418: /* usage: "BINARY-SHORT"  */
#line 2922 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_SHORT; }
#line 7511 "parser.c"
    break;

  case 419: /* usage: "BINARY-LONG" SIGNED  */
#line 2923 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_INT; }
#line 7517 "parser.c"
    break;

  case 420: /* usage: "BINARY-LONG" UNSIGNED  */
#line 2924 "parser.y"
                                { current_field->usage = CB_USAGE_UNSIGNED_INT; }
#line 7523 "parser.c"
    break;

  case 421: /* usage: "BINARY-LONG"  */
#line 2925 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_INT; }
#line 7529 "parser.c"
    break;

  case 422: /* usage: "BINARY-DOUBLE" SIGNED  */
#line 2926 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_LONG; }
#line 7535 "parser.c"
    break;

  case 423: /* usage: "BINARY-DOUBLE" UNSIGNED  */
#line 2927 "parser.y"
                                { current_field->usage = CB_USAGE_UNSIGNED_LONG; }
#line 7541 "parser.c"
    break;

  case 424: /* usage: "BINARY-DOUBLE"  */
#line 2928 "parser.y"
                                { current_field->usage = CB_USAGE_SIGNED_LONG; }
#line 7547 "parser.c"
    break;

  case 425: /* usage: "BINARY-C-LONG" SIGNED  */
#line 2930 "parser.y"
  {
	if (sizeof(long) == 4) {
		current_field->usage = CB_USAGE_SIGNED_INT;
	} else {
		current_field->usage = CB_USAGE_SIGNED_LONG;
	}
  }
#line 7559 "parser.c"
    break;

  case 426: /* usage: "BINARY-C-LONG" UNSIGNED  */
#line 2938 "parser.y"
  {
	if (sizeof(long) == 4) {
		current_field->usage = CB_USAGE_UNSIGNED_INT;
	} else {
		current_field->usage = CB_USAGE_UNSIGNED_LONG;
	}
  }
#line 7571 "parser.c"
    break;

  case 427: /* usage: "BINARY-C-LONG"  */
#line 2946 "parser.y"
  {
	if (sizeof(long) == 4) {
		current_field->usage = CB_USAGE_SIGNED_INT;
	} else {
		current_field->usage = CB_USAGE_SIGNED_LONG;
	}
  }
#line 7583 "parser.c"
    break;

  case 428: /* usage: NATIONAL  */
#line 2953 "parser.y"
                                { PENDING ("USAGE NATIONAL");}
#line 7589 "parser.c"
    break;

  case 429: /* sign_clause: _sign_is LEADING flag_separate  */
#line 2961 "parser.y"
  {
	current_field->flag_sign_separate = CB_INTEGER (yyvsp[0])->val;
	current_field->flag_sign_leading  = 1;
  }
#line 7598 "parser.c"
    break;

  case 430: /* sign_clause: _sign_is TRAILING flag_separate  */
#line 2966 "parser.y"
  {
	current_field->flag_sign_separate = CB_INTEGER (yyvsp[0])->val;
	current_field->flag_sign_leading  = 0;
  }
#line 7607 "parser.c"
    break;

  case 434: /* occurs_clause: OCCURS integer occurs_to_integer _times occurs_depending occurs_key_spec  */
#line 2983 "parser.y"
  {
	if (current_field->occurs_depending && !(yyvsp[-3])) {
		cb_verify (cb_odo_without_to, "ODO without TO clause");
	}
	current_field->occurs_min = yyvsp[-3] ? cb_get_int (yyvsp[-4]) : 1;
	current_field->occurs_max = yyvsp[-3] ? cb_get_int (yyvsp[-3]) : cb_get_int (yyvsp[-4]);
	current_field->indexes++;
	if (current_field->indexes > COB_MAX_SUBSCRIPTS) {
		cb_error (_("Maximum OCCURS depth exceeded"));
	}
	current_field->flag_occurs = 1;
  }
#line 7624 "parser.c"
    break;

  case 435: /* occurs_to_integer: %empty  */
#line 2998 "parser.y"
                                { yyval = NULL; }
#line 7630 "parser.c"
    break;

  case 436: /* occurs_to_integer: TO integer  */
#line 2999 "parser.y"
                                { yyval = yyvsp[0]; }
#line 7636 "parser.c"
    break;

  case 438: /* occurs_depending: DEPENDING _on reference  */
#line 3004 "parser.y"
  {
	current_field->occurs_depending = yyvsp[0];
  }
#line 7644 "parser.c"
    break;

  case 441: /* occurs_keys: occurs_key_list  */
#line 3013 "parser.y"
  {
	if (yyvsp[0]) {
		cb_tree		l;
		struct cb_key	*keys;
		int		i;
		int		nkeys;

		l = yyvsp[0];
		nkeys = cb_list_length (yyvsp[0]);
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
#line 7669 "parser.c"
    break;

  case 442: /* occurs_key: ascending_or_descending _key _is reference_list  */
#line 3037 "parser.y"
  {
	cb_tree l;

	for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = yyvsp[-3];
		if (qualifier && !CB_REFERENCE(CB_VALUE(l))->chain &&
		    strcasecmp (CB_NAME(CB_VALUE(l)), CB_NAME(qualifier))) {
			CB_REFERENCE(CB_VALUE(l))->chain = qualifier;
		}
	}
	yyval = yyvsp[0];
  }
#line 7686 "parser.c"
    break;

  case 443: /* occurs_key_list: occurs_key  */
#line 3052 "parser.y"
                                { yyval = yyvsp[0]; }
#line 7692 "parser.c"
    break;

  case 444: /* occurs_key_list: occurs_key_list occurs_key  */
#line 3053 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 7698 "parser.c"
    break;

  case 445: /* ascending_or_descending: ASCENDING  */
#line 3057 "parser.y"
                                { yyval = cb_int (COB_ASCENDING); }
#line 7704 "parser.c"
    break;

  case 446: /* ascending_or_descending: DESCENDING  */
#line 3058 "parser.y"
                                { yyval = cb_int (COB_DESCENDING); }
#line 7710 "parser.c"
    break;

  case 449: /* occurs_indexed: INDEXED _by occurs_index_list  */
#line 3065 "parser.y"
  {
	current_field->index_list = yyvsp[0];
  }
#line 7718 "parser.c"
    break;

  case 450: /* occurs_index_list: occurs_index  */
#line 3071 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 7724 "parser.c"
    break;

  case 451: /* occurs_index_list: occurs_index_list occurs_index  */
#line 3073 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 7730 "parser.c"
    break;

  case 452: /* occurs_index: "Identifier"  */
#line 3078 "parser.y"
  {
	yyval = cb_build_index (yyvsp[0], cb_int1, 1, current_field);
  }
#line 7738 "parser.c"
    break;

  case 453: /* justified_clause: JUSTIFIED _right  */
#line 3087 "parser.y"
                                { current_field->flag_justified = 1; }
#line 7744 "parser.c"
    break;

  case 454: /* synchronized_clause: SYNCHRONIZED left_or_right  */
#line 3094 "parser.y"
                                { current_field->flag_synchronized = 1; }
#line 7750 "parser.c"
    break;

  case 458: /* blank_clause: BLANK _when ZERO  */
#line 3106 "parser.y"
                                { current_field->flag_blank_zero = 1; }
#line 7756 "parser.c"
    break;

  case 459: /* based_clause: BASED  */
#line 3114 "parser.y"
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
#line 7780 "parser.c"
    break;

  case 460: /* value_clause: VALUE _is literal  */
#line 3138 "parser.y"
                                { current_field->values = cb_list_init (yyvsp[0]); }
#line 7786 "parser.c"
    break;

  case 461: /* $@24: %empty  */
#line 3142 "parser.y"
                                { current_field->values = yyvsp[0]; }
#line 7792 "parser.c"
    break;

  case 463: /* value_item_list: value_item  */
#line 3147 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 7798 "parser.c"
    break;

  case 464: /* value_item_list: value_item_list value_item  */
#line 3148 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 7804 "parser.c"
    break;

  case 465: /* value_item: literal  */
#line 3152 "parser.y"
                                { yyval = yyvsp[0]; }
#line 7810 "parser.c"
    break;

  case 466: /* value_item: literal THRU literal  */
#line 3153 "parser.y"
                                { yyval = cb_build_pair (yyvsp[-2], yyvsp[0]); }
#line 7816 "parser.c"
    break;

  case 468: /* false_is: "FALSE" _is literal  */
#line 3158 "parser.y"
  {
	if (current_field->level != 88) {
		cb_error (_("FALSE clause only allowed for 88 level"));
	}
	current_field->false_88 = cb_list_init (yyvsp[0]);
  }
#line 7827 "parser.c"
    break;

  case 469: /* renames_clause: RENAMES qualified_word  */
#line 3171 "parser.y"
  {
	if (cb_ref (yyvsp[0]) != cb_error_node) {
		if (CB_FIELD (cb_ref (yyvsp[0]))->level == 01 ||
		    CB_FIELD (cb_ref (yyvsp[0]))->level > 50) {
			cb_error (_("RENAMES may not reference a level 01 or > 50"));
		} else {
			current_field->redefines = CB_FIELD (cb_ref (yyvsp[0]));
			current_field->pic = current_field->redefines->pic;
		}
	}
  }
#line 7843 "parser.c"
    break;

  case 470: /* renames_clause: RENAMES qualified_word THRU qualified_word  */
#line 3183 "parser.y"
  {
	if (cb_ref (yyvsp[-2]) != cb_error_node && cb_ref (yyvsp[0]) != cb_error_node) {
		if (CB_FIELD (cb_ref (yyvsp[-2]))->level == 01 ||
		    CB_FIELD (cb_ref (yyvsp[-2]))->level > 50) {
			cb_error (_("RENAMES may not reference a level 01 or > 50"));
		} else if (CB_FIELD (cb_ref (yyvsp[0]))->level == 01 ||
		    CB_FIELD (cb_ref (yyvsp[0]))->level > 50) {
			cb_error (_("RENAMES may not reference a level 01 or > 50"));
		} else {
			current_field->redefines = CB_FIELD (cb_ref (yyvsp[-2]));
			current_field->rename_thru = CB_FIELD (cb_ref (yyvsp[0]));
		}
	}
  }
#line 7862 "parser.c"
    break;

  case 471: /* any_length_clause: ANY LENGTH  */
#line 3203 "parser.y"
  {
	if (current_field->flag_item_based) {
		cb_error (_("BASED and ANY LENGTH are mutually exclusive"));
	} else {
		current_field->flag_any_length = 1;
	}
  }
#line 7874 "parser.c"
    break;

  case 473: /* $@25: %empty  */
#line 3218 "parser.y"
  {
	current_storage = CB_STORAGE_LOCAL;
	if (current_program->nested_level) {
		cb_error (_("LOCAL-STORAGE not allowed in nested programs"));
	}
  }
#line 7885 "parser.c"
    break;

  case 474: /* local_storage_section: "LOCAL-STORAGE" SECTION '.' $@25 record_description_list  */
#line 3225 "parser.y"
  {
	if (yyvsp[0]) {
		current_program->local_storage = CB_FIELD (yyvsp[0]);
	}
  }
#line 7895 "parser.c"
    break;

  case 476: /* $@26: %empty  */
#line 3238 "parser.y"
                                { current_storage = CB_STORAGE_LINKAGE; }
#line 7901 "parser.c"
    break;

  case 477: /* linkage_section: LINKAGE SECTION '.' $@26 record_description_list  */
#line 3240 "parser.y"
  {
	if (yyvsp[0]) {
		current_program->linkage_storage = CB_FIELD (yyvsp[0]);
	}
  }
#line 7911 "parser.c"
    break;

  case 479: /* $@27: %empty  */
#line 3253 "parser.y"
  {
	cb_error (_("REPORT SECTION not supported"));
	current_storage = CB_STORAGE_REPORT;
  }
#line 7920 "parser.c"
    break;

  case 486: /* report_description_options: %empty  */
#line 3286 "parser.y"
  {
	cb_warning (_("Report description using defaults"));
  }
#line 7928 "parser.c"
    break;

  case 488: /* report_description_option: _is GLOBAL  */
#line 3294 "parser.y"
  {
	cb_error (_("GLOBAL is not allowed with RD"));
  }
#line 7936 "parser.c"
    break;

  case 497: /* identifier_list: identifier  */
#line 3317 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 7942 "parser.c"
    break;

  case 498: /* identifier_list: identifier_list identifier  */
#line 3318 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 7948 "parser.c"
    break;

  case 520: /* report_group_option: type_clause  */
#line 3374 "parser.y"
              { cb_warning (_("looking for Report line TYPE")); }
#line 7954 "parser.c"
    break;

  case 571: /* $@28: %empty  */
#line 3479 "parser.y"
                                { current_storage = CB_STORAGE_SCREEN; }
#line 7960 "parser.c"
    break;

  case 572: /* screen_section: SCREEN SECTION '.' $@28  */
#line 3480 "parser.y"
  {
	cb_error (_("SCREEN SECTION is not supported"));
  }
#line 7968 "parser.c"
    break;

  case 574: /* $@29: %empty  */
#line 3491 "parser.y"
  {
	current_section = NULL;
	current_paragraph = NULL;
	cb_define_system_name ("CONSOLE");
	cb_define_system_name ("SYSIN");
	cb_define_system_name ("SYSOUT");
	cb_define_system_name ("SYSERR");
	cb_set_in_procedure ();
  }
#line 7982 "parser.c"
    break;

  case 575: /* $@30: %empty  */
#line 3501 "parser.y"
  {
	if (current_program->flag_main && !current_program->flag_chained && yyvsp[-4]) {
		cb_error (_("Executable program requested but PROCEDURE/ENTRY has USING clause"));
	}
	emit_entry (current_program->program_id, 0, yyvsp[-4]); /* main entry point */
	if (current_program->source_name) {
		emit_entry (current_program->source_name, 1, yyvsp[-4]);
	}
  }
#line 7996 "parser.c"
    break;

  case 576: /* procedure_division: PROCEDURE DIVISION procedure_using_chaining procedure_returning '.' $@29 procedure_declaratives $@30 procedure_list  */
#line 3511 "parser.y"
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
#line 8015 "parser.c"
    break;

  case 577: /* procedure_using_chaining: %empty  */
#line 3528 "parser.y"
                                { yyval = NULL; }
#line 8021 "parser.c"
    break;

  case 578: /* $@31: %empty  */
#line 3530 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 8030 "parser.c"
    break;

  case 579: /* procedure_using_chaining: USING $@31 procedure_param_list  */
#line 3534 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8036 "parser.c"
    break;

  case 580: /* $@32: %empty  */
#line 3536 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
	current_program->flag_chained = 1;
  }
#line 8045 "parser.c"
    break;

  case 581: /* procedure_using_chaining: CHAINING $@32 procedure_param_list  */
#line 3540 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8051 "parser.c"
    break;

  case 582: /* procedure_param_list: procedure_param  */
#line 3544 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8057 "parser.c"
    break;

  case 583: /* procedure_param_list: procedure_param_list procedure_param  */
#line 3546 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 8063 "parser.c"
    break;

  case 584: /* procedure_param: procedure_type size_optional procedure_optional "Identifier"  */
#line 3551 "parser.y"
  {
	yyval = cb_build_pair (cb_int (call_mode), cb_build_identifier (yyvsp[0]));
	CB_SIZES (yyval) = size_mode;
  }
#line 8072 "parser.c"
    break;

  case 586: /* procedure_type: _by REFERENCE  */
#line 3560 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 8080 "parser.c"
    break;

  case 587: /* procedure_type: _by VALUE  */
#line 3564 "parser.y"
  {
	if (current_program->flag_chained) {
		cb_error (_("BY VALUE not allowed in CHAINED program"));
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 8092 "parser.c"
    break;

  case 589: /* size_optional: SIZE _is AUTO  */
#line 3576 "parser.y"
  {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO;
	}
  }
#line 8104 "parser.c"
    break;

  case 590: /* size_optional: SIZE _is DEFAULT  */
#line 3584 "parser.y"
  {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_4;
	}
  }
#line 8116 "parser.c"
    break;

  case 591: /* size_optional: UNSIGNED SIZE _is integer  */
#line 3592 "parser.y"
  {
	unsigned char *s = CB_LITERAL (yyvsp[0])->data;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL (yyvsp[0])->size != 1) {
		cb_error_x (yyvsp[0], _("Invalid value for SIZE"));
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
			cb_error_x (yyvsp[0], _("Invalid value for SIZE"));
			break;
		}
	}
  }
#line 8149 "parser.c"
    break;

  case 592: /* size_optional: SIZE _is integer  */
#line 3621 "parser.y"
  {
	unsigned char *s = CB_LITERAL (yyvsp[0])->data;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL (yyvsp[0])->size != 1) {
		cb_error_x (yyvsp[0], _("Invalid value for SIZE"));
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
			cb_error_x (yyvsp[0], _("Invalid value for SIZE"));
			break;
		}
	}
  }
#line 8182 "parser.c"
    break;

  case 594: /* procedure_optional: OPTIONAL  */
#line 3654 "parser.y"
  {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
	}
  }
#line 8192 "parser.c"
    break;

  case 595: /* procedure_returning: %empty  */
#line 3663 "parser.y"
  {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("RETURNING clause is required for a FUNCTION"));
	}
  }
#line 8202 "parser.c"
    break;

  case 596: /* procedure_returning: RETURNING "Identifier"  */
#line 3669 "parser.y"
  {
	if (cb_ref (yyvsp[0]) != cb_error_node) {
		current_program->returning = yyvsp[0];
		if (cb_field (yyvsp[0])->storage != CB_STORAGE_LINKAGE) {
			cb_error (_("RETURNING item is not defined in LINKAGE SECTION"));
		}
	}
  }
#line 8215 "parser.c"
    break;

  case 598: /* $@33: %empty  */
#line 3680 "parser.y"
                        { in_declaratives = 1; }
#line 8221 "parser.c"
    break;

  case 599: /* procedure_declaratives: DECLARATIVES '.' $@33 procedure_list END DECLARATIVES '.'  */
#line 3683 "parser.y"
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
#line 8243 "parser.c"
    break;

  case 605: /* procedure: statements '.'  */
#line 3716 "parser.y"
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
#line 8263 "parser.c"
    break;

  case 606: /* procedure: error  */
#line 3732 "parser.y"
  {
	check_unreached = 0;
  }
#line 8271 "parser.c"
    break;

  case 607: /* section_header: section_name SECTION opt_segment '.'  */
#line 3744 "parser.y"
  {
	non_const_word = 0;
	check_unreached = 0;
	if (yyvsp[-3] == cb_error_node) {
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
	current_section = CB_LABEL (cb_build_label (yyvsp[-3], NULL));
	current_section->is_section = 1;
	current_paragraph = NULL;
	emit_statement (CB_TREE (current_section));
  }
#line 8303 "parser.c"
    break;

  case 608: /* paragraph_header: "Identifier" '.'  */
#line 3775 "parser.y"
  {
	cb_tree label;

	non_const_word = 0;
	check_unreached = 0;
	yyval = cb_build_section_name (yyvsp[-1], 1);
	/* if ($1 == cb_error_node) */
	if (yyval == cb_error_node) {
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
	current_paragraph = CB_LABEL (cb_build_label (yyval, current_section));
	if (current_section) {
		current_section->children =
			cb_cons (CB_TREE (current_paragraph), current_section->children);
	}
	emit_statement (CB_TREE (current_paragraph));
  }
#line 8341 "parser.c"
    break;

  case 609: /* invalid_statement: section_name  */
#line 3812 "parser.y"
  {
	non_const_word = 0;
	check_unreached = 0;
	if (yyvsp[0] != cb_error_node) {
		cb_error_x (yyvsp[0], _("Unknown statement '%s'"), CB_NAME (yyvsp[0]));
	}
	YYERROR;
  }
#line 8354 "parser.c"
    break;

  case 610: /* section_name: "Identifier"  */
#line 3823 "parser.y"
                                { yyval = cb_build_section_name (yyvsp[0], 0); }
#line 8360 "parser.c"
    break;

  case 612: /* opt_segment: "Literal"  */
#line 3827 "parser.y"
                                { /* ignore */ }
#line 8366 "parser.c"
    break;

  case 613: /* @34: %empty  */
#line 3836 "parser.y"
  {
	yyval = current_program->exec_list;
	current_program->exec_list = NULL;
  }
#line 8375 "parser.c"
    break;

  case 614: /* @35: %empty  */
#line 3840 "parser.y"
  {
	yyval = CB_TREE (current_statement);
	current_statement = NULL;
  }
#line 8384 "parser.c"
    break;

  case 615: /* statement_list: @34 @35 statements  */
#line 3845 "parser.y"
  {
	yyval = cb_list_reverse (current_program->exec_list);
	current_program->exec_list = yyvsp[-2];
	current_statement = CB_STATEMENT (yyvsp[-1]);
  }
#line 8394 "parser.c"
    break;

  case 616: /* statements: %empty  */
#line 3853 "parser.y"
  {
	cb_tree label;

	if (!current_section) {
		label = cb_build_reference ("MAIN");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->is_section = 1;
		emit_statement (CB_TREE (current_section));
	}
	if (!current_paragraph) {
		const char* suffix = "_SECTION__DEFAULT_PARAGRAPH";
		char *label_name = malloc(strlen((char*)current_section->name) + strlen(suffix) + 1);
		sprintf(label_name, "%s%s", current_section->name, suffix);
		label = cb_build_reference (label_name);
		current_paragraph = CB_LABEL (cb_build_label (label, NULL));
		emit_statement (CB_TREE (current_paragraph));
		current_section->children =
			cb_cons (CB_TREE (current_paragraph), current_section->children);
	}
  }
#line 8419 "parser.c"
    break;

  case 667: /* statement: "NEXT SENTENCE"  */
#line 3928 "parser.y"
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
#line 8437 "parser.c"
    break;

  case 668: /* $@36: %empty  */
#line 3950 "parser.y"
  {
	BEGIN_STATEMENT ("ACCEPT", TERM_ACCEPT);
	dispattrs = 0;
	fgc = NULL;
	bgc = NULL;
	scroll = NULL;
  }
#line 8449 "parser.c"
    break;

  case 670: /* accept_body: identifier opt_at_line_column opt_accp_attr on_accp_exception  */
#line 3963 "parser.y"
  {
	cb_emit_accept (yyvsp[-3], yyvsp[-2], fgc, bgc, scroll, dispattrs);
  }
#line 8457 "parser.c"
    break;

  case 671: /* accept_body: identifier FROM ESCAPE KEY  */
#line 3967 "parser.y"
  {
	PENDING ("ACCEPT .. FROM ESCAPE KEY");
  }
#line 8465 "parser.c"
    break;

  case 672: /* accept_body: identifier FROM LINES  */
#line 3971 "parser.y"
  {
	cb_emit_accept_line_or_col (yyvsp[-2], 0);
  }
#line 8473 "parser.c"
    break;

  case 673: /* accept_body: identifier FROM COLUMNS  */
#line 3975 "parser.y"
  {
	cb_emit_accept_line_or_col (yyvsp[-2], 1);
  }
#line 8481 "parser.c"
    break;

  case 674: /* accept_body: identifier FROM DATE  */
#line 3979 "parser.y"
  {
	cb_emit_accept_date (yyvsp[-2]);
  }
#line 8489 "parser.c"
    break;

  case 675: /* accept_body: identifier FROM DATE YYYYMMDD  */
#line 3983 "parser.y"
  {
	cb_emit_accept_date_yyyymmdd (yyvsp[-3]);
  }
#line 8497 "parser.c"
    break;

  case 676: /* accept_body: identifier FROM DAY  */
#line 3987 "parser.y"
  {
	cb_emit_accept_day (yyvsp[-2]);
  }
#line 8505 "parser.c"
    break;

  case 677: /* accept_body: identifier FROM DAY YYYYDDD  */
#line 3991 "parser.y"
  {
	cb_emit_accept_day_yyyyddd (yyvsp[-3]);
  }
#line 8513 "parser.c"
    break;

  case 678: /* accept_body: identifier FROM "DAY-OF-WEEK"  */
#line 3995 "parser.y"
  {
	cb_emit_accept_day_of_week (yyvsp[-2]);
  }
#line 8521 "parser.c"
    break;

  case 679: /* accept_body: identifier FROM TIME  */
#line 3999 "parser.y"
  {
	cb_emit_accept_time (yyvsp[-2]);
  }
#line 8529 "parser.c"
    break;

  case 680: /* accept_body: identifier FROM "COMMAND-LINE"  */
#line 4003 "parser.y"
  {
	cb_emit_accept_command_line (yyvsp[-2]);
  }
#line 8537 "parser.c"
    break;

  case 681: /* accept_body: identifier FROM "ENVIRONMENT-VALUE" on_accp_exception  */
#line 4007 "parser.y"
  {
	cb_emit_accept_environment (yyvsp[-3]);
  }
#line 8545 "parser.c"
    break;

  case 682: /* accept_body: identifier FROM ENVIRONMENT simple_value on_accp_exception  */
#line 4011 "parser.y"
  { 
	cb_emit_get_environment (yyvsp[-1], yyvsp[-4]);
  }
#line 8553 "parser.c"
    break;

  case 683: /* accept_body: identifier FROM "ARGUMENT-NUMBER"  */
#line 4015 "parser.y"
  {
	cb_emit_accept_arg_number (yyvsp[-2]);
  }
#line 8561 "parser.c"
    break;

  case 684: /* accept_body: identifier FROM "ARGUMENT-VALUE" on_accp_exception  */
#line 4019 "parser.y"
  {
	cb_emit_accept_arg_value (yyvsp[-3]);
  }
#line 8569 "parser.c"
    break;

  case 685: /* accept_body: identifier FROM mnemonic_name  */
#line 4023 "parser.y"
  {
	cb_emit_accept_mnemonic (yyvsp[-2], yyvsp[0]);
  }
#line 8577 "parser.c"
    break;

  case 686: /* accept_body: identifier FROM "Identifier"  */
#line 4027 "parser.y"
  {
	cb_emit_accept_name (yyvsp[-2], yyvsp[0]);
  }
#line 8585 "parser.c"
    break;

  case 687: /* opt_at_line_column: %empty  */
#line 4033 "parser.y"
                                { yyval = NULL; }
#line 8591 "parser.c"
    break;

  case 688: /* opt_at_line_column: _at line_number column_number  */
#line 4034 "parser.y"
                                { yyval = cb_build_pair (yyvsp[-1], yyvsp[0]); }
#line 8597 "parser.c"
    break;

  case 689: /* opt_at_line_column: _at column_number line_number  */
#line 4035 "parser.y"
                                { yyval = cb_build_pair (yyvsp[0], yyvsp[-1]); }
#line 8603 "parser.c"
    break;

  case 690: /* opt_at_line_column: _at line_number  */
#line 4036 "parser.y"
                                { yyval = cb_build_pair (yyvsp[0], NULL); }
#line 8609 "parser.c"
    break;

  case 691: /* opt_at_line_column: _at column_number  */
#line 4037 "parser.y"
                                { yyval = cb_build_pair (NULL, yyvsp[0]); }
#line 8615 "parser.c"
    break;

  case 692: /* opt_at_line_column: AT simple_value  */
#line 4038 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8621 "parser.c"
    break;

  case 693: /* line_number: LINE _number id_or_lit  */
#line 4042 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8627 "parser.c"
    break;

  case 694: /* column_number: COLUMN _number id_or_lit  */
#line 4046 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8633 "parser.c"
    break;

  case 695: /* column_number: POSITION _number id_or_lit  */
#line 4047 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8639 "parser.c"
    break;

  case 700: /* accp_attr: BELL  */
#line 4060 "parser.y"
                { dispattrs |= COB_SCREEN_BELL; }
#line 8645 "parser.c"
    break;

  case 701: /* accp_attr: BLINK  */
#line 4061 "parser.y"
                { dispattrs |= COB_SCREEN_BLINK; }
#line 8651 "parser.c"
    break;

  case 702: /* accp_attr: HIGHLIGHT  */
#line 4062 "parser.y"
                { dispattrs |= COB_SCREEN_HIGHLIGHT; }
#line 8657 "parser.c"
    break;

  case 703: /* accp_attr: LOWLIGHT  */
#line 4063 "parser.y"
                { dispattrs |= COB_SCREEN_LOWLIGHT; }
#line 8663 "parser.c"
    break;

  case 704: /* accp_attr: "REVERSE-VIDEO"  */
#line 4064 "parser.y"
                { dispattrs |= COB_SCREEN_REVERSE; }
#line 8669 "parser.c"
    break;

  case 705: /* accp_attr: UNDERLINE  */
#line 4065 "parser.y"
                { dispattrs |= COB_SCREEN_UNDERLINE; }
#line 8675 "parser.c"
    break;

  case 706: /* accp_attr: OVERLINE  */
#line 4066 "parser.y"
                { dispattrs |= COB_SCREEN_OVERLINE; }
#line 8681 "parser.c"
    break;

  case 707: /* accp_attr: "FOREGROUND-COLOR" _is num_id_or_lit  */
#line 4068 "parser.y"
  {
	fgc = yyvsp[0];
  }
#line 8689 "parser.c"
    break;

  case 708: /* accp_attr: "BACKGROUND-COLOR" _is num_id_or_lit  */
#line 4072 "parser.y"
  {
	bgc = yyvsp[0];
  }
#line 8697 "parser.c"
    break;

  case 709: /* accp_attr: SCROLL UP _opt_scroll_lines  */
#line 4076 "parser.y"
  {
	scroll = yyvsp[0];
  }
#line 8705 "parser.c"
    break;

  case 710: /* accp_attr: SCROLL DOWN _opt_scroll_lines  */
#line 4080 "parser.y"
  {
	dispattrs |= COB_SCREEN_SCROLL_DOWN;
	scroll = yyvsp[0];
  }
#line 8714 "parser.c"
    break;

  case 711: /* accp_attr: AUTO  */
#line 4084 "parser.y"
                { dispattrs |= COB_SCREEN_AUTO; }
#line 8720 "parser.c"
    break;

  case 712: /* accp_attr: FULL  */
#line 4085 "parser.y"
                { dispattrs |= COB_SCREEN_FULL; }
#line 8726 "parser.c"
    break;

  case 713: /* accp_attr: REQUIRED  */
#line 4086 "parser.y"
                { dispattrs |= COB_SCREEN_REQUIRED; }
#line 8732 "parser.c"
    break;

  case 714: /* accp_attr: SECURE  */
#line 4087 "parser.y"
                { dispattrs |= COB_SCREEN_SECURE; }
#line 8738 "parser.c"
    break;

  case 715: /* accp_attr: UPDATE  */
#line 4088 "parser.y"
                { dispattrs |= COB_SCREEN_UPDATE; }
#line 8744 "parser.c"
    break;

  case 716: /* accp_attr: PROMPT  */
#line 4089 "parser.y"
                { dispattrs |= COB_SCREEN_PROMPT; }
#line 8750 "parser.c"
    break;

  case 717: /* end_accept: %empty  */
#line 4093 "parser.y"
                                { terminator_warning (TERM_ACCEPT); }
#line 8756 "parser.c"
    break;

  case 718: /* end_accept: "END-ACCEPT"  */
#line 4094 "parser.y"
                                { terminator_clear (TERM_ACCEPT); }
#line 8762 "parser.c"
    break;

  case 719: /* $@37: %empty  */
#line 4103 "parser.y"
                                { BEGIN_STATEMENT ("ADD", TERM_ADD); }
#line 8768 "parser.c"
    break;

  case 721: /* add_body: x_list TO arithmetic_x_list on_size_error  */
#line 4110 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], '+', cb_build_binary_list (yyvsp[-3], '+'));
  }
#line 8776 "parser.c"
    break;

  case 722: /* add_body: x_list add_to GIVING arithmetic_x_list on_size_error  */
#line 4114 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_list (yyvsp[-4], '+'));
  }
#line 8784 "parser.c"
    break;

  case 723: /* add_body: CORRESPONDING identifier TO identifier flag_rounded on_size_error  */
#line 4118 "parser.y"
  {
	cb_emit_corresponding (cb_build_add, yyvsp[-2], yyvsp[-4], yyvsp[-1]);
  }
#line 8792 "parser.c"
    break;

  case 725: /* add_to: TO x  */
#line 4124 "parser.y"
                                { cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 8798 "parser.c"
    break;

  case 726: /* end_add: %empty  */
#line 4128 "parser.y"
                                { terminator_warning (TERM_ADD); }
#line 8804 "parser.c"
    break;

  case 727: /* end_add: "END-ADD"  */
#line 4129 "parser.y"
                                { terminator_clear (TERM_ADD); }
#line 8810 "parser.c"
    break;

  case 728: /* $@38: %empty  */
#line 4138 "parser.y"
                                { BEGIN_STATEMENT ("ALLOCATE", 0); }
#line 8816 "parser.c"
    break;

  case 730: /* allocate_body: "Identifier" flag_initialized allocate_returning  */
#line 4144 "parser.y"
  {
	cb_emit_allocate (yyvsp[-2], yyvsp[0], NULL, yyvsp[-1]);
  }
#line 8824 "parser.c"
    break;

  case 731: /* allocate_body: expr CHARACTERS flag_initialized RETURNING target_x  */
#line 4148 "parser.y"
  {
	cb_emit_allocate (NULL, yyvsp[0], yyvsp[-4], yyvsp[-2]);
  }
#line 8832 "parser.c"
    break;

  case 732: /* allocate_returning: %empty  */
#line 4154 "parser.y"
                                { yyval = NULL; }
#line 8838 "parser.c"
    break;

  case 733: /* allocate_returning: RETURNING target_x  */
#line 4155 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8844 "parser.c"
    break;

  case 734: /* alter_statement: ALTER alter_options  */
#line 4165 "parser.y"
  {
	cb_error (_("ALTER statement is obsolete and unsupported"));
  }
#line 8852 "parser.c"
    break;

  case 739: /* $@39: %empty  */
#line 4183 "parser.y"
                                { BEGIN_STATEMENT ("CALL", TERM_CALL); }
#line 8858 "parser.c"
    break;

  case 740: /* call_statement: CALL $@39 id_or_lit_or_func call_using call_returning call_on_exception call_not_on_exception end_call  */
#line 4187 "parser.y"
  {
	cb_emit_call (yyvsp[-5], yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1]);
  }
#line 8866 "parser.c"
    break;

  case 741: /* call_using: %empty  */
#line 4193 "parser.y"
                                { yyval = NULL; }
#line 8872 "parser.c"
    break;

  case 742: /* $@40: %empty  */
#line 4195 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 8881 "parser.c"
    break;

  case 743: /* call_using: USING $@40 call_param_list  */
#line 4199 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8887 "parser.c"
    break;

  case 744: /* call_param_list: call_param  */
#line 4203 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8893 "parser.c"
    break;

  case 745: /* call_param_list: call_param_list call_param  */
#line 4205 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 8899 "parser.c"
    break;

  case 746: /* call_param: call_type OMITTED  */
#line 4210 "parser.y"
  {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OMITTED only allowed with BY REFERENCE"));
	}
	yyval = cb_build_pair (cb_int (call_mode), cb_null);
  }
#line 8910 "parser.c"
    break;

  case 747: /* call_param: call_type size_optional x  */
#line 4217 "parser.y"
  {
	yyval = cb_build_pair (cb_int (call_mode), yyvsp[0]);
	CB_SIZES (yyval) = size_mode;
  }
#line 8919 "parser.c"
    break;

  case 749: /* call_type: _by REFERENCE  */
#line 4226 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 8927 "parser.c"
    break;

  case 750: /* call_type: _by CONTENT  */
#line 4230 "parser.y"
  {
	if (current_program->flag_chained) {
		cb_error (_("BY CONTENT not allowed in CHAINED program"));
	} else {
		call_mode = CB_CALL_BY_CONTENT;
	}
  }
#line 8939 "parser.c"
    break;

  case 751: /* call_type: _by VALUE  */
#line 4238 "parser.y"
  {
	if (current_program->flag_chained) {
		cb_error (_("BY VALUE not allowed in CHAINED program"));
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 8951 "parser.c"
    break;

  case 752: /* call_returning: %empty  */
#line 4248 "parser.y"
                                { yyval = NULL; }
#line 8957 "parser.c"
    break;

  case 753: /* call_returning: RETURNING identifier  */
#line 4249 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8963 "parser.c"
    break;

  case 754: /* call_returning: GIVING identifier  */
#line 4250 "parser.y"
                                { yyval = yyvsp[0]; }
#line 8969 "parser.c"
    break;

  case 755: /* call_on_exception: %empty  */
#line 4255 "parser.y"
  {
	yyval = NULL;
  }
#line 8977 "parser.c"
    break;

  case 756: /* $@41: %empty  */
#line 4259 "parser.y"
  {
	check_unreached = 0;
  }
#line 8985 "parser.c"
    break;

  case 757: /* call_on_exception: exception_or_overflow $@41 statement_list  */
#line 4263 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 8993 "parser.c"
    break;

  case 758: /* call_not_on_exception: %empty  */
#line 4270 "parser.y"
  {
	yyval = NULL;
  }
#line 9001 "parser.c"
    break;

  case 759: /* $@42: %empty  */
#line 4274 "parser.y"
  {
	check_unreached = 0;
  }
#line 9009 "parser.c"
    break;

  case 760: /* call_not_on_exception: not_exception_or_overflow $@42 statement_list  */
#line 4278 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 9017 "parser.c"
    break;

  case 761: /* end_call: %empty  */
#line 4284 "parser.y"
                                { terminator_warning (TERM_CALL); }
#line 9023 "parser.c"
    break;

  case 762: /* end_call: "END-CALL"  */
#line 4285 "parser.y"
                                { terminator_clear (TERM_CALL); }
#line 9029 "parser.c"
    break;

  case 763: /* $@43: %empty  */
#line 4294 "parser.y"
                                { BEGIN_STATEMENT ("CANCEL", 0); }
#line 9035 "parser.c"
    break;

  case 766: /* cancel_list: cancel_list id_or_lit  */
#line 4300 "parser.y"
  {
	cb_emit_cancel (yyvsp[0]);
  }
#line 9043 "parser.c"
    break;

  case 767: /* cancel_list: ALL  */
#line 4304 "parser.y"
  {
	cb_emit_cancel_all ();
  }
#line 9051 "parser.c"
    break;

  case 768: /* $@44: %empty  */
#line 4315 "parser.y"
                                { BEGIN_STATEMENT ("CLOSE", 0); }
#line 9057 "parser.c"
    break;

  case 771: /* close_list: close_list file_name close_option  */
#line 4322 "parser.y"
  {
	BEGIN_IMPLICIT_STATEMENT (yyvsp[-1]);
	if (yyvsp[-1] != cb_error_node) {
		cb_emit_close (yyvsp[-1], yyvsp[0]);
	}
  }
#line 9068 "parser.c"
    break;

  case 772: /* close_option: %empty  */
#line 4331 "parser.y"
                                { yyval = cb_int (COB_CLOSE_NORMAL); }
#line 9074 "parser.c"
    break;

  case 773: /* close_option: reel_or_unit  */
#line 4332 "parser.y"
                                { yyval = cb_int (COB_CLOSE_UNIT); }
#line 9080 "parser.c"
    break;

  case 774: /* close_option: reel_or_unit _for REMOVAL  */
#line 4333 "parser.y"
                                { yyval = cb_int (COB_CLOSE_UNIT_REMOVAL); }
#line 9086 "parser.c"
    break;

  case 775: /* close_option: _with NO REWIND  */
#line 4334 "parser.y"
                                { yyval = cb_int (COB_CLOSE_NO_REWIND); }
#line 9092 "parser.c"
    break;

  case 776: /* close_option: _with LOCK  */
#line 4335 "parser.y"
                                { yyval = cb_int (COB_CLOSE_LOCK); }
#line 9098 "parser.c"
    break;

  case 779: /* $@45: %empty  */
#line 4346 "parser.y"
                                { BEGIN_STATEMENT ("COMPUTE", TERM_COMPUTE); }
#line 9104 "parser.c"
    break;

  case 781: /* compute_body: arithmetic_x_list comp_equal expr on_size_error  */
#line 4353 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-3], 0, yyvsp[-1]);
  }
#line 9112 "parser.c"
    break;

  case 782: /* end_compute: %empty  */
#line 4359 "parser.y"
                                { terminator_warning (TERM_COMPUTE); }
#line 9118 "parser.c"
    break;

  case 783: /* end_compute: "END-COMPUTE"  */
#line 4360 "parser.y"
                                { terminator_clear (TERM_COMPUTE); }
#line 9124 "parser.c"
    break;

  case 786: /* commit_statement: COMMIT  */
#line 4371 "parser.y"
  {
	BEGIN_STATEMENT ("COMMIT", 0);
	cb_emit_commit ();
  }
#line 9133 "parser.c"
    break;

  case 787: /* continue_statement: CONTINUE  */
#line 4384 "parser.y"
  {
	BEGIN_STATEMENT ("CONTINUE", 0);
	cb_emit_continue ();
  }
#line 9142 "parser.c"
    break;

  case 788: /* $@46: %empty  */
#line 4396 "parser.y"
                                { BEGIN_STATEMENT ("DELETE", TERM_DELETE); }
#line 9148 "parser.c"
    break;

  case 789: /* delete_statement: DELETE $@46 file_name _record opt_invalid_key end_delete  */
#line 4399 "parser.y"
  {
	if (yyvsp[-3] != cb_error_node) {
		cb_emit_delete (yyvsp[-3]);
	}
  }
#line 9158 "parser.c"
    break;

  case 790: /* end_delete: %empty  */
#line 4407 "parser.y"
                                { terminator_warning (TERM_DELETE); }
#line 9164 "parser.c"
    break;

  case 791: /* end_delete: "END-DELETE"  */
#line 4408 "parser.y"
                                { terminator_clear (TERM_DELETE); }
#line 9170 "parser.c"
    break;

  case 792: /* $@47: %empty  */
#line 4417 "parser.y"
                                  { BEGIN_STATEMENT ("DELETE-FILE", 0); }
#line 9176 "parser.c"
    break;

  case 793: /* delete_file_statement: DELETE $@47 "FILE" file_name_list  */
#line 4419 "parser.y"
  {
	cb_tree l;
	for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) != cb_error_node) {
			BEGIN_IMPLICIT_STATEMENT (l);
			cb_emit_delete_file (CB_VALUE (l));
		}
	}
  }
#line 9190 "parser.c"
    break;

  case 794: /* $@48: %empty  */
#line 4437 "parser.y"
  {
	BEGIN_STATEMENT ("DISPLAY", TERM_DISPLAY);
	dispattrs = 0;
	fgc = NULL;
	bgc = NULL;
	scroll = NULL;
  }
#line 9202 "parser.c"
    break;

  case 796: /* display_body: id_or_lit "UPON ENVIRONMENT-NAME" on_disp_exception  */
#line 4450 "parser.y"
  {
	cb_emit_env_name (yyvsp[-2]);
  }
#line 9210 "parser.c"
    break;

  case 797: /* display_body: id_or_lit "UPON ENVIRONMENT-VALUE" on_disp_exception  */
#line 4454 "parser.y"
  {
	cb_emit_env_value (yyvsp[-2]);
  }
#line 9218 "parser.c"
    break;

  case 798: /* display_body: id_or_lit "UPON ARGUMENT-NUMBER" on_disp_exception  */
#line 4458 "parser.y"
  {
	cb_emit_arg_number (yyvsp[-2]);
  }
#line 9226 "parser.c"
    break;

  case 799: /* display_body: id_or_lit "UPON COMMAND-LINE" on_disp_exception  */
#line 4462 "parser.y"
  {
	cb_emit_command_line (yyvsp[-2]);
  }
#line 9234 "parser.c"
    break;

  case 800: /* display_body: x_list opt_at_line_column with_clause on_disp_exception  */
#line 4466 "parser.y"
  {
	cb_emit_display (yyvsp[-3], cb_int0, yyvsp[-1], yyvsp[-2], fgc, bgc, scroll, dispattrs);
  }
#line 9242 "parser.c"
    break;

  case 801: /* display_body: x_list opt_at_line_column UPON mnemonic_name with_clause on_disp_exception  */
#line 4470 "parser.y"
  {
	cb_emit_display_mnemonic (yyvsp[-5], yyvsp[-2], yyvsp[-1], yyvsp[-4], fgc, bgc, scroll, dispattrs);
  }
#line 9250 "parser.c"
    break;

  case 802: /* display_body: x_list opt_at_line_column UPON "Identifier" with_clause on_disp_exception  */
#line 4474 "parser.y"
  {
	cb_tree word = cb_build_display_upon_direct (yyvsp[-2]);
	cb_emit_display (yyvsp[-5], word, yyvsp[-1], yyvsp[-4], fgc, bgc, scroll, dispattrs);
  }
#line 9259 "parser.c"
    break;

  case 803: /* display_body: x_list opt_at_line_column UPON PRINTER with_clause on_disp_exception  */
#line 4479 "parser.y"
  {
	cb_emit_display (yyvsp[-5], cb_int0, yyvsp[-1], yyvsp[-4], fgc, bgc, scroll, dispattrs);
  }
#line 9267 "parser.c"
    break;

  case 804: /* display_body: x_list opt_at_line_column UPON CRT with_clause on_disp_exception  */
#line 4483 "parser.y"
  {
	cb_emit_display (yyvsp[-5], cb_int0, yyvsp[-1], yyvsp[-4], fgc, bgc, scroll, dispattrs);
  }
#line 9275 "parser.c"
    break;

  case 805: /* with_clause: %empty  */
#line 4489 "parser.y"
                                { yyval = cb_int1; }
#line 9281 "parser.c"
    break;

  case 806: /* with_clause: _with "NO ADVANCING"  */
#line 4490 "parser.y"
                                { yyval = cb_int0; }
#line 9287 "parser.c"
    break;

  case 807: /* with_clause: WITH disp_attrs  */
#line 4491 "parser.y"
                                { yyval = cb_int1; }
#line 9293 "parser.c"
    break;

  case 810: /* disp_attr: BELL  */
#line 4501 "parser.y"
                { dispattrs |= COB_SCREEN_BELL; }
#line 9299 "parser.c"
    break;

  case 811: /* disp_attr: BLINK  */
#line 4502 "parser.y"
                { dispattrs |= COB_SCREEN_BLINK; }
#line 9305 "parser.c"
    break;

  case 812: /* disp_attr: ERASE EOL  */
#line 4503 "parser.y"
                { dispattrs |= COB_SCREEN_ERASE_EOL; }
#line 9311 "parser.c"
    break;

  case 813: /* disp_attr: ERASE EOS  */
#line 4504 "parser.y"
                { dispattrs |= COB_SCREEN_ERASE_EOS; }
#line 9317 "parser.c"
    break;

  case 814: /* disp_attr: HIGHLIGHT  */
#line 4505 "parser.y"
                { dispattrs |= COB_SCREEN_HIGHLIGHT; }
#line 9323 "parser.c"
    break;

  case 815: /* disp_attr: LOWLIGHT  */
#line 4506 "parser.y"
                { dispattrs |= COB_SCREEN_LOWLIGHT; }
#line 9329 "parser.c"
    break;

  case 816: /* disp_attr: "REVERSE-VIDEO"  */
#line 4507 "parser.y"
                { dispattrs |= COB_SCREEN_REVERSE; }
#line 9335 "parser.c"
    break;

  case 817: /* disp_attr: UNDERLINE  */
#line 4508 "parser.y"
                { dispattrs |= COB_SCREEN_UNDERLINE; }
#line 9341 "parser.c"
    break;

  case 818: /* disp_attr: OVERLINE  */
#line 4509 "parser.y"
                { dispattrs |= COB_SCREEN_OVERLINE; }
#line 9347 "parser.c"
    break;

  case 819: /* disp_attr: "FOREGROUND-COLOR" _is num_id_or_lit  */
#line 4511 "parser.y"
  {
	fgc = yyvsp[0];
  }
#line 9355 "parser.c"
    break;

  case 820: /* disp_attr: "BACKGROUND-COLOR" _is num_id_or_lit  */
#line 4515 "parser.y"
  {
	bgc = yyvsp[0];
  }
#line 9363 "parser.c"
    break;

  case 821: /* disp_attr: SCROLL UP _opt_scroll_lines  */
#line 4519 "parser.y"
  {
	scroll = yyvsp[0];
  }
#line 9371 "parser.c"
    break;

  case 822: /* disp_attr: SCROLL DOWN _opt_scroll_lines  */
#line 4523 "parser.y"
  {
	dispattrs |= COB_SCREEN_SCROLL_DOWN;
	scroll = yyvsp[0];
  }
#line 9380 "parser.c"
    break;

  case 823: /* disp_attr: "BLANK-LINE"  */
#line 4527 "parser.y"
                { dispattrs |= COB_SCREEN_BLANK_LINE; }
#line 9386 "parser.c"
    break;

  case 824: /* disp_attr: "BLANK-SCREEN"  */
#line 4528 "parser.y"
                { dispattrs |= COB_SCREEN_BLANK_SCREEN; }
#line 9392 "parser.c"
    break;

  case 825: /* end_display: %empty  */
#line 4532 "parser.y"
                                { terminator_warning (TERM_DISPLAY); }
#line 9398 "parser.c"
    break;

  case 826: /* end_display: "END-DISPLAY"  */
#line 4533 "parser.y"
                                { terminator_clear (TERM_DISPLAY); }
#line 9404 "parser.c"
    break;

  case 827: /* $@49: %empty  */
#line 4542 "parser.y"
                                { BEGIN_STATEMENT ("DIVIDE", TERM_DIVIDE); }
#line 9410 "parser.c"
    break;

  case 829: /* divide_body: x INTO arithmetic_x_list on_size_error  */
#line 4549 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], '/', yyvsp[-3]);
  }
#line 9418 "parser.c"
    break;

  case 830: /* divide_body: x INTO x GIVING arithmetic_x_list on_size_error  */
#line 4553 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_op (yyvsp[-3], '/', yyvsp[-5]));
  }
#line 9426 "parser.c"
    break;

  case 831: /* divide_body: x BY x GIVING arithmetic_x_list on_size_error  */
#line 4557 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_op (yyvsp[-5], '/', yyvsp[-3]));
  }
#line 9434 "parser.c"
    break;

  case 832: /* divide_body: x INTO x GIVING arithmetic_x REMAINDER arithmetic_x on_size_error  */
#line 4561 "parser.y"
  {
	cb_emit_divide (yyvsp[-5], yyvsp[-7], yyvsp[-3], yyvsp[-1]);
  }
#line 9442 "parser.c"
    break;

  case 833: /* divide_body: x BY x GIVING arithmetic_x REMAINDER arithmetic_x on_size_error  */
#line 4565 "parser.y"
  {
	cb_emit_divide (yyvsp[-7], yyvsp[-5], yyvsp[-3], yyvsp[-1]);
  }
#line 9450 "parser.c"
    break;

  case 834: /* end_divide: %empty  */
#line 4571 "parser.y"
                                { terminator_warning (TERM_DIVIDE); }
#line 9456 "parser.c"
    break;

  case 835: /* end_divide: "END-DIVIDE"  */
#line 4572 "parser.y"
                                { terminator_clear (TERM_DIVIDE); }
#line 9462 "parser.c"
    break;

  case 836: /* $@50: %empty  */
#line 4581 "parser.y"
                                { BEGIN_STATEMENT ("ENTRY", 0); }
#line 9468 "parser.c"
    break;

  case 837: /* entry_statement: ENTRY $@50 "Literal" call_using  */
#line 4583 "parser.y"
  {
	if (current_program->nested_level) {
		cb_error (_("ENTRY is invalid in nested program"));
	} else if (cb_verify (cb_entry_statement, "ENTRY")) {
		if (cobc_check_valid_name ((char *)(CB_LITERAL (yyvsp[-1])->data))) {
			cb_error (_("ENTRY '%s' invalid"), (char *)(CB_LITERAL (yyvsp[-1])->data));
		}
		emit_entry ((char *)(CB_LITERAL (yyvsp[-1])->data), 1, yyvsp[0]);
	}
	check_unreached = 0;
  }
#line 9484 "parser.c"
    break;

  case 838: /* $@51: %empty  */
#line 4603 "parser.y"
  {
	BEGIN_STATEMENT ("EVALUATE", TERM_EVALUATE);
	eval_level++;
	for (eval_inc = 0; eval_inc < 64; eval_inc++) {
		eval_check[eval_level][eval_inc] = 0;
	}
	eval_inc = 0;
	eval_inc2 = 0;
  }
#line 9498 "parser.c"
    break;

  case 839: /* evaluate_statement: EVALUATE $@51 evaluate_subject_list evaluate_condition_list end_evaluate  */
#line 4614 "parser.y"
  {
	cb_emit_evaluate (yyvsp[-2], yyvsp[-1]);
	eval_level--;
  }
#line 9507 "parser.c"
    break;

  case 840: /* evaluate_subject_list: evaluate_subject  */
#line 4621 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 9513 "parser.c"
    break;

  case 841: /* evaluate_subject_list: evaluate_subject_list _also evaluate_subject  */
#line 4624 "parser.y"
  {
 	if (!cb_allow_missing_also_clause_in_evaluate && yyvsp[-1] != cb_int1) {
 		cb_error  (_("Invalid expression"));
 	}
 	yyval = cb_list_add (yyvsp[-2], yyvsp[0]);
  }
#line 9524 "parser.c"
    break;

  case 842: /* evaluate_subject: expr  */
#line 4634 "parser.y"
  {
	yyval = yyvsp[0];
	if (CB_REFERENCE_P (yyvsp[0])) {
		eval_check[eval_level][eval_inc++] = 0;
	} else {
		eval_check[eval_level][eval_inc++] = 1;
	}
  }
#line 9537 "parser.c"
    break;

  case 843: /* evaluate_subject: "TRUE"  */
#line 4643 "parser.y"
  {
	yyval = cb_true;
	eval_check[eval_level][eval_inc++] = 2;
  }
#line 9546 "parser.c"
    break;

  case 844: /* evaluate_subject: "FALSE"  */
#line 4648 "parser.y"
  {
	yyval = cb_false;
	eval_check[eval_level][eval_inc++] = 3;
  }
#line 9555 "parser.c"
    break;

  case 845: /* evaluate_condition_list: evaluate_case_list evaluate_other  */
#line 4656 "parser.y"
  {
	yyval = yyvsp[-1];
	if (yyvsp[0]) {
		if (cb_allow_empty_imperative_statement) {
			/*
			 * some compiler implementation allow empty
			 * imperative statements in WHEN phrases, and
			 * treats WHEN OTHER phrase following that
			 * asif the rest part of when_list belonging
			 * to that.
			 */
			cb_tree l, case_item;
			l = yyval;
			while (CB_CHAIN (l)) {
				l = CB_CHAIN (l);
			}
			case_item = CB_VALUE (l);
			if (!CB_VALUE (case_item)) {
				 /* warning: duplecates ptr. here */
				CB_VALUE (case_item) = CB_VALUE (yyvsp[0]);
			}
		}
		yyval = cb_list_add (yyval, yyvsp[0]);
	}
  }
#line 9585 "parser.c"
    break;

  case 846: /* evaluate_case_list: evaluate_case  */
#line 4684 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 9591 "parser.c"
    break;

  case 847: /* evaluate_case_list: evaluate_case_list evaluate_case  */
#line 4686 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 9597 "parser.c"
    break;

  case 848: /* $@52: %empty  */
#line 4691 "parser.y"
  {
	check_unreached = 0;
  }
#line 9605 "parser.c"
    break;

  case 849: /* evaluate_case: evaluate_when_list $@52 statement_list  */
#line 4695 "parser.y"
  {
	if (!cb_allow_empty_imperative_statement && yyvsp[0] == NULL) {
		cb_error (_("syntax error"));
	}
	yyval = cb_cons (yyvsp[0], yyvsp[-2]);
	eval_inc2 = 0;
  }
#line 9617 "parser.c"
    break;

  case 850: /* evaluate_other: %empty  */
#line 4706 "parser.y"
  {
	yyval = NULL;
  }
#line 9625 "parser.c"
    break;

  case 851: /* $@53: %empty  */
#line 4710 "parser.y"
  {
	check_unreached = 0;
  }
#line 9633 "parser.c"
    break;

  case 852: /* evaluate_other: "WHEN OTHER" $@53 statement_list  */
#line 4714 "parser.y"
  {
	if (!cb_allow_empty_imperative_statement && yyvsp[0] == NULL) {
		cb_error (_("syntax error"));
	}
	yyval = cb_cons (yyvsp[0], NULL);
	eval_inc2 = 0;
  }
#line 9645 "parser.c"
    break;

  case 853: /* evaluate_when_list: WHEN evaluate_object_list  */
#line 4724 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 9651 "parser.c"
    break;

  case 854: /* evaluate_when_list: evaluate_when_list WHEN evaluate_object_list  */
#line 4726 "parser.y"
                                { yyval = cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 9657 "parser.c"
    break;

  case 855: /* evaluate_object_list: evaluate_object  */
#line 4730 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 9663 "parser.c"
    break;

  case 856: /* evaluate_object_list: evaluate_object_list _also evaluate_object  */
#line 4733 "parser.y"
  {
 	if (!cb_allow_missing_also_clause_in_evaluate && yyvsp[-1] != cb_int1) {
 		cb_error  (_("Invalid expression"));
 	}
 	yyval = cb_list_add (yyvsp[-2], yyvsp[0]);
  }
#line 9674 "parser.c"
    break;

  case 857: /* evaluate_object: partial_expr opt_evaluate_thru_expr  */
#line 4743 "parser.y"
  {
	cb_tree not;
	cb_tree e1;
	cb_tree e2;

	not = cb_int0;
	e2 = yyvsp[0];
	/* in case the first token is NOT */
	if (CB_PURPOSE_INT (yyvsp[-1]) == '!') {
		if (eval_check[eval_level][eval_inc2] < 2) {
			not = cb_int1;
			yyvsp[-1] = CB_CHAIN (yyvsp[-1]);
		}
	}

	/* build expr now */
	e1 = cb_build_expr (yyvsp[-1]);

	if (e2 == NULL) {
		/* WHEN expr */
		eval_inc2++;
		yyval = cb_build_pair (not, cb_build_pair (e1, NULL));
	} else {
		/* WHEN expr THRU expr */
		yyval = cb_build_pair (not, cb_build_pair (e1, e2));
		eval_inc2++;
	}
  }
#line 9707 "parser.c"
    break;

  case 858: /* evaluate_object: ANY  */
#line 4771 "parser.y"
                                { yyval = cb_any; eval_inc2++; }
#line 9713 "parser.c"
    break;

  case 859: /* evaluate_object: "TRUE"  */
#line 4772 "parser.y"
                                { yyval = cb_true; eval_inc2++; }
#line 9719 "parser.c"
    break;

  case 860: /* evaluate_object: "FALSE"  */
#line 4773 "parser.y"
                                { yyval = cb_false; eval_inc2++; }
#line 9725 "parser.c"
    break;

  case 861: /* opt_evaluate_thru_expr: %empty  */
#line 4776 "parser.y"
                                { yyval = NULL; }
#line 9731 "parser.c"
    break;

  case 862: /* opt_evaluate_thru_expr: THRU expr  */
#line 4777 "parser.y"
                                { yyval = yyvsp[0]; }
#line 9737 "parser.c"
    break;

  case 863: /* end_evaluate: %empty  */
#line 4781 "parser.y"
                                { terminator_warning (TERM_EVALUATE); }
#line 9743 "parser.c"
    break;

  case 864: /* end_evaluate: "END-EVALUATE"  */
#line 4782 "parser.y"
                                { terminator_clear (TERM_EVALUATE); }
#line 9749 "parser.c"
    break;

  case 865: /* $@54: %empty  */
#line 4791 "parser.y"
                                { BEGIN_STATEMENT ("EXIT", 0); }
#line 9755 "parser.c"
    break;

  case 867: /* exit_body: %empty  */
#line 4796 "parser.y"
                                { /* nothing */ }
#line 9761 "parser.c"
    break;

  case 868: /* exit_body: PROGRAM  */
#line 4798 "parser.y"
  {
	if (in_declaratives && use_global_ind) {
		cb_error (_("EXIT PROGRAM is not allowed within a USE GLOBAL procedure"));
	}
	check_unreached = 1;
	cb_emit_exit (0);
  }
#line 9773 "parser.c"
    break;

  case 869: /* exit_body: PERFORM  */
#line 4806 "parser.y"
  {
	if (!perform_stack) {
		cb_error (_("EXIT PERFORM is only valid with inline PERFORM"));
	} else {
		cb_emit_java_break ();
	}
  }
#line 9785 "parser.c"
    break;

  case 870: /* exit_body: PERFORM CYCLE  */
#line 4814 "parser.y"
  {
	if (!perform_stack) {
		cb_error (_("EXIT PERFORM is only valid with inline PERFORM"));
	} else {
		cb_emit_java_continue ();
	}
  }
#line 9797 "parser.c"
    break;

  case 871: /* exit_body: SECTION  */
#line 4822 "parser.y"
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
#line 9819 "parser.c"
    break;

  case 872: /* exit_body: PARAGRAPH  */
#line 4840 "parser.y"
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
#line 9841 "parser.c"
    break;

  case 873: /* $@55: %empty  */
#line 4864 "parser.y"
                                { BEGIN_STATEMENT ("FREE", 0); }
#line 9847 "parser.c"
    break;

  case 874: /* free_statement: FREE $@55 target_x_list  */
#line 4866 "parser.y"
  {
	cb_emit_free (yyvsp[0]);
  }
#line 9855 "parser.c"
    break;

  case 875: /* $@56: %empty  */
#line 4877 "parser.y"
                                { BEGIN_STATEMENT ("GENERATE", 0); }
#line 9861 "parser.c"
    break;

  case 876: /* generate_statement: GENERATE $@56 identifier  */
#line 4879 "parser.y"
  {
	PENDING("GENERATE");
  }
#line 9869 "parser.c"
    break;

  case 877: /* $@57: %empty  */
#line 4890 "parser.y"
                                { BEGIN_STATEMENT ("GO TO", 0); }
#line 9875 "parser.c"
    break;

  case 878: /* goto_statement: GO _to $@57 procedure_name_list goto_depending  */
#line 4892 "parser.y"
  {
	cb_emit_goto (yyvsp[-1], yyvsp[0]);
  }
#line 9883 "parser.c"
    break;

  case 879: /* goto_depending: %empty  */
#line 4899 "parser.y"
  {
	check_unreached = 1;
	yyval = NULL;
  }
#line 9892 "parser.c"
    break;

  case 880: /* goto_depending: DEPENDING _on identifier  */
#line 4904 "parser.y"
  {
	check_unreached = 0;
	yyval = yyvsp[0];
  }
#line 9901 "parser.c"
    break;

  case 881: /* $@58: %empty  */
#line 4916 "parser.y"
                                { BEGIN_STATEMENT ("GOBACK", 0); }
#line 9907 "parser.c"
    break;

  case 882: /* goback_statement: GOBACK $@58  */
#line 4917 "parser.y"
  {
	check_unreached = 1;
	cb_emit_exit (1);
  }
#line 9916 "parser.c"
    break;

  case 883: /* $@59: %empty  */
#line 4929 "parser.y"
                                { BEGIN_STATEMENT ("IF", TERM_IF); }
#line 9922 "parser.c"
    break;

  case 884: /* $@60: %empty  */
#line 4931 "parser.y"
  {
	check_unreached = 0;
  }
#line 9930 "parser.c"
    break;

  case 885: /* if_statement: IF $@59 condition _then $@60 statement_list if_else_sentence end_if  */
#line 4936 "parser.y"
  {
	if (!cb_allow_empty_imperative_statement && yyvsp[-2] == NULL) {
		cb_error (_("syntax error"));
	}
	cb_emit_if (yyvsp[-5], yyvsp[-2], yyvsp[-1]);
  }
#line 9941 "parser.c"
    break;

  case 887: /* if_else_sentence: %empty  */
#line 4947 "parser.y"
  {
	yyval = NULL;
  }
#line 9949 "parser.c"
    break;

  case 888: /* $@61: %empty  */
#line 4951 "parser.y"
  {
	check_unreached = 0;
  }
#line 9957 "parser.c"
    break;

  case 889: /* if_else_sentence: ELSE $@61 statement_list  */
#line 4955 "parser.y"
  {
	if (!cb_allow_empty_imperative_statement && yyvsp[0] == NULL) {
		cb_error (_("syntax error"));
	}
	yyval = yyvsp[0];
  }
#line 9968 "parser.c"
    break;

  case 890: /* end_if: %empty  */
#line 4964 "parser.y"
                                { terminator_warning (TERM_IF); }
#line 9974 "parser.c"
    break;

  case 891: /* end_if: "END-IF"  */
#line 4965 "parser.y"
                                { terminator_clear (TERM_IF); }
#line 9980 "parser.c"
    break;

  case 892: /* $@62: %empty  */
#line 4974 "parser.y"
                                { BEGIN_STATEMENT ("INITIALIZE", 0); }
#line 9986 "parser.c"
    break;

  case 893: /* initialize_statement: INITIALIZE $@62 target_x_list initialize_filler initialize_value initialize_replacing initialize_default  */
#line 4976 "parser.y"
  {
	cb_emit_initialize (yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1], yyvsp[0]);
  }
#line 9994 "parser.c"
    break;

  case 894: /* initialize_filler: %empty  */
#line 4982 "parser.y"
                                { yyval = NULL; }
#line 10000 "parser.c"
    break;

  case 895: /* initialize_filler: _with FILLER  */
#line 4983 "parser.y"
                                { yyval = cb_true; }
#line 10006 "parser.c"
    break;

  case 896: /* initialize_value: %empty  */
#line 4987 "parser.y"
                                { yyval = NULL; }
#line 10012 "parser.c"
    break;

  case 897: /* initialize_value: ALL _to VALUE  */
#line 4988 "parser.y"
                                { yyval = cb_true; }
#line 10018 "parser.c"
    break;

  case 898: /* initialize_value: initialize_category _to VALUE  */
#line 4989 "parser.y"
                                { yyval = yyvsp[-2]; }
#line 10024 "parser.c"
    break;

  case 899: /* initialize_replacing: %empty  */
#line 4993 "parser.y"
                                { yyval = NULL; }
#line 10030 "parser.c"
    break;

  case 900: /* initialize_replacing: REPLACING initialize_replacing_list  */
#line 4995 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10036 "parser.c"
    break;

  case 901: /* initialize_replacing_list: initialize_replacing_item  */
#line 4999 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10042 "parser.c"
    break;

  case 902: /* initialize_replacing_list: initialize_replacing_list initialize_replacing_item  */
#line 5001 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 10048 "parser.c"
    break;

  case 903: /* initialize_replacing_item: initialize_category _data BY x  */
#line 5005 "parser.y"
                                 { yyval = cb_build_pair (yyvsp[-3], yyvsp[0]); }
#line 10054 "parser.c"
    break;

  case 904: /* initialize_category: ALPHABETIC  */
#line 5009 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_ALPHABETIC); }
#line 10060 "parser.c"
    break;

  case 905: /* initialize_category: ALPHANUMERIC  */
#line 5010 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_ALPHANUMERIC); }
#line 10066 "parser.c"
    break;

  case 906: /* initialize_category: NUMERIC  */
#line 5011 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_NUMERIC); }
#line 10072 "parser.c"
    break;

  case 907: /* initialize_category: "ALPHANUMERIC-EDITED"  */
#line 5012 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
#line 10078 "parser.c"
    break;

  case 908: /* initialize_category: "NUMERIC-EDITED"  */
#line 5013 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
#line 10084 "parser.c"
    break;

  case 909: /* initialize_category: NATIONAL  */
#line 5014 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_NATIONAL); }
#line 10090 "parser.c"
    break;

  case 910: /* initialize_category: "NATIONAL-EDITED"  */
#line 5015 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
#line 10096 "parser.c"
    break;

  case 911: /* initialize_default: %empty  */
#line 5019 "parser.y"
                                { yyval = NULL; }
#line 10102 "parser.c"
    break;

  case 912: /* initialize_default: DEFAULT  */
#line 5020 "parser.y"
                                { yyval = cb_true; }
#line 10108 "parser.c"
    break;

  case 913: /* $@63: %empty  */
#line 5029 "parser.y"
                                { BEGIN_STATEMENT ("INITIATE", 0); }
#line 10114 "parser.c"
    break;

  case 914: /* initiate_statement: INITIATE $@63 identifier_list  */
#line 5031 "parser.y"
  {
	PENDING("INITIATE");
  }
#line 10122 "parser.c"
    break;

  case 915: /* $@64: %empty  */
#line 5042 "parser.y"
  {
	BEGIN_STATEMENT ("INSPECT", 0);
	sending_id = 0;
	inspect_keyword = 0;
  }
#line 10132 "parser.c"
    break;

  case 917: /* send_identifier: identifier  */
#line 5051 "parser.y"
                                { save_tree_1 = yyvsp[0]; sending_id = 0; }
#line 10138 "parser.c"
    break;

  case 918: /* send_identifier: literal  */
#line 5052 "parser.y"
                                { save_tree_1 = yyvsp[0]; sending_id = 1; }
#line 10144 "parser.c"
    break;

  case 919: /* send_identifier: function  */
#line 5053 "parser.y"
                                { save_tree_1 = yyvsp[0]; sending_id = 1; }
#line 10150 "parser.c"
    break;

  case 922: /* inspect_item: inspect_tallying  */
#line 5062 "parser.y"
                                { cb_emit_inspect (save_tree_1, yyvsp[0], cb_int0, 0); }
#line 10156 "parser.c"
    break;

  case 923: /* inspect_item: inspect_replacing  */
#line 5063 "parser.y"
                                { cb_emit_inspect (save_tree_1, yyvsp[0], cb_int1, 1); }
#line 10162 "parser.c"
    break;

  case 924: /* inspect_item: inspect_converting  */
#line 5064 "parser.y"
                                { cb_emit_inspect (save_tree_1, yyvsp[0], cb_int0, 2); }
#line 10168 "parser.c"
    break;

  case 925: /* $@65: %empty  */
#line 5070 "parser.y"
                                { cb_init_tarrying (); }
#line 10174 "parser.c"
    break;

  case 926: /* inspect_tallying: TALLYING $@65 tallying_list  */
#line 5071 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10180 "parser.c"
    break;

  case 927: /* tallying_list: tallying_item  */
#line 5075 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10186 "parser.c"
    break;

  case 928: /* tallying_list: tallying_list tallying_item  */
#line 5076 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 10192 "parser.c"
    break;

  case 929: /* tallying_item: simple_value FOR  */
#line 5080 "parser.y"
                                { yyval = cb_build_tarrying_data (yyvsp[-1]); }
#line 10198 "parser.c"
    break;

  case 930: /* tallying_item: CHARACTERS inspect_region  */
#line 5081 "parser.y"
                                { yyval = cb_build_tarrying_characters (yyvsp[0]); }
#line 10204 "parser.c"
    break;

  case 931: /* tallying_item: ALL  */
#line 5082 "parser.y"
                                { yyval = cb_build_tarrying_all (); }
#line 10210 "parser.c"
    break;

  case 932: /* tallying_item: LEADING  */
#line 5083 "parser.y"
                                { yyval = cb_build_tarrying_leading (); }
#line 10216 "parser.c"
    break;

  case 933: /* tallying_item: TRAILING  */
#line 5084 "parser.y"
                                { yyval = cb_build_tarrying_trailing (); }
#line 10222 "parser.c"
    break;

  case 934: /* tallying_item: simple_value inspect_region  */
#line 5085 "parser.y"
                                { yyval = cb_build_tarrying_value (yyvsp[-1], yyvsp[0]); }
#line 10228 "parser.c"
    break;

  case 935: /* inspect_replacing: REPLACING replacing_list  */
#line 5091 "parser.y"
                                { yyval = yyvsp[0]; inspect_keyword = 0; }
#line 10234 "parser.c"
    break;

  case 936: /* replacing_list: replacing_item  */
#line 5095 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10240 "parser.c"
    break;

  case 937: /* replacing_list: replacing_list replacing_item  */
#line 5096 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 10246 "parser.c"
    break;

  case 938: /* replacing_item: CHARACTERS BY simple_value inspect_region  */
#line 5101 "parser.y"
  {
	yyval = cb_build_replacing_characters (yyvsp[-1], yyvsp[0], save_tree_1);
	inspect_keyword = 0;
  }
#line 10255 "parser.c"
    break;

  case 939: /* replacing_item: rep_keyword replacing_region  */
#line 5105 "parser.y"
                                        { yyval = yyvsp[0]; }
#line 10261 "parser.c"
    break;

  case 940: /* rep_keyword: %empty  */
#line 5109 "parser.y"
                                { /* Nothing */ }
#line 10267 "parser.c"
    break;

  case 941: /* rep_keyword: ALL  */
#line 5110 "parser.y"
                                { inspect_keyword = 1; }
#line 10273 "parser.c"
    break;

  case 942: /* rep_keyword: LEADING  */
#line 5111 "parser.y"
                                { inspect_keyword = 2; }
#line 10279 "parser.c"
    break;

  case 943: /* rep_keyword: FIRST  */
#line 5112 "parser.y"
                                { inspect_keyword = 3; }
#line 10285 "parser.c"
    break;

  case 944: /* rep_keyword: TRAILING  */
#line 5113 "parser.y"
                                { inspect_keyword = 4; }
#line 10291 "parser.c"
    break;

  case 945: /* replacing_region: simple_value BY simple_all_value inspect_region  */
#line 5118 "parser.y"
  {
	switch (inspect_keyword) {
		case 1:
			yyval = cb_build_replacing_all (yyvsp[-3], yyvsp[-1], yyvsp[0], save_tree_1);
			break;
		case 2:
			yyval = cb_build_replacing_leading (yyvsp[-3], yyvsp[-1], yyvsp[0]);
			break;
		case 3:
			yyval = cb_build_replacing_first (yyvsp[-3], yyvsp[-1], yyvsp[0]);
			break;
		case 4:
			yyval = cb_build_replacing_trailing (yyvsp[-3], yyvsp[-1], yyvsp[0]);
			break;
		default:
			cb_error (_("INSPECT missing a keyword"));
			yyval = cb_error_node;
			break;
	}
  }
#line 10316 "parser.c"
    break;

  case 946: /* inspect_converting: CONVERTING simple_value TO simple_all_value inspect_region  */
#line 5144 "parser.y"
  {
	if (cb_validate_inspect (save_tree_1, yyvsp[-3], yyvsp[-1]) < 0 ) {
		yyval = cb_error_node;
	} else {
		yyval = cb_build_converting (yyvsp[-3], yyvsp[-1], yyvsp[0]);
	}
  }
#line 10328 "parser.c"
    break;

  case 947: /* inspect_region: %empty  */
#line 5156 "parser.y"
                                { yyval = cb_build_inspect_region_start (); }
#line 10334 "parser.c"
    break;

  case 948: /* inspect_region: inspect_region before_or_after _initial x  */
#line 5158 "parser.y"
                                { yyval = cb_build_inspect_region (yyvsp[-3], yyvsp[-2], yyvsp[0]); }
#line 10340 "parser.c"
    break;

  case 951: /* $@66: %empty  */
#line 5169 "parser.y"
                                { BEGIN_STATEMENT ("MERGE", 0); }
#line 10346 "parser.c"
    break;

  case 953: /* $@67: %empty  */
#line 5179 "parser.y"
                                { BEGIN_STATEMENT ("MOVE", 0); }
#line 10352 "parser.c"
    break;

  case 955: /* move_body: x TO target_x_list  */
#line 5185 "parser.y"
  {
	cb_emit_move (yyvsp[-2], yyvsp[0]);
  }
#line 10360 "parser.c"
    break;

  case 956: /* move_body: CORRESPONDING x TO target_x_list  */
#line 5189 "parser.y"
  {
	cb_emit_move_corresponding (yyvsp[-2], yyvsp[0]);
  }
#line 10368 "parser.c"
    break;

  case 957: /* $@68: %empty  */
#line 5200 "parser.y"
                                { BEGIN_STATEMENT ("MULTIPLY", TERM_MULTIPLY); }
#line 10374 "parser.c"
    break;

  case 959: /* multiply_body: x BY arithmetic_x_list on_size_error  */
#line 5207 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], '*', yyvsp[-3]);
  }
#line 10382 "parser.c"
    break;

  case 960: /* multiply_body: x BY x GIVING arithmetic_x_list on_size_error  */
#line 5211 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_op (yyvsp[-5], '*', yyvsp[-3]));
  }
#line 10390 "parser.c"
    break;

  case 961: /* end_multiply: %empty  */
#line 5217 "parser.y"
                                { terminator_warning (TERM_MULTIPLY); }
#line 10396 "parser.c"
    break;

  case 962: /* end_multiply: "END-MULTIPLY"  */
#line 5218 "parser.y"
                                { terminator_clear (TERM_MULTIPLY); }
#line 10402 "parser.c"
    break;

  case 963: /* $@69: %empty  */
#line 5227 "parser.y"
                                { BEGIN_STATEMENT ("OPEN", 0); }
#line 10408 "parser.c"
    break;

  case 966: /* open_list: open_list open_mode open_sharing file_name_list open_option  */
#line 5234 "parser.y"
  {
	cb_tree l;
	for (l = yyvsp[-1]; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) != cb_error_node) {
			BEGIN_IMPLICIT_STATEMENT (l);
			cb_emit_open (CB_VALUE (l), yyvsp[-3], yyvsp[-2]);
		}
	}
  }
#line 10422 "parser.c"
    break;

  case 967: /* open_mode: INPUT  */
#line 5246 "parser.y"
                                { yyval = cb_int (COB_OPEN_INPUT); }
#line 10428 "parser.c"
    break;

  case 968: /* open_mode: OUTPUT  */
#line 5247 "parser.y"
                                { yyval = cb_int (COB_OPEN_OUTPUT); }
#line 10434 "parser.c"
    break;

  case 969: /* open_mode: "I-O"  */
#line 5248 "parser.y"
                                { yyval = cb_int (COB_OPEN_I_O); }
#line 10440 "parser.c"
    break;

  case 970: /* open_mode: EXTEND  */
#line 5249 "parser.y"
                                { yyval = cb_int (COB_OPEN_EXTEND); }
#line 10446 "parser.c"
    break;

  case 971: /* open_sharing: %empty  */
#line 5253 "parser.y"
                                { yyval = NULL; }
#line 10452 "parser.c"
    break;

  case 972: /* open_sharing: SHARING _with sharing_option  */
#line 5254 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10458 "parser.c"
    break;

  case 973: /* open_option: %empty  */
#line 5258 "parser.y"
                                { yyval = NULL; }
#line 10464 "parser.c"
    break;

  case 974: /* open_option: _with NO REWIND  */
#line 5259 "parser.y"
                                { yyval = NULL; }
#line 10470 "parser.c"
    break;

  case 975: /* open_option: _with LOCK  */
#line 5260 "parser.y"
                                { PENDING ("OPEN ... WITH LOCK"); }
#line 10476 "parser.c"
    break;

  case 976: /* $@70: %empty  */
#line 5272 "parser.y"
                                { BEGIN_STATEMENT ("PERFORM", TERM_PERFORM); }
#line 10482 "parser.c"
    break;

  case 978: /* perform_body: perform_procedure perform_option  */
#line 5278 "parser.y"
  {
	cb_emit_perform (yyvsp[0], yyvsp[-1]);
  }
#line 10490 "parser.c"
    break;

  case 979: /* $@71: %empty  */
#line 5282 "parser.y"
  {
	perform_stack = cb_cons (yyvsp[0], perform_stack);
	check_unreached = 0;
  }
#line 10499 "parser.c"
    break;

  case 980: /* perform_body: perform_option $@71 statement_list end_perform  */
#line 5287 "parser.y"
  {
	perform_stack = CB_CHAIN (perform_stack);
	cb_emit_perform (yyvsp[-3], yyvsp[-1]);
  }
#line 10508 "parser.c"
    break;

  case 981: /* perform_body: perform_option "END-PERFORM"  */
#line 5292 "parser.y"
  {
	cb_emit_perform (yyvsp[-1], NULL);
  }
#line 10516 "parser.c"
    break;

  case 982: /* end_perform: %empty  */
#line 5298 "parser.y"
                                { terminator_error (); }
#line 10522 "parser.c"
    break;

  case 983: /* end_perform: "END-PERFORM"  */
#line 5299 "parser.y"
                                { terminator_clear (TERM_PERFORM); }
#line 10528 "parser.c"
    break;

  case 984: /* perform_procedure: procedure_name  */
#line 5304 "parser.y"
  {
	CB_REFERENCE (yyvsp[0])->length = cb_true; /* return from $1 */
	yyval = cb_build_pair (yyvsp[0], yyvsp[0]);
  }
#line 10537 "parser.c"
    break;

  case 985: /* perform_procedure: procedure_name THRU procedure_name  */
#line 5309 "parser.y"
  {
	CB_REFERENCE (yyvsp[0])->length = cb_true; /* return from $3 */
	yyval = cb_build_pair (yyvsp[-2], yyvsp[0]);
  }
#line 10546 "parser.c"
    break;

  case 986: /* perform_option: %empty  */
#line 5317 "parser.y"
  {
	yyval = cb_build_perform_once (NULL);
  }
#line 10554 "parser.c"
    break;

  case 987: /* perform_option: FOREVER  */
#line 5321 "parser.y"
  {
	yyval = cb_build_perform_forever (NULL);
  }
#line 10562 "parser.c"
    break;

  case 988: /* perform_option: id_or_lit_or_func TIMES  */
#line 5325 "parser.y"
  {
	yyval = cb_build_perform_times (yyvsp[-1]);
	current_program->loop_counter++;
  }
#line 10571 "parser.c"
    break;

  case 989: /* perform_option: perform_test UNTIL condition  */
#line 5330 "parser.y"
  {
	cb_tree varying;

	varying = cb_list_init (cb_build_perform_varying (NULL, NULL, NULL, yyvsp[0]));
	yyval = cb_build_perform_until (yyvsp[-2], varying);
  }
#line 10582 "parser.c"
    break;

  case 990: /* perform_option: perform_test VARYING perform_varying_list  */
#line 5337 "parser.y"
  {
	yyval = cb_build_perform_until (yyvsp[-2], yyvsp[0]);
  }
#line 10590 "parser.c"
    break;

  case 991: /* perform_test: %empty  */
#line 5343 "parser.y"
                                { yyval = CB_BEFORE; }
#line 10596 "parser.c"
    break;

  case 992: /* perform_test: _with TEST before_or_after  */
#line 5344 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10602 "parser.c"
    break;

  case 993: /* perform_varying_list: perform_varying  */
#line 5348 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 10608 "parser.c"
    break;

  case 994: /* perform_varying_list: perform_varying_list AFTER perform_varying  */
#line 5350 "parser.y"
                                { yyval = cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 10614 "parser.c"
    break;

  case 995: /* perform_varying: identifier FROM x BY x UNTIL condition  */
#line 5355 "parser.y"
  {
	yyval = cb_build_perform_varying (yyvsp[-6], yyvsp[-4], yyvsp[-2], yyvsp[0]);
  }
#line 10622 "parser.c"
    break;

  case 996: /* $@72: %empty  */
#line 5366 "parser.y"
                                { BEGIN_STATEMENT ("READ", TERM_READ); }
#line 10628 "parser.c"
    break;

  case 997: /* read_statement: READ $@72 file_name flag_next _record read_into with_lock read_key read_handler end_read  */
#line 5369 "parser.y"
  {
	if (yyvsp[-7] != cb_error_node) {
		if (cb_use_invalidkey_handler_on_status34 &&
		    current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
		    (CB_FILE(cb_ref (yyvsp[-7]))->organization != COB_ORG_RELATIVE &&
		     CB_FILE(cb_ref (yyvsp[-7]))->organization != COB_ORG_INDEXED)) {
			current_statement->handler_id = COB_EC_I_O_PERMANENT_ERROR;
		}
		if (yyvsp[-3] && (CB_FILE(cb_ref (yyvsp[-7]))->lock_mode & COB_LOCK_AUTOMATIC)) {
			cb_error (_("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if (yyvsp[-2] &&
		      (CB_FILE(cb_ref (yyvsp[-7]))->organization != COB_ORG_RELATIVE &&
		       CB_FILE(cb_ref (yyvsp[-7]))->organization != COB_ORG_INDEXED)) {
			cb_error (_("KEY clause invalid with this file type"));
		} else if (current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
		      (CB_FILE(cb_ref (yyvsp[-7]))->organization != COB_ORG_RELATIVE &&
		       CB_FILE(cb_ref (yyvsp[-7]))->organization != COB_ORG_INDEXED)) {
			cb_error (_("INVALID KEY clause invalid with this file type"));
		} else {
			cb_emit_read (yyvsp[-7], yyvsp[-6], yyvsp[-4], yyvsp[-2], yyvsp[-3]);
		}
	}
  }
#line 10656 "parser.c"
    break;

  case 998: /* read_into: %empty  */
#line 5395 "parser.y"
                                { yyval = NULL; }
#line 10662 "parser.c"
    break;

  case 999: /* read_into: INTO identifier  */
#line 5396 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10668 "parser.c"
    break;

  case 1000: /* with_lock: %empty  */
#line 5400 "parser.y"
                                { yyval = NULL; }
#line 10674 "parser.c"
    break;

  case 1001: /* with_lock: IGNORING LOCK  */
#line 5402 "parser.y"
  {
	yyval = cb_int3;
  }
#line 10682 "parser.c"
    break;

  case 1002: /* with_lock: _with LOCK  */
#line 5406 "parser.y"
  {
	yyval = cb_int1;
  }
#line 10690 "parser.c"
    break;

  case 1003: /* with_lock: _with NO LOCK  */
#line 5410 "parser.y"
  {
	yyval = cb_int2;
  }
#line 10698 "parser.c"
    break;

  case 1004: /* with_lock: _with IGNORE LOCK  */
#line 5414 "parser.y"
  {
	yyval = cb_int3;
  }
#line 10706 "parser.c"
    break;

  case 1005: /* with_lock: _with WAIT  */
#line 5418 "parser.y"
  {
	yyval = cb_int4;
  }
#line 10714 "parser.c"
    break;

  case 1006: /* read_key: %empty  */
#line 5424 "parser.y"
                                { yyval = NULL; }
#line 10720 "parser.c"
    break;

  case 1007: /* read_key: KEY _is identifier_list  */
#line 5426 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 10728 "parser.c"
    break;

  case 1011: /* end_read: %empty  */
#line 5437 "parser.y"
                                { terminator_warning (TERM_READ); }
#line 10734 "parser.c"
    break;

  case 1012: /* end_read: "END-READ"  */
#line 5438 "parser.y"
                                { terminator_clear (TERM_READ); }
#line 10740 "parser.c"
    break;

  case 1013: /* $@73: %empty  */
#line 5447 "parser.y"
                                { BEGIN_STATEMENT ("RELEASE", 0); }
#line 10746 "parser.c"
    break;

  case 1014: /* release_statement: RELEASE $@73 record_name write_from  */
#line 5449 "parser.y"
  {
	if (yyvsp[-1] != cb_error_node) {
		cb_emit_release (yyvsp[-1], yyvsp[0]);
	}
  }
#line 10756 "parser.c"
    break;

  case 1015: /* $@74: %empty  */
#line 5462 "parser.y"
                                { BEGIN_STATEMENT ("RETURN", TERM_RETURN); }
#line 10762 "parser.c"
    break;

  case 1016: /* return_statement: RETURN $@74 file_name _record read_into at_end end_return  */
#line 5465 "parser.y"
  {
	if (yyvsp[-4] != cb_error_node) {
		cb_emit_return (yyvsp[-4], yyvsp[-2]);
	}
  }
#line 10772 "parser.c"
    break;

  case 1017: /* end_return: %empty  */
#line 5473 "parser.y"
                                { terminator_warning (TERM_RETURN); }
#line 10778 "parser.c"
    break;

  case 1018: /* end_return: "END-RETURN"  */
#line 5474 "parser.y"
                                { terminator_clear (TERM_RETURN); }
#line 10784 "parser.c"
    break;

  case 1019: /* $@75: %empty  */
#line 5483 "parser.y"
                                { BEGIN_STATEMENT ("REWRITE", TERM_REWRITE); }
#line 10790 "parser.c"
    break;

  case 1020: /* rewrite_statement: REWRITE $@75 record_name write_from write_lock opt_invalid_key end_rewrite  */
#line 5486 "parser.y"
  {
	if (yyvsp[-4] != cb_error_node) {
		if (cb_use_invalidkey_handler_on_status34 &&
		    current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
		    (CB_FIELD(cb_ref (yyvsp[-4]))->file->organization != COB_ORG_RELATIVE &&
		     CB_FIELD(cb_ref (yyvsp[-4]))->file->organization != COB_ORG_INDEXED)) {
			current_statement->handler_id = COB_EC_I_O_PERMANENT_ERROR;
		}
		cb_emit_rewrite (yyvsp[-4], yyvsp[-3], yyvsp[-2]);
	}
  }
#line 10806 "parser.c"
    break;

  case 1021: /* write_lock: %empty  */
#line 5500 "parser.y"
                                { yyval = NULL; }
#line 10812 "parser.c"
    break;

  case 1022: /* write_lock: _with LOCK  */
#line 5502 "parser.y"
  {
	yyval = cb_int1;
  }
#line 10820 "parser.c"
    break;

  case 1023: /* write_lock: _with NO LOCK  */
#line 5506 "parser.y"
  {
	yyval = cb_int2;
  }
#line 10828 "parser.c"
    break;

  case 1024: /* end_rewrite: %empty  */
#line 5512 "parser.y"
                                { terminator_warning (TERM_REWRITE); }
#line 10834 "parser.c"
    break;

  case 1025: /* end_rewrite: "END-REWRITE"  */
#line 5513 "parser.y"
                                { terminator_clear (TERM_REWRITE); }
#line 10840 "parser.c"
    break;

  case 1026: /* rollback_statement: ROLLBACK  */
#line 5523 "parser.y"
  {
	BEGIN_STATEMENT ("ROLLBACK", 0);
	cb_emit_rollback ();
  }
#line 10849 "parser.c"
    break;

  case 1027: /* $@76: %empty  */
#line 5535 "parser.y"
                                { BEGIN_STATEMENT ("SEARCH", TERM_SEARCH); }
#line 10855 "parser.c"
    break;

  case 1029: /* search_body: table_name search_varying search_at_end search_whens  */
#line 5542 "parser.y"
  {
	cb_emit_search (yyvsp[-3], yyvsp[-2], yyvsp[-1], yyvsp[0]);
  }
#line 10863 "parser.c"
    break;

  case 1030: /* $@77: %empty  */
#line 5546 "parser.y"
  {
	check_unreached = 0;
  }
#line 10871 "parser.c"
    break;

  case 1031: /* search_body: ALL table_name search_at_end WHEN expr $@77 statement_list  */
#line 5550 "parser.y"
  {
	cb_emit_search_all (yyvsp[-5], yyvsp[-4], yyvsp[-2], yyvsp[0]);
  }
#line 10879 "parser.c"
    break;

  case 1032: /* search_varying: %empty  */
#line 5556 "parser.y"
                                { yyval = NULL; }
#line 10885 "parser.c"
    break;

  case 1033: /* search_varying: VARYING identifier  */
#line 5557 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10891 "parser.c"
    break;

  case 1034: /* search_at_end: %empty  */
#line 5561 "parser.y"
                                { yyval = NULL; }
#line 10897 "parser.c"
    break;

  case 1035: /* $@78: %empty  */
#line 5563 "parser.y"
  {
	check_unreached = 0;
  }
#line 10905 "parser.c"
    break;

  case 1036: /* search_at_end: _at END $@78 statement_list  */
#line 5567 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 10913 "parser.c"
    break;

  case 1037: /* search_whens: search_when  */
#line 5573 "parser.y"
                                { yyval = yyvsp[0]; }
#line 10919 "parser.c"
    break;

  case 1038: /* search_whens: search_when search_whens  */
#line 5574 "parser.y"
                                { yyval = yyvsp[-1]; CB_IF (yyvsp[-1])->stmt2 = yyvsp[0]; }
#line 10925 "parser.c"
    break;

  case 1039: /* $@79: %empty  */
#line 5579 "parser.y"
  {
	check_unreached = 0;
  }
#line 10933 "parser.c"
    break;

  case 1040: /* search_when: WHEN condition $@79 statement_list  */
#line 5583 "parser.y"
  {
	yyval = cb_build_if (yyvsp[-2], yyvsp[0], NULL);
  }
#line 10941 "parser.c"
    break;

  case 1041: /* end_search: %empty  */
#line 5589 "parser.y"
                                { terminator_warning (TERM_SEARCH); }
#line 10947 "parser.c"
    break;

  case 1042: /* end_search: "END-SEARCH"  */
#line 5590 "parser.y"
                                { terminator_clear (TERM_SEARCH); }
#line 10953 "parser.c"
    break;

  case 1043: /* $@80: %empty  */
#line 5599 "parser.y"
                                { BEGIN_STATEMENT ("SET", 0); }
#line 10959 "parser.c"
    break;

  case 1050: /* set_environment: ENVIRONMENT simple_value TO simple_value  */
#line 5615 "parser.y"
  {
	cb_emit_setenv (yyvsp[-2], yyvsp[0]);
  }
#line 10967 "parser.c"
    break;

  case 1051: /* set_to: target_x_list TO ENTRY alnum_or_id  */
#line 5624 "parser.y"
  {
	cb_emit_set_to (yyvsp[-3], cb_build_ppointer (yyvsp[0]));
  }
#line 10975 "parser.c"
    break;

  case 1052: /* set_to: target_x_list TO x  */
#line 5628 "parser.y"
  {
	cb_emit_set_to (yyvsp[-2], yyvsp[0]);
  }
#line 10983 "parser.c"
    break;

  case 1053: /* set_up_down: target_x_list up_or_down BY x  */
#line 5637 "parser.y"
  {
	cb_emit_set_up_down (yyvsp[-3], yyvsp[-2], yyvsp[0]);
  }
#line 10991 "parser.c"
    break;

  case 1054: /* up_or_down: UP  */
#line 5643 "parser.y"
                                { yyval = cb_int0; }
#line 10997 "parser.c"
    break;

  case 1055: /* up_or_down: DOWN  */
#line 5644 "parser.y"
                                { yyval = cb_int1; }
#line 11003 "parser.c"
    break;

  case 1058: /* set_to_on_off: mnemonic_name_list TO on_or_off  */
#line 5656 "parser.y"
  {
	cb_emit_set_on_off (yyvsp[-2], yyvsp[0]);
  }
#line 11011 "parser.c"
    break;

  case 1061: /* set_to_true_false: target_x_list TO "TRUE"  */
#line 5670 "parser.y"
  {
	cb_emit_set_true (yyvsp[-2]);
  }
#line 11019 "parser.c"
    break;

  case 1062: /* set_to_true_false: target_x_list TO "FALSE"  */
#line 5674 "parser.y"
  {
	cb_emit_set_false (yyvsp[-2]);
  }
#line 11027 "parser.c"
    break;

  case 1063: /* $@81: %empty  */
#line 5685 "parser.y"
                                { BEGIN_STATEMENT ("SORT", 0); }
#line 11033 "parser.c"
    break;

  case 1065: /* $@82: %empty  */
#line 5691 "parser.y"
  {
	cb_emit_sort_init (yyvsp[-3], yyvsp[-2], yyvsp[0]);
	if (CB_FILE_P (cb_ref (yyvsp[-3])) && yyvsp[-2] == NULL) {
		cb_error (_("File sort requires KEY phrase"));
	}
	/* used in sort_input/sort_output */
	save_tree_1 = yyvsp[-3];
  }
#line 11046 "parser.c"
    break;

  case 1066: /* sort_body: qualified_word sort_key_list sort_duplicates sort_collating $@82 sort_input sort_output  */
#line 5700 "parser.y"
  {
	cb_emit_sort_finish (yyvsp[-6]);
  }
#line 11054 "parser.c"
    break;

  case 1067: /* sort_key_list: %empty  */
#line 5707 "parser.y"
  {
	yyval = NULL;
  }
#line 11062 "parser.c"
    break;

  case 1068: /* sort_key_list: sort_key_list _on ascending_or_descending _key _is opt_key_list  */
#line 5712 "parser.y"
  {
	cb_tree l;

	if (!cb_allow_is_in_sort_key_spec && yyvsp[-1] != NULL) {
		cb_error (_("syntax error"));
		yyval = cb_error_node;
	} else {
		if (yyvsp[0] == NULL) {
			yyvsp[0] = cb_list_init (NULL);
		}
		for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
			CB_PURPOSE (l) = yyvsp[-3];
		}
		yyval = cb_list_append (yyvsp[-5], yyvsp[0]);
	}
  }
#line 11083 "parser.c"
    break;

  case 1069: /* opt_key_list: %empty  */
#line 5731 "parser.y"
                                { yyval = NULL; }
#line 11089 "parser.c"
    break;

  case 1070: /* opt_key_list: opt_key_list qualified_word  */
#line 5732 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 11095 "parser.c"
    break;

  case 1072: /* sort_duplicates: with_dups _in_order  */
#line 5736 "parser.y"
                                { /* nothing */ }
#line 11101 "parser.c"
    break;

  case 1073: /* sort_collating: %empty  */
#line 5740 "parser.y"
                                        { yyval = cb_null; }
#line 11107 "parser.c"
    break;

  case 1074: /* sort_collating: coll_sequence _is reference  */
#line 5741 "parser.y"
                                        { yyval = cb_ref (yyvsp[0]); }
#line 11113 "parser.c"
    break;

  case 1075: /* sort_input: %empty  */
#line 5746 "parser.y"
  {
	if (CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("File sort requires USING or INPUT PROCEDURE"));
	}
  }
#line 11123 "parser.c"
    break;

  case 1076: /* sort_input: USING file_name_list  */
#line 5752 "parser.y"
  {
	if (!CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("USING invalid with table SORT"));
	} else {
		cb_emit_sort_using (save_tree_1, yyvsp[0]);
	}
  }
#line 11135 "parser.c"
    break;

  case 1077: /* sort_input: INPUT PROCEDURE _is perform_procedure  */
#line 5760 "parser.y"
  {
	if (!CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("INPUT PROCEDURE invalid with table SORT"));
	} else {
		cb_emit_sort_input (yyvsp[0], save_tree_1);
	}
  }
#line 11147 "parser.c"
    break;

  case 1078: /* sort_output: %empty  */
#line 5771 "parser.y"
  {
	if (CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("File sort requires GIVING or OUTPUT PROCEDURE"));
	}
  }
#line 11157 "parser.c"
    break;

  case 1079: /* sort_output: GIVING file_name_list  */
#line 5777 "parser.y"
  {
	if (!CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("GIVING invalid with table SORT"));
	} else {
		cb_emit_sort_giving (save_tree_1, yyvsp[0]);
	}
  }
#line 11169 "parser.c"
    break;

  case 1080: /* sort_output: OUTPUT PROCEDURE _is perform_procedure  */
#line 5785 "parser.y"
  {
	if (!CB_FILE_P (cb_ref (save_tree_1))) {
		cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
	} else {
		cb_emit_sort_output (yyvsp[0], save_tree_1);
	}
  }
#line 11181 "parser.c"
    break;

  case 1081: /* $@83: %empty  */
#line 5800 "parser.y"
                                { BEGIN_STATEMENT ("START", TERM_START); }
#line 11187 "parser.c"
    break;

  case 1082: /* @84: %empty  */
#line 5801 "parser.y"
                                { yyval = cb_int (COB_EQ); }
#line 11193 "parser.c"
    break;

  case 1083: /* start_statement: START $@83 file_name @84 start_key opt_invalid_key end_start  */
#line 5804 "parser.y"
  {
	if (CB_FILE_P (cb_ref (yyvsp[-4]))) {
		if (CB_FILE (cb_ref (yyvsp[-4]))->organization != COB_ORG_INDEXED &&
		     CB_FILE (cb_ref (yyvsp[-4]))->organization != COB_ORG_RELATIVE) {
			cb_error (_("START not allowed on SEQUENTIAL files"));
			yyval = cb_error_node;
		} else {
			cb_emit_start (yyvsp[-4], yyvsp[-3], yyvsp[-2]);
		}
	} else {
		cb_error_x (yyvsp[-4], _("'%s' is not a file name"), CB_NAME (yyvsp[-4]));
		yyval = cb_error_node;
	}
  }
#line 11212 "parser.c"
    break;

  case 1084: /* start_key: %empty  */
#line 5821 "parser.y"
                                { yyval = NULL; }
#line 11218 "parser.c"
    break;

  case 1085: /* start_key: KEY _is start_op identifier_list  */
#line 5823 "parser.y"
  {
	yyvsp[-4] = yyvsp[-1];
	yyval = yyvsp[0];
  }
#line 11227 "parser.c"
    break;

  case 1086: /* start_op: flag_not eq  */
#line 5830 "parser.y"
                        { yyval = cb_int ((yyvsp[-1] == cb_int1) ? COB_NE : COB_EQ); }
#line 11233 "parser.c"
    break;

  case 1087: /* start_op: flag_not gt  */
#line 5831 "parser.y"
                        { yyval = cb_int ((yyvsp[-1] == cb_int1) ? COB_LE : COB_GT); }
#line 11239 "parser.c"
    break;

  case 1088: /* start_op: flag_not lt  */
#line 5832 "parser.y"
                        { yyval = cb_int ((yyvsp[-1] == cb_int1) ? COB_GE : COB_LT); }
#line 11245 "parser.c"
    break;

  case 1089: /* start_op: flag_not ge  */
#line 5833 "parser.y"
                        { yyval = cb_int ((yyvsp[-1] == cb_int1) ? COB_LT : COB_GE); }
#line 11251 "parser.c"
    break;

  case 1090: /* start_op: flag_not le  */
#line 5834 "parser.y"
                        { yyval = cb_int ((yyvsp[-1] == cb_int1) ? COB_GT : COB_LE); }
#line 11257 "parser.c"
    break;

  case 1091: /* end_start: %empty  */
#line 5838 "parser.y"
                                { terminator_warning (TERM_START); }
#line 11263 "parser.c"
    break;

  case 1092: /* end_start: "END-START"  */
#line 5839 "parser.y"
                                { terminator_clear (TERM_START); }
#line 11269 "parser.c"
    break;

  case 1093: /* $@85: %empty  */
#line 5848 "parser.y"
                                { BEGIN_STATEMENT ("STOP", 0); }
#line 11275 "parser.c"
    break;

  case 1094: /* stop_statement: STOP RUN $@85 stop_returning  */
#line 5850 "parser.y"
  {
	cb_emit_stop_run (yyvsp[0]);
  }
#line 11283 "parser.c"
    break;

  case 1095: /* $@86: %empty  */
#line 5853 "parser.y"
                                { BEGIN_STATEMENT ("STOP", 0); }
#line 11289 "parser.c"
    break;

  case 1096: /* stop_statement: STOP "Literal" $@86  */
#line 5854 "parser.y"
  {
	cb_verify (cb_stop_literal_statement, "STOP literal");
  }
#line 11297 "parser.c"
    break;

  case 1097: /* stop_returning: %empty  */
#line 5860 "parser.y"
                        { yyval = current_program->cb_return_code; }
#line 11303 "parser.c"
    break;

  case 1098: /* stop_returning: RETURNING x  */
#line 5861 "parser.y"
                        { yyval = yyvsp[0]; }
#line 11309 "parser.c"
    break;

  case 1099: /* stop_returning: GIVING x  */
#line 5862 "parser.y"
                        { yyval = yyvsp[0]; }
#line 11315 "parser.c"
    break;

  case 1100: /* $@87: %empty  */
#line 5871 "parser.y"
                                { BEGIN_STATEMENT ("STRING", TERM_STRING); }
#line 11321 "parser.c"
    break;

  case 1101: /* string_statement: STRING $@87 string_item_list INTO identifier opt_with_pointer on_overflow end_string  */
#line 5874 "parser.y"
  {
	cb_emit_string (yyvsp[-5], yyvsp[-3], yyvsp[-2]);
  }
#line 11329 "parser.c"
    break;

  case 1102: /* string_item_list: string_item  */
#line 5880 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 11335 "parser.c"
    break;

  case 1103: /* string_item_list: string_item_list string_item  */
#line 5881 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 11341 "parser.c"
    break;

  case 1104: /* string_item: x  */
#line 5885 "parser.y"
                                { yyval = yyvsp[0]; }
#line 11347 "parser.c"
    break;

  case 1105: /* string_item: DELIMITED _by SIZE  */
#line 5886 "parser.y"
                                { yyval = cb_build_pair (cb_int0, NULL); }
#line 11353 "parser.c"
    break;

  case 1106: /* string_item: DELIMITED _by x  */
#line 5887 "parser.y"
                                { yyval = cb_build_pair (yyvsp[0], NULL); }
#line 11359 "parser.c"
    break;

  case 1107: /* opt_with_pointer: %empty  */
#line 5891 "parser.y"
                                { yyval = cb_int0; }
#line 11365 "parser.c"
    break;

  case 1108: /* opt_with_pointer: _with POINTER identifier  */
#line 5892 "parser.y"
                                { yyval = yyvsp[0]; }
#line 11371 "parser.c"
    break;

  case 1109: /* end_string: %empty  */
#line 5896 "parser.y"
                                { terminator_warning (TERM_STRING); }
#line 11377 "parser.c"
    break;

  case 1110: /* end_string: "END-STRING"  */
#line 5897 "parser.y"
                                { terminator_clear (TERM_STRING); }
#line 11383 "parser.c"
    break;

  case 1111: /* $@88: %empty  */
#line 5906 "parser.y"
                                { BEGIN_STATEMENT ("SUBTRACT", TERM_SUBTRACT); }
#line 11389 "parser.c"
    break;

  case 1113: /* subtract_body: x_list FROM arithmetic_x_list on_size_error  */
#line 5913 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], '-', cb_build_binary_list (yyvsp[-3], '+'));
  }
#line 11397 "parser.c"
    break;

  case 1114: /* subtract_body: x_list FROM x GIVING arithmetic_x_list on_size_error  */
#line 5917 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_list (cb_cons (yyvsp[-3], yyvsp[-5]), '-'));
  }
#line 11405 "parser.c"
    break;

  case 1115: /* subtract_body: CORRESPONDING identifier FROM identifier flag_rounded on_size_error  */
#line 5921 "parser.y"
  {
	cb_emit_corresponding (cb_build_sub, yyvsp[-2], yyvsp[-4], yyvsp[-1]);
  }
#line 11413 "parser.c"
    break;

  case 1116: /* end_subtract: %empty  */
#line 5927 "parser.y"
                                { terminator_warning (TERM_SUBTRACT); }
#line 11419 "parser.c"
    break;

  case 1117: /* end_subtract: "END-SUBTRACT"  */
#line 5928 "parser.y"
                                { terminator_clear (TERM_SUBTRACT); }
#line 11425 "parser.c"
    break;

  case 1118: /* suppress_statement: SUPPRESS _printing  */
#line 5938 "parser.y"
  {
	BEGIN_STATEMENT ("SUPPRESS", 0);
	PENDING("SUPPRESS");
  }
#line 11434 "parser.c"
    break;

  case 1121: /* $@89: %empty  */
#line 5953 "parser.y"
                                { BEGIN_STATEMENT ("TERMINATE", 0); }
#line 11440 "parser.c"
    break;

  case 1122: /* terminate_statement: TERMINATE $@89 identifier_list  */
#line 5955 "parser.y"
  {
	PENDING("TERMINATE");
  }
#line 11448 "parser.c"
    break;

  case 1123: /* $@90: %empty  */
#line 5966 "parser.y"
                                { BEGIN_STATEMENT ("TRANSFORM", 0); }
#line 11454 "parser.c"
    break;

  case 1124: /* transform_statement: TRANSFORM $@90 identifier FROM simple_value TO simple_all_value  */
#line 5968 "parser.y"
  {
	cb_tree		x;

	x = cb_build_converting (yyvsp[-2], yyvsp[0], cb_build_inspect_region_start ());
	cb_emit_inspect (yyvsp[-4], x, cb_int0, 2);
  }
#line 11465 "parser.c"
    break;

  case 1125: /* $@91: %empty  */
#line 5982 "parser.y"
                                { BEGIN_STATEMENT ("UNLOCK", 0); }
#line 11471 "parser.c"
    break;

  case 1126: /* unlock_statement: UNLOCK $@91 file_name opt_record  */
#line 5984 "parser.y"
  {
	if (yyvsp[-1] != cb_error_node) {
		cb_emit_unlock (yyvsp[-1]);
	}
  }
#line 11481 "parser.c"
    break;

  case 1130: /* $@92: %empty  */
#line 6003 "parser.y"
                                { BEGIN_STATEMENT ("UNSTRING", TERM_UNSTRING); }
#line 11487 "parser.c"
    break;

  case 1131: /* unstring_statement: UNSTRING $@92 identifier unstring_delimited unstring_into opt_with_pointer unstring_tallying on_overflow end_unstring  */
#line 6007 "parser.y"
  {
	cb_emit_unstring (yyvsp[-6], yyvsp[-5], yyvsp[-4], yyvsp[-3], yyvsp[-2]);
  }
#line 11495 "parser.c"
    break;

  case 1132: /* unstring_delimited: %empty  */
#line 6013 "parser.y"
                                { yyval = NULL; }
#line 11501 "parser.c"
    break;

  case 1133: /* unstring_delimited: DELIMITED _by unstring_delimited_list  */
#line 6015 "parser.y"
                                { yyval = yyvsp[0]; }
#line 11507 "parser.c"
    break;

  case 1134: /* unstring_delimited_list: unstring_delimited_item  */
#line 6019 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 11513 "parser.c"
    break;

  case 1135: /* unstring_delimited_list: unstring_delimited_list OR unstring_delimited_item  */
#line 6021 "parser.y"
                                { yyval = cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 11519 "parser.c"
    break;

  case 1136: /* unstring_delimited_item: flag_all simple_value  */
#line 6026 "parser.y"
  {
	yyval = cb_build_unstring_delimited (yyvsp[-1], yyvsp[0]);
  }
#line 11527 "parser.c"
    break;

  case 1137: /* unstring_into: INTO unstring_into_item  */
#line 6032 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 11533 "parser.c"
    break;

  case 1138: /* unstring_into: unstring_into unstring_into_item  */
#line 6034 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 11539 "parser.c"
    break;

  case 1139: /* unstring_into_item: identifier unstring_into_delimiter unstring_into_count  */
#line 6039 "parser.y"
  {
	yyval = cb_build_unstring_into (yyvsp[-2], yyvsp[-1], yyvsp[0]);
  }
#line 11547 "parser.c"
    break;

  case 1140: /* unstring_into_delimiter: %empty  */
#line 6045 "parser.y"
                                { yyval = NULL; }
#line 11553 "parser.c"
    break;

  case 1141: /* unstring_into_delimiter: DELIMITER _in identifier  */
#line 6046 "parser.y"
                                { yyval = yyvsp[0]; }
#line 11559 "parser.c"
    break;

  case 1142: /* unstring_into_count: %empty  */
#line 6050 "parser.y"
                                { yyval = NULL; }
#line 11565 "parser.c"
    break;

  case 1143: /* unstring_into_count: COUNT _in identifier  */
#line 6051 "parser.y"
                                { yyval = yyvsp[0]; }
#line 11571 "parser.c"
    break;

  case 1144: /* unstring_tallying: %empty  */
#line 6055 "parser.y"
                                { yyval = NULL; }
#line 11577 "parser.c"
    break;

  case 1145: /* unstring_tallying: TALLYING _in identifier  */
#line 6056 "parser.y"
                                { yyval = yyvsp[0]; }
#line 11583 "parser.c"
    break;

  case 1146: /* end_unstring: %empty  */
#line 6060 "parser.y"
                                { terminator_warning (TERM_UNSTRING); }
#line 11589 "parser.c"
    break;

  case 1147: /* end_unstring: "END-UNSTRING"  */
#line 6061 "parser.y"
                                { terminator_clear (TERM_UNSTRING); }
#line 11595 "parser.c"
    break;

  case 1151: /* use_exception: USE use_global _after _standard exception_or_error _procedure _on use_exception_target  */
#line 6079 "parser.y"
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
#line 11617 "parser.c"
    break;

  case 1152: /* use_global: %empty  */
#line 6100 "parser.y"
  {
	use_global_ind = 0;
  }
#line 11625 "parser.c"
    break;

  case 1153: /* use_global: GLOBAL  */
#line 6104 "parser.y"
  {
	use_global_ind = 1;
	current_program->flag_global_use = 1;
  }
#line 11634 "parser.c"
    break;

  case 1154: /* use_exception_target: file_name_list  */
#line 6112 "parser.y"
  {
	cb_tree		l;

	for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) != cb_error_node) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
  }
#line 11648 "parser.c"
    break;

  case 1155: /* use_exception_target: INPUT  */
#line 6122 "parser.y"
  {
	current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  }
#line 11657 "parser.c"
    break;

  case 1156: /* use_exception_target: OUTPUT  */
#line 6127 "parser.y"
  {
	current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  }
#line 11666 "parser.c"
    break;

  case 1157: /* use_exception_target: "I-O"  */
#line 6132 "parser.y"
  {
	current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
	current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  }
#line 11675 "parser.c"
    break;

  case 1158: /* use_exception_target: EXTEND  */
#line 6137 "parser.y"
  {
	current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
	current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  }
#line 11684 "parser.c"
    break;

  case 1171: /* use_debugging: USE _for DEBUGGING _on use_debugging_target  */
#line 6169 "parser.y"
  {
	PENDING ("USE FOR DEBUGGING");
  }
#line 11692 "parser.c"
    break;

  case 1174: /* use_reporting: USE use_global BEFORE REPORTING identifier  */
#line 6181 "parser.y"
  {
	PENDING ("USE BEFORE REPORTING");
  }
#line 11700 "parser.c"
    break;

  case 1175: /* $@93: %empty  */
#line 6192 "parser.y"
                                { BEGIN_STATEMENT ("WRITE", TERM_WRITE); }
#line 11706 "parser.c"
    break;

  case 1176: /* write_statement: WRITE $@93 record_name write_from write_lock write_option write_handler end_write  */
#line 6195 "parser.y"
  {
	if (yyvsp[-5] != cb_error_node) {
		if (cb_use_invalidkey_handler_on_status34 &&
		    current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
		    (CB_FIELD(cb_ref (yyvsp[-5]))->file->organization != COB_ORG_RELATIVE &&
		     CB_FIELD(cb_ref (yyvsp[-5]))->file->organization != COB_ORG_INDEXED)) {
			current_statement->handler_id = COB_EC_I_O_PERMANENT_ERROR;
		}
		cb_emit_write (yyvsp[-5], yyvsp[-4], yyvsp[-2], yyvsp[-3]);
	}
  }
#line 11722 "parser.c"
    break;

  case 1177: /* write_from: %empty  */
#line 6209 "parser.y"
                                { yyval = NULL; }
#line 11728 "parser.c"
    break;

  case 1178: /* write_from: FROM id_or_lit  */
#line 6210 "parser.y"
                                { yyval = yyvsp[0]; }
#line 11734 "parser.c"
    break;

  case 1179: /* write_option: %empty  */
#line 6215 "parser.y"
  {
	yyval = cb_int0;
  }
#line 11742 "parser.c"
    break;

  case 1180: /* write_option: before_or_after _advancing num_id_or_lit _line_or_lines  */
#line 6219 "parser.y"
  {
	yyval = cb_build_write_advancing_lines (yyvsp[-3], yyvsp[-1]);
  }
#line 11750 "parser.c"
    break;

  case 1181: /* write_option: before_or_after _advancing mnemonic_name  */
#line 6223 "parser.y"
  {
	yyval = cb_build_write_advancing_mnemonic (yyvsp[-2], yyvsp[0]);
  }
#line 11758 "parser.c"
    break;

  case 1182: /* write_option: before_or_after _advancing PAGE  */
#line 6227 "parser.y"
  {
	yyval = cb_build_write_advancing_page (yyvsp[-2]);
  }
#line 11766 "parser.c"
    break;

  case 1183: /* before_or_after: BEFORE  */
#line 6233 "parser.y"
                                { yyval = CB_BEFORE; }
#line 11772 "parser.c"
    break;

  case 1184: /* before_or_after: AFTER  */
#line 6234 "parser.y"
                                { yyval = CB_AFTER; }
#line 11778 "parser.c"
    break;

  case 1188: /* end_write: %empty  */
#line 6243 "parser.y"
                                { terminator_warning (TERM_WRITE); }
#line 11784 "parser.c"
    break;

  case 1189: /* end_write: "END-WRITE"  */
#line 6244 "parser.y"
                                { terminator_clear (TERM_WRITE); }
#line 11790 "parser.c"
    break;

  case 1190: /* on_accp_exception: opt_on_exception opt_not_on_exception  */
#line 6259 "parser.y"
  {
	current_statement->handler_id = COB_EC_IMP_ACCEPT;
  }
#line 11798 "parser.c"
    break;

  case 1191: /* on_disp_exception: opt_on_exception opt_not_on_exception  */
#line 6267 "parser.y"
  {
	current_statement->handler_id = COB_EC_IMP_DISPLAY;
  }
#line 11806 "parser.c"
    break;

  case 1193: /* $@94: %empty  */
#line 6274 "parser.y"
  {
	check_unreached = 0;
  }
#line 11814 "parser.c"
    break;

  case 1194: /* opt_on_exception: EXCEPTION $@94 statement_list  */
#line 6278 "parser.y"
  {
	current_statement->handler1 = yyvsp[0];
  }
#line 11822 "parser.c"
    break;

  case 1196: /* $@95: %empty  */
#line 6285 "parser.y"
  {
	check_unreached = 0;
  }
#line 11830 "parser.c"
    break;

  case 1197: /* opt_not_on_exception: "NOT EXCEPTION" $@95 statement_list  */
#line 6289 "parser.y"
  {
	current_statement->handler2 = yyvsp[0];
  }
#line 11838 "parser.c"
    break;

  case 1200: /* $@96: %empty  */
#line 6305 "parser.y"
  {
	check_unreached = 0;
	current_statement->handler_id = COB_EC_SIZE;
  }
#line 11847 "parser.c"
    break;

  case 1201: /* opt_on_size_error: "SIZE ERROR" $@96 statement_list  */
#line 6310 "parser.y"
  {
	current_statement->handler1 = yyvsp[0];
  }
#line 11855 "parser.c"
    break;

  case 1203: /* $@97: %empty  */
#line 6317 "parser.y"
  {
	check_unreached = 0;
	current_statement->handler_id = COB_EC_SIZE;
  }
#line 11864 "parser.c"
    break;

  case 1204: /* opt_not_on_size_error: "NOT SIZE ERROR" $@97 statement_list  */
#line 6322 "parser.y"
  {
	current_statement->handler2 = yyvsp[0];
  }
#line 11872 "parser.c"
    break;

  case 1205: /* on_overflow: opt_on_overflow opt_not_on_overflow  */
#line 6334 "parser.y"
  {
	current_statement->handler_id = COB_EC_OVERFLOW;
  }
#line 11880 "parser.c"
    break;

  case 1207: /* $@98: %empty  */
#line 6341 "parser.y"
  {
	check_unreached = 0;
  }
#line 11888 "parser.c"
    break;

  case 1208: /* opt_on_overflow: OVERFLOW $@98 statement_list  */
#line 6345 "parser.y"
  {
	current_statement->handler1 = yyvsp[0];
  }
#line 11896 "parser.c"
    break;

  case 1210: /* $@99: %empty  */
#line 6352 "parser.y"
  {
	check_unreached = 0;
  }
#line 11904 "parser.c"
    break;

  case 1211: /* opt_not_on_overflow: "NOT OVERFLOW" $@99 statement_list  */
#line 6356 "parser.y"
  {
	current_statement->handler2 = yyvsp[0];
  }
#line 11912 "parser.c"
    break;

  case 1212: /* at_end: at_end_sentence  */
#line 6368 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_AT_END;
	current_statement->handler1 = yyvsp[0];
  }
#line 11921 "parser.c"
    break;

  case 1213: /* at_end: not_at_end_sentence  */
#line 6373 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_AT_END;
	current_statement->handler2 = yyvsp[0];
  }
#line 11930 "parser.c"
    break;

  case 1214: /* at_end: at_end_sentence not_at_end_sentence  */
#line 6378 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_AT_END;
	current_statement->handler1 = yyvsp[-1];
	current_statement->handler2 = yyvsp[0];
  }
#line 11940 "parser.c"
    break;

  case 1215: /* $@100: %empty  */
#line 6387 "parser.y"
  {
	check_unreached = 0;
  }
#line 11948 "parser.c"
    break;

  case 1216: /* at_end_sentence: END $@100 statement_list  */
#line 6391 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 11956 "parser.c"
    break;

  case 1217: /* $@101: %empty  */
#line 6398 "parser.y"
  {
	check_unreached = 0;
  }
#line 11964 "parser.c"
    break;

  case 1218: /* not_at_end_sentence: "NOT END" $@101 statement_list  */
#line 6402 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 11972 "parser.c"
    break;

  case 1219: /* at_eop: at_eop_sentence  */
#line 6414 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_EOP;
	current_statement->handler1 = yyvsp[0];
  }
#line 11981 "parser.c"
    break;

  case 1220: /* at_eop: not_at_eop_sentence  */
#line 6419 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_EOP;
	current_statement->handler2 = yyvsp[0];
  }
#line 11990 "parser.c"
    break;

  case 1221: /* at_eop: at_eop_sentence not_at_eop_sentence  */
#line 6424 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_EOP;
	current_statement->handler1 = yyvsp[-1];
	current_statement->handler2 = yyvsp[0];
  }
#line 12000 "parser.c"
    break;

  case 1222: /* $@102: %empty  */
#line 6433 "parser.y"
  {
	check_unreached = 0;
  }
#line 12008 "parser.c"
    break;

  case 1223: /* at_eop_sentence: EOP $@102 statement_list  */
#line 6437 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 12016 "parser.c"
    break;

  case 1224: /* $@103: %empty  */
#line 6444 "parser.y"
  {
	check_unreached = 0;
  }
#line 12024 "parser.c"
    break;

  case 1225: /* not_at_eop_sentence: "NOT EOP" $@103 statement_list  */
#line 6448 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 12032 "parser.c"
    break;

  case 1228: /* invalid_key: invalid_key_sentence  */
#line 6464 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
	current_statement->handler1 = yyvsp[0];
  }
#line 12041 "parser.c"
    break;

  case 1229: /* invalid_key: not_invalid_key_sentence  */
#line 6469 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
	current_statement->handler2 = yyvsp[0];
  }
#line 12050 "parser.c"
    break;

  case 1230: /* invalid_key: invalid_key_sentence not_invalid_key_sentence  */
#line 6474 "parser.y"
  {
	current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
	current_statement->handler1 = yyvsp[-1];
	current_statement->handler2 = yyvsp[0];
  }
#line 12060 "parser.c"
    break;

  case 1231: /* $@104: %empty  */
#line 6483 "parser.y"
  {
	check_unreached = 0;
  }
#line 12068 "parser.c"
    break;

  case 1232: /* invalid_key_sentence: "INVALID KEY" $@104 statement_list  */
#line 6487 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 12076 "parser.c"
    break;

  case 1233: /* $@105: %empty  */
#line 6494 "parser.y"
  {
	check_unreached = 0;
  }
#line 12084 "parser.c"
    break;

  case 1234: /* not_invalid_key_sentence: "NOT INVALID KEY" $@105 statement_list  */
#line 6498 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 12092 "parser.c"
    break;

  case 1235: /* _opt_scroll_lines: %empty  */
#line 6510 "parser.y"
  {
	yyval = cb_one;
  }
#line 12100 "parser.c"
    break;

  case 1236: /* _opt_scroll_lines: _by num_id_or_lit _line_or_lines  */
#line 6514 "parser.y"
  {
	yyval = yyvsp[-1];
  }
#line 12108 "parser.c"
    break;

  case 1237: /* condition: expr  */
#line 6526 "parser.y"
  {
	yyval = cb_build_cond (yyvsp[0]);
  }
#line 12116 "parser.c"
    break;

  case 1238: /* expr: partial_expr  */
#line 6533 "parser.y"
  {
	yyval = cb_build_expr (yyvsp[0]);
  }
#line 12124 "parser.c"
    break;

  case 1239: /* $@106: %empty  */
#line 6539 "parser.y"
  {
	current_expr = NULL;
  }
#line 12132 "parser.c"
    break;

  case 1240: /* partial_expr: $@106 expr_tokens  */
#line 6543 "parser.y"
  {
	yyval = cb_list_reverse (current_expr);
  }
#line 12140 "parser.c"
    break;

  case 1241: /* expr_tokens: expr_token x  */
#line 6549 "parser.y"
                        { push_expr ('x', yyvsp[0]); }
#line 12146 "parser.c"
    break;

  case 1242: /* expr_tokens: expr_tokens ')'  */
#line 6550 "parser.y"
                        { push_expr (')', NULL); }
#line 12152 "parser.c"
    break;

  case 1243: /* expr_tokens: expr_token OMITTED  */
#line 6552 "parser.y"
                                { push_expr ('O', NULL); }
#line 12158 "parser.c"
    break;

  case 1244: /* expr_tokens: expr_token NUMERIC  */
#line 6553 "parser.y"
                                { push_expr ('9', NULL); }
#line 12164 "parser.c"
    break;

  case 1245: /* expr_tokens: expr_token ALPHABETIC  */
#line 6554 "parser.y"
                                { push_expr ('A', NULL); }
#line 12170 "parser.c"
    break;

  case 1246: /* expr_tokens: expr_token "ALPHABETIC-LOWER"  */
#line 6555 "parser.y"
                                { push_expr ('L', NULL); }
#line 12176 "parser.c"
    break;

  case 1247: /* expr_tokens: expr_token "ALPHABETIC-UPPER"  */
#line 6556 "parser.y"
                                { push_expr ('U', NULL); }
#line 12182 "parser.c"
    break;

  case 1248: /* expr_tokens: expr_token CLASS_NAME  */
#line 6557 "parser.y"
                                { push_expr ('x', yyvsp[0]); }
#line 12188 "parser.c"
    break;

  case 1249: /* expr_tokens: expr_tokens OMITTED  */
#line 6559 "parser.y"
                                        { push_expr ('O', NULL); }
#line 12194 "parser.c"
    break;

  case 1250: /* expr_tokens: expr_tokens NUMERIC  */
#line 6560 "parser.y"
                                        { push_expr ('9', NULL); }
#line 12200 "parser.c"
    break;

  case 1251: /* expr_tokens: expr_tokens ALPHABETIC  */
#line 6561 "parser.y"
                                        { push_expr ('A', NULL); }
#line 12206 "parser.c"
    break;

  case 1252: /* expr_tokens: expr_tokens "ALPHABETIC-LOWER"  */
#line 6562 "parser.y"
                                        { push_expr ('L', NULL); }
#line 12212 "parser.c"
    break;

  case 1253: /* expr_tokens: expr_tokens "ALPHABETIC-UPPER"  */
#line 6563 "parser.y"
                                        { push_expr ('U', NULL); }
#line 12218 "parser.c"
    break;

  case 1254: /* expr_tokens: expr_tokens CLASS_NAME  */
#line 6564 "parser.y"
                                        { push_expr ('x', yyvsp[0]); }
#line 12224 "parser.c"
    break;

  case 1255: /* expr_tokens: expr_token POSITIVE  */
#line 6566 "parser.y"
                        { push_expr ('P', NULL); }
#line 12230 "parser.c"
    break;

  case 1256: /* expr_tokens: expr_token NEGATIVE  */
#line 6567 "parser.y"
                        { push_expr ('N', NULL); }
#line 12236 "parser.c"
    break;

  case 1257: /* expr_tokens: expr_tokens POSITIVE  */
#line 6569 "parser.y"
                        { push_expr ('P', NULL); }
#line 12242 "parser.c"
    break;

  case 1258: /* expr_tokens: expr_tokens NEGATIVE  */
#line 6570 "parser.y"
                        { push_expr ('N', NULL); }
#line 12248 "parser.c"
    break;

  case 1259: /* expr_tokens: expr_tokens ZERO  */
#line 6571 "parser.y"
                        { push_expr ('x', cb_zero); }
#line 12254 "parser.c"
    break;

  case 1263: /* expr_token: expr_token '('  */
#line 6578 "parser.y"
                        { push_expr ('(', NULL); }
#line 12260 "parser.c"
    break;

  case 1264: /* expr_token: expr_token '+'  */
#line 6580 "parser.y"
                        { push_expr ('+', NULL); }
#line 12266 "parser.c"
    break;

  case 1265: /* expr_token: expr_token '-'  */
#line 6581 "parser.y"
                        { push_expr ('-', NULL); }
#line 12272 "parser.c"
    break;

  case 1266: /* expr_token: expr_token '^'  */
#line 6582 "parser.y"
                        { push_expr ('^', NULL); }
#line 12278 "parser.c"
    break;

  case 1267: /* expr_token: expr_token NOT  */
#line 6584 "parser.y"
                        { push_expr ('!', NULL); }
#line 12284 "parser.c"
    break;

  case 1268: /* expr_token: expr_tokens NOT  */
#line 6585 "parser.y"
                        { push_expr ('!', NULL); }
#line 12290 "parser.c"
    break;

  case 1269: /* expr_token: expr_tokens '+'  */
#line 6587 "parser.y"
                        { push_expr ('+', NULL); }
#line 12296 "parser.c"
    break;

  case 1270: /* expr_token: expr_tokens '-'  */
#line 6588 "parser.y"
                        { push_expr ('-', NULL); }
#line 12302 "parser.c"
    break;

  case 1271: /* expr_token: expr_tokens '*'  */
#line 6589 "parser.y"
                        { push_expr ('*', NULL); }
#line 12308 "parser.c"
    break;

  case 1272: /* expr_token: expr_tokens '/'  */
#line 6590 "parser.y"
                        { push_expr ('/', NULL); }
#line 12314 "parser.c"
    break;

  case 1273: /* expr_token: expr_tokens '^'  */
#line 6591 "parser.y"
                        { push_expr ('^', NULL); }
#line 12320 "parser.c"
    break;

  case 1274: /* expr_token: expr_tokens eq  */
#line 6593 "parser.y"
                        { push_expr ('=', NULL); }
#line 12326 "parser.c"
    break;

  case 1275: /* expr_token: expr_tokens gt  */
#line 6594 "parser.y"
                        { push_expr ('>', NULL); }
#line 12332 "parser.c"
    break;

  case 1276: /* expr_token: expr_tokens lt  */
#line 6595 "parser.y"
                        { push_expr ('<', NULL); }
#line 12338 "parser.c"
    break;

  case 1277: /* expr_token: expr_tokens ge  */
#line 6596 "parser.y"
                        { push_expr (']', NULL); }
#line 12344 "parser.c"
    break;

  case 1278: /* expr_token: expr_tokens le  */
#line 6597 "parser.y"
                        { push_expr ('[', NULL); }
#line 12350 "parser.c"
    break;

  case 1279: /* expr_token: expr_tokens NE  */
#line 6598 "parser.y"
                        { push_expr ('~', NULL); }
#line 12356 "parser.c"
    break;

  case 1280: /* expr_token: expr_token eq  */
#line 6600 "parser.y"
                        { push_expr ('=', NULL); }
#line 12362 "parser.c"
    break;

  case 1281: /* expr_token: expr_token gt  */
#line 6601 "parser.y"
                        { push_expr ('>', NULL); }
#line 12368 "parser.c"
    break;

  case 1282: /* expr_token: expr_token lt  */
#line 6602 "parser.y"
                        { push_expr ('<', NULL); }
#line 12374 "parser.c"
    break;

  case 1283: /* expr_token: expr_token ge  */
#line 6603 "parser.y"
                        { push_expr (']', NULL); }
#line 12380 "parser.c"
    break;

  case 1284: /* expr_token: expr_token le  */
#line 6604 "parser.y"
                        { push_expr ('[', NULL); }
#line 12386 "parser.c"
    break;

  case 1285: /* expr_token: expr_token NE  */
#line 6605 "parser.y"
                        { push_expr ('~', NULL); }
#line 12392 "parser.c"
    break;

  case 1286: /* expr_token: expr_tokens AND  */
#line 6607 "parser.y"
                        { push_expr ('&', NULL); }
#line 12398 "parser.c"
    break;

  case 1287: /* expr_token: expr_tokens OR  */
#line 6608 "parser.y"
                        { push_expr ('|', NULL); }
#line 12404 "parser.c"
    break;

  case 1301: /* exp_list: exp  */
#line 6620 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 12410 "parser.c"
    break;

  case 1302: /* exp_list: exp_list e_sep exp  */
#line 6621 "parser.y"
                                { yyval = cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 12416 "parser.c"
    break;

  case 1306: /* exp: arith_x  */
#line 6630 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12422 "parser.c"
    break;

  case 1307: /* exp: exp '+' exp  */
#line 6631 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '+', yyvsp[0]); }
#line 12428 "parser.c"
    break;

  case 1308: /* exp: exp '-' exp  */
#line 6632 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '-', yyvsp[0]); }
#line 12434 "parser.c"
    break;

  case 1309: /* exp: exp '*' exp  */
#line 6633 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '*', yyvsp[0]); }
#line 12440 "parser.c"
    break;

  case 1310: /* exp: exp '/' exp  */
#line 6634 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '/', yyvsp[0]); }
#line 12446 "parser.c"
    break;

  case 1311: /* exp: '+' exp  */
#line 6635 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12452 "parser.c"
    break;

  case 1312: /* exp: '-' exp  */
#line 6636 "parser.y"
                                { yyval = cb_build_binary_op (cb_zero, '-', yyvsp[0]); }
#line 12458 "parser.c"
    break;

  case 1313: /* exp: exp '^' exp  */
#line 6637 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '^', yyvsp[0]); }
#line 12464 "parser.c"
    break;

  case 1314: /* exp: '(' exp ')'  */
#line 6638 "parser.y"
                                { yyval = yyvsp[-1]; }
#line 12470 "parser.c"
    break;

  case 1315: /* linage_counter: "LINAGE-COUNTER"  */
#line 6650 "parser.y"
  {
	if (current_linage > 1) {
		cb_error (_("LINAGE-COUNTER must be qualified here"));
		yyval = cb_error_node;
	} else if (current_linage == 0) {
		cb_error (_("Invalid LINAGE-COUNTER usage"));
		yyval = cb_error_node;
	} else {
		yyval = linage_file->linage_ctr;
	}
  }
#line 12486 "parser.c"
    break;

  case 1316: /* linage_counter: "LINAGE-COUNTER" in_of "Identifier"  */
#line 6662 "parser.y"
  {
	if (CB_FILE_P (cb_ref (yyvsp[0]))) {
		yyval = CB_FILE (cb_ref (yyvsp[0]))->linage_ctr;
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a file name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 12499 "parser.c"
    break;

  case 1317: /* arithmetic_x_list: arithmetic_x  */
#line 6676 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12505 "parser.c"
    break;

  case 1318: /* arithmetic_x_list: arithmetic_x_list arithmetic_x  */
#line 6678 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 12511 "parser.c"
    break;

  case 1319: /* arithmetic_x: x flag_rounded  */
#line 6682 "parser.y"
                                { yyval = cb_build_pair (yyvsp[0], yyvsp[-1]); }
#line 12517 "parser.c"
    break;

  case 1320: /* record_name: qualified_word  */
#line 6689 "parser.y"
  {
	cb_tree x;
	cb_tree r;

	if ((x = cb_build_identifier (yyvsp[0])) != cb_error_node &&
	    (r = cb_ref (x)) != cb_error_node) {
		if (!CB_FIELD_P(r)) {
			cb_error_x (x, _("Field name expected."));
			x = cb_error_node;
		} else if (!CB_FIELD(r)->file) {
			cb_error_x (x, _("Record name expected."));
			x = cb_error_node;
		}
	}
	yyval = x;
  }
#line 12538 "parser.c"
    break;

  case 1321: /* table_name: qualified_word  */
#line 6711 "parser.y"
  {
	cb_tree x;

	x = cb_ref (yyvsp[0]);
	if (!CB_FIELD_P (x)) {
		yyval = cb_error_node;
	} else if (!CB_FIELD (x)->index_list) {
		cb_error_x (yyvsp[0], _("'%s' not indexed"), cb_name (yyvsp[0]));
		cb_error_x (x, _("'%s' defined here"), cb_name (x));
		yyval = cb_error_node;
	} else {
		yyval = yyvsp[0];
	}
  }
#line 12557 "parser.c"
    break;

  case 1322: /* file_name_list: file_name  */
#line 6731 "parser.y"
  {
	yyval = cb_list_init (yyvsp[0]);
  }
#line 12565 "parser.c"
    break;

  case 1323: /* file_name_list: file_name_list file_name  */
#line 6735 "parser.y"
  {
	cb_tree		l;

	if (yyvsp[0] != cb_error_node) {
		for (l = yyvsp[-1]; l; l = CB_CHAIN (l)) {
			if (!strcasecmp (CB_NAME (yyvsp[0]), CB_NAME (CB_VALUE (l)))) {
				cb_error_x (yyvsp[0], _("Multiple reference to '%s' "), CB_NAME (yyvsp[0]));
			}
		}
		yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
	}
  }
#line 12582 "parser.c"
    break;

  case 1324: /* file_name: "Identifier"  */
#line 6751 "parser.y"
  {
	if (CB_FILE_P (cb_ref (yyvsp[0]))) {
		yyval = yyvsp[0];
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a file name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 12595 "parser.c"
    break;

  case 1325: /* mnemonic_name_list: mnemonic_name  */
#line 6764 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 12601 "parser.c"
    break;

  case 1326: /* mnemonic_name_list: mnemonic_name_list mnemonic_name  */
#line 6766 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 12607 "parser.c"
    break;

  case 1327: /* mnemonic_name: "MNEMONIC NAME"  */
#line 6770 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12613 "parser.c"
    break;

  case 1328: /* procedure_name_list: %empty  */
#line 6776 "parser.y"
                                { yyval = NULL; }
#line 12619 "parser.c"
    break;

  case 1329: /* procedure_name_list: procedure_name_list procedure_name  */
#line 6778 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 12625 "parser.c"
    break;

  case 1330: /* procedure_name: label  */
#line 6783 "parser.y"
  {
	yyval = yyvsp[0];
	CB_REFERENCE (yyval)->offset = CB_TREE (current_section);
	current_program->label_list = cb_cons (yyval, current_program->label_list);
  }
#line 12635 "parser.c"
    break;

  case 1334: /* integer_label: "Literal"  */
#line 6798 "parser.y"
  {
	yyval = cb_build_reference ((char *)(CB_LITERAL (yyvsp[0])->data));
	yyval->source_file = yyvsp[0]->source_file;
	yyval->source_line = yyvsp[0]->source_line;
  }
#line 12645 "parser.c"
    break;

  case 1335: /* reference_list: reference  */
#line 6808 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 12651 "parser.c"
    break;

  case 1336: /* reference_list: reference_list reference  */
#line 6809 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 12657 "parser.c"
    break;

  case 1337: /* reference: qualified_word  */
#line 6814 "parser.y"
  {
	yyval = yyvsp[0];
	current_program->reference_list = cb_cons (yyval, current_program->reference_list);
  }
#line 12666 "parser.c"
    break;

  case 1338: /* no_reference_list: qualified_word  */
#line 6823 "parser.y"
                                        { yyval = cb_list_init (yyvsp[0]); }
#line 12672 "parser.c"
    break;

  case 1339: /* no_reference_list: no_reference_list qualified_word  */
#line 6824 "parser.y"
                                        { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 12678 "parser.c"
    break;

  case 1340: /* opt_reference: %empty  */
#line 6828 "parser.y"
                                { yyval = NULL; }
#line 12684 "parser.c"
    break;

  case 1341: /* opt_reference: reference  */
#line 6829 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12690 "parser.c"
    break;

  case 1344: /* undefined_word: "Identifier"  */
#line 6841 "parser.y"
  {
	yyval = yyvsp[0];
	if (CB_REFERENCE (yyval)->word->count > 0) {
		redefinition_error (yyval);
		yyval = cb_error_node;
	}
  }
#line 12702 "parser.c"
    break;

  case 1345: /* target_x_list: target_x  */
#line 6860 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 12708 "parser.c"
    break;

  case 1346: /* target_x_list: target_x_list target_x  */
#line 6861 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 12714 "parser.c"
    break;

  case 1348: /* target_x: ADDRESS _of identifier_1  */
#line 6866 "parser.y"
                                { yyval = cb_build_address (yyvsp[0]); }
#line 12720 "parser.c"
    break;

  case 1349: /* x_list: x  */
#line 6870 "parser.y"
                                { yyval = cb_list_init (yyvsp[0]); }
#line 12726 "parser.c"
    break;

  case 1350: /* x_list: x_list x  */
#line 6871 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 12732 "parser.c"
    break;

  case 1352: /* x: LENGTH _of identifier_1  */
#line 6876 "parser.y"
                                                { yyval = cb_build_length (yyvsp[0]); }
#line 12738 "parser.c"
    break;

  case 1353: /* x: LENGTH _of basic_literal  */
#line 6877 "parser.y"
                                                { yyval = cb_build_length (yyvsp[0]); }
#line 12744 "parser.c"
    break;

  case 1354: /* x: LENGTH _of function  */
#line 6878 "parser.y"
                                                { yyval = cb_build_length (yyvsp[0]); }
#line 12750 "parser.c"
    break;

  case 1355: /* x: ADDRESS _of prog_or_entry alnum_or_id  */
#line 6879 "parser.y"
                                                { yyval = cb_build_ppointer (yyvsp[0]); }
#line 12756 "parser.c"
    break;

  case 1356: /* x: ADDRESS _of identifier_1  */
#line 6880 "parser.y"
                                                { yyval = cb_build_address (yyvsp[0]); }
#line 12762 "parser.c"
    break;

  case 1361: /* arith_x: LENGTH _of identifier_1  */
#line 6888 "parser.y"
                                                { yyval = cb_build_length (yyvsp[0]); }
#line 12768 "parser.c"
    break;

  case 1362: /* arith_x: LENGTH _of basic_literal  */
#line 6889 "parser.y"
                                                { yyval = cb_build_length (yyvsp[0]); }
#line 12774 "parser.c"
    break;

  case 1363: /* arith_x: LENGTH _of function  */
#line 6890 "parser.y"
                                                { yyval = cb_build_length (yyvsp[0]); }
#line 12780 "parser.c"
    break;

  case 1369: /* alnum_or_id: identifier_1  */
#line 6902 "parser.y"
                        { yyval = yyvsp[0]; }
#line 12786 "parser.c"
    break;

  case 1370: /* alnum_or_id: "Literal"  */
#line 6903 "parser.y"
                        { yyval = yyvsp[0]; }
#line 12792 "parser.c"
    break;

  case 1382: /* num_id_or_lit: ZERO  */
#line 6937 "parser.y"
                                { yyval = cb_zero; }
#line 12798 "parser.c"
    break;

  case 1383: /* identifier: identifier_1  */
#line 6945 "parser.y"
                                { yyval = cb_build_identifier (yyvsp[0]); }
#line 12804 "parser.c"
    break;

  case 1384: /* identifier_1: qualified_word  */
#line 6949 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12810 "parser.c"
    break;

  case 1385: /* identifier_1: qualified_word subref  */
#line 6950 "parser.y"
                                { yyval = yyvsp[-1]; }
#line 12816 "parser.c"
    break;

  case 1386: /* identifier_1: qualified_word refmod  */
#line 6951 "parser.y"
                                { yyval = yyvsp[-1]; }
#line 12822 "parser.c"
    break;

  case 1387: /* identifier_1: qualified_word subref refmod  */
#line 6952 "parser.y"
                                { yyval = yyvsp[-2]; }
#line 12828 "parser.c"
    break;

  case 1388: /* qualified_word: "Identifier"  */
#line 6956 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12834 "parser.c"
    break;

  case 1389: /* qualified_word: "Identifier" in_of qualified_word  */
#line 6957 "parser.y"
                                { yyval = yyvsp[-2]; CB_REFERENCE (yyvsp[-2])->chain = yyvsp[0]; }
#line 12840 "parser.c"
    break;

  case 1390: /* subref: '(' exp_list ')'  */
#line 6962 "parser.y"
  {
	if (cb_ref (yyvsp[-3]) != cb_error_node) {
		yyval = yyvsp[-3];
		CB_REFERENCE (yyvsp[-3])->subs = cb_list_reverse (yyvsp[-1]);
	}
  }
#line 12851 "parser.c"
    break;

  case 1391: /* refmod: '(' exp ':' ')'  */
#line 6972 "parser.y"
  {
	if (cb_ref (yyvsp[-4]) != cb_error_node) {
		CB_REFERENCE (yyvsp[-4])->value = CB_TREE (cb_field (yyvsp[-4]));
		if (cb_tree_category (yyvsp[-4]) == CB_CATEGORY_NATIONAL ||
		    cb_tree_category (yyvsp[-4]) == CB_CATEGORY_NATIONAL_EDITED) {
#ifdef	I18N_UTF8
			/* I18N_UTF8: No wide char support. */
#else /*!I18N_UTF8*/
			yyvsp[-2] = cb_build_binary_op (yyvsp[-2], '*', cb_int2);
			yyvsp[-2] = cb_build_binary_op (yyvsp[-2], '-', cb_int1);
#endif /*I18N_UTF8*/
		} else {
			CB_TREE (yyvsp[-4])->category = CB_CATEGORY_ALPHANUMERIC;
		}
		CB_REFERENCE (yyvsp[-4])->offset = yyvsp[-2];
	}
  }
#line 12873 "parser.c"
    break;

  case 1392: /* refmod: '(' exp ':' exp ')'  */
#line 6990 "parser.y"
  {
	if (cb_ref (yyvsp[-5]) != cb_error_node) {
		CB_REFERENCE (yyvsp[-5])->value = CB_TREE (cb_field (yyvsp[-5]));
		if (cb_tree_category (yyvsp[-5]) == CB_CATEGORY_NATIONAL ||
		    cb_tree_category (yyvsp[-5]) == CB_CATEGORY_NATIONAL_EDITED) {
#ifdef	I18N_UTF8
			/* I18N_UTF8: No wide char support. */
#else /*!I18N_UTF8*/
			yyvsp[-3] = cb_build_binary_op (yyvsp[-3], '*', cb_int2);
			yyvsp[-3] = cb_build_binary_op (yyvsp[-3], '-', cb_int1);
			yyvsp[-1] = cb_build_binary_op (yyvsp[-1], '*', cb_int2);
#endif /*I18N_UTF8*/
		} else {
			CB_TREE (yyvsp[-5])->category = CB_CATEGORY_ALPHANUMERIC;
		}
		CB_REFERENCE (yyvsp[-5])->offset = yyvsp[-3];
		CB_REFERENCE (yyvsp[-5])->length = yyvsp[-1];
	}
  }
#line 12897 "parser.c"
    break;

  case 1393: /* integer: "Literal"  */
#line 7017 "parser.y"
  {
	if (cb_tree_category (yyvsp[0]) != CB_CATEGORY_NUMERIC) {
		cb_error (_("Integer value expected"));
	} else if (CB_LITERAL (yyvsp[0])->sign < 0 || CB_LITERAL (yyvsp[0])->scale) {
		cb_error (_("Integer value expected"));
	}
	yyval = yyvsp[0];
  }
#line 12910 "parser.c"
    break;

  case 1394: /* literal: basic_literal  */
#line 7028 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12916 "parser.c"
    break;

  case 1395: /* literal: ALL basic_value  */
#line 7030 "parser.y"
  {
	yyval = yyvsp[0];
	if (CB_LITERAL_P (yyvsp[0])) {
		CB_LITERAL (yyvsp[0])->all = 1;
	}
  }
#line 12927 "parser.c"
    break;

  case 1396: /* basic_literal: basic_value  */
#line 7039 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12933 "parser.c"
    break;

  case 1397: /* basic_literal: basic_literal '&' basic_value  */
#line 7040 "parser.y"
                                { yyval = cb_concat_literals (yyvsp[-2], yyvsp[0]); }
#line 12939 "parser.c"
    break;

  case 1398: /* basic_value: "Literal"  */
#line 7044 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12945 "parser.c"
    break;

  case 1399: /* basic_value: SPACE  */
#line 7045 "parser.y"
                                { yyval = cb_space; }
#line 12951 "parser.c"
    break;

  case 1400: /* basic_value: ZERO  */
#line 7046 "parser.y"
                                { yyval = cb_zero; }
#line 12957 "parser.c"
    break;

  case 1401: /* basic_value: QUOTE  */
#line 7047 "parser.y"
                                { yyval = cb_quote; }
#line 12963 "parser.c"
    break;

  case 1402: /* basic_value: "HIGH-VALUE"  */
#line 7048 "parser.y"
                                { yyval = cb_high; }
#line 12969 "parser.c"
    break;

  case 1403: /* basic_value: "LOW-VALUE"  */
#line 7049 "parser.y"
                                { yyval = cb_low; }
#line 12975 "parser.c"
    break;

  case 1404: /* basic_value: "NULL"  */
#line 7050 "parser.y"
                                { yyval = cb_null; }
#line 12981 "parser.c"
    break;

  case 1405: /* function: "FUNCTION CURRENT-DATE" func_refmod  */
#line 7059 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-1], NULL, yyvsp[0]);
  }
#line 12989 "parser.c"
    break;

  case 1406: /* function: "FUNCTION WHEN-COMPILED" func_refmod  */
#line 7063 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-1], NULL, yyvsp[0]);
  }
#line 12997 "parser.c"
    break;

  case 1407: /* function: "FUNCTION UPPER-CASE" '(' exp ')' func_refmod  */
#line 7067 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], cb_list_init (yyvsp[-2]), yyvsp[0]);
  }
#line 13005 "parser.c"
    break;

  case 1408: /* function: "FUNCTION LOWER-CASE" '(' exp ')' func_refmod  */
#line 7071 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], cb_list_init (yyvsp[-2]), yyvsp[0]);
  }
#line 13013 "parser.c"
    break;

  case 1409: /* function: "FUNCTION REVERSE" '(' exp ')' func_refmod  */
#line 7075 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], cb_list_init (yyvsp[-2]), yyvsp[0]);
  }
#line 13021 "parser.c"
    break;

  case 1410: /* function: "FUNCTION CONCATENATE" '(' exp_list ')' func_refmod  */
#line 7079 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0]);
  }
#line 13029 "parser.c"
    break;

  case 1411: /* function: "FUNCTION SUBSTITUTE" '(' exp_list ')' func_refmod  */
#line 7083 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0]);
  }
#line 13037 "parser.c"
    break;

  case 1412: /* function: "FUNCTION SUBSTITUTE-CASE" '(' exp_list ')' func_refmod  */
#line 7087 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0]);
  }
#line 13045 "parser.c"
    break;

  case 1413: /* function: "FUNCTION TRIM" '(' trim_args ')' func_refmod  */
#line 7091 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0]);
  }
#line 13053 "parser.c"
    break;

  case 1414: /* function: "FUNCTION NUMVALC" '(' numvalc_args ')'  */
#line 7095 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-3], yyvsp[-1], NULL);
  }
#line 13061 "parser.c"
    break;

  case 1415: /* function: "FUNCTION LOCALE" '(' locale_dt_args ')' func_refmod  */
#line 7099 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0]);
  }
#line 13069 "parser.c"
    break;

  case 1416: /* function: "FUNCTION" func_args  */
#line 7103 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-1], yyvsp[0], NULL);
  }
#line 13077 "parser.c"
    break;

  case 1417: /* func_refmod: %empty  */
#line 7109 "parser.y"
                                { yyval = NULL; }
#line 13083 "parser.c"
    break;

  case 1418: /* func_refmod: '(' exp ':' ')'  */
#line 7110 "parser.y"
                                { yyval = cb_build_pair (yyvsp[-2], NULL); }
#line 13089 "parser.c"
    break;

  case 1419: /* func_refmod: '(' exp ':' exp ')'  */
#line 7111 "parser.y"
                                { yyval = cb_build_pair (yyvsp[-3], yyvsp[-1]); }
#line 13095 "parser.c"
    break;

  case 1420: /* func_args: %empty  */
#line 7115 "parser.y"
                                { yyval = NULL; }
#line 13101 "parser.c"
    break;

  case 1421: /* func_args: '(' list_func_args ')'  */
#line 7116 "parser.y"
                                { yyval = yyvsp[-1]; }
#line 13107 "parser.c"
    break;

  case 1422: /* list_func_args: %empty  */
#line 7120 "parser.y"
                                { yyval = NULL; }
#line 13113 "parser.c"
    break;

  case 1423: /* list_func_args: exp_list  */
#line 7121 "parser.y"
                                { yyval = yyvsp[0]; }
#line 13119 "parser.c"
    break;

  case 1424: /* trim_args: exp  */
#line 7127 "parser.y"
  {
	cb_tree	x;

	x = cb_list_init (yyvsp[0]);
	yyval = cb_list_add (x, cb_int0);
  }
#line 13130 "parser.c"
    break;

  case 1425: /* trim_args: exp e_sep LEADING  */
#line 7134 "parser.y"
  {
	cb_tree	x;

	x = cb_list_init (yyvsp[-2]);
	yyval = cb_list_add (x, cb_int1);
  }
#line 13141 "parser.c"
    break;

  case 1426: /* trim_args: exp e_sep TRAILING  */
#line 7141 "parser.y"
  {
	cb_tree	x;

	x = cb_list_init (yyvsp[-2]);
	yyval = cb_list_add (x, cb_int2);
  }
#line 13152 "parser.c"
    break;

  case 1427: /* numvalc_args: exp  */
#line 7151 "parser.y"
  {
	cb_tree	x;

	x = cb_list_init (yyvsp[0]);
	yyval = cb_list_add (x, cb_null);
  }
#line 13163 "parser.c"
    break;

  case 1428: /* numvalc_args: exp e_sep exp  */
#line 7158 "parser.y"
  {
	cb_tree	x;

	x = cb_list_init (yyvsp[-2]);
	yyval = cb_list_add (x, yyvsp[0]);
  }
#line 13174 "parser.c"
    break;

  case 1429: /* locale_dt_args: exp  */
#line 7168 "parser.y"
  {
	cb_tree	x;

	x = cb_list_init (yyvsp[0]);
	yyval = cb_list_add (x, cb_null);
  }
#line 13185 "parser.c"
    break;

  case 1430: /* locale_dt_args: exp e_sep reference  */
#line 7175 "parser.y"
  {
	cb_tree	x;

	x = cb_list_init (yyvsp[-2]);
	yyval = cb_list_add (x, cb_ref (yyvsp[0]));
  }
#line 13196 "parser.c"
    break;

  case 1431: /* not_const_word: %empty  */
#line 7188 "parser.y"
  {
	non_const_word = 1;
  }
#line 13204 "parser.c"
    break;

  case 1432: /* flag_all: %empty  */
#line 7198 "parser.y"
                                { yyval = cb_int0; }
#line 13210 "parser.c"
    break;

  case 1433: /* flag_all: ALL  */
#line 7199 "parser.y"
                                { yyval = cb_int1; }
#line 13216 "parser.c"
    break;

  case 1434: /* flag_duplicates: %empty  */
#line 7203 "parser.y"
                                { yyval = cb_int0; }
#line 13222 "parser.c"
    break;

  case 1435: /* flag_duplicates: with_dups  */
#line 7204 "parser.y"
                                { yyval = cb_int1; }
#line 13228 "parser.c"
    break;

  case 1436: /* flag_initialized: %empty  */
#line 7208 "parser.y"
                                { yyval = NULL; }
#line 13234 "parser.c"
    break;

  case 1437: /* flag_initialized: INITIALIZED  */
#line 7209 "parser.y"
                                { yyval = cb_int1; }
#line 13240 "parser.c"
    break;

  case 1438: /* flag_next: %empty  */
#line 7213 "parser.y"
                                { yyval = cb_int0; }
#line 13246 "parser.c"
    break;

  case 1439: /* flag_next: NEXT  */
#line 7214 "parser.y"
                                { yyval = cb_int1; }
#line 13252 "parser.c"
    break;

  case 1440: /* flag_next: PREVIOUS  */
#line 7215 "parser.y"
                                { yyval = cb_int2; }
#line 13258 "parser.c"
    break;

  case 1441: /* flag_not: %empty  */
#line 7219 "parser.y"
                                { yyval = cb_int0; }
#line 13264 "parser.c"
    break;

  case 1442: /* flag_not: NOT  */
#line 7220 "parser.y"
                                { yyval = cb_int1; }
#line 13270 "parser.c"
    break;

  case 1443: /* flag_optional: %empty  */
#line 7224 "parser.y"
                                { yyval = cb_int0; }
#line 13276 "parser.c"
    break;

  case 1444: /* flag_optional: OPTIONAL  */
#line 7225 "parser.y"
                                { yyval = cb_int1; }
#line 13282 "parser.c"
    break;

  case 1445: /* flag_rounded: %empty  */
#line 7229 "parser.y"
                                { yyval = cb_int0; }
#line 13288 "parser.c"
    break;

  case 1446: /* flag_rounded: ROUNDED  */
#line 7230 "parser.y"
                                { yyval = cb_int1; }
#line 13294 "parser.c"
    break;

  case 1447: /* flag_separate: %empty  */
#line 7234 "parser.y"
                                { yyval = cb_int0; }
#line 13300 "parser.c"
    break;

  case 1448: /* flag_separate: SEPARATE _character  */
#line 7235 "parser.y"
                                { yyval = cb_int1; }
#line 13306 "parser.c"
    break;

  case 1460: /* _also: ALSO  */
#line 7248 "parser.y"
                       { yyval = cb_int1; }
#line 13312 "parser.c"
    break;

  case 1489: /* _is: %empty  */
#line 7263 "parser.y"
                { yyval = NULL; }
#line 13318 "parser.c"
    break;

  case 1490: /* _is: IS  */
#line 7263 "parser.y"
                                    { yyval = cb_int1; }
#line 13324 "parser.c"
    break;

  case 1501: /* _literal: %empty  */
#line 7268 "parser.y"
                { yyval = NULL; }
#line 13330 "parser.c"
    break;

  case 1502: /* _literal: "Literal"  */
#line 7268 "parser.y"
                                         { yyval = yyvsp[0]; }
#line 13336 "parser.c"
    break;


#line 13340 "parser.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= TOKEN_EOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == TOKEN_EOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 7293 "parser.y"

