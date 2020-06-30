
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         ppparse
#define yylex           pplex
#define yyerror         pperror
#define yylval          pplval
#define yychar          ppchar
#define yydebug         ppdebug
#define yynerrs         ppnerrs


/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 26 "ppparse.y"

#include "config.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"

#define YYDEBUG		1
#define YYERROR_VERBOSE	1
#define pperror cb_error

static char *fix_filename (char *name);
static char *fold_lower (char *name);
static char *fold_upper (char *name);

static struct cb_replace_list *cb_replace_list_add (struct cb_replace_list *replace_list, struct cb_text_list *old_text, struct cb_text_list *new_text);
static void cb_replace_list_set_type (struct cb_replace_list *list, int replace_type);
static struct cb_replace_list *cb_replace_list_add_list (struct cb_replace_list *replace_list, struct cb_replace_list *replace_list_next);


/* Line 189 of yacc.c  */
#line 106 "ppparse.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOKEN_EOF = 0,
     COPY = 258,
     REPLACE = 259,
     SUPPRESS = 260,
     PRINTING = 261,
     REPLACING = 262,
     OFF = 263,
     IN = 264,
     OF = 265,
     BY = 266,
     EQEQ = 267,
     LEADING = 268,
     TRAILING = 269,
     JOINING = 270,
     AS = 271,
     PREFIX = 272,
     SUFFIX = 273,
     PREFIXING = 274,
     SUFFIXING = 275,
     LEVEL_NUMBER = 276,
     REDEFINES = 277,
     TOKEN = 278,
     PROGRAM_ID = 279,
     FUNCTION_ID = 280,
     ENVIRONMENT_DIVISION = 281,
     DATA_DIVISION = 282,
     PROCEDURE_DIVISION = 283,
     END_PROGRAM = 284,
     END_FUNCTION = 285
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 50 "ppparse.y"

	char			*s;
	struct cb_text_list	*l;
	struct cb_replace_list	*r;
	struct cb_joining_ext	*jx;
	cb_joining_ext_type_t	jt;



/* Line 214 of yacc.c  */
#line 183 "ppparse.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 195 "ppparse.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

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
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  11
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   121

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  34
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  47
/* YYNRULES -- Number of rules.  */
#define YYNRULES  88
/* YYNRULES -- Number of states.  */
#define YYNSTATES  133

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   285

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      32,    33,     2,     2,     2,     2,    31,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    12,    19,    26,    32,
      33,    35,    38,    39,    43,    44,    48,    49,    50,    54,
      55,    56,    60,    61,    62,    66,    67,    68,    72,    73,
      77,    78,    79,    83,    84,    87,    89,    91,    93,    94,
      97,   100,   102,   104,   106,   110,   113,   115,   119,   120,
     123,   127,   130,   137,   138,   141,   144,   145,   148,   149,
     152,   156,   160,   162,   165,   169,   174,   179,   180,   185,
     188,   191,   192,   194,   196,   198,   200,   202,   205,   209,
     211,   214,   216,   220,   224,   229,   231,   234,   235
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      35,     0,    -1,    36,    -1,    35,    36,    -1,    37,    -1,
      39,    -1,    41,    45,    47,    49,    40,    51,    -1,    41,
      45,    47,    49,    40,    53,    -1,    43,    45,    47,    49,
      55,    -1,    -1,    38,    -1,    40,    38,    -1,    -1,    24,
      42,    57,    -1,    -1,    25,    44,    57,    -1,    -1,    -1,
      26,    46,    57,    -1,    -1,    -1,    27,    48,    59,    -1,
      -1,    -1,    28,    50,    57,    -1,    -1,    -1,    29,    52,
      57,    -1,    -1,    29,    54,    57,    -1,    -1,    -1,    30,
      56,    57,    -1,    -1,    57,    58,    -1,    65,    -1,    69,
      -1,    31,    -1,    -1,    59,    60,    -1,    31,    65,    -1,
      69,    -1,    31,    -1,    61,    -1,    62,    64,    61,    -1,
      62,    64,    -1,    62,    -1,    21,    23,    63,    -1,    -1,
      22,    23,    -1,    64,    65,    31,    -1,    65,    31,    -1,
       3,    23,    66,    67,    72,    68,    -1,    -1,     9,    23,
      -1,    10,    23,    -1,    -1,     5,    80,    -1,    -1,     7,
      70,    -1,     4,    70,    31,    -1,     4,     8,    31,    -1,
      71,    -1,    70,    71,    -1,    75,    11,    75,    -1,    13,
      76,    11,    76,    -1,    14,    76,    11,    76,    -1,    -1,
      15,    23,    73,    74,    -1,    19,    23,    -1,    20,    23,
      -1,    -1,    16,    -1,    17,    -1,    18,    -1,    76,    -1,
      78,    -1,    12,    12,    -1,    12,    77,    12,    -1,    23,
      -1,    77,    23,    -1,    23,    -1,    78,     9,    23,    -1,
      78,    10,    23,    -1,    78,    32,    79,    33,    -1,    23,
      -1,    79,    23,    -1,    -1,     6,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    77,    77,    78,    82,    83,    87,    96,   105,   112,
     113,   114,   118,   118,   123,   123,   126,   127,   127,   130,
     131,   131,   134,   135,   135,   138,   139,   139,   143,   143,
     146,   147,   147,   153,   153,   154,   154,   154,   156,   156,
     157,   157,   157,   158,   162,   163,   164,   172,   180,   181,
     185,   186,   190,   212,   213,   214,   217,   218,   222,   223,
     227,   228,   232,   233,   237,   238,   243,   251,   252,   259,
     266,   275,   275,   278,   279,   283,   284,   288,   289,   293,
     294,   298,   299,   306,   313,   326,   327,   334,   334
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "COPY", "REPLACE", "SUPPRESS",
  "PRINTING", "REPLACING", "OFF", "IN", "OF", "BY", "EQEQ", "LEADING",
  "TRAILING", "JOINING", "AS", "PREFIX", "SUFFIX", "PREFIXING",
  "SUFFIXING", "LEVEL_NUMBER", "REDEFINES", "TOKEN", "PROGRAM_ID",
  "FUNCTION_ID", "ENVIRONMENT_DIVISION", "DATA_DIVISION",
  "PROCEDURE_DIVISION", "END_PROGRAM", "END_FUNCTION", "'.'", "'('", "')'",
  "$accept", "nested_list", "source_element", "program_definition",
  "program_mandatory", "function_definition", "nested_prog",
  "identification_division", "$@1", "function_division", "$@2",
  "environment_division", "$@3", "data_division", "$@4",
  "procedure_division", "$@5", "end_program", "$@6", "end_mandatory",
  "$@7", "end_function", "$@8", "statement_list", "statement",
  "data_statement_list", "data_statement", "data_description",
  "data_description_entry", "_redefines_clause",
  "recursive_copy_statement", "copy_statement", "copy_in", "copy_suppress",
  "copy_replacing", "replace_statement", "replacing_list",
  "replacing_tokens", "copy_joining", "_as", "joining_ext_type", "text",
  "pseudo_text", "token_list", "identifier", "subscripts", "_printing", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,    46,    40,    41
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    34,    35,    35,    36,    36,    37,    38,    39,    40,
      40,    40,    42,    41,    44,    43,    45,    46,    45,    47,
      48,    47,    49,    50,    49,    51,    52,    51,    54,    53,
      55,    56,    55,    57,    57,    58,    58,    58,    59,    59,
      60,    60,    60,    60,    61,    61,    61,    62,    63,    63,
      64,    64,    65,    66,    66,    66,    67,    67,    68,    68,
      69,    69,    70,    70,    71,    71,    71,    72,    72,    72,
      72,    73,    73,    74,    74,    75,    75,    76,    76,    77,
      77,    78,    78,    78,    78,    79,    79,    80,    80
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     6,     6,     5,     0,
       1,     2,     0,     3,     0,     3,     0,     0,     3,     0,
       0,     3,     0,     0,     3,     0,     0,     3,     0,     3,
       0,     0,     3,     0,     2,     1,     1,     1,     0,     2,
       2,     1,     1,     1,     3,     2,     1,     3,     0,     2,
       3,     2,     6,     0,     2,     2,     0,     2,     0,     2,
       3,     3,     1,     2,     3,     4,     4,     0,     4,     2,
       2,     0,     1,     1,     1,     1,     1,     2,     3,     1,
       2,     1,     3,     3,     4,     1,     2,     0,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    12,    14,     0,     2,     4,     5,    16,    16,    33,
      33,     1,     3,    17,    19,    19,    13,    15,    33,    20,
      22,    22,     0,     0,    37,    34,    35,    36,    18,    38,
      23,     9,    30,    53,     0,     0,     0,     0,    81,     0,
      62,     0,    75,    76,    21,    33,    10,    25,    16,    31,
       8,     0,     0,    56,    61,    77,    79,     0,     0,     0,
      60,    63,     0,     0,     0,     0,     0,    42,    39,    43,
      46,    41,    24,    26,    11,     6,    19,    33,    54,    55,
      87,    67,    78,    80,     0,     0,    64,    82,    83,    85,
       0,    48,    40,    45,     0,    33,    22,    32,    88,    57,
       0,     0,     0,    58,    65,    66,    86,    84,     0,    47,
      44,     0,    51,    27,     9,    71,    69,    70,     0,    52,
      49,    50,     0,    72,     0,    59,    28,     7,    73,    74,
      68,    33,    29
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     3,     4,     5,    46,     6,    47,    48,     9,     8,
      10,    14,    18,    20,    29,    31,    45,    75,    95,   127,
     131,    50,    77,    16,    25,    44,    68,    69,    70,   109,
      93,    26,    53,    81,   119,    27,    39,    40,   103,   124,
     130,    41,    42,    57,    43,    90,    99
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -55
static const yytype_int8 yypact[] =
{
      46,   -55,   -55,    17,   -55,   -55,   -55,    32,    32,   -55,
     -55,   -55,   -55,   -55,    35,    35,     6,     6,   -55,   -55,
      38,    38,    49,    10,   -55,   -55,   -55,   -55,     6,   -55,
     -55,    50,    43,     5,    44,    22,    64,    64,   -55,    13,
     -55,    68,   -55,    11,     7,   -55,   -55,    39,    32,   -55,
     -55,    57,    58,    77,   -55,   -55,   -55,    28,    72,    73,
     -55,   -55,    34,    63,    65,    66,    67,    88,   -55,   -55,
      88,   -55,     6,   -55,   -55,   -55,    35,   -55,   -55,   -55,
      86,    41,   -55,   -55,    64,    64,   -55,   -55,   -55,   -55,
      -4,    71,   -55,     9,    69,   -55,    38,     6,   -55,   -55,
      74,    75,    76,    87,   -55,   -55,   -55,   -55,    78,   -55,
     -55,    79,   -55,     6,    50,    80,   -55,   -55,    36,   -55,
     -55,   -55,    40,   -55,    14,    36,   -55,   -55,   -55,   -55,
     -55,   -55,     6
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -55,   -55,    92,   -55,   -45,   -55,   -12,    52,   -55,   -55,
     -55,    -1,   -55,   -11,   -55,   -18,   -55,   -55,   -55,   -55,
     -55,   -55,   -55,   -10,   -55,   -55,   -55,    12,   -55,   -55,
     -55,   -54,   -55,   -55,   -55,    59,   -14,   -38,   -55,   -55,
     -55,    45,   -31,   -55,   -55,   -55,   -55
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      17,    61,    74,    32,    21,    58,    59,    15,    28,    22,
      23,    23,    22,    92,    51,    52,    94,    11,    34,   106,
      63,    64,    35,    36,    37,    35,    36,    37,    66,   107,
      66,   128,   129,    38,    55,    72,    38,    24,    67,   111,
      82,     1,     2,    65,    60,    56,    35,    76,    35,    36,
      37,    83,     7,   104,   105,     7,   100,    38,    13,    38,
     101,   102,    19,     1,     1,    96,    30,    97,    73,   126,
       1,     2,    33,    49,     1,    54,    35,    74,   114,    62,
      78,    79,    80,    84,    85,   113,    87,    61,    88,    89,
      91,    22,    98,   108,   118,    12,   123,   115,   116,   117,
     112,   120,   122,    71,   125,   110,     0,    86,     0,     0,
     121,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132
};

static const yytype_int16 yycheck[] =
{
      10,    39,    47,    21,    15,    36,    37,     8,    18,     3,
       4,     4,     3,    67,     9,    10,    70,     0,     8,    23,
       9,    10,    12,    13,    14,    12,    13,    14,    21,    33,
      21,    17,    18,    23,    12,    45,    23,    31,    31,    93,
      12,    24,    25,    32,    31,    23,    12,    48,    12,    13,
      14,    23,     0,    84,    85,     3,    15,    23,    26,    23,
      19,    20,    27,    24,    24,    76,    28,    77,    29,    29,
      24,    25,    23,    30,    24,    31,    12,   122,    96,    11,
      23,    23,     5,    11,    11,    95,    23,   125,    23,    23,
      23,     3,     6,    22,     7,     3,    16,    23,    23,    23,
      31,    23,   114,    44,   118,    93,    -1,    62,    -1,    -1,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    24,    25,    35,    36,    37,    39,    41,    43,    42,
      44,     0,    36,    26,    45,    45,    57,    57,    46,    27,
      47,    47,     3,     4,    31,    58,    65,    69,    57,    48,
      28,    49,    49,    23,     8,    12,    13,    14,    23,    70,
      71,    75,    76,    78,    59,    50,    38,    40,    41,    30,
      55,     9,    10,    66,    31,    12,    23,    77,    76,    76,
      31,    71,    11,     9,    10,    32,    21,    31,    60,    61,
      62,    69,    57,    29,    38,    51,    45,    56,    23,    23,
       5,    67,    12,    23,    11,    11,    75,    23,    23,    23,
      79,    23,    65,    64,    65,    52,    47,    57,     6,    80,
      15,    19,    20,    72,    76,    76,    23,    33,    22,    63,
      61,    65,    31,    57,    49,    23,    23,    23,     7,    68,
      23,    31,    40,    16,    73,    70,    29,    53,    17,    18,
      74,    54,    57
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
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



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

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
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
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
      if (yyn == 0 || yyn == YYTABLE_NINF)
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

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

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
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 12:

/* Line 1455 of yacc.c  */
#line 118 "ppparse.y"
    { pp_set_current_division (PP_IDENTIFICATION_DIVISION); ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 123 "ppparse.y"
    { pp_set_current_division (PP_FUNCTION_DIVISION); ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 127 "ppparse.y"
    { pp_set_current_division (PP_ENVIRONMENT_DIVISION); ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 131 "ppparse.y"
    { pp_set_current_division (PP_DATA_DIVISION); ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 135 "ppparse.y"
    { pp_set_current_division (PP_PROCEDURE_DIVISION); ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 139 "ppparse.y"
    { pp_set_current_division (PP_OUT_OF_DIVISION); ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 143 "ppparse.y"
    { pp_set_current_division (PP_OUT_OF_DIVISION); ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 147 "ppparse.y"
    { pp_set_current_division (PP_OUT_OF_DIVISION); ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 165 "ppparse.y"
    {
	pp_omit_data_entry_name (0);
	pp_omit_data_redef_name (0);
  ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 173 "ppparse.y"
    {
	pp_omit_data_entry_name (1);
	pp_omit_data_redef_name (((yyvsp[(3) - (3)].s)) ? 1 : 0);
  ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 180 "ppparse.y"
    { (yyval.s) = NULL; ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 181 "ppparse.y"
    { (yyval.s) = (yyvsp[(2) - (2)].s); ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 191 "ppparse.y"
    {
	fputc ('\n', ppout);
	(yyvsp[(2) - (6)].s) = fix_filename ((yyvsp[(2) - (6)].s));
	if (cb_flag_fold_copy_lower) {
		(yyvsp[(2) - (6)].s) = fold_lower ((yyvsp[(2) - (6)].s));
	} else if (cb_flag_fold_copy_upper) {
		(yyvsp[(2) - (6)].s) = fold_upper ((yyvsp[(2) - (6)].s));
	}
	if ((yyvsp[(3) - (6)].s)) {
		(yyvsp[(3) - (6)].s) = fix_filename ((yyvsp[(3) - (6)].s));
		if (cb_flag_fold_copy_lower) {
			(yyvsp[(3) - (6)].s) = fold_lower ((yyvsp[(3) - (6)].s));
		} else if (cb_flag_fold_copy_upper) {
			(yyvsp[(3) - (6)].s) = fold_upper ((yyvsp[(3) - (6)].s));
		}
	}
	ppcopy ((yyvsp[(2) - (6)].s), (yyvsp[(3) - (6)].s), (yyvsp[(5) - (6)].jx), (yyvsp[(6) - (6)].r));
  ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 212 "ppparse.y"
    { (yyval.s) = NULL; ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 213 "ppparse.y"
    { (yyval.s) = (yyvsp[(2) - (2)].s); ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 214 "ppparse.y"
    { (yyval.s) = (yyvsp[(2) - (2)].s); ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 222 "ppparse.y"
    { (yyval.r) = NULL; ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 223 "ppparse.y"
    { (yyval.r) = (yyvsp[(2) - (2)].r); ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 227 "ppparse.y"
    { pp_set_replace_list ((yyvsp[(2) - (3)].r)); ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 228 "ppparse.y"
    { pp_set_replace_list (NULL); ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 232 "ppparse.y"
    { (yyval.r) = cb_replace_list_add_list (NULL, (yyvsp[(1) - (1)].r)); ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 233 "ppparse.y"
    { (yyval.r) = cb_replace_list_add_list ((yyvsp[(1) - (2)].r), (yyvsp[(2) - (2)].r)); ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 237 "ppparse.y"
    { (yyval.r) = cb_replace_list_add (NULL, (yyvsp[(1) - (3)].l), (yyvsp[(3) - (3)].l)); ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 239 "ppparse.y"
    {
	(yyval.r) = cb_replace_list_add (NULL, (yyvsp[(2) - (4)].l), (yyvsp[(4) - (4)].l));
	cb_replace_list_set_type ((yyval.r), CB_REPLACE_LEADING);
  ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 244 "ppparse.y"
    {
	(yyval.r) = cb_replace_list_add (NULL, (yyvsp[(2) - (4)].l), (yyvsp[(4) - (4)].l));
	cb_replace_list_set_type ((yyval.r), CB_REPLACE_TRAILING);
  ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 251 "ppparse.y"
    { (yyval.jx) = NULL; ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 253 "ppparse.y"
    {
	struct cb_joining_ext *p = cobc_malloc (sizeof (struct cb_joining_ext));
	p->ext = (yyvsp[(2) - (4)].s);
	p->type = (yyvsp[(4) - (4)].jt);
	(yyval.jx) = p;
  ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 260 "ppparse.y"
    {
	struct cb_joining_ext *p = cobc_malloc (sizeof (struct cb_joining_ext));
	p->ext  = (yyvsp[(2) - (2)].s);
	p->type = prefixing;
	(yyval.jx) = p;
  ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 267 "ppparse.y"
    {
	struct cb_joining_ext *p = cobc_malloc (sizeof (struct cb_joining_ext));
	p->ext  = (yyvsp[(2) - (2)].s);
	p->type = suffixing;
	(yyval.jx) = p;
  ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 278 "ppparse.y"
    { (yyval.jt) = joining_as_prefix; ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 279 "ppparse.y"
    { (yyval.jt) = joining_as_suffix; ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 283 "ppparse.y"
    { (yyval.l) = (yyvsp[(1) - (1)].l); ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 284 "ppparse.y"
    { (yyval.l) = (yyvsp[(1) - (1)].l); ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 288 "ppparse.y"
    { (yyval.l) = NULL; ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 289 "ppparse.y"
    { (yyval.l) = (yyvsp[(2) - (3)].l); ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 293 "ppparse.y"
    { (yyval.l) = cb_text_list_add (NULL, (yyvsp[(1) - (1)].s)); ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 294 "ppparse.y"
    { (yyval.l) = cb_text_list_add ((yyvsp[(1) - (2)].l), (yyvsp[(2) - (2)].s)); ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 298 "ppparse.y"
    { (yyval.l) = cb_text_list_add (NULL, (yyvsp[(1) - (1)].s)); ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 300 "ppparse.y"
    {
	(yyval.l) = cb_text_list_add ((yyvsp[(1) - (3)].l), " ");
	(yyval.l) = cb_text_list_add ((yyval.l), "IN");
	(yyval.l) = cb_text_list_add ((yyval.l), " ");
	(yyval.l) = cb_text_list_add ((yyval.l), (yyvsp[(3) - (3)].s));
  ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 307 "ppparse.y"
    {
	(yyval.l) = cb_text_list_add ((yyvsp[(1) - (3)].l), " ");
	(yyval.l) = cb_text_list_add ((yyval.l), "OF");
	(yyval.l) = cb_text_list_add ((yyval.l), " ");
	(yyval.l) = cb_text_list_add ((yyval.l), (yyvsp[(3) - (3)].s));
  ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 314 "ppparse.y"
    {
	struct cb_text_list *l;

	(yyval.l) = cb_text_list_add ((yyvsp[(1) - (4)].l), " ");
	(yyval.l) = cb_text_list_add ((yyval.l), "(");
	(yyvsp[(3) - (4)].l) = cb_text_list_add ((yyvsp[(3) - (4)].l), ")");
	for (l = (yyval.l); l->next; l = l->next);
	l->next = (yyvsp[(3) - (4)].l);
  ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 326 "ppparse.y"
    { (yyval.l) = cb_text_list_add (NULL, (yyvsp[(1) - (1)].s)); ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 328 "ppparse.y"
    {
	(yyval.l) = cb_text_list_add ((yyvsp[(1) - (2)].l), " ");
	(yyval.l) = cb_text_list_add ((yyval.l), (yyvsp[(2) - (2)].s));
  ;}
    break;



/* Line 1455 of yacc.c  */
#line 1886 "ppparse.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
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

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
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
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 336 "ppparse.y"

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

