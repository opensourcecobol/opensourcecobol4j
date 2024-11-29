/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Substitute the variable and function names.  */
#define yyparse ppparse
#define yylex pplex
#define yyerror pperror
#define yydebug ppdebug
#define yynerrs ppnerrs
#define yylval pplval
#define yychar ppchar

/* First part of user prologue.  */
#line 27 "ppparse.y"

#include "config.h"

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cobj.h"

#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define pperror cb_error

static char *fix_filename(char *name);
static char *fold_lower(char *name);
static char *fold_upper(char *name);

static struct cb_replace_list *
cb_replace_list_add(struct cb_replace_list *replace_list,
                    struct cb_text_list *old_text,
                    struct cb_text_list *new_text);
static void cb_replace_list_set_type(struct cb_replace_list *list,
                                     int replace_type);
static struct cb_replace_list *
cb_replace_list_add_list(struct cb_replace_list *replace_list,
                         struct cb_replace_list *replace_list_next);

#line 101 "ppparse.c"

#ifndef YY_CAST
#ifdef __cplusplus
#define YY_CAST(Type, Val) static_cast<Type>(Val)
#define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type>(Val)
#else
#define YY_CAST(Type, Val) ((Type)(Val))
#define YY_REINTERPRET_CAST(Type, Val) ((Type)(Val))
#endif
#endif
#ifndef YY_NULLPTR
#if defined __cplusplus
#if 201103L <= __cplusplus
#define YY_NULLPTR nullptr
#else
#define YY_NULLPTR 0
#endif
#else
#define YY_NULLPTR ((void *)0)
#endif
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
#undef YYERROR_VERBOSE
#define YYERROR_VERBOSE 1
#else
#define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_PP_PPPARSE_H_INCLUDED
#define YY_PP_PPPARSE_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#if YYDEBUG
extern int ppdebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
#define YYTOKENTYPE
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

/* Value type.  */
#if !defined YYSTYPE && !defined YYSTYPE_IS_DECLARED
union YYSTYPE {
#line 51 "ppparse.y"

  char *s;
  struct cb_text_list *l;
  struct cb_replace_list *r;
  struct cb_joining_ext *jx;
  cb_joining_ext_type_t jt;

#line 193 "ppparse.c"
};
typedef union YYSTYPE YYSTYPE;
#define YYSTYPE_IS_TRIVIAL 1
#define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE pplval;

int ppparse(void);

#endif /* !YY_PP_PPPARSE_H_INCLUDED  */

#ifdef short
#undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
#include <limits.h> /* INFRINGES ON USER NAME SPACE */
#if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#define YY_STDINT_H
#endif
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

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H &&                  \
       UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H &&                 \
       UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
#if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#define YYPTRDIFF_T __PTRDIFF_TYPE__
#define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
#elif defined PTRDIFF_MAX
#ifndef ptrdiff_t
#include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#endif
#define YYPTRDIFF_T ptrdiff_t
#define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
#else
#define YYPTRDIFF_T long
#define YYPTRDIFF_MAXIMUM LONG_MAX
#endif
#endif

#ifndef YYSIZE_T
#ifdef __SIZE_TYPE__
#define YYSIZE_T __SIZE_TYPE__
#elif defined size_t
#define YYSIZE_T size_t
#elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#define YYSIZE_T size_t
#else
#define YYSIZE_T unsigned
#endif
#endif

#define YYSIZE_MAXIMUM                                                         \
  YY_CAST(YYPTRDIFF_T,                                                         \
          (YYPTRDIFF_MAXIMUM < YY_CAST(YYSIZE_T, -1) ? YYPTRDIFF_MAXIMUM       \
                                                     : YY_CAST(YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST(YYPTRDIFF_T, sizeof(X))

/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
#if defined YYENABLE_NLS && YYENABLE_NLS
#if ENABLE_NLS
#include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#define YY_(Msgid) dgettext("bison-runtime", Msgid)
#endif
#endif
#ifndef YY_
#define YY_(Msgid) Msgid
#endif
#endif

#ifndef YY_ATTRIBUTE_PURE
#if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#define YY_ATTRIBUTE_PURE __attribute__((__pure__))
#else
#define YY_ATTRIBUTE_PURE
#endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
#if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#define YY_ATTRIBUTE_UNUSED __attribute__((__unused__))
#else
#define YY_ATTRIBUTE_UNUSED
#endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if !defined lint || defined __GNUC__
#define YYUSE(E) ((void)(E))
#else
#define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && !defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                                    \
  _Pragma("GCC diagnostic push")                                               \
      _Pragma("GCC diagnostic ignored \"-Wuninitialized\"")                    \
          _Pragma("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
#define YY_IGNORE_MAYBE_UNINITIALIZED_END _Pragma("GCC diagnostic pop")
#else
#define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
#define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
#define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
#define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && !defined __ICC && 6 <= __GNUC__
#define YY_IGNORE_USELESS_CAST_BEGIN                                           \
  _Pragma("GCC diagnostic push")                                               \
      _Pragma("GCC diagnostic ignored \"-Wuseless-cast\"")
#define YY_IGNORE_USELESS_CAST_END _Pragma("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
#define YY_IGNORE_USELESS_CAST_BEGIN
#define YY_IGNORE_USELESS_CAST_END
#endif

#define YY_ASSERT(E) ((void)(0 && (E)))

#if !defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

#ifdef YYSTACK_USE_ALLOCA
#if YYSTACK_USE_ALLOCA
#ifdef __GNUC__
#define YYSTACK_ALLOC __builtin_alloca
#elif defined __BUILTIN_VA_ARG_INCR
#include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#elif defined _AIX
#define YYSTACK_ALLOC __alloca
#elif defined _MSC_VER
#include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#define alloca _alloca
#else
#define YYSTACK_ALLOC alloca
#if !defined _ALLOCA_H && !defined EXIT_SUCCESS
#include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
/* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#endif
#endif
#endif
#endif

#ifdef YYSTACK_ALLOC
/* Pacify GCC's 'empty if-body' warning.  */
#define YYSTACK_FREE(Ptr)                                                      \
  do { /* empty */                                                             \
    ;                                                                          \
  } while (0)
#ifndef YYSTACK_ALLOC_MAXIMUM
/* The OS might guarantee only one guard page at the bottom of the stack,
   and a page size can be as small as 4096 bytes.  So we cannot safely
   invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
   to allow for a few compiler-allocated temporary stack slots.  */
#define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#endif
#else
#define YYSTACK_ALLOC YYMALLOC
#define YYSTACK_FREE YYFREE
#ifndef YYSTACK_ALLOC_MAXIMUM
#define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#endif
#if (defined __cplusplus && !defined EXIT_SUCCESS &&                           \
     !((defined YYMALLOC || defined malloc) &&                                 \
       (defined YYFREE || defined free)))
#include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#endif
#ifndef YYMALLOC
#define YYMALLOC malloc
#if !defined malloc && !defined EXIT_SUCCESS
void *malloc(YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#endif
#endif
#ifndef YYFREE
#define YYFREE free
#if !defined free && !defined EXIT_SUCCESS
void free(void *); /* INFRINGES ON USER NAME SPACE */
#endif
#endif
#endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */

#if (!defined yyoverflow &&                                                    \
     (!defined __cplusplus ||                                                  \
      (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc {
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
#define YYSTACK_GAP_MAXIMUM (YYSIZEOF(union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
#define YYSTACK_BYTES(N)                                                       \
  ((N) * (YYSIZEOF(yy_state_t) + YYSIZEOF(YYSTYPE)) + YYSTACK_GAP_MAXIMUM)

#define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
#define YYSTACK_RELOCATE(Stack_alloc, Stack)                                   \
  do {                                                                         \
    YYPTRDIFF_T yynewbytes;                                                    \
    YYCOPY(&yyptr->Stack_alloc, Stack, yysize);                                \
    Stack = &yyptr->Stack_alloc;                                               \
    yynewbytes = yystacksize * YYSIZEOF(*Stack) + YYSTACK_GAP_MAXIMUM;         \
    yyptr += yynewbytes / YYSIZEOF(*yyptr);                                    \
  } while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
#ifndef YYCOPY
#if defined __GNUC__ && 1 < __GNUC__
#define YYCOPY(Dst, Src, Count)                                                \
  __builtin_memcpy(Dst, Src, YY_CAST(YYSIZE_T, (Count)) * sizeof(*(Src)))
#else
#define YYCOPY(Dst, Src, Count)                                                \
  do {                                                                         \
    YYPTRDIFF_T yyi;                                                           \
    for (yyi = 0; yyi < (Count); yyi++)                                        \
      (Dst)[yyi] = (Src)[yyi];                                                 \
  } while (0)
#endif
#endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL 11
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST 121

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS 34
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS 47
/* YYNRULES -- Number of rules.  */
#define YYNRULES 88
/* YYNSTATES -- Number of states.  */
#define YYNSTATES 133

#define YYUNDEFTOK 2
#define YYMAXUTOK 285

/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                       \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] = {
    0,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  32, 33, 2,  2,  2,  2,  31, 2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] = {
    0,   78,  78,  79,  83,  84,  88,  97,  106, 113, 114, 115, 119, 119, 124,
    124, 127, 128, 128, 131, 132, 132, 135, 136, 136, 139, 140, 140, 144, 144,
    147, 148, 148, 154, 154, 155, 155, 155, 157, 157, 158, 158, 158, 159, 163,
    164, 165, 173, 181, 182, 186, 187, 191, 213, 214, 215, 218, 219, 223, 224,
    228, 229, 233, 234, 238, 239, 244, 252, 253, 260, 267, 276, 276, 279, 280,
    284, 285, 289, 290, 294, 295, 299, 300, 307, 314, 327, 328, 335, 335};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] = {"\"end of file\"",
                                      "error",
                                      "$undefined",
                                      "COPY",
                                      "REPLACE",
                                      "SUPPRESS",
                                      "PRINTING",
                                      "REPLACING",
                                      "OFF",
                                      "IN",
                                      "OF",
                                      "BY",
                                      "EQEQ",
                                      "LEADING",
                                      "TRAILING",
                                      "JOINING",
                                      "AS",
                                      "PREFIX",
                                      "SUFFIX",
                                      "PREFIXING",
                                      "SUFFIXING",
                                      "LEVEL_NUMBER",
                                      "REDEFINES",
                                      "TOKEN",
                                      "PROGRAM_ID",
                                      "FUNCTION_ID",
                                      "ENVIRONMENT_DIVISION",
                                      "DATA_DIVISION",
                                      "PROCEDURE_DIVISION",
                                      "END_PROGRAM",
                                      "END_FUNCTION",
                                      "'.'",
                                      "'('",
                                      "')'",
                                      "$accept",
                                      "nested_list",
                                      "source_element",
                                      "program_definition",
                                      "program_mandatory",
                                      "function_definition",
                                      "nested_prog",
                                      "identification_division",
                                      "$@1",
                                      "function_division",
                                      "$@2",
                                      "environment_division",
                                      "$@3",
                                      "data_division",
                                      "$@4",
                                      "procedure_division",
                                      "$@5",
                                      "end_program",
                                      "$@6",
                                      "end_mandatory",
                                      "$@7",
                                      "end_function",
                                      "$@8",
                                      "statement_list",
                                      "statement",
                                      "data_statement_list",
                                      "data_statement",
                                      "data_description",
                                      "data_description_entry",
                                      "_redefines_clause",
                                      "recursive_copy_statement",
                                      "copy_statement",
                                      "copy_in",
                                      "copy_suppress",
                                      "copy_replacing",
                                      "replace_statement",
                                      "replacing_list",
                                      "replacing_tokens",
                                      "copy_joining",
                                      "_as",
                                      "joining_ext_type",
                                      "text",
                                      "pseudo_text",
                                      "token_list",
                                      "identifier",
                                      "subscripts",
                                      "_printing",
                                      YY_NULLPTR};
#endif

#ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] = {
    0,   256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266,
    267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278,
    279, 280, 281, 282, 283, 284, 285, 46,  40,  41};
#endif

#define YYPACT_NINF (-55)

#define yypact_value_is_default(Yyn) ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) 0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int8 yypact[] = {
    46,  -55, -55, 17,  -55, -55, -55, 32,  32,  -55, -55, -55, -55, -55, 35,
    35,  6,   6,   -55, -55, 38,  38,  49,  10,  -55, -55, -55, -55, 6,   -55,
    -55, 50,  43,  5,   44,  22,  64,  64,  -55, 13,  -55, 68,  -55, 11,  7,
    -55, -55, 39,  32,  -55, -55, 57,  58,  77,  -55, -55, -55, 28,  72,  73,
    -55, -55, 34,  63,  65,  66,  67,  88,  -55, -55, 88,  -55, 6,   -55, -55,
    -55, 35,  -55, -55, -55, 86,  41,  -55, -55, 64,  64,  -55, -55, -55, -55,
    -4,  71,  -55, 9,   69,  -55, 38,  6,   -55, -55, 74,  75,  76,  87,  -55,
    -55, -55, -55, 78,  -55, -55, 79,  -55, 6,   50,  80,  -55, -55, 36,  -55,
    -55, -55, 40,  -55, 14,  36,  -55, -55, -55, -55, -55, -55, 6};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] = {
    0,  12, 14, 0,  2,  4,  5,  16, 16, 33, 33, 1,  3,  17, 19, 19, 13, 15, 33,
    20, 22, 22, 0,  0,  37, 34, 35, 36, 18, 38, 23, 9,  30, 53, 0,  0,  0,  0,
    81, 0,  62, 0,  75, 76, 21, 33, 10, 25, 16, 31, 8,  0,  0,  56, 61, 77, 79,
    0,  0,  0,  60, 63, 0,  0,  0,  0,  0,  42, 39, 43, 46, 41, 24, 26, 11, 6,
    19, 33, 54, 55, 87, 67, 78, 80, 0,  0,  64, 82, 83, 85, 0,  48, 40, 45, 0,
    33, 22, 32, 88, 57, 0,  0,  0,  58, 65, 66, 86, 84, 0,  47, 44, 0,  51, 27,
    9,  71, 69, 70, 0,  52, 49, 50, 0,  72, 0,  59, 28, 7,  73, 74, 68, 33, 29};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] = {
    -55, -55, 92,  -55, -45, -55, -12, 52,  -55, -55, -55, -1,
    -55, -11, -55, -18, -55, -55, -55, -55, -55, -55, -55, -10,
    -55, -55, -55, 12,  -55, -55, -55, -54, -55, -55, -55, 59,
    -14, -38, -55, -55, -55, 45,  -31, -55, -55, -55, -55};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] = {
    -1, 3,  4,   5,   46,  6,  47,  48,  9,   8,  10, 14, 18, 20,  29, 31,
    45, 75, 95,  127, 131, 50, 77,  16,  25,  44, 68, 69, 70, 109, 93, 26,
    53, 81, 119, 27,  39,  40, 103, 124, 130, 41, 42, 57, 43, 90,  99};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] = {
    17,  61,  74,  32,  21,  58,  59,  15,  28,  22,  23,  23,  22,  92,
    51,  52,  94,  11,  34,  106, 63,  64,  35,  36,  37,  35,  36,  37,
    66,  107, 66,  128, 129, 38,  55,  72,  38,  24,  67,  111, 82,  1,
    2,   65,  60,  56,  35,  76,  35,  36,  37,  83,  7,   104, 105, 7,
    100, 38,  13,  38,  101, 102, 19,  1,   1,   96,  30,  97,  73,  126,
    1,   2,   33,  49,  1,   54,  35,  74,  114, 62,  78,  79,  80,  84,
    85,  113, 87,  61,  88,  89,  91,  22,  98,  108, 118, 12,  123, 115,
    116, 117, 112, 120, 122, 71,  125, 110, 0,   86,  0,   0,   121, 0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   132};

static const yytype_int16 yycheck[] = {
    10, 39, 47, 21, 15, 36,  37, 8,  18, 3,  4,  4,  3,   67, 9,   10,  70, 0,
    8,  23, 9,  10, 12, 13,  14, 12, 13, 14, 21, 33, 21,  17, 18,  23,  12, 45,
    23, 31, 31, 93, 12, 24,  25, 32, 31, 23, 12, 48, 12,  13, 14,  23,  0,  84,
    85, 3,  15, 23, 26, 23,  19, 20, 27, 24, 24, 76, 28,  77, 29,  29,  24, 25,
    23, 30, 24, 31, 12, 122, 96, 11, 23, 23, 5,  11, 11,  95, 23,  125, 23, 23,
    23, 3,  6,  22, 7,  3,   16, 23, 23, 23, 31, 23, 114, 44, 118, 93,  -1, 62,
    -1, -1, 31, -1, -1, -1,  -1, -1, -1, -1, -1, -1, -1,  131};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] = {
    0,  24, 25, 35, 36, 37, 39, 41, 43, 42, 44, 0,  36, 26, 45, 45, 57, 57, 46,
    27, 47, 47, 3,  4,  31, 58, 65, 69, 57, 48, 28, 49, 49, 23, 8,  12, 13, 14,
    23, 70, 71, 75, 76, 78, 59, 50, 38, 40, 41, 30, 55, 9,  10, 66, 31, 12, 23,
    77, 76, 76, 31, 71, 11, 9,  10, 32, 21, 31, 60, 61, 62, 69, 57, 29, 38, 51,
    45, 56, 23, 23, 5,  67, 12, 23, 11, 11, 75, 23, 23, 23, 79, 23, 65, 64, 65,
    52, 47, 57, 6,  80, 15, 19, 20, 72, 76, 76, 23, 33, 22, 63, 61, 65, 31, 57,
    49, 23, 23, 23, 7,  68, 23, 31, 40, 16, 73, 70, 29, 53, 17, 18, 74, 54, 57};

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] = {
    0,  34, 35, 35, 36, 36, 37, 38, 39, 40, 40, 40, 42, 41, 44, 43, 45, 46,
    45, 47, 48, 47, 49, 50, 49, 51, 52, 51, 54, 53, 55, 56, 55, 57, 57, 58,
    58, 58, 59, 59, 60, 60, 60, 60, 61, 61, 61, 62, 63, 63, 64, 64, 65, 66,
    66, 66, 67, 67, 68, 68, 69, 69, 70, 70, 71, 71, 71, 72, 72, 72, 72, 73,
    73, 74, 74, 75, 75, 76, 76, 77, 77, 78, 78, 78, 78, 79, 79, 80, 80};

/* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] = {
    0, 2, 1, 2, 1, 1, 6, 6, 5, 0, 1, 2, 0, 3, 0, 3, 0, 0, 3, 0, 0, 3, 0,
    0, 3, 0, 0, 3, 0, 3, 0, 0, 3, 0, 2, 1, 1, 1, 0, 2, 2, 1, 1, 1, 3, 2,
    1, 3, 0, 2, 3, 2, 6, 0, 2, 2, 0, 2, 0, 2, 3, 3, 1, 2, 3, 4, 4, 0, 4,
    2, 2, 0, 1, 1, 1, 1, 1, 2, 3, 1, 2, 1, 3, 3, 4, 1, 2, 0, 1};

#define yyerrok (yyerrstatus = 0)
#define yyclearin (yychar = YYEMPTY)
#define YYEMPTY (-2)
#define YYEOF 0

#define YYACCEPT goto yyacceptlab
#define YYABORT goto yyabortlab
#define YYERROR goto yyerrorlab

#define YYRECOVERING() (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                                 \
  do                                                                           \
    if (yychar == YYEMPTY) {                                                   \
      yychar = (Token);                                                        \
      yylval = (Value);                                                        \
      YYPOPSTACK(yylen);                                                       \
      yystate = *yyssp;                                                        \
      goto yybackup;                                                           \
    } else {                                                                   \
      yyerror(YY_("syntax error: cannot back up"));                            \
      YYERROR;                                                                 \
    }                                                                          \
  while (0)

/* Error token number */
#define YYTERROR 1
#define YYERRCODE 256

/* Enable debugging if requested.  */
#if YYDEBUG

#ifndef YYFPRINTF
#include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#define YYFPRINTF fprintf
#endif

#define YYDPRINTF(Args)                                                        \
  do {                                                                         \
    if (yydebug)                                                               \
      YYFPRINTF Args;                                                          \
  } while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
#define YY_LOCATION_PRINT(File, Loc) ((void)0)
#endif

#define YY_SYMBOL_PRINT(Title, Type, Value, Location)                          \
  do {                                                                         \
    if (yydebug) {                                                             \
      YYFPRINTF(stderr, "%s ", Title);                                         \
      yy_symbol_print(stderr, Type, Value);                                    \
      YYFPRINTF(stderr, "\n");                                                 \
    }                                                                          \
  } while (0)

/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void yy_symbol_value_print(FILE *yyo, int yytype,
                                  YYSTYPE const *const yyvaluep) {
  FILE *yyoutput = yyo;
  YYUSE(yyoutput);
  if (!yyvaluep)
    return;
#ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT(yyo, yytoknum[yytype], *yyvaluep);
#endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE(yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}

/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void yy_symbol_print(FILE *yyo, int yytype,
                            YYSTYPE const *const yyvaluep) {
  YYFPRINTF(yyo, "%s %s (", yytype < YYNTOKENS ? "token" : "nterm",
            yytname[yytype]);

  yy_symbol_value_print(yyo, yytype, yyvaluep);
  YYFPRINTF(yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void yy_stack_print(yy_state_t *yybottom, yy_state_t *yytop) {
  YYFPRINTF(stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++) {
    int yybot = *yybottom;
    YYFPRINTF(stderr, " %d", yybot);
  }
  YYFPRINTF(stderr, "\n");
}

#define YY_STACK_PRINT(Bottom, Top)                                            \
  do {                                                                         \
    if (yydebug)                                                               \
      yy_stack_print((Bottom), (Top));                                         \
  } while (0)

/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void yy_reduce_print(yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule) {
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF(stderr, "Reducing stack by rule %d (line %d):\n", yyrule - 1,
            yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++) {
    YYFPRINTF(stderr, "   $%d = ", yyi + 1);
    yy_symbol_print(stderr, yystos[+yyssp[yyi + 1 - yynrhs]],
                    &yyvsp[(yyi + 1) - (yynrhs)]);
    YYFPRINTF(stderr, "\n");
  }
}

#define YY_REDUCE_PRINT(Rule)                                                  \
  do {                                                                         \
    if (yydebug)                                                               \
      yy_reduce_print(yyssp, yyvsp, Rule);                                     \
  } while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
#define YYDPRINTF(Args)
#define YY_SYMBOL_PRINT(Title, Type, Value, Location)
#define YY_STACK_PRINT(Bottom, Top)
#define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
#define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

#if YYERROR_VERBOSE

#ifndef yystrlen
#if defined __GLIBC__ && defined _STRING_H
#define yystrlen(S) (YY_CAST(YYPTRDIFF_T, strlen(S)))
#else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T yystrlen(const char *yystr) {
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#endif
#endif

#ifndef yystpcpy
#if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#define yystpcpy stpcpy
#else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *yystpcpy(char *yydest, const char *yysrc) {
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T yytnamerr(char *yyres, const char *yystr) {
  if (*yystr == '"') {
    YYPTRDIFF_T yyn = 0;
    char const *yyp = yystr;

    for (;;)
      switch (*++yyp) {
      case '\'':
      case ',':
        goto do_not_strip_quotes;

      case '\\':
        if (*++yyp != '\\')
          goto do_not_strip_quotes;
        else
          goto append;

      append:
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
  do_not_strip_quotes:;
  }

  if (yyres)
    return yystpcpy(yyres, yystr) - yyres;
  else
    return yystrlen(yystr);
}
#endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int yysyntax_error(YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                          yy_state_t *yyssp, int yytoken) {
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY) {
    int yyn = yypact[+*yyssp];
    YYPTRDIFF_T yysize0 = yytnamerr(YY_NULLPTR, yytname[yytoken]);
    yysize = yysize0;
    yyarg[yycount++] = yytname[yytoken];
    if (!yypact_value_is_default(yyn)) {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR &&
            !yytable_value_is_error(yytable[yyx + yyn])) {
          if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM) {
            yycount = 1;
            yysize = yysize0;
            break;
          }
          yyarg[yycount++] = yytname[yyx];
          {
            YYPTRDIFF_T yysize1 = yysize + yytnamerr(YY_NULLPTR, yytname[yyx]);
            if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
              yysize = yysize1;
            else
              return 2;
          }
        }
    }
  }

  switch (yycount) {
#define YYCASE_(N, S)                                                          \
  case N:                                                                      \
    yyformat = S;                                                              \
    break
  default: /* Avoid compiler warnings. */
    YYCASE_(0, YY_("syntax error"));
    YYCASE_(1, YY_("syntax error, unexpected %s"));
    YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
    YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
    YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
    YYCASE_(5,
            YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
  }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen(yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize) {
    *yymsg_alloc = 2 * yysize;
    if (!(yysize <= *yymsg_alloc && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
      *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
    return 1;
  }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount) {
        yyp += yytnamerr(yyp, yyarg[yyi++]);
        yyformat += 2;
      } else {
        ++yyp;
        ++yyformat;
      }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void yydestruct(const char *yymsg, int yytype, YYSTYPE *yyvaluep) {
  YYUSE(yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT(yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE(yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}

/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;

/*----------.
| yyparse.  |
`----------*/

int yyparse(void) {
  yy_state_fast_t yystate;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;

  /* The stacks and their tools:
     'yyss': related to states.
     'yyvs': related to semantic values.

     Refer to the stacks through separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yy_state_t yyssa[YYINITDEPTH];
  yy_state_t *yyss;
  yy_state_t *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs;
  YYSTYPE *yyvsp;

  YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N) (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
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
  YYDPRINTF((stderr, "Entering state %d\n", yystate));
  YY_ASSERT(0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST(yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
  {
    /* Get the current used size of the three stacks, in elements.  */
    YYPTRDIFF_T yysize = yyssp - yyss + 1;

#if defined yyoverflow
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
      yyoverflow(YY_("memory exhausted"), &yyss1, yysize * YYSIZEOF(*yyssp),
                 &yyvs1, yysize * YYSIZEOF(*yyvsp), &yystacksize);
      yyss = yyss1;
      yyvs = yyvs1;
    }
#else /* defined YYSTACK_RELOCATE */
    /* Extend the stack our own way.  */
    if (YYMAXDEPTH <= yystacksize)
      goto yyexhaustedlab;
    yystacksize *= 2;
    if (YYMAXDEPTH < yystacksize)
      yystacksize = YYMAXDEPTH;

    {
      yy_state_t *yyss1 = yyss;
      union yyalloc *yyptr =
          YY_CAST(union yyalloc *,
                  YYSTACK_ALLOC(YY_CAST(YYSIZE_T, YYSTACK_BYTES(yystacksize))));
      if (!yyptr)
        goto yyexhaustedlab;
      YYSTACK_RELOCATE(yyss_alloc, yyss);
      YYSTACK_RELOCATE(yyvs_alloc, yyvs);
#undef YYSTACK_RELOCATE
      if (yyss1 != yyssa)
        YYSTACK_FREE(yyss1);
    }
#endif

    yyssp = yyss + yysize - 1;
    yyvsp = yyvs + yysize - 1;

    YY_IGNORE_USELESS_CAST_BEGIN
    YYDPRINTF(
        (stderr, "Stack size increased to %ld\n", YY_CAST(long, yystacksize)));
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
  if (yypact_value_is_default(yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY) {
    YYDPRINTF((stderr, "Reading a token: "));
    yychar = yylex();
  }

  if (yychar <= YYEOF) {
    yychar = yytoken = YYEOF;
    YYDPRINTF((stderr, "Now at end of input.\n"));
  } else {
    yytoken = YYTRANSLATE(yychar);
    YY_SYMBOL_PRINT("Next token is", yytoken, &yylval, &yylloc);
  }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0) {
    if (yytable_value_is_error(yyn))
      goto yyerrlab;
    yyn = -yyn;
    goto yyreduce;
  }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT("Shifting", yytoken, &yylval, &yylloc);
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
  yyval = yyvsp[1 - yylen];

  YY_REDUCE_PRINT(yyn);
  switch (yyn) {
  case 12:
#line 119 "ppparse.y"
  {
    pp_set_current_division(PP_IDENTIFICATION_DIVISION);
  }
#line 1472 "ppparse.c"
  break;

  case 14:
#line 124 "ppparse.y"
  {
    pp_set_current_division(PP_FUNCTION_DIVISION);
  }
#line 1478 "ppparse.c"
  break;

  case 17:
#line 128 "ppparse.y"
  {
    pp_set_current_division(PP_ENVIRONMENT_DIVISION);
  }
#line 1484 "ppparse.c"
  break;

  case 20:
#line 132 "ppparse.y"
  {
    pp_set_current_division(PP_DATA_DIVISION);
  }
#line 1490 "ppparse.c"
  break;

  case 23:
#line 136 "ppparse.y"
  {
    pp_set_current_division(PP_PROCEDURE_DIVISION);
  }
#line 1496 "ppparse.c"
  break;

  case 26:
#line 140 "ppparse.y"
  {
    pp_set_current_division(PP_OUT_OF_DIVISION);
  }
#line 1502 "ppparse.c"
  break;

  case 28:
#line 144 "ppparse.y"
  {
    pp_set_current_division(PP_OUT_OF_DIVISION);
  }
#line 1508 "ppparse.c"
  break;

  case 31:
#line 148 "ppparse.y"
  {
    pp_set_current_division(PP_OUT_OF_DIVISION);
  }
#line 1514 "ppparse.c"
  break;

  case 46:
#line 166 "ppparse.y"
  {
    pp_omit_data_entry_name(0);
    pp_omit_data_redef_name(0);
  }
#line 1523 "ppparse.c"
  break;

  case 47:
#line 174 "ppparse.y"
  {
    pp_omit_data_entry_name(1);
    pp_omit_data_redef_name(((yyvsp[0].s)) ? 1 : 0);
  }
#line 1532 "ppparse.c"
  break;

  case 48:
#line 181 "ppparse.y"
  {
    (yyval.s) = NULL;
  }
#line 1538 "ppparse.c"
  break;

  case 49:
#line 182 "ppparse.y"
  {
    (yyval.s) = (yyvsp[0].s);
  }
#line 1544 "ppparse.c"
  break;

  case 52:
#line 192 "ppparse.y"
  {
    fputc('\n', ppout);
    (yyvsp[-4].s) = fix_filename((yyvsp[-4].s));
    if (cb_flag_fold_copy_lower) {
      (yyvsp[-4].s) = fold_lower((yyvsp[-4].s));
    } else if (cb_flag_fold_copy_upper) {
      (yyvsp[-4].s) = fold_upper((yyvsp[-4].s));
    }
    if ((yyvsp[-3].s)) {
      (yyvsp[-3].s) = fix_filename((yyvsp[-3].s));
      if (cb_flag_fold_copy_lower) {
        (yyvsp[-3].s) = fold_lower((yyvsp[-3].s));
      } else if (cb_flag_fold_copy_upper) {
        (yyvsp[-3].s) = fold_upper((yyvsp[-3].s));
      }
    }
    ppcopy((yyvsp[-4].s), (yyvsp[-3].s), (yyvsp[-1].jx), (yyvsp[0].r));
  }
#line 1567 "ppparse.c"
  break;

  case 53:
#line 213 "ppparse.y"
  {
    (yyval.s) = NULL;
  }
#line 1573 "ppparse.c"
  break;

  case 54:
#line 214 "ppparse.y"
  {
    (yyval.s) = (yyvsp[0].s);
  }
#line 1579 "ppparse.c"
  break;

  case 55:
#line 215 "ppparse.y"
  {
    (yyval.s) = (yyvsp[0].s);
  }
#line 1585 "ppparse.c"
  break;

  case 58:
#line 223 "ppparse.y"
  {
    (yyval.r) = NULL;
  }
#line 1591 "ppparse.c"
  break;

  case 59:
#line 224 "ppparse.y"
  {
    (yyval.r) = (yyvsp[0].r);
  }
#line 1597 "ppparse.c"
  break;

  case 60:
#line 228 "ppparse.y"
  {
    pp_set_replace_list((yyvsp[-1].r));
  }
#line 1603 "ppparse.c"
  break;

  case 61:
#line 229 "ppparse.y"
  {
    pp_set_replace_list(NULL);
  }
#line 1609 "ppparse.c"
  break;

  case 62:
#line 233 "ppparse.y"
  {
    (yyval.r) = cb_replace_list_add_list(NULL, (yyvsp[0].r));
  }
#line 1615 "ppparse.c"
  break;

  case 63:
#line 234 "ppparse.y"
  {
    (yyval.r) = cb_replace_list_add_list((yyvsp[-1].r), (yyvsp[0].r));
  }
#line 1621 "ppparse.c"
  break;

  case 64:
#line 238 "ppparse.y"
  {
    (yyval.r) = cb_replace_list_add(NULL, (yyvsp[-2].l), (yyvsp[0].l));
  }
#line 1627 "ppparse.c"
  break;

  case 65:
#line 240 "ppparse.y"
  {
    (yyval.r) = cb_replace_list_add(NULL, (yyvsp[-2].l), (yyvsp[0].l));
    cb_replace_list_set_type((yyval.r), CB_REPLACE_LEADING);
  }
#line 1636 "ppparse.c"
  break;

  case 66:
#line 245 "ppparse.y"
  {
    (yyval.r) = cb_replace_list_add(NULL, (yyvsp[-2].l), (yyvsp[0].l));
    cb_replace_list_set_type((yyval.r), CB_REPLACE_TRAILING);
  }
#line 1645 "ppparse.c"
  break;

  case 67:
#line 252 "ppparse.y"
  {
    (yyval.jx) = NULL;
  }
#line 1651 "ppparse.c"
  break;

  case 68:
#line 254 "ppparse.y"
  {
    struct cb_joining_ext *p = cobc_malloc(sizeof(struct cb_joining_ext));
    p->ext = (yyvsp[-2].s);
    p->type = (yyvsp[0].jt);
    (yyval.jx) = p;
  }
#line 1662 "ppparse.c"
  break;

  case 69:
#line 261 "ppparse.y"
  {
    struct cb_joining_ext *p = cobc_malloc(sizeof(struct cb_joining_ext));
    p->ext = (yyvsp[0].s);
    p->type = prefixing;
    (yyval.jx) = p;
  }
#line 1673 "ppparse.c"
  break;

  case 70:
#line 268 "ppparse.y"
  {
    struct cb_joining_ext *p = cobc_malloc(sizeof(struct cb_joining_ext));
    p->ext = (yyvsp[0].s);
    p->type = suffixing;
    (yyval.jx) = p;
  }
#line 1684 "ppparse.c"
  break;

  case 73:
#line 279 "ppparse.y"
  {
    (yyval.jt) = joining_as_prefix;
  }
#line 1690 "ppparse.c"
  break;

  case 74:
#line 280 "ppparse.y"
  {
    (yyval.jt) = joining_as_suffix;
  }
#line 1696 "ppparse.c"
  break;

  case 75:
#line 284 "ppparse.y"
  {
    (yyval.l) = (yyvsp[0].l);
  }
#line 1702 "ppparse.c"
  break;

  case 76:
#line 285 "ppparse.y"
  {
    (yyval.l) = (yyvsp[0].l);
  }
#line 1708 "ppparse.c"
  break;

  case 77:
#line 289 "ppparse.y"
  {
    (yyval.l) = NULL;
  }
#line 1714 "ppparse.c"
  break;

  case 78:
#line 290 "ppparse.y"
  {
    (yyval.l) = (yyvsp[-1].l);
  }
#line 1720 "ppparse.c"
  break;

  case 79:
#line 294 "ppparse.y"
  {
    (yyval.l) = cb_text_list_add(NULL, (yyvsp[0].s));
  }
#line 1726 "ppparse.c"
  break;

  case 80:
#line 295 "ppparse.y"
  {
    (yyval.l) = cb_text_list_add((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 1732 "ppparse.c"
  break;

  case 81:
#line 299 "ppparse.y"
  {
    (yyval.l) = cb_text_list_add(NULL, (yyvsp[0].s));
  }
#line 1738 "ppparse.c"
  break;

  case 82:
#line 301 "ppparse.y"
  {
    (yyval.l) = cb_text_list_add((yyvsp[-2].l), " ");
    (yyval.l) = cb_text_list_add((yyval.l), "IN");
    (yyval.l) = cb_text_list_add((yyval.l), " ");
    (yyval.l) = cb_text_list_add((yyval.l), (yyvsp[0].s));
  }
#line 1749 "ppparse.c"
  break;

  case 83:
#line 308 "ppparse.y"
  {
    (yyval.l) = cb_text_list_add((yyvsp[-2].l), " ");
    (yyval.l) = cb_text_list_add((yyval.l), "OF");
    (yyval.l) = cb_text_list_add((yyval.l), " ");
    (yyval.l) = cb_text_list_add((yyval.l), (yyvsp[0].s));
  }
#line 1760 "ppparse.c"
  break;

  case 84:
#line 315 "ppparse.y"
  {
    struct cb_text_list *l;

    (yyval.l) = cb_text_list_add((yyvsp[-3].l), " ");
    (yyval.l) = cb_text_list_add((yyval.l), "(");
    (yyvsp[-1].l) = cb_text_list_add((yyvsp[-1].l), ")");
    for (l = (yyval.l); l->next; l = l->next)
      ;
    l->next = (yyvsp[-1].l);
  }
#line 1774 "ppparse.c"
  break;

  case 85:
#line 327 "ppparse.y"
  {
    (yyval.l) = cb_text_list_add(NULL, (yyvsp[0].s));
  }
#line 1780 "ppparse.c"
  break;

  case 86:
#line 329 "ppparse.y"
  {
    (yyval.l) = cb_text_list_add((yyvsp[-1].l), " ");
    (yyval.l) = cb_text_list_add((yyval.l), (yyvsp[0].s));
  }
#line 1789 "ppparse.c"
  break;

#line 1793 "ppparse.c"

  default:
    break;
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
  YY_SYMBOL_PRINT("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK(yylen);
  yylen = 0;
  YY_STACK_PRINT(yyss, yyssp);

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
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE(yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus) {
    ++yynerrs;
#if !YYERROR_VERBOSE
    yyerror(YY_("syntax error"));
#else
#define YYSYNTAX_ERROR yysyntax_error(&yymsg_alloc, &yymsg, yyssp, yytoken)
    {
      char const *yymsgp = YY_("syntax error");
      int yysyntax_error_status;
      yysyntax_error_status = YYSYNTAX_ERROR;
      if (yysyntax_error_status == 0)
        yymsgp = yymsg;
      else if (yysyntax_error_status == 1) {
        if (yymsg != yymsgbuf)
          YYSTACK_FREE(yymsg);
        yymsg = YY_CAST(char *, YYSTACK_ALLOC(YY_CAST(YYSIZE_T, yymsg_alloc)));
        if (!yymsg) {
          yymsg = yymsgbuf;
          yymsg_alloc = sizeof yymsgbuf;
          yysyntax_error_status = 2;
        } else {
          yysyntax_error_status = YYSYNTAX_ERROR;
          yymsgp = yymsg;
        }
      }
      yyerror("%s", yymsgp);
      if (yysyntax_error_status == 2)
        goto yyexhaustedlab;
    }
#undef YYSYNTAX_ERROR
#endif
  }

  if (yyerrstatus == 3) {
    /* If just tried and failed to reuse lookahead token after an
       error, discard it.  */

    if (yychar <= YYEOF) {
      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        YYABORT;
    } else {
      yydestruct("Error: discarding", yytoken, &yylval);
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

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK(yylen);
  yylen = 0;
  YY_STACK_PRINT(yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;

/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3; /* Each real token shifted decrements this.  */

  for (;;) {
    yyn = yypact[yystate];
    if (!yypact_value_is_default(yyn)) {
      yyn += YYTERROR;
      if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR) {
        yyn = yytable[yyn];
        if (0 < yyn)
          break;
      }
    }

    /* Pop the current state because it cannot handle the error token.  */
    if (yyssp == yyss)
      YYABORT;

    yydestruct("Error: popping", yystos[yystate], yyvsp);
    YYPOPSTACK(1);
    yystate = *yyssp;
    YY_STACK_PRINT(yyss, yyssp);
  }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Shift the error token.  */
  YY_SYMBOL_PRINT("Shifting", yystos[yyn], yyvsp, yylsp);

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

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror(YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY) {
    /* Make sure we have latest lookahead translation.  See comments at
       user semantic actions for why this is necessary.  */
    yytoken = YYTRANSLATE(yychar);
    yydestruct("Cleanup: discarding lookahead", yytoken, &yylval);
  }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK(yylen);
  YY_STACK_PRINT(yyss, yyssp);
  while (yyssp != yyss) {
    yydestruct("Cleanup: popping", yystos[+*yyssp], yyvsp);
    YYPOPSTACK(1);
  }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE(yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE(yymsg);
#endif
  return yyresult;
}
#line 337 "ppparse.y"

static char *fix_filename(char *name) {
  /* remove quotation from alphanumeric literals */
  if (name[0] == '\'' || name[0] == '\"') {
    name++;
    name[strlen(name) - 1] = 0;
  }
  return name;
}

static char *fold_lower(char *name) {
  unsigned char *p;

  for (p = (unsigned char *)name; *p; p++) {
    if (isupper(*p)) {
      *p = tolower(*p);
    }
  }
  return name;
}

static char *fold_upper(char *name) {
  unsigned char *p;

  for (p = (unsigned char *)name; *p; p++) {
    if (islower(*p)) {
      *p = toupper(*p);
    }
  }
  return name;
}

static struct cb_replace_list *
cb_replace_list_add(struct cb_replace_list *list, struct cb_text_list *old_text,
                    struct cb_text_list *new_text) {
  struct cb_replace_list *p;
  struct cb_replace_list *l;

  p = cobc_malloc(sizeof(struct cb_replace_list));
  p->old_text = old_text;
  p->new_text = new_text;
  p->replace_type = CB_REPLACE_OTHER;
  p->next = NULL;
  if (!list) {
    return p;
  } else {
    for (l = list; l->next; l = l->next)
      ;
    l->next = p;
    return list;
  }
}

static void cb_replace_list_set_type(struct cb_replace_list *list,
                                     int replace_type) {
  struct cb_replace_list *l;
  if (list) {
    for (l = list; l->next; l = l->next)
      ;
    l->replace_type = replace_type;
  }
}

static struct cb_replace_list *
cb_replace_list_add_list(struct cb_replace_list *list,
                         struct cb_replace_list *list_next) {
  struct cb_replace_list *l;

  if (!list) {
    return list_next;
  } else {
    for (l = list; l->next; l = l->next)
      ;
    l->next = list_next;
    return list;
  }
}
