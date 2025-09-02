/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

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

#line 97 "ppparse.h"
};
typedef union YYSTYPE YYSTYPE;
#define YYSTYPE_IS_TRIVIAL 1
#define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE pplval;

int ppparse(void);

#endif /* !YY_PP_PPPARSE_H_INCLUDED  */
