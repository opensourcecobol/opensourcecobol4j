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

#ifndef CB_COBC_H
#define CB_COBC_H

#include <stdio.h>

#include "lib/gettext.h"
#include "libcob.h"

#if !defined(__i386__) && !defined(__x86_64__) && !defined(__powerpc__) &&     \
    !defined(__powerpc64__) && !defined(__ppc__) && !defined(__amd64__)
#define COB_NON_ALIGNED
/* Some DEC Alphas can only directly load shorts at 4-byte aligned addresses */
#ifdef __alpha
#define COB_SHORT_BORK
#endif
#endif

#define ABORT() cobc_abort(__FILE__, __LINE__)

#define CB_FORMAT_FIXED 0
#define CB_FORMAT_FREE 1
#define CB_FORMAT_FREE_1COL_ASTER 2

extern int cb_source_format;
extern int cb_source_format1;

extern int cb_display_sign;

extern struct cb_exception {
  const char *name; /* exception name */
  const int code;   /* exception code */
  int enable;       /* if turned on */
} cb_exception_table[];

#define CB_EXCEPTION_NAME(id) cb_exception_table[id].name
#define CB_EXCEPTION_CODE(id) cb_exception_table[id].code
#define CB_EXCEPTION_ENABLE(id) cb_exception_table[id].enable

#undef CB_FLAG
#define CB_FLAG(var, name, doc) extern int var;
#include "flag.def"
#undef CB_FLAG

#undef CB_WARNDEF
#define CB_WARNDEF(var, name, wall, doc) extern int var;
#include "warning.def"
#undef CB_WARNDEF

struct cb_text_list {
  const char *text;
  struct cb_text_list *next;
};

struct cb_constant_list {
  struct cb_constant_list *next;
  char *name;
  int type;
  int numvalue;
  char *alphavalue;
};

struct cb_replace_list {
  struct cb_text_list *old_text;
  struct cb_text_list *new_text;
  struct cb_replace_list *next;
  int replace_type;
};

typedef enum {
  joining_as_prefix,
  joining_as_suffix,
  prefixing,
  suffixing
} cb_joining_ext_type_t;

struct cb_joining_ext {
  char *ext;
  cb_joining_ext_type_t type;
};

struct local_filename {
  struct local_filename *next;
  char *local_name;
  FILE *local_fp;
};

struct filename {
  struct filename *next;
  char *source;                     /* foo.cob */
  char *preprocess;                 /* foo.i */
  char *translate;                  /* foo.c */
  char *trstorage;                  /* foo.c.h */
  char *object;                     /* foo.o */
  char *demangle_source;            /* foo */
  struct local_filename *localfile; /* foo.c.l[n].h */
  int need_preprocess;
  int need_translate;
  int need_assemble;
};

extern int cb_id;
extern int cb_attr_id;
extern int cb_literal_id;
extern int cb_field_id;
extern int cb_storage_id;
extern int cb_flag_main;

extern int external_flg;
extern int errorcount;
extern int warningcount;
extern int alt_ebcdic;
extern int optimize_flag;
extern int has_external;

extern char *cb_oc_build_stamp;
extern char *cb_source_file;
extern int cb_source_line;

extern const char *cob_config_dir;

extern char *cb_java_package_name;

extern char *source_name;
extern char *demangle_name;
extern FILE *cb_storage_file;
extern char *cb_storage_file_name;

extern char **cb_saveargv;
extern int cb_saveargc;

extern FILE *cb_listing_file;
extern FILE *cb_depend_file;
extern char *cb_depend_target;
extern struct cb_text_list *cb_depend_list;
extern struct cb_text_list *cb_include_list;
extern struct cb_text_list *cb_extension_list;
extern struct cb_constant_list *cb_const_list;

extern struct cb_program *current_program;
extern struct cb_statement *current_statement;
extern struct cb_label *current_section;
extern struct cb_label *current_paragraph;
extern size_t functions_are_all;

extern struct cb_text_list *cb_text_list_add(struct cb_text_list *list,
                                             const char *name);
extern void cb_constant_list_add(char *buff);
extern void *cobc_malloc(const size_t size);
extern void *cobc_realloc(void *prevptr, const size_t size);

DECLNORET extern void cobc_abort(const char *filename,
                                 const int linenum) COB_A_NORETURN;

extern size_t cobc_check_valid_name(char *name);

extern char edit_code_command[512];
extern char edit_code_command_is_set;

#ifdef I18N_UTF8
#define COB_U8CSIZ 3

#define COB_U8BYTE_1(c)                                                        \
  ((((c) >> 7) == 0x00)                                                        \
       ? 1                                                                     \
       : (((c) >> 5) == 0x06)                                                  \
             ? 2                                                               \
             : (((c) >> 4) == 0x0e)                                            \
                   ? 3                                                         \
                   : (((c) >> 3) == 0x1e)                                      \
                         ? 4                                                   \
                         : (((c) >> 2) == 0x3e)                                \
                               ? 5                                             \
                               : (((c) >> 1) == 0x7e) ? 6 : 0)
#define COB_U8BYTE_N(c) ((c) >> 6 == 2)
#endif /*I18N_UTF8*/

#ifdef I18N_UTF8
extern const unsigned char *utf8_ext_pick(const unsigned char *);
extern size_t utf8_strlen(const unsigned char *p);
extern int utf8_casecmp(const char *s1, const char *s2);
extern void utf8_spc_to_ascii(char *);
extern int utf8_national_length(const unsigned char *str, int len);
#else  /*!I18N_UTF8*/
extern const unsigned char *sjis_pick(const unsigned char *);
extern size_t sjis_strlen(const unsigned char *);
extern int sjis_casecmp(const char *, const char *);
extern void sjis_spc_to_ascii(char *);
#endif /*I18N_UTF8*/

#ifdef I18N_UTF8
#define cobc_strlen utf8_strlen
#define cobc_casecmp utf8_casecmp
#define cobc_mbspc2ascii utf8_spc_to_ascii
#else /*!I18N_UTF8*/
#define cobc_strlen sjis_strlen
#define cobc_casecmp sjis_casecmp
#define cobc_mbspc2ascii sjis_spc_to_ascii
#endif /*I18N_UTF8*/

/* config.c */

struct noreserve {
  struct noreserve *next;
  char *noresword;
};

extern struct noreserve *norestab;

enum cb_default_organization {
  CB_ORG_RECORD_SEQUENTIAL, /* Organization is Record Sequential */
  CB_ORG_LINE_SEQUENTIAL    /* Line Sequential */
};

enum cb_assign_clause {
  CB_ASSIGN_COBOL2002, /* COBOL 2002 standard */
  CB_ASSIGN_MF,        /* Micro Focus COBOL compatibility */
  CB_ASSIGN_IBM,       /* IBM COBOL compatibility */
  CB_ASSIGN_JPH1       /* JP compatibility */
};

enum cb_binary_byteorder { CB_BYTEORDER_NATIVE, CB_BYTEORDER_BIG_ENDIAN };

enum cb_binary_size {
  CB_BINARY_SIZE_2_4_8,   /* 2,4,8 bytes */
  CB_BINARY_SIZE_1_2_4_8, /* 1,2,4,8 bytes */
  CB_BINARY_SIZE_1__8     /* 1,2,3,4,5,6,7,8 bytes */
};

enum cb_abort_on_io_exception {
  CB_ABORT_ON_IO_ANY,
  CB_ABORT_ON_IO_FATAL,
  CB_ABORT_ON_IO_NEVER
};

enum cb_operation_type {
  CB_OPERATION_READ,
  CB_OPERATION_WRITE,
  CB_OPERATION_ASSIGN
};

enum cb_support {
  CB_OK,
  CB_WARNING,
  CB_ARCHAIC,
  CB_OBSOLETE,
  CB_SKIP,
  CB_IGNORE,
  CB_ERROR,
  CB_UNCONFORMABLE
};

enum cb_replace_type {
  CB_REPLACE_LEADING,
  CB_REPLACE_TRAILING,
  CB_REPLACE_OTHER
};

enum cb_constant_type {
  CB_CONSTANT_TYPE_NONE,
  CB_CONSTANT_TYPE_ALPANUM,
  CB_CONSTANT_TYPE_NUMERIC
};

#undef CB_CONFIG_ANY
#undef CB_CONFIG_INT
#undef CB_CONFIG_STRING
#undef CB_CONFIG_BOOLEAN
#undef CB_CONFIG_SUPPORT

#define CB_CONFIG_ANY(type, var, name) extern type var;
#define CB_CONFIG_INT(var, name) extern int var;
#define CB_CONFIG_STRING(var, name) extern const char *var;
#define CB_CONFIG_CHAR(var, name) extern char var;
#define CB_CONFIG_BOOLEAN(var, name) extern int var;
#define CB_CONFIG_SUPPORT(var, name) extern enum cb_support var;
#include "config.def"
#undef CB_CONFIG_ANY
#undef CB_CONFIG_INT
#undef CB_CONFIG_STRING
#undef CB_CONFIG_BOOLEAN
#undef CB_CONFIG_SUPPORT

extern int cb_load_std(const char *name);
extern int cb_load_conf(const char *fname, const int check_nodef,
                        const int prefix_dir);

/* preprocessor (in pplex.l, ppparse.y) */
extern FILE *ppin;
extern FILE *ppout;
extern int pplex(void);
extern int ppparse(void);

extern int ppopen(const char *name, struct cb_joining_ext *joining_ext,
                  struct cb_replace_list *replace_list);
extern int ppcopy(const char *name, const char *lib,
                  struct cb_joining_ext *joining_ext,
                  struct cb_replace_list *replace_list);
extern void pp_set_replace_list(struct cb_replace_list *replace_list);
extern void pp_set_copy_replace_list(struct cb_replace_list *replace_list);
extern void pp_set_joining_ext(struct cb_joining_ext *joining_ext);

#define PP_OUT_OF_DIVISION 0
#define PP_IDENTIFICATION_DIVISION 1
#define PP_FUNCTION_DIVISION 2
#define PP_ENVIRONMENT_DIVISION 3
#define PP_DATA_DIVISION 4
#define PP_PROCEDURE_DIVISION 5
extern void pp_set_current_division(int divno);
extern void pp_omit_data_entry_name(int on_off);
extern void pp_omit_data_redef_name(int on_off);

/* parser (in scanner.l, parser.y) */
extern FILE *yyin;
extern FILE *yyout;
extern int yylex(void);
extern int yyparse(void);

/* typeck.c */
extern size_t sending_id;
extern size_t suppress_warn;

/* error.c */
extern void cb_warning(const char *fmt, ...) COB_A_FORMAT12;
extern void cb_error(const char *fmt, ...) COB_A_FORMAT12;

extern int cb_verify(const enum cb_support tag, const char *feature);

#endif /* CB_COBC_H */
