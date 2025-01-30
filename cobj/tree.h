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

#ifndef CB_TREE_H
#define CB_TREE_H

#define YYSTYPE cb_tree

#define CB_BEFORE cb_int0
#define CB_AFTER cb_int1

#define COB_MAX_SUBSCRIPTS 16

#define CB_PREFIX_ATTR "a_"             /* field attribute (cob_field_attr) */
#define CB_PREFIX_BASE "b_"             /* base address (unsigned char *) */
#define CB_PREFIX_CONST "c_"            /* constant or literal (cob_field) */
#define CB_PREFIX_DECIMAL "d_"          /* decimal number (cob_decimal) */
#define CB_PREFIX_FIELD "f_"            /* field (cob_field) */
#define CB_PREFIX_FILE "h_"             /* file (cob_file) */
#define CB_PREFIX_KEYS "k_"             /* file keys (struct cob_file_key []) */
#define CB_PREFIX_LABEL "l_"            /* label */
#define CB_PREFIX_SEQUENCE "s_"         /* collating sequence */
#define CB_PREFIX_STRING_LITERAL "str_" /* string literal */

#define CB_PROGRAM_TYPE 0
#define CB_FUNCTION_TYPE 1

enum cb_tag {
  /* primitives */
  CB_TAG_CONST,         /* 0 constant value */
  CB_TAG_INTEGER,       /* 1 integer constant */
  CB_TAG_STRING,        /* 2 string constant */
  CB_TAG_ALPHABET_NAME, /* 3 alphabet-name */
  CB_TAG_CLASS_NAME,    /* 4 class-name */
  CB_TAG_LOCALE_NAME,   /* 5 locale-name */
  CB_TAG_SYSTEM_NAME,   /* 6 system-name */
  CB_TAG_LITERAL,       /* 7 numeric/alphanumeric literal */
  CB_TAG_DECIMAL,       /* 8 decimal number */
  CB_TAG_FIELD,         /* 9 user-defined variable */
  CB_TAG_FILE,          /* 10 file description */
  /* expressions */
  CB_TAG_REFERENCE, /* 11 reference to a field, file, or label */
  CB_TAG_BINARY_OP, /* 12 binary operation */
  CB_TAG_FUNCALL,   /* 13 run-time function call */
  CB_TAG_CAST,      /* 14 type cast */
  CB_TAG_INTRINSIC, /* 15 intrinsic function */
  /* statements */
  CB_TAG_LABEL,      /* 16 label statement */
  CB_TAG_ASSIGN,     /* 17 assignment statement */
  CB_TAG_INITIALIZE, /* 18 INITIALIZE statement */
  CB_TAG_SEARCH,     /* 19 SEARCH statement */
  CB_TAG_CALL,       /* 20 CALL statement */
  CB_TAG_GOTO,       /* 21 GO TO statement */
  CB_TAG_IF,         /* 22 IF statement */
  CB_TAG_PERFORM,    /* 23 PERFORM statement */
  CB_TAG_STATEMENT,  /* 24 general statement */
  CB_TAG_CONTINUE,   /* 25 CONTINUE statement */
  /* miscellaneous */
  CB_TAG_PERFORM_VARYING, /* 26 PERFORM VARYING parameter */
  CB_TAG_PICTURE,         /* 27 PICTURE clause */
  CB_TAG_LIST,            /* 28 list */
  CB_TAG_SORT_INIT,       /* 29 SORT initializer */
  CB_TAG_SORT_PROC,       /* 30 SORT INPUT/OUTPUT clause */
  CB_TAG_RETURN,          /* 31 RETURN statement */
  CB_TAG_RELEASE,         /* 32 RELEASE statement */
  CB_TAG_SORT_FINISH,     /* 33 SORT terminator */

  CB_TAG_JAVA_CONTINUE,
  CB_TAG_JAVA_BREAK,

  CB_TAG_SWITCH,
};

enum cb_alphabet_name_type {
  CB_ALPHABET_NATIVE,
  CB_ALPHABET_STANDARD_1,
  CB_ALPHABET_STANDARD_2,
  CB_ALPHABET_EBCDIC,
  CB_ALPHABET_CUSTOM
};

enum cb_system_name_category {
  CB_CALL_CONVENTION_NAME,
  CB_CODE_NAME,
  CB_COMPUTER_NAME,
  CB_DEVICE_NAME,
  CB_ENTRY_CONVENTION_NAME,
  CB_EXTERNAL_LOCALE_NAME,
  CB_FEATURE_NAME,
  CB_LIBRARY_NAME,
  CB_SWITCH_NAME,
  CB_TEXT_NAME,
  CB_INTERFACE_NAME
};

enum cb_device_name {
  CB_DEVICE_SYSIN,
  CB_DEVICE_SYSOUT,
  CB_DEVICE_SYSERR,
  CB_DEVICE_CONSOLE
};

enum cb_feature_name {
  CB_FEATURE_FORMFEED,
  CB_FEATURE_C01,
  CB_FEATURE_C02,
  CB_FEATURE_C03,
  CB_FEATURE_C04,
  CB_FEATURE_C05,
  CB_FEATURE_C06,
  CB_FEATURE_C07,
  CB_FEATURE_C08,
  CB_FEATURE_C09,
  CB_FEATURE_C10,
  CB_FEATURE_C11,
  CB_FEATURE_C12
};

enum cb_switch_name {
  CB_SWITCH_1,
  CB_SWITCH_2,
  CB_SWITCH_3,
  CB_SWITCH_4,
  CB_SWITCH_5,
  CB_SWITCH_6,
  CB_SWITCH_7,
  CB_SWITCH_8
};

enum cb_interface_name {
  CB_ARGUMENT_NUMBER,
  CB_ARGUMENT_VALUE,
  CB_ENVIRONMENT_NAME,
  CB_ENVIRONMENT_VALUE
};

enum cb_class {
  CB_CLASS_UNKNOWN,            /* 0 */
  CB_CLASS_ALPHABETIC,         /* 1 */
  CB_CLASS_ALPHANUMERIC,       /* 2 */
  CB_CLASS_ALPHANUMERICEDITED, /* 3 */
  CB_CLASS_BOOLEAN,            /* 4 */
  CB_CLASS_INDEX,              /* 5 */
  CB_CLASS_NATIONAL,           /* 6 */
  CB_CLASS_NATIONALEDITED,     /* 7 */
  CB_CLASS_NUMERIC,            /* 8 */
  CB_CLASS_NUMERICEDIT,        /* 9 */
  CB_CLASS_OBJECT,             /* 10 */
  CB_CLASS_POINTER             /* 11 */
};

enum cb_category {
  CB_CATEGORY_UNKNOWN,             /* 0 */
  CB_CATEGORY_ALPHABETIC,          /* 1 */
  CB_CATEGORY_ALPHANUMERIC,        /* 2 */
  CB_CATEGORY_ALPHANUMERIC_EDITED, /* 3 */
  CB_CATEGORY_BOOLEAN,             /* 4 */
  CB_CATEGORY_INDEX,               /* 5 */
  CB_CATEGORY_NATIONAL,            /* 6 */
  CB_CATEGORY_NATIONAL_EDITED,     /* 7 */
  CB_CATEGORY_NUMERIC,             /* 8 */
  CB_CATEGORY_NUMERIC_EDITED,      /* 9 */
  CB_CATEGORY_OBJECT_REFERENCE,    /* 10 */
  CB_CATEGORY_DATA_POINTER,        /* 11 */
  CB_CATEGORY_PROGRAM_POINTER      /* 12 */
};

enum cb_storage {
  CB_STORAGE_CONSTANT,     /* Constants */
  CB_STORAGE_FILE,         /* FILE SECTION */
  CB_STORAGE_WORKING,      /* WORKING-STORAGE SECTION */
  CB_STORAGE_LOCAL,        /* LOCAL-STORAGE SECTION */
  CB_STORAGE_LINKAGE,      /* LINKAGE SECTION */
  CB_STORAGE_SCREEN,       /* SCREEN SECTION */
  CB_STORAGE_REPORT,       /* REPORT SECTION */
  CB_STORAGE_COMMUNICATION /* COMMUNICATION SECTION */
};

enum cb_usage {
  CB_USAGE_BINARY,          /* 0 */
  CB_USAGE_BIT,             /* 1 */
  CB_USAGE_COMP_5,          /* 2 */
  CB_USAGE_COMP_X,          /* 3 */
  CB_USAGE_DISPLAY,         /* 4 */
  CB_USAGE_FLOAT,           /* 5 */
  CB_USAGE_DOUBLE,          /* 6 */
  CB_USAGE_INDEX,           /* 7 */
  CB_USAGE_NATIONAL,        /* 8 */
  CB_USAGE_OBJECT,          /* 9 */
  CB_USAGE_PACKED,          /* 10 */
  CB_USAGE_POINTER,         /* 11 */
  CB_USAGE_PROGRAM,         /* 12 */
  CB_USAGE_LENGTH,          /* 13 */
  CB_USAGE_PROGRAM_POINTER, /* 14 */
  CB_USAGE_UNSIGNED_CHAR,   /* 15 */
  CB_USAGE_SIGNED_CHAR,     /* 16 */
  CB_USAGE_UNSIGNED_SHORT,  /* 17 */
  CB_USAGE_SIGNED_SHORT,    /* 18 */
  CB_USAGE_UNSIGNED_INT,    /* 19 */
  CB_USAGE_SIGNED_INT,      /* 20 */
  CB_USAGE_UNSIGNED_LONG,   /* 21 */
  CB_USAGE_SIGNED_LONG      /* 22 */
};

enum cb_operand_type { CB_SENDING_OPERAND, CB_RECEIVING_OPERAND };

/*
 * Tree
 */

struct cb_tree_common {
  enum cb_tag tag;
  enum cb_category category;
  unsigned char *source_file;
  int source_line;
};

typedef struct cb_tree_common *cb_tree;

#define CB_TREE(x) ((struct cb_tree_common *)(x))
#define CB_TREE_TAG(x) (CB_TREE(x)->tag)
#define CB_TREE_CLASS(x) cb_tree_class(CB_TREE(x))
#define CB_TREE_CATEGORY(x) cb_tree_category(CB_TREE(x))

#ifdef __GNUC__
#define CB_TREE_CAST(tg, ty, x)                                                \
  ({                                                                           \
    cb_tree _x = (cb_tree)(x);                                                 \
    if (!_x || CB_TREE_TAG(_x) != tg) {                                        \
      cobc_tree_cast_error(_x, __FILE__, __LINE__, tg);                        \
    }                                                                          \
    ((ty *)(_x));                                                              \
  })
#else
#define CB_TREE_CAST(tg, ty, x) ((ty *)(x))
#endif

extern char *cb_name(cb_tree x);
extern enum cb_class cb_tree_class(cb_tree x);
extern enum cb_category cb_tree_category(cb_tree x);
extern int cb_tree_type(cb_tree x);
extern int cb_fits_int(cb_tree x);
extern int cb_fits_long_long(cb_tree x);
extern int cb_get_int(cb_tree x);
extern int cb_is_digist_data(cb_tree x);
extern long long cb_get_long_long(cb_tree x);
extern cb_tree cb_check_zero_division(cb_tree x);

/*
 * Constants
 */

extern cb_tree cb_any;
extern cb_tree cb_true;
extern cb_tree cb_false;
extern cb_tree cb_null;
extern cb_tree cb_zero;
extern cb_tree cb_zen_zero;
extern cb_tree cb_one;
extern cb_tree cb_space;
extern cb_tree cb_blank;
extern cb_tree cb_low;
extern cb_tree cb_high;
extern cb_tree cb_norm_low;
extern cb_tree cb_norm_high;
extern cb_tree cb_quote;
extern cb_tree cb_int0;
extern cb_tree cb_int1;
extern cb_tree cb_int2;
extern cb_tree cb_int3;
extern cb_tree cb_int4;
extern cb_tree cb_int5;
extern cb_tree cb_i[8];
extern cb_tree cb_error_node;

extern cb_tree cb_intr_whencomp;
extern cb_tree cb_intr_pi;
extern cb_tree cb_intr_e;

extern cb_tree cb_standard_error_handler;

extern size_t gen_screen_ptr;

struct cb_const {
  struct cb_tree_common common;
  const char *val;
};

#define CB_CONST(x) (CB_TREE_CAST(CB_TAG_CONST, struct cb_const, x))
#define CB_CONST_P(x) (CB_TREE_TAG(x) == CB_TAG_CONST)

extern void cb_init_constants(void);

/*
 * Integer
 */

struct cb_integer {
  struct cb_tree_common common;
  int val;
};

#define CB_INTEGER(x) (CB_TREE_CAST(CB_TAG_INTEGER, struct cb_integer, x))
#define CB_INTEGER_P(x) (CB_TREE_TAG(x) == CB_TAG_INTEGER)

extern cb_tree cb_int(int n);

/*
 * String
 */

struct cb_string {
  struct cb_tree_common common;
  size_t size;
  const unsigned char *data;
};

#define CB_STRING(x) (CB_TREE_CAST(CB_TAG_STRING, struct cb_string, x))
#define CB_STRING_P(x) (CB_TREE_TAG(x) == CB_TAG_STRING)

#define cb_build_string0(str) cb_build_string(str, strlen((char *)str))

extern cb_tree cb_build_string(const unsigned char *data, size_t size);

/*
 * Alphabet-name
 */

struct cb_alphabet_name {
  struct cb_tree_common common;
  const char *name;
  char *cname;
  cb_tree custom_list;
  enum cb_alphabet_name_type type;
  int low_val_char;
  int high_val_char;
};

#define CB_ALPHABET_NAME(x)                                                    \
  (CB_TREE_CAST(CB_TAG_ALPHABET_NAME, struct cb_alphabet_name, x))
#define CB_ALPHABET_NAME_P(x) (CB_TREE_TAG(x) == CB_TAG_ALPHABET_NAME)

extern cb_tree cb_build_alphabet_name(cb_tree name,
                                      enum cb_alphabet_name_type type);

/*
 * Class-name
 */

struct cb_class_name {
  struct cb_tree_common common;
  const char *name;
  char *cname;
  cb_tree list;
};

#define CB_CLASS_NAME(x)                                                       \
  (CB_TREE_CAST(CB_TAG_CLASS_NAME, struct cb_class_name, x))
#define CB_CLASS_NAME_P(x) (CB_TREE_TAG(x) == CB_TAG_CLASS_NAME)

extern cb_tree cb_build_class_name(cb_tree name, cb_tree list);
extern cb_tree cb_lookup_class_name(const char *name);

/*
 * Locale name
 */

struct cb_locale_name {
  struct cb_tree_common common;
  const char *name;
  char *cname;
  cb_tree list;
};

#define CB_LOCALE_NAME(x)                                                      \
  (CB_TREE_CAST(CB_TAG_LOCALE_NAME, struct cb_locale_name, x))
#define CB_LOCALE_NAME_P(x) (CB_TREE_TAG(x) == CB_TAG_LOCALE_NAME)

extern cb_tree cb_build_locale_name(cb_tree name, cb_tree list);

/*
 * System-name
 */

struct cb_system_name {
  struct cb_tree_common common;
  enum cb_system_name_category category;
  int token;
};

#define CB_SYSTEM_NAME(x)                                                      \
  (CB_TREE_CAST(CB_TAG_SYSTEM_NAME, struct cb_system_name, x))
#define CB_SYSTEM_NAME_P(x) (CB_TREE_TAG(x) == CB_TAG_SYSTEM_NAME)

extern cb_tree cb_build_system_name(enum cb_system_name_category category,
                                    int token);

/*
 * Literal
 */

struct cb_literal {
  struct cb_tree_common common;
  size_t size;
  unsigned char *data;
  signed char all;
  signed char sign; /* unsigned: 0 negative: -1 positive: 1 */
  signed char scale;
  signed char spare; /* spare */
};

#define CB_LITERAL(x) (CB_TREE_CAST(CB_TAG_LITERAL, struct cb_literal, x))
#define CB_LITERAL_P(x) (CB_TREE_TAG(x) == CB_TAG_LITERAL)
#define CB_NUMERIC_LITERAL_P(x)                                                \
  (CB_LITERAL_P(x) && CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC)

extern cb_tree cb_build_numeric_literal(int sign, const unsigned char *data,
                                        int scale);
extern cb_tree cb_build_alphanumeric_literal(const unsigned char *data,
                                             size_t size);
extern cb_tree cb_build_national_literal(const unsigned char *data,
                                         size_t size);
extern cb_tree cb_concat_literals(cb_tree x1, cb_tree x2);

/*
 * Decimal
 */

struct cb_decimal {
  struct cb_tree_common common;
  int id;
};

#define CB_DECIMAL(x) (CB_TREE_CAST(CB_TAG_DECIMAL, struct cb_decimal, x))
#define CB_DECIMAL_P(x) (CB_TREE_TAG(x) == CB_TAG_DECIMAL)

extern cb_tree cb_build_decimal(int id);

/*
 * Picture
 */

struct cb_picture {
  struct cb_tree_common common;
  int size;                  /* byte size */
  int lenstr;                /* length of picture string */
  char *orig;                /* original picture string */
  char *str;                 /* packed picture string */
  enum cb_category category; /* field category */
  unsigned char digits;      /* the number of digit places */
  signed char scale;         /* 1/10^scale */
  unsigned char have_sign;   /* have 'S' */
  unsigned char spare;       /* spare */
  int national;
};

#define CB_PICTURE(x) (CB_TREE_CAST(CB_TAG_PICTURE, struct cb_picture, x))
#define CB_PICTURE_P(x) (CB_TREE_TAG(x) == CB_TAG_PICTURE)

extern cb_tree cb_build_picture(const char *str);

/*
 * Field
 */

struct cb_field {
  struct cb_tree_common common;
  int id;                       /* field id */
  int storage_id;               /* storage id */
  const char *name;             /* the original name */
  const char *ename;            /* the externalized name */
  int size;                     /* field size */
  int memory_size;              /* memory size */
  int offset;                   /* byte offset from top (01 field) */
  int level;                    /* level number */
  int occurs_min;               /* OCCURS <max> */
  int occurs_max;               /* or OCCURS <min> TO <max> */
  int indexes;                  /* number of parents who have OCCURS */
  int count;                    /* reference count */
  char *definition_source_file; /* file name where the field is defined*/
  int definition_source_line;   /* line number where the field is defined */
  int prev_field_line_number;   /* line number of previous field */
  cb_tree occurs_depending;     /* OCCURS ... DEPENDING ON */
  enum cb_storage storage;
  enum cb_usage usage;          /* USAGE */
  cb_tree values;               /* VALUE */
  cb_tree false_88;             /* 88 FALSE clause */
  cb_tree index_list;           /* INDEXED BY */
  struct cb_field *parent;      /* upper level field (NULL for 01 fields) */
  struct cb_field *children;    /* top of lower level fields */
  struct cb_field *sister;      /* fields in the same level */
  struct cb_field *redefines;   /* REDEFINES */
  struct cb_field *rename_thru; /* RENAMES THRU */
  struct cb_field *index_qual;  /* INDEXED BY qualifier */
  struct cb_file *file;         /* file name associated in FD section */
  struct cb_key {
    int dir;     /* ASCENDING or DESCENDING */
    cb_tree key; /* KEY */
    cb_tree ref; /* reference used in SEARCH ALL */
    cb_tree val; /* value to be compared in SEARCH ALL */
  } *keys;
  int nkeys;              /* the number of keys */
  int param_num;          /* CHAINING param number */
  struct cb_picture *pic; /* PICTURE */
  /* screen parameters */
  cb_tree screen_line;
  cb_tree screen_column;
  cb_tree screen_from;
  cb_tree screen_to;
  cb_tree screen_foreg;
  cb_tree screen_backg;
  int screen_flag; /* flags used in SCREEN SECTION */
  /* flags */
  unsigned int flag_external : 1;      /* EXTERNAL */
  unsigned int flag_blank_zero : 1;    /* BLANK WHEN ZERO */
  unsigned int flag_justified : 1;     /* JUSTIFIED RIGHT */
  unsigned int flag_sign_leading : 1;  /* SIGN IS LEADING */
  unsigned int flag_sign_separate : 1; /* SIGN IS SEPARATE */
  unsigned int flag_synchronized : 1;  /* SYNCHRONIZED */
  unsigned int flag_occurs : 1;        /* OCCURS */
  unsigned int flag_invalid : 1;       /* is broken */
  unsigned int flag_binary_swap : 1;   /* binary byteswap */
  unsigned int flag_local : 1;         /* has local scope */
  unsigned int flag_base : 1;          /* has memory allocation */
  unsigned int flag_field : 1;         /* has been internally cached */
  unsigned int flag_item_external : 1; /* is EXTERNAL */
  unsigned int flag_chained : 1;       /* CHAINING item */
  unsigned int flag_real_binary : 1;   /* is BINARY-CHAR/SHORT/LONG/DOUBLE */
  unsigned int flag_item_based : 1;    /* is BASED */
  unsigned int flag_item_78 : 1;       /* is 78 level */
  unsigned int flag_any_length : 1;    /* is ANY LENGTH */
  unsigned int flag_anylen_done : 1;   /* ANY LENGTH is set up */
  unsigned int flag_indexed_by : 1;    /* INDEXED BY item */
  unsigned int flag_is_pointer : 1;    /* is POINTER */
  unsigned int flag_is_verified : 1;   /* has been verified */
  unsigned int flag_is_global : 1;     /* is GLOBAL */
  unsigned int flag_is_c_long : 1;     /* is BINARY-C-LONG */
  unsigned int flag_is_pdiv_parm : 1;  /* is PROC DIV USING */
  unsigned int flag_local_alloced : 1; /* LOCAL storage is allocated */
  unsigned int flag_no_init : 1;       /* no initialize unless used */
  unsigned int flag_spare : 5;
};

#define CB_FIELD(x) (CB_TREE_CAST(CB_TAG_FIELD, struct cb_field, x))
#define CB_FIELD_P(x) (CB_TREE_TAG(x) == CB_TAG_FIELD)

extern cb_tree cb_build_field(cb_tree name);
extern cb_tree cb_build_implicit_field(cb_tree name, int len);
extern cb_tree cb_build_constant(cb_tree name, cb_tree value);

extern struct cb_field *cb_field(cb_tree x);
extern struct cb_field *cb_field_add(struct cb_field *f, struct cb_field *p);
extern int cb_field_size(cb_tree x);
extern struct cb_field *cb_field_founder(struct cb_field *f);
extern struct cb_field *cb_field_variable_size(struct cb_field *f);
extern struct cb_field *cb_field_variable_address(struct cb_field *f);
extern int cb_field_subordinate(struct cb_field *p, const struct cb_field *f);

#define CB_REF_OR_FIELD_P(x) ((CB_FIELD_P(x) || CB_REFERENCE_P(x)))

/* Index */

#define CB_INDEX_P(x)                                                          \
  ((CB_FIELD_P(x) || CB_REFERENCE_P(x)) && cb_field(x)->usage == CB_USAGE_INDEX)

/*
 * Label
 */

struct cb_label {
  struct cb_tree_common common;
  const unsigned char *name;
  struct cb_label *section;
  cb_tree exit_label;
  cb_tree exit_label_ref;
  cb_tree children;
  const unsigned char *orig_name;
  int id;
  int is_section;
  int is_entry;
  unsigned char need_begin;
  unsigned char need_return;
  unsigned char is_global;
  unsigned char spare;
};

#define CB_LABEL(x) (CB_TREE_CAST(CB_TAG_LABEL, struct cb_label, x))
#define CB_LABEL_P(x) (CB_TREE_TAG(x) == CB_TAG_LABEL)

extern cb_tree cb_build_label(cb_tree name, struct cb_label *section);

struct handler_struct {
  struct cb_label *handler_label;
  struct cb_program *handler_prog;
};

/*
 * File
 */

struct cb_key_component {
  struct cb_key_component *next;
  cb_tree component;
};

struct cb_alt_key {
  struct cb_alt_key *next;
  cb_tree key;
  int duplicates;
  int offset;
  struct cb_key_component *component_list;
};

struct cb_file {
  struct cb_tree_common common;
  const char *name; /* The original name */
  char *cname;      /* The name used in C */
  /* SELECT */
  cb_tree assign;                          /* ASSIGN */
  cb_tree file_status;                     /* FILE STATUS */
  cb_tree sharing;                         /* SHARING */
  cb_tree key;                             /* RELATIVE/RECORD KEY */
  struct cb_key_component *component_list; /* RECORD KEY w/split key */
  struct cb_alt_key *alt_key_list;         /* ALTERNATE RECORD KEY */
  /* FD/SD */
  struct cb_field *record;         /* Record descriptor */
  cb_tree record_depending;        /* RECORD DEPENDING */
  cb_tree linage;                  /* LINAGE */
  cb_tree linage_ctr;              /* LINAGE COUNTER */
  cb_tree latfoot;                 /* LINAGE FOOTING */
  cb_tree lattop;                  /* LINAGE TOP */
  cb_tree latbot;                  /* LINAGE BOTTOM */
  struct cb_label *handler;        /* Error handler */
  struct cb_program *handler_prog; /* Prog where defined */
  int record_min;                  /* RECORD CONTAINS */
  int record_max;                  /* RECORD CONTAINS */
  int optional;                    /* OPTIONAL */
  int organization;                /* ORGANIZATION */
  int access_mode;                 /* ACCESS MODE */
  int lock_mode;                   /* LOCK MODE */
  int same_clause;                 /* SAME clause */
  int finalized;                   /* Is finalized */
  int external;                    /* Is EXTERNAL */
  int special;                     /* Special file */
  int external_assign;             /* ASSIGN EXTERNAL */
  int fileid_assign;               /* ASSIGN DISK */
  int global;                      /* Is GLOBAL */
};

#define CB_FILE(x) (CB_TREE_CAST(CB_TAG_FILE, struct cb_file, x))
#define CB_FILE_P(x) (CB_TREE_TAG(x) == CB_TAG_FILE)

extern struct cb_file *build_file(cb_tree name);
extern void validate_file(struct cb_file *f, cb_tree name);
extern void finalize_file(struct cb_file *f, struct cb_field *records);

/*
 * Reference
 */

#define CB_WORD_HASH_SIZE 133

struct cb_word {
  struct cb_word *next; /* next word with the same hash value */
  const char *name;     /* word name */
  cb_tree items;        /* objects associated with this word */
  int count;            /* number of words with the same name */
  int error;            /* set to 1 if error displayed */
};

struct cb_reference {
  struct cb_tree_common common;
  struct cb_word *word;
  enum cb_operand_type type;
  cb_tree value;  /* item referred by this reference */
  cb_tree subs;   /* the list of subscripts */
  cb_tree offset; /* 1st operand of reference modification */
  cb_tree length; /* 2nd operand of reference modification */
  cb_tree check;
  cb_tree chain; /* next qualified name */
  int all;
};

#define CB_REFERENCE(x) (CB_TREE_CAST(CB_TAG_REFERENCE, struct cb_reference, x))
#define CB_REFERENCE_P(x) (CB_TREE_TAG(x) == CB_TAG_REFERENCE)

#define CB_NAME(x) (CB_REFERENCE(x)->word->name)

extern char *cb_get_hexword(char *name);
extern char *cb_get_jisword_buff(const char *name, char *jbuf, size_t n);
extern char *cb_get_jisword(const char *name);
extern cb_tree cb_build_filler(void);
extern cb_tree cb_build_anonymous(void);
extern cb_tree cb_build_reference(const char *name);
extern cb_tree cb_build_field_reference(struct cb_field *f, cb_tree ref);
extern const char *cb_define(cb_tree name, cb_tree val);
extern void cb_define_system_name(const char *name);
extern cb_tree cb_ref(cb_tree x);

/*
 * Binary operation
 */

/*
  '+'	x + y
  '-'	x - y
  '*'	x * y
  '/'	x / y
  '^'	x ** y
  '='	x = y
  '>'	x > y
  '<'	x < y
  '['	x <= y
  ']'	x >= y
  '~'	x != y
  '!'	not x
  '&'	x and y
  '|'	x or y
  '@'	( x )
*/

struct cb_binary_op {
  struct cb_tree_common common;
  int op;
  cb_tree x;
  cb_tree y;
};

#define CB_BINARY_OP(x) (CB_TREE_CAST(CB_TAG_BINARY_OP, struct cb_binary_op, x))
#define CB_BINARY_OP_P(x) (CB_TREE_TAG(x) == CB_TAG_BINARY_OP)

#define cb_build_parenthesis(x) cb_build_binary_op(x, '@', NULL)
#define cb_build_negation(x) cb_build_binary_op(x, '!', NULL)

extern cb_tree cb_build_binary_op(cb_tree x, int op, cb_tree y);
extern cb_tree cb_build_binary_list(cb_tree l, int op);

/*
 * Function call
 */

struct cb_funcall {
  struct cb_tree_common common;
  const char *name;
  cb_tree argv[7];
  int argc;
  int varcnt;
  size_t screenptr;
  char call_type;
};

#define CB_CALL_FUNC 0
#define CB_CALL_METHOD 1
#define CB_CALL_FUNC_P(p) (p->call_type == CB_CALL_FUNC)
#define CB_CALL_METHOD_P(p) (p->call_type == CB_CALL_METHOD)

#define CB_FUNCALL(x) (CB_TREE_CAST(CB_TAG_FUNCALL, struct cb_funcall, x))
#define CB_FUNCALL_P(x) (CB_TREE_TAG(x) == CB_TAG_FUNCALL)

extern cb_tree cb_build_funcall(const char *name, int argc, cb_tree a1,
                                cb_tree a2, cb_tree a3, cb_tree a4, cb_tree a5,
                                cb_tree a6, cb_tree a7);
extern cb_tree cb_build_method_call(const char *name, int argc, cb_tree a1,
                                    cb_tree a2, cb_tree a3, cb_tree a4,
                                    cb_tree a5, cb_tree a6, cb_tree a7);

#define cb_build_funcall_0(f)                                                  \
  cb_build_funcall(f, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
#define cb_build_funcall_1(f, a1)                                              \
  cb_build_funcall(f, 1, a1, NULL, NULL, NULL, NULL, NULL, NULL)
#define cb_build_funcall_2(f, a1, a2)                                          \
  cb_build_funcall(f, 2, a1, a2, NULL, NULL, NULL, NULL, NULL)
#define cb_build_funcall_3(f, a1, a2, a3)                                      \
  cb_build_funcall(f, 3, a1, a2, a3, NULL, NULL, NULL, NULL)
#define cb_build_funcall_4(f, a1, a2, a3, a4)                                  \
  cb_build_funcall(f, 4, a1, a2, a3, a4, NULL, NULL, NULL)
#define cb_build_funcall_5(f, a1, a2, a3, a4, a5)                              \
  cb_build_funcall(f, 5, a1, a2, a3, a4, a5, NULL, NULL)
#define cb_build_funcall_6(f, a1, a2, a3, a4, a5, a6)                          \
  cb_build_funcall(f, 6, a1, a2, a3, a4, a5, a6, NULL)
#define cb_build_funcall_7(f, a1, a2, a3, a4, a5, a6, a7)                      \
  cb_build_funcall(f, 7, a1, a2, a3, a4, a5, a6, a7)

#define cb_build_method_call_0(f)                                              \
  cb_build_method_call(f, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
#define cb_build_method_call_1(f, a1)                                          \
  cb_build_method_call(f, 1, a1, NULL, NULL, NULL, NULL, NULL, NULL)
#define cb_build_method_call_2(f, a1, a2)                                      \
  cb_build_method_call(f, 2, a1, a2, NULL, NULL, NULL, NULL, NULL)
#define cb_build_method_call_3(f, a1, a2, a3)                                  \
  cb_build_method_call(f, 3, a1, a2, a3, NULL, NULL, NULL, NULL)
#define cb_build_method_call_4(f, a1, a2, a3, a4)                              \
  cb_build_method_call(f, 4, a1, a2, a3, a4, NULL, NULL, NULL)
#define cb_build_method_call_5(f, a1, a2, a3, a4, a5)                          \
  cb_build_method_call(f, 5, a1, a2, a3, a4, a5, NULL, NULL)
#define cb_build_method_call_6(f, a1, a2, a3, a4, a5, a6)                      \
  cb_build_method_call(f, 6, a1, a2, a3, a4, a5, a6, NULL)
#define cb_build_method_call_7(f, a1, a2, a3, a4, a5, a6, a7)                  \
  cb_build_method_call(f, 7, a1, a2, a3, a4, a5, a6, a7)

/*
 * Type cast
 */

enum cb_cast_type {
  CB_CAST_INTEGER,
  CB_CAST_ADDRESS,
  CB_CAST_ADDR_OF_ADDR,
  CB_CAST_LENGTH,
  CB_CAST_PROGRAM_POINTER
};

struct cb_cast {
  struct cb_tree_common common;
  enum cb_cast_type type;
  cb_tree val;
};

#define CB_CAST(x) (CB_TREE_CAST(CB_TAG_CAST, struct cb_cast, x))
#define CB_CAST_P(x) (CB_TREE_TAG(x) == CB_TAG_CAST)

extern cb_tree cb_build_cast(enum cb_cast_type type, cb_tree val);

#define cb_build_cast_integer(x) cb_build_cast(CB_CAST_INTEGER, x)
#define cb_build_cast_address(x) cb_build_cast(CB_CAST_ADDRESS, x)
#define cb_build_cast_addr_of_addr(x) cb_build_cast(CB_CAST_ADDR_OF_ADDR, x)
#define cb_build_cast_length(x) cb_build_cast(CB_CAST_LENGTH, x)
#define cb_build_cast_ppointer(x) cb_build_cast(CB_CAST_PROGRAM_POINTER, x)

/*
 * Assign
 */

struct cb_assign {
  struct cb_tree_common common;
  cb_tree var;
  cb_tree val;
};

#define CB_ASSIGN(x) (CB_TREE_CAST(CB_TAG_ASSIGN, struct cb_assign, x))
#define CB_ASSIGN_P(x) (CB_TREE_TAG(x) == CB_TAG_ASSIGN)

extern cb_tree cb_build_assign(cb_tree var, cb_tree val);

/*
 * Intrinsic FUNCTION
 */

enum cb_intr_enum {
  CB_INTR_ABS = 1,
  CB_INTR_ACOS,
  CB_INTR_ANNUITY,
  CB_INTR_ASIN,
  CB_INTR_ATAN,
  CB_INTR_BOOLEAN_OF_INTEGER,
  CB_INTR_BYTE_LENGTH,
  CB_INTR_CHAR,
  CB_INTR_CHAR_NATIONAL,
  CB_INTR_COMBINED_DATETIME,
  CB_INTR_CONCATENATE,
  CB_INTR_COS,
  CB_INTR_CURRENT_DATE,
  CB_INTR_DATE_OF_INTEGER,
  CB_INTR_DATE_TO_YYYYMMDD,
  CB_INTR_DAY_OF_INTEGER,
  CB_INTR_DAY_TO_YYYYDDD,
  CB_INTR_DISPLAY_OF,
  CB_INTR_E,
  CB_INTR_EXCEPTION_FILE,
  CB_INTR_EXCEPTION_FILE_N,
  CB_INTR_EXCEPTION_LOCATION,
  CB_INTR_EXCEPTION_LOCATION_N,
  CB_INTR_EXCEPTION_STATEMENT,
  CB_INTR_EXCEPTION_STATUS,
  CB_INTR_EXP,
  CB_INTR_EXP10,
  CB_INTR_FACTORIAL,
  CB_INTR_FRACTION_PART,
  CB_INTR_HIGHEST_ALGEBRAIC,
  CB_INTR_INTEGER,
  CB_INTR_INTEGER_OF_BOOLEAN,
  CB_INTR_INTEGER_OF_DATE,
  CB_INTR_INTEGER_OF_DAY,
  CB_INTR_INTEGER_PART,
  CB_INTR_LENG,
  CB_INTR_LENGTH,
  CB_INTR_LENGTH_AN,
  CB_INTR_LOCALE_COMPARE,
  CB_INTR_LOCALE_DATE,
  CB_INTR_LOCALE_TIME,
  CB_INTR_LOCALE_TIME_FROM_SECS,
  CB_INTR_LOG,
  CB_INTR_LOG10,
  CB_INTR_LOWER_CASE,
  CB_INTR_LOWEST_ALGEBRAIC,
  CB_INTR_MAX,
  CB_INTR_MEAN,
  CB_INTR_MEDIAN,
  CB_INTR_MIDRANGE,
  CB_INTR_MIN,
  CB_INTR_MOD,
  CB_INTR_NATIONAL,
  CB_INTR_NATIONAL_OF,
  CB_INTR_NUMVAL,
  CB_INTR_NUMVAL_C,
  CB_INTR_NUMVAL_F,
  CB_INTR_ORD,
  CB_INTR_ORD_MAX,
  CB_INTR_ORD_MIN,
  CB_INTR_PI,
  CB_INTR_PRESENT_VALUE,
  CB_INTR_RANDOM,
  CB_INTR_RANGE,
  CB_INTR_REM,
  CB_INTR_REVERSE,
  CB_INTR_SECONDS_FROM_FORMATTED_TIME,
  CB_INTR_SECONDS_PAST_MIDNIGHT,
  CB_INTR_SIGN,
  CB_INTR_SIN,
  CB_INTR_SQRT,
  CB_INTR_STANDARD_COMPARE,
  CB_INTR_STANDARD_DEVIATION,
  CB_INTR_STORED_CHAR_LENGTH,
  CB_INTR_SUBSTITUTE,
  CB_INTR_SUBSTITUTE_CASE,
  CB_INTR_SUM,
  CB_INTR_TAN,
  CB_INTR_TEST_DATE_YYYYMMDD,
  CB_INTR_TEST_DAY_YYYYDDD,
  CB_INTR_TEST_NUMVAL,
  CB_INTR_TEST_NUMVAL_C,
  CB_INTR_TEST_NUMVAL_F,
  CB_INTR_TRIM,
  CB_INTR_UPPER_CASE,
  CB_INTR_VARIANCE,
  CB_INTR_WHEN_COMPILED,
  CB_INTR_YEAR_TO_YYYY
};

struct cb_intrinsic_table {
  const char *name;                  /* FUNCTION NAME */
  const int args;                    /* 0-n, negative = variable */
  const int implemented;             /* Have we implemented it? */
  const enum cb_intr_enum intr_enum; /* Enum intrinsic */
  const char *intr_routine;          /* Routine name */
  const enum cb_category category;   /* Category */
  const unsigned int refmod;         /* Can be refmodded */
};

struct cb_intrinsic {
  struct cb_tree_common common;
  cb_tree name;
  cb_tree args;
  cb_tree intr_field; /* Field to use */
  struct cb_intrinsic_table *intr_tab;
  cb_tree offset;
  cb_tree length;
};

#define CB_INTRINSIC(x) (CB_TREE_CAST(CB_TAG_INTRINSIC, struct cb_intrinsic, x))
#define CB_INTRINSIC_P(x) (CB_TREE_TAG(x) == CB_TAG_INTRINSIC)

extern struct cb_intrinsic_table *lookup_intrinsic(const char *name,
                                                   const int checkres);
extern cb_tree cb_build_intrinsic(cb_tree name, cb_tree args, cb_tree refmod);
extern cb_tree cb_build_any_intrinsic(cb_tree args);

/*
 * INITIALIZE
 */

struct cb_initialize {
  struct cb_tree_common common;
  cb_tree var;
  cb_tree val;
  cb_tree rep;
  cb_tree def;
  int flag_statement;
};

#define CB_INITIALIZE(x)                                                       \
  (CB_TREE_CAST(CB_TAG_INITIALIZE, struct cb_initialize, x))
#define CB_INITIALIZE_P(x) (CB_TREE_TAG(x) == CB_TAG_INITIALIZE)

extern cb_tree cb_build_initialize(cb_tree var, cb_tree val, cb_tree rep,
                                   cb_tree def, int flag);

/*
 * SEARCH
 */

struct cb_search {
  struct cb_tree_common common;
  int flag_all;
  cb_tree table;
  cb_tree var;
  cb_tree end_stmt;
  cb_tree whens;
};

#define CB_SEARCH(x) (CB_TREE_CAST(CB_TAG_SEARCH, struct cb_search, x))
#define CB_SEARCH_P(x) (CB_TREE_TAG(x) == CB_TAG_SEARCH)

extern cb_tree cb_build_search(int flag_all, cb_tree table, cb_tree var,
                               cb_tree end_stmt, cb_tree whens);

/*
 * CALL
 */

#define CB_CALL_BY_REFERENCE 1
#define CB_CALL_BY_CONTENT 2
#define CB_CALL_BY_VALUE 3

struct cb_call {
  struct cb_tree_common common;
  cb_tree name;
  cb_tree args;
  cb_tree stmt1;
  cb_tree stmt2;
  cb_tree returning;
  int is_system;
};

#define CB_CALL(x) (CB_TREE_CAST(CB_TAG_CALL, struct cb_call, x))
#define CB_CALL_P(x) (CB_TREE_TAG(x) == CB_TAG_CALL)

extern cb_tree cb_build_call(cb_tree name, cb_tree args, cb_tree stmt1,
                             cb_tree stmt2, cb_tree returning,
                             int is_system_call);

/*
 * GO TO statement
 */

struct cb_goto {
  struct cb_tree_common common;
  cb_tree target;
  cb_tree depending;
};

#define CB_GOTO(x) (CB_TREE_CAST(CB_TAG_GOTO, struct cb_goto, x))
#define CB_GOTO_P(x) (CB_TREE_TAG(x) == CB_TAG_GOTO)

extern cb_tree cb_build_goto(cb_tree target, cb_tree depending);
extern cb_tree cb_build_java_continue(cb_tree target, cb_tree depending);
extern cb_tree cb_build_java_break(cb_tree target, cb_tree depending);

/*
 * IF
 */

struct cb_if {
  struct cb_tree_common common;
  cb_tree test;
  cb_tree stmt1;
  cb_tree stmt2;
};

#define CB_IF(x) (CB_TREE_CAST(CB_TAG_IF, struct cb_if, x))
#define CB_IF_P(x) (CB_TREE_TAG(x) == CB_TAG_IF)

extern cb_tree cb_build_if(cb_tree test, cb_tree stmt1, cb_tree stmt2);

/*
 * PERFORM
 */

enum cb_perform_type {
  CB_PERFORM_EXIT,
  CB_PERFORM_ONCE,
  CB_PERFORM_TIMES,
  CB_PERFORM_UNTIL,
  CB_PERFORM_FOREVER
};

struct cb_perform_varying {
  struct cb_tree_common common;
  cb_tree name;
  cb_tree from;
  cb_tree step;
  cb_tree until;
};

struct cb_perform {
  struct cb_tree_common common;
  enum cb_perform_type type;
  cb_tree test;
  cb_tree body;
  cb_tree data;
  cb_tree varying;
  cb_tree exit_label;
  cb_tree cycle_label;
};

#define CB_PERFORM_VARYING(x)                                                  \
  (CB_TREE_CAST(CB_TAG_PERFORM_VARYING, struct cb_perform_varying, x))

#define CB_PERFORM(x) (CB_TREE_CAST(CB_TAG_PERFORM, struct cb_perform, x))
#define CB_PERFORM_P(x) (CB_TREE_TAG(x) == CB_TAG_PERFORM)

extern cb_tree cb_build_perform(int type);
extern cb_tree cb_build_perform_varying(cb_tree name, cb_tree from,
                                        cb_tree step, cb_tree until);

/*
 * switch (evaluate)
 */

struct cb_switch {
  struct cb_tree_common common;
  cb_tree test;
  cb_tree case_list;
};

#define CB_SWITCH(x) (CB_TREE_CAST(CB_TAG_SWITCH, struct cb_switch, x))
#define CB_SWITCH_P(x) (CB_TREE_TAG(x) == CB_TAG_SWITCH)

extern cb_tree cb_build_switch(cb_tree test, cb_tree case_tree);

extern int cb_literal_to_int_for_switch_label(struct cb_literal *lit,
                                              int *result);

/*
 * SORT
 */

struct cb_sort_init {
  struct cb_tree_common common;
  const char *name;
  cb_tree sort_file;
  cb_tree nkeys;
  cb_tree col;
  cb_tree sort_return;
  cb_tree file_status;
};

#define CB_SORT_INIT(x) (CB_TREE_CAST(CB_TAG_SORT_INIT, struct cb_sort_init, x))
#define CB_SORT_INIT_P(x) (CB_TREE_TAG(x) == CB_TAG_SORT_INIT)

extern cb_tree cb_build_sort_init(const char *name, cb_tree sort_file,
                                  cb_tree nkeys, cb_tree col,
                                  cb_tree sort_return, cb_tree file_status);

struct cb_sort_proc {
  struct cb_tree_common common;
  cb_tree body;
  cb_tree sort_file;
  cb_tree sort_return;
};

#define CB_SORT_PROC(x) (CB_TREE_CAST(CB_TAG_SORT_PROC, struct cb_sort_proc, x))
#define CB_SORT_PROC_P(x) (CB_TREE_TAG(x) == CB_TAG_SORT_PROC)

extern cb_tree cb_build_sort_proc(cb_tree body, cb_tree sort_file,
                                  cb_tree sort_return);

struct cb_return {
  struct cb_sort_proc proc;
};

#define CB_RETURN(x) (CB_TREE_CAST(CB_TAG_RETURN, struct cb_return, x))
#define CB_RETURN_P(x) (CB_TREE_TAG(x) == CB_TAG_RETURN)

extern cb_tree cb_build_return(cb_tree sort_file, cb_tree sort_return);

struct cb_release {
  struct cb_sort_proc proc;
};

#define CB_RELEASE(x) (CB_TREE_CAST(CB_TAG_RELEASE, struct cb_release, x))
#define CB_RELEASE_P(x) (CB_TREE_TAG(x) == CB_TAG_RELEASE)

extern cb_tree cb_build_release(cb_tree sort_file, cb_tree sort_return);

struct cb_sort_finish {
  struct cb_tree_common common;
  cb_tree sort_file;
  cb_tree sort_return;
};

#define CB_SORT_FINISH(x)                                                      \
  (CB_TREE_CAST(CB_TAG_SORT_FINISH, struct cb_sort_finish, x))
#define CB_SORT_FINISH_P(x) (CB_TREE_TAG(x) == CB_TAG_SORT_FINISH)

extern cb_tree cb_build_sort_finish(cb_tree sort_file, cb_tree sort_return);

/*
 * Statement
 */

struct cb_statement {
  struct cb_tree_common common;
  const char *name;
  cb_tree body;
  cb_tree file;
  cb_tree handler1;
  cb_tree handler2;
  cb_tree handler3;
  cb_tree null_check;
  int handler_id;
  int need_terminator;
};

#define CB_STATEMENT(x) (CB_TREE_CAST(CB_TAG_STATEMENT, struct cb_statement, x))
#define CB_STATEMENT_P(x) (CB_TREE_TAG(x) == CB_TAG_STATEMENT)

extern struct cb_statement *cb_build_statement(const char *name);

/*
 * CONTINUE
 */

struct cb_continue {
  struct cb_tree_common common;
};

#define CB_CONTINUE(x) (CB_TREE_CAST(CB_TAG_CONTINUE, struct cb_continue, x))
#define CB_CONTINUE_P(x) (CB_TREE_TAG(x) == CB_TAG_CONTINUE)

extern cb_tree cb_build_continue(void);

/*
 * List
 */

struct cb_list {
  struct cb_tree_common common;
  cb_tree purpose;
  cb_tree value;
  cb_tree chain;
  int sizes;
};

#define CB_LIST(x) (CB_TREE_CAST(CB_TAG_LIST, struct cb_list, x))
#define CB_LIST_P(x) (CB_TREE_TAG(x) == CB_TAG_LIST)

#define CB_PURPOSE(x) (CB_LIST(x)->purpose)
#define CB_VALUE(x) (CB_LIST(x)->value)
#define CB_CHAIN(x) (CB_LIST(x)->chain)
#define CB_SIZES(x) (CB_LIST(x)->sizes)

#define CB_PURPOSE_INT(x) (CB_INTEGER(CB_PURPOSE(x))->val)

#define CB_SIZE_AUTO 0
#define CB_SIZE_1 1
#define CB_SIZE_2 2
#define CB_SIZE_4 3
#define CB_SIZE_8 4
#define CB_SIZE_UNSIGNED 8

#define CB_SIZES_INT(x) ((CB_LIST(x)->sizes) & 0x07)
#define CB_SIZES_INT_UNSIGNED(x) ((CB_LIST(x)->sizes) & CB_SIZE_UNSIGNED)

extern cb_tree cb_build_list(cb_tree purpose, cb_tree value, cb_tree rest);
extern cb_tree cb_list_add(cb_tree l, cb_tree x);
extern cb_tree cb_list_append(cb_tree l1, cb_tree l2);
extern cb_tree cb_list_reverse(cb_tree l);
extern int cb_list_length(cb_tree l);

#define cb_list_init(x) cb_build_list(NULL, x, NULL)
#define cb_cons(x, l) cb_build_list(NULL, x, l)

/* Pair */

#define CB_PAIR_P(x) (CB_LIST_P(x) && CB_PAIR_X(x))
#define CB_PAIR_X(x) CB_PURPOSE(x)
#define CB_PAIR_Y(x) CB_VALUE(x)

#define cb_build_pair(x, y) cb_build_list(x, y, NULL)

/*
 * Program
 */

struct cb_program {
  /* program variables */
  struct cb_program *next_program; /* Nested */
  const char *program_id;          /* Demangled PROGRAM-ID */
  char *source_name;               /* Source name */
  char *orig_source_name;
  FILE *local_storage_file;
  char *local_storage_name;
  cb_tree entry_list;
  cb_tree file_list;
  cb_tree exec_list;
  cb_tree label_list;
  cb_tree reference_list;
  cb_tree alphabet_name_list;
  cb_tree class_name_list;
  cb_tree parameter_list;
  cb_tree locale_list;
  cb_tree symbolic_list;
  cb_tree global_list;
  cb_tree cb_return_code;
  cb_tree cb_sort_return;
  cb_tree cb_call_params;
  cb_tree class_spec_list;
  cb_tree interface_spec_list;
  cb_tree function_spec_list;
  cb_tree program_spec_list;
  cb_tree property_spec_list;
  struct cb_field *working_storage;
  struct cb_field *local_storage;
  struct cb_field *linkage_storage;
  struct cb_field *screen_storage;
  cb_tree local_file_list;
  cb_tree global_file_list;
  struct handler_struct global_handler[5];
  cb_tree collating_sequence;
  cb_tree cursor_pos;
  cb_tree crt_status;
  cb_tree returning; /* RETURNING */
  struct cb_word *word_table[CB_WORD_HASH_SIZE];
  /* internal variables */
  int loop_counter;                /* Loop counters */
  int decimal_index;               /* cob_decimal count */
  int decimal_index_max;           /* cob_decimal max */
  unsigned char decimal_point;     /* '.' or ',' */
  unsigned char currency_symbol;   /* '$' or user-specified */
  unsigned char numeric_separator; /* ',' or '.' */
  unsigned char nested_level;      /* Nested program level */

  unsigned char flag_main;             /* Gen main function */
  unsigned char flag_common;           /* COMMON PROGRAM */
  unsigned char flag_initial;          /* INITIAL PROGRAM */
  unsigned char flag_recursive;        /* RECURSIVE PROGRAM */
  unsigned char flag_screen;           /* have SCREEN SECTION */
  unsigned char flag_validated;        /* End program validate */
  unsigned char flag_chained;          /* PROCEDURE CHAINING */
  unsigned char flag_global_use;       /* USE GLOBAL */
  unsigned char flag_sort_status_used; /* SORT-STAUS is used in program. */

  unsigned char gen_decset;     /* Gen decimal_set_int */
  unsigned char gen_udecset;    /* Gen decimal_set_uint */
  unsigned char gen_ptrmanip;   /* Gen cob_pointer_manip */
  unsigned char gen_file_error; /* Gen error routine */
  unsigned char prog_type;      /* Program type */
  unsigned char spare[3];       /* Spare */
};

extern struct cb_program *cb_build_program(struct cb_program *last_program,
                                           const int nest_level);

/* parser.y */
extern int non_const_word;

/* reserved.c */
extern cb_tree lookup_system_name(const char *name);
extern int lookup_reserved_word(const char *name);
extern void cb_list_reserved(void);
extern void cb_list_intrinsics(void);
extern void cb_list_mnemonics(void);
extern void cb_init_reserved(void);
extern void cb_list_map(cb_tree (*func)(cb_tree x), cb_tree l);

/* error.c */
extern void cb_warning_x(cb_tree x, const char *fmt, ...) COB_A_FORMAT23;
extern void cb_error_x(cb_tree x, const char *fmt, ...) COB_A_FORMAT23;

extern char *check_filler_name(char *name);
extern void redefinition_error(cb_tree x);
extern void redefinition_warning(cb_tree x, cb_tree y);
extern void undefined_error(cb_tree x);
extern void ambiguous_error(cb_tree x);
extern void group_error(cb_tree x, const char *clause);
extern void level_redundant_error(cb_tree x, const char *clause);
extern void level_require_error(cb_tree x, const char *clause);
extern void level_except_error(cb_tree x, const char *clause);

struct cb_literal *build_literal(enum cb_category category,
                                 const unsigned char *data, size_t size);

/* field.c */
extern size_t cb_needs_01;
extern int cb_get_level(cb_tree x);
extern cb_tree cb_build_field_tree(cb_tree level, cb_tree name,
                                   struct cb_field *last_field,
                                   enum cb_storage storage, struct cb_file *fn,
                                   char *definition_source_file,
                                   int definition_source_line,
                                   int prev_field_line_number);
extern struct cb_field *cb_resolve_redefines(struct cb_field *field,
                                             cb_tree redefines);
extern void cb_validate_field(struct cb_field *p);
extern void cb_validate_88_item(struct cb_field *p);
extern struct cb_field *cb_validate_78_item(struct cb_field *p);
extern void cb_clear_real_field(void);

/* typeck.c */
extern cb_tree cb_check_numeric_value(cb_tree x);

extern void cb_build_registers(void);
extern char *cb_encode_program_id(const char *name);
extern const char *cb_build_program_id(cb_tree name, cb_tree alt_name);
extern void cb_define_switch_name(cb_tree name, cb_tree sname, cb_tree flag,
                                  cb_tree ref);
extern cb_tree cb_build_section_name(cb_tree name, int sect_or_para);
extern cb_tree cb_build_assignment_name(struct cb_file *curfile, cb_tree name);
extern cb_tree cb_build_index(cb_tree name, cb_tree values, int indexed_by,
                              struct cb_field *qual);
extern int cb_reference_type_check(cb_tree ref, cb_tree x, const char *name,
                                   int size, int *retsize, int type);
extern cb_tree cb_build_identifier(cb_tree x);
extern cb_tree cb_build_length(cb_tree x);
extern cb_tree cb_build_lengths(cb_tree x);
extern cb_tree cb_build_const_length(cb_tree x);
extern cb_tree cb_build_address(cb_tree x);
extern cb_tree cb_build_ppointer(cb_tree x);

extern void cb_validate_program_environment(struct cb_program *prog);
extern void cb_validate_program_data(struct cb_program *prog);
extern void cb_validate_program_body(struct cb_program *prog);
extern void cb_validate_indexed_file_key(const struct cb_file *f);

extern cb_tree cb_build_expr(cb_tree list);
extern cb_tree cb_build_cond(cb_tree x);

extern void cb_emit_arithmetic(cb_tree vars, int op, cb_tree val);
extern cb_tree cb_build_add(cb_tree v, cb_tree n, cb_tree round_opt);
extern cb_tree cb_build_sub(cb_tree v, cb_tree n, cb_tree round_opt);
extern void cb_emit_corresponding(cb_tree (*func)(cb_tree f1, cb_tree f2,
                                                  cb_tree f3),
                                  cb_tree x1, cb_tree x2, cb_tree opt);
extern void cb_emit_move_corresponding(cb_tree x1, cb_tree x2);

extern void cb_emit_accept(cb_tree var, cb_tree pos, cb_tree fgc, cb_tree bgc,
                           cb_tree scroll, int dispattrs);
extern void cb_emit_accept_line_or_col(cb_tree var, const int l_or_c);
extern void cb_emit_accept_date(cb_tree var);
extern void cb_emit_accept_date_yyyymmdd(cb_tree var);
extern void cb_emit_accept_day(cb_tree var);
extern void cb_emit_accept_day_yyyyddd(cb_tree var);
extern void cb_emit_accept_day_of_week(cb_tree var);
extern void cb_emit_accept_time(cb_tree var);
extern void cb_emit_accept_command_line(cb_tree var);
extern void cb_emit_get_environment(cb_tree envvar, cb_tree envval);
extern void cb_emit_accept_environment(cb_tree var);
extern void cb_emit_accept_mnemonic(cb_tree var, cb_tree mnemonic);
extern void cb_emit_accept_name(cb_tree var, cb_tree name);
extern void cb_emit_accept_arg_number(cb_tree var);
extern void cb_emit_accept_arg_value(cb_tree var);

extern void cb_emit_allocate(cb_tree target1, cb_tree target2, cb_tree size,
                             cb_tree initialize);
extern void cb_emit_free(cb_tree vars);

extern void cb_emit_call(cb_tree prog, cb_tree cb_using, cb_tree returning,
                         cb_tree on_exception, cb_tree not_on_exception);

extern void cb_emit_cancel(cb_tree prog);
extern void cb_emit_cancel_all(void);

extern void cb_emit_close(cb_tree file, cb_tree opt);

extern void cb_emit_commit(void);

extern void cb_emit_continue(void);

extern void cb_emit_delete(cb_tree file);

extern void cb_emit_delete_file(cb_tree file);

extern void cb_emit_display(cb_tree values, cb_tree upon, cb_tree no_adv,
                            cb_tree pos, cb_tree fgc, cb_tree bgc,
                            cb_tree scroll, int dispattrs);

extern void cb_emit_display_mnemonic(cb_tree values, cb_tree mnemonic,
                                     cb_tree no_adv, cb_tree pos, cb_tree fgc,
                                     cb_tree bgc, cb_tree scroll,
                                     int dispattrs);

extern cb_tree cb_build_display_upon(cb_tree x);
extern cb_tree cb_build_display_upon_direct(cb_tree x);
extern void cb_emit_env_name(cb_tree value);
extern void cb_emit_env_value(cb_tree value);
extern void cb_emit_arg_number(cb_tree value);
extern void cb_emit_command_line(cb_tree value);

extern void cb_emit_divide(cb_tree dividend, cb_tree divisor, cb_tree quotient,
                           cb_tree remainder);

extern void cb_emit_evaluate(cb_tree subject_list, cb_tree case_list);

extern void cb_emit_goto(cb_tree target, cb_tree depending);
extern void cb_emit_java_continue(void);
extern void cb_emit_java_break(void);
extern void cb_emit_exit(size_t goback);

extern void cb_emit_if(cb_tree cond, cb_tree stmt1, cb_tree stmt2);

extern void cb_emit_initialize(cb_tree vars, cb_tree fillinit, cb_tree value,
                               cb_tree replacing, cb_tree def);

extern void cb_emit_inspect(cb_tree var, cb_tree body, cb_tree replacing,
                            int replconv);
extern void cb_init_tarrying(void);
extern cb_tree cb_build_tarrying_data(cb_tree x);
extern cb_tree cb_build_tarrying_characters(cb_tree l);
extern cb_tree cb_build_tarrying_all(void);
extern cb_tree cb_build_tarrying_leading(void);
extern cb_tree cb_build_tarrying_trailing(void);
extern cb_tree cb_build_tarrying_value(cb_tree x, cb_tree l);
extern int cb_validate_inspect(cb_tree var, cb_tree x, cb_tree y);
extern cb_tree cb_build_replacing_characters(cb_tree x, cb_tree l, cb_tree var);
extern cb_tree cb_build_replacing_all(cb_tree x, cb_tree y, cb_tree l,
                                      cb_tree var);
extern cb_tree cb_build_replacing_leading(cb_tree x, cb_tree y, cb_tree l);
extern cb_tree cb_build_replacing_first(cb_tree x, cb_tree y, cb_tree l);
extern cb_tree cb_build_replacing_trailing(cb_tree x, cb_tree y, cb_tree l);
extern cb_tree cb_build_converting(cb_tree x, cb_tree y, cb_tree l);
extern cb_tree cb_build_inspect_region_start(void);
extern cb_tree cb_build_inspect_region(cb_tree l, cb_tree pos, cb_tree x);

extern int validate_move(cb_tree src, cb_tree dst, size_t is_value);
extern cb_tree cb_build_move(cb_tree src, cb_tree dst);
extern void cb_emit_move(cb_tree src, cb_tree dsts);

extern void cb_emit_open(cb_tree file, cb_tree mode, cb_tree sharing);

extern void cb_emit_perform(cb_tree perform, cb_tree body);
extern cb_tree cb_build_perform_once(cb_tree body);
extern cb_tree cb_build_perform_times(cb_tree count);
extern cb_tree cb_build_perform_until(cb_tree condition, cb_tree varying);
extern cb_tree cb_build_perform_forever(cb_tree body);
extern cb_tree cb_build_perform_exit(struct cb_label *label);

extern void cb_emit_read(cb_tree ref, cb_tree next, cb_tree into, cb_tree key,
                         cb_tree lock_opts);

extern void cb_emit_rewrite(cb_tree record, cb_tree from, cb_tree lockopt);

extern void cb_emit_release(cb_tree ref, cb_tree from);
extern void cb_emit_return(cb_tree ref, cb_tree into);

extern void cb_emit_rollback(void);

extern void cb_emit_search(cb_tree table, cb_tree varying, cb_tree at_end,
                           cb_tree whens);
extern void cb_emit_search_all(cb_tree table, cb_tree at_end, cb_tree when,
                               cb_tree stmts);

extern void cb_emit_setenv(cb_tree x, cb_tree y);
extern void cb_emit_set_to(cb_tree l, cb_tree x);
extern void cb_emit_set_up_down(cb_tree l, cb_tree flag, cb_tree x);
extern void cb_emit_set_on_off(cb_tree l, cb_tree flag);
extern void cb_emit_set_true(cb_tree l);
extern void cb_emit_set_false(cb_tree l);

extern void cb_emit_sort_init(cb_tree name, cb_tree keys, cb_tree col);
extern void cb_emit_sort_using(cb_tree file, cb_tree l);
extern void cb_emit_sort_input(cb_tree proc, cb_tree file);
extern void cb_emit_sort_giving(cb_tree file, cb_tree l);
extern void cb_emit_sort_output(cb_tree proc, cb_tree file);
extern void cb_emit_sort_finish(cb_tree file);

extern void cb_emit_start(cb_tree file, cb_tree op, cb_tree key);

extern void cb_emit_stop_run(cb_tree x);

extern void cb_emit_string(cb_tree items, cb_tree into, cb_tree pointer);

extern void cb_emit_unlock(cb_tree ref);

extern void cb_emit_unstring(cb_tree name, cb_tree delimited, cb_tree into,
                             cb_tree pointer, cb_tree tallying);
extern cb_tree cb_build_unstring_delimited(cb_tree all, cb_tree value);
extern cb_tree cb_build_unstring_into(cb_tree name, cb_tree delimiter,
                                      cb_tree count);

extern void cb_emit_write(cb_tree record, cb_tree from, cb_tree opt,
                          cb_tree lockopt);
extern cb_tree cb_build_write_advancing_lines(cb_tree pos, cb_tree lines);
extern cb_tree cb_build_write_advancing_mnemonic(cb_tree pos, cb_tree mnemonic);
extern cb_tree cb_build_write_advancing_page(cb_tree pos);

extern void cobc_tree_cast_error(cb_tree x, const char *filen,
                                 const int linenum, const int tagnum);

/* codegen.c */
extern void codegen(struct cb_program *prog, const int nested,
                    char **program_id_list, char *java_source_dir,
                    char *source_file);

/* scanner.l */
extern void cb_set_in_procedure(void);
extern void cb_reset_in_procedure(void);
extern void cb_add_78(struct cb_field *f);
extern void cb_reset_78(void);
extern struct cb_field *check_level_78(const char *name);
extern void cb_unget_token(int tok, YYSTYPE lval);

/* typeck.c */
extern void *make_tree(int tag, enum cb_category category, size_t size);

#endif /* CB_TREE_H */
