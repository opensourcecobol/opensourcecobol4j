/*
 * Copyright (C) 2002-2009 Keisuke Nishida
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

#include "config.h"

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "cobj.h"
#include "tree.h"

#ifdef HAVE_ATTRIBUTE_ALIGNED
#define COB_ALIGN " __attribute__((aligned))"
#else
#define COB_ALIGN ""
#endif

#define COB_USE_SETJMP 0
#define COB_MAX_SUBSCRIPTS 16

#define INITIALIZE_NONE 0
#define INITIALIZE_ONE 1
#define INITIALIZE_DEFAULT 2
#define INITIALIZE_COMPOUND 3
#define EXECUTION_NORMAL 0
#define EXECUTION_LAST 1
#define EXECUTION_ERROR_HANDLER 2
#define BUF_SIZE 1024
#define MAX_LITERAL_SIZE 64

#ifndef __GNUC__
static int inside_check = 0;
static int inside_stack[64];
#endif
static int param_id = 0;
static int stack_id = 0;
static int num_cob_fields = 0;
static int loop_counter = 0;
static int progid = 0;
static int last_line = 0;
static int needs_exit_prog = 0;
static int need_double = 0;
static int gen_ebcdic = 0;
static int gen_ebcdic_ascii = 0;
static int gen_full_ebcdic = 0;
static int gen_native = 0;
static int gen_custom = 0;
static int field_iteration = 0;
static int screenptr = 0;

static int i_counters[COB_MAX_SUBSCRIPTS];

// static int			output_indent_level = 0;
static int joutput_indent_level = 0;
static FILE *joutput_target;
static const char *excp_current_program_id = NULL;
static const char *excp_current_section = NULL;
static const char *excp_current_paragraph = NULL;
static struct cb_program *current_prog;

extern int cb_default_byte_specified;
extern unsigned char cb_default_byte;

// OCCURS指定したときのIDNEXに対応するCobolDataStorage型変数を扱うときに必要なフラグ
static int index_read_flag = 0;

static struct label_list {
  struct label_list *next;
  int id;
  int call_num;
} *label_cache = NULL;

static struct attr_list {
  struct attr_list *next;
  unsigned char *pic;
  int id;
  int type;
  int digits;
  int scale;
  int flags;
  int lenstr;
} *attr_cache = NULL;

static struct literal_list {
  struct literal_list *next;
  struct cb_literal *literal;
  cb_tree x;
  int id;
} *literal_cache = NULL;

static struct field_list {
  struct field_list *next;
  struct cb_field *f;
  cb_tree x;
  const char *curr_prog;
  int nulldata;
} *field_cache = NULL;

static struct call_list {
  struct call_list *next;
  const char *callname;
} *call_cache = NULL;

static struct base_list {
  struct base_list *next;
  struct cb_field *f;
  const char *curr_prog;
} *base_cache = NULL;

static struct local_list {
  struct local_list *next;
  struct cb_field *f;
} *local_cache = NULL;

struct sort_list {
  struct sort_list *next;
};

struct system_table {
  const char *syst_name;
  const char *syst_call;
};

enum joutput_stmt_type {
  JOUTPUT_STMT_DEFAULT,
  JOUTPUT_STMT_TRIM,
};

static cb_tree call_parameters = NULL;

static const struct system_table system_tab[] = {
#undef COB_SYSTEM_GEN
#define COB_SYSTEM_GEN(x, y, z) {x, #z},
#ifdef _WIN32
#include "system.def"
#else
#include <system.def>
#endif
    {NULL, NULL}};

/* Globals */
int has_external = 0;

/* Compile error countermeasure */

/* End Compile error countermeasure */

static void joutput_index(cb_tree x);
static void joutput_integer(cb_tree x);
static void joutput_param(cb_tree x, int id);
static void joutput_func_1(const char *name, cb_tree x);
static void joutput_stmt(cb_tree x, enum joutput_stmt_type output_type);
static void joutput_figurative(cb_tree x, struct cb_field *f, const int value);
static void joutput_alphabet_name_initialization(struct cb_alphabet_name *p);
const int L_initextern_addr = 2000000000;
int param_wrap_string_flag = 0;

static char *get_java_identifier_field(struct cb_field *f);
static char *get_java_identifier_base(struct cb_field *f);
static void get_java_identifier_helper(struct cb_field *f, char *buf);
static void strcpy_identifier_cobol_to_java(char *buf, const char *identifier);
static void joutput_execution_list(struct cb_program *prog);
static void joutput_execution_entry_func(void);
static void joutput_init_method(struct cb_program *prog);
static void joutput_declare_member_variables(struct cb_program *prog,
                                             cb_tree parameter_list);

static char *convert_byte_value_format(char value);
static void append_label_id_map(struct cb_label *label);
static void create_label_id_map(struct cb_program *prog);
static void destroy_label_id_map(void);
static void joutput_edit_code_command(const char *target);

static void joutput_label_variable(struct cb_label *label);
static void joutput_label_variable_name(char *s, int key,
                                        struct cb_label *section);
static void joutput_label_variable_by_value(int value);

static char *get_java_identifier_field(struct cb_field *f) {
  char *buf = malloc(COB_SMALL_BUFF);
  if (cb_flag_serial_variable) {
    sprintf(buf, "%s%d", CB_PREFIX_FIELD, f->id);
  } else if (cb_flag_short_variable) {
    strcpy(buf, CB_PREFIX_FIELD);
    strcpy_identifier_cobol_to_java(buf + strlen(CB_PREFIX_FIELD), f->name);
  } else {
    strcpy(buf, CB_PREFIX_FIELD);
    get_java_identifier_helper(f, buf + strlen(CB_PREFIX_FIELD));
  }
  return buf;
}

static char *get_java_identifier_base(struct cb_field *f) {
  char *buf = malloc(COB_SMALL_BUFF);
  if (cb_flag_serial_variable) {
    sprintf(buf, "%s%d", CB_PREFIX_BASE, f->id);
  } else if (cb_flag_short_variable) {
    strcpy(buf, CB_PREFIX_BASE);
    strcpy_identifier_cobol_to_java(buf + strlen(CB_PREFIX_BASE), f->name);
  } else {
    strcpy(buf, CB_PREFIX_BASE);
    get_java_identifier_helper(f, buf + strlen(CB_PREFIX_BASE));
  }
  return buf;
}

static void get_java_identifier_helper(struct cb_field *f, char *buf) {
  strcpy_identifier_cobol_to_java(buf, f->name);
  buf += strlen(f->name);
  if (f->parent != NULL) {
    static const char *delimitor = "__";
    strcpy(buf, delimitor);
    buf += strlen(delimitor);
    get_java_identifier_helper(f->parent, buf);
  }
}

static void strcpy_identifier_cobol_to_java(char *buf, const char *identifier) {
  int all_ascii = 1;
  unsigned char *p = (unsigned char *)identifier;
  for (; *p; ++p) {
    unsigned char c = *p;
    if (c < 0x0A || 0x80 <= c) {
      all_ascii = 0;
      break;
    }
  }

  if (all_ascii) {
    for (; *identifier; ++identifier, ++buf) {
      if (*identifier == '-') {
        *buf = '_';
      } else {
        *buf = *identifier;
      }
    }
  } else {
    for (; *identifier; ++identifier) {
      sprintf(buf, "%02x", (unsigned char)*identifier);
      buf += 2;
    }
  }
  *buf = '\0';
}

struct cb_label_id_map {
  int key;
  int val;
  char *label_name;
  struct cb_label *section;
  struct cb_label_id_map *next;
};
struct cb_label_id_map *label_id_map_head = NULL;
struct cb_label_id_map *label_id_map_last = NULL;
int label_id_counter = 0;
int control_counter = 0;
int flag_execution_begin = EXECUTION_NORMAL;
int flag_execution_end = EXECUTION_NORMAL;

static char *convert_byte_value_format(char value) {
  char *s;
  if (value == '\'') {
    s = malloc(5);
    strcpy(s, "'\\''");
  } else if (isprint(value)) {
    s = malloc(4);
    sprintf(s, "'%c'", value);
  } else {
    s = malloc(5);
    sprintf(s, "%d", value);
  }
  return s;
}

static struct comment_info *comment_info_cursor = NULL;
static struct comment_info *working_storage_comment_info_cursor = NULL;
char *translating_source_file = NULL;

static void lookup_call(const char *p) {
  struct call_list *clp;

  for (clp = call_cache; clp; clp = clp->next) {
    if (strcmp(p, clp->callname) == 0) {
      return;
    }
  }
  clp = cobc_malloc(sizeof(struct call_list));
  clp->callname = p;
  clp->next = call_cache;
  call_cache = clp;
}

static struct attr_list *attr_list_reverse(struct attr_list *p) {
  struct attr_list *next;
  struct attr_list *last = NULL;

  for (; p; p = next) {
    next = p->next;
    p->next = last;
    last = p;
  }
  return last;
}

static struct literal_list *literal_list_reverse(struct literal_list *p) {
  struct literal_list *next;
  struct literal_list *last = NULL;

  for (; p; p = next) {
    next = p->next;
    p->next = last;
    last = p;
  }
  return last;
}

/*
 * Output routines
 */
typedef struct tag_joutput_buffer {
  char *buffer;
  int size;
  int index;
  int label;
  int default_flag;
} joutput_buffer;

static int joutput_buffered = 0;
const int JOUTPUT_BUFFER_SIZE_UNIT = 1024 * 1024;
static joutput_buffer joutput_buffer_list[65536];
static int joutput_buffer_list_index = 0;
static char joutput_temp_buffer[65536];

static void joutput(const char *fmt, ...) {

  if (joutput_target) {
    va_list ap;
    if (joutput_buffered) {
      va_start(ap, fmt);
      int size = vsprintf(joutput_temp_buffer, fmt, ap);
      va_end(ap);

      joutput_buffer *buf = &joutput_buffer_list[joutput_buffer_list_index];
      if (buf->index + size + 1 >= buf->size) {
        buf->size += JOUTPUT_BUFFER_SIZE_UNIT;
        buf->buffer = realloc(buf->buffer, buf->size);
      }

      strcpy(buf->buffer + buf->index, joutput_temp_buffer);
      buf->index += size;

    } else {
      va_start(ap, fmt);
      vfprintf(joutput_target, fmt, ap);
      va_end(ap);
    }
  }
}

static void joutput_newline(void) {
  if (joutput_target) {
    if (joutput_buffered) {
      joutput("\n");
    } else {
      fputs("\n", joutput_target);
    }
  }
}

static void joutput_prefix(void) {

  if (joutput_target) {
    int i;
    if (joutput_buffered) {
      for (i = 0; i < joutput_indent_level; i++) {
        joutput(" ");
      }
    } else {
      for (i = 0; i < joutput_indent_level; i++) {
        fputc(' ', joutput_target);
      }
    }
  }
}

static void joutput_line(const char *fmt, ...) {

  if (joutput_target) {
    va_list ap;
    if (joutput_buffered) {
      joutput_prefix();
      va_start(ap, fmt);
      int size = vsprintf(joutput_temp_buffer, fmt, ap);
      va_end(ap);

      joutput_buffer *buf = &joutput_buffer_list[joutput_buffer_list_index];
      if (buf->index + size + 1 >= buf->size) {
        buf->size += JOUTPUT_BUFFER_SIZE_UNIT;
        buf->buffer = realloc(buf->buffer, buf->size);
      }

      strcpy(buf->buffer + buf->index, joutput_temp_buffer);
      buf->index += size;

      joutput("\n");
    } else {
      joutput_prefix();
      va_start(ap, fmt);
      vfprintf(joutput_target, fmt, ap);
      va_end(ap);
      fputc('\n', joutput_target);
    }
  }
}

static void joutput_indent(const char *str) {
  const char *p;
  int level = 2;

  for (p = str; *p == ' '; p++) {
    level++;
  }

  if (*p == '}' && strcmp(str, "})") != 0) {
    joutput_indent_level -= level;
  }

  joutput_line(str);

  if (*p == '{' && strcmp(str, ")}") != 0) {
    joutput_indent_level += level;
  }
}

enum cb_string_category {
  CB_STRING_CATEGORY_ALL_ASCII,
  CB_STRING_CATEGORY_ALL_SJIS,
  CB_STRING_CATEGORY_CONTAINS_NON_SJIS,
};

struct string_literal_cache {
  unsigned char *string_value;
  int size;
  int param_wrap_string_flag;
  enum cb_string_category category;
  char *var_name;
  struct string_literal_cache *next;
};

int string_literal_id = 0;
struct string_literal_cache *string_literal_list = NULL;

static void init_string_literal_list() {
  string_literal_id = 0;
  string_literal_list = NULL;
}

static void free_string_literal_list() {
  string_literal_id = 0;
  struct string_literal_cache *l = string_literal_list;
  while (l != NULL) {
    struct string_literal_cache *next = l->next;
    free(l->string_value);
    free(l->var_name);
    free(l);
    l = next;
  }
}

static enum cb_string_category get_string_category(const unsigned char *s,
                                                   int size) {
  int i;
  enum cb_string_category category = CB_STRING_CATEGORY_ALL_ASCII;
  for (i = 0; i < size;) {
    int c = s[i];
    if (0x20 <= c && c <= 0x7e) {
      i += 1;
    } else if ((0x81 <= c && c <= 0x9f) || (0xe0 <= c && c <= 0xef)) {
      i += 2;
      category = CB_STRING_CATEGORY_ALL_SJIS;
    } else {
      return CB_STRING_CATEGORY_CONTAINS_NON_SJIS;
    }
  }
  return category;
}

static void joutput_string_write(const unsigned char *s, int size,
                                 enum cb_string_category category) {
  int i;

  if (category == CB_STRING_CATEGORY_ALL_ASCII) {
    if (param_wrap_string_flag) {
      joutput("new CobolDataStorage(");
    } else {
      joutput("CobolUtil.stringToBytes(");
    }

    joutput("\"");

    int output_multibyte = 0;
    for (i = 0; i < size; i++) {
      int c = s[i];
      if (!output_multibyte && (c == '\"' || c == '\\')) {
        joutput("\\%c", c);
      } else if (!output_multibyte && (c == '\n')) {
        joutput("\\n");
      } else {
        joutput("%c", c);
      }
      output_multibyte = !output_multibyte &&
                         ((0x81 <= c && c <= 0x9f) || (0xe0 <= c && c <= 0xef));
    }

    joutput("\")");
  } else {
    if (param_wrap_string_flag) {
      joutput("CobolDataStorage.makeCobolDataStorage(");
    } else {
      joutput("CobolUtil.toBytes(");
    }

    for (i = 0; i < size; i++) {
      joutput("(byte)0x%02x", s[i]);
      if (i < size - 1) {
        joutput(", ");
      }
    }

    joutput(")");
  }
}

static void joutput_string(const unsigned char *s, int size) {
  int i;
  struct string_literal_cache *new_literal_cache =
      malloc(sizeof(struct string_literal_cache));

  new_literal_cache->size = size;
  new_literal_cache->param_wrap_string_flag = param_wrap_string_flag;
  new_literal_cache->category = get_string_category(s, size);

  new_literal_cache->string_value = malloc(size);
  memcpy(new_literal_cache->string_value, s, size);

  new_literal_cache->var_name = malloc(128);
  int var_name_length = sprintf(new_literal_cache->var_name, "%s%d",
                                CB_PREFIX_STRING_LITERAL, string_literal_id++);

  // append the initial string value to the variable name if possible
  for (i = 0; i < size && i < 64; ++i) {
    char c = s[i];
    if (!(('0' <= c && c <= '9') || ('a' <= c && c <= 'z') ||
          ('A' <= c && c <= 'Z') || c == '_')) {
      break;
    }
  }

  if (i > 0) {
    new_literal_cache->var_name[var_name_length] = '_';
    memcpy(new_literal_cache->var_name + var_name_length + 1, s, i);
    new_literal_cache->var_name[var_name_length + 1 + i] = '\0';
  }

  // add the new cache to string_literal_list
  new_literal_cache->next = string_literal_list;
  string_literal_list = new_literal_cache;

  joutput("%s", new_literal_cache->var_name);
}

static void joutput_all_string_literals() {
  const char *data_type_storage = "CobolDataStorage";
  const char *data_type_bytes = "byte[]";
  const char *data_type;
  struct string_literal_cache *l = string_literal_list;
  int tmp_param_wrap_string_flag = param_wrap_string_flag;

  if (l != NULL) {
    joutput_line("/* String literals */");
  }

  while (l != NULL) {
    if (l->param_wrap_string_flag) {
      data_type = data_type_storage;
    } else {
      data_type = data_type_bytes;
    }
    joutput_prefix();
    joutput("public static final %s %s = ", data_type, l->var_name);
    param_wrap_string_flag = l->param_wrap_string_flag;
    joutput_string_write(l->string_value, l->size, l->category);
    joutput(";\n");
    l = l->next;
  }
  param_wrap_string_flag = tmp_param_wrap_string_flag;
}

static void joutput_local(const char *fmt, ...) {
  if (current_prog->local_storage_file) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(joutput_target, fmt, ap);
    va_end(ap);
  }
}

static void joutput_edit_code_command(const char *target) {
  if (!edit_code_command_is_set) {
    return;
  }

  char command[BUF_SIZE];
  char buf[BUF_SIZE];
  sprintf(command, "%s --target=%s", edit_code_command, target);

#ifdef _WIN32
  FILE *fp = _popen(command, "r");
#else
  FILE *fp = popen(command, "r");
#endif
  if (fp == NULL) {
    return;
  }
  memset(buf, 0, BUF_SIZE);

  while (fgets(buf, BUF_SIZE, fp) != NULL) {
    joutput("%s", buf);
  }
#ifdef _WIN32
  _pclose(fp);
#else
  pclose(fp);
#endif
}

/*
 * Field
 */

struct data_storage_list {
  struct cb_field *f;
  struct cb_field *top;
  struct data_storage_list *next;
};

#define DATA_STORAGE_CACHE_BUFF_SIZE (1024)
static struct data_storage_list
    *data_storage_cache[DATA_STORAGE_CACHE_BUFF_SIZE];
static unsigned int data_storage_cache_count = 0;

static void init_data_storage_list() {
  int i;
  for (i = 0; i < DATA_STORAGE_CACHE_BUFF_SIZE; ++i) {
    data_storage_cache[i] = NULL;
  }
  data_storage_cache_count = 0;
}

static void register_data_storage_list(struct cb_field *f,
                                       struct cb_field *top) {
  if (f->offset == 0 && strcmp(f->name, top->name) == 0) {
    return;
  }
  unsigned long index = (((unsigned long)f) + ((unsigned long)top)) %
                        DATA_STORAGE_CACHE_BUFF_SIZE;
  struct data_storage_list *entry = data_storage_cache[index];
  while (entry != NULL) {
    if (entry->f == f && entry->top == top) {
      return;
    }
    entry = entry->next;
  }
  struct data_storage_list *new_entry =
      malloc(sizeof(struct data_storage_list));
  new_entry->f = f;
  new_entry->top = top;
  new_entry->next = data_storage_cache[index];
  data_storage_cache[index] = new_entry;
  ++data_storage_cache_count;
}

static void free_data_storage_list() {
  int i;
  for (i = 0; i < DATA_STORAGE_CACHE_BUFF_SIZE; ++i) {
    struct data_storage_list *entry = data_storage_cache[i];
    while (entry != NULL) {
      struct data_storage_list *next = entry->next;
      free(entry);
      entry = next;
    }
  }
  data_storage_cache_count = 0;
}

struct data_storage_list **sorted_data_storage_cache = NULL;
static int comp_data_storage_cache(const struct data_storage_list **a,
                                   const struct data_storage_list **b) {
  int ret;
  if ((ret = strcmp((*a)->top->name, (*b)->top->name)) != 0) {
    return ret;
  }
  if ((ret = strcmp((*a)->f->name, (*b)->f->name)) != 0) {
    return ret;
  }
  return 0;
}

static void create_sorted_data_storage_cache() {
  sorted_data_storage_cache =
      malloc(data_storage_cache_count * sizeof(struct data_storage_list *));
  int i, j = 0;
  for (i = 0; i < DATA_STORAGE_CACHE_BUFF_SIZE; ++i) {
    struct data_storage_list *entry = data_storage_cache[i];
    while (entry != NULL) {
      sorted_data_storage_cache[j++] = entry;
      entry = entry->next;
    }
  }
  qsort(sorted_data_storage_cache, data_storage_cache_count,
        sizeof(struct data_storage_list *),
        (int (*)(const void *, const void *))comp_data_storage_cache);
}

static void destroy_sorted_data_storage_cache() {
  free(sorted_data_storage_cache);
}

static int is_call_parameter(const struct cb_field *f) {
  cb_tree l;
  for (l = call_parameters; l; l = CB_CHAIN(l)) {
    if (f == cb_field(CB_VALUE(l))) {
      return 1;
    }
  }
  return 0;
}

static int joutput_field_storage(struct cb_field *f, struct cb_field *top) {
  const char *p;
  int flag_call_parameter = is_call_parameter(top);
  if (flag_call_parameter ||
      (f->offset == 0 && strcmp(f->name, top->name) == 0)) {
    char *base_name = get_java_identifier_base(top);
    joutput(base_name);
    free(base_name);
    return flag_call_parameter;
  } else if (cb_flag_short_variable) {
    joutput("b_");
    for (p = f->name; *p != '\0'; ++p) {
      if (*p == '-') {
        joutput("_");
      } else {
        joutput("%c", *p);
      }
    }
  } else {
    joutput("b_");
    struct cb_field *field = f;
    int flag_first_iteration = 1;
    while (field) {
      if (flag_first_iteration) {
        flag_first_iteration = 0;
      } else {
        joutput("__");
      }
      for (p = field->name; *p != '\0'; ++p) {
        if (*p == '-') {
          joutput("_");
        } else {
          joutput("%c", *p);
        }
      }
      field = field->parent;
    }
  }
  return 0;
}

static void joutput_base(struct cb_field *f) {
  struct cb_field *top;
  struct cb_field *p;
  struct base_list *bl;
  char name[COB_SMALL_BUFF];
  top = cb_field_founder(f);

  if (f->flag_item_78) {
    fprintf(stderr, "Unexpected CONSTANT item\n");
    ABORT();
  }

  if (top->redefines) {
    top = top->redefines;
  }

  // EDIT
  /* Base name */
  if (top->flag_external) {
    strcpy(name, top->name);
    char *nmp;
    for (nmp = name; *nmp; nmp++) {
      if (*nmp == '-') {
        *nmp = '_';
      }
    }
  } else {
    register_data_storage_list(f, top);
  }

  if (!top->flag_base) {
    if (!top->flag_external) {
      if (!top->flag_local || top->flag_is_global) {
        bl = cobc_malloc(sizeof(struct base_list));
        bl->f = top;
        bl->curr_prog = excp_current_program_id;
        bl->next = base_cache;
        base_cache = bl;
      } else {
        if (current_prog->flag_global_use) {
          joutput_local("unsigned char\t\t*%s%s = NULL;", CB_PREFIX_BASE, name);
          joutput_local("unsigned char\t\t*%s%s = NULL;", CB_PREFIX_BASE, name);
          joutput_local("\t/* %s */\n", top->name);
          joutput_local("static unsigned char\t*save_%s%s;\n", CB_PREFIX_BASE,
                        name);
        } else {
          joutput_local("unsigned char\t*%s%s = NULL;", CB_PREFIX_BASE, name);
          joutput_local("\t/* %s */\n", top->name);
        }
      }
    }
    top->flag_base = 1;
  }

  if (top->flag_external) {
    joutput("%s%s", CB_PREFIX_BASE, name);
  } else {
    if (joutput_field_storage(f, top) && f->offset != 0) {
      joutput(".getSubDataStorage(%d)", f->offset);
    }
  }

  if (cb_field_variable_address(f)) {
    for (p = f->parent; p; f = f->parent, p = f->parent) {
      for (p = p->children; p != f; p = p->sister) {
        struct cb_field *v = cb_field_variable_size(p);
        if (v) {
          joutput(" + %d + ", v->offset - p->offset);
          if (v->size != 1) {
            joutput("%d * ", v->size);
          }
          joutput_integer(v->occurs_depending);
        } else {
          joutput(" + %d", p->size * p->occurs_max);
        }
      }
    }
  }
}

static void joutput_data(cb_tree x) {
  struct cb_literal *l;
  struct cb_reference *r;
  struct cb_field *f;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_LITERAL:
    l = CB_LITERAL(x);
    if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
      joutput("\"%s%s\"", l->data,
              (l->sign < 0)   ? "-"
              : (l->sign > 0) ? "+"
                              : "");

    } else {
      joutput_string(l->data, (int)l->size);
    }
    break;
  case CB_TAG_REFERENCE:
    r = CB_REFERENCE(x);
    f = CB_FIELD(r->value);

    /* Base address */
    joutput_base(f);

    /* Subscripts */
    if (r->subs) {
      joutput(".getSubDataStorage(");
      cb_tree lsub = r->subs;
      char first_time = 1;
      for (; f; f = f->parent) {
        if (f->flag_occurs) {
          if (!first_time) {
            joutput(" + ");
          }
          first_time = 0;
          if (f->size != 1) {
            joutput("%d * ", f->size);
          }
          joutput_index(CB_VALUE(lsub));
          lsub = CB_CHAIN(lsub);
        }
      }
      if (r->offset) {
        joutput("+ (");
        joutput_index(r->offset);
        joutput(")");
      }
      joutput(")");
    } else if (r->offset) {
      joutput(".getSubDataStorage(");
      joutput_index(r->offset);
      joutput(")");
    }
    break;
  case CB_TAG_CAST:
    joutput("&");
    joutput_param(x, 0);
    break;
  case CB_TAG_INTRINSIC:
    joutput("module.cob_procedure_parameters[%d]->data", field_iteration);
    break;
  case CB_TAG_CONST:
    if (x == cb_null) {
      joutput("null");
      return;
    }
    /* Fall through */
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    ABORT();
  }
}

static void joutput_size(cb_tree x) {
  struct cb_literal *l;
  struct cb_reference *r;
  struct cb_field *f;
  struct cb_field *p;
  struct cb_field *q;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CONST:
    joutput("1");
    break;
  case CB_TAG_LITERAL:
    l = CB_LITERAL(x);
    joutput("%d", (int)(l->size + ((l->sign != 0) ? 1 : 0)));
    break;
  case CB_TAG_REFERENCE:
    r = CB_REFERENCE(x);
    f = CB_FIELD(r->value);
    if (r->length) {
      joutput_integer(r->length);
    } else if (r->offset) {
      joutput("%d - ", f->size);
      joutput_index(r->offset);
    } else {
      p = cb_field_variable_size(f);
      q = f;

    again:
      if (p && (r->type == CB_SENDING_OPERAND ||
                !cb_field_subordinate(cb_field(p->occurs_depending), q))) {
        if (p->offset - q->offset > 0) {
          joutput("%d + ", p->offset - q->offset);
        }
        if (p->size != 1) {
          joutput("%d * ", p->size);
        }
        joutput_integer(p->occurs_depending);
        q = p;
      } else {
        joutput("%d", q->size);
      }

      for (; q != f; q = q->parent) {
        if (q->sister && !q->sister->redefines) {
          q = q->sister;
          p = q->occurs_depending ? q : cb_field_variable_size(q);
          joutput(" + ");
          goto again;
        }
      }
    }
    break;
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    ABORT();
  }
}

static int lookup_attr(int type, int digits, int scale, int flags,
                       unsigned char *pic, int lenstr) {
  struct attr_list *l;

  /* Search attribute cache */
  for (l = attr_cache; l; l = l->next) {
    if (type == l->type && digits == l->digits && scale == l->scale &&
        flags == l->flags &&
        ((pic == l->pic) ||
         (pic && l->pic && lenstr == l->lenstr &&
          memcmp((char *)pic, (char *)(l->pic), (size_t)lenstr) == 0))) {
      return l->id;
    }
  }

  /* Output new attribute */

  /* Cache it */
  l = cobc_malloc(sizeof(struct attr_list));
  l->id = cb_attr_id;
  l->type = type;
  l->digits = digits;
  l->scale = scale;
  l->flags = flags;
  l->pic = pic;
  l->lenstr = lenstr;
  l->next = attr_cache;
  attr_cache = l;

  return cb_attr_id++;
}

/**
 * CobolFieldAttribute型の変数の変数名(a_1, a_4など)を出力する.
 */
static void joutput_attr(cb_tree x) {
  struct cb_literal *l;
  struct cb_reference *r;
  struct cb_field *f;
  int id;
  int type;
  int flags;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_LITERAL:
    l = CB_LITERAL(x);
    if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
      flags = 0;
      if (l->sign != 0) {
        flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE;
      }
      id = lookup_attr(COB_TYPE_NUMERIC_DISPLAY, (int)l->size, l->scale, flags,
                       NULL, 0);
    } else if (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL) {
      if (l->all) {
        id = lookup_attr(COB_TYPE_NATIONAL_ALL, 0, 0, 0, NULL, 0);
      } else {
        id = lookup_attr(COB_TYPE_NATIONAL, 0, 0, 0, NULL, 0);
      }
    } else {
      if (l->all) {
        id = lookup_attr(COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, NULL, 0);
      } else {
        id = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
      }
    }
    break;
  case CB_TAG_REFERENCE:
    type = cb_tree_type(x);
    r = CB_REFERENCE(x);
    f = CB_FIELD(r->value);
    flags = 0;
    if (r->offset) {
      if (type == COB_TYPE_NATIONAL || type == COB_TYPE_NATIONAL_EDITED) {
        id = lookup_attr(COB_TYPE_NATIONAL, f->pic->digits, f->pic->scale,
                         flags, (ucharptr)f->pic->str, f->pic->lenstr);
      } else {
        id = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
      }
    } else {
      switch (type) {
      case COB_TYPE_GROUP:
      case COB_TYPE_ALPHANUMERIC:
        if (f->flag_justified) {
          id = lookup_attr(type, 0, 0, COB_FLAG_JUSTIFIED, NULL, 0);
        } else {
          id = lookup_attr(type, 0, 0, 0, NULL, 0);
        }
        break;
      default:
        if (f->pic->have_sign) {
          flags |= COB_FLAG_HAVE_SIGN;
          if (f->flag_sign_separate) {
            flags |= COB_FLAG_SIGN_SEPARATE;
          }
          if (f->flag_sign_leading) {
            flags |= COB_FLAG_SIGN_LEADING;
          }
        }
        if (f->flag_blank_zero) {
          flags |= COB_FLAG_BLANK_ZERO;
        }
        if (f->flag_justified) {
          flags |= COB_FLAG_JUSTIFIED;
        }
        if (f->flag_binary_swap) {
          flags |= COB_FLAG_BINARY_SWAP;
        }
        if (f->flag_real_binary) {
          flags |= COB_FLAG_REAL_BINARY;
        }
        if (f->flag_is_pointer) {
          flags |= COB_FLAG_IS_POINTER;
        }

        id = lookup_attr(type, f->pic->digits, f->pic->scale, flags,
                         (ucharptr)f->pic->str, f->pic->lenstr);
        break;
      }
    }
    break;
  case CB_TAG_ALPHABET_NAME:
    id = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
    break;
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    ABORT();
  }
  joutput("%s%d", CB_PREFIX_ATTR, id);
}

/**
 * AbstractCobolField型の変数をインスタンス化するコードを出力する
 */
static void joutput_field(cb_tree x) {
  joutput("CobolFieldFactory.makeCobolField(");
  joutput_size(x);
  joutput(", ");
  joutput_data(x);
  joutput(", ");
  joutput_attr(x);
  joutput(")");
}

/*
 * Literal
 */

static struct literal_list *lookup_literal(cb_tree x) {

  struct cb_literal *literal;
  struct literal_list *l;
  FILE *savetarget;

  literal = CB_LITERAL(x);
  /* Search literal cache */
  for (l = literal_cache; l; l = l->next) {
    if (CB_TREE_CLASS(literal) == CB_TREE_CLASS(l->literal) &&
        literal->size == l->literal->size && literal->all == l->literal->all &&
        literal->sign == l->literal->sign &&
        literal->scale == l->literal->scale &&
        memcmp(literal->data, l->literal->data, literal->size) == 0) {
      return l;
    }
  }

  /* Output new literal */

  savetarget = joutput_target;

  joutput_target = NULL;
  // output_field (x);

  joutput_target = savetarget;

  /* Cache it */
  l = cobc_malloc(sizeof(struct literal_list));
  l->id = cb_literal_id++;
  l->literal = literal;
  l->x = x;
  l->next = literal_cache;
  literal_cache = l;

  return l;
}

static void joutput_const_identifier(struct literal_list *l) {
  char s[MAX_LITERAL_SIZE + 1];
  memset(s, 0, MAX_LITERAL_SIZE + 1);
  int i = 0;
  for (i = 0; i < MAX_LITERAL_SIZE; i++) {
    char c = l->literal->data[i];
    if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ||
        ('0' <= c && c <= '9') || c == '_' || c == '$') {
      s[i] = c;
    } else {
      break;
    }
  }

  if (i == 0) {
    joutput("%s%d", CB_PREFIX_CONST, l->id);
  } else {
    joutput("%s%d_%s", CB_PREFIX_CONST, l->id, s);
  }
}

/*
 * Integer
 */
static int integer_reference_flag = 0;
static void joutput_integer(cb_tree x) {
  struct cb_binary_op *p;
  struct cb_cast *cp;
  struct cb_field *f;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CONST:
    if (x == cb_zero) {
      joutput("0");
    } else if (x == cb_null) {
      joutput("null");
    } else {
      joutput("%s", CB_CONST(x)->val);
    }
    break;
  case CB_TAG_INTEGER:
    joutput("%d", CB_INTEGER(x)->val);
    break;
  case CB_TAG_LITERAL:
    joutput("%d", cb_get_int(x));
    break;
  case CB_TAG_BINARY_OP:
    p = CB_BINARY_OP(x);
    if (p->op == '^') {
      joutput("(int) Math.pow (");
      joutput_integer(p->x);
      joutput(", ");
      joutput_integer(p->y);
      joutput(")");
    } else {
      joutput("(");
      if (need_double) {
        joutput("(double)");
      }
      joutput_integer(p->x);

      joutput(" %c ", p->op);
      if (need_double) {
        joutput("(double)");
      }
      joutput_integer(p->y);
      joutput(")");
    }
    break;
  case CB_TAG_CAST:
    cp = CB_CAST(x);
    switch (cp->type) {
    case CB_CAST_ADDRESS:
      joutput("(");
      joutput_data(cp->val);
      joutput(")");
      break;
    case CB_CAST_PROGRAM_POINTER:
      joutput_func_1("CobolResolve.resolveToPointer", x);
      break;
    default:
      fprintf(stderr, "Unexpected cast type %d\n", cp->type);
      ABORT();
    }
    break;
  case CB_TAG_REFERENCE:
    f = cb_field(x);
    switch (f->usage) {
    case CB_USAGE_INDEX:
    case CB_USAGE_LENGTH:
      joutput_data(x);
      if (!integer_reference_flag) {
        joutput(".intValue()");
      }
      return;

    case CB_USAGE_POINTER:
#ifdef COB_NON_ALIGNED
      joutput("(cob_get_pointer (");
      joutput_data(x);
      joutput("))");
#else
      joutput("(*(unsigned char **) (");
      joutput_data(x);
      joutput("))");
#endif
      return;

    case CB_USAGE_PROGRAM_POINTER:
#ifdef COB_NON_ALIGNED
      joutput("(cob_get_prog_pointer (");
      joutput_data(x);
      joutput("))");
#else
      joutput_data(x);
#endif
      return;

    case CB_USAGE_DISPLAY:
      if (f->pic && f->pic->scale >= 0 && f->size - f->pic->scale > 0 &&
          f->size - f->pic->scale <= 9 && f->pic->have_sign == 0) {
        joutput_data(x);
        joutput(".getNumdisp(%d)", f->size - f->pic->scale);

        return;
      }
      break;

    case CB_USAGE_PACKED:
      if (f->pic->scale == 0 && f->pic->digits < 10) {
        joutput_param(x, param_id);
        joutput(".getInt()");
        return;
      }
      break;

    case CB_USAGE_BINARY:
    case CB_USAGE_COMP_5:
    case CB_USAGE_COMP_X:
      if (f->size == 1) {
        joutput_data(x);
        if (!integer_reference_flag) {
          joutput(".getByte(0)");
        }
        return;
      }
#ifdef COB_NON_ALIGNED
      if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
          (
#ifdef COB_SHORT_BORK
              (f->size == 2 && (f->offset % 4 == 0)) ||
#else
              (f->size == 2 && (f->offset % 2 == 0)) ||
#endif
              (f->size == 4 && (f->offset % 4 == 0)) ||
              (f->size == 8 && (f->offset % 8 == 0)))) {
#else
      if (f->size == 2 || f->size == 4 || f->size == 8) {
#endif
        if (f->flag_binary_swap) {
          joutput_data(x);
          if (!integer_reference_flag) {
            switch (f->size) {
            case 2:
              joutput(".bswap_16()");
              break;
            case 4:
              joutput(".bswap_32()");
              break;
            case 8:
              joutput(".bswap_64()");
              break;
            }
          }
          return;
        } else {
          joutput_data(x);
          if (!integer_reference_flag) {
            switch (f->size) {
            case 2:
              joutput(".shortValue()");
              break;
            case 4:
              joutput(".intValue()");
              break;
            case 8:
              joutput(".longValue()");
              break;
            }
          }
          return;
        }
      }
      break;

    default:
      break;
    }

    joutput_param(x, param_id);
    joutput(".getInt()");
    break;
  case CB_TAG_INTRINSIC:
    joutput_param(x, -1);
    joutput(".getInt()");
    break;
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    ABORT();
  }
}

static void joutput_index(cb_tree x) {
  switch (CB_TREE_TAG(x)) {
  case CB_TAG_INTEGER:
    joutput("%d", CB_INTEGER(x)->val - 1);
    break;
  case CB_TAG_LITERAL:
    joutput("%d", cb_get_int(x) - 1);
    break;
  default:
    joutput("(");

    ++index_read_flag;
    joutput_integer(x);
    --index_read_flag;

    joutput(" - 1)");
    break;
  }
}

/*
 * Parameter
 */
static void joutput_param(cb_tree x, int id) {
  struct cb_reference *r;
  struct cb_field *f;
  struct cb_field *pechk;
  struct cb_cast *cp;
  struct cb_binary_op *bp;
  struct field_list *fl;
  FILE *savetarget;
  struct cb_intrinsic *ip;
  struct cb_alphabet_name *abp;
  struct cb_alphabet_name *rbp;
  cb_tree l;
  struct literal_list *ll;
  int n;
  int extrefs;
  int sav_stack_id;
  char fname[12];
  COB_UNUSED(extrefs);

  param_id = id;

  if (x == NULL) {
    joutput("null");
    return;
  }

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CONST:
    joutput("%s", CB_CONST(x)->val);
    break;
  case CB_TAG_INTEGER:
    joutput_integer(x);
    break;
  case CB_TAG_STRING:
    joutput_string(CB_STRING(x)->data, (int)CB_STRING(x)->size);
    break;
  case CB_TAG_LOCALE_NAME:
    joutput_param(CB_LOCALE_NAME(x)->list, id);
    break;
  case CB_TAG_ALPHABET_NAME:
    abp = CB_ALPHABET_NAME(x);
    switch (abp->type) {
    case CB_ALPHABET_STANDARD_1:
    case CB_ALPHABET_STANDARD_2:
#ifdef COB_EBCDIC_MACHINE
      gen_ebcdic_ascii = 1;
      joutput("cob_ebcdic_ascii");
      break;
#endif
    case CB_ALPHABET_NATIVE:
      gen_native = 1;
      joutput("null");
      break;
    case CB_ALPHABET_EBCDIC:
#ifdef COB_EBCDIC_MACHINE
      gen_native = 1;
      joutput("null");
#else
      gen_ebcdic = 1;
      joutput("new CobolDataStorage(cob_a2e)");
#endif
      break;
    case CB_ALPHABET_CUSTOM:
      gen_custom = 1;
      joutput("%s%s", CB_PREFIX_SEQUENCE, abp->cname);
      break;
    }
    break;
  case CB_TAG_CAST:
    cp = CB_CAST(x);
    switch (cp->type) {
    case CB_CAST_INTEGER:
      joutput_integer(cp->val);
      break;
    case CB_CAST_ADDRESS:
      joutput_data(cp->val);
      break;
    case CB_CAST_ADDR_OF_ADDR:
      joutput_data(cp->val);
      break;
    case CB_CAST_LENGTH:
      joutput_size(cp->val);
      break;
    case CB_CAST_PROGRAM_POINTER:
      joutput_param(cp->val, id);
      break;
    }
    break;
  case CB_TAG_DECIMAL:
    joutput("d%d", CB_DECIMAL(x)->id);
    break;
  case CB_TAG_FILE:
    joutput("%s%s", CB_PREFIX_FILE, CB_FILE(x)->cname);
    break;
  case CB_TAG_LITERAL:
    ll = lookup_literal(x);
    joutput_const_identifier(ll);
    break;
  case CB_TAG_FIELD:
    /* TODO: remove me */
    joutput_param(cb_build_field_reference(CB_FIELD(x), NULL), id);
    break;
  case CB_TAG_REFERENCE:
    r = CB_REFERENCE(x);
    extrefs = 0;
    if (r->check) {
      joutput("(new GetAbstractCobolField() {");
      joutput_newline();
      joutput_indent_level += 2;
      joutput_line(
          "public AbstractCobolField run() throws CobolStopRunException {");
      joutput_indent_level += 2;
      for (l = r->check; l; l = CB_CHAIN(l)) {
        sav_stack_id = stack_id;
        joutput_stmt(CB_VALUE(l), JOUTPUT_STMT_DEFAULT);
        stack_id = sav_stack_id;
      }
      joutput_prefix();
      joutput("return ");
    }

    if (CB_FILE_P(r->value)) {
      joutput("%s%s", CB_PREFIX_FILE, CB_FILE(r->value)->cname);
      if (r->check) {
#ifdef __GNUC__
#else
        --inside_check;
#endif
      }
      break;
    }
    if (CB_ALPHABET_NAME_P(r->value)) {
      rbp = CB_ALPHABET_NAME(r->value);
      switch (rbp->type) {
      case CB_ALPHABET_STANDARD_1:
      case CB_ALPHABET_STANDARD_2:
#ifdef COB_EBCDIC_MACHINE
        gen_ebcdic_ascii = 1;
        joutput("%sebcdic_ascii", CB_PREFIX_FIELD);
        break;
#endif
      case CB_ALPHABET_NATIVE:
        gen_native = 1;
        joutput("%snative", CB_PREFIX_FIELD);
        break;
      case CB_ALPHABET_EBCDIC:
#ifdef COB_EBCDIC_MACHINE
        gen_native = 1;
        joutput("%snative", CB_PREFIX_FIELD);
#else
        gen_full_ebcdic = 1;
        joutput("%sebcdic", CB_PREFIX_FIELD);
#endif
        break;
      case CB_ALPHABET_CUSTOM:
        gen_custom = 1;
        joutput("&%s%s", CB_PREFIX_FIELD, rbp->cname);
        break;
      }
      if (r->check) {
#ifdef __GNUC__
#else
        --inside_check;
#endif
        joutput(";");
        joutput_newline();
        joutput_indent_level -= 2;
        joutput_line("}");
        joutput_indent_level -= 2;
        joutput_prefix();
        joutput("}).run()");
      }
      break;
    }
    f = CB_FIELD(r->value);
    if (f->redefines && f->redefines->flag_external) {
      extrefs = 1;
      f->flag_item_external = 1;
      f->flag_external = 1;
    }
    if (f->redefines && f->redefines->flag_item_based) {
      f->flag_local = 1;
    }
    for (pechk = f->parent; pechk; pechk = pechk->parent) {
      if (pechk->flag_external) {
        extrefs = 1;
        f->flag_item_external = 1;
        break;
      }
      if (pechk->redefines && pechk->redefines->flag_external) {
        extrefs = 1;
        f->flag_item_external = 1;
        f->flag_external = 1;
        break;
      }
      if (pechk->flag_item_based) {
        f->flag_local = 1;
        break;
      }
      if (pechk->redefines && pechk->redefines->flag_item_based) {
        f->flag_local = 1;
        break;
      }
    }
    if (f->flag_external) {
      f->flag_item_external = 1;
    }
    if (!r->subs && !r->offset && f->count > 0 && !cb_field_variable_size(f) &&
        !cb_field_variable_address(f)) {
      if (!f->flag_field) {
        savetarget = joutput_target;
        joutput_target = NULL;
        joutput_field(x);

        fl = cobc_malloc(sizeof(struct field_list));
        fl->x = x;
        fl->f = f;
        fl->curr_prog = excp_current_program_id;
        fl->nulldata = (r->subs != NULL);
        fl->next = field_cache;
        field_cache = fl;

        f->flag_field = 1;
        joutput_target = savetarget;
      }
      if (f->flag_local) {
        char *field_name = get_java_identifier_field(f);
        if (f->flag_any_length && f->flag_anylen_done) {
          joutput(field_name);
        } else {
          joutput("new GetAbstractCobolField() {");
          joutput_newline();
          joutput_indent_level += 2;
          joutput_line("public AbstractCobolField run() {");
          joutput_indent_level += 2;
          joutput_prefix();
          joutput("%s.setDataStorage(", field_name);
          joutput_data(x);
          joutput(");");
          joutput_newline();
          joutput_line("return %s;", field_name);
          joutput_indent_level -= 2;
          joutput_line("}");
          joutput_indent_level -= 2;
          joutput_prefix();
          joutput("}.run()");
          if (f->flag_any_length) {
            f->flag_anylen_done = 1;
          }
        }
        free(field_name);
      } else {
        if (screenptr && f->storage == CB_STORAGE_SCREEN) {
          joutput("s_%d", f->id);
        } else {
          char *field_name = get_java_identifier_field(f);
          joutput(field_name);
          free(field_name);
        }
      }
    } else {
      if (stack_id >= num_cob_fields) {
        num_cob_fields = stack_id + 1;
      }
      sprintf(fname, "f%d", stack_id++);
#ifndef __GNUC__
      if (inside_check != 0) {
        if (inside_stack[inside_check - 1] != 0) {
          inside_stack[inside_check - 1] = 0;
          joutput(",\n");
        }
      }
#endif
      joutput("CobolFieldFactory.makeCobolField(");
      joutput_size(x);
      joutput(", ");
      joutput_data(x);
      joutput(", ");
      joutput_attr(x);
      joutput(")");
    }

    if (r->check) {
#ifdef __GNUC__
#else
      --inside_check;
#endif

      joutput(";");
      joutput_newline();
      joutput_indent_level -= 2;
      joutput_line("}");
      joutput_indent_level -= 2;
      joutput_prefix();
      joutput("}).run()");
    }
    break;
  case CB_TAG_BINARY_OP:
    bp = CB_BINARY_OP(x);
    joutput("CobolIntrinsic.intrBinop (");
    joutput_param(bp->x, id);
    joutput(", ");
    joutput("%d", bp->op);
    joutput(", ");
    joutput_param(bp->y, id);
    joutput(")");
    break;
  case CB_TAG_INTRINSIC:
    n = 0;
    ip = CB_INTRINSIC(x);
    joutput("%s (", ip->intr_tab->intr_routine);
    if (ip->intr_tab->refmod) {
      if (ip->offset) {
        joutput_integer(ip->offset);
        joutput(", ");
      } else {
        joutput("0, ");
      }
      if (ip->length) {
        joutput_integer(ip->length);
      } else {
        joutput("0");
      }
      if (ip->intr_field || ip->args) {
        joutput(", ");
      }
    }
    if (ip->intr_field) {
      if (ip->intr_field == cb_int0) {
        joutput("null");
      } else if (ip->intr_field == cb_int1) {
        for (l = ip->args; l; l = CB_CHAIN(l)) {
          n++;
        }
        joutput("%d", n);
      } else {
        joutput_param(ip->intr_field, id);
      }
      if (ip->args) {
        joutput(", ");
      }
    }
    for (l = ip->args; l; l = CB_CHAIN(l)) {
      joutput_param(CB_VALUE(l), id);
      id++;
      param_id++;
      if (CB_CHAIN(l)) {
        joutput(", ");
      }
    }
    joutput(")");
    break;
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    ABORT();
  }
}

/*
 * Function call
 */

static void joutput_funcall(cb_tree x) {
  struct cb_funcall *p;
  cb_tree l;
  int i;

  p = CB_FUNCALL(x);
  if (p->name[0] == '$') {
    int save_flag;
    switch (p->name[1]) {
    case 'E':
      /* Set of one character */
      joutput_data(p->argv[0]);
      joutput(".setByte(");
      joutput_param(p->argv[1], 1);
      joutput(")");

      break;
    case 'F':
      /* Move of one character */
      save_flag = param_wrap_string_flag;
      param_wrap_string_flag = 1;
      joutput_data(p->argv[0]);
      param_wrap_string_flag = save_flag;

      joutput(".setByte(");
      joutput_data(p->argv[1]);
      joutput(".getByte(0))");
      break;
    case 'G':
      /* Test of one character */
      joutput("(Byte.toUnsignedInt(");

      save_flag = param_wrap_string_flag;
      param_wrap_string_flag = 1;
      joutput_data(p->argv[0]);
      param_wrap_string_flag = save_flag;

      joutput(".getByte(0))");
      if (p->argv[1] == cb_space) {
        joutput(" - (int)' ')");
      } else if (p->argv[1] == cb_zero) {
        joutput(" - (int)'0')");
      } else if (p->argv[1] == cb_low) {
        joutput(")");
      } else if (p->argv[1] == cb_high) {
        joutput(" - 255)");
      } else if (CB_LITERAL_P(p->argv[1])) {
        joutput(" - (int)%d)", *(CB_LITERAL(p->argv[1])->data));
      } else {
        joutput(" - Byte.toUnsignedInt(");
        joutput_data(p->argv[1]);
        joutput(".getByte(0)))");
      }
      break;
    default:
      ABORT();
    }
    return;
  }

  if (CB_CALL_METHOD_P(p)) {
    screenptr = p->screenptr;

    int tmp_flag = param_wrap_string_flag;
    param_wrap_string_flag = 1;
    joutput_param(p->argv[0], 0);
    param_wrap_string_flag = tmp_flag;

    joutput(".%s (", p->name);
    for (i = 1; i < p->argc; i++) {
      if (p->varcnt && i + 1 == p->argc) {
        joutput("%d, ", p->varcnt);
        for (l = p->argv[i]; l; l = CB_CHAIN(l)) {
          joutput_param(CB_VALUE(l), i);
          i++;
          if (CB_CHAIN(l)) {
            joutput(", ");
          }
        }
      } else {
        joutput_param(p->argv[i], i);
        if (i + 1 < p->argc) {
          joutput(", ");
        }
      }
    }
    joutput(")");
    screenptr = 0;
  } else {
    screenptr = p->screenptr;
    joutput("%s (", p->name);

    int flag_func_is_check_odo = strcmp(p->name, "CobolCheck.checkOdo") == 0;
    int save_flag = param_wrap_string_flag;
    if (flag_func_is_check_odo) {
      param_wrap_string_flag = 0;
    }

    for (i = 0; i < p->argc; i++) {
      if (p->varcnt && i + 1 == p->argc) {
        joutput("%d, ", p->varcnt);
        for (l = p->argv[i]; l; l = CB_CHAIN(l)) {
          joutput_param(CB_VALUE(l), i);
          i++;
          if (CB_CHAIN(l)) {
            joutput(", ");
          }
        }
      } else {
        joutput_param(p->argv[i], i);
        if (i + 1 < p->argc) {
          joutput(", ");
        }
      }
    }
    param_wrap_string_flag = save_flag;
    joutput(")");
    screenptr = 0;
  }
}

static void joutput_func_1(const char *name, cb_tree x) {
  joutput("%s (", name);
  joutput_param(x, param_id);
  joutput(")");
}

/*
 * Condition
 */

static void joutput_cond(cb_tree x, int save_flag) {
  struct cb_binary_op *p;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CONST:
    if (x == cb_true) {
      joutput("true");
    } else if (x == cb_false) {
      joutput("false");
    } else {
      ABORT();
    }
    break;
  case CB_TAG_BINARY_OP:
    p = CB_BINARY_OP(x);
    switch (p->op) {
    case '!':
      joutput("!");
      joutput_cond(p->x, save_flag);
      break;

    case '&':
    case '|':
      joutput("(");
      joutput_cond(p->x, save_flag);
      joutput(p->op == '&' ? " && " : " || ");
      joutput_cond(p->y, save_flag);
      joutput(")");
      break;

    case '=':
    case '<':
    case '[':
    case '>':
    case ']':
    case '~':
      joutput("((long)");
      joutput_cond(p->x, save_flag);
      switch (p->op) {
      case '=':
        joutput(" == 0L");
        break;
      case '<':
        joutput(" <  0L");
        break;
      case '[':
        joutput(" <= 0L");
        break;
      case '>':
        joutput(" >  0L");
        break;
      case ']':
        joutput(" >= 0L");
        break;
      case '~':
        joutput(" != 0L");
        break;
      }
      joutput(")");
      break;

    default:
      joutput_integer(x);
      break;
    }
    break;
  case CB_TAG_FUNCALL:
    if (save_flag) {
      joutput("(ret = ");
    }
    joutput_funcall(x);
    if (save_flag) {
      joutput(")");
    }
    break;
  case CB_TAG_LIST:
    if (save_flag) {
      joutput("(ret = ");
    }
    //#ifdef __GNUC__
    //		joutput_indent ("({");
    //#else
    //		inside_stack[inside_check] = 0;
    //		++inside_check;
    //		joutput ("(\n");
    //#endif
    joutput_indent("(new GetInt() {");
    joutput_indent_level += 2;
    joutput_indent("public int run(){");
    joutput_indent_level += 2;
    for (; x; x = CB_CHAIN(x)) {
      //最後の文ならreturn文を書く
      if (!CB_CHAIN(x)) {
        joutput_indent("return ");
      }
      joutput_stmt(CB_VALUE(x), JOUTPUT_STMT_DEFAULT);
    }
    //#ifdef __GNUC__
    //		joutput_indent ("})");
    //#else
    //		--inside_check;
    //		joutput (")");
    //#endif
    joutput_indent_level -= 2;
    joutput_indent("}");
    joutput_indent_level -= 2;
    joutput_prefix();
    joutput("}).run()");
    if (save_flag) {
      joutput(")");
    }
    break;
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    ABORT();
  }
}

/*
 * MOVE
 */

static void joutput_move(cb_tree src, cb_tree dst) {
  /* suppress warnings */
  suppress_warn = 1;
  joutput_stmt(cb_build_move(src, dst), JOUTPUT_STMT_DEFAULT);
  suppress_warn = 0;
}

/*
 * INITIALIZE
 */

static int initialize_type(struct cb_initialize *p, struct cb_field *f,
                           int topfield) {

  if (f->flag_item_78) {
    fprintf(stderr, "Unexpected CONSTANT item\n");
    ABORT();
  }

  if (f->flag_chained) {
    return INITIALIZE_ONE;
  }

  if (f->redefines && (!topfield || !p->flag_statement)) {
    return INITIALIZE_NONE;
  }

  if (p->val && f->values) {
    if (topfield && f->flag_occurs && !p->flag_statement) {
      return INITIALIZE_COMPOUND;
    } else {
      return INITIALIZE_ONE;
    }
  }

  if (p->flag_statement && !f->children) {
    if (strlen(f->name) > 4 && f->name[4] == '$') {
      return INITIALIZE_NONE;
    }
  }

  if (f->children) {
    int type = initialize_type(p, f->children, 0);
    if (type == INITIALIZE_ONE) {
      return INITIALIZE_COMPOUND;
    }
    for (f = f->children->sister; f; f = f->sister) {
      if (type != initialize_type(p, f, 0)) {
        return INITIALIZE_COMPOUND;
      }
    }
    return type;
  } else {
    cb_tree l;
    for (l = p->rep; l; l = CB_CHAIN(l)) {
      if ((int)CB_PURPOSE_INT(l) == (int)CB_TREE_CATEGORY(f)) {
        return INITIALIZE_ONE;
      }
    }
  }

  if (p->def) {
    if (f->usage == CB_USAGE_FLOAT || f->usage == CB_USAGE_DOUBLE) {
      return INITIALIZE_ONE;
    }
    switch (CB_TREE_CATEGORY(f)) {
    case CB_CATEGORY_NUMERIC_EDITED:
    case CB_CATEGORY_ALPHANUMERIC_EDITED:
    case CB_CATEGORY_NATIONAL_EDITED:
    case CB_CATEGORY_NATIONAL:
      return INITIALIZE_ONE;
    default:
      if (cb_tree_type(CB_TREE(f)) == COB_TYPE_NUMERIC_PACKED) {
        return INITIALIZE_ONE;
      } else if (cb_tree_type(CB_TREE(f)) == COB_TYPE_NUMERIC_DISPLAY) {
        if (f->flag_sign_separate) {
          return INITIALIZE_ONE;
        } else if (cb_display_sign == COB_DISPLAY_SIGN_EBCDIC &&
                   f->pic->have_sign) {
          return INITIALIZE_ONE;
        } else {
          return INITIALIZE_DEFAULT;
        }
      } else {
        return INITIALIZE_DEFAULT;
      }
    }
  }

  return INITIALIZE_NONE;
}

static int initialize_uniform_char(struct cb_field *f, int flag_statement) {
  if (!flag_statement && cb_default_byte_specified) {
    return cb_default_byte;
  }

  if (f->children) {
    int c = initialize_uniform_char(f->children, flag_statement);
    for (f = f->children->sister; f; f = f->sister) {
      if (!f->redefines) {
        if (c != initialize_uniform_char(f, flag_statement)) {
          return -1;
        }
      }
    }
    return c;
  } else {
    switch (cb_tree_type(CB_TREE(f))) {
    case COB_TYPE_NUMERIC_BINARY:
      return 0;
    case COB_TYPE_NUMERIC_DISPLAY:
      return '0';
    case COB_TYPE_ALPHANUMERIC:
      return ' ';
    case COB_TYPE_NATIONAL:
      return ' ';
    default:
      return -1;
    }
  }
}

static void joutput_figurative(cb_tree x, struct cb_field *f, const int value) {
  joutput_prefix();
  if (f->size == 1) {
    joutput_data(x);
    char *value_string = convert_byte_value_format(value);
    joutput(".setByte(0, %s);\n", value_string);
    free(value_string);
  } else {
    joutput_data(x);
    joutput(".fillBytes (");
    if (CB_REFERENCE_P(x) && CB_REFERENCE(x)->length) {
      joutput("%d, ", value);
      joutput_size(x);
      joutput(");\n");
    } else {
      joutput("%d, %d);\n", value, f->size);
    }
  }
}

static void joutput_initialize_literal(cb_tree x, struct cb_field *f,
                                       struct cb_literal *l) {
  size_t i;
  size_t n;

  if (l->size == 1) {
    joutput_prefix();
    joutput_data(x);
    joutput(".fillBytes(");
    if (CB_REFERENCE_P(x) && CB_REFERENCE(x)->length) {
      joutput("%d, ", l->data[0]);
      joutput_size(x);
      joutput(");\n");
    } else {
      joutput("%d, %d);\n", l->data[0], f->size);
    }
    return;
  }
  if (l->size >= f->size) {
    joutput_prefix();
    joutput_data(x);
    joutput("memcpy (");
    joutput_string(l->data, f->size);
    joutput(", %d);\n", f->size);
    return;
  }
  i = f->size / l->size;
  i_counters[0] = 1;

  joutput_line("for (int i0 = 0; i0 < %u; i0++)", (unsigned int)i);
  joutput_indent("  {");
  joutput_prefix();
  joutput_data(x);
  joutput(".memcpy(i0 * %u, ", (unsigned int)l->size);
  joutput_string(l->data, l->size);
  joutput(", %u);\n", (unsigned int)l->size);
  joutput_indent("  }");

  n = f->size % l->size;
  if (n) {
    joutput_prefix();
    joutput_data(x);
    joutput(".memcpy(i0 * %u, ", (unsigned int)l->size);
    joutput_string(l->data, n);
    joutput(", %u);\n", (unsigned int)n);
  }
}

static void joutput_initialize_fp(cb_tree x, struct cb_field *f) {
  joutput_prefix();
  if (f->usage == CB_USAGE_FLOAT) {
    joutput("{float temp = 0.0;");
  } else {
    joutput("{double temp = 0.0;");
  }
  joutput(" LIBCOB.memcpy (");
  joutput_data(x);
  joutput(", (char *)&temp, sizeof(temp));}\n");
}

static void joutput_initialize_external(cb_tree x, struct cb_field *f) {
  unsigned char *p;
  cb_tree file;
  char name[COB_MINI_BUFF];

  joutput_prefix();
  joutput_data(x);
  if (f->ename) {
    joutput(" = CobolExternal.getStorageAddress (\"%s\", %d);\n", f->ename,
            f->size);
  } else if (f->storage == CB_STORAGE_FILE) {
    file = CB_TREE(f->file);
    strcpy(name, CB_FILE(file)->record->name);
    for (p = (unsigned char *)name; *p; p++) {
      if (*p == '-') {
        *p = '_';
      }
    }
    joutput(" = CobolExternal.getStorageAddress (\"%s\", %d);\n", name,
            f->size);
  } else {
    strcpy(name, f->name);
    for (p = (unsigned char *)name; *p; p++) {
      if (islower(*p)) {
        *p = (unsigned char)toupper(*p);
      }
    }
    joutput(" = CobolExternal.getStorageAddress (\"%s\", %d);\n", name,
            f->size);
  }
}

static void joutput_initialize_uniform(cb_tree x, int c, int size) {
  joutput_prefix();
  if (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL) {
    if (c == ' ') {
      joutput_param(cb_space, 1);
      joutput(".moveFrom(");
      joutput_param(x, 2);
      joutput(");\n");
    }
    return;
  }
  if (size == 1) {
    joutput_data(x);

    char *value_string = convert_byte_value_format(c);
    joutput(".setByte(%s);\n", value_string);
    free(value_string);

  } else {
    if (CB_REFERENCE_P(x) && CB_REFERENCE(x)->length) {
      joutput_data(x);
      char *value_string = convert_byte_value_format(c);
      joutput(".fillBytes(%s, ", value_string);
      free(value_string);
      joutput_size(x);
      joutput(");\n");
    } else {
      joutput_data(x);
      char *value_string = convert_byte_value_format(c);
      joutput(".fillBytes(%s, %d);\n", value_string, size);
      free(value_string);
    }
  }
}

static void joutput_initialize_one(struct cb_initialize *p, cb_tree x) {
  struct cb_field *f;

  f = cb_field(x);

  /* CHAINING */
  if (f->flag_chained) {
    joutput_prefix();
    joutput("cob_chain_setup (");
    joutput_data(x);
    joutput(", %d, %d);\n", f->param_num, f->size);
    return;
  }
  /* Initialize by value */
  if (p->val && f->values) {
    cb_tree value = CB_VALUE(f->values);

    /* NATIONAL also needs no editing but mbchar conversion. */
    if (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL) {
      joutput_prefix();
      joutput_param(x, 2);
      joutput(".moveFrom(");
      joutput_param(value, 1);
      joutput(");\n");
      return;
    }
    if (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL_EDITED) {
      cb_tree tmpx = cb_build_reference(f->name);
      CB_REFERENCE(tmpx)->value = cb_ref(tmpx);
      CB_TREE_CATEGORY(tmpx);
      CB_REFERENCE(tmpx)->offset =
          cb_build_numeric_literal(0, (unsigned char *)"1", 1);
      CB_REFERENCE(tmpx)->subs = CB_REFERENCE(x)->subs;

      joutput_param((cb_tree)tmpx, 2);
      joutput(".moveFrom(");
      joutput_param(value, 1);
      joutput(");\n");

      return;
    }

    if (value == cb_space) {
      /* Fixme: This is to avoid an error when a
         numeric-edited item has VALUE SPACE because
         cob_build_move doubly checks the value.
         We should instead check the value only once.  */
      joutput_figurative(x, f, ' ');
    } else if (value == cb_low) {
      joutput_figurative(x, f, 0);
    } else if (value == cb_high) {
      joutput_figurative(x, f, 255);
    } else if (value == cb_quote) {
      joutput_figurative(x, f, '"');
    } else if (value == cb_zero && f->usage == CB_USAGE_DISPLAY) {
      if (f->flag_sign_separate) {
        joutput_move(cb_zero, x);
      } else if (cb_display_sign == COB_DISPLAY_SIGN_EBCDIC &&
                 f->pic->have_sign) {
        joutput_move(cb_zero, x);
      } else {
        joutput_figurative(x, f, '0');
      }
    } else if (value == cb_null && f->usage == CB_USAGE_DISPLAY) {
      joutput_figurative(x, f, 0);
    } else if (CB_LITERAL_P(value) && CB_LITERAL(value)->all) {
      /* ALL literal */
      joutput_initialize_literal(x, f, CB_LITERAL(value));
    } else if (CB_CONST_P(value) || CB_TREE_CLASS(value) == CB_CLASS_NUMERIC) {
      /* Figurative literal, numeric literal */
      joutput_move(value, x);
    } else {
      /* Alphanumeric literal */
      /* We do not use joutput_move here because
         we do not want to have the value be edited. */

      struct cb_literal *l = CB_LITERAL(value);
      static char *buff = NULL;
      static int lastsize = 0;
      if (!buff) {
        if (f->size <= COB_SMALL_BUFF) {
          buff = cobc_malloc(COB_SMALL_BUFF);
          lastsize = COB_SMALL_BUFF;
        } else {
          buff = cobc_malloc((size_t)f->size);
          lastsize = f->size;
        }
      } else {
        if (f->size > lastsize) {
          free(buff);
          buff = cobc_malloc((size_t)f->size);
          lastsize = f->size;
        }
      }
      if ((int)l->size >= (int)f->size) {
        memcpy(buff, l->data, (size_t)f->size);
      } else {
        memcpy(buff, l->data, l->size);
        memset(buff + l->size, ' ', f->size - l->size);
      }
      joutput_prefix();
      if (f->size == 1) {
        joutput_data(x);
        char *value_string = convert_byte_value_format(*(unsigned char *)buff);
        joutput(".setByte(0, %s);", value_string);
        free(value_string);
      } else {
        int buffchar = *buff;
        int i;
        for (i = 0; i < f->size; i++) {
          if (*(buff + i) != buffchar) {
            break;
          }
        }
        if (i == f->size) {
          joutput_data(x);
          joutput(".fillBytes(%d, %d);\n", buffchar, f->size);
        } else {
          if (f->size >= 8) {
            buffchar = *(buff + f->size - 1);
            int n = 0;
            for (i = f->size - 1; i >= 0; i--, n++) {
              if (*(buff + i) != buffchar) {
                break;
              }
            }
            if (n > 2) {
              joutput_data(x);
              joutput(".memcpy(");
              joutput_string((ucharptr)buff, f->size - n);
              joutput(", %d);\n", f->size - n);
              joutput_prefix();
              joutput_data(x);
              joutput(".memset(%d, %d, %d);", f->size - n, buffchar, n);
              return;
            }
          }
          joutput_data(x);
          joutput(".setBytes (");
          joutput_string((ucharptr)buff, f->size);
          joutput(", %d);\n", f->size);
        }
      }
    }
    return;
  }

  /* Initialize replacing */
  if (!f->children) {
    cb_tree lrp;
    for (lrp = p->rep; lrp; lrp = CB_CHAIN(lrp)) {
      if ((int)CB_PURPOSE_INT(lrp) == (int)CB_TREE_CATEGORY(x)) {
        joutput_move(CB_VALUE(lrp), x);
        return;
      }
    }
  }

  /* Initialize by default */
  if (p->def) {
    if (f->usage == CB_USAGE_FLOAT || f->usage == CB_USAGE_DOUBLE) {
      joutput_initialize_fp(x, f);
      return;
    }
    switch (CB_TREE_CATEGORY(x)) {
    case CB_CATEGORY_NUMERIC:
    case CB_CATEGORY_NUMERIC_EDITED:
      joutput_move(cb_zero, x);
      break;
    case CB_CATEGORY_ALPHANUMERIC_EDITED:
    case CB_CATEGORY_NATIONAL_EDITED:
    case CB_CATEGORY_NATIONAL:
      if (CB_TREE_TAG(p) == CB_TAG_INITIALIZE) {
        joutput_move(cb_blank, x);
      } else {
        joutput_move(cb_space, x);
      }
      break;
    default:
      fprintf(stderr, "Unexpected tree category %d\n", CB_TREE_CATEGORY(x));
      ABORT();
    }
  }
}

static void joutput_initialize_compound(struct cb_initialize *p, cb_tree x) {
  struct cb_field *ff;
  struct cb_field *f;
  struct cb_field *last_field;
  int last_char;
  int i;
  size_t size;
  static int recurs_level = 0;

  ff = cb_field(x);
  if (!recurs_level && !p->flag_statement && ff->parent == NULL &&
      ff->flag_occurs) {
    f = ff;
  } else {
    f = ff->children;
  }
  ++recurs_level;
  while (f) {
    int type = initialize_type(p, f, 0);
    cb_tree c = cb_build_field_reference(f, x);

    switch (type) {
    case INITIALIZE_NONE:
      break;
    case INITIALIZE_DEFAULT: {
      last_field = f;
      last_char = initialize_uniform_char(f, p->flag_statement);

      if (last_char != -1) {
        if (f->flag_occurs) {
          CB_REFERENCE(c)->subs = cb_cons(cb_int1, CB_REFERENCE(c)->subs);
        }

        for (; f->sister; f = f->sister) {
          if (!f->sister->redefines) {
            if (initialize_type(p, f->sister, 0) != INITIALIZE_DEFAULT ||
                initialize_uniform_char(f->sister, p->flag_statement) !=
                    last_char ||
                CB_TREE_CATEGORY(f->sister) != CB_TREE_CATEGORY(last_field)) {
              break;
            }
          }
        }

        if (f->sister) {
          size = f->sister->offset - last_field->offset;
        } else {
          size = ff->offset + ff->size - last_field->offset;
        }

        joutput_initialize_uniform(c, last_char, (int)size);
        break;
      }
      /* Fall through */
    }
    default:
      if (f->flag_occurs) {
        /* Begin occurs loop */
        i = f->indexes;
        i_counters[i] = 1;
        joutput_line("for (int i%d = 1; i%d <= %d; i%d++)", i, i, f->occurs_max,
                     i);
        joutput_indent("  {");
        CB_REFERENCE(c)->subs = cb_cons(cb_i[i], CB_REFERENCE(c)->subs);
      }

      if (type == INITIALIZE_ONE) {
        joutput_initialize_one(p, c);
      } else {
        joutput_initialize_compound(p, c);
      }
      joutput_newline();

      if (f->flag_occurs) {
        /* Close loop */
        CB_REFERENCE(c)->subs = CB_CHAIN(CB_REFERENCE(c)->subs);
        joutput_indent("  }");
      }
    }
    if (f == ff)
      break;
    f = f->sister;
  }
  --recurs_level;
}

static void joutput_initialize(struct cb_initialize *p) {
  struct cb_field *f;
  int c;

  f = cb_field(p->var);
  if (f->flag_external) {
    joutput_initialize_external(p->var, f);
    if (!p->flag_statement) {
      return;
    }
  }
  switch (initialize_type(p, f, 1)) {
  case INITIALIZE_NONE:
    break;
  case INITIALIZE_ONE:
    joutput_initialize_one(p, p->var);
    break;
  case INITIALIZE_DEFAULT:
    c = initialize_uniform_char(f, p->flag_statement);
    if (c != -1) {
      joutput_initialize_uniform(p->var, c, f->size);
    } else {
      joutput_initialize_compound(p, p->var);
    }
    break;
  case INITIALIZE_COMPOUND:
    joutput_initialize_compound(p, p->var);
    break;
  }
}
/*
 * SEARCH
 */

static void joutput_occurs(struct cb_field *p) {
  if (p->occurs_depending) {
    joutput_integer(p->occurs_depending);
  } else {
    joutput("%d", p->occurs_max);
  }
}

static void joutput_search_whens(cb_tree table, cb_tree var, cb_tree stmt,
                                 cb_tree whens) {
  struct cb_field *p;
  cb_tree idx = NULL;

  p = cb_field(table);
  /* Determine the index to use */
  if (var) {
    cb_tree l;
    for (l = p->index_list; l; l = CB_CHAIN(l)) {
      if (cb_ref(CB_VALUE(l)) == cb_ref(var)) {
        idx = var;
      }
    }
  }
  if (!idx) {
    idx = CB_VALUE(p->index_list);
  }

  /* Start loop */
  joutput_line("for (;;)");
  joutput_indent("  {");

  /* End test */
  joutput_prefix();
  joutput("if (");
  joutput_integer(idx);

  struct cb_field *f = cb_field(idx);
  if (CB_TREE_TAG(idx) == CB_TAG_REFERENCE &&
      (f->usage == CB_USAGE_INDEX || f->usage == CB_USAGE_LENGTH)) {
    // joutput(".intValue()");
  }

  joutput(" > ");
  joutput_occurs(p);
  joutput(")\n");
  joutput_indent("  {");
  if (stmt) {
    joutput_stmt(stmt, JOUTPUT_STMT_DEFAULT);
  }
  joutput_line("break;");
  joutput_indent("  }");

  /* WHEN test */
  joutput_stmt(whens, JOUTPUT_STMT_DEFAULT);
  joutput_line("else");
  joutput_indent("  {");
  joutput_prefix();

  int tmp_flag = integer_reference_flag;
  integer_reference_flag = 1;
  joutput_integer(idx);
  integer_reference_flag = tmp_flag;

  joutput(".set(");
  joutput_integer(idx);
  joutput_line(" + 1);");

  if (var && var != idx) {
    joutput_move(idx, var);
  }
  joutput_line("continue;");
  joutput_indent("  }");
  joutput_line("break;");
  joutput_indent("  }");
}

static void joutput_search_all(cb_tree table, cb_tree stmt, cb_tree cond,
                               cb_tree when) {
  struct cb_field *p;
  cb_tree idx;

  p = cb_field(table);
  idx = CB_VALUE(p->index_list);

  /* Header */
  joutput_indent("{");
  joutput_line("int ret;");
  joutput_line("int head = %d;", p->occurs_min - 1);
  joutput_prefix();
  joutput("int tail = ");
  if (p->occurs_depending) {
    joutput_integer(p->occurs_depending);
    joutput(" + 1;\n");
  } else {
    joutput("%d;\n", p->occurs_max + 1);
  }

  /* Start loop */
  joutput_line("for (;;)");
  joutput_indent("  {");

  /* End test */
  joutput_line("if (head >= tail - 1)");
  joutput_indent("  {");
  if (stmt) {
    joutput_stmt(stmt, JOUTPUT_STMT_DEFAULT);
  }
  joutput_line("break;");
  joutput_indent("  }");

  /* Next index */
  joutput_prefix();
  int tmp_flag = integer_reference_flag;
  integer_reference_flag = 1;
  joutput_integer(idx);
  integer_reference_flag = tmp_flag;
  joutput(".set((head + tail) / 2);\n");

  /* WHEN test */
  joutput_prefix();
  joutput("if (");
  joutput_cond(cond, 1);
  joutput(")\n");
  joutput_indent_level += 2;
  joutput_stmt(when, JOUTPUT_STMT_DEFAULT);
  joutput_indent_level -= 2;
  joutput_line("else");
  joutput_indent("  {");
  joutput_line("if (ret < 0)");
  joutput_prefix();
  joutput("  head = ");
  joutput_integer(idx);
  joutput(";\n");
  joutput_line("else");
  joutput_prefix();
  joutput("  tail = ");
  joutput_integer(idx);
  joutput(";\n");
  joutput_line("continue;");
  joutput_indent("  }");
  joutput_line("break;");
  joutput_indent("  }");
  joutput_indent("}");
}

static void joutput_search(struct cb_search *p) {
  if (p->flag_all) {
    joutput_search_all(p->table, p->end_stmt, CB_IF(p->whens)->test,
                       CB_IF(p->whens)->stmt1);
  } else {
    joutput_search_whens(p->table, p->var, p->end_stmt, p->whens);
  }
}

/*
 * CALL
 */

static void joutput_call(struct cb_call *p) {
  cb_tree x;
  cb_tree l;
  struct cb_literal *lp;
  char *callp;
  struct cb_field *f;
  char *system_call = NULL;
  struct system_table *psyst;
  size_t n;
  size_t retptr;
  int dynamic_link = 1;
  int sizes;

  retptr = 0;
  if (p->returning && CB_TREE_CLASS(p->returning) == CB_CLASS_POINTER) {
    retptr = 1;
  }
  /* System routine entry points */
  if (p->is_system) {
    lp = CB_LITERAL(p->name);
    psyst = (struct system_table *)&system_tab[0];
    for (; psyst->syst_name; psyst++) {
      if (!strcmp((const char *)lp->data, (const char *)psyst->syst_name)) {
        system_call = (char *)psyst->syst_call;
        dynamic_link = 0;
        break;
      }
    }
  }

  if (cb_flag_static_call && CB_LITERAL_P(p->name)) {
    dynamic_link = 0;
  }

  /* Local variables */
#ifdef COB_NON_ALIGNED
  if (dynamic_link && retptr) {
    // output_line ("void *temptr;");
  }
#endif

  if (CB_REFERENCE_P(p->name) && CB_FIELD_P(CB_REFERENCE(p->name)->value) &&
      CB_FIELD(CB_REFERENCE(p->name)->value)->usage ==
          CB_USAGE_PROGRAM_POINTER) {
    dynamic_link = 0;
  }

  /* Setup arguments */
  // TODO BY VALUE and BY CONTENT
  for (l = p->args, n = 1; l; l = CB_CHAIN(l), n++) {
    x = CB_VALUE(l);
    switch (CB_PURPOSE_INT(l)) {
    case CB_CALL_BY_REFERENCE:
      if (CB_NUMERIC_LITERAL_P(x) || CB_BINARY_OP_P(x)) {
        joutput_line(
            "CobolCallDataContent content_%d = new CobolCallDataContent(8);",
            (int)n);
      } else if (CB_CAST_P(x)) {
        joutput_line("void *ptr_%d;", (int)n);
      }
      break;
    case CB_CALL_BY_CONTENT:
      if (CB_CAST_P(x)) {
        joutput_line("void *ptr_%d;", (int)n);
      } else if (CB_TREE_TAG(x) != CB_TAG_INTRINSIC && x != cb_null &&
                 !(CB_CAST_P(x))) {
        joutput_prefix();
        joutput("CobolCallDataContent content_%d = new CobolCallDataContent (",
                (int)n);
        if (CB_NUMERIC_LITERAL_P(x) || CB_BINARY_OP_P(x) || CB_CAST_P(x)) {
          joutput("8");
        } else {
          if (CB_REF_OR_FIELD_P(x)) {
            joutput("%d", (int)cb_field(x)->size);
          } else {
            joutput_size(x);
          }
        }
        joutput_line(");");
      }
      break;
    }
  }
  for (l = p->args, n = 1; l; l = CB_CHAIN(l), n++) {
    x = CB_VALUE(l);
    switch (CB_PURPOSE_INT(l)) {
    case CB_CALL_BY_REFERENCE:
      if (CB_NUMERIC_LITERAL_P(x)) {
        joutput_prefix();
        if (cb_fits_int(x)) {
          joutput("content_%d.dataint = ", (int)n);
          joutput("%d", cb_get_int(x));
        } else {
          joutput("content_%d.datall = ", (int)n);
          joutput("%lldLL", cb_get_long_long(x));
        }
        joutput(";\n");
      } else if (CB_BINARY_OP_P(x)) {
        joutput_prefix();
        joutput("content_%d.dataint = ", (int)n);
        joutput_integer(x);
        joutput(";\n");
      } else if (CB_CAST_P(x)) {
        joutput_prefix();
        joutput("ptr_%d = ", (int)n);
        joutput_integer(x);
        joutput(";\n");
      }
      break;
    case CB_CALL_BY_CONTENT:
      if (CB_CAST_P(x)) {
        joutput_prefix();
        joutput("ptr_%d = ", (int)n);
        joutput_integer(x);
        joutput(";\n");
      } else if (CB_TREE_TAG(x) != CB_TAG_INTRINSIC) {
        if (CB_NUMERIC_LITERAL_P(x)) {
          joutput_prefix();
          if (cb_fits_int(x)) {
            joutput("content_%d.dataint = ", (int)n);
            joutput("%d", cb_get_int(x));
          } else {
            joutput("content_%d.datall = ", (int)n);
            joutput("%lldLL", cb_get_long_long(x));
          }
          joutput(";\n");
        } else if (CB_REF_OR_FIELD_P(x) &&
                   CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC &&
                   cb_field(x)->usage == CB_USAGE_LENGTH) {
          joutput_prefix();
          joutput("content_%d.dataint = ", (int)n);
          joutput_integer(x);
          joutput(";\n");
        } else if (x != cb_null && !(CB_CAST_P(x))) {
          joutput_prefix();
          joutput("content_%d.data.memcpy(", (int)n);
          joutput_data(x);
          joutput(", ");
          joutput_size(x);
          joutput(");\n");
        }
      }
      break;
    }
  }

  /* Function name */
  joutput_prefix();
  joutput("CobolModule.getCurrentModule ().setParameters (");
  n = 0;
  for (l = p->args; l; l = CB_CHAIN(l), n++) {
    x = CB_VALUE(l);
    field_iteration = (int)n;
    switch (CB_TREE_TAG(x)) {
    case CB_TAG_LITERAL:
    case CB_TAG_FIELD:
    case CB_TAG_INTRINSIC:
      joutput_param(x, -1);
      break;
    case CB_TAG_REFERENCE:
      switch (CB_TREE_TAG(CB_REFERENCE(x)->value)) {
      case CB_TAG_LITERAL:
      case CB_TAG_FIELD:
      case CB_TAG_INTRINSIC:
        joutput_param(x, -1);
        break;
      default:
        joutput("null");
        break;
      }
      break;
    default:
      joutput("null");
      break;
    }
    if (CB_CHAIN(l)) {
      joutput(", ");
    }
  }
  joutput(");\n");
  joutput_line("CobolCallParams.callParams = %d;", (int)n);

  if (!dynamic_link) {
    if (CB_REFERENCE_P(p->name) && CB_FIELD_P(CB_REFERENCE(p->name)->value) &&
        CB_FIELD(CB_REFERENCE(p->name)->value)->usage ==
            CB_USAGE_PROGRAM_POINTER) {
      joutput_prefix();
      joutput("cob_unifunc = CobolResolve.resolveFromPointer(");
      joutput_data(p->name);
      joutput(");\n");
      joutput_prefix();
      if (retptr) {
        // TODO in case of RETURNING POINTER
        joutput_data(p->returning);
        joutput(".setPointer (cob_unifunc.run");
      } else {
        // joutput_data (cb_field (current_prog->cb_return_code));
        joutput_data(current_prog->cb_return_code);
        joutput(".set (cob_unifunc.run");
      }
    } else {
      /* Static link */
      joutput_prefix();
      if (retptr) {
        // TODO in case of RETURNING POINTER
        joutput_data(p->returning);
        joutput(".setPointer (");
      } else {
        // joutput_data (cb_field (current_prog->cb_return_code));
        joutput_data(current_prog->cb_return_code);
        joutput(".set (");
      }

      if (system_call) {
        joutput("CobolSystemRoutine.%s", system_call);
      } else {
        joutput("new %s.run",
                cb_encode_program_id((char *)(CB_LITERAL(p->name)->data)));
      }
    }
  } else {
    /* Dynamic link */
    if (CB_LITERAL_P(p->name)) {
      callp = cb_encode_program_id((char *)(CB_LITERAL(p->name)->data));
      lookup_call(callp);
      if (cb_java_package_name) {
        joutput_line("call_%s = CobolResolve.resolve(\"%s\", \"%s\", call_%s);",
                     callp, cb_java_package_name,
                     (char *)(CB_LITERAL(p->name)->data), callp);
      } else {
        joutput_line("call_%s = CobolResolve.resolve(null, \"%s\", call_%s);",
                     callp, (char *)(CB_LITERAL(p->name)->data), callp);
      }
    } else {
      callp = NULL;
      joutput_prefix();
      joutput("  cob_unifunc = ");
      joutput("CobolResolve.resolve(");
      if (cb_java_package_name) {
        joutput("\"%s\", ", cb_java_package_name);
      } else {
        joutput("null, ");
      }
      joutput_param(p->name, -1);
      joutput(");\n");
    }
    joutput_prefix();
    if (retptr) {
      // TODO in case of RETURNING POINTER
      joutput("  ");
      joutput_data(p->returning);
      if (callp) {
        joutput(".setPointer (call_%s.run", callp);
      } else {
        joutput(".setPointer (cob_unifunc.run");
      }
    } else {
      // joutput_data (cb_field (current_prog->cb_return_code));
      joutput_data(current_prog->cb_return_code);
      if (callp) {
        joutput(".set (call_%s.run", callp);
      } else {
        joutput(".set (cob_unifunc.run");
      }
    }
  }

  /* Arguments */
  joutput(" (");
  for (l = p->args, n = 1; l; l = CB_CHAIN(l), n++) {
    x = CB_VALUE(l);
    switch (CB_PURPOSE_INT(l)) {
    case CB_CALL_BY_REFERENCE:
      if (CB_NUMERIC_LITERAL_P(x) || CB_BINARY_OP_P(x)) {
        joutput("content_%d.data", (int)n);
      } else if (CB_REFERENCE_P(x) && CB_FILE_P(cb_ref(x))) {
        joutput_param(cb_ref(x), -1);
      } else if (CB_CAST_P(x)) {
        joutput("&ptr_%d", (int)n);
      } else {
        int tmp_param_wrap_string_flag = param_wrap_string_flag;
        param_wrap_string_flag = 1;
        joutput_data(x);
        param_wrap_string_flag = tmp_param_wrap_string_flag;
      }
      break;
    case CB_CALL_BY_CONTENT:
      if (CB_TREE_TAG(x) != CB_TAG_INTRINSIC && x != cb_null) {
        if (CB_CAST_P(x)) {
          joutput("&ptr_%d", (int)n);
        } else {
          joutput("content_%d.data", (int)n);
        }
      } else {
        int tmp_param_wrap_string_flag = param_wrap_string_flag;
        param_wrap_string_flag = 1;
        joutput_data(x);
        param_wrap_string_flag = tmp_param_wrap_string_flag;
      }
      break;
    case CB_CALL_BY_VALUE:
      if (CB_TREE_TAG(x) != CB_TAG_INTRINSIC) {
        switch (CB_TREE_TAG(x)) {
        case CB_TAG_CAST:
          joutput_integer(x);
          break;
        case CB_TAG_LITERAL:
          if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
            joutput("CobolDataStorage.primitiveToDataStorage(%d)",
                    cb_get_int(x));
          } else {
            joutput("CobolDataStroage.primitiveToDataStorage(%d)",
                    CB_LITERAL(x)->data[0]);
          }
          break;
        default:
          /* RXWRXW
                                                  if (CB_TREE_CLASS (x) ==
             CB_CLASS_NUMERIC) { joutput_integer (x); } else { joutput ("*(");
                                                          joutput_data (x);
                                                          joutput (")");
                                                  }
          */
          f = cb_field(x);
          switch (f->usage) {
          case CB_USAGE_BINARY:
          case CB_USAGE_COMP_5:
          case CB_USAGE_COMP_X:
          /* RXWRXW */
          case CB_USAGE_PACKED:
          case CB_USAGE_DISPLAY:
            sizes = CB_SIZES_INT(l);
            if (sizes == CB_SIZE_AUTO) {
              if (f->pic->have_sign) {
                joutput("(unsigned ");
              } else {
                joutput("(");
              }
              if (f->usage == CB_USAGE_PACKED || f->usage == CB_USAGE_DISPLAY) {
                sizes = f->pic->digits - f->pic->scale;
              } else {
                sizes = f->size;
              }
              switch (sizes) {
              case 0:
                sizes = CB_SIZE_4;
                break;
              case 1:
                sizes = CB_SIZE_1;
                break;
              case 2:
                sizes = CB_SIZE_2;
                break;
              case 3:
                sizes = CB_SIZE_4;
                break;
              case 4:
                sizes = CB_SIZE_4;
                break;
              case 5:
                sizes = CB_SIZE_8;
                break;
              case 6:
                sizes = CB_SIZE_8;
                break;
              case 7:
                sizes = CB_SIZE_8;
                break;
              default:
                sizes = CB_SIZE_8;
                break;
              }
            } else {
              if (CB_SIZES_INT_UNSIGNED(l)) {
                joutput("(unsigned ");
              } else {
                joutput("(");
              }
            }
            switch (sizes) {
            case CB_SIZE_1:
              joutput("char");
              break;
            case CB_SIZE_2:
              joutput("short");
              break;
            case CB_SIZE_4:
              joutput("int");
              break;
            case CB_SIZE_8:
              joutput("long long");
              break;
            default:
              joutput("int");
              break;
            }
            joutput(")(");
            joutput_integer(x);
            joutput(")");
            break;
          case CB_USAGE_INDEX:
          case CB_USAGE_LENGTH:
          case CB_USAGE_POINTER:
          case CB_USAGE_PROGRAM_POINTER:
            joutput_integer(x);
            break;
          default:
            joutput("*(");
            joutput_data(x);
            joutput(")");
            break;
          }
          break;
        }
      } else {
        int tmp_param_wrap_string_flag = param_wrap_string_flag;
        param_wrap_string_flag = 1;
        joutput_data(x);
        param_wrap_string_flag = tmp_param_wrap_string_flag;
      }
      break;
    }
    if (CB_CHAIN(l)) {
      joutput(", ");
    }
  }
  if (!system_call) {
    if (cb_sticky_linkage || cb_flag_null_param) {
      for (n = 0; n < 4; n++) {
        joutput(", ");
        joutput("null");
      }
    }
  }
  joutput("));\n");
  if (p->returning) {
    if (!retptr) {
      /* suppress warnings */
      suppress_warn = 1;
      joutput_stmt(cb_build_move(current_prog->cb_return_code, p->returning),
                   JOUTPUT_STMT_DEFAULT);
      suppress_warn = 0;
#ifdef COB_NON_ALIGNED
    } else {
      // output_prefix ();
      // output ("memcpy (");
      // output_data (p->returning);
      // output (", &temptr, %d);\n", sizeof (void *));
#endif
    }
  }
  if (p->stmt2) {
    joutput_stmt(p->stmt2, JOUTPUT_STMT_DEFAULT);
  }
}

/*
 * GO TO
 */

static void joutput_goto_1(cb_tree x) {
  joutput_prefix();
  joutput("if(true) return Optional.of(contList[");
  joutput_label_variable(CB_LABEL(cb_ref(x)));
  joutput_line("]);\n");
}

static void joutput_goto(struct cb_goto *p) {

  if (p->depending) {
    int i = 1;
    joutput_prefix();
    joutput("switch ((int)");
    joutput_param(cb_build_cast_integer(p->depending), 0);
    joutput(")\n");
    joutput_indent("  {");
    cb_tree l;
    for (l = p->target; l; l = CB_CHAIN(l)) {
      joutput_indent_level -= 2;
      joutput_line("case %d:", i++);
      joutput_indent_level += 2;
      joutput_goto_1(CB_VALUE(l));
    }
    joutput_indent("  }");
  } else if (p->target == NULL) {
    needs_exit_prog = 1;
    if (cb_flag_implicit_init) {
      joutput_line(
          "if(true) return Optional.of(contList[contList.length - 1]);");
    } else {
      joutput_line("if (!CobolModule.isQueueEmpty()) {");
      joutput_line("  return Optional.of(contList[contList.length - 1]);");
      joutput_line("}");
    }
  } else if (p->target == cb_int1) {
    needs_exit_prog = 1;
    joutput_line("if(true) return Optional.of(contList[contList.length - 1]);");
  } else {
    joutput_goto_1(p->target);
  }
}

/*
 * PERFORM
 */

static void joutput_perform_call(struct cb_label *lb, struct cb_label *le) {
  if (lb == le) {
    joutput_line("/* PERFORM %s */", lb->name);
    joutput_prefix();
    joutput("CobolControl.perform(contList, ");
    joutput_label_variable(lb);
    joutput(").run();\n");
  } else {
    joutput_line("/* PERFORM %s THRU %s */", lb->name, le->name);
    joutput_line("CobolControl.performThrough(contList, ");
    joutput_label_variable(lb);
    joutput(", ");
    joutput_label_variable(le);
    joutput(").run();\n");
  }

  cb_id++;
}

static void joutput_perform_once(struct cb_perform *p) {
  if (p->body && CB_PAIR_P(p->body)) {
    joutput_perform_call(CB_LABEL(cb_ref(CB_PAIR_X(p->body))),
                         CB_LABEL(cb_ref(CB_PAIR_Y(p->body))));
  } else {
    joutput_stmt(p->body, JOUTPUT_STMT_DEFAULT);
  }
  if (p->cycle_label) {
    joutput_stmt(cb_ref(p->cycle_label), JOUTPUT_STMT_DEFAULT);
  }
}

static void joutput_perform_until(struct cb_perform *p, cb_tree l) {
  struct cb_perform_varying *v;
  cb_tree next;

  if (l == NULL) {
    /* Perform body at the end */
    joutput_perform_once(p);
    return;
  }

  v = CB_PERFORM_VARYING(CB_VALUE(l));
  next = CB_CHAIN(l);

  if (v->step) {
    joutput_prefix();
    joutput("for(;;");
    joutput_stmt(v->step, JOUTPUT_STMT_TRIM);
    joutput(")\n");
  } else {
    joutput_line("for (;;)");
  }
  joutput_indent("  {");

  if (next && CB_PERFORM_VARYING(CB_VALUE(next))->name) {
    joutput_move(CB_PERFORM_VARYING(CB_VALUE(next))->from,
                 CB_PERFORM_VARYING(CB_VALUE(next))->name);
  }

  if (p->test == CB_AFTER) {
    joutput_perform_until(p, next);
  }

  joutput_prefix();
  joutput("if (");

  ++index_read_flag;
  joutput_cond(v->until, 0);
  --index_read_flag;

  joutput(")\n");
  joutput_line("  break;");

  if (p->test == CB_BEFORE) {
    joutput_perform_until(p, next);
  }

  joutput_indent("  }");
}

static void joutput_perform(struct cb_perform *p) {
  struct cb_perform_varying *v;

  switch (p->type) {
  case CB_PERFORM_EXIT:
    if (CB_LABEL(p->data)->need_return) {
      // joutput_perform_exit (CB_LABEL (p->data));
    }
    break;
  case CB_PERFORM_ONCE:
    joutput_perform_once(p);
    break;
  case CB_PERFORM_TIMES:
    joutput_prefix();
    joutput("for (int n%d = ", loop_counter);
    joutput_param(cb_build_cast_integer(p->data), 0);
    joutput("; n%d > 0; n%d--)\n", loop_counter, loop_counter);
    loop_counter++;
    joutput_indent("  {");
    joutput_perform_once(p);
    joutput_indent("  }");
    break;
  case CB_PERFORM_UNTIL:
    v = CB_PERFORM_VARYING(CB_VALUE(p->varying));
    if (v->name) {
      joutput_move(v->from, v->name);
    }
    joutput_perform_until(p, p->varying);
    break;
  case CB_PERFORM_FOREVER:
    joutput_prefix();
    joutput("for (;;)\n");
    joutput_indent("  {");
    joutput_perform_once(p);
    joutput_indent("  }");
    break;
  }
  if (p->exit_label) {
    joutput_stmt(cb_ref(p->exit_label), JOUTPUT_STMT_DEFAULT);
  }
}

/*
 * SORT
 */

static void joutput_sort_init(struct cb_sort_init *p) {
  joutput_prefix();
  joutput("%s (", p->name);
  joutput_param(p->sort_file, 0);
  joutput(", ");
  joutput_param(p->nkeys, 1);
  joutput(", ");
  joutput_param(p->col, 2);
  joutput(", ");
  joutput_param(p->sort_return, 3);
  joutput(", ");
  joutput_param(p->file_status, 4);
  joutput(");\n");
}

static void joutput_sort_proc(struct cb_sort_proc *p) {
  struct cb_label *lb = CB_LABEL(cb_ref(CB_PAIR_X(p->body)));
  struct cb_label *le = CB_LABEL(cb_ref(CB_PAIR_Y(p->body)));

  if (lb == le) {
    joutput_line("/* PERFORM %s */", lb->name);
    joutput_prefix();
    joutput("CobolControl.perform(contList, ");
    joutput_label_variable(lb);
    joutput(").run();\n");
  } else {
    joutput_line("/* PERFORM %s THRU %s */", lb->name, le->name);
    joutput_prefix();
    joutput("CobolControl.performThrough(contList, ");
    joutput_label_variable(lb);
    joutput(", ");
    joutput_label_variable(le);
    joutput(").run();\n");
  }

  cb_id++;
}

static void joutput_file_return(struct cb_return *p) {
  joutput_prefix();
  joutput("CobolFileSort.performReturn(");
  joutput_param(p->proc.sort_file, 0);
  joutput(");\n");
}

static void joutput_file_release(struct cb_release *p) {
  joutput_prefix();
  joutput("CobolFileSort.performRelease(");
  joutput_param(p->proc.sort_file, 0);
  joutput(");\n");
}

static void joutput_sort_finish(struct cb_sort_finish *p) {
  if (p->sort_file != NULL) {
    joutput_prefix();
    joutput("CobolFileSort.sortClose (");
    joutput_param(p->sort_file, 0);
    joutput(");\n");
  }
  if (cb_enable_sort_status_register &&
      !current_program->flag_sort_status_used) {
    joutput("    if (");
    joutput_param(p->sort_return, 0);
    joutput(".intValue() != 0)\n");
    joutput("      {\n");
    joutput("        CobolUtil.runtimeError (");
    joutput("\"SORT-STATUS is set to \" + ");
    joutput_param(p->sort_return, 0);
    joutput(".intValue() + \".\");\n");
    joutput("        CobolStopRunException.stopRunAndThrow (1);\n");
    joutput("      }\n");
  }
}

static void joutput_file_error(struct cb_file *pfile) {
  struct cb_file *fl;
  cb_tree l;

  for (l = current_prog->local_file_list; l; l = CB_CHAIN(l)) {
    fl = CB_FILE(CB_VALUE(l));
    if (!strcmp(pfile->name, fl->name)) {
      joutput_perform_call(fl->handler, fl->handler);
      return;
    }
  }
  for (l = current_prog->global_file_list; l; l = CB_CHAIN(l)) {
    fl = CB_FILE(CB_VALUE(l));
    if (!strcmp(pfile->name, fl->name)) {
      if (fl->handler_prog == current_prog) {
        joutput_perform_call(fl->handler, fl->handler);
      } else {
        if (cb_flag_traceall) {
          joutput_line("CobolUtil.resetTrace();");
        }
        joutput_line("%s_ (%d);", fl->handler_prog->program_id,
                     fl->handler->id);
        if (cb_flag_traceall) {
          joutput_line("CobolUtil.readyTrace();");
        }
      }
      return;
    }
  }
  joutput_perform_call(pfile->handler, pfile->handler);
}

/*
 * Output statement
 */

static void joutput_ferror_stmt(struct cb_statement *p, int code) {
  joutput_line("if (CobolRuntimeException.code != 0)");
  joutput_indent("  {");
  if (p->handler1) {
    if ((code & 0x00ff) == 0) {
      joutput_line("if ((CobolRuntimeException.code & 0xff00) == 0x%04x)",
                   code);
    } else {
      joutput_line("if (CobolRuntimeException.code == 0x%04x)", code);
    }
    joutput_indent("  {");
    joutput_stmt(p->handler1, JOUTPUT_STMT_DEFAULT);
    joutput_indent("  }");
    joutput_line("else");
    joutput_indent("  {");
  }
  joutput_file_error(CB_FILE(p->file));
  joutput_indent("  }");
  if (p->handler1) {
    joutput_indent("  }");
  }
  if (p->handler2 || p->handler3) {
    joutput_line("else");
    joutput_indent("  {");
    if (p->handler3) {
      joutput_stmt(p->handler3, JOUTPUT_STMT_DEFAULT);
    }
    if (p->handler2) {
      joutput_stmt(p->handler2, JOUTPUT_STMT_DEFAULT);
    }
    joutput_indent("  }");
  }
}

// static void
// joutput_cobol_comment_before_identification_division(struct comment_info *p)
// {
//   joutput_line("// %s:%d: %s", p->file, p->line - 1, p->comment);
// }
//
// static void joutput_cobol_comment(struct comment_info *p) {
//   joutput_line("// %s:%d: %s", p->file, p->line, p->comment);
// }

static void joutput_stmt(cb_tree x, enum joutput_stmt_type output_type) {
  struct cb_statement *p;
  struct cb_label *lp;
  struct cb_assign *ap;
  struct cb_if *ip;
#ifdef COB_NON_ALIGNED
  struct cb_cast *cp;
#endif
  int code;
  int putParen = 0;

  stack_id = 0;
  if (x == NULL) {
    joutput_line(";");
    return;
  }
#ifndef __GNUC__
  if (inside_check != 0) {
    if (inside_stack[inside_check - 1] != 0) {
      inside_stack[inside_check - 1] = 0;
      joutput(",\n");
    }
  }
#endif
  switch (CB_TREE_TAG(x)) {
  case CB_TAG_STATEMENT:
    p = CB_STATEMENT(x);
    /* Output source location as a comment */
    if (p->name) {
      /* Output comments in a COBOL source file */
      /*if (!cb_flag_no_cobol_comment &&
          strcmp((char *)x->source_file, translating_source_file) == 0) {
        char *file_name_of_last_comment = NULL;
        for (; comment_info_cursor;
             comment_info_cursor = comment_info_cursor->next) {
          if (!comment_info_cursor->is_base_cobol_file) {
            continue;
          }
          if (comment_info_cursor->position_in_source_code <
              POSITION_AFTER_PROCEDURE_DIVISION) {
            continue;
          }
          if (comment_info_cursor->line <= procedure_division_line_number) {
            continue;
          }
          if (file_name_of_last_comment != NULL &&
              strcmp(file_name_of_last_comment, comment_info_cursor->file) !=
                  0) {
            break;
          }
          if (strcmp(comment_info_cursor->file, (char *)x->source_file) == 0 &&
              comment_info_cursor->line > x->source_line) {
            break;
          }
          joutput_cobol_comment(comment_info_cursor);
          file_name_of_last_comment = comment_info_cursor->file;
        }
      }*/
      joutput_line("/* %s:%d: %s */", x->source_file, x->source_line, p->name);
    }
    /* Output source location as a code */
    if (x->source_file && last_line != x->source_line) {
      if (cb_flag_source_location) {
        joutput_prefix();
        joutput("CobolUtil.setLocation (\"%s\", \"%s\", %d, ",
                excp_current_program_id, x->source_file, x->source_line);
        if (excp_current_section) {
          joutput("\"%s\", ", excp_current_section);
        } else {
          joutput("null, ");
        }
        if (excp_current_paragraph) {
          joutput("\"%s\", ", excp_current_paragraph);
        } else {
          joutput("null, ");
        }
        if (p->name) {
          joutput("\"%s\");\n", p->name);
        } else {
          joutput("null);\n");
        }
      }
      last_line = x->source_line;
    }

    if (p->handler1 || p->handler2 ||
        (p->file && CB_EXCEPTION_ENABLE(COB_EC_I_O))) {

      joutput_line("CobolRuntimeException.code = 0;");
    }

    if (cb_enable_zero_division_error && p->name &&
        ((strcmp(p->name, "DIVIDE") == 0) ||
         (strcmp(p->name, "COMPUTE") == 0)) &&
        (!p->handler1 && !p->handler2)) {
      joutput_line("CobolUtil.cobErrorOnExitFlag = true;");
    }

    if (p->null_check) {
      joutput_stmt(p->null_check, output_type);
    }

    if (p->body) {
      joutput_stmt(p->body, output_type);
    }

    if (p->handler1 || p->handler2 ||
        (p->file && CB_EXCEPTION_ENABLE(COB_EC_I_O))) {
      code = CB_EXCEPTION_CODE(p->handler_id);
      if (p->file) {
        joutput_ferror_stmt(p, code);
      } else {
        if (p->handler1) {
          if ((code & 0x00ff) == 0) {
            joutput_line("if ((CobolRuntimeException.code & 0xff00) == 0x%04x)",
                         code);
          } else {
            joutput_line("if (CobolRuntimeException.code == 0x%04x)", code);
          }
          joutput_indent("  {");
          joutput_stmt(p->handler1, output_type);
          joutput_indent("  }");
          if (p->handler2) {
            joutput_line("else");
          }
        }
        if (p->handler2) {
          if (p->handler1 == NULL) {
            joutput_line("if (CobolRuntimeException.code == 0)");
          }
          joutput_indent("  {");
          joutput_stmt(p->handler2, output_type);
          joutput_indent("  }");
        }
      }
    }
    break;
  case CB_TAG_LABEL:
    lp = CB_LABEL(x);
    joutput_newline();

    // the end of the previous label.
    if (flag_execution_end == EXECUTION_NORMAL) {
      joutput_prefix();
      joutput("return Optional.of(contList[");
      joutput_label_variable_by_value(++control_counter);
      joutput("]);\n");
    } else {
      joutput_line("return Optional.of(CobolControl.pure());");
    }
    joutput_indent_level -= 2;
    joutput_line("}");
    joutput_indent_level -= 2;
    joutput_line("},");

    // output comment
    if (lp->is_section) {
      if (strcmp((const char *)(lp->name), "MAIN SECTION")) {
        joutput_line("/* %s SECTION */", lp->name);
      } else {
        joutput_line("/* %s */", lp->name);
      }
      excp_current_section = (const char *)lp->name;
      excp_current_paragraph = NULL;
    } else {
      if (lp->is_entry) {
        joutput_line("/* Entry %s */", lp->orig_name);
      } else {
        joutput_line("/* %s */", lp->name);
      }
      excp_current_paragraph = (const char *)lp->name;
      // if (!lp->need_begin) {
      //	joutput_newline ();
      // }
    }

    // the start of the label
    joutput_prefix();
    joutput("new CobolControl(");
    if (flag_execution_begin == EXECUTION_ERROR_HANDLER) {
      joutput_label_variable_by_value(control_counter + 1);
    } else {
      joutput_label_variable_by_value(control_counter);
    }

    if (lp->is_section) {
      joutput(", CobolControl.LabelType.section) {");
    } else {
      joutput(", CobolControl.LabelType.label) {");
    }
    joutput_newline();

    joutput_indent_level += 2;
    joutput_line(
        "public Optional<CobolControl> run() throws CobolRuntimeException, "
        "CobolGoBackException, CobolStopRunException {");
    joutput_indent_level += 2;

    if (cb_flag_trace) {
      if (lp->is_section) {
        if (strcmp((const char *)(lp->name), "MAIN SECTION")) {
          joutput_line("System.err.println(\"PROGRAM-ID: %s: %s SECTION\");",
                       excp_current_program_id, lp->orig_name);
        } else {
          joutput_line("System.err.println(\"PROGRAM-ID: %s: %s\");",
                       excp_current_program_id, lp->orig_name);
        }
      } else if (lp->is_entry) {
        joutput_line("System.err.println(\"PROGRAM-ID: %s: ENTRY %s\");",
                     excp_current_program_id, lp->orig_name);
      } else {
        joutput_line("System.err.println(\"PROGRAM-ID: %s: %s\");",
                     excp_current_program_id, lp->orig_name);
      }
      joutput_line("System.err.flush();");
    }
    break;
  case CB_TAG_FUNCALL:
    if (output_type != JOUTPUT_STMT_TRIM) {
      joutput_prefix();
    }
    joutput_funcall(x);
    if (output_type != JOUTPUT_STMT_TRIM) {
      joutput(";\n");
    }
    break;
  case CB_TAG_ASSIGN:
    ap = CB_ASSIGN(x);
#ifdef COB_NON_ALIGNED
    /* Nonaligned */
    if (CB_TREE_CLASS(ap->var) == CB_CLASS_POINTER ||
        CB_TREE_CLASS(ap->val) == CB_CLASS_POINTER) {
      /* Pointer assignment */
      joutput_indent("{");
      joutput_line("void *temp_ptr;");

      /* temp_ptr = source address; */
      joutput_prefix();
      if (ap->val == cb_null || ap->val == cb_zero) {
        /* MOVE NULL ... */
        joutput("temp_ptr = 0;\n");
      } else if (CB_TREE_TAG(ap->val) == CB_TAG_CAST) {
        /* MOVE ADDRESS OF val ... */
        cp = CB_CAST(ap->val);
        joutput("temp_ptr = ");
        switch (cp->type) {
        case CB_CAST_ADDRESS:
          joutput_data(cp->val);
          break;
        case CB_CAST_PROGRAM_POINTER:
          joutput_func_1("CobolResolve.resolveToPointer", ap->val);
          break;
        default:
          fprintf(stderr, "Unexpected cast type %d\n", cp->type);
          ABORT();
        }
        joutput(";\n");
      } else {
        /* MOVE val ... */
        joutput("LIBCOB.memcpy(&temp_ptr, ");
        joutput_data(ap->val);
        joutput(", sizeof(temp_ptr));\n");
      }

      /* destination address = temp_ptr; */
      joutput_prefix();
      if (CB_TREE_TAG(ap->var) == CB_TAG_CAST) {
        /* SET ADDRESS OF var ... */
        cp = CB_CAST(ap->var);
        if (cp->type != CB_CAST_ADDRESS) {
          fprintf(stderr, "Unexpected tree type %d\n", cp->type);
          ABORT();
        }
        joutput_data(cp->val);
        joutput(" = temp_ptr;\n");
      } else {
        /* MOVE ... TO var */
        joutput("LIBCOB.memcpy(");
        joutput_data(ap->var);
        joutput(", &temp_ptr, sizeof(temp_ptr));\n");
      }

      joutput_indent("}");
    } else {
      /* Numeric assignment */
      joutput_prefix();

      int tmp_flag = integer_reference_flag;
      integer_reference_flag = 1;
      joutput_integer(ap->var);
      integer_reference_flag = tmp_flag;
      joutput(".set(");
      ++index_read_flag;
      joutput_integer(ap->val);
      --index_read_flag;
      if (output_type == JOUTPUT_STMT_TRIM) {
        joutput(")\n");
      } else {
        joutput(");\n");
      }
    }
#else  /* Nonaligned */
    joutput_prefix();

    int tmp_flag = integer_reference_flag;
    integer_reference_flag = 1;
    joutput_integer(ap->var);
    integer_reference_flag = tmp_flag;

    joutput(".set(");

    struct cb_field *f = cb_field(ap->var);
    if (f->usage == CB_USAGE_BINARY || f->usage == CB_USAGE_COMP_5 ||
        f->usage == CB_USAGE_INDEX) {
      if (f->size == 1) {
        joutput("(byte)");
      } else if (f->size == 2) {
        joutput("(short)");
      } else if (f->size == 4) {
        joutput("(int)");
      } else if (f->size == 8) {
        joutput("(long)");
      }
    }

    ++index_read_flag;
    joutput_integer(ap->val);
    --index_read_flag;
    if (output_type == JOUTPUT_STMT_TRIM) {
      joutput(")\n");
    } else {
      joutput(");\n");
    }
#endif /* Nonaligned */
    break;
  case CB_TAG_INITIALIZE:
    joutput_initialize(CB_INITIALIZE(x));
    break;
  case CB_TAG_SEARCH:
    joutput_search(CB_SEARCH(x));
    break;
  case CB_TAG_CALL:
    joutput_call(CB_CALL(x));
    break;
  case CB_TAG_GOTO:
    joutput_goto(CB_GOTO(x));
    break;
  case CB_TAG_JAVA_CONTINUE:
    joutput_line("if(true) continue;");
    break;
  case CB_TAG_JAVA_BREAK:
    joutput_line("if(true) break;");
    break;
  case CB_TAG_IF:
    ip = CB_IF(x);
    joutput_prefix();
    joutput("if (");

    index_read_flag++;
    joutput_cond(ip->test, 0);
    index_read_flag--;

    joutput(")\n");
    if (ip->stmt1) {
      joutput_indent_level += 2;
      joutput_stmt(ip->stmt1, output_type);
      joutput_indent_level -= 2;
    } else {
      joutput_line("  /* nothing */;");
    }
    if (ip->stmt2) {
      joutput_line("else");
      joutput_indent_level += 2;
      joutput_stmt(ip->stmt2, output_type);
      joutput_indent_level -= 2;
    }
    break;
  case CB_TAG_PERFORM:
    joutput_perform(CB_PERFORM(x));
    break;
  case CB_TAG_SORT_INIT:
    joutput_sort_init(CB_SORT_INIT(x));
    break;
  case CB_TAG_SORT_PROC:
    joutput_sort_proc(CB_SORT_PROC(x));
    break;
  case CB_TAG_RETURN:
    joutput_file_return(CB_RETURN(x));
    break;
  case CB_TAG_RELEASE:
    joutput_file_release(CB_RELEASE(x));
    break;
  case CB_TAG_SORT_FINISH:
    joutput_sort_finish(CB_SORT_FINISH(x));
    break;
  case CB_TAG_CONTINUE:
    joutput_prefix();
    joutput(";\n");
    break;
  case CB_TAG_LIST:
    if (x && CB_TREE_TAG(CB_VALUE(x)) == CB_TAG_PERFORM) {
      putParen = 0;
    } else {
      putParen = 1;
      joutput_indent("{");
    }

    for (; x; x = CB_CHAIN(x)) {
      joutput_stmt(CB_VALUE(x), output_type);
    }

    if (putParen) {
      joutput_indent("}");
    }

    break;
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    ABORT();
  }
}

/*
 * File definition
 */

static int joutput_file_allocation(struct cb_file *f) {

  if (f->global) {
    joutput_line("/* Global file %s */", f->name);
  } else {
    joutput_line("/* File %s */", f->name);
  }
  /* Output RELATIVE/RECORD KEY's */
  if (f->organization == COB_ORG_RELATIVE ||
      f->organization == COB_ORG_INDEXED) {
    if (f->global) {
      joutput_line("private CobolFileKey[]\t%s%s = null;", CB_PREFIX_KEYS,
                   f->cname);
    } else {
      joutput_line("private CobolFileKey[]\t%s%s = null;", CB_PREFIX_KEYS,
                   f->cname);
    }
  }
  if (f->global) {
    joutput_line("private CobolFile\t\t%s%s = null;", CB_PREFIX_FILE, f->cname);
    joutput_line("private byte[]\t%s%s_status = new byte[4];", CB_PREFIX_FILE,
                 f->cname);
  } else {
    joutput_line("private CobolFile\t\t%s%s = null;", CB_PREFIX_FILE, f->cname);
    joutput_line("private byte[]\t%s%s_status = new byte[4];", CB_PREFIX_FILE,
                 f->cname);
  }
  if (f->linage) {
    return 1;
  }
  return 0;
}

static void joutput_file_initialization(struct cb_file *f) {
  int nkeys = 1;
  struct cb_key_component *key_component;
  struct cb_alt_key *l;

  /* Output RELATIVE/RECORD KEY's */
  if (f->organization == COB_ORG_RELATIVE ||
      f->organization == COB_ORG_INDEXED) {
    for (l = f->alt_key_list; l; l = l->next) {
      nkeys++;
    }
    joutput_line("if (%s%s == null)", CB_PREFIX_KEYS, f->cname);
    joutput_indent("{");
    joutput_line("%s%s = new CobolFileKey[%d];", CB_PREFIX_KEYS, f->cname,
                 nkeys);
    joutput_line("for (int i=0; i<%d; ++i)", nkeys);
    joutput_line("  %s%s[i] = new CobolFileKey();", CB_PREFIX_KEYS, f->cname);
    joutput_indent("}");
    nkeys = 1;
    joutput_prefix();
    joutput("%s%s[0].setField(", CB_PREFIX_KEYS, f->cname);
    joutput_param(f->key, -1);
    joutput(");\n");
    joutput_prefix();
    joutput("%s%s[0].setFlag(0);\n", CB_PREFIX_KEYS, f->cname);
    joutput_prefix();
    int i_keycomp;
    if (f->key) {
      if (f->component_list != NULL) {
        joutput("%s%s[0].setOffset(%d);\n", CB_PREFIX_KEYS, f->cname, -1);
        for (key_component = f->component_list, i_keycomp = 0;
             key_component != NULL;
             key_component = key_component->next, ++i_keycomp) {
          joutput_prefix();
          joutput("%s%s[0].getComponent()[%d].field = ", CB_PREFIX_KEYS,
                  f->cname, i_keycomp);
          joutput_param(key_component->component, -1);
          joutput(";\n");
          joutput_prefix();
          joutput("%s%s[0].getComponent()[%d].rb = %d;\n", CB_PREFIX_KEYS,
                  f->cname, i_keycomp,
                  cb_field(key_component->component)->offset);
        }
        joutput_line("%s%s[0].setCountComponents(%d);", CB_PREFIX_KEYS,
                     f->cname, i_keycomp);
      } else {
        joutput("%s%s[0].setOffset(%d);\n", CB_PREFIX_KEYS, f->cname,
                cb_field(f->key)->offset);
      }
    } else {
      joutput("%s%s[0].setOffset(0);\n", CB_PREFIX_KEYS, f->cname);
    }
    for (l = f->alt_key_list; l; l = l->next) {
      joutput_prefix();
      joutput("%s%s[%d].setField(", CB_PREFIX_KEYS, f->cname, nkeys);
      joutput_param(l->key, -1);
      joutput(");\n");
      joutput_prefix();
      joutput("%s%s[%d].setFlag(%d);\n", CB_PREFIX_KEYS, f->cname, nkeys,
              l->duplicates);
      /* BCS/JR PATCH split-keys May 2012: */
      joutput_prefix();
      joutput("%s%s[%d].setOffset(%d);\n", CB_PREFIX_KEYS, f->cname, nkeys,
              (l->component_list == NULL) ? cb_field(l->key)->offset : -1);
      if (l->component_list != NULL) {

        int num_key_components = 0;
        for (key_component = l->component_list; key_component != NULL;
             key_component = key_component->next) {
          num_key_components++;
        }
        joutput_line("{");
        joutput_indent_level += 2;
        joutput_line("KeyComponent[] keyComponents = new KeyComponent[%d];",
                     num_key_components);
        joutput_line("for (int i = 0; i < %d; i++) {", num_key_components);
        joutput_line("  keyComponents[i] = new KeyComponent();");
        joutput_line("}");
        joutput_line("%s%s[%d].setComponent(keyComponents);", CB_PREFIX_KEYS,
                     f->cname, nkeys);
        joutput_indent_level -= 2;
        joutput_line("}");

        for (key_component = l->component_list, i_keycomp = 0;
             key_component != NULL;
             key_component = key_component->next, ++i_keycomp) {
          joutput_prefix();
          joutput("%s%s[%d].getComponent()[%d].field = ", CB_PREFIX_KEYS,
                  f->cname, nkeys, i_keycomp);
          joutput_param(key_component->component, -1);
          joutput(";\n");
          joutput_prefix();
          joutput("%s%s[%d].getComponent()[%d].rb = %d;\n", CB_PREFIX_KEYS,
                  f->cname, nkeys, i_keycomp,
                  cb_field(key_component->component)->offset);
        }
        joutput_prefix();
        joutput("%s%s[%d].setCountComponents(%d);\n", CB_PREFIX_KEYS, f->cname,
                nkeys, i_keycomp);
      }
      nkeys++;
    }
  }

  /*if (!(f->external && !f->file_status)) {
          joutput_line ("byte byte_%s%s = new byte[4];");
          joutput_line ("for (int i_%s%s=0; i<2; ++i) {");
          joutput_line ("  byte_%s%s[i] = '0;'");
          joutput_line ("}");
  }*/
  if (f->external) {
    joutput_line("%s%s = CobolFile.getExternalFile(\"%s\");", CB_PREFIX_FILE,
                 f->cname, f->cname);
    joutput_line("if(%s%s == null) {", CB_PREFIX_FILE, f->cname);
    joutput_indent_level += 2;
  }

  joutput_line("%s%s = CobolFileFactory.makeCobolFileInstance(", CB_PREFIX_FILE,
               f->cname);
  joutput_line("/* select_name = */ \"%s\",", f->name);
  if (f->external && !f->file_status) {
    joutput_line("/* file_status = */ CobolFile.getExternalFileStatus "
                 "(\"%s\"),",
                 f->cname);
  } else {
    joutput_line("/* file_status = */ %s%s_status,", CB_PREFIX_FILE, f->cname);
  }
  joutput_prefix();
  joutput("/* assign = */ ");
  if (f->special) {
    joutput("null");
  } else {
    joutput_param(f->assign, -1);
  }
  joutput(",\n");
  joutput_prefix();
  joutput("/* record = */ ");
  joutput_param(CB_TREE(f->record), -1);
  joutput(",\n");
  joutput_prefix();
  joutput("/* record_size = */ ");
  if (f->record_depending) {
    joutput_param(f->record_depending, -1);
  } else {
    joutput("null");
  }
  joutput(",\n");
  joutput_line("/* record_min = */ %d,", f->record_min);
  joutput_line("/* record_max = */ %d,", f->record_max);
  if (f->organization == COB_ORG_RELATIVE ||
      f->organization == COB_ORG_INDEXED) {
    joutput_line("/* nkeys = */ %d,", nkeys);
    joutput_line("/* keys = */ %s%s,", CB_PREFIX_KEYS, f->cname);
  } else {
    joutput_line("/* nkeys = */ 0,");
    joutput_line("/* keys = */ null,");
  }
  // joutput_line ("/* file = */ null,", CB_PREFIX_FILE, f->cname);

  joutput_line("/* organization = */ (char)%d,", f->organization);
  joutput_line("/* access_mode = */ (char)%d,", f->access_mode);
  joutput_line("/* lock_mode = */ (char)%d,", f->lock_mode);
  joutput_line("/* open_mode = */ (char)0,");
  joutput_line("/* flag_optional = */ %s,", f->optional ? "true" : "false");
  joutput_line("/* last_open_mode = */ (char)0,");
  joutput_line("/* special = */ (char)%d,", f->special);
  joutput_line("/* flag_nonexistent = */ false,");
  joutput_line("/* flag_end_of_file = */ false,");
  joutput_line("/* flag_begin_of_file = */ false,");
  joutput_line("/* flag_first_read = */ (char)0,");
  joutput_line("/* flag_read_done = */ false,");
  joutput_line("/* flag_select_features = */ (char)%d,",
               ((f->file_status ? COB_SELECT_FILE_STATUS : 0) |
                (f->linage ? COB_SELECT_LINAGE : 0) |
                (f->external_assign ? COB_SELECT_EXTERNAL : 0)));
  joutput_line("/* flag_needs_nl = */ false,");
  joutput_line("/* flag_needs_top = */ false,");
  joutput_line("/* file_version = */ (char)%d", COB_FILE_VERSION);
  joutput_line(");");

  if (f->external) {
    joutput_line("CobolFile.putExternalFile(\"%s\", %s%s);", f->cname,
                 CB_PREFIX_FILE, f->cname);
    joutput_indent_level -= 2;
    joutput_line("}");
  }

  if (f->linage) {
    joutput_line("%s%s.setLinorkeyptr(new Linage());", CB_PREFIX_FILE,
                 f->cname);
    joutput_line("lingptr = (Linage)(%s%s.getLinorkeyptr());", CB_PREFIX_FILE,
                 f->cname);
    joutput_prefix();
    joutput("lingptr.setLinage(");
    joutput_param(f->linage, -1);
    joutput(");\n");
    joutput_prefix();
    joutput("lingptr.setLinageCtr(");
    joutput_param(f->linage_ctr, -1);
    joutput(");\n");
    if (f->latfoot) {
      joutput_prefix();
      joutput("lingptr.setLatfoot(");
      joutput_param(f->latfoot, -1);
      joutput(");\n");
    } else {
      joutput_line("lingptr.setLatfoot(null);");
    }
    if (f->lattop) {
      joutput_prefix();
      joutput("lingptr.setLattop(");
      joutput_param(f->lattop, -1);
      joutput(");\n");
    } else {
      joutput_line("lingptr.setLattop(null);");
    }
    if (f->latbot) {
      joutput_prefix();
      joutput("lingptr.setLatbot(");
      joutput_param(f->latbot, -1);
      joutput(");\n");
    } else {
      joutput_line("lingptr.setLatbot(null);");
    }
    joutput_line("lingptr.setLinLines(0);");
    joutput_line("lingptr.setLinFoot(0);");
    joutput_line("lingptr.setLinTop(0);");
    joutput_line("lingptr.setLinBot(0);");
  }
}

/*
 * Alphabet-name
 */

static int literal_value(cb_tree x) {
  if (x == cb_space) {
    return ' ';
  } else if (x == cb_zero) {
    return '0';
  } else if (x == cb_quote) {
    return '"';
  } else if (x == cb_norm_low) {
    return 0;
  } else if (x == cb_norm_high) {
    return 255;
  } else if (x == cb_null) {
    return 0;
  } else if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
    return cb_get_int(x) - 1;
  } else {
    return CB_LITERAL(x)->data[0];
  }
}

static void joutput_initial_values(struct cb_field *p) {
  cb_tree def;

  def = cb_auto_initialize ? cb_true : NULL;
  for (; p; p = p->sister) {
    cb_tree x = cb_build_field_reference(p, NULL);
    if (p->flag_item_based) {
      continue;
    }
    /* For special registers */
    if (p->flag_no_init && !p->count) {
      continue;
    }
    int tmp_flag = integer_reference_flag;
    integer_reference_flag = 1;
    joutput_stmt(cb_build_initialize(x, cb_true, NULL, def, 0),
                 JOUTPUT_STMT_DEFAULT);
    integer_reference_flag = tmp_flag;
  }
}

struct call_parameter_list {
  struct call_parameter_list *next;
  struct cb_field *field;
  cb_tree x;
};
struct call_parameter_list *call_parameter_cache = NULL;

static void write_json_info(struct cb_program *prog) {
  if (!cb_flag_info_json) {
    return;
  }

  cb_tree l;
  cb_tree parameter_list = prog->parameter_list;
  char json_file_path[COB_MEDIUM_BUFF];
  snprintf(json_file_path, COB_MEDIUM_BUFF, "%s/info_%s.json", cb_info_json_dir,
           prog->program_id);

  FILE *fp = fopen(json_file_path, "w");
  if (!fp) {
    fprintf(stderr, "Warning: failed to open a json file %s\n", json_file_path);
    return;
  }

  fprintf(fp, "{\n");
  fprintf(fp, "  \"opensourcecobol4j_version\": \"%s\",\n", PACKAGE_VERSION);
  fprintf(fp, "  \"program_id\": \"%s\",\n", prog->program_id);
  fprintf(fp, "  \"procedure_division_using_parameters\": [\n");

  for (l = parameter_list; l; l = CB_CHAIN(l)) {
    struct cb_field *arg_field = cb_field(CB_VALUE(l));
    int type = cb_tree_type(CB_TREE(arg_field));
    fprintf(fp, "    {\n");
    fprintf(fp, "      \"variable_name\": \"%s\",\n", arg_field->name);
    fprintf(fp, "      \"java_type\": ");
    if (type & COB_TYPE_NUMERIC) {
      if (arg_field->pic->scale > 0) {
        fprintf(fp, "\"double\"");
      } else {
        fprintf(fp, "\"int\"");
      }
    } else {
      fprintf(fp, "\"String\"");
    }
    fprintf(fp, "\n");
    fprintf(fp, "    }");
    if (CB_CHAIN(l)) {
      fprintf(fp, ",");
    }
    fprintf(fp, "\n");
  }
  fprintf(fp, "  ]\n");
  fprintf(fp, "}\n");

  fclose(fp);
}

static void joutput_java_entrypoint(struct cb_program *prog,
                                    cb_tree parameter_list) {
  cb_tree l;
  char arg_field_name[COB_SMALL_BUFF];

  joutput_prefix();
  joutput("public CobolResultSet execute (");

  for (l = parameter_list; l; l = CB_CHAIN(l)) {
    struct cb_field *arg_field = cb_field(CB_VALUE(l));
    int type = cb_tree_type(CB_TREE(arg_field));
    get_java_identifier_helper(arg_field, arg_field_name);
    if (type & COB_TYPE_NUMERIC) {
      if (arg_field->pic->scale > 0) {
        joutput("double");
      } else {
        joutput("int");
      }
    } else {
      joutput("String");
    }
    joutput(" %s", arg_field_name);
    if (CB_CHAIN(l)) {
      joutput(", ");
    }

    struct call_parameter_list *call_parameter =
        malloc(sizeof(struct call_parameter_list));
    call_parameter->next = call_parameter_cache;
    call_parameter->field = arg_field;
    call_parameter->x = CB_VALUE(l);
    call_parameter_cache = call_parameter;
  }

  joutput(") {\n");
  joutput_indent_level += 2;

  for (l = parameter_list; l; l = CB_CHAIN(l)) {
    struct cb_field *arg_field = cb_field(CB_VALUE(l));
    char *field_name = get_java_identifier_field(arg_field);
    char *base_name = get_java_identifier_base(arg_field);
    get_java_identifier_helper(arg_field, arg_field_name);
    joutput_line("this.%s.setDataStorage(new CobolDataStorage(%d));",
                 field_name, arg_field->size);
    joutput_line("this.%s.moveFrom(%s);", field_name, arg_field_name);
    joutput_line("this.%s = this.%s.getDataStorage();", base_name, field_name);
    free(field_name);
    free(base_name);
  }

  joutput_line("int returnCode = run_module(0);", prog->program_id);

  joutput_prefix();
  joutput("return new CobolResultSet(returnCode");
  if (parameter_list) {
    joutput(",\n");
    joutput_indent_level += 2;
    for (l = parameter_list; l; l = CB_CHAIN(l)) {
      struct cb_field *arg_field = cb_field(CB_VALUE(l));
      char *field_name = get_java_identifier_field(arg_field);
      int type = cb_tree_type(CB_TREE(arg_field));
      joutput_prefix();
      const char *constructor;
      const char *getter;
      if (type & COB_TYPE_NUMERIC) {
        if (arg_field->pic->scale > 0) {
          constructor = "CobolResultDouble";
          getter = "getDouble";
        } else {
          constructor = "CobolResultInt";
          getter = "getInt";
        }
      } else {
        constructor = "CobolResultString";
        getter = "getString";
      }
      joutput("new %s(%s.%s())", constructor, field_name, getter);
      joutput(CB_CHAIN(l) ? ",\n" : "\n");
      free(field_name);
    }
    joutput_indent_level -= 2;
    joutput_prefix();
  }

  joutput(");\n");
  joutput_indent_level -= 2;
  joutput_line("}\n");
}

static void joutput_internal_function(struct cb_program *prog,
                                      cb_tree parameter_list) {
  cb_tree l;
  struct cb_field *f;
  struct cb_file *fl;
  char *p;
  int i;
  // int			n;
  int parmnum = 0;
  // int			seen = 0;
  // int			anyseen;
  char name[COB_MINI_BUFF];

  /* Program function */
  // output ("static int\n%s_ (const int entry", prog->program_id);
  // if (!prog->flag_chained) {
  //	for (l = parameter_list; l; l = CB_CHAIN (l)) {
  //		output (", unsigned char *%s%d",
  //			CB_PREFIX_BASE, cb_field (CB_VALUE (l))->id);
  //		parmnum++;
  //	}
  // }
  // output (")\n");
  // output_indent ("{");

  joutput_line("public int %s_ (int entry, CobolDataStorage ...argStorages) {",
               prog->program_id);
  joutput_indent_level += 2;

  joutput_line("this.entry = entry;");
  if (!prog->flag_chained) {
    int k;
    for (k = 0, l = parameter_list; l; l = CB_CHAIN(l), ++k) {
      char *base_name = get_java_identifier_base(cb_field(CB_VALUE(l)));
      joutput_line(
          "this.%s = %d < argStorages.length ? argStorages[%d] : null;",
          base_name, k, k);
      free(base_name);
      parmnum++;
    }
  }
  joutput_line("return this.run_module(entry);");
  joutput_indent_level -= 2;
  joutput_line("}");
  joutput("\n");

  joutput_line("int run_module (int entry) {");
  joutput_indent_level += 2;
  // if (!prog->flag_chained) {
  //	for (l = parameter_list; l; l = CB_CHAIN (l)) {
  //		joutput_line ("if (fields.length > %d) {", parmnum);
  //		joutput_line ("  %s%d = fields[%d];",
  //			CB_PREFIX_FIELD, cb_field (CB_VALUE (l))->id, parmnum);
  //		joutput_line ("} else {");
  //		joutput_line ("  %s%d = null;",
  //			CB_PREFIX_FIELD, cb_field (CB_VALUE (l))->id);
  //		joutput_line ("}");
  //		parmnum++;
  //	}
  // }
  // joutput("\n");

  /* Local variables */
  // output_line ("/* Local variables */");
  // output_line ("#include \"%s\"", prog->local_storage_name);
  // output_newline ();

  // output_line ("static int initialized = 0;");
  // if (prog->decimal_index_max) {
  //	output_local ("/* Decimal structures */\n");
  //	for (i = 0; i < prog->decimal_index_max; i++) {
  //		output_local ("static cob_decimal d%d;\n", i);
  //	}
  //	output_local ("\n");
  // }

  joutput_prefix();
  joutput("this.module = new CobolModule(null, ");
  if (prog->collating_sequence) {
    joutput_param(cb_ref(prog->collating_sequence), -1);
  } else {
    joutput("null");
  }
  joutput(", ");
  if (prog->crt_status && cb_field(prog->crt_status)->count) {
    joutput_param(cb_ref(prog->crt_status), -1);
  } else {
    joutput("null");
  }
  joutput(", ");
  if (prog->cursor_pos) {
    joutput_param(cb_ref(prog->cursor_pos), -1);
  } else {
    joutput("null");
  }

  /* Note spare byte at end */
  if (prog->currency_symbol != '\\') {
    joutput(", %d, '%c', '%c', '%c', %d, %d, %d, 0, null );\n", cb_display_sign,
            prog->decimal_point, prog->currency_symbol, prog->numeric_separator,
            cb_filename_mapping, cb_binary_truncate, cb_pretty_display);
  } else {
    joutput(", %d, '%c', '\\%c', '%c', %d, %d, %d, 0, null );\n",
            cb_display_sign, prog->decimal_point, prog->currency_symbol,
            prog->numeric_separator, cb_filename_mapping, cb_binary_truncate,
            cb_pretty_display);
  }
  joutput_newline();

  // if (cb_sticky_linkage && parmnum) {
  //	output_local ("\n/* Sticky linkage save pointers */\n");
  //	for (i = 0; i < parmnum; i++) {
  //		output_local ("static unsigned char\t*cob_parm_%d = NULL;\n",
  // i);
  //	}
  //	output_local ("\n");
  // }

  // if (prog->loop_counter) {
  //	output_local ("\n/* Loop counters */\n");
  //	for (i = 0; i < prog->loop_counter; i++) {
  //		output_local ("int n%d;\n", i);
  //	}
  //	output_local ("\n");
  // }

  ///* BASED working-storage */
  // i = 0;
  // for (f = prog->working_storage; f; f = f->sister) {
  //	if (f->flag_item_based) {
  //		if (!i) {
  //			i = 1;
  //			output_local("/* BASED WORKING-STORAGE SECTION */\n");
  //		}
  //		output_local ("static unsigned char *%s%d = NULL; /* %s */\n",
  //			CB_PREFIX_BASE, f->id, f->name);
  //	}
  // }
  // if (i) {
  //	output_local ("\n");
  // }

  ///* BASED local-storage */
  // i = 0;
  // for (f = prog->local_storage; f; f = f->sister) {
  //	if (f->flag_item_based) {
  //		if (!i) {
  //			i = 1;
  //			output_local("/* BASED LOCAL-STORAGE */\n");
  //		}
  //		output_local ("unsigned char\t\t*%s%d = NULL; /* %s */\n",
  //			CB_PREFIX_BASE, f->id, f->name);
  //		if (prog->flag_global_use) {
  //			output_local ("static unsigned char\t*save_%s%d =
  // NULL;\n", 				CB_PREFIX_BASE, f->id, f->name);
  //		}
  //	}
  // }
  // if (i) {
  //	output_local ("\n");
  // }

  ///* Screens */
  // if (prog->screen_storage) {
  //	output_target = current_prog->local_storage_file;
  //	output ("\n/* Screens */\n\n");
  //	output_screen_definition (prog->screen_storage);
  //	output_newline ();
  //	output_target = yyout;
  // }

  // output_local ("\n/* Define perform frame stack */\n\n");
  // if (cb_perform_osvs) {
  //	output_local ("struct cob_frame\t*temp_index;\n");
  // }
  // if (cb_flag_stack_check) {
  //	output_local ("struct cob_frame\t*frame_overflow;\n");
  // }
  // output_local ("struct cob_frame\t*frame_ptr;\n");
  // output_local ("struct cob_frame\tframe_stack[%d];\n\n", COB_STACK_SIZE);

  // i = 0;
  // anyseen = 0;
  // for (l = parameter_list; l; l = CB_CHAIN (l), i++) {
  //	f = cb_field (CB_VALUE (l));
  //	if (f->flag_any_length) {
  //		if (!anyseen) {
  //			anyseen = 1;
  //			output_local ("/* ANY LENGTH fields */\n");
  //		}
  //		output_local ("cob_field\t\t*anylen_%d;\n", i);
  //		if (prog->flag_global_use) {
  //			output_local ("static cob_field\t*save_anylen_%d;\n",
  // i);
  //		}
  //	}
  // }
  // if (anyseen) {
  //	output_local ("\n");
  // }
  // if (prog->flag_global_use && parameter_list) {
  //	output_local ("/* Parameter save */\n");
  //	for (l = parameter_list; l; l = CB_CHAIN (l)) {
  //		f = cb_field (CB_VALUE (l));
  //		output_local ("static unsigned char\t*save_%s%d;\n",
  //			CB_PREFIX_BASE, f->id);
  //	}
  //	output_local ("\n");
  // }

  joutput_line("/* Start of function code */");
  joutput_newline();
  joutput_line("/* CANCEL callback handling */");
  joutput_line("if (entry < 0) {");
  joutput_line("	if (!this.initialized) {");
  // TODO Replace the follwing line with cob_init()
  joutput_line("		CobolDecimal.cobInitNumeric();");
  joutput_line("		return 0;");
  joutput_line("	}");
  for (l = prog->file_list; l; l = CB_CHAIN(l)) {
    fl = CB_FILE(CB_VALUE(l));
    if (fl->organization != COB_ORG_SORT) {
      joutput_line("	%s%s.close (0, null);", CB_PREFIX_FILE, fl->cname);
    }
  }
  if (prog->decimal_index_max) {
    for (i = 0; i < prog->decimal_index_max; i++) {
      joutput_line("	d%d.clear();", i);
      joutput_line("	d%d.setScale(0);", i);
    }
  }
  joutput_line("	this.initialized = false;");
  joutput_line("	return 0;");
  joutput_line("}");
  joutput_newline();

  joutput_line("/* Push module stack */");
  joutput_line("CobolModule.push (module);");
  joutput_newline();

  /* Initialization */

  joutput_line("/* Initialize program */");
  joutput_line("if (!this.initialized) {");
  joutput_indent_level += 2;
  joutput_line("module.setProgramId(\"%s\");\n", prog->program_id);

  if (prog->decimal_index_max) {
    joutput_line("/* Initialize decimal numbers */");
    for (i = 0; i < prog->decimal_index_max; i++) {
      joutput_line("d%d.decimalInit();", i);
    }
    joutput_newline();
  }
  if (!prog->flag_initial) {
    for (l = prog->file_list; l; l = CB_CHAIN(l)) {
      f = CB_FILE(CB_VALUE(l))->record;
      if (f->flag_external) {
        strcpy(name, f->name);
        for (p = name; *p; p++) {
          if (*p == '-') {
            *p = '_';
          }
        }
        joutput_line("%s%s = CobolExternal.getStorageAddress (\"%s\", %d);",
                     CB_PREFIX_BASE, name, name,
                     CB_FILE(CB_VALUE(l))->record_max);
      }
    }
    joutput_initial_values(prog->working_storage);

    if (has_external) {
      joutput_line("/* init_extern */");
      joutput_line("this.initExternProc();", L_initextern_addr);
    }
    if (prog->file_list) {
      joutput_newline();
      for (l = prog->file_list; l; l = CB_CHAIN(l)) {
        joutput_file_initialization(CB_FILE(CB_VALUE(l)));
      }
      joutput_newline();
    }
  }

  joutput_line("this.initialized = true;");
  joutput_indent_level -= 2;
  joutput_line("}");
  // if (prog->flag_chained) {
  //	joutput ("    } else {\n");
  //	joutput_line ("  cob_fatal_error (COB_FERROR_CHAINING);");
  //	joutput_indent ("  }");
  // } else {
  //	joutput_indent ("  }");
  // }
  // output_newline ();

  /* Set up LOCAL-STORAGE cache */
  // if (prog->local_storage) {
  //	for (f = prog->local_storage; f; f = f->sister) {
  //		ff = cb_field_founder (f);
  //		if (ff->redefines) {
  //			ff = ff->redefines;
  //		}
  //		if (ff->flag_item_based || ff->flag_local_alloced) {
  //			continue;
  //		}
  //		if (ff->flag_item_78) {
  //			fprintf (stderr, "Unexpected CONSTANT item\n");
  //			ABORT ();
  //		}
  //		ff->flag_local_alloced = 1;
  //		locptr = cobc_malloc (sizeof (struct local_list));
  //		locptr->f = ff;
  //		locptr->next = local_cache;
  //		local_cache = locptr;
  //	}
  //	local_cache = local_list_reverse (local_cache);
  // }
  ///* Global entry dispatch */
  // if (prog->global_list) {
  //	output_line ("/* Global entry dispatch */");
  //	output_newline ();
  //	for (l = prog->global_list; l; l = CB_CHAIN (l)) {
  //		output_line ("if (unlikely(entry == %d)) {",
  //				CB_LABEL (CB_VALUE (l))->id);
  //		if (cb_flag_traceall) {
  //			output_line ("\tcob_ready_trace ();");
  //		}
  //		for (locptr = local_cache; locptr; locptr = locptr->next) {
  //			output_line ("\t%s%d = save_%s%d;",
  //				CB_PREFIX_BASE, locptr->f->id,
  //				CB_PREFIX_BASE, locptr->f->id);
  //		}
  //		i = 0;
  //		for (l2 = parameter_list; l2; l2 = CB_CHAIN (l), i++) {
  //			f = cb_field (CB_VALUE (l2));
  //			output_line ("\t%s%d = save_%s%d;",
  //				CB_PREFIX_BASE, f->id,
  //				CB_PREFIX_BASE, f->id);
  //			if (f->flag_any_length) {
  //				output_line ("\tanylen_%d = save_anylen_%d;", i,
  // i);
  //			}
  //		}
  //		output_line ("\tgoto %s%d;",
  //				CB_PREFIX_LABEL,
  //				CB_LABEL (CB_VALUE (l))->id);
  //		output_line ("}");
  //	}
  //	output_newline ();
  // }

  if (prog->flag_initial) {
    for (l = prog->file_list; l; l = CB_CHAIN(l)) {
      f = CB_FILE(CB_VALUE(l))->record;
      if (f->flag_external) {
        strcpy(name, f->name);
        for (p = name; *p; p++) {
          if (*p == '-') {
            *p = '_';
          }
        }
        joutput_line("%s%s = CobolExternal.getStorageAddress (\"%s\", %d);",
                     CB_PREFIX_BASE, name, name,
                     CB_FILE(CB_VALUE(l))->record_max);
      }
    }
    joutput_initial_values(prog->working_storage);
    if (has_external) {
      joutput_line("this.initExternProc();");
    }
    joutput_newline();
    for (l = prog->file_list; l; l = CB_CHAIN(l)) {
      joutput_file_initialization(CB_FILE(CB_VALUE(l)));
    }
    joutput_newline();
  }
  // if (prog->local_storage) {
  //	if (local_cache) {
  //		output_line ("/* Allocate LOCAL storage */");
  //	}
  //	for (locptr = local_cache; locptr; locptr = locptr->next) {
  //		output_line ("%s%d = cob_malloc (%d);", CB_PREFIX_BASE,
  //				locptr->f->id, locptr->f->memory_size);
  //		if (current_prog->flag_global_use) {
  //			output_line ("save_%s%d = %s%d;",
  //					CB_PREFIX_BASE, locptr->f->id,
  //					CB_PREFIX_BASE, locptr->f->id);
  //		}
  //	}
  //	output_newline ();
  //	output_line ("/* Initialialize LOCAL storage */");
  //	output_initial_values (prog->local_storage);
  //	output_newline ();
  // }

  if (cb_field(current_prog->cb_call_params)->count) {
    joutput_line("/* Initialize number of call params */");
    joutput_prefix();
    joutput_param(current_prog->cb_call_params, -1);
    joutput(".setInt(CobolCallParams.callParams);");
    joutput_newline();
  }
  // output_line ("cob_save_call_params = cob_call_params;");
  // output_newline ();
  if (cb_flag_traceall) {
    joutput_line("CobolUtil.readyTrace();");
    joutput_newline();
  }

  // i = 0;
  // if (anyseen) {
  //	output_line ("/* Initialize ANY LENGTH parameters */");
  // }
  // for (l = parameter_list; l; l = CB_CHAIN (l), i++) {
  //	f = cb_field (CB_VALUE (l));
  //	if (f->flag_any_length) {
  //		output ("  anylen_%d = ", i);
  //		output_param (CB_VALUE (l), i);
  //		output (";\n");
  //		if (prog->flag_global_use) {
  //			output_line ("save_anylen_%d = anylen_%d;", i, i);
  //		}
  //		output_line ("if (cob_call_params > %d && %s%d%s)",
  //			i, "module.next->cob_procedure_parameters[",
  //			i, "]");
  //		output_line ("  anylen_%d->size = %s%d%s;", i,
  //			"module.next->cob_procedure_parameters[",
  //			i, "]->size");
  //	}
  // }
  // if (anyseen) {
  //	output_newline ();
  // }
  // if (prog->flag_global_use && parameter_list) {
  //	output_line ("/* Parameter save */");
  //	for (l = parameter_list; l; l = CB_CHAIN (l)) {
  //		f = cb_field (CB_VALUE (l));
  //		output_line ("save_%s%d = %s%d;",
  //			CB_PREFIX_BASE, f->id,
  //			CB_PREFIX_BASE, f->id);
  //	}
  //	output_newline ();
  // }

  /* PROCEDURE DIVISION */
  joutput_line("/* PROCEDURE DIVISION */");
  joutput_line("try{");
  joutput_line("  CobolStopRunException.dummy();");
  joutput_line("  CobolGoBackException.dummy();");
  joutput_indent_level += 2;

  // EDIT
  // joutput_init_buffer_list();
  // joutput_buffered = 1;
  // for (l = prog->exec_list; l; l = CB_CHAIN (l)) {
  //	joutput_stmt (CB_VALUE (l));
  // }
  // joutput_buffered = 0;

  /* Entry dispatch */
  joutput_line("/* Entry dispatch */");
  if (cb_list_length(prog->entry_list) > 1) {
    joutput_line("//multiple entry dispatch is not implemented");
    joutput_newline();
    joutput_line("switch (entry)");
    joutput_line("  {");
    for (i = 0, l = prog->entry_list; l; l = CB_CHAIN(l)) {
      joutput_line("  case %d:", i++);
      joutput_prefix();
      joutput("    execEntry(");
      joutput_label_variable(CB_LABEL(CB_PURPOSE(l)));
      joutput(");\n");
    }
    joutput_line("  }");
    joutput_line("/* This should never be reached */");
    joutput_line("CobolUtil.fatalError (CobolUtil.FERROR_CHAINING);");
    joutput_newline();
  } else {
    l = prog->entry_list;
    joutput_prefix();
    joutput("execEntry(");
    joutput_label_variable(CB_LABEL(CB_PURPOSE(l)));
    joutput(");\n");
    joutput_newline();
  }

  joutput_indent_level -= 2;
  joutput_line("} catch(CobolGoBackException e) {");
  joutput_line("  return e.getReturnCode();");
  joutput_line("} catch(CobolStopRunException e) {");
  joutput_line("  CobolStopRunException.stopRun();");
  joutput_line("  System.exit(e.getReturnCode());");
  joutput_line("}");

  /* PROCEDURE DIVISION */
  // output_newline ();
  // output_line ("/* Program exit */");
  // output_newline ();

  // if (needs_exit_prog) {
  //	output_line ("exit_program:");
  //	output_newline ();
  // }
  // if (prog->local_storage) {
  //	output_line ("/* Deallocate LOCAL storage */");
  //	local_cache = local_list_reverse (local_cache);
  //	for (locptr = local_cache; locptr; locptr = locptr->next) {
  //		output_line ("if (%s%d) {", CB_PREFIX_BASE, locptr->f->id);
  //		output_line ("\tfree (%s%d);", CB_PREFIX_BASE, locptr->f->id);
  //		output_line ("\t%s%d = NULL;", CB_PREFIX_BASE, locptr->f->id);
  //		output_line ("}");
  //	}
  //	output_newline ();
  // }
  joutput_line("/* Pop module stack */");
  joutput_line("CobolModule.pop();");
  joutput_newline();
  if (cb_flag_traceall) {
    joutput_line("CobolUtil.resetTrace();");
    joutput_newline();
  }
  // output_line ("/* Program return */");
  // output_prefix ();
  // output ("return ");
  // output_integer (current_prog->cb_return_code);
  // output (";\n");

  // joutput_next_buffer(1);
  // joutput_buffered = 1;
  // joutput_buffered = 0;
#ifndef __GNUC__
  // output_newline ();
  // output_line ("/* Frame stack jump table */");
  // output_line ("P_switch:");
  // if (label_cache) {
  //	output_line (" switch (frame_ptr->return_address) {");
  //  struct label_list *pl;
  //	for (pl = label_cache; pl; pl = pl->next) {
  //		output_line (" case %d:", pl->call_num);
  //		output_line ("   goto %s%d;", CB_PREFIX_LABEL, pl->id);
  //	}
  //	output_line (" }");
  // }
  // output_line (" cob_fatal_error (COB_FERROR_CODEGEN);");
  // output_newline ();
#endif

  joutput_line("/* Program return */");
  joutput_prefix();
  joutput("return ");
  joutput_integer(current_prog->cb_return_code);
  joutput(";\n");

  joutput_indent_level -= 2;
  joutput_line("}");
}

static int field_cache_cmp(void *mp1, void *mp2) {
  struct field_list *fl1;
  struct field_list *fl2;
  int ret;

  fl1 = (struct field_list *)mp1;
  fl2 = (struct field_list *)mp2;
  ret = cobc_casecmp(fl1->curr_prog, fl2->curr_prog);
  if (ret) {
    return ret;
  }
  return fl1->f->id - fl2->f->id;
}

static int base_cache_cmp(void *mp1, void *mp2) {
  struct base_list *fl1;
  struct base_list *fl2;

  fl1 = (struct base_list *)mp1;
  fl2 = (struct base_list *)mp2;
  return fl1->f->id - fl2->f->id;
}

/* Sort a structure linked list in place */
/* Assumed that "next" is first item in structure */
static void *list_cache_sort(void *inlist,
                             int (*cmpfunc)(void *mp1, void *mp2)) {
  struct sort_list *p;
  struct sort_list *q;
  struct sort_list *e;
  struct sort_list *list;
  int insize;
  int psize;
  int qsize;
  int i;

  if (!inlist) {
    return NULL;
  }
  list = (struct sort_list *)inlist;
  insize = 1;
  for (;;) {
    p = list;
    list = NULL;

    struct sort_list *tail = NULL;
    int nmerges = 0;
    while (p) {
      nmerges++;
      q = p;
      psize = 0;
      for (i = 0; i < insize; i++) {
        psize++;
        q = q->next;
        if (!q) {
          break;
        }
      }
      qsize = insize;
      while (psize > 0 || (qsize > 0 && q)) {
        if (psize == 0) {
          e = q;
          q = q->next;
          qsize--;
        } else if (qsize == 0 || !q) {
          e = p;
          p = p->next;
          psize--;
        } else if ((*cmpfunc)(p, q) <= 0) {
          e = p;
          p = p->next;
          psize--;
        } else {
          e = q;
          q = q->next;
          qsize--;
        }
        if (tail) {
          tail->next = e;
        } else {
          list = e;
        }
        tail = e;
      }
      p = q;
    }
    if (tail) {
      tail->next = NULL;
    }
    if (nmerges <= 1) {
      return (void *)list;
    }
    insize *= 2;
  }
}

/**
 * メンバ変数の初期化を行うメソッドinitを出力する
 */
static void joutput_init_method(struct cb_program *prog) {
  struct literal_list *m;
  struct field_list *k;
  struct attr_list *j;
  struct base_list *blp;
  const char *prevprog;
  cb_tree l;

  joutput_line("public void init() ");
  joutput_line("{");
  joutput_indent_level += 2;
  joutput_line("try {");
  joutput_indent_level += 2;

  if (prog->decimal_index_max) {
    joutput_line("/* Decimal structures */\n");
    int i;
    for (i = 0; i < prog->decimal_index_max; i++) {
      joutput_line("d%d = new CobolDecimal();", i);
    }
    joutput("\n");
  }

  /* CobolDataStorage型変数の初期化(定数) */
  if (base_cache) {
    joutput_line("/* Data storage */\n");
    joutput_line("cob_unifunc = null;\n");
    base_cache = list_cache_sort(base_cache, &base_cache_cmp);
    prevprog = NULL;
    // EDIT
    for (blp = base_cache; blp; blp = blp->next) {
      char *base_name = get_java_identifier_base(blp->f);
      if (blp->curr_prog != prevprog) {
        prevprog = blp->curr_prog;
        joutput_prefix();
        joutput("/* PROGRAM-ID : %s */\n", prevprog);
        joutput_prefix();
        joutput("%s = new CobolDataStorage(%d);", base_name,
                blp->f->memory_size);
      } else {
        joutput_prefix();
        joutput("%s = new CobolDataStorage(%d);", base_name,
                blp->f->memory_size);
      }
      free(base_name);
      joutput("\t/* %s */\n", blp->f->name);
    }
    int i;
    for (i = 0; i < data_storage_cache_count; ++i) {
      struct data_storage_list *entry = sorted_data_storage_cache[i];
      if (is_call_parameter(entry->top) && entry->f != entry->top) {
        continue;
      }
      joutput_prefix();
      joutput_field_storage(entry->f, entry->top);
      joutput(" = ");
      char *base_name = get_java_identifier_base(entry->top);
      joutput("%s", base_name);
      free(base_name);
      if (entry->f->offset != 0) {
        joutput(".getSubDataStorage(%d)", entry->f->offset);
      }
      joutput(";\n");
    }
    joutput("\n");
    joutput_line("/* End of data storage */\n\n");
  }

  /* init attribute function*/
  joutput_line("initAttr();\n");

  /* AbstractCobolField型変数の初期化(非定数) */
  if (field_cache) {
    joutput_line("/* Fields */\n");
    field_cache = list_cache_sort(field_cache, &field_cache_cmp);
    prevprog = NULL;
    for (k = field_cache; k; k = k->next) {
      if (k->curr_prog != prevprog) {
        prevprog = k->curr_prog;
        joutput_prefix();
        joutput("/* PROGRAM-ID : %s */\n", prevprog);
      }

      /*if (!cb_flag_no_cobol_comment && k->f->definition_source_file &&
          strcmp(k->f->definition_source_file, translating_source_file) == 0) {
        char *file_name_of_last_comment = NULL;
        for (; working_storage_comment_info_cursor;
             working_storage_comment_info_cursor =
                 working_storage_comment_info_cursor->next) {
          if (working_storage_comment_info_cursor->position_in_source_code <=
                  POSITION_BEFORE_WORKING_STORAGE ||
              working_storage_comment_info_cursor->line <=
                  working_storage_section_line_number ||
              working_storage_comment_info_cursor->position_in_source_code >=
                  POSITION_AFTER_PROCEDURE_DIVISION) {
            continue;
          }
          if (working_storage_comment_info_cursor->line <=
              k->f->prev_field_line_number) {
            continue;
          }

          if (file_name_of_last_comment != NULL &&
              strcmp(file_name_of_last_comment,
                     working_storage_comment_info_cursor->file) != 0) {
            break;
          }
          if (k->f->definition_source_file == NULL) {
            break;
          }
          if (strcmp(working_storage_comment_info_cursor->file,
                     k->f->definition_source_file) == 0 &&
              working_storage_comment_info_cursor->line >
                  k->f->definition_source_line) {
            break;
          }
          joutput_cobol_comment(working_storage_comment_info_cursor);
          file_name_of_last_comment = working_storage_comment_info_cursor->file;
        }
      }*/

      joutput_prefix();
      char *field_name = get_java_identifier_field(k->f);

      if (!k->f->flag_local && !k->f->flag_item_external) {
        joutput("%s\t= ", field_name);
        joutput_field(k->x);
      } else {
        joutput("%s\t= ", field_name);
        joutput("CobolFieldFactory.makeCobolField(");
        joutput_size(k->x);
        joutput(", (CobolDataStorage)null, ");
        joutput_attr(k->x);
        joutput(")");
      }

      free(field_name);
      joutput(";\t/* %s */\n", k->f->name);
    }
    joutput("\n");
    joutput_line("/* End of fields */\n\n");
  }

  if (call_parameter_cache) {
    joutput_line("/* Call parameters */");
    struct call_parameter_list *cp;
    for (cp = call_parameter_cache; cp; cp = cp->next) {
      int cached = 0;
      char *call_parameter_field_name = get_java_identifier_field(cp->field);
      if (field_cache) {
        struct field_list *f;
        for (f = field_cache; f; f = f->next) {
          char *field_name = get_java_identifier_field(f->f);
          if (f->f == cp->field &&
              strcmp(call_parameter_field_name, field_name) == 0) {
            cached = 1;
            free(field_name);
            break;
          }
          free(field_name);
        }
      }
      if (!cached) {
        joutput_prefix();
        joutput("%s = CobolFieldFactory.makeCobolField(",
                call_parameter_field_name);
        joutput("%d, (CobolDataStorage)null, ", cp->field->size);
        joutput_attr(cp->x);
        joutput(");\n");
      }
      free(call_parameter_field_name);
    }
  }

  /* AbstractCobolField型変数の初期化(定数) */
  if (literal_cache) {
    joutput_line("/* Constants */\n");
    literal_cache = literal_list_reverse(literal_cache);
    for (m = literal_cache; m; m = m->next) {
      joutput_prefix();
      joutput_const_identifier(m);
      joutput("\t= ");
      joutput_field(m->x);
      joutput(";\n");
    }
    joutput_newline();
  }

  /* Dangling linkage section items */
  int seen = 0;
  struct cb_field *f;
  for (f = prog->linkage_storage; f; f = f->sister) {
    for (l = prog->parameter_list; l; l = CB_CHAIN(l)) {
      if (f == cb_field(CB_VALUE(l))) {
        break;
      }
    }
    if (l == NULL) {
      if (!seen) {
        seen = 1;
        joutput(
            "\n/* LINKAGE SECTION (Items not referenced by USING clause) */\n");
      }
      char *base_name = get_java_identifier_base(f);
      joutput_line("%s = null;  /* %s */", base_name, f->name);
      free(base_name);
    }
  }
  if (seen) {
    joutput("\n");
  }

  /* Alphabet-names */
  if (prog->alphabet_name_list) {
    joutput("/* Alphabet names */\n");
    for (l = prog->alphabet_name_list; l; l = CB_CHAIN(l)) {
      joutput_alphabet_name_initialization(CB_ALPHABET_NAME(CB_VALUE(l)));
    }
    joutput("\n");
  }

  if (gen_native) {
    int index = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
    joutput_line("%snative = CobolFieldFactory.makeCobolField(256, new "
                 "CobolDataStorage(cob_native), %s%d);\n",
                 CB_PREFIX_FIELD, CB_PREFIX_ATTR, index);
  }

  joutput_indent_level -= 2;
  joutput_line("} catch(NullPointerException e) {");
  joutput_line("  System.out.println(\"Error - NullpointerException\");");
  joutput_line("} catch(IndexOutOfBoundsException e) {");
  joutput_line("  System.out.println(\"Error - IndexOutOfBoundsException\");");
  joutput_line("}");
  joutput_indent_level -= 2;
  joutput_line("}\n");

  joutput_line("private void initAttr() {");
  joutput_indent_level += 2;

  /* CobolFieldAttribute型変数の初期化 */
  if (attr_cache) {
    joutput_line("/* Attributes */\n");
    attr_cache = attr_list_reverse(attr_cache);
    for (j = attr_cache; j; j = j->next) {
      joutput_prefix();
      joutput("%s%d = ", CB_PREFIX_ATTR, j->id);
      joutput("new CobolFieldAttribute (%d, %d, %d, %d, ", j->type, j->digits,
              j->scale, j->flags);
      if (j->pic) {
        joutput("\"");
        unsigned char *s;
        for (s = j->pic; *s; s += 5) {
          if (s[0] == '\\') {
            joutput("\\%c\\%03o\\%03o\\%03o\\%03o", s[0], s[1], s[2], s[3],
                    s[4]);
          } else {
            joutput("%c\\%03o\\%03o\\%03o\\%03o", s[0], s[1], s[2], s[3], s[4]);
          }
        }
        joutput("\"");
      } else {
        joutput("null");
      }
      joutput(");\n");
    }
  }

  joutput("\n");
  joutput_indent_level -= 2;
  joutput_line("}");
}

static void joutput_alphabet_name_initialization(struct cb_alphabet_name *p) {
  cb_tree l;
  cb_tree ls;
  cb_tree x;
  unsigned char *data;
  int i;
  int n = 0;
  int size;
  int upper;
  int lower;
  int table[256];

  /* Reset to -1 */
  for (i = 0; i < 256; i++) {
    table[i] = -1;
  }

  for (l = p->custom_list; l; l = CB_CHAIN(l)) {
    x = CB_VALUE(l);
    if (CB_PAIR_P(x)) {
      /* X THRU Y */
      lower = literal_value(CB_PAIR_X(x));
      upper = literal_value(CB_PAIR_Y(x));
      if (lower <= upper) {
        for (i = lower; i <= upper; i++) {
          table[i] = n++;
        }
      } else {
        for (i = upper; i >= lower; i--) {
          table[i] = n++;
        }
      }
    } else if (CB_LIST_P(x)) {
      /* X ALSO Y ... */
      for (ls = x; ls; ls = CB_CHAIN(ls)) {
        table[literal_value(CB_VALUE(ls))] = n;
      }
      n++;
    } else {
      /* Literal */
      if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
        table[literal_value(x)] = n++;
      } else if (CB_LITERAL_P(x)) {
        size = (int)CB_LITERAL(x)->size;
        data = CB_LITERAL(x)->data;
        for (i = 0; i < size; i++) {
          table[data[i]] = n++;
        }
      } else {
        table[literal_value(x)] = n++;
      }
    }
  }

  /* Fill the rest of characters */
  for (i = 0; i < 256; i++) {
    if (table[i] == -1) {
      table[i] = n++;
    }
  }

  /* Output the table */

  joutput("%s%s = new CobolDataStorage(%s_byte_array_%s);", CB_PREFIX_SEQUENCE,
          p->cname, CB_PREFIX_SEQUENCE, p->cname);
  i = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
  joutput("%s%s = CobolFieldFactory.makeCobolField(256, %s%s, %s%d);\n",
          CB_PREFIX_FIELD, p->cname, CB_PREFIX_SEQUENCE, p->cname,
          CB_PREFIX_ATTR, i);
  joutput("\n");
}

static void joutput_alphabet_name_definition(struct cb_alphabet_name *p) {
  cb_tree l;
  cb_tree ls;
  cb_tree x;
  unsigned char *data;
  int i;
  int n = 0;
  int size;
  int upper;
  int lower;
  int table[256];

  /* Reset to -1 */
  for (i = 0; i < 256; i++) {
    table[i] = -1;
  }

  for (l = p->custom_list; l; l = CB_CHAIN(l)) {
    x = CB_VALUE(l);
    if (CB_PAIR_P(x)) {
      /* X THRU Y */
      lower = literal_value(CB_PAIR_X(x));
      upper = literal_value(CB_PAIR_Y(x));
      if (lower <= upper) {
        for (i = lower; i <= upper; i++) {
          table[i] = n++;
        }
      } else {
        for (i = upper; i >= lower; i--) {
          table[i] = n++;
        }
      }
    } else if (CB_LIST_P(x)) {
      /* X ALSO Y ... */
      for (ls = x; ls; ls = CB_CHAIN(ls)) {
        table[literal_value(CB_VALUE(ls))] = n;
      }
      n++;
    } else {
      /* Literal */
      if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
        table[literal_value(x)] = n++;
      } else if (CB_LITERAL_P(x)) {
        size = (int)CB_LITERAL(x)->size;
        data = CB_LITERAL(x)->data;
        for (i = 0; i < size; i++) {
          table[data[i]] = n++;
        }
      } else {
        table[literal_value(x)] = n++;
      }
    }
  }

  /* Fill the rest of characters */
  for (i = 0; i < 256; i++) {
    if (table[i] == -1) {
      table[i] = n++;
    }
  }

  /* Output the table */
  joutput("static byte[] %s_byte_array_%s = {\n", CB_PREFIX_SEQUENCE, p->cname);
  for (i = 0; i < 256; i++) {
    if (i == 255) {
      joutput(" (byte)%d", table[i]);
    } else {
      joutput(" (byte)%d,", table[i]);
    }
    if (i % 16 == 15) {
      joutput("\n");
    }
  }
  joutput("};\n");
  joutput("CobolDataStorage %s%s;", CB_PREFIX_SEQUENCE, p->cname);
  i = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
  joutput("AbstractCobolField %s%s;\n", CB_PREFIX_FIELD, p->cname,
          CB_PREFIX_SEQUENCE, p->cname, CB_PREFIX_ATTR, i);
  joutput("\n");
}

/**
 * メンバ変数の宣言部分を出力
 */
static void joutput_declare_member_variables(struct cb_program *prog,
                                             cb_tree parameter_list) {
  int i;
  cb_tree l;
  struct literal_list *m;
  struct field_list *k;
  struct attr_list *j;
  struct base_list *blp;
  const char *prevprog;
  struct cb_field *f;
  char *p;
  char name[COB_MINI_BUFF];

  /* CobolDecimal型変数の宣言 */
  if (prog->decimal_index_max) {
    joutput_line("/* Decimal structures */\n");
    for (i = 0; i < prog->decimal_index_max; i++) {
      joutput_line("private CobolDecimal d%d;", i);
    }
    joutput("\n");
  }

  /* Program local stuff */

  /* CALL cache */
  // if (call_cache) {
  //	output_local ("\n/* Call pointers */\n");
  //	for (clp = call_cache; clp; clp = clp->next) {
  //		output_local ("static union cob_call_union\tcall_%s = { NULL
  //};\n", clp->callname);
  //	}
  //	output_local ("\n");
  // }

  /* Local indexes */
  for (i = 0; i < COB_MAX_SUBSCRIPTS; i++) {
    if (i_counters[i]) {
      joutput_local("private int\t\ti%d;\n", i);
    }
  }

  /* Local implicit fields */
  // if (num_cob_fields) {
  //	output_local ("\n/* Local AbstractCobolField items */\n");
  //	for (i = 0; i < num_cob_fields; i++) {
  //		output_local ("AbstractCobolField\tf%d;\n", i);
  //	}
  //	output_local ("\n");
  // }

  /* Skip to next nested program */

  // if (prog->next_program) {
  //	codegen (prog->next_program, 1);
  //	return;
  // }

  /* Alphabet-names */
  if (prog->alphabet_name_list) {
    joutput("/* Alphabet names */\n");
    for (l = prog->alphabet_name_list; l; l = CB_CHAIN(l)) {
      joutput_alphabet_name_definition(CB_ALPHABET_NAME(CB_VALUE(l)));
    }
    joutput("\n");
  }

  /* CobolDataStorge型変数の宣言 */
  if (base_cache) {
    joutput_line("/* Data storage */\n");
    base_cache = list_cache_sort(base_cache, &base_cache_cmp);
    prevprog = NULL;
    for (blp = base_cache; blp; blp = blp->next) {
      char *base_name = get_java_identifier_base(blp->f);
      if (blp->curr_prog != prevprog) {
        prevprog = blp->curr_prog;
        joutput_prefix();
        joutput("/* PROGRAM-ID : %s */\n", prevprog);
        joutput_prefix();
        joutput("private CobolDataStorage %s;", base_name, blp->f->memory_size);
      } else {
        joutput_prefix();
        joutput("private CobolDataStorage %s;", base_name, blp->f->memory_size);
      }
      free(base_name);
      joutput("\t/* %s */\n", blp->f->name);
    }

    for (i = 0; i < data_storage_cache_count; ++i) {
      struct data_storage_list *entry = sorted_data_storage_cache[i];
      if (is_call_parameter(entry->top) && entry->f != entry->top) {
        continue;
      }
      joutput_prefix();
      joutput("private CobolDataStorage ");
      joutput_field_storage(entry->f, entry->top);
      joutput(";\n");
    }

    joutput("\n");
    joutput_line("/* End of data storage */\n\n");
  }

  /* Dangling linkage section items */
  int seen = 0;
  for (f = prog->linkage_storage; f; f = f->sister) {
    for (l = prog->parameter_list; l; l = CB_CHAIN(l)) {
      if (f == cb_field(CB_VALUE(l))) {
        break;
      }
    }
    if (l == NULL) {
      if (!seen) {
        seen = 1;
        joutput(
            "\n/* LINKAGE SECTION (Items not referenced by USING clause) */\n");
      }
      char *base_name = get_java_identifier_base(f);
      joutput_line("private CobolDataStorage %s;  /* %s */", base_name,
                   f->name);
      free(base_name);
    }
  }
  if (seen) {
    joutput("\n");
  }

  /* External items */
  for (f = prog->working_storage; f; f = f->sister) {
    if (f->flag_external) {
      strcpy(name, f->name);
      for (p = name; *p; p++) {
        if (*p == '-') {
          *p = '_';
        }
      }
      joutput("private CobolDataStorage\t%s%s = null;", CB_PREFIX_BASE, name);
      joutput("  /* %s */\n", f->name);
    }
  }
  for (l = prog->file_list; l; l = CB_CHAIN(l)) {
    f = CB_FILE(CB_VALUE(l))->record;
    if (f->flag_external) {
      strcpy(name, f->name);
      for (p = name; *p; p++) {
        if (*p == '-') {
          *p = '_';
        }
      }
      joutput("private CobolDataStorage\t%s%s = null;", CB_PREFIX_BASE, name);
      joutput("  /* %s */\n", f->name);
    }
  }

  /* AbstractCobolField型変数の宣言(非定数) */
  if (field_cache) {
    joutput_line("/* Fields */\n");
    field_cache = list_cache_sort(field_cache, &field_cache_cmp);
    prevprog = NULL;
    for (k = field_cache; k; k = k->next) {
      if (k->curr_prog != prevprog) {
        prevprog = k->curr_prog;
        joutput_prefix();
        joutput("/* PROGRAM-ID : %s */\n", prevprog);
      }
      joutput_prefix();
      char *field_name = get_java_identifier_field(k->f);
      joutput("private AbstractCobolField %s;", field_name);
      free(field_name);
      joutput("\t/* %s */\n", k->f->name);
    }
    joutput("\n");
    joutput_line("/* End of fields */\n\n");
  }

  joutput_line("private static AbstractCobolField %snative;\n",
               CB_PREFIX_FIELD);

  /* AbstractCobolField型変数の宣言(定数) */
  if (literal_cache) {
    joutput_line("/* Constants */\n");
    literal_cache = literal_list_reverse(literal_cache);
    for (m = literal_cache; m; m = m->next) {
      joutput_prefix();
      joutput("private AbstractCobolField ");
      joutput_const_identifier(m);
      joutput(";\n");
    }
    joutput("\n");
  }

  /* CobolFieldAttribute型変数の宣言(定数) */
  if (attr_cache) {
    joutput_line("/* Attributes */\n");
    attr_cache = attr_list_reverse(attr_cache);
    for (j = attr_cache; j; j = j->next) {
      joutput_line("private CobolFieldAttribute %s%d;", CB_PREFIX_ATTR, j->id);
    }
    joutput("\n");
  }

  if (call_parameter_cache) {
    joutput_line("/* Call parameters */");
    struct call_parameter_list *cp;
    for (cp = call_parameter_cache; cp; cp = cp->next) {
      int cached = 0;
      if (field_cache) {
        struct field_list *field;
        for (field = field_cache; field; field = field->next) {
          if (field->f == cp->field) {
            cached = 1;
            break;
          }
        }
      }
      if (!cached) {
        char *field_name = get_java_identifier_field(cp->field);
        joutput_line("private AbstractCobolField %s;", field_name);
        free(field_name);
      }
      char *base_name = get_java_identifier_base(cp->field);
      joutput_line("private CobolDataStorage %s;", base_name);
      free(base_name);
    }
  }

  joutput("\n");
}

/*
 * Class definition
 */

static void joutput_class_name_definition(struct cb_class_name *p) {
  cb_tree l;
  unsigned char *data;
  size_t i;
  size_t size;
  int lower;
  int upper;

  joutput_line("static boolean");
  joutput_line("%s (AbstractCobolField f)", p->cname);
  joutput_indent("{");
  joutput_line("for (int i = 0; i < f.getSize(); i++)");
  joutput_prefix();
  joutput("  if (!(    ");
  for (l = p->list; l; l = CB_CHAIN(l)) {
    cb_tree x = CB_VALUE(l);
    if (CB_PAIR_P(x)) {
      lower = literal_value(CB_PAIR_X(x));
      upper = literal_value(CB_PAIR_Y(x));
      if (!lower) {
        joutput("f.getDataStorage().getByte(i) <= %d", upper);
      } else {
        joutput("(%d <= f.getDataStorage().getByte(i) && "
                "f.getDataStorage().getByte(i) <= %d)",
                lower, upper);
      }
    } else {
      if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
        joutput("f.getDataStorage().getByte(i) == %d", literal_value(x));
      } else if (x == cb_space) {
        joutput("f.getDataStorage().getByte(i) == %d", ' ');
      } else if (x == cb_zero) {
        joutput("f.getDataStorage().getByte(i) == %d", '0');
      } else if (x == cb_quote) {
        joutput("f.getDataStorage().getByte(i) == %d", '"');
      } else if (x == cb_null) {
        joutput("f.getDataStorage().getByte(i) == 0");
      } else {
        size = CB_LITERAL(x)->size;
        data = CB_LITERAL(x)->data;
        for (i = 0; i < size; i++) {
          joutput("f.getDataStorage().getByte(i) == %d", data[i]);
          if (i + 1 < size) {
            joutput(" || ");
          }
        }
      }
    }
    if (CB_CHAIN(l)) {
      joutput("\n");
      joutput_prefix();
      joutput("         || ");
    }
  }
  joutput(" ))\n");
  joutput_line("    return false;");
  joutput_line("return true;");
  joutput_indent("}");
  joutput_newline();
}

static void append_label_id_map(struct cb_label *label) {
  struct cb_label_id_map *new_entry = malloc(sizeof(struct cb_label_id_map));
  new_entry->key = label->id;
  new_entry->val = ++label_id_counter;
  new_entry->section = label->section;
  // clone label name
  if (label->name) {
    new_entry->label_name = malloc(strlen((char *)label->name) + 1);
    strcpy(new_entry->label_name, (char *)label->name);
  } else {
    new_entry->label_name = NULL;
  }
  new_entry->next = NULL;
  if (label_id_map_last) {
    label_id_map_last->next = new_entry;
  } else {
    label_id_map_head = new_entry;
  }
  label_id_map_last = new_entry;
}

static void create_label_id_map(struct cb_program *prog) {
  label_id_counter = 0;
  label_id_map_head = NULL;
  label_id_map_last = NULL;
  cb_tree l;

  for (l = prog->exec_list; l; l = CB_CHAIN(l)) {
    if (CB_TREE_TAG(CB_VALUE(l)) == CB_TAG_LABEL) {
      struct cb_label *label = CB_LABEL(CB_VALUE(l));
      append_label_id_map(label);
    }
  }
  append_label_id_map(CB_LABEL(cb_standard_error_handler));
}

static void joutput_label_variable_name(char *s, int key,
                                        struct cb_label *section) {
  joutput(CB_PREFIX_LABEL);
  if (s) {
    if (section && section->name) {
      const char *c;
      for (c = (const char *)section->name; *c; ++c) {
        if (*c == ' ') {
          joutput("_");
        } else if (*c == '-') {
          joutput("_");
        } else {
          joutput("%c", *c);
        }
      }
      joutput("__");
    }
    char buf[COB_SMALL_BUFF];
    strcpy_identifier_cobol_to_java(buf, s);
    char *p = buf;
    while (*p) {
      if (*p < 0x80) {
        if (*p == '-') {
          *p = '_';
        } else if (*p == ' ') {
          *p = '_';
        }
        p++;
      } else {
        if (*(p + 1) == '\0') {
          break;
        } else {
          p = p + 2;
        }
      }
    }
    joutput("%s", buf);
  } else {
    joutput("anonymous__%d", key);
  }
}

static void joutput_label_variable(struct cb_label *label) {
  if (!label) {
    fprintf(stderr, "[internal error] label is null\n");
    return;
  }
  int id = CB_LABEL(label)->id;
  struct cb_label_id_map *l;
  for (l = label_id_map_head; l; l = l->next) {
    if (l->key == id) {
      joutput_label_variable_name(l->label_name, l->key, l->section);
      return;
    }
  }
  fprintf(stderr, "[internal error] cannot find label_id: %d %s\n",
          CB_LABEL(label)->id, CB_LABEL(label)->name);
}

static void joutput_label_variable_by_value(int value) {
  struct cb_label_id_map *l;
  for (l = label_id_map_head; l; l = l->next) {
    if (l->val == value) {
      joutput_label_variable_name(l->label_name, l->key, l->section);
      return;
    }
  }
  fprintf(stderr, "[internal error] cannot find label_value: %d\n", value);
}

static void destroy_label_id_map() {
  while (label_id_map_head) {
    struct cb_label_id_map *next = label_id_map_head->next;
    if (label_id_map_head->label_name) {
      free(label_id_map_head->label_name);
    }
    free(label_id_map_head);
    label_id_map_head = next;
  }
  label_id_map_head = NULL;
}

static void joutput_execution_list(struct cb_program *prog) {
  control_counter = 0;

  joutput_line("public CobolControl[] contList = {");
  joutput_indent_level += 2;
  joutput_line("new CobolControl(0, CobolControl.LabelType.label) {");
  joutput_indent_level += 2;
  joutput_line(
      "public Optional<CobolControl> run() throws CobolRuntimeException, "
      "CobolGoBackException, CobolStopRunException {");
  joutput_indent_level += 2;
  cb_tree l;
  flag_execution_begin = EXECUTION_NORMAL;
  flag_execution_end = EXECUTION_NORMAL;
  for (l = prog->exec_list; l; l = CB_CHAIN(l)) {
    joutput_stmt(CB_VALUE(l), JOUTPUT_STMT_DEFAULT);
  }

  flag_execution_end = EXECUTION_LAST;
  flag_execution_begin = EXECUTION_ERROR_HANDLER;
  /* Error handlers */
  if (prog->file_list || prog->gen_file_error) {
    joutput_newline();
    int seen = 0;
    int i = 0;
    for (i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; i++) {
      if (prog->global_handler[i].handler_label) {
        seen = 1;
        break;
      }
    }
    joutput_stmt(cb_standard_error_handler, JOUTPUT_STMT_DEFAULT);
    joutput_newline();
    if (seen) {
      joutput_line("switch (CobolFile.errorFile.last_open_mode)");
      joutput_indent("{");
      for (i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; i++) {
        struct handler_struct *hstr = &prog->global_handler[i];
        if (hstr->handler_label) {
          joutput_line("case %d:", i);
          joutput_indent("{");
          if (prog == hstr->handler_prog) {
            joutput_perform_call(hstr->handler_label, hstr->handler_label);
          } else {
            if (cb_flag_traceall) {
              joutput_line("CobolUtil.resetTrace ();");
            }
            joutput_prefix();
            joutput("%s_ (%d", hstr->handler_prog->program_id,
                    hstr->handler_label->id);
            int parmnum = cb_list_length(hstr->handler_prog->parameter_list);
            int n;
            for (n = 0; n < parmnum; n++) {
              joutput(", null");
            }
            joutput(");\n");
            if (cb_flag_traceall) {
              joutput_line("CobolUti.readyTrace ();");
            }
          }
          joutput_line("break;");
          joutput_indent("}");
        }
      }
      joutput_line("default:");
      joutput_indent("{");
    }
    joutput_line("if ((CobolFile.errorFile.flag_select_features & "
                 "CobolFile.COB_SELECT_FILE_STATUS) == 0) {");
    switch (cb_abort_on_io_exception) {
    case CB_ABORT_ON_IO_ANY:
      joutput_line("	CobolFile.defaultErrorHandle ();");
      joutput_line("	CobolStopRunException.stopRunAndThrow (1);");
      break;
    case CB_ABORT_ON_IO_FATAL:
      joutput_line("	if (CobolFile.errorFile.file_status[0] == '3'");
      joutput_line("	    || CobolFile.errorFile.file_status[0] == '4'");
      joutput_line("	    || CobolFile.errorFile.file_status[0] == '9') {");
      joutput_line("		CobolFile.defaultErrorHandle ();");
      joutput_line("		CobolStopRunException.stopRunAndThrow (1);");
      joutput_line("	}");
      break;
    case CB_ABORT_ON_IO_NEVER:
    default:
      joutput_line("	/* Do nothing on unchecked file error status. */");
      joutput_line("	/*  (abort-on-io-exception is set to 'never') */");
      break;
    }
    joutput_line("}");
    if (seen) {
      joutput_line("break;");
      joutput_indent("}");
      joutput_indent("}");
    }
    joutput_newline();
  }

  joutput_line("return Optional.of(CobolControl.pure());");
  joutput_indent_level -= 2;
  joutput_line("}");
  joutput_indent_level -= 2;
  joutput_line("},");

  joutput_line("CobolControl.pure()");
  joutput_indent_level -= 2;
  joutput_line("};");
}

static void joutput_execution_entry_func() {
  joutput_line("public void execEntry(int start) throws CobolRuntimeException, "
               "CobolGoBackException, CobolStopRunException {");
  joutput_indent_level += 2;
  joutput_line(
      "Optional<CobolControl> nextLabel = Optional.of(contList[start]);");
  joutput_line("while(nextLabel.isPresent()) {");
  joutput_indent_level += 2;
  joutput_line("CobolControl section = nextLabel.get();");
  joutput_line("nextLabel = section.run();");
  joutput_indent_level -= 2;
  joutput_line("}");
  joutput_indent_level -= 2;
  joutput_line("}");
}

void codegen(struct cb_program *prog, const int nested, char **program_id_list,
             char *java_source_dir, char *source_file) {

  int i;
  cb_tree l;
  struct field_list *k;
  struct call_list *clp;
  struct cb_program *cp;
  time_t loctime;

  /* Clear local program stuff */
  current_prog = prog;
  param_id = 0;
  stack_id = 0;
  num_cob_fields = 0;
  progid = 0;
  loop_counter = 0;
  joutput_indent_level = 0;
  last_line = 0;
  needs_exit_prog = 0;
  gen_custom = 0;
  call_cache = NULL;
  while (call_parameter_cache) {
    struct call_parameter_list *next = call_parameter_cache->next;
    free(call_parameter_cache);
    call_parameter_cache = next;
  }
  call_parameter_cache = NULL;
  label_cache = NULL;
  local_cache = NULL;
  excp_current_program_id = prog->orig_source_name;
  excp_current_section = NULL;
  excp_current_paragraph = NULL;
  memset((char *)i_counters, 0, sizeof(i_counters));
  comment_info_cursor = comment_info_list_head;
  working_storage_comment_info_cursor = comment_info_list_head;
  translating_source_file = source_file;

  // modify 8/29 11:00
  joutput_target = yyout;
  char java_file_name[1024];
  sprintf(java_file_name, "%s/%s.java", java_source_dir, prog->program_id);
  *program_id_list = (char *)prog->program_id;
  joutput_target = fopen(java_file_name, "w");

  init_string_literal_list();

  if (!nested) {
    gen_ebcdic = 0;
    gen_ebcdic_ascii = 0;
    gen_full_ebcdic = 0;
    gen_native = 0;
    attr_cache = NULL;
    base_cache = NULL;
    literal_cache = NULL;
    field_cache = NULL;

    loctime = time(NULL);
    char locbuff[48];
    strftime(locbuff, sizeof(locbuff) - 1, "%b %d %Y %H:%M:%S %Z",
             localtime(&loctime));

    for (cp = prog; cp; cp = cp->next_program) {
      /* Build parameter list */
      for (l = cp->entry_list; l; l = CB_CHAIN(l)) {
        cb_tree l1;
        for (l1 = CB_VALUE(l); l1; l1 = CB_CHAIN(l1)) {
          cb_tree l2;
          for (l2 = cp->parameter_list; l2; l2 = CB_CHAIN(l2)) {
            if (cobc_casecmp(cb_field(CB_VALUE(l1))->name,
                             cb_field(CB_VALUE(l2))->name) == 0) {
              break;
            }
          }
          if (l2 == NULL) {
            cp->parameter_list = cb_list_add(cp->parameter_list, CB_VALUE(l1));
          }
        }
      }
      if (cp->flag_main) {
        // output ("int %s ();\n", cp->program_id);
      } else {
        for (l = cp->entry_list; l; l = CB_CHAIN(l)) {
          // output_entry_function (cp, l, cp->parameter_list, 0);
        }
      }
      // output ("static int %s_ (const int", cp->program_id);
      if (!cp->flag_chained) {
        for (l = cp->parameter_list; l; l = CB_CHAIN(l)) {
          // output (", unsigned char *");
        }
      }
      // output (");\n");
    }
    // output ("\n");
  }

  write_json_info(prog);

  call_parameters = prog->parameter_list;
  joutput_line("/* Generated by %s %s */", PACKAGE_NAME, PACKAGE_VERSION);

  if (cb_java_package_name) {
    joutput_line("package %s;\n", cb_java_package_name);
  }
  if (edit_code_command_is_set) {
    joutput_edit_code_command("file-header");
  }

  joutput_line("import java.io.UnsupportedEncodingException;");
  joutput_line("import jp.osscons.opensourcecobol.libcobj.*;");
  joutput_line("import jp.osscons.opensourcecobol.libcobj.common.*;");
  joutput_line("import jp.osscons.opensourcecobol.libcobj.data.*;");
  joutput_line("import jp.osscons.opensourcecobol.libcobj.exceptions.*;");
  joutput_line("import jp.osscons.opensourcecobol.libcobj.termio.*;");
  joutput_line("import jp.osscons.opensourcecobol.libcobj.call.*;");
  joutput_line("import jp.osscons.opensourcecobol.libcobj.file.*;");
  joutput_line("import jp.osscons.opensourcecobol.libcobj.ui.*;");
  joutput_line("import java.util.Optional;");
  joutput_line("import java.lang.NullPointerException;");
  joutput_line("import java.lang.IndexOutOfBoundsException;");
  joutput("\n");

  /*if (!cb_flag_no_cobol_comment) {
    for (; comment_info_cursor;
         comment_info_cursor = comment_info_cursor->next) {
      if (comment_info_cursor->is_base_cobol_file &&
          comment_info_cursor->line <= identification_division_line_number) {
        joutput_cobol_comment_before_identification_division(
            comment_info_cursor);
      } else {
        break;
      }
    }
  }*/

  if (edit_code_command_is_set) {
    joutput_edit_code_command("main-class-annotation");
  }
  if (edit_code_command_is_set) {
    joutput("public class %s implements CobolRunnable, ", prog->program_id);
    joutput_edit_code_command("main-class-implements");
    joutput(" {\n");
  } else {
    joutput_line("public class %s implements CobolRunnable {",
                 prog->program_id);
  }
  joutput_indent_level += 2;
  joutput("\n");

  joutput_line("private boolean initialized = false;");
  joutput_line("private CobolModule cobolCurrentModule;");
  joutput_line("private CobolModule module;");
  joutput_line("private int entry;");
  joutput("\n");

  // output_storage ("union cob_call_union\tcob_unifunc;\n\n");
  joutput_line("private CobolRunnable cob_unifunc;\n");

  if (edit_code_command_is_set) {
    joutput_edit_code_command("main-class-contents");
  }
  joutput("\n");

  joutput_line("@Override");
  joutput_line("public int run(CobolDataStorage... argStorages) {");
  joutput_line("  return %s_(0, argStorages);", prog->program_id);
  joutput_line("}\n");

  joutput_line("@Override");
  joutput_line("public void cancel() {");
  joutput_line("  %s_(-1);", prog->program_id);
  joutput_line("}\n");

  joutput_line("@Override");
  joutput_line("public boolean isActive() {");
  joutput_line("  return false;");
  joutput_line("}\n");

  /* Class-names */
  if (!prog->nested_level && prog->class_name_list) {
    joutput("/* Class names */\n");
    for (l = prog->class_name_list; l; l = CB_CHAIN(l)) {
      joutput_class_name_definition(CB_CLASS_NAME(CB_VALUE(l)));
    }
  }

  /* Main function */
  // if (prog->flag_main) {
  // joutput_main_function (prog);
  //}

  /* Functions */
  if (!nested) {
    // output ("/* Functions */\n\n");
  }
  // for (l = prog->entry_list; l; l = CB_CHAIN (l)) {
  // joutput_entry_function (prog, l, prog->parameter_list, 1);
  //}

  create_label_id_map(prog);
  init_data_storage_list();
  joutput_java_entrypoint(prog, prog->parameter_list);
  joutput_internal_function(prog, prog->parameter_list);

  joutput_execution_list(prog);
  // Output rest COBOL comments
  /*if (!cb_flag_no_cobol_comment) {
    for (; comment_info_cursor;
         comment_info_cursor = comment_info_cursor->next) {
      if (!comment_info_cursor->is_base_cobol_file) {
        continue;
      }
      joutput_cobol_comment(comment_info_cursor);
    }
  }*/
  joutput_execution_entry_func();

  if (has_external) {
    joutput_newline();
    joutput_line("/* EXTERNAL data initialization */");
    joutput_line("private void initExternProc () {");
    joutput_indent_level += 2;
    for (k = field_cache; k; k = k->next) {
      if (k->f->flag_item_external) {
        joutput_prefix();
        char *field_name = get_java_identifier_field(k->f);
        joutput("\t%s.setDataStorage(", field_name);
        free(field_name);
        joutput_data(k->x);
        joutput(");\n");
      }
    }
    joutput_indent_level -= 2;
    joutput_line("};");
  }
  joutput("\n");

  // output label procedure
  // EDIT
  // joutput_label_function();
  // joutput_entry_function();
  // joutput_close_buffer_list();

  if (!prog->next_program) {
    // output ("/* End functions */\n\n");
  }

  // mainメソッドの出力
  joutput_line("public static void main(String[] args)");
  joutput_line("{");
  joutput_indent_level += 2;

  // if (prog->flag_main) {
  joutput_line("CobolUtil.cob_init(args, false);");
  //}

  joutput_line("CobolDecimal.cobInitNumeric();");
  joutput_line("new %s().%s_(0);", prog->program_id, prog->program_id);
  joutput_line("CobolStopRunException.stopRun();");
  joutput_indent_level -= 2;
  joutput_line("}\n");

  if (gen_native || gen_full_ebcdic || gen_ebcdic_ascii ||
      prog->alphabet_name_list) {
    (void)lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
  }

  /* Program local stuff */

  //コンストラクタの実装コードを出力
  //メンバ変数の初期化を行う
  joutput_line("public %s()", prog->program_id);
  joutput_line("{");
  joutput_line("  init();");
  joutput_line("}");
  joutput_newline();

  //メンバ変数の初期化メソッドを出力
  create_sorted_data_storage_cache();
  joutput_init_method(prog);
  joutput_newline();

  //メンバ変数の出力
  joutput_declare_member_variables(prog, prog->parameter_list);
  joutput("\n");

  // Output the declarations of string literals
  joutput_all_string_literals();
  free_string_literal_list();
  destroy_sorted_data_storage_cache();
  free_data_storage_list();
  /* Files */
  if (prog->file_list) {
    i = 0;
    for (l = prog->file_list; l; l = CB_CHAIN(l)) {
      i += joutput_file_allocation(CB_FILE(CB_VALUE(l)));
    }
    if (i) {
      joutput_line("\nprivate static Linage lingptr;\n");
    }
  }
  joutput("\n");

  joutput_line("/* Sections and Labels */");
  struct cb_label_id_map *label;
  for (label = label_id_map_head; label; label = label->next) {
    joutput_prefix();
    joutput("private final static int ");
    joutput_label_variable_name(label->label_name, label->key, label->section);
    joutput(" = %d;\n", label->val);
  }
  destroy_label_id_map();
  joutput("\n");

  /* CALL cache */
  if (call_cache) {
    // output_local ("\n/* Call pointers */\n");
    for (clp = call_cache; clp; clp = clp->next) {
      // output_local ("static union cob_call_union\tcall_%s = { NULL };\n",
      // clp->callname);
      joutput_line("private CobolRunnable call_%s = null;", clp->callname);
    }
    // output_local ("\n");
  }

  /* Local indexes */
  for (i = 0; i < COB_MAX_SUBSCRIPTS; i++) {
    if (i_counters[i]) {
      // output_local ("int\t\ti%d;\n", i);
    }
  }

  /* Local implicit fields */
  if (num_cob_fields) {
    // output_local ("\n/* Local cob_field items */\n");
    for (i = 0; i < num_cob_fields; i++) {
      // output_local ("cob_field\tf%d;\n", i);
    }
    // output_local ("\n");
  }

  /* Skip to next nested program */

  if (prog->next_program) {
    joutput_indent_level -= 2;
    joutput_line("}");
    fclose(joutput_target);
    ++program_id_list;
    codegen(prog->next_program, 1, program_id_list, java_source_dir,
            source_file);
    return;
  }

  /* Collating tables */
  if (gen_ebcdic) {
    joutput_indent("\n/* ASCII to EBCDIC translate table (restricted) */\n");
    joutput_indent("private static final byte[] cob_a2e = {\n");
    joutput_indent_level += 2;
    if (alt_ebcdic) {
      joutput_indent("(byte)0x00, (byte)0x01, (byte)0x02, (byte)0x03, "
                     "(byte)0x37, (byte)0x2D, (byte)0x2E, (byte)0x2F,");
      joutput_indent("(byte)0x16, (byte)0x05, (byte)0x25, (byte)0x0B, "
                     "(byte)0x0C, (byte)0x0D, (byte)0x0E, (byte)0x0F,");
      joutput_indent("(byte)0x10, (byte)0x11, (byte)0x12, (byte)0x13, "
                     "(byte)0x3C, (byte)0x3D, (byte)0x32, (byte)0x26,");
      joutput_indent("(byte)0x18, (byte)0x19, (byte)0x3F, (byte)0x27, "
                     "(byte)0x1C, (byte)0x1D, (byte)0x1E, (byte)0x1F,");
      joutput_indent("(byte)0x40, (byte)0x5A, (byte)0x7F, (byte)0x7B, "
                     "(byte)0x5B, (byte)0x6C, (byte)0x50, (byte)0x7D,");
      joutput_indent("(byte)0x4D, (byte)0x5D, (byte)0x5C, (byte)0x4E, "
                     "(byte)0x6B, (byte)0x60, (byte)0x4B, (byte)0x61,");
      joutput_indent("(byte)0xF0, (byte)0xF1, (byte)0xF2, (byte)0xF3, "
                     "(byte)0xF4, (byte)0xF5, (byte)0xF6, (byte)0xF7,");
      joutput_indent("(byte)0xF8, (byte)0xF9, (byte)0x7A, (byte)0x5E, "
                     "(byte)0x4C, (byte)0x7E, (byte)0x6E, (byte)0x6F,");
      joutput_indent("(byte)0x7C, (byte)0xC1, (byte)0xC2, (byte)0xC3, "
                     "(byte)0xC4, (byte)0xC5, (byte)0xC6, (byte)0xC7,");
      joutput_indent("(byte)0xC8, (byte)0xC9, (byte)0xD1, (byte)0xD2, "
                     "(byte)0xD3, (byte)0xD4, (byte)0xD5, (byte)0xD6,");
      joutput_indent("(byte)0xD7, (byte)0xD8, (byte)0xD9, (byte)0xE2, "
                     "(byte)0xE3, (byte)0xE4, (byte)0xE5, (byte)0xE6,");
      joutput_indent("(byte)0xE7, (byte)0xE8, (byte)0xE9, (byte)0xAD, "
                     "(byte)0xE0, (byte)0xBD, (byte)0x5F, (byte)0x6D,");
      joutput_indent("(byte)0x79, (byte)0x81, (byte)0x82, (byte)0x83, "
                     "(byte)0x84, (byte)0x85, (byte)0x86, (byte)0x87,");
      joutput_indent("(byte)0x88, (byte)0x89, (byte)0x91, (byte)0x92, "
                     "(byte)0x93, (byte)0x94, (byte)0x95, (byte)0x96,");
      joutput_indent("(byte)0x97, (byte)0x98, (byte)0x99, (byte)0xA2, "
                     "(byte)0xA3, (byte)0xA4, (byte)0xA5, (byte)0xA6,");
      joutput_indent("(byte)0xA7, (byte)0xA8, (byte)0xA9, (byte)0xC0, "
                     "(byte)0x6A, (byte)0xD0, (byte)0xA1, (byte)0x07,");
      joutput_indent("(byte)0x68, (byte)0xDC, (byte)0x51, (byte)0x42, "
                     "(byte)0x43, (byte)0x44, (byte)0x47, (byte)0x48,");
      joutput_indent("(byte)0x52, (byte)0x53, (byte)0x54, (byte)0x57, "
                     "(byte)0x56, (byte)0x58, (byte)0x63, (byte)0x67,");
      joutput_indent("(byte)0x71, (byte)0x9C, (byte)0x9E, (byte)0xCB, "
                     "(byte)0xCC, (byte)0xCD, (byte)0xDB, (byte)0xDD,");
      joutput_indent("(byte)0xDF, (byte)0xEC, (byte)0xFC, (byte)0xB0, "
                     "(byte)0xB1, (byte)0xB2, (byte)0x3E, (byte)0xB4,");
      joutput_indent("(byte)0x45, (byte)0x55, (byte)0xCE, (byte)0xDE, "
                     "(byte)0x49, (byte)0x69, (byte)0x9A, (byte)0x9B,");
      joutput_indent("(byte)0xAB, (byte)0x9F, (byte)0xBA, (byte)0xB8, "
                     "(byte)0xB7, (byte)0xAA, (byte)0x8A, (byte)0x8B,");
      joutput_indent("(byte)0xB6, (byte)0xB5, (byte)0x62, (byte)0x4F, "
                     "(byte)0x64, (byte)0x65, (byte)0x66, (byte)0x20,");
      joutput_indent("(byte)0x21, (byte)0x22, (byte)0x70, (byte)0x23, "
                     "(byte)0x72, (byte)0x73, (byte)0x74, (byte)0xBE,");
      joutput_indent("(byte)0x76, (byte)0x77, (byte)0x78, (byte)0x80, "
                     "(byte)0x24, (byte)0x15, (byte)0x8C, (byte)0x8D,");
      joutput_indent("(byte)0x8E, (byte)0x41, (byte)0x06, (byte)0x17, "
                     "(byte)0x28, (byte)0x29, (byte)0x9D, (byte)0x2A,");
      joutput_indent("(byte)0x2B, (byte)0x2C, (byte)0x09, (byte)0x0A, "
                     "(byte)0xAC, (byte)0x4A, (byte)0xAE, (byte)0xAF,");
      joutput_indent("(byte)0x1B, (byte)0x30, (byte)0x31, (byte)0xFA, "
                     "(byte)0x1A, (byte)0x33, (byte)0x34, (byte)0x35,");
      joutput_indent("(byte)0x36, (byte)0x59, (byte)0x08, (byte)0x38, "
                     "(byte)0xBC, (byte)0x39, (byte)0xA0, (byte)0xBF,");
      joutput_indent("(byte)0xCA, (byte)0x3A, (byte)0xFE, (byte)0x3B, "
                     "(byte)0x04, (byte)0xCF, (byte)0xDA, (byte)0x14,");
      joutput_indent("(byte)0xE1, (byte)0x8F, (byte)0x46, (byte)0x75, "
                     "(byte)0xFD, (byte)0xEB, (byte)0xEE, (byte)0xED,");
      joutput_indent("(byte)0x90, (byte)0xEF, (byte)0xB3, (byte)0xFB, "
                     "(byte)0xB9, (byte)0xEA, (byte)0xBB, (byte)0xFF");
    } else {
      /* MF */
      joutput_indent("(byte)0x00, (byte)0x01, (byte)0x02, (byte)0x03, "
                     "(byte)0x1D, (byte)0x19, (byte)0x1A, (byte)0x1B,");
      joutput_indent("(byte)0x0F, (byte)0x04, (byte)0x16, (byte)0x06, "
                     "(byte)0x07, (byte)0x08, (byte)0x09, (byte)0x0A,");
      joutput_indent("(byte)0x0B, (byte)0x0C, (byte)0x0D, (byte)0x0E, "
                     "(byte)0x1E, (byte)0x1F, (byte)0x1C, (byte)0x17,");
      joutput_indent("(byte)0x10, (byte)0x11, (byte)0x20, (byte)0x18, "
                     "(byte)0x12, (byte)0x13, (byte)0x14, (byte)0x15,");
      joutput_indent("(byte)0x21, (byte)0x27, (byte)0x3A, (byte)0x36, "
                     "(byte)0x28, (byte)0x30, (byte)0x26, (byte)0x38,");
      joutput_indent("(byte)0x24, (byte)0x2A, (byte)0x29, (byte)0x25, "
                     "(byte)0x2F, (byte)0x2C, (byte)0x22, (byte)0x2D,");
      joutput_indent("(byte)0x73, (byte)0x74, (byte)0x75, (byte)0x76, "
                     "(byte)0x77, (byte)0x78, (byte)0x79, (byte)0x7A,");
      joutput_indent("(byte)0x7B, (byte)0x7C, (byte)0x35, (byte)0x2B, "
                     "(byte)0x23, (byte)0x39, (byte)0x32, (byte)0x33,");
      joutput_indent("(byte)0x37, (byte)0x57, (byte)0x58, (byte)0x59, "
                     "(byte)0x5A, (byte)0x5B, (byte)0x5C, (byte)0x5D,");
      joutput_indent("(byte)0x5E, (byte)0x5F, (byte)0x61, (byte)0x62, "
                     "(byte)0x63, (byte)0x64, (byte)0x65, (byte)0x66,");
      joutput_indent("(byte)0x67, (byte)0x68, (byte)0x69, (byte)0x6B, "
                     "(byte)0x6C, (byte)0x6D, (byte)0x6E, (byte)0x6F,");
      joutput_indent("(byte)0x70, (byte)0x71, (byte)0x72, (byte)0x7D, "
                     "(byte)0x6A, (byte)0x7E, (byte)0x7F, (byte)0x31,");
      joutput_indent("(byte)0x34, (byte)0x3B, (byte)0x3C, (byte)0x3D, "
                     "(byte)0x3E, (byte)0x3F, (byte)0x40, (byte)0x41,");
      joutput_indent("(byte)0x42, (byte)0x43, (byte)0x44, (byte)0x45, "
                     "(byte)0x46, (byte)0x47, (byte)0x48, (byte)0x49,");
      joutput_indent("(byte)0x4A, (byte)0x4B, (byte)0x4C, (byte)0x4E, "
                     "(byte)0x4F, (byte)0x50, (byte)0x51, (byte)0x52,");
      joutput_indent("(byte)0x53, (byte)0x54, (byte)0x55, (byte)0x56, "
                     "(byte)0x2E, (byte)0x60, (byte)0x4D, (byte)0x05,");
      joutput_indent("(byte)0x80, (byte)0x81, (byte)0x82, (byte)0x83, "
                     "(byte)0x84, (byte)0x85, (byte)0x86, (byte)0x87,");
      joutput_indent("(byte)0x88, (byte)0x89, (byte)0x8A, (byte)0x8B, "
                     "(byte)0x8C, (byte)0x8D, (byte)0x8E, (byte)0x8F,");
      joutput_indent("(byte)0x90, (byte)0x91, (byte)0x92, (byte)0x93, "
                     "(byte)0x94, (byte)0x95, (byte)0x96, (byte)0x97,");
      joutput_indent("(byte)0x98, (byte)0x99, (byte)0x9A, (byte)0x9B, "
                     "(byte)0x9C, (byte)0x9D, (byte)0x9E, (byte)0x9F,");
      joutput_indent("(byte)0xA0, (byte)0xA1, (byte)0xA2, (byte)0xA3, "
                     "(byte)0xA4, (byte)0xA5, (byte)0xA6, (byte)0xA7,");
      joutput_indent("(byte)0xA8, (byte)0xA9, (byte)0xAA, (byte)0xAB, "
                     "(byte)0xAC, (byte)0xAD, (byte)0xAE, (byte)0xAF,");
      joutput_indent("(byte)0xB0, (byte)0xB1, (byte)0xB2, (byte)0xB3, "
                     "(byte)0xB4, (byte)0xB5, (byte)0xB6, (byte)0xB7,");
      joutput_indent("(byte)0xB8, (byte)0xB9, (byte)0xBA, (byte)0xBB, "
                     "(byte)0xBC, (byte)0xBD, (byte)0xBE, (byte)0xBF,");
      joutput_indent("(byte)0xC0, (byte)0xC1, (byte)0xC2, (byte)0xC3, "
                     "(byte)0xC4, (byte)0xC5, (byte)0xC6, (byte)0xC7,");
      joutput_indent("(byte)0xC8, (byte)0xC9, (byte)0xCA, (byte)0xCB, "
                     "(byte)0xCC, (byte)0xCD, (byte)0xCE, (byte)0xCF,");
      joutput_indent("(byte)0xD0, (byte)0xD1, (byte)0xD2, (byte)0xD3, "
                     "(byte)0xD4, (byte)0xD5, (byte)0xD6, (byte)0xD7,");
      joutput_indent("(byte)0xD8, (byte)0xD9, (byte)0xDA, (byte)0xDB, "
                     "(byte)0xDC, (byte)0xDD, (byte)0xDE, (byte)0xDF,");
      joutput_indent("(byte)0xE0, (byte)0xE1, (byte)0xE2, (byte)0xE3, "
                     "(byte)0xE4, (byte)0xE5, (byte)0xE6, (byte)0xE7,");
      joutput_indent("(byte)0xE8, (byte)0xE9, (byte)0xEA, (byte)0xEB, "
                     "(byte)0xEC, (byte)0xED, (byte)0xEE, (byte)0xEF,");
      joutput_indent("(byte)0xF0, (byte)0xF1, (byte)0xF2, (byte)0xF3, "
                     "(byte)0xF4, (byte)0xF5, (byte)0xF6, (byte)0xF7,");
      joutput_indent("(byte)0xF8, (byte)0xF9, (byte)0xFA, (byte)0xFB, "
                     "(byte)0xFC, (byte)0xFD, (byte)0xFE, (byte)0xFF");
    }

    joutput_indent_level -= 2;
    joutput_indent("};\n");
    joutput("\n");
  }
  if (gen_full_ebcdic) {
    joutput_indent("\n/* ASCII to EBCDIC table */\n");
    joutput_indent("private static final byte[] cob_ebcdic = {");
    joutput_indent_level += 2;
    joutput_indent("(byte)0x00, (byte)0x01, (byte)0x02, (byte)0x03, "
                   "(byte)0x37, (byte)0x2D, (byte)0x2E, (byte)0x2F,");
    joutput_indent("(byte)0x16, (byte)0x05, (byte)0x25, (byte)0x0B, "
                   "(byte)0x0C, (byte)0x0D, (byte)0x0E, (byte)0x0F,");
    joutput_indent("(byte)0x10, (byte)0x11, (byte)0x12, (byte)0x13, "
                   "(byte)0x3C, (byte)0x3D, (byte)0x32, (byte)0x26,");
    joutput_indent("(byte)0x18, (byte)0x19, (byte)0x3F, (byte)0x27, "
                   "(byte)0x1C, (byte)0x1D, (byte)0x1E, (byte)0x1F,");
    joutput_indent("(byte)0x40, (byte)0x5A, (byte)0x7F, (byte)0x7B, "
                   "(byte)0x5B, (byte)0x6C, (byte)0x50, (byte)0x7D,");
    joutput_indent("(byte)0x4D, (byte)0x5D, (byte)0x5C, (byte)0x4E, "
                   "(byte)0x6B, (byte)0x60, (byte)0x4B, (byte)0x61,");
    joutput_indent("(byte)0xF0, (byte)0xF1, (byte)0xF2, (byte)0xF3, "
                   "(byte)0xF4, (byte)0xF5, (byte)0xF6, (byte)0xF7,");
    joutput_indent("(byte)0xF8, (byte)0xF9, (byte)0x7A, (byte)0x5E, "
                   "(byte)0x4C, (byte)0x7E, (byte)0x6E, (byte)0x6F,");
    joutput_indent("(byte)0x7C, (byte)0xC1, (byte)0xC2, (byte)0xC3, "
                   "(byte)0xC4, (byte)0xC5, (byte)0xC6, (byte)0xC7,");
    joutput_indent("(byte)0xC8, (byte)0xC9, (byte)0xD1, (byte)0xD2, "
                   "(byte)0xD3, (byte)0xD4, (byte)0xD5, (byte)0xD6,");
    joutput_indent("(byte)0xD7, (byte)0xD8, (byte)0xD9, (byte)0xE2, "
                   "(byte)0xE3, (byte)0xE4, (byte)0xE5, (byte)0xE6,");
    joutput_indent("(byte)0xE7, (byte)0xE8, (byte)0xE9, (byte)0xAD, "
                   "(byte)0xE0, (byte)0xBD, (byte)0x5F, (byte)0x6D,");
    joutput_indent("(byte)0x79, (byte)0x81, (byte)0x82, (byte)0x83, "
                   "(byte)0x84, (byte)0x85, (byte)0x86, (byte)0x87,");
    joutput_indent("(byte)0x88, (byte)0x89, (byte)0x91, (byte)0x92, "
                   "(byte)0x93, (byte)0x94, (byte)0x95, (byte)0x96,");
    joutput_indent("(byte)0x97, (byte)0x98, (byte)0x99, (byte)0xA2, "
                   "(byte)0xA3, (byte)0xA4, (byte)0xA5, (byte)0xA6,");
    joutput_indent("(byte)0xA7, (byte)0xA8, (byte)0xA9, (byte)0xC0, "
                   "(byte)0x6A, (byte)0xD0, (byte)0xA1, (byte)0x07,");
    joutput_indent("(byte)0x68, (byte)0xDC, (byte)0x51, (byte)0x42, "
                   "(byte)0x43, (byte)0x44, (byte)0x47, (byte)0x48,");
    joutput_indent("(byte)0x52, (byte)0x53, (byte)0x54, (byte)0x57, "
                   "(byte)0x56, (byte)0x58, (byte)0x63, (byte)0x67,");
    joutput_indent("(byte)0x71, (byte)0x9C, (byte)0x9E, (byte)0xCB, "
                   "(byte)0xCC, (byte)0xCD, (byte)0xDB, (byte)0xDD,");
    joutput_indent("(byte)0xDF, (byte)0xEC, (byte)0xFC, (byte)0xB0, "
                   "(byte)0xB1, (byte)0xB2, (byte)0x3E, (byte)0xB4,");
    joutput_indent("(byte)0x45, (byte)0x55, (byte)0xCE, (byte)0xDE, "
                   "(byte)0x49, (byte)0x69, (byte)0x9A, (byte)0x9B,");
    joutput_indent("(byte)0xAB, (byte)0x9F, (byte)0xBA, (byte)0xB8, "
                   "(byte)0xB7, (byte)0xAA, (byte)0x8A, (byte)0x8B,");
    joutput_indent("(byte)0xB6, (byte)0xB5, (byte)0x62, (byte)0x4F, "
                   "(byte)0x64, (byte)0x65, (byte)0x66, (byte)0x20,");
    joutput_indent("(byte)0x21, (byte)0x22, (byte)0x70, (byte)0x23, "
                   "(byte)0x72, (byte)0x73, (byte)0x74, (byte)0xBE,");
    joutput_indent("(byte)0x76, (byte)0x77, (byte)0x78, (byte)0x80, "
                   "(byte)0x24, (byte)0x15, (byte)0x8C, (byte)0x8D,");
    joutput_indent("(byte)0x8E, (byte)0x41, (byte)0x06, (byte)0x17, "
                   "(byte)0x28, (byte)0x29, (byte)0x9D, (byte)0x2A,");
    joutput_indent("(byte)0x2B, (byte)0x2C, (byte)0x09, (byte)0x0A, "
                   "(byte)0xAC, (byte)0x4A, (byte)0xAE, (byte)0xAF,");
    joutput_indent("(byte)0x1B, (byte)0x30, (byte)0x31, (byte)0xFA, "
                   "(byte)0x1A, (byte)0x33, (byte)0x34, (byte)0x35,");
    joutput_indent("(byte)0x36, (byte)0x59, (byte)0x08, (byte)0x38, "
                   "(byte)0xBC, (byte)0x39, (byte)0xA0, (byte)0xBF,");
    joutput_indent("(byte)0xCA, (byte)0x3A, (byte)0xFE, (byte)0x3B, "
                   "(byte)0x04, (byte)0xCF, (byte)0xDA, (byte)0x14,");
    joutput_indent("(byte)0xE1, (byte)0x8F, (byte)0x46, (byte)0x75, "
                   "(byte)0xFD, (byte)0xEB, (byte)0xEE, (byte)0xED,");
    joutput_indent("(byte)0x90, (byte)0xEF, (byte)0xB3, (byte)0xFB, "
                   "(byte)0xB9, (byte)0xEA, (byte)0xBB, (byte)0xFF");

    joutput_indent_level -= 2;
    joutput_indent("};\n");

    i = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
    joutput("  ");
    joutput("private static AbstractCobolField %sebcdic = "
            "CobolFieldFactori.makeField(256, new "
            "CobolDataStorage(cob_ebcdic), %s%d);\n",
            CB_PREFIX_FIELD, CB_PREFIX_ATTR, i);
    joutput("\n");
  }
  if (gen_ebcdic_ascii) {
    joutput_indent("\n/* EBCDIC to ASCII table */\n");
    joutput_indent("private static final byte[] cob_ebcdic_ascii = {");
    joutput_indent_level += 2;
    joutput_indent("(byte)0x00, (byte)0x01, (byte)0x02, (byte)0x03, "
                   "(byte)0xEC, (byte)0x09, (byte)0xCA, (byte)0x7F,");
    joutput_indent("(byte)0xE2, (byte)0xD2, (byte)0xD3, (byte)0x0B, "
                   "(byte)0x0C, (byte)0x0D, (byte)0x0E, (byte)0x0F,");
    joutput_indent("(byte)0x10, (byte)0x11, (byte)0x12, (byte)0x13, "
                   "(byte)0xEF, (byte)0xC5, (byte)0x08, (byte)0xCB,");
    joutput_indent("(byte)0x18, (byte)0x19, (byte)0xDC, (byte)0xD8, "
                   "(byte)0x1C, (byte)0x1D, (byte)0x1E, (byte)0x1F,");
    joutput_indent("(byte)0xB7, (byte)0xB8, (byte)0xB9, (byte)0xBB, "
                   "(byte)0xC4, (byte)0x0A, (byte)0x17, (byte)0x1B,");
    joutput_indent("(byte)0xCC, (byte)0xCD, (byte)0xCF, (byte)0xD0, "
                   "(byte)0xD1, (byte)0x05, (byte)0x06, (byte)0x07,");
    joutput_indent("(byte)0xD9, (byte)0xDA, (byte)0x16, (byte)0xDD, "
                   "(byte)0xDE, (byte)0xDF, (byte)0xE0, (byte)0x04,");
    joutput_indent("(byte)0xE3, (byte)0xE5, (byte)0xE9, (byte)0xEB, "
                   "(byte)0x14, (byte)0x15, (byte)0x9E, (byte)0x1A,");
    joutput_indent("(byte)0x20, (byte)0xC9, (byte)0x83, (byte)0x84, "
                   "(byte)0x85, (byte)0xA0, (byte)0xF2, (byte)0x86,");
    joutput_indent("(byte)0x87, (byte)0xA4, (byte)0xD5, (byte)0x2E, "
                   "(byte)0x3C, (byte)0x28, (byte)0x2B, (byte)0xB3,");
    joutput_indent("(byte)0x26, (byte)0x82, (byte)0x88, (byte)0x89, "
                   "(byte)0x8A, (byte)0xA1, (byte)0x8C, (byte)0x8B,");
    joutput_indent("(byte)0x8D, (byte)0xE1, (byte)0x21, (byte)0x24, "
                   "(byte)0x2A, (byte)0x29, (byte)0x3B, (byte)0x5E,");
    joutput_indent("(byte)0x2D, (byte)0x2F, (byte)0xB2, (byte)0x8E, "
                   "(byte)0xB4, (byte)0xB5, (byte)0xB6, (byte)0x8F,");
    joutput_indent("(byte)0x80, (byte)0xA5, (byte)0x7C, (byte)0x2C, "
                   "(byte)0x25, (byte)0x5F, (byte)0x3E, (byte)0x3F,");
    joutput_indent("(byte)0xBA, (byte)0x90, (byte)0xBC, (byte)0xBD, "
                   "(byte)0xBE, (byte)0xF3, (byte)0xC0, (byte)0xC1,");
    joutput_indent("(byte)0xC2, (byte)0x60, (byte)0x3A, (byte)0x23, "
                   "(byte)0x40, (byte)0x27, (byte)0x3D, (byte)0x22,");
    joutput_indent("(byte)0xC3, (byte)0x61, (byte)0x62, (byte)0x63, "
                   "(byte)0x64, (byte)0x65, (byte)0x66, (byte)0x67,");
    joutput_indent("(byte)0x68, (byte)0x69, (byte)0xAE, (byte)0xAF, "
                   "(byte)0xC6, (byte)0xC7, (byte)0xC8, (byte)0xF1,");
    joutput_indent("(byte)0xF8, (byte)0x6A, (byte)0x6B, (byte)0x6C, "
                   "(byte)0x6D, (byte)0x6E, (byte)0x6F, (byte)0x70,");
    joutput_indent("(byte)0x71, (byte)0x72, (byte)0xA6, (byte)0xA7, "
                   "(byte)0x91, (byte)0xCE, (byte)0x92, (byte)0xA9,");
    joutput_indent("(byte)0xE6, (byte)0x7E, (byte)0x73, (byte)0x74, "
                   "(byte)0x75, (byte)0x76, (byte)0x77, (byte)0x78,");
    joutput_indent("(byte)0x79, (byte)0x7A, (byte)0xAD, (byte)0xA8, "
                   "(byte)0xD4, (byte)0x5B, (byte)0xD6, (byte)0xD7,");
    joutput_indent("(byte)0x9B, (byte)0x9C, (byte)0x9D, (byte)0xFA, "
                   "(byte)0x9F, (byte)0xB1, (byte)0xB0, (byte)0xAC,");
    joutput_indent("(byte)0xAB, (byte)0xFC, (byte)0xAA, (byte)0xFE, "
                   "(byte)0xE4, (byte)0x5D, (byte)0xBF, (byte)0xE7,");
    joutput_indent("(byte)0x7B, (byte)0x41, (byte)0x42, (byte)0x43, "
                   "(byte)0x44, (byte)0x45, (byte)0x46, (byte)0x47,");
    joutput_indent("(byte)0x48, (byte)0x49, (byte)0xE8, (byte)0x93, "
                   "(byte)0x94, (byte)0x95, (byte)0xA2, (byte)0xED,");
    joutput_indent("(byte)0x7D, (byte)0x4A, (byte)0x4B, (byte)0x4C, "
                   "(byte)0x4D, (byte)0x4E, (byte)0x4F, (byte)0x50,");
    joutput_indent("(byte)0x51, (byte)0x52, (byte)0xEE, (byte)0x96, "
                   "(byte)0x81, (byte)0x97, (byte)0xA3, (byte)0x98,");
    joutput_indent("(byte)0x5C, (byte)0xF0, (byte)0x53, (byte)0x54, "
                   "(byte)0x55, (byte)0x56, (byte)0x57, (byte)0x58,");
    joutput_indent("(byte)0x59, (byte)0x5A, (byte)0xFD, (byte)0xF5, "
                   "(byte)0x99, (byte)0xF7, (byte)0xF6, (byte)0xF9,");
    joutput_indent("(byte)0x30, (byte)0x31, (byte)0x32, (byte)0x33, "
                   "(byte)0x34, (byte)0x35, (byte)0x36, (byte)0x37,");
    joutput_indent("(byte)0x38, (byte)0x39, (byte)0xDB, (byte)0xFB, "
                   "(byte)0x9A, (byte)0xF4, (byte)0xEA, (byte)0xFF");

    joutput_indent_level -= 2;
    joutput_indent("};\n");

    i = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
    joutput("  ");
    joutput("private static AbstractCobolField %sebcdic_ascii = "
            "CobolFieldFactory.makeField(256, new "
            "CobolDataStorage(cob_ebcdic_ascii), %s%d);\n",
            CB_PREFIX_FIELD, CB_PREFIX_ATTR, i);
    joutput("\n");
  }
  if (gen_native) {
    joutput_indent("\n/* NATIVE table */\n");
    joutput_indent("private static final byte[] cob_native = {");
    joutput_indent_level += 2;
    joutput_indent("(byte)0, (byte)1, (byte)2, (byte)3, (byte)4, (byte)5, "
                   "(byte)6, (byte)7,");
    joutput_indent("(byte)8, (byte)9, (byte)10, (byte)11, (byte)12, (byte)13, "
                   "(byte)14, (byte)15,");
    joutput_indent("(byte)16, (byte)17, (byte)18, (byte)19, (byte)20, "
                   "(byte)21, (byte)22, (byte)23,");
    joutput_indent("(byte)24, (byte)25, (byte)26, (byte)27, (byte)28, "
                   "(byte)29, (byte)30, (byte)31,");
    joutput_indent("(byte)32, (byte)33, (byte)34, (byte)35, (byte)36, "
                   "(byte)37, (byte)38, (byte)39,");
    joutput_indent("(byte)40, (byte)41, (byte)42, (byte)43, (byte)44, "
                   "(byte)45, (byte)46, (byte)47,");
    joutput_indent("(byte)48, (byte)49, (byte)50, (byte)51, (byte)52, "
                   "(byte)53, (byte)54, (byte)55,");
    joutput_indent("(byte)56, (byte)57, (byte)58, (byte)59, (byte)60, "
                   "(byte)61, (byte)62, (byte)63,");
    joutput_indent("(byte)64, (byte)65, (byte)66, (byte)67, (byte)68, "
                   "(byte)69, (byte)70, (byte)71,");
    joutput_indent("(byte)72, (byte)73, (byte)74, (byte)75, (byte)76, "
                   "(byte)77, (byte)78, (byte)79,");
    joutput_indent("(byte)80, (byte)81, (byte)82, (byte)83, (byte)84, "
                   "(byte)85, (byte)86, (byte)87,");
    joutput_indent("(byte)88, (byte)89, (byte)90, (byte)91, (byte)92, "
                   "(byte)93, (byte)94, (byte)95,");
    joutput_indent("(byte)96, (byte)97, (byte)98, (byte)99, (byte)100, "
                   "(byte)101, (byte)102, (byte)103,");
    joutput_indent("(byte)104, (byte)105, (byte)106, (byte)107, (byte)108, "
                   "(byte)109, (byte)110, (byte)111,");
    joutput_indent("(byte)112, (byte)113, (byte)114, (byte)115, (byte)116, "
                   "(byte)117, (byte)118, (byte)119,");
    joutput_indent("(byte)120, (byte)121, (byte)122, (byte)123, (byte)124, "
                   "(byte)125, (byte)126, (byte)127,");
    joutput_indent("(byte)128, (byte)129, (byte)130, (byte)131, (byte)132, "
                   "(byte)133, (byte)134, (byte)135,");
    joutput_indent("(byte)136, (byte)137, (byte)138, (byte)139, (byte)140, "
                   "(byte)141, (byte)142, (byte)143,");
    joutput_indent("(byte)144, (byte)145, (byte)146, (byte)147, (byte)148, "
                   "(byte)149, (byte)150, (byte)151,");
    joutput_indent("(byte)152, (byte)153, (byte)154, (byte)155, (byte)156, "
                   "(byte)157, (byte)158, (byte)159,");
    joutput_indent("(byte)160, (byte)161, (byte)162, (byte)163, (byte)164, "
                   "(byte)165, (byte)166, (byte)167,");
    joutput_indent("(byte)168, (byte)169, (byte)170, (byte)171, (byte)172, "
                   "(byte)173, (byte)174, (byte)175,");
    joutput_indent("(byte)176, (byte)177, (byte)178, (byte)179, (byte)180, "
                   "(byte)181, (byte)182, (byte)183,");
    joutput_indent("(byte)184, (byte)185, (byte)186, (byte)187, (byte)188, "
                   "(byte)189, (byte)190, (byte)191,");
    joutput_indent("(byte)192, (byte)193, (byte)194, (byte)195, (byte)196, "
                   "(byte)197, (byte)198, (byte)199,");
    joutput_indent("(byte)200, (byte)201, (byte)202, (byte)203, (byte)204, "
                   "(byte)205, (byte)206, (byte)207,");
    joutput_indent("(byte)208, (byte)209, (byte)210, (byte)211, (byte)212, "
                   "(byte)213, (byte)214, (byte)215,");
    joutput_indent("(byte)216, (byte)217, (byte)218, (byte)219, (byte)220, "
                   "(byte)221, (byte)222, (byte)223,");
    joutput_indent("(byte)224, (byte)225, (byte)226, (byte)227, (byte)228, "
                   "(byte)229, (byte)230, (byte)231,");
    joutput_indent("(byte)232, (byte)233, (byte)234, (byte)235, (byte)236, "
                   "(byte)237, (byte)238, (byte)239,");
    joutput_indent("(byte)240, (byte)241, (byte)242, (byte)243, (byte)244, "
                   "(byte)245, (byte)246, (byte)247,");
    joutput_indent("(byte)248, (byte)249, (byte)250, (byte)251, (byte)252, "
                   "(byte)253, (byte)254, (byte)255");

    joutput_indent_level -= 2;
    joutput_indent("};\n");

    // i = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
    // joutput("  ");
    // joutput
    //     ("private static AbstractCobolField f_native =
    //     CobolFieldFactory.makeField(256, new CobolDataStorage(cob_native),
    //     %s%d);\n",
    //      CB_PREFIX_ATTR, i);
    // joutput ("\n");
  }

  joutput_indent_level -= 2;
  joutput_line("}");
  fclose(joutput_target);
}
