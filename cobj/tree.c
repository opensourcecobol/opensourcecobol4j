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

#include "config.h"

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cobj.h"
#include "tree.h"

#define PIC_ALPHABETIC 0x01
#define PIC_NUMERIC 0x02
#define PIC_NATIONAL 0x04
#define PIC_EDITED 0x08
#define PIC_ALPHANUMERIC (PIC_ALPHABETIC | PIC_NUMERIC)
#define PIC_ALPHABETIC_EDITED (PIC_ALPHABETIC | PIC_EDITED)
#define PIC_ALPHANUMERIC_EDITED (PIC_ALPHANUMERIC | PIC_EDITED)
#define PIC_NUMERIC_EDITED (PIC_NUMERIC | PIC_EDITED)
#define PIC_NATIONAL_EDITED (PIC_NATIONAL | PIC_EDITED)

/* Local macros */

#define CHARACTER_LENGTH_OVERFLOW (-1)
#define INCREASE_CHARACTER_LENGTH(increase)                                    \
  increase_character_length(&character_length, (increase))
#define CHECK_CHARACTER_LENGTH(limit, msg)                                     \
  {                                                                            \
    if (character_length == CHARACTER_LENGTH_OVERFLOW ||                       \
        character_length > (limit)) {                                          \
      cb_error(_(msg), (limit));                                               \
    }                                                                          \
  }

/* Local variables */

static const enum cb_class category_to_class_table[] = {
    CB_CLASS_UNKNOWN,      /* CB_CATEGORY_UNKNOWN */
    CB_CLASS_ALPHABETIC,   /* CB_CATEGORY_ALPHABETIC */
    CB_CLASS_ALPHANUMERIC, /* CB_CATEGORY_ALPHANUMERIC */
    CB_CLASS_ALPHANUMERIC, /* CB_CATEGORY_ALPHANUMERIC_EDITED */
    CB_CLASS_BOOLEAN,      /* CB_CATEGORY_BOOLEAN */
    CB_CLASS_INDEX,        /* CB_CATEGORY_INDEX */
    CB_CLASS_NATIONAL,     /* CB_CATEGORY_NATIONAL */
    CB_CLASS_NATIONAL,     /* CB_CATEGORY_NATIONAL_EDITED */
    CB_CLASS_NUMERIC,      /* CB_CATEGORY_NUMERIC */
    CB_CLASS_ALPHANUMERIC, /* CB_CATEGORY_NUMERIC_EDITED */
    CB_CLASS_OBJECT,       /* CB_CATEGORY_OBJECT_REFERENCE */
    CB_CLASS_POINTER,      /* CB_CATEGORY_DATA_POINTER */
    CB_CLASS_POINTER,      /* CB_CATEGORY_PROGRAM_POINTER */
};

static struct int_node {
  struct int_node *next;
  cb_tree node;
  int n;
} *int_node_table = NULL;

static char *treenamebuff = NULL;
static int filler_id = 1;
static int anonymous_id = 1;

/* Global variables */

/*
 * Constants
 */

cb_tree cb_any;
cb_tree cb_true;
cb_tree cb_false;
cb_tree cb_null;
cb_tree cb_zero;
cb_tree cb_zen_zero;
cb_tree cb_one;
cb_tree cb_space;
cb_tree cb_blank;
cb_tree cb_low;
cb_tree cb_high;
cb_tree cb_norm_low;
cb_tree cb_norm_high;
cb_tree cb_quote;
cb_tree cb_int0;
cb_tree cb_int1;
cb_tree cb_int2;
cb_tree cb_int3;
cb_tree cb_int4;
cb_tree cb_int5;
cb_tree cb_i[8];
cb_tree cb_error_node;

cb_tree cb_intr_whencomp;
cb_tree cb_intr_pi;
cb_tree cb_intr_e;

cb_tree cb_standard_error_handler;

size_t gen_screen_ptr = 0;

/* Local functions */

static char *to_cname(const char *s) {
  char *copy;
  unsigned char *p;

  copy = strdup(s);
  for (p = (unsigned char *)copy; *p;) {
    if ((0x81 <= *p && *p <= 0x9F) || (0xE0 <= *p && *p <= 0xFC)) {
      p += 2;
    } else {
      *p = (*p == '-') ? '_' : (unsigned char)toupper(*p);
      p++;
    }
  }
  return copy;
}

static size_t hash(const unsigned char *s) {
  size_t val = 0;

  for (; *s; s++) {
    val += toupper(*s);
  }
  return val % CB_WORD_HASH_SIZE;
}

static struct cb_word *lookup_word(const char *name) {
  struct cb_word *p;
  size_t val;

  val = hash((const unsigned char *)name);
  /* find the existing word */
  if (current_program) {
    for (p = current_program->word_table[val]; p; p = p->next) {
      if (cobc_casecmp(p->name, name) == 0) {
        return p;
      }
    }
  }

  /* create new word */
  p = cobc_malloc(sizeof(struct cb_word));
  p->name = strdup(name);

  /* insert it into the table */
  if (current_program) {
    p->next = current_program->word_table[val];
    current_program->word_table[val] = p;
  }

  return p;
}

static void file_error(cb_tree name, const char *clause) {
  cb_error_x(name, _("%s clause is required for file '%s'"), clause,
             CB_NAME(name));
}

static void increase_character_length(int *character_length, int increase) {

  if (*character_length != CHARACTER_LENGTH_OVERFLOW) {
    if (increase == CHARACTER_LENGTH_OVERFLOW) {
      *character_length = CHARACTER_LENGTH_OVERFLOW;
    } else {
      unsigned int remain = INT_MAX - *character_length;
      if (remain < increase) {
        *character_length = CHARACTER_LENGTH_OVERFLOW;
      } else {
        *character_length += increase;
      }
    }
  }
}

/*
 * Tree
 */

extern void *make_tree(int tag, enum cb_category category, size_t size) {
  cb_tree x;

  x = cobc_malloc(size);
  x->tag = tag;
  x->category = category;
  x->source_file = (unsigned char *)cb_source_file;
  x->source_line = cb_source_line;
  return x;
}

static cb_tree make_constant(enum cb_category category, const char *val) {
  struct cb_const *p;

  p = make_tree(CB_TAG_CONST, category, sizeof(struct cb_const));
  p->val = val;
  return CB_TREE(p);
}

static cb_tree make_constant_label(const char *name) {
  struct cb_label *p;

  p = CB_LABEL(cb_build_label(cb_build_reference(name), NULL));
  p->need_begin = 1;
  return CB_TREE(p);
}

static int cb_name_1(char *s, cb_tree x) {
  char *orig;
  struct cb_funcall *cbip;
  struct cb_binary_op *cbop;
  struct cb_reference *p;
  struct cb_intrinsic *cbit;
  int i;

  orig = s;
  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CONST:
    if (x == cb_any) {
      strcpy(s, "ANY");
    } else if (x == cb_true) {
      strcpy(s, "TRUE");
    } else if (x == cb_false) {
      strcpy(s, "FALSE");
    } else if (x == cb_null) {
      strcpy(s, "NULL");
    } else if (x == cb_zero) {
      strcpy(s, "ZERO");
    } else if (x == cb_space) {
      strcpy(s, "SPACE");
    } else if (x == cb_low || x == cb_norm_low) {
      strcpy(s, "LOW-VALUE");
    } else if (x == cb_high || x == cb_norm_high) {
      strcpy(s, "HIGH-VALUE");
    } else if (x == cb_quote) {
      strcpy(s, "QUOTE");
    } else if (x == cb_error_node) {
      strcpy(s, "Internal error node");
    } else {
      strcpy(s, "#<unknown constant>");
    }
    break;

  case CB_TAG_LITERAL:
    if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
      strcpy(s, (char *)CB_LITERAL(x)->data);
    } else {
      sprintf(s, "\"%s\"", CB_LITERAL(x)->data);
    }
    break;

  case CB_TAG_FIELD:
    strcpy(s, CB_FIELD(x)->name);
    break;

  case CB_TAG_REFERENCE:
    p = CB_REFERENCE(x);
    s += sprintf(s, "%s", p->word->name);
    if (p->subs) {
      cb_tree l = p->subs = cb_list_reverse(p->subs);
      s += sprintf(s, " (");
      for (; l; l = CB_CHAIN(l)) {
        s += cb_name_1(s, CB_VALUE(l));
        s += sprintf(s, CB_CHAIN(l) ? ", " : ")");
      }
      p->subs = cb_list_reverse(p->subs);
    }
    if (p->offset) {
      s += sprintf(s, " (");
      s += cb_name_1(s, p->offset);
      s += sprintf(s, ":");
      if (p->length) {
        s += cb_name_1(s, p->length);
      }
      strcpy(s, ")");
    }
    if (p->chain) {
      s += sprintf(s, " in ");
      s += cb_name_1(s, p->chain);
    }
    break;

  case CB_TAG_LABEL:
    sprintf(s, "%s", CB_LABEL(x)->name);
    break;

  case CB_TAG_ALPHABET_NAME:
    sprintf(s, "%s", CB_ALPHABET_NAME(x)->name);
    break;

  case CB_TAG_CLASS_NAME:
    sprintf(s, "%s", CB_CLASS_NAME(x)->name);
    break;

  case CB_TAG_LOCALE_NAME:
    sprintf(s, "%s", CB_LOCALE_NAME(x)->name);
    break;

  case CB_TAG_BINARY_OP:
    cbop = CB_BINARY_OP(x);
    if (cbop->op == '@') {
      s += sprintf(s, "(");
      s += cb_name_1(s, cbop->x);
      s += sprintf(s, ")");
    } else if (cbop->op == '!') {
      s += sprintf(s, "!");
      s += cb_name_1(s, cbop->x);
    } else {
      s += sprintf(s, "(");
      s += cb_name_1(s, cbop->x);
      s += sprintf(s, " %c ", cbop->op);
      s += cb_name_1(s, cbop->y);
      strcpy(s, ")");
    }
    break;

  case CB_TAG_FUNCALL:
    cbip = CB_FUNCALL(x);
    s += sprintf(s, "%s", cbip->name);
    for (i = 0; i < cbip->argc; i++) {
      s += sprintf(s, (i == 0) ? "(" : ", ");
      s += cb_name_1(s, cbip->argv[i]);
    }
    s += sprintf(s, ")");
    break;

  case CB_TAG_INTRINSIC:
    cbit = CB_INTRINSIC(x);
    sprintf(s, "FUNCTION %s", cbit->intr_tab->name);
    break;
  default:
    sprintf(s, "#<unknown %d %p>", CB_TREE_TAG(x), x);
  }

  return strlen(orig);
}

static cb_tree make_intrinsic(cb_tree name, struct cb_intrinsic_table *cbp,
                              cb_tree args, cb_tree field, cb_tree refmod) {
  struct cb_intrinsic *x;

  /* Leave in, we may need this
          cb_tree			l;
          for (l = args; l; l = CB_CHAIN(l)) {
                  switch (CB_TREE_TAG (CB_VALUE(l))) {
                  case CB_TAG_CONST:
                  case CB_TAG_INTEGER:
                  case CB_TAG_LITERAL:
                  case CB_TAG_DECIMAL:
                  case CB_TAG_FIELD:
                  case CB_TAG_REFERENCE:
                  case CB_TAG_INTRINSIC:
                          break;
                  default:
                          cb_error (_("FUNCTION %s has invalid/not supported
     arguments - Tag %d"), cbp->name, CB_TREE_TAG(l)); return cb_error_node;

                  }
          }
  */
  x = make_tree(CB_TAG_INTRINSIC, cbp->category, sizeof(struct cb_intrinsic));
  x->name = name;
  x->args = args;
  x->intr_tab = cbp;
  x->intr_field = field;
  if (refmod) {
    x->offset = CB_PAIR_X(refmod);
    x->length = CB_PAIR_Y(refmod);
  }
  return CB_TREE(x);
}

static cb_tree global_check(struct cb_reference *r, cb_tree items,
                            size_t *ambiguous) {
  cb_tree candidate = NULL;
  struct cb_field *p;
  cb_tree v;

  for (; items; items = CB_CHAIN(items)) {
    /* find a candidate value by resolving qualification */
    v = CB_VALUE(items);
    cb_tree c = r->chain;
    if (CB_FIELD_P(v)) {
      if (!CB_FIELD(v)->flag_is_global) {
        continue;
      }
      /* in case the value is a field, it might be qualified
         by its parent names and a file name */
      if (CB_FIELD(v)->flag_indexed_by) {
        p = CB_FIELD(v)->index_qual;
      } else {
        p = CB_FIELD(v)->parent;
      }
      /* resolve by parents */
      for (; p; p = p->parent) {
        if (c && cobc_casecmp(CB_NAME(c), p->name) == 0) {
          c = CB_REFERENCE(c)->chain;
        }
      }

      /* resolve by file */
      if (c && CB_REFERENCE(c)->chain == NULL) {
        if (CB_REFERENCE(c)->word->count == 1 && CB_FILE_P(cb_ref(c)) &&
            (CB_FILE(cb_ref(c)) == cb_field_founder(CB_FIELD(v))->file)) {
          c = CB_REFERENCE(c)->chain;
        }
      }
    }
    /* a well qualified value is a good candidate */
    if (c == NULL) {
      if (candidate == NULL) {
        /* keep the first candidate */
        candidate = v;
      } else {
        /* multiple candidates and possibly ambiguous */
        *ambiguous = 1;
      }
    }
  }
  return candidate;
}

/* Global functions */

struct cb_literal *build_literal(enum cb_category category,
                                 const unsigned char *data, size_t size) {
  struct cb_literal *p;

  p = make_tree(CB_TAG_LITERAL, category, sizeof(struct cb_literal));
  p->data = cobc_malloc((size_t)(size + 1));
  p->size = size;
  memcpy(p->data, data, (size_t)size);
  /* RXW - malloc zeroes
  p->data[size] = 0;
  */
  return p;
}

char *cb_name(cb_tree x) {
  if (!treenamebuff) {
    treenamebuff = cobc_malloc(COB_NORMAL_BUFF);
  }
  cb_name_1(treenamebuff, x);
  return treenamebuff;
}

enum cb_class cb_tree_class(cb_tree x) {

  return category_to_class_table[CB_TREE_CATEGORY(x)];
}

enum cb_category cb_tree_category(cb_tree x) {
  struct cb_cast *p;
  struct cb_reference *r;
  struct cb_field *f;

  if (x == cb_error_node) {
    return 0;
  }
  if (x->category != CB_CATEGORY_UNKNOWN) {
    return x->category;
  }

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CAST:
    p = CB_CAST(x);
    switch (p->type) {
    case CB_CAST_ADDRESS:
    case CB_CAST_ADDR_OF_ADDR:
      x->category = CB_CATEGORY_DATA_POINTER;
      break;
    case CB_CAST_PROGRAM_POINTER:
      x->category = CB_CATEGORY_PROGRAM_POINTER;
      break;
    default:
      fprintf(stderr, "Unexpected cast type -> %d\n", p->type);
      ABORT();
    }
    break;
  case CB_TAG_REFERENCE:
    r = CB_REFERENCE(x);
    if (r->offset) {
      x->category = CB_CATEGORY_ALPHANUMERIC;
    } else {
      x->category = cb_tree_category(r->value);
    }
    break;
  case CB_TAG_FIELD:
    f = CB_FIELD(x);
    if (f->children) {
      x->category = CB_CATEGORY_ALPHANUMERIC;
    } else if (f->usage == CB_USAGE_POINTER && f->level != 88) {
      x->category = CB_CATEGORY_DATA_POINTER;
    } else if (f->usage == CB_USAGE_PROGRAM_POINTER && f->level != 88) {
      x->category = CB_CATEGORY_PROGRAM_POINTER;
    } else {
      switch (f->level) {
      case 66:
        if (f->rename_thru) {
          x->category = CB_CATEGORY_ALPHANUMERIC;
        } else {
          x->category = cb_tree_category(CB_TREE(f->redefines));
        }
        break;
      case 88:
        x->category = CB_CATEGORY_BOOLEAN;
        break;
      default:
        x->category = f->pic->category;
        break;
      }
    }
    break;
  case CB_TAG_ALPHABET_NAME:
  case CB_TAG_LOCALE_NAME:
    x->category = CB_CATEGORY_ALPHANUMERIC;
    break;
  case CB_TAG_BINARY_OP:
    x->category = CB_CATEGORY_BOOLEAN;
    break;
  default:
    fprintf(stderr, "Unknown tree tag %d Category %d\n", CB_TREE_TAG(x),
            x->category);
    ABORT();
  }

  return x->category;
}

int cb_tree_type(cb_tree x) {
  struct cb_field *f;

  f = cb_field(x);
  if (f->children) {
    return COB_TYPE_GROUP;
  }

  switch (CB_TREE_CATEGORY(x)) {
  case CB_CATEGORY_ALPHABETIC:
    return COB_TYPE_ALPHANUMERIC;
  case CB_CATEGORY_ALPHANUMERIC:
    return COB_TYPE_ALPHANUMERIC;
  case CB_CATEGORY_NATIONAL:
    return COB_TYPE_NATIONAL;
  case CB_CATEGORY_ALPHANUMERIC_EDITED:
    return COB_TYPE_ALPHANUMERIC_EDITED;
  case CB_CATEGORY_NATIONAL_EDITED:
    return COB_TYPE_NATIONAL_EDITED;
  case CB_CATEGORY_NUMERIC:
    switch (f->usage) {
    case CB_USAGE_DISPLAY:
      return COB_TYPE_NUMERIC_DISPLAY;
    case CB_USAGE_BINARY:
    case CB_USAGE_COMP_5:
    case CB_USAGE_COMP_X:
    case CB_USAGE_INDEX:
    case CB_USAGE_LENGTH:
      return COB_TYPE_NUMERIC_BINARY;
    case CB_USAGE_FLOAT:
      return COB_TYPE_NUMERIC_FLOAT;
    case CB_USAGE_DOUBLE:
      return COB_TYPE_NUMERIC_DOUBLE;
    case CB_USAGE_PACKED:
      return COB_TYPE_NUMERIC_PACKED;
    default:
      fprintf(stderr, "Unexpected numeric usage -> %d\n", f->usage);
      ABORT();
    }
  case CB_CATEGORY_NUMERIC_EDITED:
    return COB_TYPE_NUMERIC_EDITED;
  case CB_CATEGORY_OBJECT_REFERENCE:
  case CB_CATEGORY_DATA_POINTER:
  case CB_CATEGORY_PROGRAM_POINTER:
    return COB_TYPE_NUMERIC_BINARY;
  default:
    fprintf(stderr, "Unexpected category -> %d\n", CB_TREE_CATEGORY(x));
    ABORT();
  }
  /* NOT REACHED */
#ifndef _MSC_VER
  return 0;
#endif
}

int cb_fits_int(cb_tree x) {
  struct cb_literal *l;
  struct cb_field *f;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_LITERAL:
    l = CB_LITERAL(x);
    if (l->scale <= 0 && l->size < 10) {
      return 1;
    }
    return 0;
  case CB_TAG_FIELD:
    f = CB_FIELD(x);
    switch (f->usage) {
    case CB_USAGE_INDEX:
    case CB_USAGE_LENGTH:
      return 1;
    case CB_USAGE_BINARY:
    case CB_USAGE_COMP_5:
    case CB_USAGE_COMP_X:
      if (f->pic->scale <= 0 && f->size <= (int)sizeof(int)) {
        return 1;
      }
      return 0;
    case CB_USAGE_DISPLAY:
      if (f->size < 10) {
        if (!f->pic || f->pic->scale <= 0) {
          return 1;
        }
      }
      return 0;
    case CB_USAGE_PACKED:
      if (f->pic->scale <= 0 && f->pic->digits < 10) {
        return 1;
      }
      return 0;
    default:
      return 0;
    }
  case CB_TAG_REFERENCE:
    return cb_fits_int(CB_REFERENCE(x)->value);
  default:
    return 0;
  }
}

int cb_fits_long_long(cb_tree x) {
  struct cb_literal *l;
  struct cb_field *f;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_LITERAL:
    l = CB_LITERAL(x);
    if (l->scale <= 0 && l->size < 19) {
      return 1;
    }
    return 0;
  case CB_TAG_FIELD:
    f = CB_FIELD(x);
    switch (f->usage) {
    case CB_USAGE_INDEX:
    case CB_USAGE_LENGTH:
      return 1;
    case CB_USAGE_BINARY:
    case CB_USAGE_COMP_5:
    case CB_USAGE_COMP_X:
      if (f->pic->scale <= 0 && f->size <= (int)sizeof(long long)) {
        return 1;
      }
      return 0;
    case CB_USAGE_DISPLAY:
      if (f->pic->scale <= 0 && f->size < 19) {
        return 1;
      }
      return 0;
    default:
      return 0;
    }
  case CB_TAG_REFERENCE:
    return cb_fits_long_long(CB_REFERENCE(x)->value);
  default:
    return 0;
  }
}

int cb_is_digist_data(cb_tree x) {
  struct cb_literal *l;
  size_t i;

  l = CB_LITERAL(x);
  for (i = 0; i < l->size; i++) {
    if (l->data[i] != '0') {
      break;
    }
  }
  for (; i < l->size; i++) {
    if (l->data[i] - '0' > 9 || l->data[i] - '0' < 0) {
      return 0;
    }
  }
  return 1;
}

int cb_get_int(cb_tree x) {
  struct cb_literal *l;
  size_t i;
  int val = 0;

  l = CB_LITERAL(x);
  for (i = 0; i < l->size; i++) {
    if (l->data[i] != '0') {
      break;
    }
  }

  /* RXWRXW
          if (l->size - i >= 10) {
                  ABORT ();
          }
  */

  for (; i < l->size; i++) {
    val = val * 10 + l->data[i] - '0';
  }
  if (l->sign < 0) {
    val = -val;
  }
  return val;
}

long long cb_get_long_long(cb_tree x) {
  struct cb_literal *l;
  size_t i;
  long long val = 0;

  l = CB_LITERAL(x);
  for (i = 0; i < l->size; i++) {
    if (l->data[i] != '0') {
      break;
    }
  }

  if (l->size - i >= 19) {
    ABORT();
  }

  for (; i < l->size; i++) {
    val = val * 10 + l->data[i] - '0';
  }
  if (l->sign < 0) {
    val = -val;
  }
  return val;
}

void cb_init_constants(void) {
  int i;

  cb_error_node = make_constant(CB_CATEGORY_UNKNOWN, NULL);
  cb_any = make_constant(CB_CATEGORY_UNKNOWN, NULL);
  cb_true = make_constant(CB_CATEGORY_BOOLEAN, "1");
  cb_false = make_constant(CB_CATEGORY_BOOLEAN, "0");
  cb_null = make_constant(CB_CATEGORY_DATA_POINTER, "0");
  cb_zero = make_constant(CB_CATEGORY_NUMERIC, "CobolConstant.zero");
  cb_zen_zero = make_constant(CB_CATEGORY_NUMERIC, "CobolConstant.zenZero");
  cb_one = make_constant(CB_CATEGORY_NUMERIC, "CobolConstant.One");
  cb_space = make_constant(CB_CATEGORY_ALPHANUMERIC, "CobolConstant.space");
  cb_blank = make_constant(CB_CATEGORY_ALPHANUMERIC, "CobolConstant.blank");
  cb_low = make_constant(CB_CATEGORY_ALPHANUMERIC, "CobolConstant.low");
  cb_norm_low = cb_low;
  cb_high = make_constant(CB_CATEGORY_ALPHANUMERIC, "CobolConstant.high");
  cb_norm_high = cb_high;
  cb_quote = make_constant(CB_CATEGORY_ALPHANUMERIC, "CobolConstant.quote");
  cb_int0 = cb_int(0);
  cb_int1 = cb_int(1);
  cb_int2 = cb_int(2);
  cb_int3 = cb_int(3);
  cb_int4 = cb_int(4);
  cb_int5 = cb_int(5);
  for (i = 1; i < 8; i++) {
    char *s = cobc_malloc(4);
    sprintf(s, "i%d", i);
    cb_i[i] = make_constant(CB_CATEGORY_NUMERIC, s);
  }
  cb_standard_error_handler = make_constant_label("Default Error Handler");
}

/*
 * List
 */

cb_tree cb_build_list(cb_tree purpose, cb_tree value, cb_tree rest) {
  struct cb_list *p;

  p = make_tree(CB_TAG_LIST, CB_CATEGORY_UNKNOWN, sizeof(struct cb_list));
  p->purpose = purpose;
  p->value = value;
  p->chain = rest;
  if (value && value->source_file) {
    CB_TREE(p)->source_file = value->source_file;
    CB_TREE(p)->source_line = value->source_line;
  }
  return CB_TREE(p);
}

cb_tree cb_list_append(cb_tree l1, cb_tree l2) {

  if (l1 == NULL) {
    return l2;
  } else {
    cb_tree l = l1;
    while (CB_CHAIN(l)) {
      l = CB_CHAIN(l);
    }
    CB_CHAIN(l) = l2;
    return l1;
  }
}

cb_tree cb_list_add(cb_tree l, cb_tree x) {
  return cb_list_append(l, cb_list_init(x));
}

cb_tree cb_list_reverse(cb_tree l) {
  cb_tree next;
  cb_tree last = NULL;

  for (; l; l = next) {
    next = CB_CHAIN(l);
    CB_CHAIN(l) = last;
    last = l;
  }
  return last;
}

int cb_list_length(cb_tree l) {
  int n = 0;

  for (; l; l = CB_CHAIN(l)) {
    n++;
  }
  return n;
}

void cb_list_map(cb_tree (*func)(cb_tree x), cb_tree l) {
  for (; l; l = CB_CHAIN(l)) {
    CB_VALUE(l) = func(CB_VALUE(l));
  }
}

/*
 * Program
 */

struct cb_program *cb_build_program(struct cb_program *last_program,
                                    const int nest_level) {
  struct cb_program *p;

  cb_reset_78();
  cb_reset_in_procedure();
  cb_clear_real_field();
  p = cobc_malloc(sizeof(struct cb_program));
  p->next_program = last_program;
  p->nested_level = nest_level;
  p->decimal_point = '.';
  p->currency_symbol = cb_default_currency_symbol;
  p->numeric_separator = ',';
  if (nest_level) {
    p->global_file_list = last_program->global_file_list;
    p->collating_sequence = last_program->collating_sequence;
    p->function_spec_list = last_program->function_spec_list;
    p->class_spec_list = last_program->class_spec_list;
    p->interface_spec_list = last_program->interface_spec_list;
    p->program_spec_list = last_program->program_spec_list;
    p->property_spec_list = last_program->property_spec_list;
    p->alphabet_name_list = last_program->alphabet_name_list;
    p->class_name_list = last_program->class_name_list;
    p->locale_list = last_program->locale_list;
    p->symbolic_list = last_program->symbolic_list;
    p->decimal_point = last_program->decimal_point;
    p->numeric_separator = last_program->numeric_separator;
    p->currency_symbol = last_program->currency_symbol;
    p->cb_return_code = last_program->cb_return_code;
  } else {
    functions_are_all = cb_flag_functions_all;
  }
  return p;
}

/*
 * Integer
 */

cb_tree cb_int(int n) {
  struct cb_integer *x;
  struct int_node *p;

  for (p = int_node_table; p; p = p->next) {
    if (p->n == n) {
      return p->node;
    }
  }

  x = make_tree(CB_TAG_INTEGER, CB_CATEGORY_NUMERIC, sizeof(struct cb_integer));
  x->val = n;

  p = cobc_malloc(sizeof(struct int_node));
  p->n = n;
  p->node = CB_TREE(x);
  p->next = int_node_table;
  int_node_table = p;
  return p->node;
}

/*
 * String
 */

cb_tree cb_build_string(const unsigned char *data, size_t size) {
  struct cb_string *p;

  p = make_tree(CB_TAG_STRING, CB_CATEGORY_ALPHANUMERIC,
                sizeof(struct cb_string));
  p->size = size;
  p->data = data;
  return CB_TREE(p);
}

/*
 * Alphabet-name
 */

cb_tree cb_build_alphabet_name(cb_tree name, enum cb_alphabet_name_type type) {
  struct cb_alphabet_name *p;

  p = make_tree(CB_TAG_ALPHABET_NAME, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_alphabet_name));
  p->name = cb_define(name, CB_TREE(p));
  p->cname = to_cname(p->name);
  p->type = type;
  return CB_TREE(p);
}

/*
 * Class-name
 */

cb_tree cb_build_class_name(cb_tree name, cb_tree list) {
  struct cb_class_name *p;
  char buff[COB_MINI_BUFF];

  p = make_tree(CB_TAG_CLASS_NAME, CB_CATEGORY_BOOLEAN,
                sizeof(struct cb_class_name));
  p->name = cb_define(name, CB_TREE(p));
  snprintf(buff, COB_MINI_MAX, "is_%s", to_cname(p->name));
  p->cname = strdup(buff);
  p->list = list;
  return CB_TREE(p);
}

cb_tree cb_lookup_class_name(const char *name) {
  struct cb_class_name *p = NULL;
  cb_tree l;

  if (!current_program) {
    return NULL;
  }
  l = current_program->class_name_list;
  while (l) {
    if (!cobc_casecmp(CB_CLASS_NAME(CB_VALUE(l))->name, name)) {
      p = CB_CLASS_NAME(CB_VALUE(l));
      break;
    }
    l = CB_CHAIN(l);
  }
  return CB_TREE(p);
}

/*
 * Locale-name
 */

cb_tree cb_build_locale_name(cb_tree name, cb_tree list) {
  struct cb_class_name *p;

  p = make_tree(CB_TAG_LOCALE_NAME, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_locale_name));
  p->name = cb_define(name, CB_TREE(p));
  p->cname = to_cname(p->name);
  p->list = list;
  return CB_TREE(p);
}

/*
 * System-name
 */

cb_tree cb_build_system_name(enum cb_system_name_category category, int token) {
  struct cb_system_name *p;

  p = make_tree(CB_TAG_SYSTEM_NAME, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_system_name));
  p->category = category;
  p->token = token;
  return CB_TREE(p);
}

/*
 * Literal
 */

cb_tree cb_build_numeric_literal(int sign, const unsigned char *data,
                                 int scale) {
  struct cb_literal *p;

  p = build_literal(CB_CATEGORY_NUMERIC, data, strlen((char *)data));
  p->sign = (char)sign;
  p->scale = (char)scale;
  return CB_TREE(p);
}

cb_tree cb_build_alphanumeric_literal(const unsigned char *data, size_t size) {
  return CB_TREE(build_literal(CB_CATEGORY_ALPHANUMERIC, data, size));
}

cb_tree cb_build_national_literal(const unsigned char *data, size_t size) {
  return CB_TREE(build_literal(CB_CATEGORY_NATIONAL, data, size));
}

cb_tree cb_concat_literals(cb_tree x1, cb_tree x2) {
  unsigned char *buff;
  cb_tree x;
  unsigned char *data1;
  unsigned char *data2;
  size_t size1;
  size_t size2;

  if (x1 == cb_error_node || x2 == cb_error_node) {
    return cb_error_node;
  }
  if (CB_LITERAL_P(x1)) {
    data1 = CB_LITERAL(x1)->data;
    size1 = CB_LITERAL(x1)->size;
  } else if (CB_CONST_P(x1)) {
    size1 = 1;
    if (x1 == cb_space) {
      data1 = (unsigned char *)" ";
    } else if (x1 == cb_zero) {
      data1 = (unsigned char *)"0";
    } else if (x1 == cb_quote) {
      data1 = (unsigned char *)"\"";
    } else if (x1 == cb_norm_low) {
      data1 = (unsigned char *)"\0";
    } else if (x1 == cb_norm_high) {
      data1 = (unsigned char *)"\255";
    } else if (x1 == cb_null) {
      data1 = (unsigned char *)"\0";
    } else {
      return cb_error_node;
    }
  } else {
    return cb_error_node;
  }
  if (CB_LITERAL_P(x2)) {
    data2 = CB_LITERAL(x2)->data;
    size2 = CB_LITERAL(x2)->size;
  } else if (CB_CONST_P(x2)) {
    size2 = 1;
    if (x2 == cb_space) {
      data2 = (unsigned char *)" ";
    } else if (x2 == cb_zero) {
      data2 = (unsigned char *)"0";
    } else if (x2 == cb_quote) {
      data2 = (unsigned char *)"\"";
    } else if (x2 == cb_norm_low) {
      data2 = (unsigned char *)"\0";
    } else if (x2 == cb_norm_high) {
      data2 = (unsigned char *)"\255";
    } else if (x2 == cb_null) {
      data2 = (unsigned char *)"\0";
    } else {
      return cb_error_node;
    }
  } else {
    return cb_error_node;
  }
  buff = cobc_malloc(size1 + size2 + 3);
  memcpy(buff, data1, size1);
  memcpy(buff + size1, data2, size2);
  x = cb_build_alphanumeric_literal(buff, size1 + size2);
  free(buff);
  return x;
}

/*
 * Decimal
 */

cb_tree cb_build_decimal(int id) {
  struct cb_decimal *p;

  p = make_tree(CB_TAG_DECIMAL, CB_CATEGORY_NUMERIC, sizeof(struct cb_decimal));
  p->id = id;
  return CB_TREE(p);
}

/*
 * Picture
 */

static int guess_pic_category(const char *str) {
  int category = 0;
  const char *p;
  int skip = 0;

  for (p = str; *p; p++) {
    if (*p == '(') {
      skip++;
    } else if (*p == ')') {
      --skip;
    }
    if (!skip) {
      switch (*p) {
      case 'A':
        category |= PIC_ALPHABETIC;
        break;
      case 'X':
        category |= PIC_ALPHANUMERIC;
        break;
      case '9':
        category |= PIC_NUMERIC;
        break;
      case 'N':
        category |= PIC_NATIONAL;
        break;
      case 'S':
        category |= PIC_NUMERIC;
        break;
      case 'V':
      case 'P':
        category |= PIC_NUMERIC;
        break;
      case '0':
      case 'B':
      case '/':
        category |= PIC_EDITED;
        break;
      case ',':
      case '.':
      case '*':
      case 'Z':
      case '+':
      case '-':
        category |= PIC_NUMERIC_EDITED;
        break;
      case 'C':
        if (p[1] == 'R' && p[2] == 0) {
          category |= PIC_NUMERIC_EDITED;
        }
        break;
      case 'D':
        if (p[1] == 'B' && p[2] == 0) {
          category |= PIC_NUMERIC_EDITED;
        }
        break;
      default:
        if (*p == current_program->currency_symbol) {
          category |= PIC_NUMERIC_EDITED;
        }
        break;
      }
    }
  }
  return category;
}

cb_tree cb_build_picture(const char *str) {
  struct cb_picture *pic;
  const char *p;
  size_t idx = 0;
  size_t buffcnt = 0;
  size_t at_beginning;
  size_t at_end;
  int p_char_seen = 0;
  int s_char_seen = 0;
  int cur_char_seen = 0;
  int category = 0;
  int size = 0;
  int digits = 0;
  int scale = 0;
  int s_count = 0;
  int v_count = 0;
  int count_value;
  int count_increase;
  int n;
  int flg = 0;
  int character_length = 0;
  int remain;
  unsigned char c;
  unsigned char lastonechar = 0;
  unsigned char lasttwochar = 0;
  unsigned char buff[COB_SMALL_BUFF];
  COB_UNUSED(flg);

  pic =
      make_tree(CB_TAG_PICTURE, CB_CATEGORY_UNKNOWN, sizeof(struct cb_picture));
  if (strlen(str) > 50) {
    goto error;
  }
  memset(buff, 0, sizeof(buff));

  /* guess category first */
  category = guess_pic_category(str);

  for (p = str; *p; p++) {
    n = 1;
    c = *p;
  repeat:
    /* count the number of repeated chars */
    while (p[1] == c) {
      p++, n++;
    }

    /* add parenthesized numbers */
    if (p[1] == '(') {
      flg = 1;
      count_value = 0;
      p += 2;
      for (; *p == '0'; p++) {
        ;
      }
      for (; *p != ')'; p++) {
        if (!isdigit(*p)) {
          goto error;
        } else {
          if (count_value > INT_MAX / 10) {
            count_value = CHARACTER_LENGTH_OVERFLOW;
          } else {
            count_value *= 10;
            count_increase = *p - '0';
            remain = INT_MAX - count_value;
            if (remain < count_increase) {
              count_value = CHARACTER_LENGTH_OVERFLOW;
            } else {
              count_value += count_increase;
            }
          }
        }
      }
      if (count_value == 0) {
        goto error;
      } else if (count_value == CHARACTER_LENGTH_OVERFLOW) {
        n = CHARACTER_LENGTH_OVERFLOW;
      } else if (n != CHARACTER_LENGTH_OVERFLOW) {
        remain = INT_MAX - n;
        if (remain < (count_value - 1)) {
          n = CHARACTER_LENGTH_OVERFLOW;
        } else {
          n += count_value - 1;
        }
      }
      goto repeat;
    }

    /* check grammar and category */
    /* FIXME: need more error check */
    switch (c) {
    case 'A':
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      INCREASE_CHARACTER_LENGTH(n);
      break;

    case 'X':
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      INCREASE_CHARACTER_LENGTH(n);
      break;

    case '9':
      digits += n;
      INCREASE_CHARACTER_LENGTH(n);
      if (v_count) {
        scale += n;
      }
      break;

    case 'N':
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      pic->national = 1;
      INCREASE_CHARACTER_LENGTH(n);
      break;

    case 'S':
      if (category & PIC_ALPHABETIC) {
        goto error;
      }
      s_count += n;
      if (s_count > 1 || idx != 0) {
        goto error;
      }
      s_char_seen = 1;
      continue;

    case ',':
    case '.':
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      if (c != current_program->decimal_point) {
        break;
      }
      /* fall through */
    case 'V':
      if (category & PIC_ALPHABETIC) {
        goto error;
      }
      v_count += n;
      if (v_count > 1) {
        goto error;
      }
      break;

    case 'P':
      if (category & PIC_ALPHABETIC) {
        goto error;
      }
      if (p_char_seen) {
        goto error;
      }
      at_beginning = 0;
      at_end = 0;
      switch (buffcnt) {
      case 0:
        /* P..... */
        at_beginning = 1;
        break;
      case 1:
        /* VP.... */
        /* SP.... */
        if (lastonechar == 'V' || lastonechar == 'S') {
          at_beginning = 1;
        }
        break;
      case 2:
        /* SVP... */
        if (lasttwochar == 'S' && lastonechar == 'V') {
          at_beginning = 1;
        }
        break;
      }
      if (p[1] == 0 || (p[1] == 'V' && p[2] == 0)) {
        /* .....P */
        /* ....PV */
        at_end = 1;
      }
      if (!at_beginning && !at_end) {
        goto error;
      }
      p_char_seen = 1;
      if (at_beginning) {
        v_count++; /* implicit V */
      }
      digits += n;
      INCREASE_CHARACTER_LENGTH(n);
      if (v_count) {
        scale += n;
      } else {
        scale -= n;
      }
      break;

    case '0':
    case 'B':
    case '/':
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      INCREASE_CHARACTER_LENGTH(n);
      break;

    case '*':
    case 'Z':
      if (category & PIC_ALPHABETIC) {
        goto error;
      }
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      digits += n;
      INCREASE_CHARACTER_LENGTH(n);
      if (v_count) {
        scale += n;
      }
      break;

    case '+':
    case '-':
      if (category & PIC_ALPHABETIC) {
        goto error;
      }
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      digits += n - 1;
      INCREASE_CHARACTER_LENGTH(n - 1);
      if (s_count > 0) {
        ++digits;
        INCREASE_CHARACTER_LENGTH(1);
      }
      s_count++;
      /* FIXME: need more check */
      break;

    case 'C':
      category |= PIC_NUMERIC_EDITED;
      if (!(p[1] == 'R' && p[2] == 0)) {
        goto error;
      }
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      p++;
      s_count++;
      break;

    case 'D':
      if (!(p[1] == 'B' && p[2] == 0)) {
        goto error;
      }
      if (s_char_seen || p_char_seen) {
        goto error;
      }
      p++;
      s_count++;
      break;

    default:
      if (c == current_program->currency_symbol) {
        if (cur_char_seen == 0) {
          digits += n - 1;
          INCREASE_CHARACTER_LENGTH(n - 1);
          cur_char_seen = 1;
        } else {
          digits += n;
          INCREASE_CHARACTER_LENGTH(n);
        }
        /* FIXME: need more check */
        break;
      }

      goto error;
    }

    /* calculate size */
    if (c != 'V' && c != 'P') {
      size += n;
    }
    if (c == 'C' || c == 'D') {
      size += n;
    } else if (c == 'N' || (category == PIC_NATIONAL_EDITED &&
                            (c == '0' || c == 'B' || c == '/'))) {
      size += n;
    }

    /* store in the buffer */
    buff[idx++] = c;
    lasttwochar = lastonechar;
    lastonechar = c;
    memcpy(&buff[idx], (unsigned char *)&n, sizeof(int));
    idx += sizeof(int);
    ++buffcnt;
  }
  buff[idx] = 0;

  if (size == 0 && v_count) {
    goto error;
  }
  /* set picture */
  pic->orig = strdup(str);
  pic->size = size;
  pic->digits = (unsigned char)digits;
  pic->scale = (signed char)scale;
  pic->have_sign = (unsigned char)s_count;

  /* set picture category */
  switch (category) {
  case PIC_ALPHABETIC:
    pic->category = CB_CATEGORY_ALPHABETIC;
    CHECK_CHARACTER_LENGTH(cb_max_alpha_character_data_size,
                           "Alphabetic field cannot be larger than %d digits");
    break;
  case PIC_NUMERIC:
    pic->category = CB_CATEGORY_NUMERIC;
    if (digits > 36) {
      cb_error(_("Numeric field cannot be larger than 36 digits"));
    }
    break;
  case PIC_ALPHANUMERIC:
    pic->category = CB_CATEGORY_ALPHANUMERIC;
    CHECK_CHARACTER_LENGTH(
        cb_max_alpha_character_data_size,
        "AlphaNumeric field cannot be larger than %d digits");
    break;
  case PIC_NATIONAL:
    pic->category = CB_CATEGORY_NATIONAL;
    // #ifdef I18N_UTF8
    //     /* I18N_UTF8: NATIONAL allocates 3bytes/char for BMP. */
    //     CHECK_CHARACTER_LENGTH(cb_max_utf8_character_data_size,
    //                            "National field cannot be larger than %d
    //                            digits");
    // #else  /*!I18N_UTF8*/
    CHECK_CHARACTER_LENGTH(cb_max_sjis_character_data_size,
                           "National field cannot be larger than %d digits");
    // #endif /*I18N_UTF8*/
    break;
  case PIC_NUMERIC_EDITED:
    pic->str = cobc_malloc(idx + 1);
    memcpy(pic->str, buff, idx);
    pic->category = CB_CATEGORY_NUMERIC_EDITED;
    pic->lenstr = idx;
    CHECK_CHARACTER_LENGTH(160,
                           "NumericEdit field cannot be larger than %d digits");
    break;
  case PIC_EDITED:
  case PIC_ALPHABETIC_EDITED:
  case PIC_ALPHANUMERIC_EDITED:
    pic->str = cobc_malloc(idx + 1);
    memcpy(pic->str, buff, idx);
    pic->category = CB_CATEGORY_ALPHANUMERIC_EDITED;
    pic->lenstr = idx;
    CHECK_CHARACTER_LENGTH(
        cb_max_alpha_character_data_size,
        "AlphaNumericEdit field cannot be larger than %d digits");
    break;
  case PIC_NATIONAL_EDITED:
    pic->str = cobc_malloc(idx + 1);
    memcpy(pic->str, buff, idx);
    pic->category = CB_CATEGORY_NATIONAL_EDITED;
    pic->lenstr = idx;
    // #ifdef I18N_UTF8
    //     /* I18N_UTF8: NATIONAL allocates 3bytes/char for BMP. */
    //     CHECK_CHARACTER_LENGTH(
    //         cb_max_utf8_character_data_size,
    //         "NationalEdit field cannot be larger than %d digits");
    // #else  /*!I18N_UTF8*/
    CHECK_CHARACTER_LENGTH(
        cb_max_sjis_character_data_size,
        "NationalEdit field cannot be larger than %d digits");
    // #endif /*I18N_UTF8*/
    break;
  default:
    goto error;
  }
  goto end;

error:
  cb_error(_("Invalid picture string - '%s'"), str);

end:
  return CB_TREE(pic);
}

/*
 * Field
 */

cb_tree cb_build_field(cb_tree name) {
  struct cb_field *p;

  p = make_tree(CB_TAG_FIELD, CB_CATEGORY_UNKNOWN, sizeof(struct cb_field));
  p->id = cb_field_id++;
  p->name = cb_define(name, CB_TREE(p));
  p->ename = NULL;
  p->usage = CB_USAGE_DISPLAY;
  p->storage = CB_STORAGE_WORKING;
  p->occurs_max = 1;
  return CB_TREE(p);
}

cb_tree cb_build_implicit_field(cb_tree name, int len) {
  cb_tree x;
  char pic[32];

  x = cb_build_field(name);
  memset(pic, 0, sizeof(pic));
  sprintf(pic, "X(%d)", len);
  CB_FIELD(x)->pic = CB_PICTURE(cb_build_picture(pic));
  cb_validate_field(CB_FIELD(x));
  return x;
}

cb_tree cb_build_constant(cb_tree name, cb_tree value) {
  cb_tree x;

  x = cb_build_field(name);
  x->category = cb_tree_category(value);
  CB_FIELD(x)->storage = CB_STORAGE_CONSTANT;
  CB_FIELD(x)->values = cb_list_init(value);
  return x;
}

struct cb_field *cb_field(cb_tree x) {
  if (CB_REFERENCE_P(x)) {
    return CB_FIELD(cb_ref(x));
  } else {
    return CB_FIELD(x);
  }
}

struct cb_field *cb_field_add(struct cb_field *f, struct cb_field *p) {
  struct cb_field *t;

  if (f == NULL) {
    return p;
  }
  for (t = f; t->sister; t = t->sister) {
    ;
  }
  t->sister = p;
  return f;
}

int cb_field_size(cb_tree x) {
  struct cb_reference *r;
  struct cb_field *f;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_LITERAL:
    return CB_LITERAL(x)->size;
  case CB_TAG_FIELD:
    return CB_FIELD(x)->size;
  case CB_TAG_REFERENCE:
    r = CB_REFERENCE(x);
    f = CB_FIELD(r->value);

    if (r->length) {
      if (CB_LITERAL_P(r->length)) {
        return cb_get_int(r->length);
      } else {
        return -1;
      }
    } else if (r->offset) {
      if (CB_LITERAL_P(r->offset)) {
        return f->size - cb_get_int(r->offset) + 1;
      } else {
        return -1;
      }
    } else {
      return f->size;
    }
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    ABORT();
  }
  /* NOT REACHED */
#ifndef _MSC_VER
  return 0;
#endif
}

struct cb_field *cb_field_founder(struct cb_field *f) {
  while (f->parent) {
    f = f->parent;
  }
  return f;
}

struct cb_field *cb_field_variable_size(struct cb_field *f) {
  struct cb_field *p;

  for (f = f->children; f; f = f->sister) {
    if (f->occurs_depending) {
      return f;
    } else if ((p = cb_field_variable_size(f)) != NULL) {
      return p;
    }
  }
  return NULL;
}

struct cb_field *cb_field_variable_address(struct cb_field *f) {
  struct cb_field *p;

  for (p = f->parent; p; f = f->parent, p = f->parent) {
    for (p = p->children; p != f; p = p->sister) {
      if (p->occurs_depending || cb_field_variable_size(p)) {
        return p;
      }
    }
  }
  return NULL;
}

/* Return 1 if P is subordinate to F */

int cb_field_subordinate(struct cb_field *p, const struct cb_field *f) {
  for (p = p->parent; p; p = p->parent) {
    if (p == f) {
      return 1;
    }
  }
  return 0;
}

/*
 * File
 */

struct cb_file *build_file(cb_tree name) {
  struct cb_file *p;

  p = make_tree(CB_TAG_FILE, CB_CATEGORY_UNKNOWN, sizeof(struct cb_file));
  p->name = cb_define(name, CB_TREE(p));
  p->cname = to_cname(p->name);

  p->organization = cb_default_organization;
  p->access_mode = COB_ACCESS_SEQUENTIAL;
  p->handler = CB_LABEL(cb_standard_error_handler);
  p->handler_prog = current_program;
  if (external_flg == 1) {
    p->external_assign = 1;
  }
  return p;
}

void validate_file(struct cb_file *f, cb_tree name) {
  /* check RECORD/RELATIVE KEY clause */
  switch (f->organization) {
  case COB_ORG_INDEXED:
    if (f->key == NULL) {
      file_error(name, "RECORD KEY");
    }
    break;
  case COB_ORG_RELATIVE:
    if (f->key == NULL && f->access_mode != COB_ACCESS_SEQUENTIAL) {
      file_error(name, "RELATIVE KEY");
    }
    break;
  }
}

static void compute_composite_key(cb_tree key,
                                  struct cb_key_component *component_list) {
  int cb;
  char pic[32];
  struct cb_key_component *key_component;
  struct cb_field *composite_key;

  cb = 0;
  for (key_component = component_list; key_component != NULL;
       key_component = key_component->next) {
    /* resolution of references in key components must be done here */
    cb += cb_field_size(cb_ref(key_component->component));
  }
  composite_key = (struct cb_field *)cb_ref(key);
  memset(pic, 0, sizeof(pic));
  sprintf(pic, "X(%d)", cb);
  if (composite_key->pic != NULL)
    free(composite_key->pic);
  composite_key->pic = CB_PICTURE(cb_build_picture(pic));
  cb_validate_field(composite_key);
}

void finalize_file(struct cb_file *f, struct cb_field *records) {
  struct cb_field *p;
  struct cb_field *v;
  char buff[COB_MINI_BUFF];

  if (f->special) {
    f->organization = COB_ORG_LINE_SEQUENTIAL;
  }
  if (f->fileid_assign && !f->assign) {
    f->assign = cb_build_alphanumeric_literal((unsigned char *)f->name,
                                              strlen(f->name));
  }

  /* compute composite key total length */
  if (f->component_list != NULL) {
    compute_composite_key(f->key, f->component_list);
  }

  if (f->alt_key_list != NULL) {
    struct cb_alt_key *alt_key;
    for (alt_key = f->alt_key_list; alt_key != NULL; alt_key = alt_key->next) {
      if (alt_key->component_list != NULL) {
        compute_composite_key(alt_key->key, alt_key->component_list);
      }
    }
  }

  /* check the record size if it is limited */
  if (cb_ignore_invalid_record_contains) {
    f->record_max = 0;
    f->record_min = 0;
  } else {
    for (p = records; p; p = p->sister) {
      if (f->record_min > 0) {
        if (p->size < f->record_min) {
          cb_error(_("Record size too small '%s'"), p->name);
        }
      }
      if (f->record_max > 0) {
        if (p->size > f->record_max) {
          cb_error(_("Record size too large '%s' (%d)"), p->name, p->size);
        }
      }
    }
  }

  /* compute the record size */
  if (f->record_min == 0) {
    if (records) {
      f->record_min = records->size;
    } else {
      f->record_min = 0;
    }
  }
  for (p = records; p; p = p->sister) {
    v = cb_field_variable_size(p);
    if (v && v->offset + v->size * v->occurs_min < f->record_min) {
      f->record_min = v->offset + v->size * v->occurs_min;
    }
    if (p->size < f->record_min) {
      f->record_min = p->size;
    }
    if (p->size > f->record_max) {
      f->record_max = p->size;
    }
  }

  if (f->same_clause) {
    cb_tree l;
    for (l = current_program->file_list; l; l = CB_CHAIN(l)) {
      if (CB_FILE(CB_VALUE(l))->same_clause == f->same_clause) {
        if (CB_FILE(CB_VALUE(l))->finalized) {
          if (f->record_max > CB_FILE(CB_VALUE(l))->record->memory_size) {
            CB_FILE(CB_VALUE(l))->record->memory_size = f->record_max;
          }
          f->record = CB_FILE(CB_VALUE(l))->record;
          for (p = records; p; p = p->sister) {
            p->file = f;
            p->redefines = f->record;
          }
          for (p = f->record->sister; p; p = p->sister) {
            if (!p->sister) {
              p->sister = records;
              break;
            }
          }
          f->finalized = 1;
          return;
        }
      }
    }
  }
  /* create record */
  snprintf(buff, COB_MINI_MAX, "%s_record", f->name);
  if (f->record_max == 0) {
    f->record_max = 32;
    f->record_min = 32;
  }
  if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
    f->record_min = 0;
  }
  f->record = CB_FIELD(
      cb_build_implicit_field(cb_build_reference(buff), f->record_max));
  f->record->sister = records;
  f->record->count++;
  if (f->external) {
    has_external = 1;
    f->record->flag_external = 1;
  }

  for (p = records; p; p = p->sister) {
    p->file = f;
    p->redefines = f->record;
  }
  f->finalized = 1;
  if (f->linage) {
    snprintf(buff, COB_MINI_MAX, "LC_%s", f->name);
    cb_tree x = cb_build_field(cb_build_reference(buff));
    CB_FIELD(x)->pic = CB_PICTURE(cb_build_picture("9(9)"));
    CB_FIELD(x)->usage = CB_USAGE_COMP_5;
    CB_FIELD(x)->values = cb_list_init(cb_zero);
    CB_FIELD(x)->count++;
    cb_validate_field(CB_FIELD(x));
    f->linage_ctr = cb_build_field_reference(CB_FIELD(x), NULL);
    current_program->working_storage =
        cb_field_add(current_program->working_storage, CB_FIELD(x));
  }
}

/*
 * Reference
 */

cb_tree cb_build_reference(const char *name) {
  struct cb_reference *p;

  p = make_tree(CB_TAG_REFERENCE, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_reference));
  p->word = lookup_word(name);
  return CB_TREE(p);
}

cb_tree cb_build_filler(void) {
  cb_tree x;
  char name[16];

  sprintf(name, "WORK$%d", filler_id++);
  x = cb_build_reference(name);
  x->source_line = cb_source_line;
  return x;
}

cb_tree cb_build_anonymous(void) {
  cb_tree x;
  char name[16];

  sprintf(name, "ANON$%d", anonymous_id++);
  x = cb_build_reference(name);
  x->source_line = cb_source_line;
  return x;
}

cb_tree cb_build_field_reference(struct cb_field *f, cb_tree ref) {
  cb_tree x;
  struct cb_word *word;

  x = cb_build_reference(f->name);
  word = CB_REFERENCE(x)->word;
  if (ref) {
    memcpy(x, ref, sizeof(struct cb_reference));
  }
  x->category = CB_CATEGORY_UNKNOWN;
  CB_REFERENCE(x)->word = word;
  CB_REFERENCE(x)->value = CB_TREE(f);
  return x;
}

const char *cb_define(cb_tree name, cb_tree val) {
  struct cb_word *w;

  w = CB_REFERENCE(name)->word;
  w->items = cb_list_add(w->items, val);
  w->count++;
  val->source_file = name->source_file;
  val->source_line = name->source_line;
  CB_REFERENCE(name)->value = val;
  return w->name;
}

void cb_define_system_name(const char *name) {
  cb_tree x;

  x = cb_build_reference(name);
  if (CB_REFERENCE(x)->word->count == 0) {
    cb_define(x, lookup_system_name(name));
  }
}

cb_tree cb_ref(cb_tree x) {
  struct cb_reference *r;
  struct cb_field *p;
  struct cb_label *s;
  cb_tree candidate = NULL;
  cb_tree items;
  cb_tree cb1;
  cb_tree cb2;
  cb_tree v;
  cb_tree c;
  struct cb_program *prog;
  struct cb_word *w;
  size_t val;
  size_t ambiguous = 0;

  r = CB_REFERENCE(x);
  /* if this reference has already been resolved (and the value
     has been cached), then just return the value */
  if (r->value) {
    return r->value;
  }
  /* resolve the value */

  items = r->word->items;
  for (; items; items = CB_CHAIN(items)) {
    /* find a candidate value by resolving qualification */
    v = CB_VALUE(items);
    c = r->chain;
    switch (CB_TREE_TAG(v)) {
    case CB_TAG_FIELD:
      /* in case the value is a field, it might be qualified
         by its parent names and a file name */
      if (CB_FIELD(v)->flag_indexed_by) {
        p = CB_FIELD(v)->index_qual;
      } else {
        p = CB_FIELD(v)->parent;
      }
      /* resolve by parents */
      for (; p; p = p->parent) {
        if (c && cobc_casecmp(CB_NAME(c), p->name) == 0) {
          c = CB_REFERENCE(c)->chain;
        }
      }

      /* resolve by file */
      if (c && CB_REFERENCE(c)->chain == NULL) {
        if (CB_REFERENCE(c)->word->count == 1 && CB_FILE_P(cb_ref(c)) &&
            (CB_FILE(cb_ref(c)) == cb_field_founder(CB_FIELD(v))->file)) {
          c = CB_REFERENCE(c)->chain;
        }
      }

      break;
    case CB_TAG_LABEL:
      /* in case the value is a label, it might be qualified
         by its section name */
      s = CB_LABEL(v)->section;

      /* unqualified paragraph name referenced within the section
         is resolved without ambiguity check if not duplicated */
      if (c == NULL && r->offset && s == CB_LABEL(r->offset)) {
        for (cb1 = CB_CHAIN(items); cb1; cb1 = CB_CHAIN(cb1)) {
          cb2 = CB_VALUE(cb1);
          if (s == CB_LABEL(cb2)->section) {
            ambiguous_error(x);
            goto error;
          }
        }
        candidate = v;
        goto end;
      }

      /* resolve by section name */
      if (c && s && cobc_casecmp(CB_NAME(c), (char *)s->name) == 0) {
        c = CB_REFERENCE(c)->chain;
      }

      break;
    default:
      /* other values cannot be qualified */
      break;
    }

    /* a well qualified value is a good candidate */
    if (c == NULL) {
      if (candidate == NULL) {
        /* keep the first candidate */
        candidate = v;
      } else {
        /* multiple candidates and possibly ambiguous */
        ambiguous = 1;
        /* continue search because the reference might not
           be ambiguous and exit loop by "goto end" later */
      }
    }
  }

  /* there is no candidate */
  if (candidate == NULL) {
    if (current_program->nested_level > 0) {
      /* Nested program - check parents for GLOBAL candidate */
      ambiguous = 0;
      val = hash((const unsigned char *)r->word->name);
      prog = current_program->next_program;
      for (; prog; prog = prog->next_program) {
        if (prog->nested_level >= current_program->nested_level) {
          continue;
        }
        for (w = prog->word_table[val]; w; w = w->next) {
          if (cobc_casecmp(r->word->name, w->name) == 0) {
            candidate = global_check(r, w->items, &ambiguous);
            if (candidate) {
              if (ambiguous) {
                ambiguous_error(x);
                goto error;
              }
              if (CB_FILE_P(candidate)) {
                current_program->gen_file_error = 1;
              }
              goto end;
            }
          }
        }
        if (prog->nested_level == 0) {
          break;
        }
      }
    }
    undefined_error(x);
    goto error;
  }

  /* the reference is ambiguous */
  if (ambiguous) {
    ambiguous_error(x);
    goto error;
  }

end:
  if (CB_FIELD_P(candidate)) {
    CB_FIELD(candidate)->count++;
    if (CB_FIELD(candidate)->flag_invalid) {
      goto error;
    }
  }

  r->value = candidate;
  return r->value;

error:
  r->value = cb_error_node;
  return cb_error_node;
}

/*
 * Expression
 */

cb_tree cb_build_binary_op(cb_tree x, int op, cb_tree y) {
  struct cb_binary_op *p;
  enum cb_category category = CB_CATEGORY_UNKNOWN;

  switch (op) {
  case '+':
  case '-':
  case '*':
  case '/':
  case '^':
    /* arithmetic operators */
    if (CB_TREE_CLASS(x) == CB_CLASS_POINTER ||
        CB_TREE_CLASS(y) == CB_CLASS_POINTER) {
      category = CB_CATEGORY_DATA_POINTER;
      break;
    }
    x = cb_check_numeric_value(x);
    y = cb_check_numeric_value(y);
    if (x == cb_error_node || y == cb_error_node) {
      return cb_error_node;
    }
    if (cb_enable_zero_division_error && op == '/') {
      y = cb_check_zero_division(y);
      if (y == cb_error_node) {
        return cb_error_node;
      }
    }
    category = CB_CATEGORY_NUMERIC;
    break;

  case '=':
  case '~':
  case '<':
  case '>':
  case '[':
  case ']':
    /* relational operators */
    category = CB_CATEGORY_BOOLEAN;
    break;

  case '!':
  case '&':
  case '|':
    /* logical operators */
    if (CB_TREE_CLASS(x) != CB_CLASS_BOOLEAN ||
        (y && CB_TREE_CLASS(y) != CB_CLASS_BOOLEAN)) {
      cb_error(_("Invalid expression"));
      return cb_error_node;
    }
    category = CB_CATEGORY_BOOLEAN;
    break;

  case '@':
    /* parentheses */
    category = CB_TREE_CATEGORY(x);
    break;

  default:
    fprintf(stderr, "Unexpected operator -> %d\n", op);
    ABORT();
  }

  p = make_tree(CB_TAG_BINARY_OP, category, sizeof(struct cb_binary_op));
  if (x && CB_TREE(x)->source_file) {
    CB_TREE(p)->source_file = CB_TREE(x)->source_file;
    CB_TREE(p)->source_line = CB_TREE(x)->source_line;
  }
  p->op = op;
  p->x = x;
  p->y = y;
  return CB_TREE(p);
}

cb_tree cb_build_binary_list(cb_tree l, int op) {
  cb_tree e;

  e = CB_VALUE(l);
  for (l = CB_CHAIN(l); l; l = CB_CHAIN(l)) {
    e = cb_build_binary_op(e, op, CB_VALUE(l));
  }
  return e;
}

/*
 * Function call
 */

cb_tree cb_build_funcall(const char *name, int argc, cb_tree a1, cb_tree a2,
                         cb_tree a3, cb_tree a4, cb_tree a5, cb_tree a6,
                         cb_tree a7) {
  struct cb_funcall *p;

  p = make_tree(CB_TAG_FUNCALL, CB_CATEGORY_BOOLEAN, sizeof(struct cb_funcall));
  p->call_type = CB_CALL_FUNC;
  p->name = name;
  p->argc = argc;
  p->varcnt = 0;
  p->screenptr = gen_screen_ptr;
  p->argv[0] = a1;
  p->argv[1] = a2;
  p->argv[2] = a3;
  p->argv[3] = a4;
  p->argv[4] = a5;
  p->argv[5] = a6;
  p->argv[6] = a7;
  return CB_TREE(p);
}

/*
 * Method call
 */
cb_tree cb_build_method_call(const char *name, int argc, cb_tree a1, cb_tree a2,
                             cb_tree a3, cb_tree a4, cb_tree a5, cb_tree a6,
                             cb_tree a7) {
  struct cb_funcall *p;

  p = make_tree(CB_TAG_FUNCALL, CB_CATEGORY_BOOLEAN, sizeof(struct cb_funcall));
  p->call_type = CB_CALL_METHOD;
  p->name = name;
  p->argc = argc;
  p->varcnt = 0;
  p->screenptr = gen_screen_ptr;
  p->argv[0] = a1;
  p->argv[1] = a2;
  p->argv[2] = a3;
  p->argv[3] = a4;
  p->argv[4] = a5;
  p->argv[5] = a6;
  p->argv[6] = a7;
  return CB_TREE(p);
}

/*
 * Type cast
 */

cb_tree cb_build_cast(enum cb_cast_type type, cb_tree val) {
  struct cb_cast *p;
  enum cb_category category;

  if (type == CB_CAST_INTEGER) {
    category = CB_CATEGORY_NUMERIC;
  } else {
    category = CB_CATEGORY_UNKNOWN;
  }
  p = make_tree(CB_TAG_CAST, category, sizeof(struct cb_cast));
  p->type = type;
  p->val = val;
  return CB_TREE(p);
}

/*
 * Label
 */

cb_tree cb_build_label(cb_tree name, struct cb_label *section) {
  struct cb_label *p;

  p = make_tree(CB_TAG_LABEL, CB_CATEGORY_UNKNOWN, sizeof(struct cb_label));
  p->id = cb_id++;
  p->name = (const unsigned char *)cb_define(name, CB_TREE(p));
  p->orig_name = p->name;
  p->section = section;
  return CB_TREE(p);
}

/*
 * Assign
 */

cb_tree cb_build_assign(cb_tree var, cb_tree val) {
  struct cb_assign *p;

  p = make_tree(CB_TAG_ASSIGN, CB_CATEGORY_UNKNOWN, sizeof(struct cb_assign));
  p->var = var;
  p->val = val;
  return CB_TREE(p);
}

/*
 * INITIALIZE
 */

cb_tree cb_build_initialize(cb_tree var, cb_tree val, cb_tree rep, cb_tree def,
                            int flag) {
  struct cb_initialize *p;

  p = make_tree(CB_TAG_INITIALIZE, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_initialize));
  p->var = var;
  p->val = val;
  p->rep = rep;
  p->def = def;
  p->flag_statement = flag;
  return CB_TREE(p);
}

/*
 * SEARCH
 */

cb_tree cb_build_search(int flag_all, cb_tree table, cb_tree var,
                        cb_tree end_stmt, cb_tree whens) {
  struct cb_search *p;

  p = make_tree(CB_TAG_SEARCH, CB_CATEGORY_UNKNOWN, sizeof(struct cb_search));
  p->flag_all = flag_all;
  p->table = table;
  p->var = var;
  p->end_stmt = end_stmt;
  p->whens = whens;
  return CB_TREE(p);
}

/*
 * CALL
 */

cb_tree cb_build_call(cb_tree name, cb_tree args, cb_tree stmt1, cb_tree stmt2,
                      cb_tree returning, int is_system_call) {
  struct cb_call *p;

  p = make_tree(CB_TAG_CALL, CB_CATEGORY_UNKNOWN, sizeof(struct cb_call));
  p->name = name;
  p->args = args;
  p->stmt1 = stmt1;
  p->stmt2 = stmt2;
  p->returning = returning;
  p->is_system = is_system_call;
  return CB_TREE(p);
}

/*
 * GO TO
 */

cb_tree cb_build_goto(cb_tree target, cb_tree depending) {
  struct cb_goto *p;

  p = make_tree(CB_TAG_GOTO, CB_CATEGORY_UNKNOWN, sizeof(struct cb_goto));
  p->target = target;
  p->depending = depending;
  return CB_TREE(p);
}

cb_tree cb_build_java_continue(cb_tree target, cb_tree depending) {
  struct cb_goto *p;

  p = make_tree(CB_TAG_JAVA_CONTINUE, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_goto));
  p->target = target;
  p->depending = depending;
  return CB_TREE(p);
}

cb_tree cb_build_java_break(cb_tree target, cb_tree depending) {
  struct cb_goto *p;

  p = make_tree(CB_TAG_JAVA_BREAK, CB_CATEGORY_UNKNOWN, sizeof(struct cb_goto));
  p->target = target;
  p->depending = depending;
  return CB_TREE(p);
}

/*
 * IF
 */

cb_tree cb_build_if(cb_tree test, cb_tree stmt1, cb_tree stmt2) {
  struct cb_if *p;

  p = make_tree(CB_TAG_IF, CB_CATEGORY_UNKNOWN, sizeof(struct cb_if));
  p->test = test;
  p->stmt1 = stmt1;
  p->stmt2 = stmt2;
  return CB_TREE(p);
}

/*
 * PERFORM
 */

cb_tree cb_build_perform(int type) {
  struct cb_perform *p;

  p = make_tree(CB_TAG_PERFORM, CB_CATEGORY_UNKNOWN, sizeof(struct cb_perform));
  p->type = type;
  return CB_TREE(p);
}

cb_tree cb_build_perform_varying(cb_tree name, cb_tree from, cb_tree by,
                                 cb_tree until) {
  struct cb_perform_varying *p;

  p = make_tree(CB_TAG_PERFORM_VARYING, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_perform_varying));
  p->name = name;
  p->from = from;
  p->step = name ? cb_build_add(name, by, cb_high) : NULL;
  p->until = until;
  return CB_TREE(p);
}

/*
 * SORT
 */

cb_tree cb_build_sort_init(const char *name, cb_tree sort_file, cb_tree nkeys,
                           cb_tree col, cb_tree sort_return,
                           cb_tree file_status) {
  struct cb_sort_init *p;

  p = make_tree(CB_TAG_SORT_INIT, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_sort_init));
  p->name = name;
  p->sort_file = sort_file;
  p->nkeys = nkeys;
  p->col = col;
  p->sort_return = sort_return;
  p->file_status = file_status;
  return CB_TREE(p);
}

cb_tree cb_build_sort_proc(cb_tree body, cb_tree sort_file,
                           cb_tree sort_return) {
  struct cb_sort_proc *p;

  p = make_tree(CB_TAG_SORT_PROC, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_sort_proc));
  p->body = body;
  p->sort_file = sort_file;
  p->sort_return = sort_return;
  return CB_TREE(p);
}

cb_tree cb_build_return(cb_tree sort_file, cb_tree sort_return) {
  struct cb_return *p;

  p = make_tree(CB_TAG_RETURN, CB_CATEGORY_UNKNOWN, sizeof(struct cb_return));
  p->proc.sort_file = sort_file;
  p->proc.sort_return = sort_return;
  return CB_TREE(p);
}

cb_tree cb_build_release(cb_tree sort_file, cb_tree sort_return) {
  struct cb_release *p;

  p = make_tree(CB_TAG_RELEASE, CB_CATEGORY_UNKNOWN, sizeof(struct cb_release));
  p->proc.sort_file = sort_file;
  p->proc.sort_return = sort_return;
  return CB_TREE(p);
}

cb_tree cb_build_sort_finish(cb_tree sort_file, cb_tree sort_return) {
  struct cb_sort_finish *p;

  p = make_tree(CB_TAG_SORT_FINISH, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_sort_finish));
  p->sort_file = sort_file;
  p->sort_return = sort_return;
  return CB_TREE(p);
}

/*
 * Statement
 */

struct cb_statement *cb_build_statement(const char *name) {
  struct cb_statement *p;

  p = make_tree(CB_TAG_STATEMENT, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_statement));
  p->name = name;
  return p;
}

/*
 * CONTINUE
 */

cb_tree cb_build_continue(void) {
  struct cb_continue *p;

  p = make_tree(CB_TAG_CONTINUE, CB_CATEGORY_UNKNOWN,
                sizeof(struct cb_continue));
  return CB_TREE(p);
}

/*
 * SWITCH
 */

cb_tree cb_build_switch(cb_tree test, cb_tree case_list) {
  struct cb_switch *p;

  p = make_tree(CB_TAG_SWITCH, CB_CATEGORY_UNKNOWN, sizeof(struct cb_switch));
  p->test = test;
  p->case_list = case_list;
  return CB_TREE(p);
}

/*
 * FUNCTION
 */

cb_tree cb_build_any_intrinsic(cb_tree args) {
  struct cb_intrinsic_table *cbp;

  cbp = lookup_intrinsic("LENGTH", 0);
  return make_intrinsic(NULL, cbp, args, NULL, NULL);
}

cb_tree cb_build_intrinsic(cb_tree name, cb_tree args, cb_tree refmod) {
  struct cb_intrinsic_table *cbp;
  cb_tree x;
  int numargs;

  numargs = cb_list_length(args);

  cbp = lookup_intrinsic(CB_NAME(name), 0);
  if (cbp) {
    if ((cbp->args != -1 && numargs != cbp->args) ||
        (cbp->args == -1 && cbp->intr_enum != CB_INTR_RANDOM && numargs < 1)) {
      cb_error_x(name, _("FUNCTION %s has wrong number of arguments"),
                 cbp->name);
      return cb_error_node;
    }
    if (refmod) {
      if (!cbp->refmod) {
        cb_error_x(name, _("FUNCTION %s can not have reference modification"),
                   cbp->name);
        return cb_error_node;
      }
      if (CB_LITERAL_P(CB_PAIR_X(refmod)) &&
          cb_get_int(CB_PAIR_X(refmod)) < 1) {
        cb_error_x(name, _("FUNCTION %s has invalid reference modification"),
                   cbp->name);
        return cb_error_node;
      }
      if (CB_PAIR_Y(refmod) && CB_LITERAL_P(CB_PAIR_Y(refmod)) &&
          cb_get_int(CB_PAIR_Y(refmod)) < 1) {
        cb_error_x(name, _("FUNCTION %s has invalid reference modification"),
                   cbp->name);
        return cb_error_node;
      }
    }
    /* cb_tree      x; */
    switch (cbp->intr_enum) {
    case CB_INTR_LENGTH:
      x = CB_VALUE(args);
      if (CB_INTRINSIC_P(x)) {
        return make_intrinsic(name, cbp, args, NULL, NULL);
      } else if ((CB_FIELD_P(x) || CB_REFERENCE_P(x)) &&
                 cb_field(x)->flag_any_length) {
        return make_intrinsic(name, cbp, args, NULL, NULL);
      } else {
#ifdef I18N_UTF8
        /* I18N_UTF8: No wide char support,         */
        /*            use normal length() function. */
        return cb_build_length(CB_VALUE(args));
#else  /*!I18N_UTF8*/
        if ((cb_tree_class(x) == CB_CLASS_NATIONAL) ||
            (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL) ||
            (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL_EDITED)) {
          return cb_build_lengths(CB_VALUE(args));
        } else {
          return cb_build_length(CB_VALUE(args));
        }
#endif /*I18N_UTF8*/
      }

    case CB_INTR_BYTE_LENGTH:
    case CB_INTR_LENG:
    case CB_INTR_LENGTH_AN:
      if ((cbp->intr_enum == CB_INTR_LENG &&
           !cb_enable_leng_intrinsic_function) ||
          (cbp->intr_enum == CB_INTR_LENGTH_AN &&
           !cb_enable_length_an_intrinsic_function)) {
        cb_error_x(name, _("FUNCTION %s not implemented"), CB_NAME(name));
        return cb_error_node;
      }
      x = CB_VALUE(args);
      if (CB_INTRINSIC_P(x)) {
        return make_intrinsic(name, cbp, args, NULL, NULL);
      } else if ((CB_FIELD_P(x) || CB_REFERENCE_P(x)) &&
                 cb_field(x)->flag_any_length) {
        return make_intrinsic(name, cbp, args, NULL, NULL);
      } else {
        return cb_build_length(CB_VALUE(args));
      }

    case CB_INTR_WHEN_COMPILED:
      if (refmod) {
        return make_intrinsic(name, cbp, cb_list_init(cb_intr_whencomp), NULL,
                              refmod);
      } else {
        return cb_intr_whencomp;
      }
    case CB_INTR_PI:
      return cb_intr_pi;
    case CB_INTR_E:
      return cb_intr_e;

    case CB_INTR_LOWER_CASE:
    case CB_INTR_UPPER_CASE:
    case CB_INTR_REVERSE:
      /* RXW Why did I do this ? - still do not know
                              if (CB_INTRINSIC_P (CB_VALUE (args))) {
                                      return make_intrinsic (name, cbp, args,
      cb_int0); } else { return make_intrinsic (name, cbp, args, cb_build_length
      (CB_VALUE (args)));
                              }
      RXW */

    case CB_INTR_ABS:
    case CB_INTR_ACOS:
    case CB_INTR_ANNUITY:
    case CB_INTR_ASIN:
    case CB_INTR_ATAN:
    case CB_INTR_CHAR:
    case CB_INTR_COMBINED_DATETIME:
    case CB_INTR_COS:
    case CB_INTR_CURRENT_DATE:
    case CB_INTR_DATE_OF_INTEGER:
    case CB_INTR_DAY_OF_INTEGER:
    case CB_INTR_EXCEPTION_FILE:
    case CB_INTR_EXCEPTION_LOCATION:
    case CB_INTR_EXCEPTION_STATUS:
    case CB_INTR_EXCEPTION_STATEMENT:
    case CB_INTR_EXP:
    case CB_INTR_EXP10:
    case CB_INTR_FACTORIAL:
    case CB_INTR_FRACTION_PART:
    case CB_INTR_INTEGER:
    case CB_INTR_INTEGER_OF_DATE:
    case CB_INTR_INTEGER_OF_DAY:
    case CB_INTR_INTEGER_PART:
    case CB_INTR_LOCALE_DATE:
    case CB_INTR_LOCALE_TIME:
    case CB_INTR_LOCALE_TIME_FROM_SECS:
    case CB_INTR_LOG:
    case CB_INTR_LOG10:
    case CB_INTR_MOD:
    case CB_INTR_NUMVAL:
    case CB_INTR_NUMVAL_C:
    case CB_INTR_ORD:
    case CB_INTR_REM:
    case CB_INTR_SECONDS_FROM_FORMATTED_TIME:
    case CB_INTR_SECONDS_PAST_MIDNIGHT:
    case CB_INTR_SIGN:
    case CB_INTR_SIN:
    case CB_INTR_SQRT:
    case CB_INTR_STORED_CHAR_LENGTH:
    case CB_INTR_TAN:
    case CB_INTR_TEST_DATE_YYYYMMDD:
    case CB_INTR_TEST_DAY_YYYYDDD:
    case CB_INTR_TRIM:
      return make_intrinsic(name, cbp, args, NULL, refmod);
    case CB_INTR_NATIONAL:
      if (cb_enable_national_intrinsic_function) {
        return make_intrinsic(name, cbp, args, NULL, refmod);
      } else {
        cb_error_x(name, _("FUNCTION %s not implemented"), CB_NAME(name));
        return cb_error_node;
      }

    case CB_INTR_CONCATENATE:
      return make_intrinsic(name, cbp, args, cb_int1, refmod);
    case CB_INTR_DATE_TO_YYYYMMDD:
    case CB_INTR_DAY_TO_YYYYDDD:
    case CB_INTR_MAX:
    case CB_INTR_MEAN:
    case CB_INTR_MEDIAN:
    case CB_INTR_MIDRANGE:
    case CB_INTR_MIN:
    case CB_INTR_ORD_MAX:
    case CB_INTR_ORD_MIN:
    case CB_INTR_PRESENT_VALUE:
    case CB_INTR_RANDOM:
    case CB_INTR_RANGE:
    case CB_INTR_STANDARD_DEVIATION:
    case CB_INTR_SUM:
    case CB_INTR_VARIANCE:
    case CB_INTR_YEAR_TO_YYYY:
      return make_intrinsic(name, cbp, args, cb_int1, NULL);
    case CB_INTR_SUBSTITUTE:
    case CB_INTR_SUBSTITUTE_CASE:
      if (numargs < 3 || (numargs % 2) == 0) {
        cb_error_x(name, _("FUNCTION %s has wrong number of arguments"),
                   cbp->name);
        return cb_error_node;
      }
      return make_intrinsic(name, cbp, args, cb_int1, refmod);

    default:
      break;
    }
  }
  cb_error_x(name, _("FUNCTION %s not implemented"), CB_NAME(name));
  return cb_error_node;
}

char *cb_get_hexword(char *name) {
  unsigned char *p;
  int non_sjis_utf8 = 0;
  char *rt = NULL;

  for (p = (unsigned char *)name; *p;) {
    unsigned char c = *p;
#ifdef I18N_UTF8
    int n;
    if ((n = COB_U8BYTE_1(c))) {
      p += n;
    } else {
      non_sjis_utf8 = 1;
      break;
    }
#else  /*!I18N_UTF8*/
    if (c < 0x80) {
      p++;
    } else if ((0x81 <= c && c <= 0x9F) || (0xE0 <= c && c <= 0xEF)) {
      p += 2;
    } else {
      non_sjis_utf8 = 1;
      break;
    }
#endif /*I18N_UTF8*/
  }
  if (!non_sjis_utf8) {
    rt = strdup(name);
  } else {
    rt = cobc_malloc(strlen(name) * 2 + 7);
    p = (unsigned char *)name;
    char *p2 = rt;
    memcpy(p2, "___", 3);
    p2 += 3;
    while (*p) {
      sprintf(p2, "%02X", *p++);
      p2 += 2;
    }
    memcpy(p2, "___", 3);
    p2 += 3;
    *p2 = '\0';
  }
  return rt;
}

char *cb_get_jisword_buff(const char *name, char *jbuf, size_t n) {
  const char *cs, *ce;
  int flag_quoted = 0;
  char *rt = NULL;

  cs = &(name[0]);
  ce = &(name[strlen(name) - 1]);

  /* strip quotes */
  if (*cs == '\'' && *ce == '\'') {
    cs++;
    --ce;
    flag_quoted = 1;
  }

  /* decode if encoded */
  if (ce - cs >= 5 && !memcmp(cs, "___", 3) && !memcmp(ce - 2, "___", 3)) {
    cs += 3;
    ce -= 3;
    size_t siz;
    if (!flag_quoted) {
      siz = (ce - cs + 1) / 2 + 1;
    } else {
      siz = (ce - cs + 1) / 2 + 3;
    }
    unsigned int c;
    if (!jbuf) {
      rt = cobc_malloc(siz);
    } else {
      if (siz > n) {
        c = siz - n;
        siz -= c;
        ce -= c * 2;
      }
      memset(jbuf, 0, n);
      rt = jbuf;
    }
    char *p;
    if (flag_quoted && siz > 2) {
      rt[0] = rt[siz - 2] = '\'';
      p = &(rt[1]);
    } else {
      p = &(rt[0]);
    }
    const char *cp;
    for (c = 1, cp = cs; cp <= ce; cp++, p += (c = !c)) {
      if (*cp >= '0' && *cp <= '9') {
        *p |= (*cp - '0') << (c << 2);
      } else if (*cp >= 'A' && *cp <= 'F') {
        *p |= (*cp - 'A' + 10) << (c << 2);
      } else {
        *p = '?';
        cp += c;
        c = 0;
      }
    }
  } else {
    if (!jbuf) {
      rt = strdup(name);
    } else {
      memset(jbuf, 0, n);
      strncpy(jbuf, name, n - 1);
      rt = jbuf;
    }
  }
  return rt;
}

char *cb_get_jisword(const char *name) {
  return cb_get_jisword_buff(name, NULL, 0);
}
