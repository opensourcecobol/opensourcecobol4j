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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#include "cobj.h"
#include "tree.h"

struct system_table {
  const char *syst_name;
  const int syst_params;
};

struct expr_node {
  /* The token of this node.
   *  'x'                          - values (cb_tree)
   *  '+', '-', '*', '/', '^'      - arithmetic operators
   *  '=', '~', '<', '>', '[', ']' - relational operators
   *  '!', '&', '|'                - logical operators
   *  '(', ')'                     - parentheses
   */
  int token;
  /* The value itself if this node is a value */
  cb_tree value;
};

#define START_STACK_SIZE 32
#define TOKEN(offset) (expr_stack[expr_index + offset].token)
#define VALUE(offset) (expr_stack[expr_index + offset].value)

#define dpush(x) decimal_stack = cb_cons(x, decimal_stack)

#define cb_emit(x)                                                             \
  current_statement->body = cb_list_add(current_statement->body, x)
#define cb_emit_list(l)                                                        \
  current_statement->body = cb_list_append(current_statement->body, l)

/* Global variables */

size_t sending_id = 0;
size_t suppress_warn = 0;

/* Local variables */

static cb_tree decimal_stack = NULL;

static const char *inspect_func;
static cb_tree inspect_data;

static int expr_op;     /* last operator */
static cb_tree expr_lh; /* last left hand */

static int expr_index;               /* stack index */
static int expr_stack_size;          /* stack max size */
static struct expr_node *expr_stack; /* expr node stack */

static char expr_prio[256];

static const struct system_table system_tab[] = {
#undef COB_SYSTEM_GEN
#define COB_SYSTEM_GEN(x, y, z) {x, y},
#include <system.def>
    {NULL, 0}};

static const char *const bin_set_funcs[] = {NULL,
                                            "setSwpU16Binary",
                                            "setSwpU24Binary",
                                            "setSwpU32Binary",
                                            "setSwpU40Binary",
                                            "setSwpU48Binary",
                                            "setSwpU56Binary",
                                            "setSwpU64Binary",
                                            NULL,
                                            "setSwpS16Binary",
                                            "setSwpS24Binary",
                                            "setSwpS32Binary",
                                            "setSwpS40Binary",
                                            "setSwpS48Binary",
                                            "setSwpS56Binary",
                                            "setSwpS64Binary"};

static const char *const bin_compare_funcs[] = {
    "cmpU8Binary",     "cmpU16Binary",    "cmpU24Binary",    "cmpU32Binary",
    "cmpU40Binary",    "cmpU48Binary",    "cmpU56Binary",    "cmpU64Binary",
    "cmpS8Binary",     "cmpS16Binary",    "cmpS24Binary",    "cmpS32Binary",
    "cmpS40Binary",    "cmpS48Binary",    "cmpS56Binary",    "cmpS64Binary",
    "cmpU8Binary",     "cmpSwpU16Binary", "cmpSwpU24Binary", "cmpSwpU32Binary",
    "cmpSwpU40Binary", "cmpSwpU48Binary", "cmpSwpU56Binary", "cmpSwpU64Binary",
    "cmpS8Binary",     "cmpSwpS16Binary", "cmpSwpS24Binary", "cmpSwpS32Binary",
    "cmpSwpS40Binary", "cmpSwpS48Binary", "cmpSwpS56Binary", "cmpSwpS64Binary"};

static const char *const bin_add_funcs[] = {
    "addU8Binary",     "addU16Binary",    "addU24Binary",    "addU32Binary",
    "addU40Binary",    "addU48Binary",    "addU56Binary",    "addU64Binary",
    "addS8Binary",     "addS16Binary",    "addS24Binary",    "addS32Binary",
    "addS40Binary",    "addS48Binary",    "addS56Binary",    "addS64Binary",
    "addU8Binary",     "addSwpU16Binary", "addSwpU24Binary", "addSwpU32Binary",
    "addSwpU40Binary", "addSwpU48Binary", "addSwpU56Binary", "addSwpU64Binary",
    "addS8Binary",     "addSwpS16Binary", "addSwpS24Binary", "addSwpS32Binary",
    "addSwpS40Binary", "addSwpS48Binary", "addSwpS56Binary", "addSwpS64Binary"};

static const char *const bin_sub_funcs[] = {
    "subU8Binary",     "subU16Binary",    "subU24Binary",    "subU32Binary",
    "subU40Binary",    "subU48Binary",    "subU56Binary",    "subU64Binary",
    "subS8Binary",     "subS16Binary",    "subS24Binary",    "subS32Binary",
    "subS40Binary",    "subS48Binary",    "subS56Binary",    "subS64Binary",
    "subU8Binary",     "subSwpU16Binary", "subSwpU24Binary", "subSwpU32Binary",
    "subSwpU40Binary", "subSwpU48Binary", "subSwpU56Binary", "subSwpU64Binary",
    "subS8Binary",     "subSwpS16Binary", "subSwpS24Binary", "subSwpS32Binary",
    "subSwpS40Binary", "subSwpS48Binary", "subSwpS56Binary", "subSwpS64Binary"};

/* functions */

static size_t cb_validate_one(cb_tree x) {
  cb_tree y;

  if (x == cb_error_node) {
    return 1;
  }
  if (!x) {
    return 0;
  }
  if (CB_REFERENCE_P(x)) {
    y = cb_ref(x);
    if (y == cb_error_node) {
      return 1;
    }
    if (CB_FIELD_P(y) && CB_FIELD(y)->level == 88) {
      cb_error_x(x, _("Invalid use of 88 level item"));
      return 1;
    }
  }
  return 0;
}

static size_t cb_validate_numeric(cb_tree x) {
  if (x == cb_error_node) {
    return 1;
  }
  if (!x) {
    return 0;
  }
  if (CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC) {
    return 0;
  }
  cb_error_x(x, _("'%s' must be a numeric type!"), cb_name(x));
  return 1;
}

static size_t cb_validate_list(cb_tree l) {
  if (l == cb_error_node) {
    return 1;
  }
  for (; l; l = CB_CHAIN(l)) {
    if (cb_validate_one(CB_VALUE(l))) {
      return 1;
    }
  }
  return 0;
}

static cb_tree cb_check_group_name(cb_tree x) {
  cb_tree y;

  if (x == cb_error_node) {
    return cb_error_node;
  }

  if (CB_REFERENCE_P(x)) {
    y = cb_ref(x);
    if (y == cb_error_node) {
      return cb_error_node;
    }
    if (CB_FIELD_P(y) && CB_FIELD(y)->children != NULL &&
        CB_REFERENCE(x)->offset == NULL) {
      return x;
    }
  }

  cb_error_x(x, _("'%s' is not group name"), cb_name(x));
  return cb_error_node;
}

static cb_tree cb_check_numeric_name(cb_tree x) {
  if (x == cb_error_node) {
    return cb_error_node;
  }

  if (CB_REFERENCE_P(x) && CB_FIELD_P(cb_ref(x)) &&
      CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC) {
    return x;
  }

  cb_error_x(x, _("'%s' is not a numeric name"), cb_name(x));
  return cb_error_node;
}

static cb_tree cb_check_numeric_edited_name(cb_tree x) {
  if (x == cb_error_node) {
    return cb_error_node;
  }

  if (CB_REFERENCE_P(x) && CB_FIELD_P(cb_ref(x)) &&
      (CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC ||
       CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC_EDITED)) {
    return x;
  }

  cb_error_x(x, _("'%s' is not numeric or numeric-edited name"), cb_name(x));
  return cb_error_node;
}

cb_tree cb_check_numeric_value(cb_tree x) {
  if (x == cb_error_node) {
    return cb_error_node;
  }

  if (CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC) {
    return x;
  }

  cb_error_x(x, _("'%s' is not a numeric value"), cb_name(x));
  return cb_error_node;
}

static cb_tree cb_check_integer_value(cb_tree x) {
  struct cb_literal *l;
  struct cb_field *f;
  cb_tree y;

  if (x == cb_error_node) {
    return cb_error_node;
  }

  if (CB_TREE_CATEGORY(x) != CB_CATEGORY_NUMERIC) {
    goto invalid;
  }

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CONST:
    if (x != cb_zero) {
      goto invalid;
    }
    return x;
  case CB_TAG_LITERAL:
    l = CB_LITERAL(x);
    if (l->sign < 0 || l->scale > 0) {
      goto invliteral;
    }
    return x;
  case CB_TAG_REFERENCE:
    y = cb_ref(x);
    if (y == cb_error_node) {
      return cb_error_node;
    }
    f = CB_FIELD(y);
    if (f->pic->scale > 0) {
      goto invalid;
    }
    return x;
  case CB_TAG_BINARY_OP:
    /* TODO: need to check */
    return x;
  case CB_TAG_INTRINSIC:
    /* TODO: need to check */
    return x;
  default:
  invalid:
    cb_error_x(x, _("'%s' is not an integer value"), cb_name(x));
    return cb_error_node;
  }
invliteral:
  cb_error_x(x, _("A positive numeric integer is required here"));
  return cb_error_node;
}

void cb_build_registers(void) {
#if !defined(__linux__) && !defined(__CYGWIN__) && defined(HAVE_TIMEZONE)
  long contz;
#endif
  time_t t;
  char buff[48];

  /* RETURN-CODE */
  if (!current_program->nested_level) {
    current_program->cb_return_code =
        cb_build_index(cb_build_reference("RETURN-CODE"), cb_zero, 0, NULL);
    cb_field(current_program->cb_return_code)->flag_is_global = 1;
  }

  /* SORT-RETURN */
  current_program->cb_sort_return =
      cb_build_index(cb_build_reference("SORT-RETURN"), cb_zero, 0, NULL);
  cb_field(current_program->cb_sort_return)->flag_no_init = 1;

  /* NUMBER-OF-CALL-PARAMETERS */
  current_program->cb_call_params = cb_build_index(
      cb_build_reference("NUMBER-OF-CALL-PARAMETERS"), cb_zero, 0, NULL);
  cb_field(current_program->cb_call_params)->flag_no_init = 1;

  /* TALLY */
  /* 01 TALLY GLOBAL PICTURE 9(9) USAGE COMP-5 VALUE ZERO. */
  /* TALLY/EXAMINE  not standard/supported */

  t = time(NULL);

  /* WHEN-COMPILED */
  memset(buff, 0, sizeof(buff));
  strftime(buff, 17, "%m/%d/%y%H.%M.%S", localtime(&t));
  cb_build_constant(cb_build_reference("WHEN-COMPILED"),
                    cb_build_alphanumeric_literal((ucharptr)buff, 16));

  /* FUNCTION WHEN-COMPILED */
  memset(buff, 0, sizeof(buff));
#if defined(__linux__) || defined(__CYGWIN__)
  strftime(buff, 22, "%Y%m%d%H%M%S00%z", localtime(&t));
#elif defined(HAVE_TIMEZONE)
  strftime(buff, 17, "%Y%m%d%H%M%S00", localtime(&t));
  if (timezone <= 0) {
    contz = -timezone;
    buff[16] = '+';
  } else {
    contz = timezone;
    buff[16] = '-';
  }
  sprintf(&buff[17], "%2.2ld%2.2ld", contz / 3600, contz % 60);
#else
  strftime(buff, 22, "%Y%m%d%H%M%S0000000", localtime(&t));
#endif
  cb_intr_whencomp = cb_build_alphanumeric_literal((ucharptr)buff, 21);

  /* FUNCTION PI */
  memset(buff, 0, sizeof(buff));
  strcpy(buff, "31415926535897932384626433832795029");
  cb_intr_pi = cb_build_numeric_literal(0, (ucharptr)buff, 34);

  /* FUNCTION E */
  memset(buff, 0, sizeof(buff));
  strcpy(buff, "27182818284590452353602874713526625");
  cb_intr_e = cb_build_numeric_literal(0, (ucharptr)buff, 34);
}

char *cb_encode_program_id(const char *name) {
  unsigned char *p;
  const unsigned char *s;
  unsigned char buff[COB_SMALL_BUFF];

  p = buff;
  s = (const unsigned char *)name;
  /* encode the initial digit */
  if (isdigit(*s)) {
    p += sprintf((char *)p, "_%02X", *s++);
  }
  /* encode invalid letters */
  for (; *s; s++) {
    if (isalnum(*s) || *s == '_') {
      *p++ = *s;
    } else if (*s == '-') {
      *p++ = '_';
      *p++ = '_';
    } else {
      p += sprintf((char *)p, "_%02X", *s);
    }
  }
  *p = 0;
  return strdup((char *)buff);
}

const char *cb_build_program_id(cb_tree name, cb_tree alt_name) {
  const char *s;

  /* This needs some more thought, should we generate an entry
          point per program source name ?
          if (alt_name) {
                  s = (char *)CB_LITERAL (alt_name)->data;
          } else if (CB_LITERAL_P (name)) {
                  s = (char *)CB_LITERAL (name)->data;
          } else {
                  s = (char *)CB_NAME (name);
          }

          if (!cb_flag_main && strcmp (s, source_name)) {
                  cb_warning (_("Source name '%s' differs from PROGRAM-ID
   '%s'"), source_name, s); current_program->source_name = strdup (source_name);
          }
   End comment out */

  if (alt_name) {
    current_program->orig_source_name =
        strdup((char *)CB_LITERAL(alt_name)->data);
    s = (char *)CB_LITERAL(alt_name)->data;
  } else if (CB_LITERAL_P(name)) {
    current_program->orig_source_name = strdup((char *)CB_LITERAL(name)->data);
    s = cb_encode_program_id((char *)CB_LITERAL(name)->data);
  } else {
    current_program->orig_source_name = strdup(CB_NAME(name));
    s = cb_encode_program_id(CB_NAME(name));
  }
  if (cobc_check_valid_name(current_program->orig_source_name)) {
    cb_error(_("PROGRAM-ID '%s' invalid"), current_program->orig_source_name);
  }
  return s;
}

void cb_define_switch_name(cb_tree name, cb_tree sname, cb_tree flag,
                           cb_tree ref) {
  cb_tree switch_id;
  cb_tree value;

  if (name == cb_error_node) {
    return;
  }
  if (sname == cb_error_node) {
    return;
  }
  if (CB_SYSTEM_NAME(sname)->category != CB_SWITCH_NAME) {
    if (!ref) {
      cb_error_x(ref, _("Switch-name is expected '%s'"), CB_NAME(ref));
    } else {
      cb_error_x(name, _("'%s' with no Switch-name"), CB_NAME(name));
    }
  } else {
    switch_id = cb_int(CB_SYSTEM_NAME(sname)->token);
    value = cb_build_funcall_1("CobolUtil.getSwitch", switch_id);
    if (flag == cb_int0) {
      value = cb_build_negation(value);
    }
    cb_build_constant(name, value);
  }
}

cb_tree cb_build_section_name(cb_tree name, int sect_or_para) {
  cb_tree x;

  if (name == cb_error_node) {
    return cb_error_node;
  }

  if (CB_REFERENCE(name)->word->count > 0) {
    x = CB_VALUE(CB_REFERENCE(name)->word->items);
    /* Used as a non-label name or used as a section name.
       Duplicate paragraphs are allowed if not referenced;
       Checked in typeck.c */
    if (!CB_LABEL_P(x) || sect_or_para == 0 ||
        (sect_or_para && CB_LABEL_P(x) && CB_LABEL(x)->is_section)) {
      redefinition_error(name);
      return cb_error_node;
    }
  }

  return name;
}

static char *get_coded_filename(const char *cname, int idx) {
  const char *p = cname;
  int cnt = 0;
  char *rt = NULL;

  while (*p && cnt < idx) {
    if (*p == '-') {
      cnt++;
    }
    p++;
  }
  if (*p) {
    rt = strdup(p);
    p = rt;
    while (*p && *p != '-') {
      p++;
    }
    rt[p - rt] = '\0';
  }
  return rt;
}

cb_tree cb_build_assignment_name(struct cb_file *cfile, cb_tree name) {
  const char *s;
  const char *p;
  cb_tree x;
  char *pp;

  if (name == cb_error_node) {
    return cb_error_node;
  }

  if (cfile->fileid_assign == 1 && cfile->external_assign != 1) {
    return NULL;
  }

  switch (CB_TREE_TAG(name)) {
  case CB_TAG_LITERAL:
    if (strcmp((char *)(CB_LITERAL(name)->data), "$#@DUMMY@#$") == 0) {
      cfile->special = 2;
    }
    return name;

  case CB_TAG_REFERENCE:
    s = CB_REFERENCE(name)->word->name;
    if (strcasecmp(s, "KEYBOARD") == 0) {
      s = "#DUMMY#";
      cfile->special = 1;
      return cb_build_alphanumeric_literal((ucharptr)s, strlen(s));
    }
    switch (cb_assign_clause) {
    case CB_ASSIGN_COBOL2002:
      /* TODO */
      return cb_error_node;

    case CB_ASSIGN_MF:
      if (cfile->external_assign) {
        p = strrchr(s, '-');
        if (p) {
          s = p + 1;
        }
        return cb_build_alphanumeric_literal((ucharptr)s, strlen(s));
      }
      current_program->reference_list =
          cb_list_add(current_program->reference_list, name);
      return name;

    case CB_ASSIGN_IBM:
      /* check organization */
      if (strncmp(s, "S-", 2) == 0 || strncmp(s, "AS-", 3) == 0) {
        goto org;
      }
      /* skip the device label if exists */
      if ((p = strchr(s, '-')) != NULL) {
        s = p + 1;
      }
      /* check organization again */
      if (strncmp(s, "S-", 2) == 0 || strncmp(s, "AS-", 3) == 0) {
      org:
        /* skip it for now */
        s = strchr(s, '-') + 1;
      }
      /* convert the name into literal */
      return cb_build_alphanumeric_literal((ucharptr)s, strlen(s));

    case CB_ASSIGN_JPH1:
      if (!(pp = get_coded_filename(s, 4))) {
        pp = get_coded_filename(s, 0);
      }
      if (pp) {
        x = cb_build_alphanumeric_literal((ucharptr)pp, strlen(pp));
        free(pp);
      } else {
        x = cb_build_alphanumeric_literal((ucharptr)s, strlen(s));
      }
      return x;
    }
  default:
    return cb_error_node;
  }
}

cb_tree cb_build_index(cb_tree x, cb_tree values, int indexed_by,
                       struct cb_field *qual) {
  struct cb_field *f;

  f = CB_FIELD(cb_build_field(x));
  f->usage = CB_USAGE_INDEX;
  cb_validate_field(f);
  if (values) {
    f->values = cb_list_init(values);
  }
  if (qual) {
    f->index_qual = qual;
  }
  f->flag_indexed_by = indexed_by;
  current_program->working_storage =
      cb_field_add(current_program->working_storage, f);
  return x;
}

int cb_reference_type_check(cb_tree ref, cb_tree x, const char *name, int size,
                            int *retsize, int type) {
  struct cb_field *pTmp;
  struct cb_binary_op *p;
  struct cb_reference *r;
  char strbuf[256];
  int offset = 0;
  int ret = 0;
  COB_UNUSED(r);

  r = CB_REFERENCE(ref);
  switch (CB_TREE_TAG(x)) {
  case CB_TAG_REFERENCE:
    pTmp = CB_FIELD(cb_ref(x));
    if (CB_TREE(pTmp) != cb_error_node) {
      if (pTmp->pic) {
        if (pTmp->pic->category != CB_CATEGORY_NUMERIC) {
          cb_error_x(x, _("'%s' is not a numeric value"), pTmp->name);
          ret = 1;
        }
      }
    }
    break;
  case CB_TAG_LITERAL:
    if (!cb_is_digist_data(x)) {
      memset(strbuf, 0, sizeof(strbuf));
      sprintf(strbuf, "%s", CB_LITERAL(x)->data);
      if (type) {
        cb_error_x(x, _("Offset of '%s' out of bounds: %s "), name, strbuf);
      } else {
        cb_error_x(x, _("Length of '%s' out of bounds: %s "), name, strbuf);
      }
      ret = 1;
    } else {
      offset = cb_get_int(x);
      if (offset < 1 || offset > size) {
        if (type) {
          cb_error_x(x, _("Offset of '%s' out of bounds: %d"), name, offset);
        } else {
          cb_error_x(x, _("Length of '%s' out of bounds: %d"), name, offset);
        }
        ret = 1;
      }
    }
    break;
  case CB_TAG_BINARY_OP:
    if (cb_tree_category(ref) == CB_CATEGORY_NATIONAL ||
        cb_tree_category(ref) == CB_CATEGORY_NATIONAL_EDITED) {
      p = CB_BINARY_OP(x);
      return cb_reference_type_check(ref, p->x, name, size, retsize, type);
    }
  default:
    break;
  }
  *retsize += offset;
  return ret;
}

cb_tree cb_build_identifier(cb_tree x) {
  struct cb_reference *r;
  struct cb_field *f;
  struct cb_field *p;
  const char *name;
  cb_tree v;
  cb_tree e1;
  cb_tree e2;
  cb_tree l;
  cb_tree sub;
  int size;
  int n;

  if (x == cb_error_node) {
    return cb_error_node;
  }

  r = CB_REFERENCE(x);
  name = r->word->name;

  /* resolve reference */
  v = cb_ref(x);
  if (v == cb_error_node) {
    return cb_error_node;
  }

  /* check if it is a data name */
  if (!CB_FIELD_P(v)) {
    if (r->subs) {
      cb_error_x(x, _("'%s' cannot be subscripted"), name);
      return cb_error_node;
    }
    if (r->offset) {
      cb_error_x(x, _("'%s' cannot be reference modified"), name);
      return cb_error_node;
    }
    return x;
  }
  f = CB_FIELD(v);

  /* BASED check */
  if (CB_EXCEPTION_ENABLE(COB_EC_BOUND_PTR)) {
    for (p = f; p->parent; p = p->parent) {
      ;
    }
    if (current_statement) {
      if (p->flag_item_based ||
          (f->storage == CB_STORAGE_LINKAGE && !p->flag_is_pdiv_parm)) {
        current_statement->null_check = cb_build_funcall_2(
            "cob_check_based",
            cb_build_address(cb_build_field_reference(p, NULL)),
            cb_build_string0((ucharptr)name));
      }
    }
  }

  /* check the number of subscripts */
  if (!r->all && cb_list_length(r->subs) != f->indexes) {
    switch (f->indexes) {
    case 0:
      cb_error_x(x, _("'%s' cannot be subscripted"), name);
      return cb_error_node;
    case 1:
      cb_error_x(x, _("'%s' requires 1 subscript"), name);
      return cb_error_node;
    default:
      cb_error_x(x, _("'%s' requires %d subscripts"), name, f->indexes);
      return cb_error_node;
    }
  }

  /* subscript check */
  if (!r->all && r->subs) {
    l = r->subs;
    for (p = f; p; p = p->parent) {
      if (p->flag_occurs) {
        sub = cb_check_integer_value(CB_VALUE(l));

        l = CB_CHAIN(l);

        if (sub == cb_error_node) {
          continue;
        }

        /* compile-time check */
        if (CB_LITERAL_P(sub)) {
          n = cb_get_int(sub);
          if (n < 1 || n > p->occurs_max) {
            cb_error_x(x, _("Subscript of '%s' out of bounds: %d"), name, n);
          }
        }

        /* run-time check */
        if (CB_EXCEPTION_ENABLE(COB_EC_BOUND_SUBSCRIPT)) {
          if (p->occurs_depending) {
            if (CB_FIELD(cb_ref(p->occurs_depending))->values) {
              e1 = cb_build_funcall_4(
                  "CobolCheck.checkOdo",
                  cb_build_cast_integer(p->occurs_depending),
                  cb_int(p->occurs_min), cb_int(p->occurs_max),
                  cb_build_string0(
                      (ucharptr)(cb_field(p->occurs_depending)->name)));
              e2 = cb_build_funcall_4(
                  "CobolCheck.checkSubscript", cb_build_cast_integer(sub),
                  cb_int1, cb_build_cast_integer(p->occurs_depending),
                  cb_build_string0((ucharptr)name));
            } else {
              e1 = cb_build_funcall_4(
                  "CobolCheck.checkOdo", cb_int(p->occurs_max),
                  cb_int(p->occurs_min), cb_int(p->occurs_max),
                  cb_build_string0(
                      (ucharptr)(cb_field(p->occurs_depending)->name)));
              e2 = cb_build_funcall_4("CobolCheck.checkSubscript",
                                      cb_build_cast_integer(sub), cb_int1,
                                      cb_int(p->occurs_max),
                                      cb_build_string0((ucharptr)name));
            }
            r->check = cb_list_add(r->check, e1);
            r->check = cb_list_add(r->check, e2);
          } else {
            if (!CB_LITERAL_P(sub)) {
              e1 = cb_build_funcall_4("CobolCheck.checkSubscript",
                                      cb_build_cast_integer(sub), cb_int1,
                                      cb_int(p->occurs_max),
                                      cb_build_string0((ucharptr)name));
              r->check = cb_list_add(r->check, e1);
            }
          }
        }
      }
    }
  }

  /* reference modification check */
  if (r->offset) {
    /* compile-time check */
#ifdef I18N_UTF8
    /* I18N_UTF8: No wide char support. */
    size = 0;
    if (!cb_reference_type_check(x, r->offset, name, f->size, &size, 1)) {
      if (size <= f->size && r->length) {
        cb_reference_type_check(x, r->length, name, f->size - size + 1, &size,
                                0);
      }
    }
#else  /*!I18N_UTF8*/
    size = 0;
    if (cb_tree_category(CB_TREE(r)) == CB_CATEGORY_NATIONAL ||
        cb_tree_category(CB_TREE(r)) == CB_CATEGORY_NATIONAL_EDITED) {
      if (!cb_reference_type_check(x, r->offset, name, (f->size) / 2, &size,
                                   1)) {
        if (size <= (f->size) / 2) {
          if (r->length) {
            cb_reference_type_check(x, r->length, name,
                                    (f->size) / 2 - size + 1, &size, 0);
          }
        }
      }
    } else {
      if (!cb_reference_type_check(x, r->offset, name, (f->size), &size, 1)) {
        if (size <= (f->size)) {
          if (r->length) {
            cb_reference_type_check(x, r->length, name, (f->size) - size + 1,
                                    &size, 0);
          }
        }
      }
    }
#endif /*I18N_UTF8*/

    /* run-time check */
#ifdef I18N_UTF8
    /* I18N_UTF8: No wide char support. */
    if (CB_EXCEPTION_ENABLE(COB_EC_BOUND_REF_MOD)) {
      if (!CB_LITERAL_P(r->offset) || (r->length && !CB_LITERAL_P(r->length))) {
        e1 = cb_build_funcall_4(
            "cob_check_ref_mod", cb_build_cast_integer(r->offset),
            r->length ? cb_build_cast_integer(r->length) : cb_int1,
            cb_int(f->size), cb_build_string0((ucharptr)f->name));
        r->check = cb_list_add(r->check, e1);
      }
    }
#else  /*!I18N_UTF8*/
    if (CB_EXCEPTION_ENABLE(COB_EC_BOUND_REF_MOD)) {
      if (!CB_LITERAL_P(r->offset) || (r->length && !CB_LITERAL_P(r->length))) {
        if (cb_tree_category(CB_TREE(r)) == CB_CATEGORY_NATIONAL ||
            cb_tree_category(CB_TREE(r)) == CB_CATEGORY_NATIONAL_EDITED) {
          e1 = cb_build_funcall_4(
              "cob_check_ref_mod_national", cb_build_cast_integer(r->offset),
              r->length ? cb_build_cast_integer(r->length) : cb_int2,
              cb_int(f->size), cb_build_string0((ucharptr)f->name));
        } else {
          e1 = cb_build_funcall_4(
              "cob_check_ref_mod", cb_build_cast_integer(r->offset),
              r->length ? cb_build_cast_integer(r->length) : cb_int1,
              cb_int(f->size), cb_build_string0((ucharptr)f->name));
        }
        r->check = cb_list_add(r->check, e1);
      }
    }
#endif /*I18N_UTF8*/
  }

  if (f->storage == CB_STORAGE_CONSTANT) {
    return CB_VALUE(f->values);
  }

  return x;
}

static cb_tree cb_build_length_1(cb_tree x) {
  struct cb_field *f;
  cb_tree e;
  cb_tree size;

  f = CB_FIELD(cb_ref(x));

  if (cb_field_variable_size(f) == NULL) {
    /* constant size */
    return cb_int(cb_field_size(x));
  } else {
    /* variable size */
    e = NULL;
    for (f = f->children; f; f = f->sister) {
      size = cb_build_length_1(cb_build_field_reference(f, x));
      if (f->occurs_depending) {
        size = cb_build_binary_op(size, '*', f->occurs_depending);
      } else if (f->occurs_max > 1) {
        size = cb_build_binary_op(size, '*', cb_int(f->occurs_max));
      }
      e = e ? cb_build_binary_op(e, '+', size) : size;
    }
    return e;
  }
}

cb_tree cb_build_const_length(cb_tree x) {
  struct cb_field *f;
  char buff[64];

  if (x == cb_error_node) {
    return cb_error_node;
  }
  if (CB_REFERENCE_P(x) && cb_ref(x) == cb_error_node) {
    return cb_error_node;
  }

  memset(buff, 0, sizeof(buff));
  f = CB_FIELD(cb_ref(x));
  if (f->flag_any_length) {
    cb_error(_("ANY LENGTH item not allowed here"));
    return cb_error_node;
  }
  if (f->level == 88) {
    cb_error(_("88 level item not allowed here"));
    return cb_error_node;
  }
  if (!f->flag_is_verified) {
    cb_validate_field(f);
  }
  sprintf(buff, "%d", f->memory_size);
  return cb_build_numeric_literal(0, (ucharptr)buff, 0);
}

cb_tree cb_build_length(cb_tree x) {
  struct cb_field *f;
  struct cb_literal *l;
  cb_tree temp;
  char buff[64];

  if (x == cb_error_node) {
    return cb_error_node;
  }
  if (CB_REFERENCE_P(x) && cb_ref(x) == cb_error_node) {
    return cb_error_node;
  }

  memset(buff, 0, sizeof(buff));
  if (CB_LITERAL_P(x)) {
    l = CB_LITERAL(x);
    sprintf(buff, "%d", (int)l->size);
    return cb_build_numeric_literal(0, (ucharptr)buff, 0);
  }
  if (CB_REF_OR_FIELD_P(x)) {
    f = CB_FIELD(cb_ref(x));
    if (f->flag_any_length) {
      return cb_build_any_intrinsic(cb_list_init(x));
    }
    if (cb_field_variable_size(f) == NULL) {
      sprintf(buff, "%d", cb_field_size(x));
      return cb_build_numeric_literal(0, (ucharptr)buff, 0);
    }
  }
  if (CB_INTRINSIC_P(x)) {
    return cb_build_any_intrinsic(cb_list_init(x));
  }
  temp = cb_build_index(cb_build_filler(), NULL, 0, NULL);
  CB_FIELD(cb_ref(temp))->usage = CB_USAGE_LENGTH;
  CB_FIELD(cb_ref(temp))->count++;
  cb_emit(cb_build_assign(temp, cb_build_length_1(x)));
  return temp;
}

#ifdef I18N_UTF8
/* I18N_UTF8: No wide char support. cb_build_lengths() is not needed. */
#else  /*!I18N_UTF8*/
cb_tree cb_build_lengths(cb_tree x) {
  char buff[64];

  if (x == cb_error_node) {
    return cb_error_node;
  }
  if (CB_REFERENCE_P(x) && cb_ref(x) == cb_error_node) {
    return cb_error_node;
  }
  if ((cb_tree_class(x) == CB_CLASS_NATIONAL) ||
      (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL) ||
      (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL_EDITED)) {
    sprintf(buff, "%d", (cb_field_size(x)) / 2);
  }
  return cb_build_numeric_literal(0, (ucharptr)buff, 0);
}
#endif /*I18N_UTF8*/

cb_tree cb_build_address(cb_tree x) {
  if (x == cb_error_node || (CB_REFERENCE_P(x) && cb_ref(x) == cb_error_node)) {
    return cb_error_node;
  }

  return cb_build_cast_address(x);
}

cb_tree cb_build_ppointer(cb_tree x) {
  struct cb_field *f;

  if (x == cb_error_node || (CB_REFERENCE_P(x) && cb_ref(x) == cb_error_node)) {
    return cb_error_node;
  }

  if (CB_REFERENCE_P(x)) {
    f = cb_field(cb_ref(x));
    f->count++;
  }
  return cb_build_cast_ppointer(x);
}

/* validate program */

static int get_value(cb_tree x) {
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

void cb_validate_program_environment(struct cb_program *prog) {
  cb_tree x;
  cb_tree y;
  cb_tree l;
  cb_tree ls;
  struct cb_alphabet_name *ap;
  unsigned char *data;
  size_t dupls;
  size_t unvals;
  size_t count;
  int lower;
  int upper;
  int size;
  int n;
  int i;
  int lastval;
  int values[256];

  /* Check ALPHABET clauses */
  for (l = current_program->alphabet_name_list; l; l = CB_CHAIN(l)) {
    ap = CB_ALPHABET_NAME(CB_VALUE(l));
    if (ap->type != CB_ALPHABET_CUSTOM) {
      continue;
    }
    ap->low_val_char = 0;
    ap->high_val_char = 255;
    dupls = 0;
    unvals = 0;
    count = 0;
    lastval = 0;
    for (n = 0; n < 256; n++) {
      values[n] = -1;
    }
    for (y = ap->custom_list; y; y = CB_CHAIN(y)) {
      if (count > 255) {
        unvals = 1;
        break;
      }
      x = CB_VALUE(y);
      if (CB_PAIR_P(x)) {
        /* X THRU Y */
        lower = get_value(CB_PAIR_X(x));
        upper = get_value(CB_PAIR_Y(x));
        lastval = upper;
        if (!count) {
          ap->low_val_char = lower;
        }
        if (lower < 0 || lower > 255) {
          unvals = 1;
          continue;
        }
        if (upper < 0 || upper > 255) {
          unvals = 1;
          continue;
        }
        if (lower <= upper) {
          for (i = lower; i <= upper; i++) {
            if (values[i] != -1) {
              dupls = 1;
            }
            values[i] = i;
            count++;
          }
        } else {
          for (i = lower; i >= upper; i--) {
            if (values[i] != -1) {
              dupls = 1;
            }
            values[i] = i;
            count++;
          }
        }
      } else if (CB_LIST_P(x)) {
        /* X ALSO Y ... */
        if (!count) {
          ap->low_val_char = get_value(CB_VALUE(x));
        }
        for (ls = x; ls; ls = CB_CHAIN(ls)) {
          n = get_value(CB_VALUE(ls));
          if (!CB_CHAIN(ls)) {
            lastval = n;
          }
          if (n < 0 || n > 255) {
            unvals = 1;
            continue;
          }
          if (values[n] != -1) {
            dupls = 1;
          }
          values[n] = n;
          count++;
        }
      } else {
        /* literal */
        if (CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
          n = get_value(x);
          lastval = n;
          if (!count) {
            ap->low_val_char = n;
          }
          if (n < 0 || n > 255) {
            unvals = 1;
            continue;
          }
          if (values[n] != -1) {
            dupls = 1;
          }
          values[n] = n;
          count++;
        } else if (CB_LITERAL_P(x)) {
          size = (int)CB_LITERAL(x)->size;
          data = CB_LITERAL(x)->data;
          if (!count) {
            ap->low_val_char = data[0];
          }
          lastval = data[size - 1];
          for (i = 0; i < size; i++) {
            n = data[i];
            if (values[n] != -1) {
              dupls = 1;
            }
            values[n] = n;
            count++;
          }
        } else {
          n = get_value(x);
          lastval = n;
          if (!count) {
            ap->low_val_char = n;
          }
          if (n < 0 || n > 255) {
            unvals = 1;
            continue;
          }
          if (values[n] != -1) {
            dupls = 1;
          }
          values[n] = n;
          count++;
        }
      }
    }
    if (dupls || unvals) {
      if (dupls) {
        cb_error_x(l, _("Duplicate character values in alphabet '%s'"),
                   cb_name(CB_VALUE(l)));
      }
      if (unvals) {
        cb_error_x(l, _("Invalid character values in alphabet '%s'"),
                   cb_name(CB_VALUE(l)));
      }
      ap->low_val_char = 0;
      ap->high_val_char = 255;
      continue;
    }
    /* Calculate HIGH-VALUE */
    /* If all 256 values have been specified, HIGH-VALUE is the last one */
    /* Otherwise if HIGH-VALUE has been specified, find the highest */
    /* value that has not been used */
    if (count == 256) {
      ap->high_val_char = lastval;
    } else if (values[255] != -1) {
      for (n = 254; n >= 0; n--) {
        if (values[n] == -1) {
          ap->high_val_char = n;
          break;
        }
      }
    }
  }
  /* Rest HIGH/LOW-VALUES */
  cb_low = cb_norm_low;
  cb_high = cb_norm_high;
  /* resolve the program collating sequence */
  if (!prog->collating_sequence) {
    return;
  }
  x = cb_ref(prog->collating_sequence);
  /* RXWRXW
          if (x == cb_error_node) {
                  prog->collating_sequence = NULL;
                  return;
          }
  */
  if (!CB_ALPHABET_NAME_P(x)) {
    cb_error_x(prog->collating_sequence, _("'%s' not alphabet name"),
               cb_name(prog->collating_sequence));
    prog->collating_sequence = NULL;
    return;
  }
  if (CB_ALPHABET_NAME(x)->type != CB_ALPHABET_CUSTOM) {
    return;
  }
  if (CB_ALPHABET_NAME(x)->low_val_char) {
    cb_low = cb_build_alphanumeric_literal((ucharptr) "\0", 1);
    CB_LITERAL(cb_low)->data[0] = CB_ALPHABET_NAME(x)->low_val_char;
    CB_LITERAL(cb_low)->all = 1;
  }
  if (CB_ALPHABET_NAME(x)->high_val_char != 255) {
    cb_high = cb_build_alphanumeric_literal((ucharptr) "\0", 1);
    CB_LITERAL(cb_high)->data[0] = CB_ALPHABET_NAME(x)->high_val_char;
    CB_LITERAL(cb_high)->all = 1;
  }
}

void cb_validate_program_data(struct cb_program *prog) {
  cb_tree l;
  cb_tree x;
  cb_tree assign;
  struct cb_field *p;
  struct cb_file *f;
  unsigned char *c;

  for (l = current_program->file_list; l; l = CB_CHAIN(l)) {
    f = CB_FILE(CB_VALUE(l));
    if (!f->finalized) {
      finalize_file(f, NULL);
    }
  }
  /* build undeclared assignment name now */
  if (cb_assign_clause == CB_ASSIGN_MF) {
    for (l = current_program->file_list; l; l = CB_CHAIN(l)) {
      assign = CB_FILE(CB_VALUE(l))->assign;
      if (!assign) {
        continue;
      }
      if (CB_REFERENCE_P(assign)) {
        for (x = current_program->file_list; x; x = CB_CHAIN(x)) {
          if (!strcmp(CB_FILE(CB_VALUE(x))->name,
                      CB_REFERENCE(assign)->word->name)) {
            redefinition_error(assign);
          }
        }
        p = check_level_78(CB_REFERENCE(assign)->word->name);
        if (p) {
          c = (unsigned char *)CB_LITERAL(CB_VALUE(p->values))->data;
          assign = CB_TREE(
              build_literal(CB_CATEGORY_ALPHANUMERIC, c, strlen((char *)c)));
          CB_FILE(CB_VALUE(l))->assign = assign;
        }
      }
      if (CB_REFERENCE_P(assign) && CB_REFERENCE(assign)->word->count == 0) {
        if (cb_warn_implicit_define) {
          cb_warning(_("'%s' will be implicitly defined"), CB_NAME(assign));
        }
        x = cb_build_implicit_field(assign, COB_SMALL_BUFF);
        p = current_program->working_storage;
        CB_FIELD(x)->count++;
        if (p) {
          while (p->sister) {
            p = p->sister;
          }
          p->sister = CB_FIELD(x);
        } else {
          current_program->working_storage = CB_FIELD(x);
        }
      }
      if (CB_REFERENCE_P(assign)) {
        x = cb_ref(assign);
        if (CB_FIELD_P(x) && CB_FIELD(x)->level == 88) {
          cb_error_x(assign, _("ASSIGN data item '%s' invalid"),
                     CB_NAME(assign));
        }
      }
    }
  }

  if (prog->cursor_pos) {
    x = cb_ref(prog->cursor_pos);
    if (x == cb_error_node) {
      prog->cursor_pos = NULL;
    } else if (CB_FIELD(x)->size != 6 && CB_FIELD(x)->size != 4) {
      cb_error_x(prog->cursor_pos,
                 _("'%s' CURSOR is not 4 or 6 characters long"),
                 cb_name(prog->cursor_pos));
      prog->cursor_pos = NULL;
    }
  }
  if (prog->crt_status) {
    x = cb_ref(prog->crt_status);
    if (x == cb_error_node) {
      prog->crt_status = NULL;
    } else if (CB_FIELD(x)->size != 4) {
      cb_error_x(prog->crt_status,
                 _("'%s' CRT STATUS is not 4 characters long"),
                 cb_name(prog->crt_status));
      prog->crt_status = NULL;
    }
  } else {
    l = cb_build_reference("COB-CRT-STATUS");
    p = CB_FIELD(cb_build_field(l));
    p->usage = CB_USAGE_DISPLAY;
    p->pic = CB_PICTURE(cb_build_picture("9(4)"));
    cb_validate_field(p);
    p->flag_no_init = 1;
    /* Do not initialize/bump ref count here
    p->values = cb_list_init (cb_zero);
    p->count++;
    */
    current_program->working_storage =
        cb_field_add(current_program->working_storage, p);
    prog->crt_status = l;
    /* RXWRXW - Maybe better
    prog->crt_status = cb_build_index (cb_build_reference ("COB-CRT-STATUS"),
    cb_zero, 0, NULL);
    */
  }

  /* resolve all references so far */
  for (l = cb_list_reverse(prog->reference_list); l; l = CB_CHAIN(l)) {
    cb_ref(CB_VALUE(l));
  }
  for (l = current_program->file_list; l; l = CB_CHAIN(l)) {
    f = CB_FILE(CB_VALUE(l));
    if (f->record_depending && f->record_depending != cb_error_node) {
      x = f->record_depending;
      if (cb_ref(x) != cb_error_node) {
        /* RXW - This breaks old legacy programs
                                        if (CB_REF_OR_FIELD_P(x)) {
                                                p = cb_field (x);
                                                switch (p->storage) {
                                                case CB_STORAGE_WORKING:
                                                case CB_STORAGE_LOCAL:
                                                case CB_STORAGE_LINKAGE:
                                                        break;
                                                default:
                                                        cb_error (_("RECORD
           DEPENDING item must be in WORKING/LOCAL/LINKAGE section"));
                                                }
                                        } else {
        */
        if (!CB_REFERENCE_P(x) && !CB_FIELD_P(x)) {
          cb_error(_("Invalid RECORD DEPENDING item"));
        }
      }
    }
  }
}

static int check_section_escape(cb_tree x) {
  cb_tree l;
  struct cb_goto *gp;
  struct cb_label *lp;
  int rt = 0;

  if (!x) {
    /* return 0 */
  } else if (CB_TREE_TAG(x) == CB_TAG_STATEMENT) {
    current_statement = CB_STATEMENT(x);
    rt += check_section_escape(current_statement->null_check);
    rt += check_section_escape(current_statement->body);
    rt += check_section_escape(current_statement->handler1);
    rt += check_section_escape(current_statement->handler2);
  } else if (CB_TREE_TAG(x) == CB_TAG_LABEL) {
    lp = CB_LABEL(x);
    if (lp->is_section) {
      current_section = lp;
      current_paragraph = NULL;
    } else {
      current_paragraph = lp;
    }
  } else if (CB_TREE_TAG(x) == CB_TAG_SEARCH) {
    struct cb_search *p = CB_SEARCH(x);
    rt += check_section_escape(p->end_stmt);
    rt += check_section_escape(p->whens);
  } else if (CB_TREE_TAG(x) == CB_TAG_CALL) {
    struct cb_call *p = CB_CALL(x);
    rt += check_section_escape(p->stmt1);
    rt += check_section_escape(p->stmt2);
  } else if (CB_TREE_TAG(x) == CB_TAG_PERFORM) {
    struct cb_perform *p = CB_PERFORM(x);
    if (p->body && !CB_PAIR_P(p->body)) {
      rt += check_section_escape(p->body);
    }
  } else if (CB_TREE_TAG(x) == CB_TAG_GOTO) {
    gp = CB_GOTO(x);
    if (gp->depending) {
      for (l = gp->target; l; l = CB_CHAIN(l)) {
        lp = CB_LABEL(cb_ref(CB_VALUE(l)));
        if (current_section && lp->section != current_section) {
          if (!lp->section) {
            cb_warning_x(CB_TREE(current_statement),
                         _("GO TO escape from SECTION %s"),
                         current_section->name);
          } else {
            cb_warning_x(CB_TREE(current_statement),
                         _("GO TO escape from SECTION %s to %s"),
                         current_section->name, lp->section->name);
          }
          rt++;
        }
      }
    } else if (gp->target == NULL || gp->target == cb_int1) {
      /* goto exit_program */
    } else {
      lp = CB_LABEL(cb_ref(gp->target));
      if (current_section && lp->section != current_section) {
        if (!lp->section) {
          cb_warning_x(CB_TREE(current_statement),
                       _("GO TO escape from SECTION %s"),
                       current_section->name);
        } else {
          cb_warning_x(CB_TREE(current_statement),
                       _("GO TO escape from SECTION %s to %s"),
                       current_section->name, lp->section->name);
        }
        rt++;
      }
    }
  } else if (CB_TREE_TAG(x) == CB_TAG_IF) {
    if (CB_IF(x)->stmt1) {
      rt += check_section_escape(CB_IF(x)->stmt1);
    }
    if (CB_IF(x)->stmt2) {
      rt += check_section_escape(CB_IF(x)->stmt2);
    }
  } else if (CB_TREE_TAG(x) == CB_TAG_LIST) {
    for (; x; x = CB_CHAIN(x)) {
      rt += check_section_escape(CB_VALUE(x));
    }
  }
  return rt;
}

void cb_validate_program_body(struct cb_program *prog) {
  /* resolve all labels */
  cb_tree l;
  cb_tree x;
  cb_tree v;

  for (l = cb_list_reverse(prog->label_list); l; l = CB_CHAIN(l)) {
    x = CB_VALUE(l);
    v = cb_ref(x);
    if (CB_LABEL_P(v)) {
      CB_LABEL(v)->need_begin = 1;
      if (CB_REFERENCE(x)->length) {
        CB_LABEL(v)->need_return = 1;
      }
    } else if (v != cb_error_node) {
      cb_error_x(x, _("'%s' not procedure name"), cb_name(x));
    }
  }

  prog->file_list = cb_list_reverse(prog->file_list);
  prog->exec_list = cb_list_reverse(prog->exec_list);

  if (cb_warn_compat) {
    for (l = prog->exec_list; l; l = CB_CHAIN(l)) {
      check_section_escape(CB_VALUE(l));
    }
  }
}

/*
 * Expressions
 */

static void cb_expr_init(void) {
  static int initialized = 0;

  if (initialized == 0) {
    /* init priority talble */
    expr_prio['x'] = 0;
    expr_prio['^'] = 1;
    expr_prio['*'] = 2;
    expr_prio['/'] = 2;
    expr_prio['+'] = 3;
    expr_prio['-'] = 3;
    expr_prio['='] = 4;
    expr_prio['~'] = 4;
    expr_prio['<'] = 4;
    expr_prio['>'] = 4;
    expr_prio['['] = 4;
    expr_prio[']'] = 4;
    expr_prio['!'] = 5;
    expr_prio['&'] = 6;
    expr_prio['|'] = 7;
    expr_prio[')'] = 8;
    expr_prio['('] = 9;
    expr_prio[0] = 10;
    /* init stack */
    expr_stack_size = START_STACK_SIZE;
    expr_stack = cobc_malloc(sizeof(struct expr_node) * START_STACK_SIZE);
    expr_stack[0].token = 0; /* dummy */
    expr_stack[1].token = 0; /* dummy */
    expr_stack[2].token = 0; /* dummy */
    initialized = 1;
  }

  expr_op = 0;
  expr_lh = NULL;
  expr_index = 3;
}

static int expr_reduce(int token) {
  /* Example:
   * index: -3  -2  -1   0
   * token: 'x' '*' 'x' '+' ...
   */

  int op;

  while (expr_prio[TOKEN(-2)] <= expr_prio[token]) {
    /* Reduce the expression depending on the last operator */
    op = TOKEN(-2);
    switch (op) {
    case 'x':
      return 0;

    case '+':
    case '-':
    case '*':
    case '/':
    case '^':
      /* Arithmetic operators: 'x' op 'x' */
      if (TOKEN(-1) != 'x' || TOKEN(-3) != 'x') {
        return -1;
      }
      TOKEN(-3) = 'x';
      VALUE(-3) = cb_build_binary_op(VALUE(-3), op, VALUE(-1));
      expr_index -= 2;
      break;

    case '!':
      /* Negation: '!' 'x' */
      if (TOKEN(-1) != 'x') {
        return -1;
      }
      /* 'x' '=' 'x' '|' '!' 'x' */
      if (expr_lh) {
        if (CB_TREE_CLASS(VALUE(-1)) != CB_CLASS_BOOLEAN) {
          VALUE(-1) = cb_build_binary_op(expr_lh, expr_op, VALUE(-1));
        }
      }
      TOKEN(-2) = 'x';
      VALUE(-2) = cb_build_negation(VALUE(-1));
      expr_index -= 1;
      break;

    case '&':
    case '|':
      /* Logical AND/OR: 'x' op 'x' */
      if (TOKEN(-1) != 'x' || TOKEN(-3) != 'x') {
        return -1;
      }
      /* 'x' '=' 'x' '|' 'x' */
      if (expr_lh) {
        if (CB_TREE_CLASS(VALUE(-1)) != CB_CLASS_BOOLEAN) {
          VALUE(-1) = cb_build_binary_op(expr_lh, expr_op, VALUE(-1));
        }
        if (CB_TREE_CLASS(VALUE(-3)) != CB_CLASS_BOOLEAN) {
          VALUE(-3) = cb_build_binary_op(expr_lh, expr_op, VALUE(-3));
        }
      }
      /* warning for complex expressions without explicit parentheses
         (i.e., "a OR b AND c" or "a AND b OR c") */
      if (cb_warn_parentheses && op == '|') {
        if ((CB_BINARY_OP_P(VALUE(-3)) && CB_BINARY_OP(VALUE(-3))->op == '&') ||
            (CB_BINARY_OP_P(VALUE(-1)) && CB_BINARY_OP(VALUE(-1))->op == '&')) {
          cb_warning(_("Suggest parentheses around AND within OR"));
        }
      }
      TOKEN(-3) = 'x';
      VALUE(-3) = cb_build_binary_op(VALUE(-3), op, VALUE(-1));
      expr_index -= 2;
      break;

    case '(':
    case ')':
      return 0;

    default:
      /* Relational operators */
      if (TOKEN(-1) != 'x') {
        return -1;
      }
      switch (TOKEN(-3)) {
      case 'x':
        /* Simple condition: 'x' op 'x' */
        if (VALUE(-3) == cb_error_node || VALUE(-1) == cb_error_node) {
          VALUE(-3) = cb_error_node;
        } else {
          expr_lh = VALUE(-3);
          if (CB_REF_OR_FIELD_P(expr_lh)) {
            if (cb_field(expr_lh)->level == 88) {
              VALUE(-3) = cb_error_node;
              return -1;
            }
          }
          if (CB_REF_OR_FIELD_P(VALUE(-1))) {
            if (cb_field(VALUE(-1))->level == 88) {
              VALUE(-3) = cb_error_node;
              return -1;
            }
          }
          expr_op = op;
          TOKEN(-3) = 'x';
          if (CB_TREE_CLASS(VALUE(-1)) != CB_CLASS_BOOLEAN) {
            VALUE(-3) = cb_build_binary_op(expr_lh, op, VALUE(-1));
          } else {
            VALUE(-3) = VALUE(-1);
          }
        }
        expr_index -= 2;
        break;
      case '&':
      case '|':
        /* Complex condition: 'x' '=' 'x' '|' op 'x' */
        if (VALUE(-1) == cb_error_node) {
          VALUE(-2) = cb_error_node;
        } else {
          expr_op = op;
          TOKEN(-2) = 'x';
          if (CB_TREE_CLASS(VALUE(-1)) != CB_CLASS_BOOLEAN) {
            VALUE(-2) = cb_build_binary_op(expr_lh, op, VALUE(-1));
          } else {
            VALUE(-2) = VALUE(-1);
          }
        }
        expr_index -= 1;
        break;
      default:
        return -1;
      }
      break;
    }
  }

  /* handle special case "op OR x AND" */
  if (token == '&' && TOKEN(-2) == '|' &&
      CB_TREE_CLASS(VALUE(-1)) != CB_CLASS_BOOLEAN) {
    TOKEN(-1) = 'x';
    VALUE(-1) = cb_build_binary_op(expr_lh, expr_op, VALUE(-1));
  }

  return 0;
}

static void cb_expr_shift_sign(const int op) {
  int have_not = 0;

  if (TOKEN(-1) == '!') {
    have_not = 1;
    expr_index--;
  }
  expr_reduce('=');
  if (TOKEN(-1) == 'x') {
    VALUE(-1) = cb_build_binary_op(VALUE(-1), op, cb_zero);
    if (have_not) {
      VALUE(-1) = cb_build_negation(VALUE(-1));
    }
  }
}

static void cb_expr_shift_class(const char *name) {
  int have_not = 0;

  if (TOKEN(-1) == '!') {
    have_not = 1;
    expr_index--;
  }
  expr_reduce('=');
  if (TOKEN(-1) == 'x') {
    VALUE(-1) = cb_build_funcall_1(name, VALUE(-1));
    if (have_not) {
      VALUE(-1) = cb_build_negation(VALUE(-1));
    }
  }
}

static void cb_expr_shift_class_method_call(const char *name) {
  int have_not = 0;

  if (TOKEN(-1) == '!') {
    have_not = 1;
    expr_index--;
  }
  expr_reduce('=');
  if (TOKEN(-1) == 'x') {
    VALUE(-1) = cb_build_method_call_1(name, VALUE(-1));
    if (have_not) {
      VALUE(-1) = cb_build_negation(VALUE(-1));
    }
  }
}

static void cb_expr_shift(int token, cb_tree value) {
  switch (token) {
  case 'x':
    /* sign ZERO condition */
    if (value == cb_zero) {
      if (TOKEN(-1) == 'x' || TOKEN(-1) == '!') {
        cb_expr_shift_sign('=');
        return;
      }
    }

    /* class condition */
    if (CB_REFERENCE_P(value) && CB_CLASS_NAME_P(cb_ref(value))) {
      cb_expr_shift_class(CB_CLASS_NAME(cb_ref(value))->cname);
      return;
    }

    /* unary sign */
    if ((TOKEN(-1) == '+' || TOKEN(-1) == '-') && TOKEN(-2) != 'x') {
      if (TOKEN(-1) == '-') {
        value = cb_build_binary_op(cb_zero, '-', value);
      }
      expr_index -= 1;
    }
    break;

  case '(':
    /* 'x' op '(' --> '(' 'x' op */
    switch (TOKEN(-1)) {
    case '=':
    case '~':
    case '<':
    case '>':
    case '[':
    case ']':
      expr_op = TOKEN(-1);
      if (TOKEN(-2) == 'x') {
        expr_lh = VALUE(-2);
      }
    }
    break;

  case ')':
    /* enclose by parentheses */
    expr_reduce(token);
    if (TOKEN(-2) == '(') {
      value = cb_build_parenthesis(VALUE(-1));
      expr_index -= 2;
      cb_expr_shift('x', value);
      return;
    }
    break;

  default:
    /* '<' '|' '=' --> '[' */
    /* '>' '|' '=' --> ']' */
    if (token == '=' && TOKEN(-1) == '|' &&
        (TOKEN(-2) == '<' || TOKEN(-2) == '>')) {
      token = (TOKEN(-2) == '<') ? '[' : ']';
      expr_index -= 2;
    }

    /* '!' '=' --> '~', etc. */
    if (TOKEN(-1) == '!') {
      switch (token) {
      case '=':
        token = '~';
        expr_index--;
        break;
      case '~':
        token = '=';
        expr_index--;
        break;
      case '<':
        token = ']';
        expr_index--;
        break;
      case '>':
        token = '[';
        expr_index--;
        break;
      case '[':
        token = '>';
        expr_index--;
        break;
      case ']':
        token = '<';
        expr_index--;
        break;
      }
    }
    break;
  }

  /* reduce */
  expr_reduce(token);

  /* allocate sufficient stack memory */
  if (expr_index >= expr_stack_size) {
    expr_stack_size *= 2;
    expr_stack =
        cobc_realloc(expr_stack, sizeof(struct expr_node) * expr_stack_size);
  }

  /* put on the stack */
  TOKEN(0) = token;
  VALUE(0) = value;
  expr_index++;
}

static void expr_expand(cb_tree *x) {
  struct cb_binary_op *p;

start:
  /* remove parenthesis */
  if (CB_BINARY_OP_P(*x)) {
    p = CB_BINARY_OP(*x);
    if (p->op == '@') {
      *x = p->x;
      goto start;
    }
    expr_expand(&p->x);
    if (p->y) {
      expr_expand(&p->y);
    }
  }
}

static cb_tree cb_expr_finish(void) {
  expr_reduce(0); /* reduce all */

  if (expr_index != 4) {
    cb_error(_("Invalid expression"));
    return cb_error_node;
  }

  if (!expr_stack[3].value) {
    cb_error(_("Invalid expression"));
    return cb_error_node;
  }
  expr_expand(&expr_stack[3].value);
  if (expr_stack[3].token != 'x') {
    cb_error(_("Invalid expression"));
    return cb_error_node;
  }
  return expr_stack[3].value;
}

static int check_div_mul_order(cb_tree n) {
  int flg = 0;

  if (CB_BINARY_OP(n)->x && CB_BINARY_OP_P(CB_BINARY_OP(n)->x)) {
    flg = check_div_mul_order(CB_BINARY_OP(n)->x);
  }
  if (CB_BINARY_OP(n)->y && CB_BINARY_OP_P(CB_BINARY_OP(n)->y)) {
    flg = check_div_mul_order(CB_BINARY_OP(n)->y);
  }
  if (CB_BINARY_OP(n)->op == '/') {
    flg = 1;
  } else if (CB_BINARY_OP(n)->op == '*') {
    if (flg) {
      cb_warning(_("MUL operation after DIV can cause the precision issue."));
      flg = 0;
    }
  } else {
    flg = 0;
  }
  return flg;
}

static cb_tree cb_validate_expr(cb_tree x) {
  if (cb_warn_compat) {
    if (CB_BINARY_OP_P(x)) {
      check_div_mul_order(x);
    }
  }
  return x;
}

cb_tree cb_build_expr(cb_tree list) {
  cb_tree l;
  /* RXW
          cb_tree x;
  */
  int op;

  cb_expr_init();

  for (l = list; l; l = CB_CHAIN(l)) {
    op = CB_PURPOSE_INT(l);
    switch (op) {
    case '9': /* NUMERIC */
      cb_expr_shift_class_method_call("isNumeric");
      break;
    case 'A': /* ALPHABETIC */
      cb_expr_shift_class_method_call("isAlpha");
      break;
    case 'L': /* ALPHABETIC_LOWER */
      cb_expr_shift_class_method_call("isLower");
      break;
    case 'U': /* ALPHABETIC_UPPER */
      cb_expr_shift_class_method_call("isUpper");
      break;
    case 'P': /* POSITIVE */
      cb_expr_shift_sign('>');
      break;
    case 'N': /* NEGATIVE */
      cb_expr_shift_sign('<');
      break;
    case 'O': /* OMITTED */
      current_statement->null_check = NULL;
      cb_expr_shift_class_method_call("isOmitted");
      break;
      /* RXW
                      case 'x':
                              if (CB_VALUE (l) && CB_REFERENCE_P (CB_VALUE (l)))
         { x = CB_CHAIN (l); if (x && cb_field (CB_VALUE (l))->level == 88) {
                                              switch (CB_PURPOSE_INT (x)) {
                                              case '&':
                                              case '|':
                                              case '(':
                                              case ')':
                                                      break;
                                              default:
                                                      cb_error (_("Invalid
         condition")); break;
                                              }
                                      }
                              }
                              cb_expr_shift (op, CB_VALUE (l));
                              break;
      */
    default:
      cb_expr_shift(op, CB_VALUE(l));
      break;
    }
  }

  return cb_validate_expr(cb_expr_finish());
}

/*
 * Numerical operation
 */

static cb_tree build_store_option(cb_tree x, cb_tree round_opt) {
  int opt = 0;

  if (round_opt == cb_int1) {
    opt |= COB_STORE_ROUND;
  }

  switch (CB_FIELD(cb_ref(x))->usage) {
  case CB_USAGE_COMP_5:
  case CB_USAGE_COMP_X:
    if (current_statement->handler1) {
      opt |= COB_STORE_KEEP_ON_OVERFLOW;
    }
    break;
  default:
    if (!cb_binary_truncate) {
      if (current_statement->handler1) {
        opt |= COB_STORE_KEEP_ON_OVERFLOW;
      }
      break;
    }

    /* RXW Fixme - It seems as though we have NEVER implemented TRUNC,
       Code has always been wrong. Hmm. The following statement would
       activate what was intended but ...
       What should we do here?
                    if (current_statement->handler1) {
    */
    if (current_statement->handler_id) {
      opt |= COB_STORE_KEEP_ON_OVERFLOW;
    } else if (cb_binary_truncate) {
      opt |= COB_STORE_TRUNC_ON_OVERFLOW;
    }
    break;
  }

  return cb_int(opt);
}

static cb_tree decimal_alloc(void) {
  cb_tree x;

  x = cb_build_decimal(current_program->decimal_index);
  current_program->decimal_index++;
  if (current_program->decimal_index > current_program->decimal_index_max) {
    current_program->decimal_index_max = current_program->decimal_index;
  }
  return x;
}

static void decimal_free(void) { current_program->decimal_index--; }

static int decimal_compute(const int op, cb_tree x, cb_tree y) {
  const char *func;

  switch (op) {
  case '+':
    func = "add";
    break;
  case '-':
    func = "sub";
    break;
  case '*':
    func = "mul";
    break;
  case '/':
    func = "div";
    break;
  case '^':
    func = "pow";
    break;
  default:
    fprintf(stderr, "Unexpected operation %d\n", op);
    return 1; /* don't ABORT (), continue parsing */
  }
  dpush(cb_build_method_call_2(func, x, y));
  return 0;
}

static int decimal_expand(cb_tree d, cb_tree x) {
  struct cb_literal *l;
  struct cb_field *f;
  struct cb_binary_op *p;
  cb_tree t;
  int rt = 0;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CONST:
    if (x == cb_zero) {
      dpush(cb_build_method_call_2("set", d, cb_int0));
      current_program->gen_decset = 1;
    } else {
      fprintf(stderr, "Unexpected constant expansion\n");
      rt = 1; /* don't ABORT (), continue parsing */
    }
    break;
  case CB_TAG_LITERAL:
    /* set d, N */
    l = CB_LITERAL(x);
    if (l->size < 10 && l->scale == 0) {
      dpush(cb_build_method_call_2("set", d, cb_build_cast_integer(x)));
      current_program->gen_decset = 1;
    } else {
      dpush(cb_build_method_call_2("setField", d, x));
    }
    break;
  case CB_TAG_REFERENCE:
    /* set d, X */
    f = cb_field(x);
    /* check numeric */
    if (CB_EXCEPTION_ENABLE(COB_EC_DATA_INCOMPATIBLE)) {
      if (f->usage == CB_USAGE_DISPLAY || f->usage == CB_USAGE_PACKED) {
        dpush(cb_build_method_call_2("checkNumeric", x,
                                     cb_build_string0((ucharptr)(f->name))));
      }
    }

    if (cb_fits_int(x)) {
      if (f->pic->have_sign) {
        dpush(cb_build_method_call_2("set", d, cb_build_cast_integer(x)));
        current_program->gen_decset = 1;
      } else {
        dpush(cb_build_method_call_2("set", d, cb_build_cast_integer(x)));
        current_program->gen_udecset = 1;
      }
    } else {
      dpush(cb_build_method_call_2("setField", d, x));
    }
    break;
  case CB_TAG_BINARY_OP:
    /* set d, X
     * set t, Y
     * OP d, t */
    p = CB_BINARY_OP(x);
    t = decimal_alloc();
    if (decimal_expand(d, p->x) || decimal_expand(t, p->y) ||
        decimal_compute(p->op, d, t)) {
      rt = 1;
    }
    decimal_free();
    break;
  case CB_TAG_INTRINSIC:
    dpush(cb_build_method_call_2("setField", d, x));
    break;
  default:
    fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
    rt = 1; /* don't ABORT (), continue parsing */
  }
  return rt;
}

static void decimal_assign(cb_tree x, cb_tree d, cb_tree round_opt) {
  dpush(cb_build_method_call_3("getField", d, x,
                               build_store_option(x, round_opt)));
}

static cb_tree build_decimal_assign(cb_tree vars, int op, cb_tree val) {
  cb_tree l;
  cb_tree t;
  cb_tree s1 = NULL;
  cb_tree d;

  d = decimal_alloc();

  /* set d, VAL */
  if (decimal_expand(d, val)) {
    s1 = NULL;
  } else if (op == 0) {
    for (l = vars; l; l = CB_CHAIN(l)) {
      /* set VAR, d */
      decimal_assign(CB_VALUE(l), d, CB_PURPOSE(l));
      s1 = cb_list_add(s1, cb_list_reverse(decimal_stack));
      decimal_stack = NULL;
    }
  } else {
    t = decimal_alloc();
    for (l = vars; l; l = CB_CHAIN(l)) {
      /* set t, VAR
       * OP t, d
       * set VAR, t
       */
      if (decimal_expand(t, CB_VALUE(l)) || decimal_compute(op, t, d)) {
        s1 = NULL;
      } else {
        decimal_assign(CB_VALUE(l), t, CB_PURPOSE(l));
        s1 = cb_list_add(s1, cb_list_reverse(decimal_stack));
      }
      decimal_stack = NULL;
    }
    decimal_free();
  }

  decimal_free();
  return s1;
}

void cb_emit_arithmetic(cb_tree vars, int op, cb_tree val) {
  cb_tree l;
  cb_tree t;
  struct cb_field *f;

  val = cb_check_numeric_value(val);
  if (op) {
    cb_list_map(cb_check_numeric_name, vars);
  } else {
    cb_list_map(cb_check_numeric_edited_name, vars);
  }

  if (cb_validate_one(val)) {
    return;
  }
  if (cb_validate_list(vars)) {
    return;
  }

  if (!CB_BINARY_OP_P(val)) {
    if (op == '+' || op == '-') {
      if (CB_EXCEPTION_ENABLE(COB_EC_DATA_INCOMPATIBLE) &&
          (CB_REF_OR_FIELD_P(val))) {
        f = cb_field(val);
        if (f->usage == CB_USAGE_DISPLAY || f->usage == CB_USAGE_PACKED) {
          cb_emit(cb_build_method_call_2(
              "checkNumeric", val, cb_build_string0((ucharptr)(f->name))));
        }
      }
      for (l = vars; l; l = CB_CHAIN(l)) {
        if (CB_EXCEPTION_ENABLE(COB_EC_DATA_INCOMPATIBLE) &&
            (CB_REF_OR_FIELD_P(CB_VALUE(l)))) {
          f = cb_field(CB_VALUE(l));
          if (f->usage == CB_USAGE_DISPLAY || f->usage == CB_USAGE_PACKED) {
            cb_emit(
                cb_build_method_call_2("checkNumeric", CB_VALUE(l),
                                       cb_build_string0((ucharptr)(f->name))));
          }
        }
        if (op == '+') {
          CB_VALUE(l) = cb_build_add(CB_VALUE(l), val, CB_PURPOSE(l));
        } else {
          CB_VALUE(l) = cb_build_sub(CB_VALUE(l), val, CB_PURPOSE(l));
        }
      }
      cb_emit_list(vars);
      return;
    }
  }

  t = build_decimal_assign(vars, op, val);
  if (t) {
    cb_emit(t);
  }
}

/*
 * Condition
 */

static cb_tree build_cond_88(cb_tree x) {
  struct cb_field *f;
  cb_tree l;
  cb_tree t;
  cb_tree c1 = NULL;
  cb_tree c2;

  f = cb_field(x);
  /* refer to parent's data storage */
  x = cb_build_field_reference(f->parent, x);
  f->parent->count++;

  /* build condition */
  for (l = f->values; l; l = CB_CHAIN(l)) {
    t = CB_VALUE(l);
    if (CB_PAIR_P(t)) {
      /* VALUE THRU VALUE */
      c2 = cb_build_binary_op(cb_build_binary_op(CB_PAIR_X(t), '[', x), '&',
                              cb_build_binary_op(x, '[', CB_PAIR_Y(t)));
    } else {
      /* VALUE */
      c2 = cb_build_binary_op(x, '=', t);
    }
    if (c1 == NULL) {
      c1 = c2;
    } else {
      c1 = cb_build_binary_op(c1, '|', c2);
    }
  }
  return c1;
}

static cb_tree cb_build_optim_cond(struct cb_binary_op *p) {
  struct cb_field *f;
  struct cb_field *fy;
  const char *s;
  size_t n;

  if (CB_REF_OR_FIELD_P(p->y)) {
    fy = cb_field(p->y);
    if (!fy->pic->have_sign &&
        (fy->usage == CB_USAGE_BINARY || fy->usage == CB_USAGE_COMP_5 ||
         fy->usage == CB_USAGE_COMP_X)) {
      return cb_build_method_call_2("cmpUint", p->x,
                                    cb_build_cast_integer(p->y));
    }
  }
  if (CB_REF_OR_FIELD_P(p->x)) {
    f = cb_field(p->x);
    if (!f->pic->scale && f->usage == CB_USAGE_PACKED) {
      if (f->pic->digits < 10) {
        return cb_build_method_call_2("cmpInt", p->x,
                                      cb_build_cast_integer(p->y));
      } else {
        return cb_build_method_call_2("cmpInt", p->x,
                                      cb_build_cast_integer(p->y));
      }
    }
    if (!f->pic->scale && f->usage == CB_USAGE_DISPLAY &&
        !f->flag_sign_leading && !f->flag_sign_separate) {
      if (cb_fits_int(p->x)) {
        if (!f->pic->have_sign) {
          return cb_build_method_call_3(
              "cmpNumdisp", cb_build_cast_address(p->x), cb_int(f->size),
              cb_build_cast_integer(p->y));
        } else {
          return cb_build_method_call_3(
              "cmpSignNumdisp", cb_build_cast_address(p->x), cb_int(f->size),
              cb_build_cast_integer(p->y));
        }
      } else if (cb_fits_long_long(p->x)) {
        if (!f->pic->have_sign) {
          return cb_build_method_call_3(
              "cmpLongNumdisp", cb_build_cast_address(p->x), cb_int(f->size),
              cb_build_cast_integer(p->y));
        } else {
          return cb_build_method_call_3(
              "cmpLongSignNumdisp", cb_build_cast_address(p->x),
              cb_int(f->size), cb_build_cast_integer(p->y));
        }
      }
    }
    if (!f->pic->scale &&
        (f->usage == CB_USAGE_BINARY || f->usage == CB_USAGE_COMP_5 ||
         f->usage == CB_USAGE_INDEX || f->usage == CB_USAGE_COMP_X)) {
      n = (f->size - 1) + (8 * (f->pic->have_sign ? 1 : 0)) +
          (16 * (f->flag_binary_swap ? 1 : 0));
#if defined(COB_NON_ALIGNED) && !defined(_MSC_VER)
      switch (f->size) {
      case 2:
#ifdef COB_SHORT_BORK
        s = bin_compare_funcs[n];
        break;
#endif
      case 4:
      case 8:
        if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
            (f->offset % f->size) == 0) {
          s = align_bin_compare_funcs[n];
        } else {
          s = bin_compare_funcs[n];
        }
        break;
      default:
        s = bin_compare_funcs[n];
        break;
      }
#else
      s = bin_compare_funcs[n];
#endif
      if (s) {
        return cb_build_method_call_2(s, cb_build_cast_address(p->x),
                                      cb_build_cast_integer(p->y));
      }
    }
  }
  return cb_build_method_call_2("cmpInt", p->x, cb_build_cast_integer(p->y));
}

static int cb_chk_num_cond(cb_tree x, cb_tree y) {
  struct cb_field *fx;
  struct cb_field *fy;

  if (!CB_REFERENCE_P(x) && !CB_FIELD_P(x)) {
    return 0;
  }
  if (!CB_REFERENCE_P(y) && !CB_FIELD_P(y)) {
    return 0;
  }
  if (CB_TREE_CATEGORY(x) != CB_CATEGORY_NUMERIC) {
    return 0;
  }
  if (CB_TREE_CATEGORY(y) != CB_CATEGORY_NUMERIC) {
    return 0;
  }
  if (CB_TREE_CLASS(x) != CB_CLASS_NUMERIC) {
    return 0;
  }
  if (CB_TREE_CLASS(y) != CB_CLASS_NUMERIC) {
    return 0;
  }
  fx = cb_field(x);
  fy = cb_field(y);
  if (fx->usage != CB_USAGE_DISPLAY) {
    return 0;
  }
  if (fy->usage != CB_USAGE_DISPLAY) {
    return 0;
  }
  if (fx->pic->have_sign || fy->pic->have_sign) {
    return 0;
  }
  if (fx->size != fy->size) {
    return 0;
  }
  if (fx->pic->scale != fy->pic->scale) {
    return 0;
  }
  return 1;
}

static int cb_chk_alpha_cond(cb_tree x) {
  if (current_program->alphabet_name_list) {
    return 0;
  }
  if (CB_LITERAL_P(x)) {
    return 1;
  }
  if (!CB_REFERENCE_P(x) && !CB_FIELD_P(x)) {
    return 0;
  }
  if (CB_TREE_CATEGORY(x) != CB_CATEGORY_ALPHANUMERIC &&
      CB_TREE_CATEGORY(x) != CB_CATEGORY_ALPHABETIC) {
    return 0;
  }
  if (cb_field_variable_size(cb_field(x))) {
    return 0;
  }
  if (cb_field_size(x) < 0) {
    return 0;
  }
  return 1;
}

static int national_kanji_comparison(cb_tree x, cb_tree y) {
  if (((CB_LITERAL_P(x) && (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL)) ||
       ((CB_REF_OR_FIELD_P(x)) &&
        (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL ||
         CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL_EDITED))) &&
      ((CB_TREE_CATEGORY(y) == CB_CATEGORY_ALPHABETIC ||
        CB_TREE_CATEGORY(y) == CB_CATEGORY_NUMERIC) &&
       y != cb_zero && y != cb_space)) {
    return 1;
  } else {
    return 0;
  }
}

cb_tree cb_build_cond(cb_tree x) {
  int size1;
  int size2;
  struct cb_field *f;
  struct cb_binary_op *p;
  cb_tree d1;
  cb_tree d2;
  cb_tree err = NULL;

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_CONST:
  case CB_TAG_FUNCALL:
    return x;
  case CB_TAG_REFERENCE:
    if (!CB_FIELD_P(cb_ref(x))) {
      return cb_build_cond(cb_ref(x));
    }

    f = cb_field(x);

    /* level 88 condition */
    if (f->level == 88) {
      /* We need to build a 88 condition at every occurrence
         instead of once at the beginning because a 88 item
         may be subscripted (i.e., it is not a constant tree). */
      return cb_build_cond(build_cond_88(x));
    }

    cb_error_x(x, _("Invalid expression"));
    return cb_error_node;
  case CB_TAG_BINARY_OP:
    p = CB_BINARY_OP(x);
    switch (p->op) {
    case '!':
      return cb_build_negation(cb_build_cond(p->x));
    case '&':
    case '|':
      return cb_build_binary_op(cb_build_cond(p->x), p->op,
                                cb_build_cond(p->y));
    default:
      if (CB_INDEX_P(p->x) || CB_INDEX_P(p->y) ||
          CB_TREE_CLASS(p->x) == CB_CLASS_POINTER ||
          CB_TREE_CLASS(p->y) == CB_CLASS_POINTER) {
        x = cb_build_binary_op(p->x, '-', p->y);
      } else if (CB_BINARY_OP_P(p->x) || CB_BINARY_OP_P(p->y)) {
        /* decimal comparison */
        d1 = decimal_alloc();
        d2 = decimal_alloc();

        if (decimal_expand(d1, p->x)) {
          err = p->x;
        } else if (decimal_expand(d2, p->y)) {
          err = p->y;
        }
        if (err) {
          decimal_free();
          decimal_free();
          decimal_stack = NULL;
          cb_error_x(err, _("Invalid expression"));
          return cb_error_node;
        }
        dpush(cb_build_method_call_2("compareTo", d1, d2));
        decimal_free();
        decimal_free();
        x = cb_list_reverse(decimal_stack);
        decimal_stack = NULL;
      } else {
        /* field comparison */
        if (national_kanji_comparison(p->x, p->y) ||
            national_kanji_comparison(p->y, p->x)) {
          cb_error_x(x, _("Invalid expression test"));
        }

        if (CB_EXCEPTION_ENABLE(COB_EC_BOUND_SUBSCRIPT)) {
          /* optim'ed funcs below don't check subscript boundary */
          x = cb_build_method_call_2("compareTo", p->x, p->y);
          break;
        }

        if (cb_chk_num_cond(p->x, p->y)) {
          size1 = cb_field_size(p->x);
          x = cb_build_method_call_3("memcmp", cb_build_cast_address(p->x),
                                     cb_build_cast_address(p->y),
                                     cb_int(size1));
          break;
        }
        if (CB_TREE_CLASS(p->x) == CB_CLASS_NUMERIC &&
            CB_TREE_CLASS(p->y) == CB_CLASS_NUMERIC && cb_fits_int(p->y)) {
          x = cb_build_optim_cond(p);
          break;
        }
        if ((CB_REF_OR_FIELD_P(p->x)) &&
            (CB_TREE_CATEGORY(p->x) == CB_CATEGORY_ALPHANUMERIC ||
             CB_TREE_CATEGORY(p->x) == CB_CATEGORY_ALPHABETIC) &&
            (cb_field_size(p->x) == 1) &&
            (!current_program->alphabet_name_list) &&
            (p->y == cb_space || p->y == cb_low || p->y == cb_high ||
             p->y == cb_zero)) {
          x = cb_build_funcall_2("$G", p->x, p->y);
          break;
        }
        if (cb_chk_alpha_cond(p->x) && cb_chk_alpha_cond(p->y)) {
          size1 = cb_field_size(p->x);
          size2 = cb_field_size(p->y);
        } else {
          size1 = 0;
          size2 = 0;
        }
        if (size1 == 1 && size2 == 1) {
          x = cb_build_funcall_2("$G", p->x, p->y);
        } else if (size1 != 0 && size1 == size2) {
          x = cb_build_method_call_3("memcmp", cb_build_cast_address(p->x),
                                     cb_build_cast_address(p->y),
                                     cb_int(size1));
        } else {
          if (CB_TREE_CLASS(p->x) == CB_CLASS_NUMERIC && p->y == cb_zero) {
            x = cb_build_optim_cond(p);
          } else {
            x = cb_build_method_call_2("compareTo", p->x, p->y);
          }
        }
      }
    }
    return cb_build_binary_op(x, p->op, p->y);
  default:
    break;
  }
  cb_error_x(x, _("Invalid expression"));
  return cb_error_node;
}

/*
 * ADD/SUBTRACT CORRESPONDING
 */

static cb_tree cb_build_optim_add(cb_tree v, cb_tree n) {
  size_t z;
  const char *s;
  struct cb_field *f;

  if (CB_REF_OR_FIELD_P(v)) {
    f = cb_field(v);
    if (!f->pic->scale &&
        (f->usage == CB_USAGE_BINARY || f->usage == CB_USAGE_COMP_5 ||
         f->usage == CB_USAGE_COMP_X)) {
      z = (f->size - 1) + (8 * (f->pic->have_sign ? 1 : 0)) +
          (16 * (f->flag_binary_swap ? 1 : 0));
#if defined(COB_NON_ALIGNED) && !defined(_MSC_VER)
      switch (f->size) {
      case 2:
#ifdef COB_SHORT_BORK
        s = bin_add_funcs[z];
        break;
#endif
      case 4:
      case 8:
        if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
            (f->offset % f->size) == 0) {
          s = align_bin_add_funcs[z];
        } else {
          s = bin_add_funcs[z];
        }
        break;
      default:
        s = bin_add_funcs[z];
        break;
      }
#else
      if (f->usage == CB_USAGE_COMP_5) {
        switch (f->size) {
        case 1:
        case 2:
        case 4:
        case 8:
          return cb_build_assign(v, cb_build_binary_op(v, '+', n));
        }
      }
      s = bin_add_funcs[z];
#endif
      if (s) {
        return cb_build_method_call_2(s, cb_build_cast_address(v),
                                      cb_build_cast_integer(n));
      }
    } else if (!f->pic->scale && f->usage == CB_USAGE_PACKED &&
               f->pic->digits < 10) {
      return cb_build_method_call_2("addPackedInt", v,
                                    cb_build_cast_integer(n));
    }
  }
  return cb_build_method_call_2("addInt", v, cb_build_cast_integer(n));
}

static cb_tree cb_build_optim_sub(cb_tree v, cb_tree n) {
  size_t z;
  const char *s;
  struct cb_field *f;

  if (CB_REF_OR_FIELD_P(v)) {
    f = cb_field(v);
    if (!f->pic->scale &&
        (f->usage == CB_USAGE_BINARY || f->usage == CB_USAGE_COMP_5 ||
         f->usage == CB_USAGE_COMP_X)) {
      z = (f->size - 1) + (8 * (f->pic->have_sign ? 1 : 0)) +
          (16 * (f->flag_binary_swap ? 1 : 0));
#if defined(COB_NON_ALIGNED) && !defined(_MSC_VER)
      switch (f->size) {
      case 2:
#ifdef COB_SHORT_BORK
        s = bin_sub_funcs[z];
        break;
#endif
      case 4:
      case 8:
        if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
            (f->offset % f->size) == 0) {
          s = align_bin_sub_funcs[z];
        } else {
          s = bin_sub_funcs[z];
        }
        break;
      default:
        s = bin_sub_funcs[z];
        break;
      }
#else
      if (f->usage == CB_USAGE_COMP_5) {
        switch (f->size) {
        case 1:
        case 2:
        case 4:
        case 8:
          return cb_build_assign(v, cb_build_binary_op(v, '-', n));
        }
      }
      s = bin_sub_funcs[z];
#endif
      if (s) {
        return cb_build_method_call_2(s, cb_build_cast_address(v),
                                      cb_build_cast_integer(n));
      }
    }
  }
  return cb_build_funcall_2("cob_sub_int", v, cb_build_cast_integer(n));
}

cb_tree cb_build_add(cb_tree v, cb_tree n, cb_tree round_opt) {
  cb_tree opt;
  struct cb_field *f;

#ifdef COB_NON_ALIGNED
  if (CB_INDEX_P(v)) {
    return cb_build_move(cb_build_binary_op(v, '+', n), v);
  }
  if (CB_TREE_CLASS(v) == CB_CLASS_POINTER) {
    current_program->gen_ptrmanip = 1;
    return cb_build_funcall_3("cob_pointer_manip", v, n, cb_int0);
  }
#else
  if (CB_INDEX_P(v) || CB_TREE_CLASS(v) == CB_CLASS_POINTER) {
    return cb_build_move(cb_build_binary_op(v, '+', n), v);
  }
#endif

  if (CB_REF_OR_FIELD_P(v)) {
    f = cb_field(v);
    f->count++;
  }
  if (CB_REF_OR_FIELD_P(n)) {
    f = cb_field(n);
    f->count++;
  }
  if (round_opt == cb_high) {
    if (cb_fits_int(n)) {
      return cb_build_optim_add(v, n);
    } else {
      return cb_build_method_call_3("add", v, n, cb_int0);
    }
  }
  opt = build_store_option(v, round_opt);
  if (opt == cb_int0 && cb_fits_int(n)) {
    return cb_build_optim_add(v, n);
  }
  return cb_build_method_call_3("add", v, n, opt);
}

cb_tree cb_build_sub(cb_tree v, cb_tree n, cb_tree round_opt) {
  cb_tree opt;
  struct cb_field *f;

#ifdef COB_NON_ALIGNED
  if (CB_INDEX_P(v)) {
    return cb_build_move(cb_build_binary_op(v, '-', n), v);
  }
  if (CB_TREE_CLASS(v) == CB_CLASS_POINTER) {
    current_program->gen_ptrmanip = 1;
    return cb_build_funcall_3("cob_pointer_manip", v, n, cb_int1);
  }
#else
  if (CB_INDEX_P(v) || CB_TREE_CLASS(v) == CB_CLASS_POINTER) {
    return cb_build_move(cb_build_binary_op(v, '-', n), v);
  }
#endif

  if (CB_REF_OR_FIELD_P(v)) {
    f = cb_field(v);
    f->count++;
  }
  if (CB_REF_OR_FIELD_P(n)) {
    f = cb_field(n);
    f->count++;
  }
  opt = build_store_option(v, round_opt);
  if (opt == cb_int0 && cb_fits_int(n)) {
    return cb_build_optim_sub(v, n);
  }
  return cb_build_method_call_3("sub", v, n, opt);
}

static void emit_corresponding(cb_tree (*func)(cb_tree f1, cb_tree f2,
                                               cb_tree f3),
                               cb_tree x1, cb_tree x2, cb_tree opt) {
  struct cb_field *f1, *f2;
  cb_tree t1;
  cb_tree t2;

  for (f1 = cb_field(x1)->children; f1; f1 = f1->sister) {
    if (!f1->redefines && !f1->flag_occurs) {
      for (f2 = cb_field(x2)->children; f2; f2 = f2->sister) {
        if (!f2->redefines && !f2->flag_occurs) {
          if (strcmp(f1->name, f2->name) == 0) {
            t1 = cb_build_field_reference(f1, x1);
            t2 = cb_build_field_reference(f2, x2);
            if (f1->children && f2->children) {
              emit_corresponding(func, t1, t2, opt);
            } else {
              cb_emit(func(t1, t2, opt));
            }
          }
        }
      }
    }
  }
}

void cb_emit_corresponding(cb_tree (*func)(cb_tree f1, cb_tree f2, cb_tree f3),
                           cb_tree x1, cb_tree x2, cb_tree opt) {
  x1 = cb_check_group_name(x1);
  x2 = cb_check_group_name(x2);

  if (cb_validate_one(x1)) {
    return;
  }
  if (cb_validate_one(x2)) {
    return;
  }

  emit_corresponding(func, x1, x2, opt);
}

static void emit_move_corresponding(cb_tree x1, cb_tree x2) {
  struct cb_field *f1, *f2;
  cb_tree t1;
  cb_tree t2;

  for (f1 = cb_field(x1)->children; f1; f1 = f1->sister) {
    if (!f1->redefines && !f1->flag_occurs) {
      for (f2 = cb_field(x2)->children; f2; f2 = f2->sister) {
        if (!f2->redefines && !f2->flag_occurs) {
          if (strcmp(f1->name, f2->name) == 0) {
            t1 = cb_build_field_reference(f1, x1);
            t2 = cb_build_field_reference(f2, x2);
            if (f1->children && f2->children) {
              emit_move_corresponding(t1, t2);
            } else {
              cb_emit(cb_build_move(t1, t2));
            }
          }
        }
      }
    }
  }
}

void cb_emit_move_corresponding(cb_tree x1, cb_tree x2) {
  cb_tree l;
  cb_tree v;

  x1 = cb_check_group_name(x1);
  if (cb_validate_one(x1)) {
    return;
  }
  for (l = x2; l; l = CB_CHAIN(l)) {
    v = CB_VALUE(l);
    v = cb_check_group_name(v);
    if (cb_validate_one(v)) {
      return;
    }
    emit_move_corresponding(x1, v);
  }
}

static void output_screen_from(struct cb_field *p, const size_t sisters) {
  int type;

  if (sisters && p->sister) {
    output_screen_from(p->sister, 1);
  }
  if (p->children) {
    output_screen_from(p->children, 1);
  }

  type = (p->children ? COB_SCREEN_TYPE_GROUP
                      : p->values ? COB_SCREEN_TYPE_VALUE
                                  : (p->size > 0) ? COB_SCREEN_TYPE_FIELD
                                                  : COB_SCREEN_TYPE_ATTRIBUTE);
  if (type == COB_SCREEN_TYPE_FIELD && p->screen_from) {
    cb_emit(cb_build_method_call_2("moveFrom", p->screen_from, CB_TREE(p)));
  }
}

static void output_screen_to(struct cb_field *p, const size_t sisters) {
  int type;

  if (sisters && p->sister) {
    output_screen_to(p->sister, 1);
  }
  if (p->children) {
    output_screen_to(p->children, 1);
  }

  type = (p->children ? COB_SCREEN_TYPE_GROUP
                      : p->values ? COB_SCREEN_TYPE_VALUE
                                  : (p->size > 0) ? COB_SCREEN_TYPE_FIELD
                                                  : COB_SCREEN_TYPE_ATTRIBUTE);
  if (type == COB_SCREEN_TYPE_FIELD && p->screen_to) {
    cb_emit(cb_build_method_call_2("moveFrom", CB_TREE(p), p->screen_to));
  }
}

/*
 * ACCEPT statement
 */

void cb_emit_accept(cb_tree var, cb_tree pos, cb_tree fgc, cb_tree bgc,
                    cb_tree scroll, int dispattrs) {
  cb_tree line;
  cb_tree column;

  if (cb_validate_one(var)) {
    return;
  }
  if (cb_validate_one(pos)) {
    return;
  }
  if (cb_validate_one(fgc)) {
    return;
  }
  if (cb_validate_one(bgc)) {
    return;
  }
  if (cb_validate_one(scroll)) {
    return;
  }
  if (current_program->flag_screen) {
    /* Bump ref count to force CRT STATUS field generation */
    if (current_program->crt_status != NULL) {
      cb_field(current_program->crt_status)->count++;
    }
    if ((CB_REF_OR_FIELD_P(var)) &&
        CB_FIELD(cb_ref(var))->storage == CB_STORAGE_SCREEN) {
      output_screen_from(CB_FIELD(cb_ref(var)), 0);
      gen_screen_ptr = 1;
      if (pos) {
        if (CB_PAIR_P(pos)) {
          line = CB_PAIR_X(pos);
          column = CB_PAIR_Y(pos);
          cb_emit(cb_build_funcall_3("cob_screen_accept", var, line, column));
        } else {
          cb_emit(cb_build_funcall_3("cob_screen_accept", var, pos, NULL));
        }
      } else {
        cb_emit(cb_build_funcall_3("cob_screen_accept", var, NULL, NULL));
      }
      gen_screen_ptr = 0;
      output_screen_to(CB_FIELD(cb_ref(var)), 0);
    } else {
      if (pos || fgc || bgc) {
        if (!pos) {
          cb_emit(cb_build_funcall_7("cob_field_accept", var, NULL, NULL, fgc,
                                     bgc, scroll, cb_int(dispattrs)));
        } else if (CB_PAIR_P(pos)) {
          line = CB_PAIR_X(pos);
          column = CB_PAIR_Y(pos);
          cb_emit(cb_build_funcall_7("cob_field_accept", var, line, column, fgc,
                                     bgc, scroll, cb_int(dispattrs)));
        } else {
          cb_emit(cb_build_funcall_7("cob_field_accept", var, pos, NULL, fgc,
                                     bgc, scroll, cb_int(dispattrs)));
        }
      } else {
        cb_emit(cb_build_funcall_7("cob_field_accept", var, NULL, NULL, fgc,
                                   bgc, scroll, cb_int(dispattrs)));
      }
    }
  } else if (pos || fgc || bgc || scroll) {
    /* Bump ref count to force CRT STATUS field generation */
    if (current_program->crt_status != NULL) {
      cb_field(current_program->crt_status)->count++;
    }
    if (!pos) {
      cb_emit(cb_build_funcall_7("cob_field_accept", var, NULL, NULL, fgc, bgc,
                                 scroll, cb_int(dispattrs)));
    } else if (CB_PAIR_P(pos)) {
      line = CB_PAIR_X(pos);
      column = CB_PAIR_Y(pos);
      cb_emit(cb_build_funcall_7("cob_field_accept", var, line, column, fgc,
                                 bgc, scroll, cb_int(dispattrs)));
    } else {
      cb_emit(cb_build_funcall_7("cob_field_accept", var, pos, NULL, fgc, bgc,
                                 scroll, cb_int(dispattrs)));
    }
  } else {
    cb_emit(cb_build_funcall_1("CobolTerminal.accept", var));
  }
}

void cb_emit_accept_line_or_col(cb_tree var, const int l_or_c) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_2("cob_screen_line_col", var, cb_int(l_or_c)));
}

void cb_emit_accept_date(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptDate", var));
}

void cb_emit_accept_date_yyyymmdd(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptDate_yyyymmdd", var));
}

void cb_emit_accept_day(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptDay", var));
}

void cb_emit_accept_day_yyyyddd(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptDay_yyyyddd", var));
}

void cb_emit_accept_day_of_week(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptDayOfWeek", var));
}

void cb_emit_accept_time(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptTime", var));
}

void cb_emit_accept_command_line(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptCommandLine", var));
}

void cb_emit_get_environment(cb_tree envvar, cb_tree envval) {
  if (cb_validate_one(envvar)) {
    return;
  }
  if (cb_validate_one(envval)) {
    return;
  }
  cb_emit(cb_build_funcall_2("CobolUtil.getEnvironment", envvar, envval));
}

void cb_emit_accept_environment(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptEnvironment", var));
}

void cb_emit_accept_arg_number(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptArgNumber", var));
}

void cb_emit_accept_arg_value(cb_tree var) {
  if (cb_validate_one(var)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.acceptArgValue", var));
}

void cb_emit_accept_mnemonic(cb_tree var, cb_tree mnemonic) {
  if (cb_validate_one(var)) {
    return;
  }
  if (CB_SYSTEM_NAME(cb_ref(mnemonic))->category == CB_DEVICE_NAME) {
    switch (CB_SYSTEM_NAME(cb_ref(mnemonic))->token) {
    case CB_DEVICE_CONSOLE:
    case CB_DEVICE_SYSIN:
      cb_emit(cb_build_funcall_1("CobolTerminal.accept", var));
      break;
    default:
      cb_error_x(mnemonic, _("Invalid input stream '%s'"), cb_name(mnemonic));
      break;
    }
  }
  if (CB_SYSTEM_NAME(cb_ref(mnemonic))->category == CB_INTERFACE_NAME) {
    switch (CB_SYSTEM_NAME(cb_ref(mnemonic))->token) {
    case CB_ARGUMENT_NUMBER:
      cb_emit_accept_arg_number(var);
      break;
    case CB_ARGUMENT_VALUE:
      cb_emit_accept_arg_value(var);
      break;
    case CB_ENVIRONMENT_VALUE:
      cb_emit_accept_environment(var);
      break;
    default:
      cb_error_x(mnemonic, _("Invalid interface name '%s'"), cb_name(mnemonic));
      break;
    }
  }
}

void cb_emit_accept_name(cb_tree var, cb_tree name) {
  cb_tree sys;

  if (cb_validate_one(var)) {
    return;
  }
  if (CB_REFERENCE(name)->word->count == 0) {
    sys = lookup_system_name(CB_NAME(name));

    if (sys != cb_error_node) {
      switch (CB_SYSTEM_NAME(sys)->token) {
      case CB_DEVICE_CONSOLE:
      case CB_DEVICE_SYSIN:
        cb_warning_x(name, _("'%s' undefined in SPECIAL-NAMES"), CB_NAME(name));
        cb_emit(cb_build_funcall_1("CobolTerminal.accept", var));
        return;
      default:
        break;
      }
    }
  }

  cb_error_x(name, _("'%s' undefined in SPECIAL-NAMES"), CB_NAME(name));
}

/*
 * ALLOCATE statement
 */

void cb_emit_allocate(cb_tree target1, cb_tree target2, cb_tree size,
                      cb_tree initialize) {
  cb_tree x;
  char buff[32];

  if (cb_validate_one(target1)) {
    return;
  }
  if (cb_validate_one(target2)) {
    return;
  }
  if (cb_validate_one(size)) {
    return;
  }
  if (target1) {
    if (!(CB_REFERENCE_P(target1) && cb_field(target1)->flag_item_based)) {
      cb_error_x(CB_TREE(current_statement),
                 _("Target of ALLOCATE is not a BASED item"));
    }
  }
  if (target2) {
    if (!(CB_REFERENCE_P(target2) &&
          CB_TREE_CLASS(target2) == CB_CLASS_POINTER)) {
      cb_error_x(CB_TREE(current_statement),
                 _("Target of RETURNING is not a data pointer"));
    }
  }
  if (size) {
    if (CB_TREE_CLASS(size) != CB_CLASS_NUMERIC) {
      cb_error_x(CB_TREE(current_statement),
                 _("The CHARACTERS field of ALLOCATE must be numeric"));
    }
  }
  if (target1) {
    sprintf(buff, "%d", cb_field(target1)->memory_size);
    x = cb_build_numeric_literal(0, (ucharptr)buff, 0);
    cb_emit(cb_build_funcall_3(
        "cob_allocate", cb_build_cast_addr_of_addr(target1), target2, x));
  } else {
    cb_emit(cb_build_funcall_3("cob_allocate", NULL, target2, size));
  }
  if (initialize && target1) {
    current_statement->handler2 =
        cb_build_initialize(target1, cb_true, NULL, cb_true, 0);
  }
}

/*
 * CALL statement
 */

void cb_emit_call(cb_tree prog, cb_tree cb_using, cb_tree returning,
                  cb_tree on_exception, cb_tree not_on_exception) {
  cb_tree l;
  cb_tree x;
  const struct system_table *psyst;
  int is_sys_call = 0;

  if (CB_INTRINSIC_P(prog)) {
    if (CB_INTRINSIC(prog)->intr_tab->category != CB_CATEGORY_ALPHANUMERIC) {
      cb_error(_("Only alphanumeric FUNCTION types are allowed here"));
      return;
    }
  }
  if (returning) {
    if (CB_TREE_CLASS(returning) != CB_CLASS_NUMERIC &&
        CB_TREE_CLASS(returning) != CB_CLASS_POINTER) {
      cb_error(_("Invalid RETURNING field"));
      return;
    }
  }
  for (l = cb_using; l; l = CB_CHAIN(l)) {
    x = CB_VALUE(l);
    if (x == cb_error_node) {
      continue;
    }
    if (CB_CONST_P(x) && x != cb_null) {
      cb_error_x(x, _("Figurative constant invalid here"));
    }
    if ((CB_REFERENCE_P(x) && CB_FIELD_P(CB_REFERENCE(x)->value)) ||
        CB_FIELD_P(x)) {
      if (cb_field(x)->level == 88) {
        cb_error_x(x, _("'%s' Not a data name"), CB_NAME(x));
        return;
      }
      if (cb_warn_call_params && CB_PURPOSE_INT(l) == CB_CALL_BY_REFERENCE) {
        if (cb_field(x)->level != 01 && cb_field(x)->level != 77) {
          cb_warning_x(x, _("'%s' is not 01 or 77 level item"), CB_NAME(x));
        }
      }
    }
  }

  if (CB_LITERAL_P(prog)) {
    for (psyst = (const struct system_table *)&system_tab[0]; psyst->syst_name;
         psyst++) {
      if (!strcmp((const char *)CB_LITERAL(prog)->data,
                  (const char *)psyst->syst_name)) {
        if (psyst->syst_params > cb_list_length(cb_using)) {
          cb_error(_("Wrong number of CALL parameters for '%s'"),
                   (char *)psyst->syst_name);
          return;
        }
        is_sys_call = 1;
        break;
      }
    }
  }

  cb_emit(cb_build_call(prog, cb_using, on_exception, not_on_exception,
                        returning, is_sys_call));
}

/*
 * CANCEL statement
 */

void cb_emit_cancel(cb_tree prog) {
  if (cb_validate_one(prog)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolResolve.fieldCancel", prog));
}

void cb_emit_cancel_all() { cb_emit(cb_build_funcall_0("cob_cancel_all")); }

/*
 * CLOSE statement
 */

void cb_emit_close(cb_tree file, cb_tree opt) {
  if (file == cb_error_node) {
    return;
  }
  file = cb_ref(file);
  if (file == cb_error_node) {
    return;
  }
  current_statement->file = file;
  if (CB_FILE(file)->organization == COB_ORG_SORT) {
    cb_error_x(CB_TREE(current_statement),
               _("Operation not allowed on SORT files"));
  }
  cb_emit(
      cb_build_method_call_3("close", file, opt, CB_FILE(file)->file_status));
}

/*
 * COMMIT statement
 */

void cb_emit_commit(void) { cb_emit(cb_build_funcall_0("CobolFile.commit")); }

/*
 * CONTINUE statement
 */

void cb_emit_continue(void) { cb_emit(cb_build_continue()); }

/*
 * DELETE statement
 */

void cb_emit_delete(cb_tree file) {
  if (file == cb_error_node) {
    return;
  }
  file = cb_ref(file);
  if (file == cb_error_node) {
    return;
  }
  current_statement->file = file;
  if (CB_FILE(file)->organization == COB_ORG_SORT) {
    cb_error_x(CB_TREE(current_statement),
               _("Operation not allowed on SORT files"));
  }
  cb_emit(cb_build_method_call_2("delete", file, CB_FILE(file)->file_status));
}

/*
 * DELETE FILE statement
 */

void cb_emit_delete_file(cb_tree file) {
  if (file == cb_error_node) {
    return;
  }
  file = cb_ref(file);
  if (file == cb_error_node) {
    return;
  }
  current_statement->file = file;
  if (CB_FILE(file)->organization == COB_ORG_SORT) {
    cb_error_x(CB_TREE(current_statement),
               _("Operation not allowed on SORT files"));
  }
  cb_emit(cb_build_method_call_2("cob_delete_file", file,
                                 CB_FILE(file)->file_status));
}

/*
 * DISPLAY statement
 */

void cb_emit_env_name(cb_tree value) {
  if (cb_validate_one(value)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.displayEnvironment", value));
}

void cb_emit_env_value(cb_tree value) {
  if (cb_validate_one(value)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.displayEnvValue", value));
}

void cb_emit_arg_number(cb_tree value) {
  if (cb_validate_one(value)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.displayArgNumber", value));
}

void cb_emit_command_line(cb_tree value) {
  if (cb_validate_one(value)) {
    return;
  }
  cb_emit(cb_build_funcall_1("CobolTerminal.displayCommandLine", value));
}

void cb_emit_display(cb_tree values, cb_tree upon, cb_tree no_adv, cb_tree pos,
                     cb_tree fgc, cb_tree bgc, cb_tree scroll, int dispattrs) {
  cb_tree l;
  cb_tree x;
  cb_tree line;
  cb_tree column;
  cb_tree p;

  if (cb_validate_list(values)) {
    return;
  }
  if (cb_validate_one(pos)) {
    return;
  }
  if (cb_validate_one(fgc)) {
    return;
  }
  if (cb_validate_one(bgc)) {
    return;
  }
  if (cb_validate_one(scroll)) {
    return;
  }
  for (l = values; l; l = CB_CHAIN(l)) {
    x = CB_VALUE(l);
    if (x == cb_error_node) {
      return;
    }

    switch (CB_TREE_TAG(x)) {
    case CB_TAG_LITERAL:
    case CB_TAG_INTRINSIC:
    case CB_TAG_CONST:
    case CB_TAG_STRING:
    case CB_TAG_INTEGER:
      break;
    case CB_TAG_REFERENCE:
      if (!CB_FIELD_P(CB_REFERENCE(x)->value)) {
        cb_error_x(x, _("'%s' is an invalid type for DISPLAY operand"),
                   cb_name(x));
        return;
      }
      break;
    default:
      cb_error_x(x, _("Invalid type for DISPLAY operand"));
      return;
    }
  }
  if (upon == cb_error_node) {
    return;
  }

  x = CB_VALUE(values);
  if ((CB_REF_OR_FIELD_P(x)) &&
      CB_FIELD(cb_ref(x))->storage == CB_STORAGE_SCREEN) {
    output_screen_from(CB_FIELD(cb_ref(x)), 0);
    gen_screen_ptr = 1;
    if (pos) {
      if (CB_PAIR_P(pos)) {
        line = CB_PAIR_X(pos);
        column = CB_PAIR_Y(pos);
        if (line == NULL) {
          line = cb_one;
        }
        if (column == NULL) {
          column = cb_one;
        }
        cb_emit(cb_build_funcall_3("cob_screen_display", x, line, column));
      } else {
        cb_emit(cb_build_funcall_3("cob_screen_display", x, pos, NULL));
      }
    } else {
      cb_emit(cb_build_funcall_3("cob_screen_display", x, NULL, NULL));
    }
    gen_screen_ptr = 0;
  } else if (pos || fgc || bgc || scroll || dispattrs) {
    if (!pos) {
      cb_emit(cb_build_funcall_7("cob_field_display", CB_VALUE(values), NULL,
                                 NULL, fgc, bgc, scroll, cb_int(dispattrs)));
    } else if (CB_PAIR_P(pos)) {
      line = CB_PAIR_X(pos);
      column = CB_PAIR_Y(pos);
      if (line == NULL) {
        line = cb_one;
      }
      if (column == NULL) {
        column = cb_one;
      }
      cb_emit(cb_build_funcall_7("cob_field_display", CB_VALUE(values), line,
                                 column, fgc, bgc, scroll, cb_int(dispattrs)));
    } else {
      cb_emit(cb_build_funcall_7("cob_field_display", CB_VALUE(values), pos,
                                 NULL, fgc, bgc, scroll, cb_int(dispattrs)));
    }
  } else {
    /* DISPLAY x ... [UPON device-name] */
    p = cb_build_funcall_3("CobolTerminal.display", upon, no_adv, values);
    CB_FUNCALL(p)->varcnt = cb_list_length(values);
    cb_emit(p);
    for (l = values; l; l = CB_CHAIN(l)) {
      x = CB_VALUE(l);
      if (CB_FIELD_P(x)) {
        CB_FIELD(cb_ref(x))->count++;
      }
    }
  }
}

void cb_emit_display_mnemonic(cb_tree values, cb_tree mnemonic, cb_tree no_adv,
                              cb_tree pos, cb_tree fgc, cb_tree bgc,
                              cb_tree scroll, int dispattrs) {
  if (CB_SYSTEM_NAME(cb_ref(mnemonic))->category == CB_INTERFACE_NAME) {
    cb_tree v = CB_VALUE(values);
    switch (CB_SYSTEM_NAME(cb_ref(mnemonic))->token) {
    case CB_ARGUMENT_NUMBER:
      cb_emit_arg_number(v);
      break;
    case CB_ENVIRONMENT_NAME:
      cb_emit_env_name(v);
      break;
    case CB_ENVIRONMENT_VALUE:
      cb_emit_env_value(v);
      break;
    default:
      cb_error_x(mnemonic, _("Invalid interface name '%s'"), cb_name(mnemonic));
      break;
    }
  } else {
    cb_tree var = cb_build_display_upon(mnemonic);
    cb_emit_display(values, var, no_adv, pos, fgc, bgc, scroll, dispattrs);
  }
}

cb_tree cb_build_display_upon(cb_tree x) {
  if (x == cb_error_node) {
    return cb_error_node;
  }

  switch (CB_SYSTEM_NAME(cb_ref(x))->token) {
  case CB_DEVICE_CONSOLE:
  case CB_DEVICE_SYSOUT:
    return cb_int0;
  case CB_DEVICE_SYSERR:
    return cb_int1;
  default:
    cb_error_x(x, _("Invalid output stream"));
    return cb_error_node;
  }
}

cb_tree cb_build_display_upon_direct(cb_tree x) {
  const char *name;
  cb_tree sys;

  if (x == cb_error_node) {
    return cb_error_node;
  }
  name = CB_NAME(x);
  if (CB_REFERENCE(x)->word->count == 0) {
    sys = lookup_system_name(CB_NAME(x));
    if (sys != cb_error_node) {
      switch (CB_SYSTEM_NAME(sys)->token) {
      case CB_DEVICE_CONSOLE:
      case CB_DEVICE_SYSOUT:
        cb_warning_x(x, _("'%s' undefined in SPECIAL-NAMES"), name);
        return cb_int0;
      case CB_DEVICE_SYSERR:
        cb_warning_x(x, _("'%s' undefined in SPECIAL-NAMES"), name);
        return cb_int1;
      default:
        break;
      }
    }
  }

  cb_error_x(x, _("'%s' undefined in SPECIAL-NAMES"), name);
  return cb_error_node;
}

/*
 * DIVIDE statement
 */

void cb_emit_divide(cb_tree dividend, cb_tree divisor, cb_tree quotient,
                    cb_tree remainder) {
  if (cb_validate_one(dividend)) {
    return;
  }
  if (cb_validate_one(divisor)) {
    return;
  }
  CB_VALUE(quotient) = cb_check_numeric_edited_name(CB_VALUE(quotient));
  CB_VALUE(remainder) = cb_check_numeric_edited_name(CB_VALUE(remainder));

  if (cb_validate_one(CB_VALUE(quotient))) {
    return;
  }
  if (cb_validate_one(CB_VALUE(remainder))) {
    return;
  }

  cb_emit(cb_build_method_call_4(
      "divQuotient", dividend, divisor, CB_VALUE(quotient),
      build_store_option(CB_VALUE(quotient), CB_PURPOSE(quotient))));
  cb_emit(
      cb_build_method_call_2("divRemainder", CB_VALUE(remainder),
                             build_store_option(CB_VALUE(remainder), cb_int0)));
}

/*
 * EVALUATE statement
 */

static cb_tree evaluate_test(cb_tree s, cb_tree o) {
  int flag;
  cb_tree x, y;
  cb_tree t;

  /* ANY is always true */
  if (o == cb_any) {
    return cb_true;
  }

  /* object TRUE or FALSE */
  if (o == cb_true) {
    return s;
  }
  if (o == cb_false) {
    return cb_build_negation(s);
  }

  flag = CB_PURPOSE_INT(o);
  x = CB_PAIR_X(CB_VALUE(o));
  y = CB_PAIR_Y(CB_VALUE(o));

  /* subject TRUE or FALSE */
  if (s == cb_true) {
    return flag ? cb_build_negation(x) : x;
  }
  if (s == cb_false) {
    return flag ? x : cb_build_negation(x);
  }

  /* x THRU y */
  if (y) {
    t = cb_build_binary_op(cb_build_binary_op(x, '[', s), '&',
                           cb_build_binary_op(s, '[', y));

    return flag ? cb_build_negation(t) : t;
  }

  if (CB_REFERENCE_P(x) && CB_FIELD_P(CB_REFERENCE(x)->value) &&
      CB_FIELD(CB_REFERENCE(x)->value)->level == 88) {
    cb_error_x(CB_TREE(current_statement),
               _("Invalid use of 88 level in WHEN expression"));
    return NULL;
  }

  /* regular comparison */
  if (flag) {
    return cb_build_binary_op(s, '~', x);
  } else {
    return cb_build_binary_op(s, '=', x);
  }
}

static int check_error_node(cb_tree x) {
  int rt = 0;

  if (x == cb_error_node) {
    rt = 1;
  } else if (CB_LIST_P(x)) {
    if (CB_PURPOSE(x) == cb_error_node) {
      rt = 1;
    } else if (CB_VALUE(x)) {
      rt = check_error_node(CB_VALUE(x));
    }
    if (!rt && CB_CHAIN(x)) {
      rt = check_error_node(CB_CHAIN(x));
    }
  }
  return rt;
}

static cb_tree build_evaluate(cb_tree subject_list, cb_tree case_list) {
  cb_tree c1 = NULL;
  cb_tree c2;
  cb_tree c3;
  cb_tree subjs;
  cb_tree whens;
  cb_tree objs;
  cb_tree stmt;
  cb_tree dummy_cond = NULL;

  if (case_list == NULL) {
    return NULL;
  }

  whens = CB_VALUE(case_list);
  stmt = CB_VALUE(whens);
  whens = CB_CHAIN(whens);

  /* for each WHEN sequence */
  for (; whens; whens = CB_CHAIN(whens)) {
    c2 = NULL;
    dummy_cond = NULL;
    /* single WHEN test */
    for (subjs = subject_list, objs = CB_VALUE(whens); subjs && objs;
         subjs = CB_CHAIN(subjs), objs = CB_CHAIN(objs)) {
      if (check_error_node(CB_VALUE(subjs)) ||
          check_error_node(CB_VALUE(objs))) {
        dummy_cond = cb_false;
        break;
      }
      c3 = evaluate_test(CB_VALUE(subjs), CB_VALUE(objs));
      if (c3 == NULL) {
        return NULL;
      }

      if (c2 == NULL) {
        c2 = c3;
      } else {
        c2 = cb_build_binary_op(c2, '&', c3);
      }
    }
    if (!dummy_cond && (subjs || objs)) {
      cb_error_x(CB_VALUE(whens) ? CB_VALUE(whens) : whens,
                 _("Wrong number of WHEN parameters"));
      dummy_cond = cb_false; /* suppress redundant error */
    }
    /* connect multiple WHEN's */
    if (c1 == NULL) {
      c1 = c2;
    } else {
      c1 = cb_build_binary_op(c1, '|', c2);
    }
  }

  if (c1 == NULL) {
    return stmt;
  } else {
    return cb_build_if((dummy_cond ? dummy_cond : cb_build_cond(c1)), stmt,
                       build_evaluate(subject_list, CB_CHAIN(case_list)));
  }
}

void cb_emit_evaluate(cb_tree subject_list, cb_tree case_list) {
  cb_emit(build_evaluate(subject_list, case_list));
}

/*
 * FREE statement
 */

void cb_emit_free(cb_tree vars) {
  cb_tree l;
  struct cb_field *f;
  int i;

  if (cb_validate_list(vars)) {
    return;
  }
  for (l = vars, i = 1; l; l = CB_CHAIN(l), i++) {
    if (CB_TREE_CLASS(CB_VALUE(l)) == CB_CLASS_POINTER) {
      if (CB_CAST_P(CB_VALUE(l))) {
        f = cb_field(CB_CAST(CB_VALUE(l))->val);
        if (!f->flag_item_based) {
          cb_error_x(CB_TREE(current_statement),
                     _("Target %d of FREE, a data address identifier, must "
                       "address a BASED data item"),
                     i);
        }
        cb_emit(cb_build_funcall_2("cob_free_alloc",
                                   cb_build_cast_address(CB_VALUE(l)), NULL));
      } else {
        cb_emit(cb_build_funcall_2("cob_free_alloc", NULL,
                                   cb_build_cast_address(CB_VALUE(l))));
      }
    } else if (CB_REF_OR_FIELD_P(CB_VALUE(l))) {
      f = cb_field(CB_VALUE(l));
      if (!f->flag_item_based) {
        cb_error_x(CB_TREE(current_statement),
                   _("Target %d of FREE, a data address identifier, must "
                     "address a BASED data item"),
                   i);
      }
      cb_emit(cb_build_funcall_2(
          "cob_free_alloc", cb_build_cast_addr_of_addr(CB_VALUE(l)), NULL));
    } else {
      cb_error_x(CB_TREE(current_statement),
                 _("Target %d of FREE must be a data pointer"), i);
    }
  }
}

/*
 * GO TO statement
 */

void cb_emit_goto(cb_tree target, cb_tree depending) {
  if (target == cb_error_node) {
    return;
  }
  if (depending) {
    /* GO TO procedure-name ... DEPENDING ON identifier */
    cb_emit(cb_build_goto(target, depending));
  } else {
    /* GO TO procedure-name */
    if (target == NULL) {
      cb_verify(cb_goto_statement_without_name, "GO TO without procedure-name");
    } else if (CB_CHAIN(target)) {
      cb_error(_("GO TO with multiple procedure-names"));
    } else {
      cb_emit(cb_build_goto(CB_VALUE(target), NULL));
    }
  }
}

void cb_emit_java_continue(void) {
  cb_emit(make_tree(CB_TAG_JAVA_CONTINUE, CB_CATEGORY_UNKNOWN,
                    sizeof(struct cb_tree_common)));
}

void cb_emit_java_break(void) {

  cb_emit(make_tree(CB_TAG_JAVA_BREAK, CB_CATEGORY_UNKNOWN,
                    sizeof(struct cb_tree_common)));
}

void cb_emit_exit(size_t goback) {
  if (goback) {
    cb_emit(cb_build_goto(cb_int1, NULL));
  } else {
    cb_emit(cb_build_goto(NULL, NULL));
  }
}

/*
 * IF statement
 */

void cb_emit_if(cb_tree cond, cb_tree stmt1, cb_tree stmt2) {
  cb_emit(cb_build_if(cond, stmt1, stmt2));
}

/*
 * INITIALIZE statement
 */

void cb_emit_initialize(cb_tree vars, cb_tree fillinit, cb_tree value,
                        cb_tree replacing, cb_tree def) {
  cb_tree l;
  int fill_init = 1;

  if (cb_validate_list(vars)) {
    return;
  }
  if (value == NULL && replacing == NULL) {
    def = cb_true;
  }
  if (fillinit == cb_true) {
    fill_init = 0;
  }
  for (l = vars; l; l = CB_CHAIN(l)) {
    cb_emit(cb_build_initialize(CB_VALUE(l), value, replacing, def, fill_init));
  }
}

/*
 * INSPECT statement
 */

void cb_emit_inspect(cb_tree var, cb_tree body, cb_tree replacing,
                     int replconv) {
  switch (CB_TREE_TAG(var)) {
  case CB_TAG_REFERENCE:
    break;
  case CB_TAG_INTRINSIC:
    switch (CB_TREE_CATEGORY(var)) {
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
    case CB_CATEGORY_NATIONAL:
      break;
    default:
      cb_error(_("Invalid target for INSPECT"));
      return;
    }
    break;
  case CB_TAG_LITERAL:
    break;
  default:
    cb_error(_("Invalid target for REPLACING/CONVERTING"));
    return;
  }
  if (replconv && sending_id) {
    cb_error(_("Invalid target for REPLACING/CONVERTING"));
  }
  cb_emit(cb_build_funcall_2("CobolInspect.init", var, replacing));
  cb_emit_list(body);
  cb_emit(cb_build_funcall_0("CobolInspect.finish"));
}

void cb_init_tarrying(void) {
  inspect_func = NULL;
  inspect_data = NULL;
}

cb_tree cb_build_tarrying_data(cb_tree x) {
  inspect_data = x;
  return NULL;
}

cb_tree cb_build_tarrying_characters(cb_tree l) {
  if (inspect_data == NULL) {
    cb_error(_("Data name expected before CHARACTERS"));
  }
  inspect_func = NULL;
  return cb_list_add(
      l, cb_build_funcall_1("CobolInspect.characters", inspect_data));
}

cb_tree cb_build_tarrying_all(void) {
  if (inspect_data == NULL) {
    cb_error(_("Data name expected before ALL"));
  }
  inspect_func = "CobolInspect.all";
  return NULL;
}

cb_tree cb_build_tarrying_leading(void) {
  if (inspect_data == NULL) {
    cb_error(_("Data name expected before LEADING"));
  }
  inspect_func = "CobolInspect.leading";
  return NULL;
}

cb_tree cb_build_tarrying_trailing(void) {
  if (inspect_data == NULL) {
    cb_error(_("Data name expected before TRAILING"));
  }
  inspect_func = "CobolInspect.trailing";
  return NULL;
}

cb_tree cb_build_tarrying_value(cb_tree x, cb_tree l) {
  if (inspect_func == NULL) {
    cb_error_x(x, _("ALL, LEADING or TRAILING expected before '%s'"),
               cb_name(x));
  }
  return cb_list_add(l, cb_build_funcall_2(inspect_func, inspect_data, x));
}

#ifdef I18N_UTF8

static int cb_validate_single_char_data(cb_tree x) {
  char msgbuf[256];
  int rt = 0;

  if (CB_LITERAL_P(x)) {
    struct cb_literal *lp = CB_LITERAL(x);
    if (lp->size != COB_U8BYTE_1(lp->data[0])) {

      memset(msgbuf, 0, sizeof(msgbuf));
      strncpy(msgbuf, (char *)lp->data, 253);
      cb_error_x(x, "Illegal replacement size: '%s'.", msgbuf);
      rt = 1;
    }
  } else {
    /* can't determine char length statically. */
  }
  return rt;
}

static int check_equal_data_size(cb_tree x, cb_tree y) {
  char msgbuf1[256], msgbuf2[256];
  size_t len1 = 0, len2 = 0;
  int rt = 0;

  memset(msgbuf1, 0, sizeof(msgbuf1));
  memset(msgbuf2, 0, sizeof(msgbuf2));

  if (CB_LITERAL_P(x)) {
    len1 = CB_LITERAL(x)->size;
    strcat(msgbuf1, "'");
    strncat(msgbuf1, (char *)CB_LITERAL(x)->data, 253);
    strcat(msgbuf1, "'");
  } else if (CB_REFERENCE_P(x)) {
    len1 = CB_FIELD(cb_ref(x))->size;
    cb_get_jisword_buff((char *)CB_FIELD(cb_ref(x))->name, msgbuf1,
                        sizeof(msgbuf1));
  } else {
    cb_error_x(x, "Unexpected tag %d.", CB_TREE_TAG(x));
    rt = 1;
  }
  if (CB_LITERAL_P(y)) {
    len2 = CB_LITERAL(y)->size;
    strcat(msgbuf2, "'");
    strncat(msgbuf2, (char *)CB_LITERAL(y)->data, 253);
    strcat(msgbuf2, "'");
  } else if (CB_REFERENCE_P(y)) {
    len2 = CB_FIELD(cb_ref(y))->size;
    cb_get_jisword_buff((char *)CB_FIELD(cb_ref(y))->name, msgbuf2,
                        sizeof(msgbuf2));
  } else {
    cb_error_x(y, "Unexpected tag %d.", CB_TREE_TAG(y));
    rt = 1;
  }
  if (!rt && len1 != len2) {
    cb_error_x(x, "%s and %s have not same size!", msgbuf1, msgbuf2);
    rt = (int)(len1 - len2);
  }
  return rt;
}

static int cb_validate_inspect_replaceable(cb_tree x, cb_tree y) {
  int rt = 0;

  if (y == cb_zero || y == cb_space || y == cb_quote || y == cb_high ||
      y == cb_low) {
    /* always replaceable */
  } else if (check_equal_data_size(x, y)) {
    rt = 1;
  }
  return rt;
}

static int cb_validate_inspect_convertible(cb_tree x, cb_tree y) {
  unsigned char *data1;
  unsigned char *data2;
  size_t i, n, nc;
  int rt = 0;

  /* should be convertible char by char in UTF-8 mode */

  if (y == cb_zero || y == cb_space || y == cb_quote || y == cb_high ||
      y == cb_low) {
    if (CB_LITERAL_P(x)) {
      data1 = CB_LITERAL(x)->data;
      n = CB_LITERAL(x)->size;
      for (i = 0, nc = 0; !rt && i < n; i += nc) {
        nc = COB_U8BYTE_1(data1[i]);
        if (!nc) {
          cb_error_x(x, "Unexpected char in literal.");
          rt = 1;
        } else if (nc != 1 && nc != COB_U8CSIZ) {
          cb_error_x(x, "Illegal conversion chars.");
          rt = 1;
        }
      }
    } else {
      /* can't determine char length statically. */
    }
  } else if (check_equal_data_size(x, y)) {
    /* should be at least in same length */
    rt = 1;
  } else if (CB_LITERAL_P(x) && CB_LITERAL_P(y)) {
    data1 = CB_LITERAL(x)->data;
    data2 = CB_LITERAL(y)->data;
    n = CB_LITERAL(x)->size;
    for (i = 0, nc = 0; !rt && i < n; i += nc) {
      nc = COB_U8BYTE_1(data1[i]);
      if (!nc) {
        cb_error_x(x, "Unexpected char in literal.");
        rt = 1;
      } else if (nc != COB_U8BYTE_1(data2[i])) {
        cb_error_x(x, "Illegal conversion chars.");
        rt = 1;
      }
    }
  } else {
    /* can't determine char length statically. */
  }
  return rt;
}

int cb_validate_inspect(cb_tree var, cb_tree x, cb_tree y) {
  /*
   * never return error result(<0), as original
   * cb_validate_inspect() also doesn't.
   */
  cb_validate_inspect_convertible(x, y);
  return 0;
}

#else  /*I18N_UTF8*/

int cb_validate_inspect(cb_tree var, cb_tree x, cb_tree y) {
  int s1, s2;
  struct cb_field *pfield;
  struct cb_literal *pliteral;
  char name1[256], name2[256];

  memset(name1, 0, sizeof(name1));
  memset(name2, 0, sizeof(name2));

  switch (CB_TREE_TAG(x)) {
  case CB_TAG_REFERENCE:
    pfield = CB_FIELD(cb_ref(x));
    s1 = pfield->size;
    cb_get_jisword_buff((char *)pfield->name, name1, sizeof(name1));
    break;
  case CB_TAG_LITERAL:
    pliteral = CB_LITERAL(x);
    s1 = pliteral->size;
    strcpy(name1, "\'");
    if (s1 >= 253) {
      memcpy(name1 + 1, pliteral->data, 253);
    } else {
      memcpy(name1 + 1, pliteral->data, s1);
    }
    strcat(name1, "\'");
    break;
  default:
    s1 = 0;
    break;
  }
  if (y == 0) {
    if (x != cb_zero && x != cb_space && x != cb_quote && x != cb_high &&
        x != cb_low) {
      if (CB_TREE_CATEGORY(var) == CB_CATEGORY_NATIONAL ||
          CB_TREE_CATEGORY(var) == CB_CATEGORY_NATIONAL_EDITED) {
        if (s1 != 2) {
          cb_error_x(x, "Illegal replacement size: %s", name1);
        }
      } else {
        if (s1 != 1) {
          cb_error_x(x, "Illegal replacement size: %s", name1);
        }
      }
    }
    return 0;
  }
  switch (CB_TREE_TAG(y)) {
  case CB_TAG_REFERENCE:
    pfield = CB_FIELD(cb_ref(y));
    s2 = pfield->size;
    cb_get_jisword_buff((char *)pfield->name, name2, sizeof(name2));
    break;
  case CB_TAG_LITERAL:
    pliteral = CB_LITERAL(y);
    s2 = pliteral->size;
    strcpy(name2, "\'");
    if (s1 >= 253) {
      memcpy(name2 + 1, pliteral->data, 253);
    } else {
      memcpy(name2 + 1, pliteral->data, s2);
    }
    strcat(name2, "\'");
    break;
  default:
    s2 = 0;
    break;
  }
  if (s1 != s2 && y != cb_zero && y != cb_space && y != cb_quote &&
      y != cb_high && y != cb_low && x != cb_zero && x != cb_space &&
      x != cb_quote && x != cb_high && x != cb_low) {
    cb_error_x(x, "%s and %s have not same size!", name1, name2);
    return 0;
  }
  switch (CB_TREE_CATEGORY(var)) {
  case CB_CATEGORY_NATIONAL:
  case CB_CATEGORY_NATIONAL_EDITED:
    if (CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL ||
        CB_TREE_CATEGORY(x) == CB_CATEGORY_NATIONAL_EDITED) {
      if (CB_TREE_CATEGORY(y) != CB_CATEGORY_NATIONAL &&
          CB_TREE_CATEGORY(y) != CB_CATEGORY_NATIONAL_EDITED && y != cb_zero &&
          y != cb_space && y != cb_quote && y != cb_high && y != cb_low) {
        cb_warning_x(y, _("%s and %s have not same type!"), name1, name2);
      }
    }
    break;
  default:
    if (CB_TREE_CATEGORY(y) != CB_TREE_CATEGORY(x) && y != cb_zero &&
        y != cb_space && y != cb_quote && y != cb_high && y != cb_low) {
      cb_warning_x(y, _("%s and %s have not same type!"), name1, name2);
    }
    break;
  }
  return 0;
}
#endif /*I18N_UTF8*/

cb_tree cb_build_replacing_characters(cb_tree x, cb_tree l, cb_tree var) {
#ifdef I18N_UTF8
  cb_validate_single_char_data(x);
#else  /*I18N_UTF8*/
  /*
   * caution: cb_validate_inspect() never returns error (<0)
   */
  if (cb_validate_inspect(var, x, 0) < 0) {
    return cb_error_node;
  }
#endif /*I18N_UTF8*/
  return cb_list_add(l, cb_build_funcall_1("CobolInspect.characters", x));
}

cb_tree cb_build_replacing_all(cb_tree x, cb_tree y, cb_tree l, cb_tree var) {
#ifdef I18N_UTF8
  cb_validate_inspect_replaceable(x, y);
#else  /*I18N_UTF8*/
  /*
   * caution: cb_validate_inspect() never returns error (<0)
   */
  if (cb_validate_inspect(var, x, y) < 0) {
    return cb_error_node;
  }
#endif /*I18N_UTF8*/
  return cb_list_add(l, cb_build_funcall_2("CobolInspect.all", y, x));
}

cb_tree cb_build_replacing_leading(cb_tree x, cb_tree y, cb_tree l) {
  return cb_list_add(l, cb_build_funcall_2("CobolInspect.leading", y, x));
}

cb_tree cb_build_replacing_first(cb_tree x, cb_tree y, cb_tree l) {
  return cb_list_add(l, cb_build_funcall_2("CobolInspect.first", y, x));
}

cb_tree cb_build_replacing_trailing(cb_tree x, cb_tree y, cb_tree l) {
  return cb_list_add(l, cb_build_funcall_2("CobolInspect.trailing", y, x));
}

cb_tree cb_build_converting(cb_tree x, cb_tree y, cb_tree l) {
  return cb_list_add(l, cb_build_funcall_2("CobolInspect.converting", x, y));
}

cb_tree cb_build_inspect_region_start(void) {
  return cb_list_init(cb_build_funcall_0("CobolInspect.start"));
}

cb_tree cb_build_inspect_region(cb_tree l, cb_tree pos, cb_tree x) {
  if (pos == CB_BEFORE) {
    return cb_list_add(l, cb_build_funcall_1("CobolInspect.before", x));
  } else {
    return cb_list_add(l, cb_build_funcall_1("CobolInspect.after", x));
  }
}

/*
 * MOVE statement
 */

static void warning_destination(cb_tree x) {
  struct cb_reference *r;
  struct cb_field *f;
  cb_tree loc;

  r = CB_REFERENCE(x);
  f = CB_FIELD(r->value);
  loc = CB_TREE(f);

  if (r->offset) {
    return;
  }

  if (!strcmp(f->name, "RETURN-CODE") || !strcmp(f->name, "SORT-RETURN") ||
      !strcmp(f->name, "NUMBER-OF-CALL-PARAMETERS")) {
    cb_warning(_("Internal register '%s' defined as BINARY-LONG"), f->name);
  } else if (f->pic) {
    cb_warning_x(loc, _("'%s' defined here as PIC %s"),
                 check_filler_name((char *)f->name), f->pic->orig);
  } else {
    cb_warning_x(loc, _("'%s' defined here as a group of length %d"),
                 check_filler_name((char *)f->name), f->size);
  }
}

static int move_error(cb_tree src, cb_tree dst, const size_t value_flag,
                      const int flag, const int src_flag, const char *msg) {
  cb_tree loc;

  if (suppress_warn) {
    return 0;
  }
  loc = src->source_line ? src : dst;
  if (value_flag) {
    /* VALUE clause */
    cb_warning_x(loc, msg);
  } else {
    /* MOVE statement */
    if (flag) {
      cb_warning_x(loc, msg);
      if (src_flag) {
        warning_destination(src);
      }
      warning_destination(dst);
    }
  }

  return 0;
}

static void error_destination(cb_tree x) {
  struct cb_reference *r;
  struct cb_field *f;
  cb_tree loc;

  r = CB_REFERENCE(x);
  f = CB_FIELD(r->value);
  loc = CB_TREE(f);

  if (r->offset) {
    return;
  }

  if (!strcmp(f->name, "RETURN-CODE") || !strcmp(f->name, "SORT-RETURN") ||
      !strcmp(f->name, "NUMBER-OF-CALL-PARAMETERS")) {
    cb_error(_("Internal register '%s' defined as BINARY-LONG"), f->name);
  } else if (f->pic) {
    cb_error_x(loc, _("'%s' defined here as PIC %s"),
               check_filler_name((char *)f->name), f->pic->orig);
  } else {
    cb_error_x(loc, _("'%s' defined here as a group of length %d"),
               check_filler_name((char *)f->name), f->size);
  }
}

static int move_error2(cb_tree src, cb_tree dst, const size_t value_flag,
                       const int flag, const int src_flag, const char *msg) {
  cb_tree loc;

  loc = src->source_line ? src : dst;
  if (value_flag) {
    /* VALUE clause */
    cb_error_x(loc, msg);
  } else {
    /* MOVE statement */
    if (flag) {
      cb_error_x(loc, msg);
      if (src_flag) {
        error_destination(src);
      }
      error_destination(dst);
    }
  }

  return 0;
}

/* count the number of free places in an alphanumeric edited field */
static int count_pic_alphanumeric_edited(struct cb_field *field) {
  int count;
  int repeat;
  unsigned char *p;

  count = 0;
  for (p = (unsigned char *)(field->pic->str); *p; p += 5) {
    if (*p == '9' || *p == 'A' || *p == 'X') {
      memcpy((unsigned char *)&repeat, p + 1, sizeof(int));
      count += repeat;
    }
  }
  return count;
}

int validate_move(cb_tree src, cb_tree dst, size_t is_value) {
  struct cb_field *f;
  struct cb_field *pTmp;
  struct cb_literal *l;
  unsigned char *p;
  cb_tree loc;
  long long val;
  size_t i;
  size_t is_numeric_edited = 0;
  int src_scale_mod;
  int dst_scale_mod;
  int dst_size_mod;
  int size;
  int most_significant;
  int least_significant;

  loc = src->source_line ? src : dst;
  if (CB_REFERENCE_P(dst) && CB_ALPHABET_NAME_P(CB_REFERENCE(dst)->value)) {
    goto invalid;
  }
  if (CB_REFERENCE_P(dst) && CB_FILE_P(CB_REFERENCE(dst)->value)) {
    goto invalid;
  }
  if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_BOOLEAN) {
    cb_error_x(loc, _("Invalid destination for MOVE"));
    return -1;
  }

  if (CB_TREE_CLASS(dst) == CB_CLASS_POINTER) {
    if (CB_TREE_CLASS(src) == CB_CLASS_POINTER) {
      return 0;
    } else {
      goto invalid;
    }
  }

  f = cb_field(dst);
  if (CB_TREE_TAG(dst) == CB_TAG_REFERENCE) {
    if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_NATIONAL ||
        CB_TREE_CATEGORY(dst) == CB_CATEGORY_NATIONAL_EDITED) {
      if (CB_REFERENCE(dst)->offset) {
        switch (CB_TREE_CATEGORY(src)) {
        case CB_CATEGORY_ALPHABETIC:
          goto invalid;
          break;
        case CB_CATEGORY_NUMERIC:
          if (CB_REFERENCE_P(src)) {
            pTmp = CB_FIELD(cb_ref(src));
            if (CB_TREE(pTmp) != cb_error_node) {
              if (pTmp->pic) {
                if (pTmp->pic->category == CB_CATEGORY_NUMERIC) {
                  if (pTmp->pic->scale > 0) {
                    goto invalid;
                  }
                }
              }
            }
          }
          break;
        default:
          break;
        }
      }
    }
  }
  switch (CB_TREE_TAG(src)) {
  case CB_TAG_CONST:
    if (src == cb_space) {
      if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_NUMERIC ||
          (CB_TREE_CATEGORY(dst) == CB_CATEGORY_NUMERIC_EDITED && !is_value)) {
        goto invalid;
      }
    } else if (src == cb_zero) {
      if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_ALPHABETIC) {
        goto invalid;
      }
    }
    break;
  case CB_TAG_LITERAL:
    /* TODO: ALL literal */

    l = CB_LITERAL(src);
    if (CB_TREE_CLASS(src) == CB_CLASS_NUMERIC) {
      /* Numeric literal */
      if (l->all) {
        goto invalid;
      }
      most_significant = -999;
      least_significant = 999;

      /* compute the most significant figure place */
      for (i = 0; i < l->size; i++) {
        if (l->data[i] != '0') {
          break;
        }
      }
      if (i != l->size) {
        most_significant = (int)(l->size - l->scale - i - 1);
      }

      /* compute the least significant figure place */
      for (i = 0; i < l->size; i++) {
        if (l->data[l->size - i - 1] != '0') {
          break;
        }
      }
      if (i != l->size) {
        least_significant = (int)(-l->scale + i);
      }

      /* value check */
      switch (CB_TREE_CATEGORY(dst)) {
      case CB_CATEGORY_NATIONAL:
      case CB_CATEGORY_NATIONAL_EDITED:
        if (is_value) {
          goto expect_national;
        }

        if (l->scale == 0) {
          goto expect_national;
        } else {
          goto invalid;
        }
      case CB_CATEGORY_ALPHANUMERIC:
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
        if (is_value) {
          goto expect_alphanumeric;
        }

        if (l->scale == 0) {
          goto expect_alphanumeric;
        } else {
          goto invalid;
        }
      case CB_CATEGORY_NUMERIC:
        if (f->pic->scale < 0) {
          /* check for PIC 9(n)P(m) */
          if (least_significant < -f->pic->scale) {
            goto value_mismatch;
          }
        } else if (f->pic->scale > f->pic->size) {
          /* check for PIC P(n)9(m) */
          if (most_significant >= f->pic->size - f->pic->scale) {
            goto value_mismatch;
          }
        }
        break;
      case CB_CATEGORY_NUMERIC_EDITED:
        if (is_value) {
          goto expect_alphanumeric;
        }

        /* TODO */
        break;
      default:
        if (is_value) {
          goto expect_alphanumeric;
        }
        goto invalid;
      }

      /* sign check */
      if (l->sign != 0 && !f->pic->have_sign) {
        if (is_value) {
          cb_error_x(loc, _("Data item not signed"));
          return -1;
        }
        if (cb_warn_constant) {
          cb_warning_x(loc, _("Ignoring negative sign"));
        }
      }

      /* size check */
      if (f->flag_real_binary ||
          ((f->usage == CB_USAGE_COMP_5 || f->usage == CB_USAGE_COMP_X ||
            f->usage == CB_USAGE_BINARY) &&
           f->pic->scale == 0)) {
        p = l->data;
        for (i = 0; i < l->size; i++) {
          if (l->data[i] != '0') {
            p = &l->data[i];
            break;
          }
        }
        i = l->size - i;
        switch (f->size) {
        case 1:
          if (i > 18) {
            goto numlit_overflow;
          }
          val = cb_get_long_long(src);
          if (f->pic->have_sign) {
            if (val < -128LL || val > 127LL) {
              goto numlit_overflow;
            }
          } else {
            if (val > 255LL) {
              goto numlit_overflow;
            }
          }
          break;
        case 2:
          if (i > 18) {
            goto numlit_overflow;
          }
          val = cb_get_long_long(src);
          if (f->pic->have_sign) {
            if (val < -32768LL || val > 32767LL) {
              goto numlit_overflow;
            }
          } else {
            if (val > 65535LL) {
              goto numlit_overflow;
            }
          }
          break;
        case 3:
          if (i > 18) {
            goto numlit_overflow;
          }
          val = cb_get_long_long(src);
          if (f->pic->have_sign) {
            if (val < -8388608LL || val > 8388607LL) {
              goto numlit_overflow;
            }
          } else {
            if (val > 16777215LL) {
              goto numlit_overflow;
            }
          }
          break;
        case 4:
          if (i > 18) {
            goto numlit_overflow;
          }
          val = cb_get_long_long(src);
          if (f->pic->have_sign) {
            if (val < -2147483648LL || val > 2147483647LL) {
              goto numlit_overflow;
            }
          } else {
            if (val > 4294967295LL) {
              goto numlit_overflow;
            }
          }
          break;
        case 5:
          if (i > 18) {
            goto numlit_overflow;
          }
          val = cb_get_long_long(src);
          if (f->pic->have_sign) {
            if (val < -549755813888LL || val > 549755813887LL) {
              goto numlit_overflow;
            }
          } else {
            if (val > 1099511627775LL) {
              goto numlit_overflow;
            }
          }
          break;
        case 6:
          if (i > 18) {
            goto numlit_overflow;
          }
          val = cb_get_long_long(src);
          if (f->pic->have_sign) {
            if (val < -140737488355328LL || val > 140737488355327LL) {
              goto numlit_overflow;
            }
          } else {
            if (val > 281474976710655LL) {
              goto numlit_overflow;
            }
          }
          break;
        case 7:
          if (i > 18) {
            goto numlit_overflow;
          }
          val = cb_get_long_long(src);
          if (f->pic->have_sign) {
            if (val < -36028797018963968LL || val > 36028797018963967LL) {
              goto numlit_overflow;
            }
          } else {
            if (val > 72057594037927935LL) {
              goto numlit_overflow;
            }
          }
          break;
        default:
          if (f->pic->have_sign) {
            if (i < 19) {
              break;
            }
            if (i > 19) {
              goto numlit_overflow;
            }
            if (memcmp(p, "9223372036854775807", 19) > 0) {
              goto numlit_overflow;
            }
          } else {
            if (i < 20) {
              break;
            }
            if (i > 20) {
              goto numlit_overflow;
            }
            if (memcmp(p, "18446744073709551615", 20) > 0) {
              goto numlit_overflow;
            }
          }
          break;
        }
        return 0;
      }
      if (least_significant < -f->pic->scale) {
        goto size_overflow;
      }
      if (f->pic->scale > 0) {
        size = f->pic->digits - f->pic->scale;
      } else {
        size = f->pic->digits;
      }
      if (most_significant >= size) {
        goto size_overflow;
      }
    } else {
      /* Alphanumeric literal */

      /* value check */
      switch (CB_TREE_CATEGORY(src)) {
      case CB_CATEGORY_NATIONAL:
        switch (CB_TREE_CATEGORY(dst)) {
        case CB_CATEGORY_ALPHABETIC:
        case CB_CATEGORY_NUMERIC:
        case CB_CATEGORY_NUMERIC_EDITED:
          goto invalid;
        default:
          break;
        }
      default:
        switch (CB_TREE_CATEGORY(dst)) {
        case CB_CATEGORY_ALPHABETIC:
          for (i = 0; i < l->size; i++) {
            if (!isalpha(l->data[i]) && !isspace(l->data[i])) {
              goto value_mismatch;
            }
          }
          break;
        case CB_CATEGORY_NUMERIC:
          goto expect_numeric;
        case CB_CATEGORY_NUMERIC_EDITED:
          if (!is_value) {
            goto expect_numeric;
          }

          /* TODO: validate the value */
          break;
        default:
          break;
        }
      }

      /* size check */
      size = cb_field_size(dst);
#ifdef I18N_UTF8
      if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_NATIONAL) {
        /* I18N_UTF8: check in converted length. */
        i = utf8_national_length(l->data, l->size);
        if ((int)i < 0) {
          goto invalid_national;
        }
        if (size >= 0 && i > size) {
          goto size_overflow;
        }
      } else if (size >= 0 && (int)l->size > size) {
        goto size_overflow;
      }
#else  /*!I18N_UTF8*/
      if (size >= 0 && (int)l->size > size) {
        goto size_overflow;
      }
#endif /*I18N_UTF8*/
    }
    break;
  case CB_TAG_FIELD:
  case CB_TAG_REFERENCE:
    if (CB_REFERENCE_P(src) && CB_ALPHABET_NAME_P(CB_REFERENCE(src)->value)) {
      break;
    }
    if (CB_REFERENCE_P(src) && CB_FILE_P(CB_REFERENCE(src)->value)) {
      goto invalid;
    }
    size = cb_field_size(src);
    if (size < 0) {
      size = cb_field(src)->size;
    }
    /* non-elementary move */
    if (cb_field(src)->children || cb_field(dst)->children) {
      if (size > cb_field(dst)->size) {
        goto size_overflow_1;
      }
      break;
    }

    /* elementary move */
    switch (CB_TREE_CATEGORY(src)) {
    case CB_CATEGORY_ALPHANUMERIC:
      switch (CB_TREE_CATEGORY(dst)) {
      case CB_CATEGORY_NUMERIC:
      case CB_CATEGORY_NUMERIC_EDITED:
        if (size > cb_field(dst)->pic->digits) {
          goto size_overflow_2;
        }
        break;
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
      case CB_CATEGORY_NATIONAL_EDITED:
        if (size > count_pic_alphanumeric_edited(cb_field(dst))) {
          goto size_overflow_1;
        }
        break;
      default:
        if (size > cb_field(dst)->size) {
          goto size_overflow_1;
        }
        break;
      }
      break;
    case CB_CATEGORY_NATIONAL:
      switch (CB_TREE_CATEGORY(dst)) {
      case CB_CATEGORY_ALPHABETIC:
      case CB_CATEGORY_NUMERIC:
      case CB_CATEGORY_NUMERIC_EDITED:
        goto invalid;
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
      case CB_CATEGORY_NATIONAL_EDITED:
        if (size > count_pic_alphanumeric_edited(cb_field(dst))) {
          goto size_overflow_1;
        }
        break;
      default:
        if (size > cb_field(dst)->size) {
          goto size_overflow_1;
        }
        break;
      }
      break;
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC_EDITED:
      switch (CB_TREE_CATEGORY(dst)) {
      case CB_CATEGORY_NUMERIC:
      case CB_CATEGORY_NUMERIC_EDITED:
        goto invalid;
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
        if (size > count_pic_alphanumeric_edited(cb_field(dst))) {
          goto size_overflow_1;
        }
        break;
      default:
        if (size > cb_field(dst)->size) {
          goto size_overflow_1;
        }
        break;
      }
      break;
    case CB_CATEGORY_NATIONAL_EDITED:
      switch (CB_TREE_CATEGORY(dst)) {
      case CB_CATEGORY_ALPHABETIC:
      case CB_CATEGORY_NUMERIC:
      case CB_CATEGORY_NUMERIC_EDITED:
        goto invalid;
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
        if (size > count_pic_alphanumeric_edited(cb_field(dst))) {
          goto size_overflow_1;
        }
        break;
      default:
        if (size > cb_field(dst)->size) {
          goto size_overflow_1;
        }
        break;
      }
      break;
    case CB_CATEGORY_NUMERIC:
      switch (CB_TREE_CATEGORY(dst)) {
      case CB_CATEGORY_ALPHABETIC:
        goto invalid;
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
        is_numeric_edited = 1;
        /* Drop through */
      case CB_CATEGORY_ALPHANUMERIC:
        if (is_numeric_edited) {
          dst_size_mod = count_pic_alphanumeric_edited(cb_field(dst));
        } else {
          dst_size_mod = cb_field(dst)->size;
        }
        if (CB_TREE_CATEGORY(src) == CB_CATEGORY_NUMERIC &&
            cb_field(src)->pic->scale > 0) {
          if (cb_move_noninteger_to_alphanumeric == CB_ERROR) {
            goto invalid;
          }
          cb_warning_x(loc, _("Move non-integer to alphanumeric"));
          break;
        }
        if (CB_TREE_CATEGORY(src) == CB_CATEGORY_NUMERIC &&
            cb_field(src)->pic->digits > dst_size_mod) {
          goto size_overflow_2;
        }
        if (CB_TREE_CATEGORY(src) == CB_CATEGORY_NUMERIC_EDITED &&
            cb_field(src)->size > dst_size_mod) {
          goto size_overflow_1;
        }
        break;
      case CB_CATEGORY_NATIONAL_EDITED:
      case CB_CATEGORY_NATIONAL:
        if (cb_field(src)->pic->scale > 0) {
          goto invalid;
        }
      default:
        src_scale_mod =
            cb_field(src)->pic->scale < 0 ? 0 : cb_field(src)->pic->scale;
        dst_scale_mod =
            cb_field(dst)->pic->scale < 0 ? 0 : cb_field(dst)->pic->scale;
        if (cb_field(src)->pic->digits - src_scale_mod >
                cb_field(dst)->pic->digits - dst_scale_mod ||
            src_scale_mod > dst_scale_mod) {
          goto size_overflow_2;
        }
        break;
      }
      break;
    case CB_CATEGORY_NUMERIC_EDITED:
      switch (CB_TREE_CATEGORY(dst)) {
      case CB_CATEGORY_ALPHABETIC:
        goto invalid;
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
        is_numeric_edited = 1;
        /* Drop through */
      case CB_CATEGORY_ALPHANUMERIC:
        if (is_numeric_edited) {
          dst_size_mod = count_pic_alphanumeric_edited(cb_field(dst));
        } else {
          dst_size_mod = cb_field(dst)->size;
        }
        if (CB_TREE_CATEGORY(src) == CB_CATEGORY_NUMERIC &&
            cb_field(src)->pic->scale > 0) {
          if (cb_move_noninteger_to_alphanumeric == CB_ERROR) {
            goto invalid;
          }
          cb_warning_x(loc, _("Move non-integer to alphanumeric"));
          break;
        }
        if (CB_TREE_CATEGORY(src) == CB_CATEGORY_NUMERIC &&
            cb_field(src)->pic->digits > dst_size_mod) {
          goto size_overflow_2;
        }
        if (CB_TREE_CATEGORY(src) == CB_CATEGORY_NUMERIC_EDITED &&
            cb_field(src)->size > dst_size_mod) {
          goto size_overflow_1;
        }
        break;
      default:
        src_scale_mod =
            cb_field(src)->pic->scale < 0 ? 0 : cb_field(src)->pic->scale;
        dst_scale_mod =
            cb_field(dst)->pic->scale < 0 ? 0 : cb_field(dst)->pic->scale;
        if (cb_field(src)->pic->digits - src_scale_mod >
                cb_field(dst)->pic->digits - dst_scale_mod ||
            src_scale_mod > dst_scale_mod) {
          goto size_overflow_2;
        }
        break;
      }
      break;
    default:
      cb_error_x(loc, _("Invalid source for MOVE"));
      return -1;
    }
    break;
  case CB_TAG_INTEGER:
  case CB_TAG_BINARY_OP:
  case CB_TAG_INTRINSIC:
    /* TODO: check this */
    break;
  default:
    fprintf(stderr, "Invalid tree tag %d\n", CB_TREE_TAG(src));
    goto invalid; /* don't ABORT (), continue parsing */
  }
  return 0;

invalid:
  if (is_value) {
    cb_error_x(loc, _("Invalid VALUE clause"));
  } else {
    cb_error_x(loc, _("Invalid MOVE statement"));
    return 0;
  }
  return -1;

numlit_overflow:
  if (is_value) {
    cb_error_x(loc, _("Invalid VALUE clause - literal exceeds data size"));
    return -1;
  }
  if (cb_warn_constant) {
    cb_warning_x(loc, _("Numeric literal exceeds data size"));
  }
  return 0;

expect_numeric:
  if (cb_enable_expect_numeric_error) {
    return move_error2(src, dst, is_value, 1, 0,
                       _("Numeric value is expected"));
  }

  return move_error(src, dst, is_value, cb_warn_strict_typing, 0,
                    _("Numeric value is expected"));

expect_alphanumeric:
  return move_error(src, dst, is_value, cb_warn_strict_typing, 0,
                    _("Alphanumeric value is expected"));

expect_national:
  return move_error(src, dst, is_value, cb_warn_strict_typing, 0,
                    _("National value is expected"));

value_mismatch:
  return move_error(src, dst, is_value, cb_warn_constant, 0,
                    _("Value does not fit the picture string"));

size_overflow:
  return move_error(src, dst, is_value, cb_warn_constant, 0,
                    _("Value size exceeds data size"));

size_overflow_1:
  return move_error(src, dst, is_value, cb_warn_truncate, 1,
                    _("Sending field larger than receiving field"));

size_overflow_2:
  return move_error(src, dst, is_value, cb_warn_truncate, 1,
                    _("Some digits may be truncated"));

#ifdef I18N_UTF8
invalid_national:
  return move_error(src, dst, is_value, cb_warn_constant, 1,
                    _("Invalid NATIONAL string."));
#endif /*I18N_UTF8*/
}

static cb_tree cb_build_memset(cb_tree x, int c) {
  int size = cb_field_size(x);

  if (cb_field(x)->pic) {
    if (cb_field(x)->pic->national == 1) {
      return cb_build_funcall_2("cob_la_memset", x, cb_int(c));
    }
  }
  if (size == 1) {
    return cb_build_funcall_2("$E", x, cb_int(c));
  } else {
    return cb_build_method_call_3("fillBytes", cb_build_cast_address(x),
                                  cb_int(c), cb_build_cast_length(x));
  }
}

static cb_tree cb_build_move_copy(cb_tree src, cb_tree dst) {
  int size = cb_field_size(dst);

  if (size == 1) {
    return cb_build_funcall_2("$F", dst, src);
  } else {
    return cb_build_method_call_3("setBytes", cb_build_cast_address(dst),
                                  cb_build_cast_address(src),
                                  cb_build_cast_length(dst));
  }
}

static cb_tree cb_build_move_call(cb_tree src, cb_tree dst) {
  return cb_build_method_call_2("moveFrom", dst, src);
}

static cb_tree cb_build_move_num_zero(cb_tree x) {
  struct cb_field *f;

  f = cb_field(x);
  switch (f->usage) {
  case CB_USAGE_BINARY:
  case CB_USAGE_COMP_5:
  case CB_USAGE_COMP_X:
    if (f->flag_binary_swap) {
      return cb_build_memset(x, 0);
    }
    switch (f->size) {
#ifdef COB_NON_ALIGNED
    case 1:
      return cb_build_assign(x, cb_int0);
    case 2:
#ifdef COB_SHORT_BORK
      if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
          (f->offset % 4 == 0)) {
        return cb_build_assign(x, cb_int0);
      }
      break;
#endif
    case 4:
    case 8:
      if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
          (f->offset % f->size == 0)) {
        return cb_build_assign(x, cb_int0);
      }
      break;
#else
    case 1:
    case 2:
    case 4:
    case 8:
      return cb_build_assign(x, cb_int0);
#endif
    }
    return cb_build_memset(x, 0);
  case CB_USAGE_DISPLAY:
    if (f->flag_sign_separate) {
      return cb_build_move_call(cb_zero, x);
    } else if (cb_display_sign == COB_DISPLAY_SIGN_EBCDIC &&
               f->pic->have_sign) {
      return cb_build_move_call(cb_zero, x);
    } else {
      return cb_build_memset(x, '0');
    }
  case CB_USAGE_PACKED:
    return cb_build_method_call_1("setZero", x);
  default:
    return cb_build_move_call(cb_zero, x);
  }
}

static cb_tree cb_build_move_space(cb_tree x) {
  switch (CB_TREE_CATEGORY(x)) {
  case CB_CATEGORY_NUMERIC:
  case CB_CATEGORY_ALPHABETIC:
  case CB_CATEGORY_ALPHANUMERIC:
    return cb_build_memset(x, ' ');
  default:
    return cb_build_move_call(cb_space, x);
  }
}

static cb_tree cb_build_move_blank(cb_tree x) {
  switch (CB_TREE_CATEGORY(x)) {
  case CB_CATEGORY_NUMERIC:
  case CB_CATEGORY_ALPHABETIC:
  case CB_CATEGORY_ALPHANUMERIC:
    return cb_build_memset(x, ' ');
  default:
    return cb_build_move_call(cb_blank, x);
  }
}

static cb_tree cb_build_move_zero(cb_tree x) {
  switch (CB_TREE_CATEGORY(x)) {
  case CB_CATEGORY_NUMERIC:
    if (cb_field(x)->flag_blank_zero) {
      return cb_build_move_space(x);
    } else {
      return cb_build_move_num_zero(x);
    }
  case CB_CATEGORY_ALPHABETIC:
  case CB_CATEGORY_ALPHANUMERIC:
    return cb_build_memset(x, '0');
  default:
    return cb_build_move_call(cb_zero, x);
  }
}

static cb_tree cb_build_move_high(cb_tree x) {
  switch (CB_TREE_CATEGORY(x)) {
  case CB_CATEGORY_NUMERIC:
  case CB_CATEGORY_ALPHABETIC:
  case CB_CATEGORY_ALPHANUMERIC:
    if (cb_high == cb_norm_high) {
      return cb_build_memset(x, 255);
    } else {
      return cb_build_move_call(cb_high, x);
    }
  default:
    return cb_build_move_call(cb_high, x);
  }
}

static cb_tree cb_build_move_low(cb_tree x) {
  switch (CB_TREE_CATEGORY(x)) {
  case CB_CATEGORY_NUMERIC:
  case CB_CATEGORY_ALPHABETIC:
  case CB_CATEGORY_ALPHANUMERIC:
    if (cb_low == cb_norm_low) {
      return cb_build_memset(x, 0);
    } else {
      return cb_build_move_call(cb_low, x);
    }
  default:
    return cb_build_move_call(cb_low, x);
  }
}

static cb_tree cb_build_move_quote(cb_tree x) {
  switch (CB_TREE_CATEGORY(x)) {
  case CB_CATEGORY_NUMERIC:
  case CB_CATEGORY_ALPHABETIC:
  case CB_CATEGORY_ALPHANUMERIC:
    return cb_build_memset(x, '"');
  default:
    return cb_build_move_call(cb_quote, x);
  }
}

#ifdef COB_EBCDIC_MACHINE
static void cob_put_sign_ascii(unsigned char *p) {
  switch (*p) {
  case '0':
    *p = (unsigned char)'p';
    return;
  case '1':
    *p = (unsigned char)'q';
    return;
  case '2':
    *p = (unsigned char)'r';
    return;
  case '3':
    *p = (unsigned char)'s';
    return;
  case '4':
    *p = (unsigned char)'t';
    return;
  case '5':
    *p = (unsigned char)'u';
    return;
  case '6':
    *p = (unsigned char)'v';
    return;
  case '7':
    *p = (unsigned char)'w';
    return;
  case '8':
    *p = (unsigned char)'x';
    return;
  case '9':
    *p = (unsigned char)'y';
    return;
  }
}
#endif

static void cob_put_sign_ebcdic(unsigned char *p, const int sign) {
  if (sign < 0) {
    switch (*p) {
    case '0':
      *p = (unsigned char)'}';
      return;
    case '1':
      *p = (unsigned char)'J';
      return;
    case '2':
      *p = (unsigned char)'K';
      return;
    case '3':
      *p = (unsigned char)'L';
      return;
    case '4':
      *p = (unsigned char)'M';
      return;
    case '5':
      *p = (unsigned char)'N';
      return;
    case '6':
      *p = (unsigned char)'O';
      return;
    case '7':
      *p = (unsigned char)'P';
      return;
    case '8':
      *p = (unsigned char)'Q';
      return;
    case '9':
      *p = (unsigned char)'R';
      return;
    default:
      /* What to do here */
      *p = (unsigned char)'}';
      return;
    }
  }
  switch (*p) {
  case '0':
    *p = (unsigned char)'{';
    return;
  case '1':
    *p = (unsigned char)'A';
    return;
  case '2':
    *p = (unsigned char)'B';
    return;
  case '3':
    *p = (unsigned char)'C';
    return;
  case '4':
    *p = (unsigned char)'D';
    return;
  case '5':
    *p = (unsigned char)'E';
    return;
  case '6':
    *p = (unsigned char)'F';
    return;
  case '7':
    *p = (unsigned char)'G';
    return;
  case '8':
    *p = (unsigned char)'H';
    return;
  case '9':
    *p = (unsigned char)'I';
    return;
  default:
    /* What to do here */
    *p = (unsigned char)'{';
    return;
  }
  /* NOT REACHED */
}

static cb_tree cb_build_move_literal(cb_tree src, cb_tree dst) {
  struct cb_literal *l;
  struct cb_field *f;
  unsigned char *buff;
  unsigned char *p;
  enum cb_category cat;
  int i;
  int diff;
  int val;
  int n;
  unsigned char bbyte;

  l = CB_LITERAL(src);
  f = cb_field(dst);
  cat = CB_TREE_CATEGORY(dst);

  if (l->all) {
    if (cat == CB_CATEGORY_NUMERIC || cat == CB_CATEGORY_NUMERIC_EDITED) {
      return cb_build_move_call(src, dst);
    } else if (cat == CB_CATEGORY_NATIONAL_EDITED ||
               cat == CB_CATEGORY_NATIONAL) {
      return cb_build_move_call(src, dst);
    } else if (cat == CB_CATEGORY_ALPHANUMERIC ||
               cat == CB_CATEGORY_ALPHANUMERIC_EDITED) {
      return cb_build_move_call(src, dst);
    }
    if (l->size == 1) {
      return cb_build_method_call_3("fillBytes", cb_build_cast_address(dst),
                                    cb_int(l->data[0]),
                                    cb_build_cast_length(dst));
    }
    bbyte = l->data[0];
    for (i = 0; i < (int)l->size; i++) {
      if (bbyte != l->data[i]) {
        break;
      }
      bbyte = l->data[i];
    }
    if (i == (int)l->size) {
      return cb_build_method_call_3("fillBytes", cb_build_cast_address(dst),
                                    cb_int(l->data[0]),
                                    cb_build_cast_length(dst));
    }
    if (f->size > 128) {
      return cb_build_move_call(src, dst);
    }
    buff = cobc_malloc((size_t)f->size);
    for (i = 0; i < f->size; i++) {
      buff[i] = l->data[i % l->size];
    }
#ifdef I18N_UTF8
    /* I18N_UTF8: termination of multi octet
       charactrer sequence is pending. */
#else  /*!I18N_UTF8*/
    if ((0x81 <= buff[i - 1] && buff[i - 1] <= 0x9F) ||
        (0xE0 <= buff[i - 1] && buff[i - 1] <= 0xFC)) {
      buff[i - 1] = ' ';
    }
#endif /*I18N_UTF8*/
    return cb_build_method_call_3("setBytes", cb_build_cast_address(dst),
                                  cb_build_string(buff, f->size),
                                  cb_build_cast_length(dst));
  } else if ((cat == CB_CATEGORY_NUMERIC && f->usage == CB_USAGE_DISPLAY &&
              f->pic->scale == l->scale && !f->flag_sign_leading &&
              !f->flag_sign_separate) ||
             ((cat == CB_CATEGORY_ALPHABETIC ||
               cat == CB_CATEGORY_ALPHANUMERIC) &&
              f->size < (int)(l->size + 16) && !cb_field_variable_size(f))) {
    buff = cobc_malloc((size_t)f->size);
    diff = (int)(f->size - l->size);
    if (cat == CB_CATEGORY_NUMERIC) {
      if (diff <= 0) {
        memcpy(buff, l->data - diff, (size_t)f->size);
      } else {
        memset(buff, '0', (size_t)diff);
        memcpy(buff + diff, l->data, (size_t)l->size);
      }
      if (f->pic->have_sign) {
        p = &buff[f->size - 1];
        if (cb_display_sign) {
          cob_put_sign_ebcdic(p, l->sign);
        } else if (l->sign < 0) {
#ifdef COB_EBCDIC_MACHINE
          cob_put_sign_ascii(p);
#else
          *p += 0x40;
#endif
        }
      }
    } else {
      if (f->flag_justified) {
        if (diff <= 0) {
          memcpy(buff, l->data - diff, (size_t)f->size);
        } else {
          memset(buff, ' ', (size_t)diff);
          memcpy(buff + diff, l->data, (size_t)l->size);
        }
      } else {
        if (diff <= 0) {
          memcpy(buff, l->data, (size_t)f->size);
        } else {
          memcpy(buff, l->data, (size_t)l->size);
          memset(buff + l->size, ' ', (size_t)diff);
        }
      }
    }
    bbyte = *buff;
    if (f->size == 1) {
      free(buff);
      return cb_build_funcall_2("$E", dst, cb_int(bbyte));
    }
    for (i = 0; i < f->size; i++) {
      if (bbyte != buff[i]) {
        break;
      }
    }
    if (i == f->size) {
      free(buff);
      return cb_build_method_call_3("fillBytes", cb_build_cast_address(dst),
                                    cb_int(bbyte), cb_build_cast_length(dst));
    }
    return cb_build_method_call_3("setBytes", cb_build_cast_address(dst),
                                  cb_build_string(buff, f->size),
                                  cb_build_cast_length(dst));
  } else if (cb_fits_int(src) && f->size <= 8 &&
             (f->usage == CB_USAGE_BINARY || f->usage == CB_USAGE_COMP_5 ||
              f->usage == CB_USAGE_COMP_X)) {
    val = cb_get_int(src);
    n = f->pic->scale - l->scale;
    if ((l->size + n) > 9) {
      return cb_build_move_call(src, dst);
    }
    for (; n > 0; n--) {
      val *= 10;
    }
    for (; n < 0; n++) {
      val /= 10;
    }
    if (val == 0) {
      return cb_build_move_num_zero(dst);
    }
    if (f->size == 1) {
      return cb_build_assign(dst, cb_int(val));
    }
    if (f->flag_binary_swap) {
      i = (f->size - 1) + (8 * (f->pic->have_sign ? 1 : 0));
      return cb_build_method_call_2(bin_set_funcs[i],
                                    cb_build_cast_address(dst), cb_int(val));
    }
    switch (f->size) {
    case 2:
#ifdef COB_SHORT_BORK
      if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
          (f->offset % 4 == 0)) {
        return cb_build_assign(dst, cb_int(val));
      }
      break;
#endif
    case 4:
    case 8:
#ifdef COB_NON_ALIGNED
      if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
          (f->offset % f->size == 0)) {
        return cb_build_assign(dst, cb_int(val));
      }
      break;
#else
      return cb_build_assign(dst, cb_int(val));
#endif
    }
    return cb_build_move_call(src, dst);
  } else if (cb_fits_int(src) && f->usage == CB_USAGE_PACKED) {
    if (f->pic->scale < 0) {
      return cb_build_move_call(src, dst);
    }
    val = cb_get_int(src);
    n = f->pic->scale - l->scale;
    if ((l->size + n) > 9) {
      return cb_build_move_call(src, dst);
    }
    for (; n > 0; n--) {
      val *= 10;
    }
    for (; n < 0; n++) {
      val /= 10;
    }
    if (val == 0) {
      return cb_build_move_num_zero(dst);
    }
    return cb_build_method_call_2("moveFrom", dst, cb_int(val));
  } else {
    return cb_build_move_call(src, dst);
  }
}

static cb_tree cb_build_move_field(cb_tree src, cb_tree dst) {
  struct cb_field *src_f;
  struct cb_field *dst_f;
  int src_size;
  int dst_size;

  src_f = cb_field(src);
  src_size = cb_field_size(src);
  dst_f = cb_field(dst);
  dst_size = cb_field_size(dst);

  if ((src_size > 0 && dst_size > 0 && src_size >= dst_size) &&
      (!cb_field_variable_size(src_f) && !cb_field_variable_size(dst_f))) {
    switch (CB_TREE_CATEGORY(src)) {
    case CB_CATEGORY_ALPHABETIC:
      if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_ALPHABETIC ||
          CB_TREE_CATEGORY(dst) == CB_CATEGORY_ALPHANUMERIC) {
        if (dst_f->flag_justified == 0) {
          return cb_build_move_copy(src, dst);
        }
      }
      break;
    case CB_CATEGORY_ALPHANUMERIC:
      if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_ALPHANUMERIC) {
        if (dst_f->flag_justified == 0) {
          return cb_build_move_copy(src, dst);
        }
      }
      break;
    case CB_CATEGORY_NUMERIC:
      if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_NUMERIC &&
          src_f->usage == dst_f->usage &&
          src_f->pic->size == dst_f->pic->size &&
          src_f->pic->digits == dst_f->pic->digits &&
          src_f->pic->scale == dst_f->pic->scale &&
          src_f->pic->have_sign == dst_f->pic->have_sign &&
          src_f->flag_binary_swap == dst_f->flag_binary_swap &&
          src_f->flag_sign_leading == dst_f->flag_sign_leading &&
          src_f->flag_sign_separate == dst_f->flag_sign_separate) {
        return cb_build_move_copy(src, dst);
      } else if (CB_TREE_CATEGORY(dst) == CB_CATEGORY_ALPHANUMERIC &&
                 src_f->usage == CB_USAGE_DISPLAY &&
                 src_f->pic->have_sign == 0 && !src_f->flag_sign_leading &&
                 !src_f->flag_sign_separate) {
        return cb_build_move_copy(src, dst);
      }
      break;
    default:
      break;
    }
  }

  return cb_build_move_call(src, dst);
}

cb_tree cb_build_move(cb_tree src, cb_tree dst) {
  struct cb_field *f;
  struct cb_field *p;

  if (src == cb_error_node || dst == cb_error_node) {
    return cb_error_node;
  }

  if (validate_move(src, dst, 0) < 0) {
    return cb_error_node;
  }

  if (CB_REFERENCE_P(src)) {
    CB_REFERENCE(src)->type = CB_SENDING_OPERAND;
  }
  if (CB_REFERENCE_P(dst)) {
    CB_REFERENCE(dst)->type = CB_RECEIVING_OPERAND;
  }

  if (CB_TREE_CLASS(dst) == CB_CLASS_POINTER) {
    return cb_build_assign(dst, src);
  }

  if (CB_REFERENCE_P(src) && CB_ALPHABET_NAME_P(CB_REFERENCE(src)->value)) {
    return cb_build_move_call(src, dst);
  }
  if (CB_INDEX_P(dst)) {
    if (src == cb_null) {
      return cb_build_assign(dst, cb_zero);
    }
    return cb_build_assign(dst, src);
  }

  if (CB_INDEX_P(src)) {
    return cb_build_method_call_2("setInt", dst, cb_build_cast_integer(src));
  }

  if (CB_INTRINSIC_P(src) || CB_INTRINSIC_P(dst)) {
    return cb_build_move_call(src, dst);
  }

  f = cb_field(dst);

  if (CB_EXCEPTION_ENABLE(COB_EC_BOUND_SUBSCRIPT)) {
    for (p = f; p; p = p->parent) {
      if (p->flag_occurs) {
        return cb_build_move_call(src, dst);
      }
    }
    if (CB_REF_OR_FIELD_P(src)) {
      for (p = cb_field(src); p; p = p->parent) {
        if (p->flag_occurs) {
          return cb_build_move_call(src, dst);
        }
      }
    }
  }

  if (CB_EXCEPTION_ENABLE(COB_EC_BOUND_REF_MOD)) {
    if ((CB_REFERENCE_P(src) && CB_REFERENCE(src)->offset != NULL) ||
        (CB_REFERENCE_P(dst) && CB_REFERENCE(dst)->offset != NULL)) {
      return cb_build_move_call(src, dst);
    }
  }

  /* output optimal code */
  if (src == cb_zero) {
    return cb_build_move_zero(dst);
  } else if (src == cb_space) {
    return cb_build_move_space(dst);
  } else if (src == cb_blank) {
    return cb_build_move_blank(dst);
  } else if (src == cb_high) {
    return cb_build_move_high(dst);
  } else if (src == cb_low) {
    return cb_build_move_low(dst);
  } else if (src == cb_quote) {
    return cb_build_move_quote(dst);
  } else if (CB_LITERAL_P(src)) {
    return cb_build_move_literal(src, dst);
  }
  return cb_build_move_field(src, dst);
}

void cb_emit_move(cb_tree src, cb_tree dsts) {
  cb_tree l;

  if (cb_validate_one(src)) {
    return;
  }
  if (cb_validate_list(dsts)) {
    return;
  }

  for (l = dsts; l; l = CB_CHAIN(l)) {
    if (cb_enable_expect_numeric_error) {
      if (CB_TREE_TAG(src) == CB_TAG_REFERENCE) {
        cb_emit(cb_build_method_call_2("checkMoveStrNum", src, CB_VALUE(l)));
      }
    }
    cb_emit(cb_build_move(src, CB_VALUE(l)));
  }
}

/*
 * OPEN statement
 */

void cb_emit_open(cb_tree file, cb_tree mode, cb_tree sharing) {
  if (file == cb_error_node) {
    return;
  }
  file = cb_ref(file);
  if (file == cb_error_node) {
    return;
  }
  current_statement->file = file;

  if (CB_FILE(file)->organization == COB_ORG_SORT) {
    cb_error_x(CB_TREE(current_statement),
               _("Operation not allowed on SORT files"));
  }
  if (sharing == NULL) {
    sharing = CB_FILE(file)->sharing ? CB_FILE(file)->sharing : cb_int0;
  }

  /* READ ONLY */
  if (sharing == cb_int0 && CB_INTEGER(mode)->val != COB_OPEN_INPUT) {
    sharing = cb_int1;
  }

  cb_emit(cb_build_method_call_4("open", file, mode, sharing,
                                 CB_FILE(file)->file_status));
}

/*
 * PERFORM statement
 */

void cb_emit_perform(cb_tree perform, cb_tree body) {
  if (perform == cb_error_node) {
    return;
  }
  CB_PERFORM(perform)->body = body;
  cb_emit(perform);
}

cb_tree cb_build_perform_once(cb_tree body) {
  cb_tree x;

  if (body == cb_error_node) {
    return cb_error_node;
  }
  x = cb_build_perform(CB_PERFORM_ONCE);
  CB_PERFORM(x)->body = body;
  return x;
}

cb_tree cb_build_perform_times(cb_tree times) {
  cb_tree x;

  if (cb_check_integer_value(times) == cb_error_node) {
    return cb_error_node;
  }

  x = cb_build_perform(CB_PERFORM_TIMES);
  CB_PERFORM(x)->data = times;
  return x;
}

cb_tree cb_build_perform_until(cb_tree condition, cb_tree varying) {
  cb_tree x;

  x = cb_build_perform(CB_PERFORM_UNTIL);
  CB_PERFORM(x)->test = condition;
  CB_PERFORM(x)->varying = varying;
  return x;
}

cb_tree cb_build_perform_forever(cb_tree body) {
  cb_tree x;

  if (body == cb_error_node) {
    return cb_error_node;
  }
  x = cb_build_perform(CB_PERFORM_FOREVER);
  CB_PERFORM(x)->body = body;
  return x;
}

cb_tree cb_build_perform_exit(struct cb_label *label) {
  cb_tree x;

  x = cb_build_perform(CB_PERFORM_EXIT);
  CB_PERFORM(x)->data = CB_TREE(label);
  return x;
}

/*
 * READ statement
 */

static int match_compound_key(struct cb_key_component *pkcomp,
                              struct cb_list *keys) {
  struct cb_field *pfld;
  struct cb_reference *pref;

  while (pkcomp && keys) {
    pfld = CB_FIELD(CB_REFERENCE(keys->value)->value);
    pref = CB_REFERENCE(pkcomp->component);
    if (pfld != CB_FIELD(pref->value)) {
      break;
    }
    pkcomp = pkcomp->next;
    keys = (keys->chain) ? CB_LIST(keys->chain) : NULL;
  }
  return (!pkcomp && !keys);
}

static cb_tree lookup_compound_key(struct cb_file *f, struct cb_list *keys) {
  struct cb_alt_key *paltkey;
  cb_tree key = NULL;

  if (match_compound_key(f->component_list, keys)) {
    key = f->key;
  } else {
    for (paltkey = f->alt_key_list; paltkey; paltkey = paltkey->next) {
      if (match_compound_key(paltkey->component_list, keys)) {
        key = paltkey->key;
        break;
      }
    }
  }
  return key;
}

void cb_emit_read(cb_tree ref, cb_tree next, cb_tree into, cb_tree keys,
                  cb_tree lock_opts) {
  int read_opts = 0;
  cb_tree file;
  cb_tree rec;
  cb_tree key = NULL;

  if (lock_opts == cb_int1) {
    read_opts = COB_READ_LOCK;
  } else if (lock_opts == cb_int2) {
    read_opts = COB_READ_NO_LOCK;
  } else if (lock_opts == cb_int3) {
    read_opts = COB_READ_IGNORE_LOCK;
  } else if (lock_opts == cb_int4) {
    read_opts = COB_READ_WAIT_LOCK;
  }
  if (ref == cb_error_node) {
    return;
  }
  file = cb_ref(ref);
  if (file == cb_error_node) {
    return;
  }
  rec = cb_build_field_reference(CB_FILE(file)->record, ref);
  if (CB_FILE(file)->organization == COB_ORG_SORT) {
    cb_error_x(CB_TREE(current_statement),
               _("Operation not allowed on SORT files"));
  }
  if (next == cb_int1 || next == cb_int2 ||
      CB_FILE(file)->access_mode == COB_ACCESS_SEQUENTIAL) {
    /* READ NEXT/PREVIOUS */
    if (next == cb_int2) {
      if (CB_FILE(file)->organization != COB_ORG_INDEXED) {
        cb_error_x(
            CB_TREE(current_statement),
            _("READ PREVIOUS only allowed for INDEXED SEQUENTIAL files"));
      }
      read_opts |= COB_READ_PREVIOUS;
    } else {
      read_opts |= COB_READ_NEXT;
    }
    if (keys) {
      cb_warning(_("KEY ignored with sequential READ"));
    }
    cb_emit(cb_build_method_call_4(
        "read", file, cb_int0, CB_FILE(file)->file_status, cb_int(read_opts)));
  } else {
    if (keys) {
      if (CB_LIST(keys)->chain != NULL) {
        key = lookup_compound_key(CB_FILE(file), CB_LIST(keys));
      } else {
        key = CB_LIST(keys)->value;
      }
      if (!key) {
        cb_error_x(CB_TREE(current_statement), _("Undefined compound keys"));
        return;
      }
    }
    /* READ */
    cb_emit(cb_build_method_call_4("read", file, key ? key : CB_FILE(file)->key,
                                   CB_FILE(file)->file_status,
                                   cb_int(read_opts)));
  }
  if (into) {
    current_statement->handler3 = cb_build_move(rec, into);
  }
  current_statement->file = file;
}

/*
 * REWRITE statement
 */

void cb_emit_rewrite(cb_tree record, cb_tree from, cb_tree lockopt) {
  cb_tree file;
  int opts = 0;

  if (record == cb_error_node || cb_ref(record) == cb_error_node) {
    return;
  }
  if (!CB_REF_OR_FIELD_P(cb_ref(record))) {
    cb_error_x(CB_TREE(current_statement),
               _("REWRITE requires a record name as subject"));
    return;
  }
  if (cb_field(record)->storage != CB_STORAGE_FILE) {
    cb_error_x(CB_TREE(current_statement),
               _("REWRITE subject does not refer to a record name"));
    return;
  }
  file = CB_TREE(CB_FIELD(cb_ref(record))->file);
  current_statement->file = file;
  if (CB_FILE(file)->organization == COB_ORG_SORT) {
    cb_error_x(CB_TREE(current_statement),
               _("Operation not allowed on SORT files"));
  } else if (current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
             (CB_FILE(file)->organization != COB_ORG_RELATIVE &&
              CB_FILE(file)->organization != COB_ORG_INDEXED)) {
    cb_error_x(CB_TREE(current_statement),
               _("INVALID KEY clause invalid with this file type"));
  } else if ((CB_FILE(file)->lock_mode & COB_LOCK_AUTOMATIC) && lockopt) {
    cb_error_x(CB_TREE(current_statement),
               _("LOCK clause invalid with file LOCK AUTOMATIC"));
  } else if (lockopt == cb_int1) {
    opts = COB_WRITE_LOCK;
  }
  if (from) {
    cb_emit(cb_build_move(from, record));
  }
  cb_emit(cb_build_method_call_4("rewrite", file, record, cb_int(opts),
                                 CB_FILE(file)->file_status));
}

/*
 * RELEASE statement
 */

void cb_emit_release(cb_tree record, cb_tree from) {
  struct cb_field *f;
  cb_tree file;

  if (record == cb_error_node) {
    return;
  }
  if (from == cb_error_node) {
    return;
  }
  if (cb_ref(record) == cb_error_node) {
    return;
  }
  if (!CB_REF_OR_FIELD_P(cb_ref(record))) {
    cb_error_x(CB_TREE(current_statement),
               _("RELEASE requires a record name as subject"));
    return;
  }
  if (cb_field(record)->storage != CB_STORAGE_FILE) {
    cb_error_x(CB_TREE(current_statement),
               _("RELEASE subject does not refer to a record name"));
    return;
  }
  f = CB_FIELD(cb_ref(record));
  file = CB_TREE(f->file);
  if (CB_FILE(file)->organization != COB_ORG_SORT) {
    cb_error_x(CB_TREE(current_statement),
               _("RELEASE not allowed on this record item"));
    return;
  }
  current_statement->file = file;
  if (from) {
    cb_emit(cb_build_move(from, record));
  }
  cb_emit(cb_build_release(
      file, cb_build_cast_address(current_program->cb_sort_return)));
}

/*
 * RETURN statement
 */

void cb_emit_return(cb_tree ref, cb_tree into) {
  cb_tree file;
  cb_tree rec;

  if (ref == cb_error_node) {
    return;
  }
  if (into == cb_error_node) {
    return;
  }
  file = cb_ref(ref);
  if (file == cb_error_node) {
    return;
  }
  rec = cb_build_field_reference(CB_FILE(file)->record, ref);
  cb_emit(cb_build_return(
      file, cb_build_cast_address(current_program->cb_sort_return)));
  if (into) {
    current_statement->handler3 = cb_build_move(rec, into);
  }
  current_statement->file = file;
}

/*
 * ROLLBACK statement
 */

void cb_emit_rollback(void) {
  cb_emit(cb_build_funcall_0("CobolFile.rollback"));
}

/*
 * SEARCH statement
 */

static void search_set_keys(struct cb_field *f, cb_tree x) {
  struct cb_binary_op *p;
  int i;

  if (CB_REFERENCE_P(x)) {
    x = build_cond_88(x);
  }

  p = CB_BINARY_OP(x);
  switch (p->op) {
  case '&':
    search_set_keys(f, p->x);
    search_set_keys(f, p->y);
    break;
  case '=':
    for (i = 0; i < f->nkeys; i++) {
      if (cb_field(p->x) == cb_field(f->keys[i].key)) {
        f->keys[i].ref = p->x;
        f->keys[i].val = p->y;
        break;
      }
    }
    if (cb_allow_search_key_in_rhs) {
      /* relaxed syntax: try to find key in RHS to accept
       * also L<->R reversed conditional expresssion.
       */
      if (i == f->nkeys && CB_REFERENCE_P(p->y) && CB_FIELD_P(cb_ref(p->y))) {
        for (i = 0; i < f->nkeys; i++) {
          if (cb_field(p->y) == cb_field(f->keys[i].key)) {
            f->keys[i].ref = p->y;
            f->keys[i].val = p->x;
            break;
          }
        }
      }
    }
    if (i == f->nkeys) {
      cb_error_x(x, _("Undeclared key '%s'"), cb_field(p->x)->name);
    }
    break;
  default:
    cb_error_x(x, _("Invalid SEARCH ALL condition"));
    break;
  }
}

static cb_tree cb_build_search_all(cb_tree table, cb_tree cond) {
  cb_tree c1 = NULL;
  cb_tree c2;
  struct cb_field *f;
  int i;

  f = cb_field(table);
  /* set keys */
  for (i = 0; i < f->nkeys; i++) {
    f->keys[i].ref = NULL;
  }
  search_set_keys(f, cond);

  /* build condition */
  for (i = 0; i < f->nkeys; i++) {
    if (f->keys[i].ref) {
      if (f->keys[i].dir == COB_ASCENDING) {
        c2 = cb_build_binary_op(f->keys[i].ref, '=', f->keys[i].val);
      } else {
        c2 = cb_build_binary_op(f->keys[i].val, '=', f->keys[i].ref);
      }
      if (c1 == NULL) {
        c1 = c2;
      } else {
        c1 = cb_build_binary_op(c1, '&', c2);
      }
    }
  }

  return cb_build_cond(c1);
}

void cb_emit_search(cb_tree table, cb_tree varying, cb_tree at_end,
                    cb_tree whens) {
  if (cb_validate_one(table)) {
    return;
  }
  if (cb_validate_one(varying)) {
    return;
  }
  if (table == cb_error_node) {
    return;
  }
  cb_emit(cb_build_search(0, table, varying, at_end, whens));
}

void cb_emit_search_all(cb_tree table, cb_tree at_end, cb_tree when,
                        cb_tree stmts) {
  if (cb_validate_one(table)) {
    return;
  }
  if (table == cb_error_node) {
    return;
  }
  cb_emit(cb_build_search(
      1, table, NULL, at_end,
      cb_build_if(cb_build_search_all(table, when), stmts, NULL)));
}

/*
 * SET statement
 */

void cb_emit_setenv(cb_tree x, cb_tree y) {
  cb_emit(cb_build_funcall_2("CobolUtil.setEnv", x, y));
}

void cb_emit_set_to(cb_tree vars, cb_tree x) {
  cb_tree l;
  cb_tree v;
  struct cb_cast *p;
#if 0
	enum cb_class class = CB_CLASS_UNKNOWN;
#endif

  if (cb_validate_one(x)) {
    return;
  }
  if (cb_validate_list(vars)) {
    return;
  }

#if 0
	/* determine the class of targets */
	for (l = vars; l; l = CB_CHAIN (l)) {
		if (CB_TREE_CLASS (CB_VALUE (l)) != CB_CLASS_UNKNOWN) {
			if (class == CB_CLASS_UNKNOWN) {
				class = CB_TREE_CLASS (CB_VALUE (l));
			} else if (class != CB_TREE_CLASS (CB_VALUE (l))) {
				break;
			}
		}
	}
	if (l || (class != CB_CLASS_INDEX && class != CB_CLASS_POINTER)) {
		cb_error_x (CB_TREE (current_statement),
			    _("The targets of SET must be either indexes or pointers"));
		return;
	}
#endif

  if (CB_CAST_P(x)) {
    p = CB_CAST(x);
    if (p->type == CB_CAST_PROGRAM_POINTER) {
      for (l = vars; l; l = CB_CHAIN(l)) {
        v = CB_VALUE(l);
        if (!CB_REFERENCE_P(v)) {
          cb_error_x(CB_TREE(current_statement),
                     _("SET targets must be PROGRAM-POINTER"));
          CB_VALUE(l) = cb_error_node;
        } else if (CB_FIELD(cb_ref(v))->usage != CB_USAGE_PROGRAM_POINTER) {
          cb_error_x(CB_TREE(current_statement),
                     _("SET targets must be PROGRAM-POINTER"));
          CB_VALUE(l) = cb_error_node;
        }
      }
    }
  }
  /* validate the targets */
  for (l = vars; l; l = CB_CHAIN(l)) {
    v = CB_VALUE(l);
    if (CB_CAST_P(v)) {
      p = CB_CAST(v);
      if (p->type == CB_CAST_ADDRESS &&
          !CB_FIELD(cb_ref(p->val))->flag_item_based &&
          CB_FIELD(cb_ref(p->val))->storage != CB_STORAGE_LINKAGE) {
        cb_error_x(p->val, _("The address of '%s' cannot be changed"),
                   cb_name(p->val));
        CB_VALUE(l) = cb_error_node;
      }
    }
  }
  if (cb_validate_list(vars)) {
    return;
  }

  for (l = vars; l; l = CB_CHAIN(l)) {
    cb_emit(cb_build_move(x, CB_VALUE(l)));
  }
}

void cb_emit_set_up_down(cb_tree l, cb_tree flag, cb_tree x) {
  if (cb_validate_one(x)) {
    return;
  }
  if (cb_validate_list(l)) {
    return;
  }
  for (; l; l = CB_CHAIN(l)) {
    if (flag == cb_int0) {
      cb_emit(cb_build_add(CB_VALUE(l), x, cb_int0));
    } else {
      cb_emit(cb_build_sub(CB_VALUE(l), x, cb_int0));
    }
  }
}

void cb_emit_set_on_off(cb_tree l, cb_tree flag) {
  struct cb_system_name *s;

  if (cb_validate_list(l)) {
    return;
  }
  for (; l; l = CB_CHAIN(l)) {
    s = CB_SYSTEM_NAME(cb_ref(CB_VALUE(l)));
    cb_emit(cb_build_funcall_2("CobolUtil.setSwitch", cb_int(s->token), flag));
  }
}

void cb_emit_set_true(cb_tree l) {
  cb_tree x;
  struct cb_field *f;
  cb_tree ref;
  cb_tree val;

  for (; l; l = CB_CHAIN(l)) {
    x = CB_VALUE(l);
    if (x == cb_error_node) {
      return;
    }
    if (!(CB_REFERENCE_P(x) && CB_FIELD_P(CB_REFERENCE(x)->value)) &&
        !CB_FIELD_P(x)) {
      cb_error_x(x, _("Invalid SET statement"));
      return;
    }
    f = cb_field(x);
    if (f->level != 88) {
      cb_error_x(x, _("Invalid SET statement"));
      return;
    }
    ref = cb_build_field_reference(f->parent, x);
    val = CB_VALUE(f->values);
    if (CB_PAIR_P(val)) {
      val = CB_PAIR_X(val);
    }
    cb_emit(cb_build_move(val, ref));
  }
}

void cb_emit_set_false(cb_tree l) {
  cb_tree x;
  struct cb_field *f;
  cb_tree ref;
  cb_tree val;

  for (; l; l = CB_CHAIN(l)) {
    x = CB_VALUE(l);
    if (x == cb_error_node) {
      return;
    }
    if (!(CB_REFERENCE_P(x) && CB_FIELD_P(CB_REFERENCE(x)->value)) &&
        !CB_FIELD_P(x)) {
      cb_error_x(x, _("Invalid SET statement"));
      return;
    }
    f = cb_field(x);
    if (f->level != 88) {
      cb_error_x(x, _("Invalid SET statement"));
      return;
    }
    if (!f->false_88) {
      cb_error_x(x, _("Field does not have FALSE clause"));
      return;
    }
    ref = cb_build_field_reference(f->parent, x);
    val = CB_VALUE(f->false_88);
    if (CB_PAIR_P(val)) {
      val = CB_PAIR_X(val);
    }
    cb_emit(cb_build_move(val, ref));
  }
}

/*
 * SORT statement
 */

void cb_emit_sort_init(cb_tree name, cb_tree keys, cb_tree col) {
  cb_tree l;
  struct cb_field *f;

  if (cb_validate_list(keys)) {
    return;
  }
  for (l = keys; l; l = CB_CHAIN(l)) {
    if (CB_VALUE(l) == NULL) {
      CB_VALUE(l) = name;
    }
    cb_ref(CB_VALUE(l));
  }

  if (CB_FILE_P(cb_ref(name))) {
    if (CB_FILE(cb_ref(name))->organization != COB_ORG_SORT) {
      cb_error_x(name, _("Invalid SORT filename"));
    }
    cb_field(current_program->cb_sort_return)->count++;
    /*cb_emit (cb_build_sort_init ("cob_file_sort_init", cb_ref (name),
                                     cb_int (cb_list_length (keys)), col,
                                     cb_build_cast_address
       (current_program->cb_sort_return), CB_FILE(cb_ref
       (name))->file_status));*/
    cb_emit(cb_build_funcall_5(
        "CobolFileSort.sortInit", cb_ref(name), cb_int(cb_list_length(keys)),
        col, cb_build_cast_address(current_program->cb_sort_return),
        CB_FILE(cb_ref(name))->file_status));
    for (l = keys; l; l = CB_CHAIN(l)) {
      cb_emit(cb_build_funcall_4("CobolFileSort.sortInitKey", cb_ref(name),
                                 CB_PURPOSE(l), CB_VALUE(l),
                                 cb_int(cb_field(CB_VALUE(l))->offset)));
    }
  } else {
    f = CB_FIELD(cb_ref(name));
    if (keys == NULL) {
      cb_error_x(name, _("Table sort without keys not implemented yet"));
    }
    cb_emit(cb_build_funcall_2("CobolFileSort.sortTableInit",
                               cb_int(cb_list_length(keys)), col));
    for (l = keys; l; l = CB_CHAIN(l)) {
      cb_emit(cb_build_funcall_3("CobolFileSort.sortTableInitKey",
                                 CB_PURPOSE(l), CB_VALUE(l),
                                 cb_int(cb_field(CB_VALUE(l))->offset)));
    }
    cb_emit(cb_build_funcall_2("CobolFileSort.sortTable", name,
                               (f->occurs_depending
                                    ? cb_build_cast_integer(f->occurs_depending)
                                    : cb_int(f->occurs_max))));
  }
}

void cb_emit_sort_using(cb_tree file, cb_tree l) {
  if (cb_validate_list(l)) {
    return;
  }
  for (; l; l = CB_CHAIN(l)) {
    if (CB_FILE(cb_ref(CB_VALUE(l)))->organization == COB_ORG_SORT) {
      cb_error(_("Invalid SORT USING parameter"));
    }
    cb_emit(cb_build_funcall_2("CobolFileSort.sortUsing", cb_ref(file),
                               cb_ref(CB_VALUE(l))));
  }
}

void cb_emit_sort_input(cb_tree proc, cb_tree file) {
  cb_emit(cb_build_sort_proc(
      proc, cb_ref(file),
      cb_build_cast_address(current_program->cb_sort_return)));
}

void cb_emit_sort_giving(cb_tree file, cb_tree l) {
  cb_tree p;
  int listlen;

  if (cb_validate_list(l)) {
    return;
  }
  for (p = l; p; p = CB_CHAIN(p)) {
    if (CB_FILE(cb_ref(CB_VALUE(p)))->organization == COB_ORG_SORT) {
      cb_error(_("Invalid SORT GIVING parameter"));
    }
  }
  listlen = cb_list_length(l);
  p = cb_build_funcall_2("CobolFileSort.sortGiving", cb_ref(file), l);
  CB_FUNCALL(p)->varcnt = listlen;
  cb_emit(p);
}

void cb_emit_sort_output(cb_tree proc, cb_tree file) {
  cb_emit(cb_build_sort_proc(
      proc, cb_ref(file),
      cb_build_cast_address(current_program->cb_sort_return)));
}

void cb_emit_sort_finish(cb_tree file) {
  cb_emit(cb_build_sort_finish(
      (CB_FILE_P(cb_ref(file))) ? cb_ref(file) : NULL,
      cb_build_cast_address(current_program->cb_sort_return)));
}

/*
 * START statement
 */

void cb_emit_start(cb_tree file, cb_tree op, cb_tree keys) {
  cb_tree key = NULL;

  if (cb_validate_one(keys)) {
    return;
  }
  if (keys) {
    if (CB_LIST(keys)->chain != NULL) {
      key = lookup_compound_key(CB_FILE(cb_ref(file)), CB_LIST(keys));
    } else {
      key = CB_LIST(keys)->value;
    }
    if (!key) {
      cb_error_x(CB_TREE(current_statement), _("Undefined compound keys"));
      return;
    }
  }
  if (file != cb_error_node) {
    current_statement->file = cb_ref(file);
    cb_emit(cb_build_method_call_4("start", cb_ref(file), op,
                                   key ? key : CB_FILE(cb_ref(file))->key,
                                   CB_FILE(cb_ref(file))->file_status));
  }
}

/*
 * STOP statement
 */

void cb_emit_stop_run(cb_tree x) {
  cb_emit(cb_build_funcall_1("CobolStopRunException.throwException",
                             cb_build_cast_integer(x)));
}

/*
 * STRING statement
 */

static void cb_validate_string(cb_tree items, cb_tree into) {
  cb_tree item_value;
  cb_tree item_purpose;
  cb_tree start;

  char name1[256], name2[256], name3[256];
  struct cb_field *pfield;
  struct cb_literal *pliteral;
  int size;

  start = items;
  while (start) {
    memset(name1, 0, sizeof(name1));
    memset(name2, 0, sizeof(name2));
    memset(name3, 0, sizeof(name3));

    for (item_value = start; item_value; item_value = CB_CHAIN(item_value)) {
      if (CB_VALUE(item_value) && !CB_PAIR_P(CB_VALUE(item_value))) {
        break;
      }
    }
    for (item_purpose = item_value; item_purpose;
         item_purpose = CB_CHAIN(item_purpose)) {
      if (CB_VALUE(item_purpose) && CB_PAIR_P(CB_VALUE(item_purpose))) {
        break;
      }
    }
    if (item_value) {
      switch (CB_TREE_TAG(CB_PAIR_Y(item_value))) {
      case CB_TAG_REFERENCE:
        pfield = CB_FIELD(cb_ref(CB_PAIR_Y(item_value)));
        cb_get_jisword_buff((char *)pfield->name, name1, sizeof(name1));
        break;
      case CB_TAG_LITERAL:
        pliteral = CB_LITERAL(CB_PAIR_Y(item_value));
        size = pliteral->size;
        strcpy(name1, "\'");
        if (size >= 253) {
          memcpy(name1 + 1, pliteral->data, 253);
        } else {
          memcpy(name1 + 1, pliteral->data, size);
        }
        strcat(name1, "\'");
        break;
      default:
        break;
      }
      if (item_purpose != NULL &&
          CB_PAIR_X(CB_VALUE(item_purpose)) != cb_int0 &&
          CB_PAIR_X(CB_VALUE(item_purpose)) != cb_zero &&
          CB_PAIR_X(CB_VALUE(item_purpose)) != cb_space &&
          CB_PAIR_X(CB_VALUE(item_purpose)) != cb_quote &&
          CB_PAIR_X(CB_VALUE(item_purpose)) != cb_high &&
          CB_PAIR_X(CB_VALUE(item_purpose)) != cb_low) {
        switch (CB_TREE_TAG(CB_PAIR_X(CB_VALUE(item_purpose)))) {
        case CB_TAG_REFERENCE:
          pfield = CB_FIELD(cb_ref(CB_PAIR_X(CB_VALUE(item_purpose))));
          cb_get_jisword_buff((char *)pfield->name, name2, sizeof(name2));
          break;
        case CB_TAG_LITERAL:
          pliteral = CB_LITERAL(CB_PAIR_X(CB_VALUE(item_purpose)));
          size = pliteral->size;
          strcpy(name2, "\'");
          if (size >= 253) {
            memcpy(name2 + 1, pliteral->data, 253);
          } else {
            memcpy(name2 + 1, pliteral->data, size);
          }
          strcat(name2, "\'");
          break;
        default:
          break;
        }
      }
      switch (CB_TREE_TAG(into)) {
      case CB_TAG_REFERENCE:
        pfield = CB_FIELD(cb_ref(into));
        cb_get_jisword_buff((char *)pfield->name, name3, sizeof(name3));
        break;
      case CB_TAG_LITERAL:
        pliteral = CB_LITERAL(into);
        size = pliteral->size;
        strcpy(name3, "\'");
        if (size >= 253) {
          memcpy(name3 + 1, pliteral->data, 253);
        } else {
          memcpy(name3 + 1, pliteral->data, size);
        }
        strcat(name3, "\'");
        break;
      default:
        break;
      }
      switch (CB_TREE_CATEGORY(into)) {
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
      case CB_CATEGORY_NATIONAL_EDITED:
      case CB_CATEGORY_NUMERIC_EDITED:
        switch (CB_TREE_CATEGORY(CB_PAIR_Y(item_value))) {
        case CB_CATEGORY_NUMERIC:
        case CB_CATEGORY_NUMERIC_EDITED:
          switch (CB_FIELD(cb_ref(CB_PAIR_Y(item_value)))->usage) {
          case CB_USAGE_BINARY:
          case CB_USAGE_FLOAT:
          case CB_USAGE_DOUBLE:
          case CB_USAGE_PACKED:
          case CB_USAGE_COMP_5:
            cb_warning_x(CB_PAIR_Y(item_value),
                         _("%s  must be a non-comp type!"), name1);
            break;
          default:
            break;
          }
          break;
        default:
          break;
        }
        switch (CB_TREE_CATEGORY(CB_PAIR_X(CB_VALUE(item_purpose)))) {
        case CB_CATEGORY_NUMERIC:
        case CB_CATEGORY_NUMERIC_EDITED:
          switch (CB_FIELD(cb_ref(CB_PAIR_X(CB_VALUE(item_purpose))))->usage) {
          case CB_USAGE_BINARY:
          case CB_USAGE_FLOAT:
          case CB_USAGE_DOUBLE:
          case CB_USAGE_PACKED:
          case CB_USAGE_COMP_5:
            cb_warning_x(CB_PAIR_X(CB_VALUE(item_purpose)),
                         _("%s  must be a non-comp type!"), name2);
            break;
          default:
            break;
          }
          break;
        default:
          break;
        }
        cb_error_x(into, "%s must be a non-edit type!", name3);
        break;
      case CB_CATEGORY_NUMERIC:
        switch (CB_TREE_CATEGORY(CB_PAIR_Y(item_value))) {
        case CB_CATEGORY_NUMERIC:
        case CB_CATEGORY_NUMERIC_EDITED:
          switch (CB_FIELD(cb_ref(CB_PAIR_Y(item_value)))->usage) {
          case CB_USAGE_BINARY:
          case CB_USAGE_FLOAT:
          case CB_USAGE_DOUBLE:
          case CB_USAGE_PACKED:
          case CB_USAGE_COMP_5:
            cb_warning_x(CB_PAIR_Y(item_value),
                         _("%s  must be a non-comp type!"), name1);
            break;
          default:
            break;
          }
          break;
#ifndef I18N_UTF8
        case CB_CATEGORY_NATIONAL:
        case CB_CATEGORY_NATIONAL_EDITED:
          cb_warning_x(into, _("%s and %s and %s have not same national type!"),
                       name1, name2, name3);
          break;
#endif /*I18N_UTF8*/
        default:
          break;
        }
        switch (CB_TREE_CATEGORY(CB_PAIR_X(CB_VALUE(item_purpose)))) {
        case CB_CATEGORY_NUMERIC:
        case CB_CATEGORY_NUMERIC_EDITED:
          switch (CB_FIELD(cb_ref(CB_PAIR_X(CB_VALUE(item_purpose))))->usage) {
          case CB_USAGE_BINARY:
          case CB_USAGE_FLOAT:
          case CB_USAGE_DOUBLE:
          case CB_USAGE_PACKED:
          case CB_USAGE_COMP_5:
            cb_warning_x(CB_PAIR_X(CB_VALUE(item_purpose)),
                         _("%s  must be a non-comp type!"), name2);
            break;
          default:
            break;
          }
          break;
#ifndef I18N_UTF8
        case CB_CATEGORY_NATIONAL:
        case CB_CATEGORY_NATIONAL_EDITED:
          cb_warning_x(into, _("%s and %s and %s have not same national type!"),
                       name1, name2, name3);
          break;
#endif /*I18N_UTF8*/
        default:
          break;
        }
        switch (CB_FIELD(cb_ref(into))->usage) {
        case CB_USAGE_BINARY:
        case CB_USAGE_FLOAT:
        case CB_USAGE_DOUBLE:
        case CB_USAGE_PACKED:
        case CB_USAGE_COMP_5:
          cb_warning_x(into, _("%s  must be a non-comp type!"), name3);
          break;
        default:
          break;
        }
        break;
      case CB_CATEGORY_NATIONAL:
        switch (CB_TREE_CATEGORY(CB_PAIR_Y(item_value))) {
        case CB_CATEGORY_NATIONAL:
        case CB_CATEGORY_NATIONAL_EDITED:
          if (item_purpose != NULL &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_int0 &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_zero &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_space &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_quote &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_high &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_low) {
            switch (CB_TREE_CATEGORY(CB_PAIR_X(CB_VALUE(item_purpose)))) {
            case CB_CATEGORY_NATIONAL:
            case CB_CATEGORY_NATIONAL_EDITED:
              break;
            case CB_CATEGORY_NUMERIC:
            case CB_CATEGORY_NUMERIC_EDITED:
              switch (
                  CB_FIELD(cb_ref(CB_PAIR_X(CB_VALUE(item_purpose))))->usage) {
              case CB_USAGE_BINARY:
              case CB_USAGE_FLOAT:
              case CB_USAGE_DOUBLE:
              case CB_USAGE_PACKED:
              case CB_USAGE_COMP_5:
                cb_warning_x(CB_PAIR_X(CB_VALUE(item_purpose)),
                             _("%s  must be a non-comp type!"), name2);
                break;
              default:
                break;
              }
            default:
#ifndef I18N_UTF8
              cb_warning_x(into,
                           _("%s and %s and %s have not same national type!"),
                           name1, name2, name3);
#endif /*I18N_UTF8*/
              break;
            }
          }
          break;
        case CB_CATEGORY_NUMERIC:
        case CB_CATEGORY_NUMERIC_EDITED:
          switch (CB_FIELD(cb_ref(CB_PAIR_Y(item_value)))->usage) {
          case CB_USAGE_BINARY:
          case CB_USAGE_FLOAT:
          case CB_USAGE_DOUBLE:
          case CB_USAGE_PACKED:
          case CB_USAGE_COMP_5:
            cb_warning_x(CB_PAIR_Y(item_value),
                         _("%s  must be a non-comp type!"), name1);
            break;
          default:
            break;
          }
        default:
#ifndef I18N_UTF8
          if (CB_PAIR_X(CB_VALUE(item_purpose)) == cb_zero ||
              CB_PAIR_X(CB_VALUE(item_purpose)) == cb_space ||
              CB_PAIR_X(CB_VALUE(item_purpose)) == cb_quote ||
              CB_PAIR_X(CB_VALUE(item_purpose)) == cb_high ||
              CB_PAIR_X(CB_VALUE(item_purpose)) == cb_low) {
            cb_warning_x(into, _("%s and %s have not same national type!"),
                         name1, name3);
          } else {
            cb_warning_x(into,
                         _("%s and %s and %s have not same national type!"),
                         name1, name2, name3);
          }
#endif /*I18N_UTF8*/
          break;
        }
        break;
      case CB_CATEGORY_ALPHABETIC:
      case CB_CATEGORY_ALPHANUMERIC:
        switch (CB_TREE_CATEGORY(CB_PAIR_Y(item_value))) {
        case CB_CATEGORY_ALPHABETIC:
        case CB_CATEGORY_ALPHANUMERIC:
        case CB_CATEGORY_ALPHANUMERIC_EDITED:
          if (item_purpose != NULL &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_int0 &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_zero &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_space &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_quote &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_high &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_low) {
            switch (CB_TREE_CATEGORY(CB_PAIR_X(CB_VALUE(item_purpose)))) {
            case CB_CATEGORY_ALPHABETIC:
            case CB_CATEGORY_ALPHANUMERIC:
            case CB_CATEGORY_ALPHANUMERIC_EDITED:
              break;
            case CB_CATEGORY_NUMERIC:
            case CB_CATEGORY_NUMERIC_EDITED:
              switch (
                  CB_FIELD(cb_ref(CB_PAIR_X(CB_VALUE(item_purpose))))->usage) {
              case CB_USAGE_BINARY:
              case CB_USAGE_FLOAT:
              case CB_USAGE_DOUBLE:
              case CB_USAGE_PACKED:
              case CB_USAGE_COMP_5:
                cb_warning_x(CB_PAIR_X(CB_VALUE(item_purpose)),
                             _("%s  must be a non-comp type!"), name2);
                break;
              default:
                break;
              }
              break;
            default:
#ifndef I18N_UTF8
              cb_warning_x(into,
                           _("%s and %s and %s have not same national type!"),
                           name1, name2, name3);
#endif /*I18N_UTF8*/
              break;
            }
          }
          break;
        case CB_CATEGORY_NUMERIC:
        case CB_CATEGORY_NUMERIC_EDITED:
          if (item_purpose != NULL &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_int0 &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_zero &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_space &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_quote &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_high &&
              CB_PAIR_X(CB_VALUE(item_purpose)) != cb_low) {
            switch (CB_FIELD(cb_ref(CB_PAIR_Y(item_value)))->usage) {
            case CB_USAGE_BINARY:
            case CB_USAGE_FLOAT:
            case CB_USAGE_DOUBLE:
            case CB_USAGE_PACKED:
            case CB_USAGE_COMP_5:
              cb_warning_x(CB_PAIR_Y(item_value),
                           _("%s  must be a non-comp type!"), name1);
              break;
            default:
              break;
            }
            switch (CB_TREE_CATEGORY(CB_PAIR_X(CB_VALUE(item_purpose)))) {
#ifndef I18N_UTF8
            case CB_CATEGORY_NATIONAL:
            case CB_CATEGORY_NATIONAL_EDITED:
              cb_warning_x(into,
                           _("%s and %s and %s have not same national type!"),
                           name1, name2, name3);
              break;
#endif /*I18N_UTF8*/
            case CB_CATEGORY_NUMERIC:
            case CB_CATEGORY_NUMERIC_EDITED:
              switch (
                  CB_FIELD(cb_ref(CB_PAIR_X(CB_VALUE(item_purpose))))->usage) {
              case CB_USAGE_BINARY:
              case CB_USAGE_FLOAT:
              case CB_USAGE_DOUBLE:
              case CB_USAGE_PACKED:
              case CB_USAGE_COMP_5:
                cb_warning_x(CB_PAIR_X(CB_VALUE(item_purpose)),
                             _("%s  must be a non-comp type!"), name2);
                break;
              default:
                break;
              }
              break;
            default:
              break;
            }
          }
          break;
        default:
#ifndef I18N_UTF8
          cb_warning_x(into, _("%s and %s and %s have not same national type!"),
                       name1, name2, name3);
#endif /*I18N_UTF8*/
          break;
        }
        break;
      default:
        break;
      }
    }
    start = item_value ? CB_CHAIN(item_value) : NULL;
  }
}

void cb_emit_string(cb_tree items, cb_tree into, cb_tree pointer) {
  cb_tree start;
  cb_tree l;
  cb_tree end;
  cb_tree dlm;

  if (cb_validate_one(into)) {
    return;
  }
  if (cb_validate_one(pointer) || cb_validate_numeric(pointer)) {
    return;
  }
  cb_validate_string(items, into);
  start = items;
  cb_emit(cb_build_funcall_2("CobolString.stringInit", into, pointer));
  while (start) {

    /* find DELIMITED item */
    for (end = start; end; end = CB_CHAIN(end)) {
      if (CB_PAIR_P(CB_VALUE(end))) {
        break;
      }
    }

    /* cob_string_delimited */
    dlm = end ? CB_PAIR_X(CB_VALUE(end)) : cb_int0;
    cb_emit(cb_build_funcall_1("CobolString.stringDelimited", dlm));

    /* cob_string_append */
    for (l = start; l != end; l = CB_CHAIN(l)) {
      cb_emit(cb_build_funcall_1("CobolString.stringAppend", CB_VALUE(l)));
    }

    start = end ? CB_CHAIN(end) : NULL;
  }
  cb_emit(cb_build_funcall_0("CobolString.stringFinish"));
}

/*
 * UNLOCK statement
 */

void cb_emit_unlock(cb_tree ref) {
  cb_tree file;

  if (ref != cb_error_node) {
    file = cb_ref(ref);
    cb_emit(cb_build_method_call_2("unlock", file, CB_FILE(file)->file_status));
    current_statement->file = file;
  }
}

/*
 * UNSTRING statement
 */

static void cb_validate_unstring(cb_tree name, cb_tree delimited,
                                 cb_tree into) {
  cb_tree item_value1;
  cb_tree item_value2;
  cb_tree start1;
  cb_tree start2;

  char name1[256], name2[256], name3[256], name4[256], name5[256];
  struct cb_field *pfield;
  struct cb_literal *pliteral;
  int size;
  int nationalflg = 0;
  int nationalflg2 = 0;
  char buff[1024];

  memset(buff, 0, sizeof(buff));
  memset(name1, 0, sizeof(name1));
  switch (CB_TREE_TAG(name)) {
  case CB_TAG_REFERENCE:
    pfield = CB_FIELD(cb_ref(name));
    cb_get_jisword_buff((char *)pfield->name, name1, sizeof(name1));
    break;
  case CB_TAG_LITERAL:
    pliteral = CB_LITERAL(name);
    size = pliteral->size;
    strcpy(name1, "\'");
    if (size >= 253) {
      memcpy(name1 + 1, pliteral->data, 253);
    } else {
      memcpy(name1 + 1, pliteral->data, size);
    }
    strcat(name1, "\'");
    break;
  default:
    break;
  }
  switch (CB_TREE_CATEGORY(name)) {
  case CB_CATEGORY_NUMERIC:
  case CB_CATEGORY_NUMERIC_EDITED:
    strcpy(buff, name1);
    strcat(buff, ",");
    switch (CB_FIELD(cb_ref(name))->usage) {
    case CB_USAGE_BINARY:
    case CB_USAGE_FLOAT:
    case CB_USAGE_DOUBLE:
    case CB_USAGE_PACKED:
    case CB_USAGE_COMP_5:
      cb_warning_x(name, _("%s  must be a non-comp type!"), name1);
      break;
    default:
      break;
    }
    break;
  case CB_CATEGORY_NATIONAL:
  case CB_CATEGORY_NATIONAL_EDITED:
    nationalflg = 1;
    nationalflg2 = 1;
    break;
  default:
    strcpy(buff, name1);
    strcat(buff, ",");
    break;
  }
  start1 = delimited;
  while (start1) {
    for (item_value1 = start1; item_value1;
         item_value1 = CB_CHAIN(item_value1)) {
      memset(name2, 0, sizeof(name2));
      if (CB_VALUE(item_value1)) {
        switch (CB_TREE_TAG(CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0])) {
        case CB_TAG_REFERENCE:
          pfield =
              CB_FIELD(cb_ref(CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0]));
          cb_get_jisword_buff((char *)pfield->name, name2, sizeof(name2));
          break;
        case CB_TAG_LITERAL:
          pliteral = CB_LITERAL(CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0]);
          size = pliteral->size;
          strcpy(name2, "\'");
          if (size >= 253) {
            memcpy(name2 + 1, pliteral->data, 253);
          } else {
            memcpy(name2 + 1, pliteral->data, size);
          }
          strcat(name2, "\'");
          break;
        default:
          break;
        }
        if (item_value1 != NULL &&
            CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0] != cb_zero &&
            CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0] != cb_space &&
            CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0] != cb_quote &&
            CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0] != cb_high &&
            CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0] != cb_low) {
          switch (
              CB_TREE_CATEGORY(CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0])) {
          case CB_CATEGORY_NUMERIC:
          case CB_CATEGORY_NUMERIC_EDITED:
            if (sizeof(buff) == 0) {
              strcpy(buff, name2);
            } else {
              strcat(buff, name2);
            }
            strcat(buff, ",");
            switch (
                CB_FIELD(cb_ref(CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0]))
                    ->usage) {
            case CB_USAGE_BINARY:
            case CB_USAGE_FLOAT:
            case CB_USAGE_DOUBLE:
            case CB_USAGE_PACKED:
            case CB_USAGE_COMP_5:
              cb_warning_x(CB_FUNCALL(CB_PAIR_Y(item_value1))->argv[0],
                           _("%s  must be a non-comp type!"), name2);
              break;
            default:
              break;
            }
            break;
          case CB_CATEGORY_NATIONAL:
          case CB_CATEGORY_NATIONAL_EDITED:
            nationalflg &= 1;
            nationalflg2 = 1;
            break;
          default:
            if (sizeof(buff) == 0) {
              strcpy(buff, name2);
            } else {
              strcat(buff, name2);
            }
            strcat(buff, ",");
            nationalflg &= 0;
            break;
          }
        }
      }
    }
    start1 = item_value1 ? CB_CHAIN(item_value1) : NULL;
  }
  start2 = into;
  while (start2) {
    for (item_value2 = start2; item_value2;
         item_value2 = CB_CHAIN(item_value2)) {
      memset(name3, 0, sizeof(name3));
      if (CB_VALUE(item_value2)) {
        switch (CB_TREE_TAG(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0])) {
        case CB_TAG_REFERENCE:
          pfield =
              CB_FIELD(cb_ref(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0]));
          cb_get_jisword_buff((char *)pfield->name, name3, sizeof(name3));
          break;
        case CB_TAG_LITERAL:
          pliteral = CB_LITERAL(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0]);
          size = pliteral->size;
          strcpy(name3, "\'");
          if (size >= 253) {
            memcpy(name3 + 1, pliteral->data, 253);
          } else {
            memcpy(name3 + 1, pliteral->data, size);
          }
          strcat(name3, "\'");
          break;
        default:
          break;
        }
        if (item_value2 != NULL &&
            CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0] != cb_zero &&
            CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0] != cb_space &&
            CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0] != cb_quote &&
            CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0] != cb_high &&
            CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0] != cb_low) {
          switch (
              CB_TREE_CATEGORY(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0])) {
          case CB_CATEGORY_NUMERIC:
            if (sizeof(buff) == 0) {
              strcpy(buff, name3);
            } else {
              strcat(buff, name3);
            }
            strcat(buff, ",");
            switch (
                CB_FIELD(cb_ref(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0]))
                    ->usage) {
            case CB_USAGE_BINARY:
            case CB_USAGE_FLOAT:
            case CB_USAGE_DOUBLE:
            case CB_USAGE_PACKED:
            case CB_USAGE_COMP_5:
              cb_warning_x(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0],
                           _("%s  must be a non-comp type!"), name3);
              break;
            default:
              break;
            }
            break;
          case CB_CATEGORY_NATIONAL:
            nationalflg &= 1;
            nationalflg2 = 1;
            break;
          case CB_CATEGORY_ALPHANUMERIC_EDITED:
          case CB_CATEGORY_NUMERIC_EDITED:
            if (sizeof(buff) == 0) {
              strcpy(buff, name3);
            } else {
              strcat(buff, name3);
            }
            strcat(buff, ",");
            cb_error_x(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0],
                       "%s must be a non-edit type!", name3);
            break;
          case CB_CATEGORY_NATIONAL_EDITED:
            cb_error_x(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[0],
                       "%s must be a non-edit type!", name3);
            break;
          default:
            if (sizeof(buff) == 0) {
              strcpy(buff, name3);
            } else {
              strcat(buff, name3);
            }
            strcat(buff, ",");
            nationalflg &= 0;
            break;
          }
        }
        memset(name4, 0, sizeof(name4));
        switch (CB_TREE_TAG(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[1])) {
        case CB_TAG_REFERENCE:
          pfield =
              CB_FIELD(cb_ref(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[1]));
          cb_get_jisword_buff((char *)pfield->name, name4, sizeof(name4));
          break;
        case CB_TAG_LITERAL:
          pliteral = CB_LITERAL(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[1]);
          size = pliteral->size;
          strcpy(name4, "\'");
          if (size >= 253) {
            memcpy(name4 + 1, pliteral->data, 253);
          } else {
            memcpy(name4 + 1, pliteral->data, size);
          }
          strcat(name4, "\'");
          break;
        default:
          break;
        }
        if (CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[1] != cb_int0) {
          switch (
              CB_TREE_CATEGORY(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[1])) {
          case CB_CATEGORY_NUMERIC:
          case CB_CATEGORY_NUMERIC_EDITED:
            switch (
                CB_FIELD(cb_ref(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[1]))
                    ->usage) {
            case CB_USAGE_BINARY:
            case CB_USAGE_FLOAT:
            case CB_USAGE_DOUBLE:
            case CB_USAGE_PACKED:
            case CB_USAGE_COMP_5:
              cb_warning_x(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[1],
                           _("%s  must be a non-comp type!"), name4);
              break;
            default:
              break;
            }
            break;
          default:
            break;
          }
        }
        memset(name5, 0, sizeof(name5));
        switch (CB_TREE_TAG(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[2])) {
        case CB_TAG_REFERENCE:
          pfield =
              CB_FIELD(cb_ref(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[2]));
          cb_get_jisword_buff((char *)pfield->name, name5, sizeof(name5));
          break;
        case CB_TAG_LITERAL:
          pliteral = CB_LITERAL(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[2]);
          size = pliteral->size;
          strcpy(name5, "\'");
          if (size >= 253) {
            memcpy(name5 + 1, pliteral->data, 253);
          } else {
            memcpy(name5 + 1, pliteral->data, size);
          }
          strcat(name5, "\'");
          break;
        default:
          break;
        }
        if (CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[2] != cb_int0) {
          switch (
              CB_TREE_CATEGORY(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[2])) {
          case CB_CATEGORY_NUMERIC:
            break;
          default:
            cb_error_x(CB_FUNCALL(CB_PAIR_Y(item_value2))->argv[2],
                       "%s  must be a numeric type!", name5);
            break;
          }
        }
      }
    }
    start2 = item_value2 ? CB_CHAIN(item_value2) : NULL;
  }
  if (strlen(buff) != 0) {
    buff[strlen(buff) - 1] = '\0';
  }
  if (nationalflg != 1 && nationalflg2 == 1) {
    cb_warning_x(name, _("%s must be national type!"), buff);
  }
}

void cb_emit_unstring(cb_tree name, cb_tree delimited, cb_tree into,
                      cb_tree pointer, cb_tree tallying) {
  if (cb_validate_one(name)) {
    return;
  }
  if (cb_validate_one(tallying) || cb_validate_numeric(tallying)) {
    return;
  }
  if (cb_validate_list(delimited)) {
    return;
  }
  if (cb_validate_list(into)) {
    return;
  }
  if (cb_validate_one(pointer) || cb_validate_numeric(pointer)) {
    return;
  }
  cb_validate_unstring(name, delimited, into);
  cb_emit(cb_build_funcall_3("CobolString.unstringInit", name, pointer,
                             cb_int(cb_list_length(delimited))));
  cb_emit_list(delimited);
  cb_emit_list(into);
  if (tallying) {
    cb_emit(cb_build_funcall_1("CobolString.unstringTallying", tallying));
  }
  cb_emit(cb_build_funcall_0("CobolString.unstringFinish"));
}

cb_tree cb_build_unstring_delimited(cb_tree all, cb_tree value) {
  if (cb_validate_one(value)) {
    return cb_error_node;
  }
  return cb_build_funcall_2("CobolString.unstringDelimited", value, all);
}

cb_tree cb_build_unstring_into(cb_tree name, cb_tree delimiter, cb_tree count) {
  if (cb_validate_one(name)) {
    return cb_error_node;
  }
  if (delimiter == NULL) {
    delimiter = cb_int0;
  }
  if (count == NULL) {
    count = cb_int0;
  }
  return cb_build_funcall_3("CobolString.unstringInto", name, delimiter, count);
}

/*
 * WRITE statement
 */

void cb_emit_write(cb_tree record, cb_tree from, cb_tree opt, cb_tree lockopt) {
  cb_tree file;
  int val;

  if (record != cb_error_node && cb_ref(record) != cb_error_node) {
    if (!CB_REF_OR_FIELD_P(cb_ref(record))) {
      cb_error_x(CB_TREE(current_statement),
                 _("WRITE requires a record name as subject"));
      return;
    }
    if (cb_field(record)->storage != CB_STORAGE_FILE) {
      cb_error_x(CB_TREE(current_statement),
                 _("WRITE subject does not refer to a record name"));
      return;
    }
    file = CB_TREE(CB_FIELD(cb_ref(record))->file);
    current_statement->file = file;
    if (CB_FILE(file)->organization == COB_ORG_SORT) {
      cb_error_x(CB_TREE(current_statement),
                 _("Operation not allowed on SORT files"));
    } else if (current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
               (CB_FILE(file)->organization != COB_ORG_RELATIVE &&
                CB_FILE(file)->organization != COB_ORG_INDEXED)) {
      cb_error_x(CB_TREE(current_statement),
                 _("INVALID KEY clause invalid with this file type"));
    } else if (lockopt) {
      if ((CB_FILE(file)->lock_mode & COB_LOCK_AUTOMATIC)) {
        cb_error_x(CB_TREE(current_statement),
                   _("LOCK clause invalid with file LOCK AUTOMATIC"));
      } else if (opt != cb_int0) {
        cb_error_x(CB_TREE(current_statement), _("LOCK clause invalid here"));
      } else if (lockopt == cb_int1) {
        opt = cb_int(COB_WRITE_LOCK);
      }
    }
    if (from) {
      cb_emit(cb_build_move(from, record));
    }
    if (CB_FILE(file)->organization == COB_ORG_LINE_SEQUENTIAL &&
        opt == cb_int0) {
      opt = cb_int(COB_WRITE_BEFORE | COB_WRITE_LINES | 1);
    }
    /* RXW - This is horrible */
    if (current_statement->handler_id == COB_EC_I_O_EOP &&
        current_statement->handler1) {
      if (CB_CAST_P(opt)) {
        val = CB_INTEGER(CB_BINARY_OP(CB_CAST(opt)->val)->x)->val;
        val |= COB_WRITE_EOP;
        CB_BINARY_OP(CB_CAST(opt)->val)->x = cb_int(val);
      } else {
        val = CB_INTEGER(opt)->val;
        val |= COB_WRITE_EOP;
        opt = cb_int(val);
      }
    }
    cb_emit(cb_build_method_call_4("write", file, record, opt,
                                   CB_FILE(file)->file_status));
  }
}

cb_tree cb_build_write_advancing_lines(cb_tree pos, cb_tree lines) {
  cb_tree e;
  int opt;

  opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;
  e = cb_build_binary_op(cb_int(opt | COB_WRITE_LINES), '+', lines);
  return cb_build_cast_integer(e);
}

cb_tree cb_build_write_advancing_mnemonic(cb_tree pos, cb_tree mnemonic) {
  int opt;
  int token;

  token = CB_SYSTEM_NAME(cb_ref(mnemonic))->token;
  switch (token) {
  case CB_FEATURE_FORMFEED:
    opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;
    return cb_int(opt | COB_WRITE_PAGE);
  case CB_FEATURE_C01:
  case CB_FEATURE_C02:
  case CB_FEATURE_C03:
  case CB_FEATURE_C04:
  case CB_FEATURE_C05:
  case CB_FEATURE_C06:
  case CB_FEATURE_C07:
  case CB_FEATURE_C08:
  case CB_FEATURE_C09:
  case CB_FEATURE_C10:
  case CB_FEATURE_C11:
  case CB_FEATURE_C12:
    opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;
    return cb_int(opt | COB_WRITE_CHANNEL | COB_WRITE_PAGE | token);
  default:
    cb_error_x(mnemonic, _("Invalid mnemonic name"));
    return cb_error_node;
  }
}

cb_tree cb_build_write_advancing_page(cb_tree pos) {
  int opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;

  return cb_int(opt | COB_WRITE_PAGE);
}

cb_tree cb_check_zero_division(cb_tree x) {
  if (x == cb_error_node) {
    return cb_error_node;
  }

  if (!CB_NUMERIC_LITERAL_P(x)) {
    return x;
  }

  if (cb_get_int(x) == 0) {
    cb_error_x(x, _("Detected division by zero."));
    return cb_error_node;
  }

  return x;
}
