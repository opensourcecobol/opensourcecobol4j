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

/* Global variables */

size_t cb_needs_01 = 0;

/* Local variables */

static struct cb_field *last_real_field = NULL;
static const int pic_digits[] = {2, 4, 7, 9, 12, 14, 16, 18};

int cb_get_level(cb_tree x) {
  const char *p;
  const char *name;
  int level = 0;

  name = CB_NAME(x);
  /* get level */
  for (p = name; *p; p++) {
    if (!isdigit(*p)) {
      goto level_error;
    }
    level = level * 10 + (*p - '0');
  }

  /* check level */
  switch (level) {
  case 66:
  case 77:
  case 78:
  case 88:
    break;
  default:
    if (level < 1 || level > 49) {
      goto level_error;
    }
    break;
  }

  return level;

level_error:
  cb_error_x(x, _("Invalid level number '%s'"), name);
  return 0;
}

cb_tree cb_build_field_tree(cb_tree level, cb_tree name,
                            struct cb_field *last_field,
                            enum cb_storage storage, struct cb_file *fn) {
  struct cb_reference *r;
  struct cb_field *f;
  struct cb_field *p;
  struct cb_field *field_fill;
  cb_tree dummy_fill;
  cb_tree l;
  cb_tree x;
  int lv;

  if (level == cb_error_node || name == cb_error_node) {
    return cb_error_node;
  }

  /* check the level number */
  lv = cb_get_level(level);
  if (!lv) {
    return cb_error_node;
  }

  /* build the field */
  r = CB_REFERENCE(name);
  f = CB_FIELD(cb_build_field(name));
  f->storage = storage;
  last_real_field = last_field;
  if (lv == 78) {
    f->level = 01;
    f->flag_item_78 = 1;
    return CB_TREE(f);
  } else {
    f->level = lv;
  }
  if (f->level == 01 && storage == CB_STORAGE_FILE) {
    if (fn->external) {
      f->flag_external = 1;
      has_external = 1;
    } else if (fn->global) {
      f->flag_is_global = 1;
    }
  }
  if (last_field) {
    if (last_field->level == 77 && f->level != 01 && f->level != 77 &&
        f->level != 66 && f->level != 88) {
      cb_error_x(name, _("Level number must begin with 01 or 77"));
      return cb_error_node;
    }
  }

  /* checks for redefinition */
  if (cb_warn_redefinition) {
    if (r->word->count > 1) {
      if (f->level == 01 || f->level == 77) {
        redefinition_warning(name, NULL);
      } else {
        for (l = r->word->items; l; l = CB_CHAIN(l)) {
          x = CB_VALUE(l);
          if (!CB_FIELD_P(x) || CB_FIELD(x)->level == 01 ||
              CB_FIELD(x)->level == 77 ||
              (f->level == last_field->level &&
               CB_FIELD(x)->parent == last_field->parent)) {
            redefinition_warning(name, x);
            break;
          }
        }
      }
    }
  }

  if (last_field && last_field->level == 88) {
    last_field = last_field->parent;
  }

  /* link the field into the tree */
  if (f->level == 01 || f->level == 77) {
    /* top level */
    cb_needs_01 = 0;
    if (last_field) {
      /*
                              cb_field_add (cb_field_founder (last_field), f);
      */
      cb_field_founder(last_field)->sister = f;
    }
  } else if (!last_field || cb_needs_01) {
    /* invalid top level */
    cb_error_x(name, _("Level number must begin with 01 or 77"));
    return cb_error_node;
  } else if (f->level == 66) {
    /* level 66 */
    f->parent = cb_field_founder(last_field);
    for (p = f->parent->children; p && p->sister; p = p->sister)
      ;
    if (p) {
      p->sister = f;
    }
  } else if (f->level == 88) {
    /* level 88 */
    f->parent = last_field;
  } else if (f->level > last_field->level) {
    /* lower level */
    last_field->children = f;
    f->parent = last_field;
  } else if (f->level == last_field->level) {
    /* same level */
  same_level:
    last_field->sister = f;
    f->parent = last_field->parent;
  } else {
    /* upper level */
    for (p = last_field->parent; p; p = p->parent) {
      if (p->level == f->level) {
        last_field = p;
        goto same_level;
      }
      if (cb_relax_level_hierarchy && p->level < f->level) {
        break;
      }
    }
    if (cb_relax_level_hierarchy) {
      dummy_fill = cb_build_filler();
      field_fill = CB_FIELD(cb_build_field(dummy_fill));
      cb_warning_x(name, _("No previous data item of level %02d"), f->level);
      field_fill->level = f->level;
      field_fill->storage = storage;
      field_fill->children = p->children;
      field_fill->parent = p;
      for (p = p->children; p != NULL; p = p->sister) {
        p->parent = field_fill;
      }
      field_fill->parent->children = field_fill;
      field_fill->sister = f;
      f->parent = field_fill->parent;
      last_field = field_fill;
    } else {
      cb_error_x(name, _("No previous data item of level %02d"), f->level);
      return cb_error_node;
    }
  }

  /* inherit parent's properties */
  if (f->parent) {
    f->usage = f->parent->usage;
    f->indexes = f->parent->indexes;
    f->flag_sign_leading = f->parent->flag_sign_leading;
    f->flag_sign_separate = f->parent->flag_sign_separate;
    f->flag_is_global = f->parent->flag_is_global;
  }
  return CB_TREE(f);
}

struct cb_field *cb_resolve_redefines(struct cb_field *field,
                                      cb_tree redefines) {
  struct cb_field *f;
  struct cb_reference *r;
  const char *name;
  cb_tree x;

  r = CB_REFERENCE(redefines);
  name = CB_NAME(redefines);
  x = CB_TREE(field);

  /* check qualification */
  if (r->chain) {
    cb_error_x(x, _("'%s' cannot be qualified here"), name);
    return NULL;
  }

  /* check subscripts */
  if (r->subs) {
    cb_error_x(x, _("'%s' cannot be subscripted here"), name);
    return NULL;
  }

  /* resolve the name in the current group (if any) */
  if (field->parent && field->parent->children) {
    for (f = field->parent->children; f; f = f->sister) {
      if (cobc_casecmp(f->name, name) == 0) {
        break;
      }
    }
    if (f == NULL) {
      cb_error_x(x, _("'%s' undefined in '%s'"), name, field->parent->name);
      return NULL;
    }
  } else {
    if (cb_ref(redefines) == cb_error_node) {
      return NULL;
    }
    f = cb_field(redefines);
  }

  /* check level number */
  if (f->level != field->level) {
    cb_error_x(x, _("Level number of REDEFINES entries must be identical"));
    return NULL;
  }
  if (f->level == 66 || f->level == 88) {
    cb_error_x(x, _("Level number of REDEFINES entry cannot be 66 or 88"));
    return NULL;
  }

  if (!cb_indirect_redefines && f->redefines) {
    cb_error_x(x, _("'%s' not the original definition"), f->name);
    return NULL;
  }

  /* return the original definition */
  while (f->redefines) {
    f = f->redefines;
  }
  return f;
}

static int validate_field_1(struct cb_field *f) {
  cb_tree x;
  cb_tree l;
  char *name;
  struct cb_field *p;
  char *pp;
  unsigned char *pstr;
  int vorint;
  int need_picture;
  char pic[16];

  x = CB_TREE(f);
  name = cb_name(x);
  if (f->flag_any_length) {
    if (f->storage != CB_STORAGE_LINKAGE) {
      cb_error_x(x, _("'%s' ANY LENGTH only allowed in LINKAGE"), name);
      return -1;
    }
    if (f->level != 01) {
      cb_error_x(x, _("'%s' ANY LENGTH must be 01 level"), name);
      return -1;
    }
    if (f->flag_item_based || f->flag_external) {
      cb_error_x(x, _("'%s' ANY LENGTH can not be BASED/EXTERNAL"), name);
      return -1;
    }
    if (f->flag_occurs || f->occurs_depending || f->children || f->values ||
        f->flag_blank_zero) {
      cb_error_x(x, _("'%s' ANY LENGTH has invalid definition"), name);
      return -1;
    }
    if (!f->pic) {
      cb_error_x(x, _("'%s' ANY LENGTH must have a PICTURE"), name);
      return -1;
    }
    if (f->pic->size != 1 || f->usage != CB_USAGE_DISPLAY) {
      cb_error_x(x, _("'%s' ANY LENGTH has invalid definition"), name);
      return -1;
    }
    f->count++;
    return 0;
  }

  if (f->level == 77) {
    if (f->storage != CB_STORAGE_WORKING && f->storage != CB_STORAGE_LOCAL &&
        f->storage != CB_STORAGE_LINKAGE) {
      cb_error_x(x, _("'%s' 77 level not allowed here"), name);
    }
  }
  if (f->flag_external) {
    if (f->level != 01 && f->level != 77) {
      cb_error_x(x, _("'%s' EXTERNAL must be specified at 01/77 level"), name);
    }
    if (f->storage != CB_STORAGE_WORKING && f->storage != CB_STORAGE_FILE) {
      cb_error_x(
          x,
          _("'%s' EXTERNAL can only be specified in WORKING-STORAGE section"),
          name);
    }
    if (f->flag_item_based) {
      cb_error_x(x, _("'%s' EXTERNAL and BASED are mutually exclusive"), name);
    }
    if (f->redefines) {
      cb_error_x(x, _("'%s' EXTERNAL not allowed with REDEFINES"), name);
    }
  }
  if (f->flag_item_based) {
    if (f->storage != CB_STORAGE_WORKING && f->storage != CB_STORAGE_LOCAL &&
        f->storage != CB_STORAGE_LINKAGE) {
      cb_error_x(x, _("'%s' BASED not allowed here"), name);
    }
    if (f->redefines) {
      cb_error_x(x, _("'%s' BASED not allowed with REDEFINES"), name);
    }
    if (f->level != 01 && f->level != 77) {
      cb_error_x(x, _("'%s' BASED only allowed at the 01 and 77 levels"), name);
    }
  }
  if (f->level == 66) {
    if (!f->redefines) {
      level_require_error(x, "RENAMES");
      return -1;
    }
    if (f->flag_occurs) {
      level_except_error(x, "RENAMES");
    }
    return 0;
  }

  /* validate OCCURS */
  if (f->flag_occurs) {
    if ((!cb_verify(cb_top_level_occurs_clause, "01/77 OCCURS") &&
         (f->level == 01 || f->level == 77)) ||
        (f->level == 66 || f->level == 88)) {
      level_redundant_error(x, "OCCURS");
    }
    for (l = f->index_list; l; l = CB_CHAIN(l)) {
      cb_field(CB_VALUE(l))->flag_is_global = f->flag_is_global;
    }
  }

  /* validate OCCURS DEPENDING */
  if (f->occurs_depending) {
    /* the data item that contains a OCCURS DEPENDING clause shall not
       be subordinate to a data item that has the OCCURS clause */
    for (p = f->parent; p; p = p->parent) {
      if (p->flag_occurs) {
        cb_error_x(CB_TREE(p),
                   _("'%s' cannot have the OCCURS clause due to '%s'"),
                   check_filler_name((char *)p->name), check_filler_name(name));
        break;
      }
    }

    /* the data item that contains a OCCURS DEPENDING clause must be
       the last data item in the group */
    for (p = f; p->parent; p = p->parent) {
      for (; p->sister; p = p->sister) {
        if (p->sister == cb_field(f->occurs_depending)) {
          cb_error_x(x, _("'%s' ODO field item invalid here"), p->sister->name);
        }
        if (!p->sister->redefines) {
          if (!cb_complex_odo) {
            cb_error_x(x, _("'%s' cannot have OCCURS DEPENDING"),
                       check_filler_name(name));
            break;
          }
        }
      }
    }
    /* If the field is GLOBAL, then the ODO must also be GLOBAL */
    if (f->flag_is_global) {
      if (!cb_field(f->occurs_depending)->flag_is_global) {
        cb_error_x(x, _("'%s' ODO item must have GLOBAL attribute"),
                   cb_field(f->occurs_depending)->name);
      }
      if (f->storage != cb_field(f->occurs_depending)->storage) {
        cb_error_x(
            x, _("GLOBAL '%s' ODO item is not in the same section as OCCURS"),
            cb_field(f->occurs_depending)->name);
      }
    }
  }

  /* validate REDEFINES */
  if (f->redefines) {
    /* check OCCURS */
    if (f->redefines->flag_occurs) {
      cb_warning_x(x, _("The original definition '%s' should not have OCCURS"),
                   f->redefines->name);
    }

    /* check definition */
    for (p = f->redefines->sister; p && p != f; p = p->sister) {
      if (!p->redefines) {
        cb_error_x(x, _("REDEFINES must follow the original definition"));
        break;
      }
    }

    /* check variable occurrence */
    if (f->occurs_depending || cb_field_variable_size(f)) {
      cb_error_x(x, _("'%s' cannot be variable length"), f->name);
    }
    if (cb_field_variable_size(f->redefines)) {
      cb_error_x(x, _("The original definition '%s' cannot be variable length"),
                 f->redefines->name);
    }
  }

  if (f->children) {
    /* group item */

    if (f->pic) {
      group_error(x, "PICTURE");
    }
    if (f->flag_justified) {
      group_error(x, "JUSTIFIED RIGHT");
    }
    if (f->flag_blank_zero) {
      group_error(x, "BLANK WHEN ZERO");
    }

    for (f = f->children; f; f = f->sister) {
      if (validate_field_1(f) != 0) {
        return -1;
      }
    }
  } else {
    /* elementary item */

    /* validate PICTURE */
    need_picture = 1;
    if (f->usage == CB_USAGE_INDEX || f->usage == CB_USAGE_LENGTH ||
        f->usage == CB_USAGE_OBJECT || f->usage == CB_USAGE_POINTER ||
        f->usage == CB_USAGE_PROGRAM_POINTER || f->usage == CB_USAGE_FLOAT ||
        f->usage == CB_USAGE_DOUBLE || f->usage == CB_USAGE_SIGNED_CHAR ||
        f->usage == CB_USAGE_SIGNED_SHORT || f->usage == CB_USAGE_SIGNED_INT ||
        f->usage == CB_USAGE_SIGNED_LONG ||
        f->usage == CB_USAGE_UNSIGNED_CHAR ||
        f->usage == CB_USAGE_UNSIGNED_SHORT ||
        f->usage == CB_USAGE_UNSIGNED_INT ||
        f->usage == CB_USAGE_UNSIGNED_LONG || f->usage == CB_USAGE_PROGRAM) {
      need_picture = 0;
    }
    if (f->pic == NULL && need_picture != 0) {
      if (f->storage == CB_STORAGE_SCREEN) {
        /* RXW
                                        if (f->values &&
                                            CB_LITERAL(CB_VALUE(f->values))->size)
           {
        */
        if (f->values) {
          sprintf(pic, "X(%d)", (int)CB_LITERAL(CB_VALUE(f->values))->size);
        } else {
          sprintf(pic, "X(1)");
        }
        f->pic = CB_PICTURE(cb_build_picture(pic));
      } else if (f->flag_item_78 && f->values &&
                 CB_VALUE(f->values) != cb_error_node) {
        f->count++;
        if (CB_NUMERIC_LITERAL_P(CB_VALUE(f->values))) {
          memset(pic, 0, sizeof(pic));
          pp = pic;
          if (CB_LITERAL(CB_VALUE(f->values))->sign) {
            *pp++ = 'S';
          }
          vorint = CB_LITERAL(CB_VALUE(f->values))->size -
                   CB_LITERAL(CB_VALUE(f->values))->scale;
          if (vorint) {
            pp += sprintf(pp, "9(%d)", vorint);
          }
          if (CB_LITERAL(CB_VALUE(f->values))->scale) {
            sprintf(pp, "V9(%d)", CB_LITERAL(CB_VALUE(f->values))->scale);
          }
          if (CB_LITERAL(CB_VALUE(f->values))->size < 10) {
            f->usage = CB_USAGE_COMP_5;
          } else {
            f->usage = CB_USAGE_DISPLAY;
          }
          f->pic = CB_PICTURE(cb_build_picture(pic));
          f->pic->category = CB_CATEGORY_NUMERIC;
        } else {
          sprintf(pic, "X(%d)", (int)CB_LITERAL(CB_VALUE(f->values))->size);
          f->pic = CB_PICTURE(cb_build_picture(pic));
          f->pic->category = CB_CATEGORY_ALPHANUMERIC;
          f->usage = CB_USAGE_DISPLAY;
        }
      } else {
        if (f->flag_item_78) {
          cb_error_x(x, _("Value required for constant item '%s'"), name);
        } else {
          cb_error_x(x, _("PICTURE clause required for '%s'"),
                     check_filler_name(name));
        }
        return -1;
      }
    }
    if (f->pic != NULL && need_picture == 0) {
      cb_error_x(x, _("'%s' cannot have PICTURE clause"),
                 check_filler_name(name));
    }

    /* validate USAGE */
    switch (f->usage) {
    case CB_USAGE_SIGNED_CHAR:
      f->usage = CB_USAGE_COMP_5;
      f->pic = CB_PICTURE(cb_build_picture("S99"));
      f->flag_real_binary = 1;
      break;
    case CB_USAGE_SIGNED_SHORT:
      f->usage = CB_USAGE_COMP_5;
      f->pic = CB_PICTURE(cb_build_picture("S9(4)"));
      f->flag_real_binary = 1;
      break;
    case CB_USAGE_SIGNED_INT:
      f->usage = CB_USAGE_COMP_5;
      f->pic = CB_PICTURE(cb_build_picture("S9(9)"));
      f->flag_real_binary = 1;
      break;
    case CB_USAGE_SIGNED_LONG:
      f->usage = CB_USAGE_COMP_5;
      f->pic = CB_PICTURE(cb_build_picture("S9(18)"));
      f->flag_real_binary = 1;
      break;
    case CB_USAGE_UNSIGNED_CHAR:
      f->usage = CB_USAGE_COMP_5;
      f->pic = CB_PICTURE(cb_build_picture("99"));
      f->flag_real_binary = 1;
      break;
    case CB_USAGE_UNSIGNED_SHORT:
      f->usage = CB_USAGE_COMP_5;
      f->pic = CB_PICTURE(cb_build_picture("9(4)"));
      f->flag_real_binary = 1;
      break;
    case CB_USAGE_UNSIGNED_INT:
      f->usage = CB_USAGE_COMP_5;
      f->pic = CB_PICTURE(cb_build_picture("9(9)"));
      f->flag_real_binary = 1;
      break;
    case CB_USAGE_UNSIGNED_LONG:
      f->usage = CB_USAGE_COMP_5;
      f->pic = CB_PICTURE(cb_build_picture("9(18)"));
      f->flag_real_binary = 1;
      break;
    case CB_USAGE_BINARY:
    case CB_USAGE_PACKED:
      if (f->pic->category != CB_CATEGORY_NUMERIC) {
        cb_error_x(x, _("'%s' PICTURE clause not compatible with USAGE"), name);
      }
      break;
    case CB_USAGE_COMP_5:
    case CB_USAGE_COMP_X:
      if (f->pic) {
        if (f->pic->category != CB_CATEGORY_NUMERIC &&
            f->pic->category != CB_CATEGORY_ALPHANUMERIC) {
          cb_error_x(x, _("'%s' PICTURE clause not compatible with USAGE"),
                     name);
        }
      }
      break;
    default:
      break;
    }

    /* validate SIGN */

    /* validate JUSTIFIED RIGHT */
    if (f->flag_justified) {
      switch (f->pic->category) {
      case CB_CATEGORY_ALPHABETIC:
      case CB_CATEGORY_ALPHANUMERIC:
      case CB_CATEGORY_NATIONAL:
        break;
      default:
        cb_error_x(x, _("'%s' cannot have JUSTIFIED RIGHT"), name);
        break;
      }
    }

    /* validate SYNCHRONIZED */

    /* validate BLANK ZERO */
    if (f->flag_blank_zero) {
      switch (f->pic->category) {
      case CB_CATEGORY_NUMERIC:
        /* reconstruct the picture string */
        if (f->pic->scale > 0) {
          f->pic->str = cobc_malloc(20);
          pstr = (unsigned char *)(f->pic->str);
          *pstr++ = '9';
          vorint = f->pic->digits - f->pic->scale;
          memcpy(pstr, (unsigned char *)&vorint, sizeof(int));
          pstr += sizeof(int);
          *pstr++ = 'V';
          vorint = 1;
          memcpy(pstr, (unsigned char *)&vorint, sizeof(int));
          pstr += sizeof(int);
          *pstr++ = '9';
          vorint = f->pic->scale;
          memcpy(pstr, (unsigned char *)&vorint, sizeof(int));
          f->pic->size++;
        } else {
          f->pic->str = cobc_malloc(8);
          pstr = (unsigned char *)(f->pic->str);
          *pstr++ = '9';
          vorint = f->pic->digits;
          memcpy(pstr, (unsigned char *)&vorint, sizeof(int));
        }
        f->pic->category = CB_CATEGORY_NUMERIC_EDITED;
        break;
      case CB_CATEGORY_NUMERIC_EDITED:
        break;
      default:
        cb_error_x(x, _("'%s' cannot have BLANK WHEN ZERO"), name);
        break;
      }
    }

    /* validate VALUE */
    if (f->values) {
      if (CB_PAIR_P(CB_VALUE(f->values)) || CB_CHAIN(f->values)) {
        cb_error_x(x, _("Only level 88 item may have multiple values"));
      }

      /* ISO+IEC+1989-2002: 13.16.42.2-10 */
      for (p = f; p; p = p->parent) {
        if (p->redefines) {
          cb_error_x(x, _("Entries under REDEFINES cannot have VALUE clause"));
        }
        if (p->flag_external) {
          cb_warning_x(x, _("VALUE clause ignored for EXTERNAL items"));
        }
      }
    }
  }

  return 0;
}

static void setup_parameters(struct cb_field *f) {
  int flag_local;
  char pic[8];

  /* determine the class */
  if (f->children) {
    /* group field */
    flag_local = f->flag_local;
    for (f = f->children; f; f = f->sister) {
      f->flag_local = flag_local;
      setup_parameters(f);
    }
  } else {
    /* regular field */
    switch (f->usage) {
    case CB_USAGE_BINARY:
#ifndef WORDS_BIGENDIAN
      if (cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
        f->flag_binary_swap = 1;
      }
#endif
      break;

    case CB_USAGE_INDEX:
      f->pic = CB_PICTURE(cb_build_picture("S9(9)"));
      break;

    case CB_USAGE_LENGTH:
      f->pic = CB_PICTURE(cb_build_picture("9(9)"));
      break;

    case CB_USAGE_POINTER:
    case CB_USAGE_PROGRAM_POINTER:
      f->pic = CB_PICTURE(cb_build_picture("9(10)"));
      break;
    case CB_USAGE_FLOAT:
      f->pic = CB_PICTURE(cb_build_picture("S9(7)V9(7)"));
      break;
    case CB_USAGE_DOUBLE:
      f->pic = CB_PICTURE(cb_build_picture("S9(9)V9(9)"));
      break;

    case CB_USAGE_COMP_5:
    case CB_USAGE_COMP_X:
      if (f->pic->category == CB_CATEGORY_ALPHANUMERIC) {
        if (f->pic->size > 8) {
          sprintf(pic, "9(36)");
        } else {
          sprintf(pic, "9(%d)", pic_digits[f->pic->size - 1]);
        }
        f->pic = CB_PICTURE(cb_build_picture(pic));
      }
#ifndef WORDS_BIGENDIAN
      if (f->usage == CB_USAGE_COMP_X) {
        if (cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
          f->flag_binary_swap = 1;
        }
      }
#endif
      break;

    default:
      break;
    }
  }
}

static int compute_size(struct cb_field *f) {
  struct cb_field *c;
  unsigned int size;
  int align_size;
  int pad;

  if (f->level == 66) {
    /* rename */
    if (f->rename_thru) {
      f->size =
          f->rename_thru->offset + f->rename_thru->size - f->redefines->offset;
    } else {
      f->size = f->redefines->size;
    }
    return f->size;
  }

  if (f->children) {
    /* groups */
    size = 0;
    for (c = f->children; c; c = c->sister) {
      if (c->redefines) {
        c->offset = c->redefines->offset;
        compute_size(c);
        /* increase the size if redefinition is larger */
        if (c->level != 66 &&
            c->size * c->occurs_max >
                c->redefines->size * c->redefines->occurs_max) {
          if (cb_larger_redefines_ok) {
            cb_warning_x(CB_TREE(c), _("Size of '%s' larger than size of '%s'"),
                         c->name, c->redefines->name);
            size += (c->size * c->occurs_max) -
                    (c->redefines->size * c->redefines->occurs_max);
          } else {
            cb_error_x(CB_TREE(c), _("Size of '%s' larger than size of '%s'"),
                       c->name, c->redefines->name);
          }
        }
      } else {
        c->offset = f->offset + size;
        size += compute_size(c) * c->occurs_max;

        /* word alignment */
        if (c->flag_synchronized && cb_verify(cb_synchronized_clause, "SYNC")) {
          align_size = 1;
          switch (c->usage) {
          case CB_USAGE_BINARY:
          case CB_USAGE_COMP_5:
          case CB_USAGE_COMP_X:
          case CB_USAGE_FLOAT:
          case CB_USAGE_DOUBLE:
            if (c->size == 2 || c->size == 4 || c->size == 8) {
              align_size = c->size;
            }
            break;
          case CB_USAGE_INDEX:
          case CB_USAGE_LENGTH:
            align_size = sizeof(int);
            break;
          case CB_USAGE_OBJECT:
          case CB_USAGE_POINTER:
          case CB_USAGE_PROGRAM_POINTER:
          case CB_USAGE_PROGRAM:
            align_size = sizeof(void *);
            break;
          default:
            break;
          }
          if (c->offset % align_size != 0) {
            pad = align_size - (c->offset % align_size);
            c->offset += pad;
            size += pad;
          }
        }
      }
    }
    /* extra check for group size */
    if (size > INT_MAX) {
      cb_error_x(CB_TREE(f), _("Size of '%s' exceed maximum '%d'"), f->name,
                 INT_MAX);
    }
    f->size = (int)size;
  } else {
    /* elementary item */
    switch (f->usage) {
    case CB_USAGE_COMP_X:
      if (f->pic->category == CB_CATEGORY_ALPHANUMERIC) {
        break;
      }
      size = f->pic->size;
      f->size =
          ((size <= 2)
               ? 1
               : (size <= 4)
                     ? 2
                     : (size <= 7)
                           ? 3
                           : (size <= 9)
                                 ? 4
                                 : (size <= 12)
                                       ? 5
                                       : (size <= 14)
                                             ? 6
                                             : (size <= 16)
                                                   ? 7
                                                   : (size <= 18) ? 8 : 16);
      break;
    case CB_USAGE_BINARY:
    case CB_USAGE_COMP_5:
      size = f->pic->size;
      if (size > 18) {
        f->flag_binary_swap = 0;
        cb_error_x(CB_TREE(f),
                   _("'%s' binary field cannot be larger than 18 digits"),
                   f->name);
      }
      switch (cb_binary_size) {
      case CB_BINARY_SIZE_2_4_8:
        if (f->flag_real_binary && size <= 2) {
          f->size = 1;
        } else {
          f->size = ((size <= 4) ? 2 : (size <= 9) ? 4 : (size <= 18) ? 8 : 16);
        }
        break;
      case CB_BINARY_SIZE_1_2_4_8:
        f->size =
            ((size <= 2)
                 ? 1
                 : (size <= 4) ? 2 : (size <= 9) ? 4 : (size <= 18) ? 8 : 16);
        break;
      case CB_BINARY_SIZE_1__8:
        if (f->pic->have_sign) {
          f->size =
              ((size <= 2)
                   ? 1
                   : (size <= 4)
                         ? 2
                         : (size <= 6)
                               ? 3
                               : (size <= 9)
                                     ? 4
                                     : (size <= 11)
                                           ? 5
                                           : (size <= 14)
                                                 ? 6
                                                 : (size <= 16)
                                                       ? 7
                                                       : (size <= 18) ? 8 : 16);
        } else {
          f->size =
              ((size <= 2)
                   ? 1
                   : (size <= 4)
                         ? 2
                         : (size <= 7)
                               ? 3
                               : (size <= 9)
                                     ? 4
                                     : (size <= 12)
                                           ? 5
                                           : (size <= 14)
                                                 ? 6
                                                 : (size <= 16)
                                                       ? 7
                                                       : (size <= 18) ? 8 : 16);
        }
        break;
      }
      break;
    case CB_USAGE_DISPLAY:
      f->size = f->pic->size;
      if (f->pic->category == CB_CATEGORY_NUMERIC && f->pic->have_sign &&
          f->flag_sign_separate) {
        f->size++;
      }
      break;
    case CB_USAGE_PACKED:
      f->size = f->pic->size / 2 + 1;
      break;
    case CB_USAGE_INDEX:
    case CB_USAGE_LENGTH:
      f->size = sizeof(int);
      break;
    case CB_USAGE_FLOAT:
      f->size = sizeof(float);
      break;
    case CB_USAGE_DOUBLE:
      f->size = sizeof(double);
      break;
    case CB_USAGE_OBJECT:
    case CB_USAGE_POINTER:
    case CB_USAGE_PROGRAM_POINTER:
    case CB_USAGE_PROGRAM:
      f->size = sizeof(void *);
      break;
    default:
      ABORT();
    }
  }

  /* the size of redefining field should not be larger than
     the size of redefined field unless the redefined field
     is level 01 and non-external */
  if (f->redefines && f->redefines->flag_external &&
      (f->size * f->occurs_max >
       f->redefines->size * f->redefines->occurs_max)) {
    if (cb_larger_redefines_ok) {
      cb_warning_x(CB_TREE(f), _("Size of '%s' larger than size of '%s'"),
                   f->name, f->redefines->name);
    } else {
      cb_error_x(CB_TREE(f), _("Size of '%s' larger than size of '%s'"),
                 f->name, f->redefines->name);
    }
  }

  return f->size;
}

static int validate_field_value(struct cb_field *f) {
  if (f->values) {
    validate_move(CB_VALUE(f->values), CB_TREE(f), 1);
  }

  if (f->children) {
    for (f = f->children; f; f = f->sister) {
      validate_field_value(f);
    }
  }

  return 0;
}

void cb_validate_field(struct cb_field *f) {
  struct cb_field *c;

  if (validate_field_1(f) != 0) {
    f->flag_invalid = 1;
    return;
  }
  /* RXW - Remove */
  if (f->flag_item_78) {
    f->flag_is_verified = 1;
    return;
  }

  /* setup parameters */
  if (f->storage == CB_STORAGE_LOCAL || f->storage == CB_STORAGE_LINKAGE ||
      f->flag_item_based) {
    f->flag_local = 1;
  }
  if (f->storage == CB_STORAGE_LINKAGE || f->flag_item_based) {
    f->flag_base = 1;
  }
  setup_parameters(f);

  /* compute size */
  compute_size(f);
  if (!f->redefines) {
    f->memory_size = f->size * f->occurs_max;
  } else if (f->redefines->memory_size < f->size * f->occurs_max) {
    f->redefines->memory_size = f->size * f->occurs_max;
  }

  validate_field_value(f);
  if (f->flag_is_global) {
    f->count++;
    for (c = f->children; c; c = c->sister) {
      c->flag_is_global = 1;
      c->count++;
    }
  }
  f->flag_is_verified = 1;
}

void cb_validate_88_item(struct cb_field *f) {
  cb_tree x;

  x = CB_TREE(f);
  if (!f->values) {
    level_require_error(x, "VALUE");
  }

  if (f->pic || f->flag_occurs) {
    level_except_error(x, "VALUE");
  }
}

struct cb_field *cb_validate_78_item(struct cb_field *f) {
  cb_tree x;

  x = CB_TREE(f);
  if (!f->values) {
    level_require_error(x, "VALUE");
  }

  if (f->pic || f->flag_occurs) {
    level_except_error(x, "VALUE");
  }
  cb_add_78(f);
  return last_real_field;
}

void cb_clear_real_field(void) { last_real_field = NULL; }
