/*
 * Copyright (C) 2003-2009 Keisuke Nishida
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
#include "defaults.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cobj.h"

#undef CB_CONFIG_ANY
#undef CB_CONFIG_INT
#undef CB_CONFIG_STRING
#undef CB_CONFIG_CHAR
#undef CB_CONFIG_BOOLEAN
#undef CB_CONFIG_SUPPORT
#define CB_CONFIG_ANY(type, var, name) type var;
#define CB_CONFIG_INT(var, name) int var;
#define CB_CONFIG_STRING(var, name) const char *var;
#define CB_CONFIG_CHAR(var, name) char var;
#define CB_CONFIG_BOOLEAN(var, name) int var;
#define CB_CONFIG_SUPPORT(var, name) enum cb_support var;
#include "config.def"

enum cb_config_type {
  ANY,
  INT,     /* integer */
  STRING,  /* "..." */
  CHAR,    /* single character */
  BOOLEAN, /* 'yes', 'no' */
  SUPPORT  /* 'ok', 'archaic', 'obsolete',
              'skip', 'ignore', 'unconformable' */
};

struct noreserve *norestab = NULL;

static struct {
  const enum cb_config_type type;
  const char *name;
  void *var;
  char *val;
} config_table[] = {{STRING, "include", NULL, NULL},
                    {STRING, "not-reserved", NULL, NULL},
#undef CB_CONFIG_ANY
#undef CB_CONFIG_INT
#undef CB_CONFIG_STRING
#undef CB_CONFIG_CHAR
#undef CB_CONFIG_BOOLEAN
#undef CB_CONFIG_SUPPORT
#define CB_CONFIG_ANY(type, var, name) {ANY, name, &var, NULL},
#define CB_CONFIG_INT(var, name) {INT, name, &var, NULL},
#define CB_CONFIG_STRING(var, name) {STRING, name, &var, NULL},
#define CB_CONFIG_CHAR(var, name) {CHAR, name, &var, NULL},
#define CB_CONFIG_BOOLEAN(var, name) {BOOLEAN, name, &var, NULL},
#define CB_CONFIG_SUPPORT(var, name) {SUPPORT, name, &var, NULL},
#include "config.def"
                    {0, NULL, NULL, NULL}};

static char *read_string(const char *text) {
  char *p;
  char *s = strdup(text);

  if (*s == '\"') {
    s++;
  }
  for (p = s; *p; p++) {
    if (*p == '\"') {
      *p = '\0';
    }
  }
  return s;
}

static void invalid_value(const char *fname, const int line, const char *name) {
  fprintf(stderr, _("%s:%d: invalid value for '%s'\n"), fname, line, name);
}

static void unsupported_value(const char *fname, const int line,
                              const char *val) {
  fprintf(stderr, _("%s:%d: '%s' not supported\n"), fname, line, val);
}

int cb_load_std(const char *name) { return cb_load_conf(name, 1, 1); }

int cb_load_conf(const char *fname, const int check_nodef,
                 const int prefix_dir) {
  char *s;
  char *e;
  const char *name;
  const char *val;
  void *var;
  FILE *fp;
  char *nores;
  struct noreserve *noresptr;
  int i;
  int j;
  int ret;
  int saveret;
  int line;
  char buff[COB_SMALL_BUFF];

  /* initialize the config table */
  if (check_nodef) {
    for (i = 0; config_table[i].name; i++) {
      config_table[i].val = NULL;
    }
  }

  if (prefix_dir) {
#ifdef _WIN32
    snprintf(buff, COB_SMALL_MAX, "%s\\%s", cob_config_dir, fname);
#else
    snprintf(buff, COB_SMALL_MAX, "%s/%s", cob_config_dir, fname);
#endif
    name = buff;
  } else {
    name = fname;
  }
  /* open the config file */
  fp = fopen(name, "r");
  if (fp == NULL) {
    perror(name);
    return -1;
  }

  /* read the config file */
  ret = 0;
  line = 0;
  while (fgets(buff, COB_SMALL_BUFF, fp)) {
    line++;

    /* skip comments */
    if (buff[0] == '#') {
      continue;
    }

    /* skip blank lines */
    for (s = buff; *s; s++) {
      if (isgraph(*s)) {
        break;
      }
    }
    if (!*s) {
      continue;
    }

    /* get the tag */
    s = strpbrk(buff, " \t:=");
    if (!s) {
      fprintf(stderr, "%s:%d: invalid line\n", fname, line);
      ret = -1;
      continue;
    }
    *s = 0;

    /* find the entry */
    for (i = 0; config_table[i].name; i++) {
      if (strcmp(buff, config_table[i].name) == 0) {
        break;
      }
    }
    if (!config_table[i].name) {
      fprintf(stderr, "%s:%d: unknown tag '%s'\n", fname, line, buff);
      ret = -1;
      continue;
    }

    /* get the value */
    for (s++; *s && strchr(" \t:=", *s); s++) {
      ;
    }
    e = s + strlen(s) - 1;
    for (; e >= s && strchr(" \t\r\n", *e); e--) {
      ;
    }
    e[1] = 0;
    config_table[i].val = s;

    /* set the value */
    name = config_table[i].name;
    var = config_table[i].var;
    val = config_table[i].val;
    switch (config_table[i].type) {
    case ANY:
      if (strcmp(name, "assign-clause") == 0) {
        if (strcmp(val, "cobol2002") == 0) {
          unsupported_value(fname, line, val);
          ret = -1;
        } else if (strcmp(val, "mf") == 0) {
          cb_assign_clause = CB_ASSIGN_MF;
        } else if (strcmp(val, "ibm") == 0) {
          cb_assign_clause = CB_ASSIGN_IBM;
        } else if (strcmp(val, "jph1") == 0) {
          cb_assign_clause = CB_ASSIGN_JPH1;
        } else {
          invalid_value(fname, line, name);
          ret = -1;
        }
      } else if (strcmp(name, "binary-size") == 0) {
        if (strcmp(val, "2-4-8") == 0) {
          cb_binary_size = CB_BINARY_SIZE_2_4_8;
        } else if (strcmp(val, "1-2-4-8") == 0) {
          cb_binary_size = CB_BINARY_SIZE_1_2_4_8;
        } else if (strcmp(val, "1--8") == 0) {
          cb_binary_size = CB_BINARY_SIZE_1__8;
        } else {
          invalid_value(fname, line, name);
          ret = -1;
        }
      } else if (strcmp(name, "binary-byteorder") == 0) {
        if (strcmp(val, "little-endian") == 0) {
          cb_binary_byteorder = CB_BYTEORDER_LITTLE_ENDIAN;
        } else if (strcmp(val, "big-endian") == 0) {
          cb_binary_byteorder = CB_BYTEORDER_BIG_ENDIAN;
        } else {
          invalid_value(fname, line, name);
          ret = -1;
        }
      } else if (strcmp(name, "abort-on-io-exception") == 0) {
        if (strcmp(val, "any") == 0) {
          cb_abort_on_io_exception = CB_ABORT_ON_IO_ANY;
        } else if (strcmp(val, "fatal") == 0) {
          cb_abort_on_io_exception = CB_ABORT_ON_IO_FATAL;
        } else if (strcmp(val, "never") == 0) {
          cb_abort_on_io_exception = CB_ABORT_ON_IO_NEVER;
        } else {
          invalid_value(fname, line, name);
          ret = -1;
        }
      } else if (strcmp(name, "default-organization") == 0) {
        if (strcmp(val, "record-sequential") == 0) {
          cb_default_organization = CB_ORG_RECORD_SEQUENTIAL;
        } else if (strcmp(val, "line-sequential") == 0) {
          cb_default_organization = CB_ORG_LINE_SEQUENTIAL;
        } else {
          invalid_value(fname, line, name);
          ret = -1;
        }
      }
      break;
    case INT:
      for (j = 0; val[j]; j++) {
        if (!isdigit(val[j])) {
          invalid_value(fname, line, name);
          ret = -1;
          break;
        }
      }
      *((int *)var) = atoi(val);
      break;
    case STRING:
      val = read_string(val);

      if (strcmp(name, "include") == 0) {
        /* include another conf file */
        saveret = ret;
        if (cb_load_conf(val, 0, 1) != 0) {
          return -1;
        }
        ret = saveret;
      } else if (strcmp(name, "not-reserved") == 0) {
        nores = read_string(val);
        noresptr = cobc_malloc(sizeof(struct noreserve));
        noresptr->noresword = cobc_malloc(strlen(nores) + 1);
        strcpy(noresptr->noresword, nores);
        noresptr->next = norestab;
        norestab = noresptr;
      } else {
        *((const char **)var) = val;
      }
      break;
    case CHAR:
      if (1 != strnlen(val, 2)) {
        invalid_value(fname, line, name);
        ret = -1;
      } else {
        *((char *)var) = *val;
      }
      break;
    case BOOLEAN:
      if (strcmp(val, "yes") == 0) {
        *((int *)var) = 1;
      } else if (strcmp(val, "no") == 0) {
        *((int *)var) = 0;
      } else {
        invalid_value(fname, line, name);
        ret = -1;
      }
      break;
    case SUPPORT:
      if (strcmp(val, "ok") == 0) {
        *((enum cb_support *)var) = CB_OK;
      } else if (strcmp(val, "warning") == 0) {
        *((enum cb_support *)var) = CB_WARNING;
      } else if (strcmp(val, "archaic") == 0) {
        *((enum cb_support *)var) = CB_ARCHAIC;
      } else if (strcmp(val, "obsolete") == 0) {
        *((enum cb_support *)var) = CB_OBSOLETE;
      } else if (strcmp(val, "skip") == 0) {
        *((enum cb_support *)var) = CB_SKIP;
      } else if (strcmp(val, "ignore") == 0) {
        *((enum cb_support *)var) = CB_IGNORE;
      } else if (strcmp(val, "error") == 0) {
        *((enum cb_support *)var) = CB_ERROR;
      } else if (strcmp(val, "unconformable") == 0) {
        *((enum cb_support *)var) = CB_UNCONFORMABLE;
      } else {
        invalid_value(fname, line, name);
        ret = -1;
      }
      break;
    default:
      fprintf(stderr, _("%s:%d: invalid type for '%s'\n"), fname, line, name);
      ret = -1;
      break;
    }
  }
  fclose(fp);

  /* if assign_external is not setted in config file */
  for (i = 0; config_table[i].name; i++) {
    if (config_table[i].val == NULL &&
        strcmp(config_table[i].name, "assign_external") == 0) {
      config_table[i].val = (char *)"no";
      *((int *)config_table[i].var) = 0;
    }
  }

  /* checks for no definition */
  if (check_nodef) {
    for (i = 2; config_table[i].name; i++) {
      if (config_table[i].val == NULL) {
        fprintf(stderr, "%s: no definition of '%s'\n", fname,
                config_table[i].name);
        ret = -1;
      }
    }
  }

  return ret;
}
