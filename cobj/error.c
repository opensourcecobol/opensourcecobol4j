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

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cobj.h"
#include "tree.h"

#define max_names 5

/* Number of source lines displayed before and after the offending line */
#define CARET_CONTEXT_LINES 2
/* Line numbers are printed in a field of at least this width */
#define CARET_LINENO_WIDTH 5

static char *errnamebuff = NULL;
static char *errmsgbuff = NULL;

/* Source file the context is currently read from, the line the stream is
   positioned at, and the buffer one source line is read into.  The file is
   kept open so that a run of diagnostics does not read the source from the
   beginning over and over again. */
static FILE *caret_fd = NULL;
static char caret_file[COB_NORMAL_BUFF] = "";
static int caret_file_line = 0;
static char *caret_line = NULL;
static size_t caret_line_size = 0;

/* Width of the field the line numbers of a source context are printed in */
static int line_number_width(int line) {
  int width = 1;

  while (line >= 10) {
    line /= 10;
    width++;
  }
  return width > CARET_LINENO_WIDTH ? width : CARET_LINENO_WIDTH;
}

/*
 * Read the next line of the source file into caret_line, dropping the
 * trailing whitespace so that no diagnostic ends in a blank.  The buffer
 * grows as needed, so a line is never cut.  Returns 0 at the end of the file.
 */
static int read_source_line(void) {
  size_t len = 0;
  int c;

  if (!caret_line) {
    caret_line_size = COB_MINI_BUFF;
    caret_line = cobc_malloc(caret_line_size);
  }
  while ((c = fgetc(caret_fd)) != EOF && c != '\n') {
    if (len + 1 >= caret_line_size) {
      caret_line_size *= 2;
      caret_line = cobc_realloc(caret_line, caret_line_size);
    }
    caret_line[len++] = (char)c;
  }
  if (c == EOF && len == 0) {
    return 0;
  }
  while (len > 0 &&
         (caret_line[len - 1] == ' ' || caret_line[len - 1] == '\t' ||
          caret_line[len - 1] == '\r')) {
    len--;
  }
  caret_line[len] = 0;
  return 1;
}

/*
 * Display the source lines surrounding the location of a diagnostic,
 * marking the offending line with '>'.  Only used when
 * cb_diagnostics_show_caret is set.
 *
 * The source file is read directly, so the text shown is the original one,
 * before COPY ... REPLACING and friends have been applied.  There is no caret
 * for the offending column since only the line number is tracked.
 */
static void print_source_context(const int line) {
  const int line_start =
      line > CARET_CONTEXT_LINES ? line - CARET_CONTEXT_LINES : 1;
  const int line_end = line + CARET_CONTEXT_LINES;
  const int width = line_number_width(line_end);

  if (caret_file_line > line_start) {
    rewind(caret_fd);
    caret_file_line = 1;
  }

  while (caret_file_line <= line_end) {
    const int line_pos = caret_file_line;

    if (!read_source_line()) {
      /* no more source to display */
      break;
    }
    caret_file_line++;
    if (line_pos < line_start) {
      continue;
    }

    if (cb_diagnostics_show_line_numbers) {
      fprintf(stderr, "%*d %c", width, line_pos, line == line_pos ? '>' : '|');
    } else {
      fprintf(stderr, " %c", line == line_pos ? '>' : ' ');
    }
    /* an empty source line must not leave trailing whitespace behind */
    if (caret_line[0]) {
      fprintf(stderr, " %s", caret_line);
    }
    fputc('\n', stderr);
  }
}

/*
 * Print the source context of the diagnostic just written for file:line.
 * Consecutive diagnostics pointing at the same location share a single
 * context display.
 */
static void print_error_source_context(const char *file, const int line) {
  static char last_caret_file[COB_NORMAL_BUFF] = "";
  static int last_caret_line = 0;

  if (!cb_diagnostics_show_caret || !file || line <= 0) {
    return;
  }
  if (last_caret_line == line && !strcmp(last_caret_file, file)) {
    return;
  }

  if (!caret_fd || strcmp(caret_file, file)) {
    if (caret_fd) {
      fclose(caret_fd);
    }
    caret_fd = fopen(file, "r");
    if (!caret_fd) {
      caret_file[0] = 0;
      return;
    }
    snprintf(caret_file, sizeof(caret_file), "%s", file);
    caret_file_line = 1;
  }

  snprintf(last_caret_file, sizeof(last_caret_file), "%s", file);
  last_caret_line = line;
  print_source_context(line);
}

static void print_error(char *file, int line, const char *prefix,
                        const char *fmt, va_list ap) {
  int flag_too_many_names = 0;
  static struct cb_label *last_section = NULL;
  static struct cb_label *last_paragraph = NULL;
  int cnt = 0;
  const char *pfmt, *pstr;
  void *param;
  void *allname[max_names];
  void *p_bfree[max_names];

  file = file ? file : cb_source_file;
  line = line ? line : cb_source_line;

  /* print the paragraph or section name */
  if (current_section != last_section || current_paragraph != last_paragraph) {
    char msgword[COB_MINI_BUFF];
    if (current_paragraph && strstr((const char *)(current_paragraph->name),
                                    "_SECTION__DEFAULT_PARAGRAPH") == NULL) {
      cb_get_jisword_buff((const char *)current_paragraph->name, msgword,
                          sizeof(msgword));
      fprintf(stderr, _("%s: In paragraph '%s':\n"), file, msgword);
    } else if (current_section &&
               strcmp((const char *)(current_section->name), "MAIN")) {
      cb_get_jisword_buff((const char *)current_section->name, msgword,
                          sizeof(msgword));
      fprintf(stderr, _("%s: In section '%s':\n"), file, msgword);
    }
    last_section = current_section;
    last_paragraph = current_paragraph;
  }

  /* print error */
  fprintf(stderr, "%s:%d: %s", file, line, prefix);
  if (!errmsgbuff) {
    errmsgbuff = cobc_malloc(COB_NORMAL_BUFF);
  }

  /* decode arguments */
  memset(p_bfree, 0, sizeof(p_bfree));
  pfmt = fmt;
  while ((pstr = strchr(pfmt, '%')) != NULL) {
    while (isalnum(*(pstr += 1)) != 0) {
      if (isalpha(*pstr) != 0) {
        param = va_arg(ap, void *);
        switch (*pstr) {
        case 's':
          allname[cnt] = p_bfree[cnt] = cb_get_jisword(param);
          cnt++;
          break;
        case 'c':
        case 'd':
        default:
          allname[cnt] = param;
          cnt++;
          break;
        }
      }
    }
    if (cnt > max_names) {
      flag_too_many_names = 1;
      break;
    }
    pfmt = pstr;
  }

  if (flag_too_many_names) {
    fputs(_("Internal error: Too many params in message. output suppressed.\n"),
          stderr);
  } else {
    /* error output */
    switch (cnt) {
    case 0:
      snprintf(errmsgbuff, COB_NORMAL_BUFF, fmt);
      break;
    case 1:
      snprintf(errmsgbuff, COB_NORMAL_BUFF, fmt, allname[0]);
      break;
    case 2:
      snprintf(errmsgbuff, COB_NORMAL_BUFF, fmt, allname[0], allname[1]);
      break;
    case 3:
      snprintf(errmsgbuff, COB_NORMAL_BUFF, fmt, allname[0], allname[1],
               allname[2]);
      break;
    case 4:
      snprintf(errmsgbuff, COB_NORMAL_BUFF, fmt, allname[0], allname[1],
               allname[2], allname[3]);
      break;
    case 5:
    default:
      /* upper limit of cnt should be checked above. */
      snprintf(errmsgbuff, COB_NORMAL_BUFF, fmt, allname[0], allname[1],
               allname[2], allname[3], allname[4]);
      break;
    }
    fprintf(stderr, "%s\n", errmsgbuff);
  }
  for (cnt = 0; cnt < max_names; cnt++) {
    if (p_bfree[cnt]) {
      free(p_bfree[cnt]);
    }
  }

  print_error_source_context(file, line);
}

char *check_filler_name(char *name) {
  if (!memcmp(name, "WORK$", 5)) {
    name = (char *)"FILLER";
  }
  return name;
}

void cb_warning(const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  print_error(NULL, 0, "Warning: ", fmt, ap);
  va_end(ap);

  warningcount++;
}

void cb_error(const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  print_error(NULL, 0, "Error: ", fmt, ap);
  va_end(ap);

  errorcount++;
}

void cb_warning_x(cb_tree x, const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  print_error((char *)(x->source_file), x->source_line, "Warning: ", fmt, ap);
  va_end(ap);

  warningcount++;
}

void cb_error_x(cb_tree x, const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  print_error((char *)(x->source_file), x->source_line, "Error: ", fmt, ap);
  va_end(ap);

  errorcount++;
}

int cb_verify(const enum cb_support tag, const char *feature) {
  switch (tag) {
  case CB_OK:
    return 1;
  case CB_WARNING:
    return 1;
  case CB_ARCHAIC:
    if (cb_warn_archaic) {
      cb_warning(_("%s is archaic in %s"), feature, cb_config_name);
    }
    return 1;
  case CB_OBSOLETE:
    if (cb_warn_obsolete) {
      cb_warning(_("%s is obsolete in %s"), feature, cb_config_name);
    }
    return 1;
  case CB_SKIP:
    return 0;
  case CB_IGNORE:
    cb_warning(_("%s ignored"), feature);
    return 0;
  case CB_ERROR:
    return 0;
  case CB_UNCONFORMABLE:
    cb_error(_("%s does not conform to %s"), feature, cb_config_name);
    return 0;
  }
  return 0;
}

void redefinition_error(cb_tree x) {
  struct cb_word *w;

  w = CB_REFERENCE(x)->word;
  cb_error_x(x, _("Redefinition of '%s'"), w->name);
  cb_error_x(CB_VALUE(w->items), _("'%s' previously defined here"), w->name);
}

void redefinition_warning(cb_tree x, cb_tree y) {
  struct cb_word *w;

  w = CB_REFERENCE(x)->word;
  cb_warning_x(x, _("Redefinition of '%s'"), w->name);
  if (y) {
    cb_warning_x(y, _("'%s' previously defined here"), w->name);
  } else {
    cb_warning_x(CB_VALUE(w->items), _("'%s' previously defined here"),
                 w->name);
  }
}

#define jisword_cat(w, buf, len)                                               \
  {                                                                            \
    size_t n = strnlen((buf), (len));                                          \
    cb_get_jisword_buff((w), &((buf)[n]), (len) + 1 - n);                      \
  }

#define jisword_quote(w, buf, len)                                             \
  {                                                                            \
    strncpy((buf), "'", (len));                                                \
    cb_get_jisword_buff((w), &((buf)[1]), (len));                              \
    strncat((buf), "'", (len));                                                \
  }

void undefined_error(cb_tree x) {
  struct cb_reference *r;
  cb_tree c;

  if (!errnamebuff) {
    errnamebuff = cobc_malloc(COB_NORMAL_BUFF);
  }
  r = CB_REFERENCE(x);
  jisword_quote(CB_NAME(x), errnamebuff, COB_NORMAL_MAX);
  for (c = r->chain; c; c = CB_REFERENCE(c)->chain) {
    strncat(errnamebuff, " in '", COB_NORMAL_MAX);
    jisword_cat(CB_NAME(c), errnamebuff, COB_NORMAL_MAX);
    strncat(errnamebuff, "'", COB_NORMAL_MAX);
  }
  cb_error_x(x, _("%s undefined"), errnamebuff);
}

void ambiguous_error(cb_tree x) {
  struct cb_word *w;
  struct cb_field *p;

  w = CB_REFERENCE(x)->word;
  if (w->error == 0) {
    cb_tree l;
    if (!errnamebuff) {
      errnamebuff = cobc_malloc(COB_NORMAL_BUFF);
    }
    /* display error on the first time */
    jisword_quote(CB_NAME(x), errnamebuff, COB_NORMAL_MAX);
    for (l = CB_REFERENCE(x)->chain; l; l = CB_REFERENCE(l)->chain) {
      strncat(errnamebuff, " in '", COB_NORMAL_MAX);
      jisword_cat(CB_NAME(l), errnamebuff, COB_NORMAL_MAX);
      strncat(errnamebuff, "'", COB_NORMAL_MAX);
    }
    cb_error_x(x, _("%s ambiguous; need qualification"), errnamebuff);
    w->error = 1;

    /* display all fields with the same name */
    struct cb_label *l2;
    for (l = w->items; l; l = CB_CHAIN(l)) {
      cb_tree y = CB_VALUE(l);
      jisword_quote(w->name, errnamebuff, COB_NORMAL_MAX);
      switch (CB_TREE_TAG(y)) {
      case CB_TAG_FIELD:
        for (p = CB_FIELD(y)->parent; p; p = p->parent) {
          strncat(errnamebuff, " in '", COB_NORMAL_MAX);
          jisword_cat(check_filler_name((char *)p->name), errnamebuff,
                      COB_NORMAL_MAX);
          strncat(errnamebuff, "'", COB_NORMAL_MAX);
        }
        break;
      case CB_TAG_LABEL:
        l2 = CB_LABEL(y);
        if (l2->section) {
          strncat(errnamebuff, " in '", COB_NORMAL_MAX);
          jisword_cat((const char *)(l2->section->name), errnamebuff,
                      COB_NORMAL_MAX);
          strncat(errnamebuff, "'", COB_NORMAL_MAX);
        }
        break;
      default:
        break;
      }
      strcat(errnamebuff, _(" defined here"));
      cb_error_x(y, errnamebuff);
    }
  }
}

void group_error(cb_tree x, const char *clause) {
  cb_error_x(x, _("Group item '%s' cannot have %s clause"),
             check_filler_name(cb_name(x)), clause);
}

void level_redundant_error(cb_tree x, const char *clause) {
  cb_error_x(x, _("Level %02d item '%s' cannot have %s clause"),
             cb_field(x)->level, check_filler_name(cb_name(x)), clause);
}

void level_require_error(cb_tree x, const char *clause) {
  cb_error_x(x, _("Level %02d item '%s' requires %s clause"),
             cb_field(x)->level, check_filler_name(cb_name(x)), clause);
}

void level_except_error(cb_tree x, const char *clause) {
  cb_error_x(x, _("Level %02d item '%s' cannot have other than %s clause"),
             cb_field(x)->level, check_filler_name(cb_name(x)), clause);
}
