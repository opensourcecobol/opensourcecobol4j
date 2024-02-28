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
#include <stdlib.h>
#include <string.h>

#include "cobj.h"
#include "parser.h"
#include "tree.h"

static struct {
  const char *name;
  const enum cb_system_name_category category;
  const int token;
  cb_tree node;
} system_table[] = {
    {"SYSIN", CB_DEVICE_NAME, CB_DEVICE_SYSIN, NULL},
    {"SYSIPT", CB_DEVICE_NAME, CB_DEVICE_SYSIN, NULL},
    {"SYSOUT", CB_DEVICE_NAME, CB_DEVICE_SYSOUT, NULL},
    {"SYSLIST", CB_DEVICE_NAME, CB_DEVICE_SYSOUT, NULL},
    {"SYSLST", CB_DEVICE_NAME, CB_DEVICE_SYSOUT, NULL},
    {"PRINTER", CB_DEVICE_NAME, CB_DEVICE_SYSOUT, NULL},
    {"SYSERR", CB_DEVICE_NAME, CB_DEVICE_SYSERR, NULL},
    {"CONSOLE", CB_DEVICE_NAME, CB_DEVICE_CONSOLE, NULL},
    {"C01", CB_FEATURE_NAME, CB_FEATURE_C01, NULL},
    {"C02", CB_FEATURE_NAME, CB_FEATURE_C02, NULL},
    {"C03", CB_FEATURE_NAME, CB_FEATURE_C03, NULL},
    {"C04", CB_FEATURE_NAME, CB_FEATURE_C04, NULL},
    {"C05", CB_FEATURE_NAME, CB_FEATURE_C05, NULL},
    {"C06", CB_FEATURE_NAME, CB_FEATURE_C06, NULL},
    {"C07", CB_FEATURE_NAME, CB_FEATURE_C07, NULL},
    {"C08", CB_FEATURE_NAME, CB_FEATURE_C08, NULL},
    {"C09", CB_FEATURE_NAME, CB_FEATURE_C09, NULL},
    {"C10", CB_FEATURE_NAME, CB_FEATURE_C10, NULL},
    {"C11", CB_FEATURE_NAME, CB_FEATURE_C11, NULL},
    {"C12", CB_FEATURE_NAME, CB_FEATURE_C12, NULL},
    {"FORMFEED", CB_FEATURE_NAME, CB_FEATURE_FORMFEED, NULL},
    {"SWITCH-1", CB_SWITCH_NAME, CB_SWITCH_1, NULL},
    {"SWITCH-2", CB_SWITCH_NAME, CB_SWITCH_2, NULL},
    {"SWITCH-3", CB_SWITCH_NAME, CB_SWITCH_3, NULL},
    {"SWITCH-4", CB_SWITCH_NAME, CB_SWITCH_4, NULL},
    {"SWITCH-5", CB_SWITCH_NAME, CB_SWITCH_5, NULL},
    {"SWITCH-6", CB_SWITCH_NAME, CB_SWITCH_6, NULL},
    {"SWITCH-7", CB_SWITCH_NAME, CB_SWITCH_7, NULL},
    {"SWITCH-8", CB_SWITCH_NAME, CB_SWITCH_8, NULL},
    {"UPSI-0", CB_SWITCH_NAME, CB_SWITCH_1, NULL},
    {"UPSI-1", CB_SWITCH_NAME, CB_SWITCH_2, NULL},
    {"UPSI-2", CB_SWITCH_NAME, CB_SWITCH_3, NULL},
    {"UPSI-3", CB_SWITCH_NAME, CB_SWITCH_4, NULL},
    {"UPSI-4", CB_SWITCH_NAME, CB_SWITCH_5, NULL},
    {"UPSI-5", CB_SWITCH_NAME, CB_SWITCH_6, NULL},
    {"UPSI-6", CB_SWITCH_NAME, CB_SWITCH_7, NULL},
    {"UPSI-7", CB_SWITCH_NAME, CB_SWITCH_8, NULL},
    {"ARGUMENT-NUMBER", CB_INTERFACE_NAME, CB_ARGUMENT_NUMBER, NULL},
    {"ARGUMENT-VALUE", CB_INTERFACE_NAME, CB_ARGUMENT_VALUE, NULL},
    {"ENVIRONMENT-NAME", CB_INTERFACE_NAME, CB_ENVIRONMENT_NAME, NULL},
    {"ENVIRONMENT-VALUE", CB_INTERFACE_NAME, CB_ENVIRONMENT_VALUE, NULL},
    {NULL, 0, 0, NULL}};

struct reserved {
  const char *name;
  const int token;
};

static const struct reserved reserved_words[] = {
    {"ACCEPT", ACCEPT},                           /* 2002 */
    {"ACCESS", ACCESS},                           /* 2002 */
    {"ACTIVE-CLASS", -1},                         /* 2002 */
    {"ADD", ADD},                                 /* 2002 */
    {"ADDRESS", ADDRESS},                         /* 2002 */
    {"ADVANCING", ADVANCING},                     /* 2002 */
    {"AFTER", AFTER},                             /* 2002 */
    {"ALIGNED", -1},                              /* 2002 */
    {"ALL", ALL},                                 /* 2002 */
    {"ALLOCATE", ALLOCATE},                       /* 2002 */
    {"ALPHABET", ALPHABET},                       /* 2002 */
    {"ALPHABETIC", ALPHABETIC},                   /* 2002 */
    {"ALPHABETIC-LOWER", ALPHABETIC_LOWER},       /* 2002 */
    {"ALPHABETIC-UPPER", ALPHABETIC_UPPER},       /* 2002 */
    {"ALPHANUMERIC", ALPHANUMERIC},               /* 2002 */
    {"ALPHANUMERIC-EDITED", ALPHANUMERIC_EDITED}, /* 2002 */
    {"ALSO", ALSO},                               /* 2002 */
    {"ALTER", ALTER},                             /* 85 */
    {"ALTERNATE", ALTERNATE},                     /* 2002 */
    {"AND", AND},                                 /* 2002 */
    {"ANY", ANY},                                 /* 2002 */
    {"ANYCASE", -1},                              /* 2002 */
    {"APPLY", APPLY},                             /* JP */
    {"ARE", ARE},                                 /* 2002 */
    {"AREA", AREA},                               /* 2002 */
    {"AREAS", AREA},                              /* 2002 */
    {"ARGUMENT-NUMBER", ARGUMENT_NUMBER},         /* extension */
    {"ARGUMENT-VALUE", ARGUMENT_VALUE},           /* extension */
    {"ARITHMETIC", -1},                           /* 2002 (C/S) */
    {"AS", AS},                                   /* 2002 */
    {"ASCENDING", ASCENDING},                     /* 2002 */
    {"ASSIGN", ASSIGN},                           /* 2002 */
    {"AT", AT},                                   /* 2002 */
    {"ATTRIBUTE", -1},                            /* 2002 (C/S) */
    {"AUTO", AUTO},                               /* 2002 (C/S) */
    {"AUTO-SKIP", AUTO},                          /* extension */
    {"AUTOMATIC", AUTOMATIC},                     /* extension */
    {"AUTOTERMINATE", AUTO},                      /* extension */
    {"B-AND", -1},                                /* 2002 */
    {"B-NOT", -1},                                /* 2002 */
    {"B-OR", -1},                                 /* 2002 */
    {"B-XOR", -1},                                /* 2002 */
    {"BACKGROUND-COLOR", BACKGROUND_COLOR},       /* 2002 (C/S) */
    {"BASED", BASED},                             /* 2002 */
    {"BEEP", BELL},                               /* extension */
    {"BEFORE", BEFORE},                           /* 2002 */
    {"BELL", BELL},                               /* 2002 (C/S) */
    {"BINARY", BINARY},                           /* 2002 */
    {"BINARY-C-LONG", BINARY_C_LONG},             /* Extension */
    {"BINARY-CHAR", BINARY_CHAR},                 /* 2002 */
    {"BINARY-DOUBLE", BINARY_DOUBLE},             /* 2002 */
    {"BINARY-LONG", BINARY_LONG},                 /* 2002 */
    {"BINARY-SHORT", BINARY_SHORT},               /* 2002 */
    {"BIT", -1},                                  /* 2002 */
    {"BLANK", BLANK},                             /* 2002 */
    {"BLINK", BLINK},                             /* 2002 (C/S) */
    {"BLOCK", BLOCK},                             /* 2002 */
    {"BOOLEAN", -1},                              /* 2002 */
    {"BOTTOM", BOTTOM},                           /* 2002 */
    {"BY", BY},                                   /* 2002 */
    {"BYTE-LENGTH", BYTE_LENGTH},                 /* 2002 (C/S) */
    {"CALL", CALL},                               /* 2002 */
    {"CANCEL", CANCEL},                           /* 2002 */
    {"CD", -1},                                   /* 2002 */
    {"CENTER", -1},                               /* 2002 (C/S) */
    {"CF", CONTROL_FOOTING},                      /* 2002 */
    {"CH", CONTROL_HEADING},                      /* 2002 */
    {"CHAIN", -1},                                /* extension */
    {"CHAINING", CHAINING},                       /* extension */
    {"CHARACTER", CHARACTER},                     /* 2002 */
    {"CHARACTERS", CHARACTERS},                   /* 2002 */
    {"CLASS", CLASS},                             /* 2002 */
    {"CLASS-ID", -1},                             /* 2002 */
    {"CLASSIFICATION", -1},                       /* 2002 (C/S) */
    {"CLOSE", CLOSE},                             /* 2002 */
    {"CLOSE-NOFEED", CLOSE_NOFEED},               /* JP */
    {"CODE", CODE},                               /* 2002 */
    {"CODE-SET", CODE_SET},                       /* 2002 */
    {"COL", COLUMN},                              /* 2002 */
    {"COLLATING", COLLATING},                     /* 2002 */
    {"COLS", COLUMNS},                            /* 2002 */
    {"COLUMN", COLUMN},                           /* 2002 */
    {"COLUMNS", COLUMNS},                         /* 2002 */
    {"COMMA", COMMA},                             /* 2002 */
    {"COMMAND-LINE", COMMAND_LINE},               /* extension */
    {"COMMIT", COMMIT},                           /* extension */
    {"COMMITMENT-CONTROL", COMMITMENT_CONTROL},   /* JP */
    {"COMMON", COMMON},                           /* 2002 */
    {"COMMUNICATION", -1},                        /* 2002 */
    {"COMP", COMP},                               /* 2002 */
#ifdef __MVS__                                    /* EBCDIC! */
    {"COMP-X", COMP_X},                           /* extension */
#endif
    {"COMP-1", COMP_1}, /* extension */
    {"COMP-2", COMP_2}, /* extension */
    {"COMP-3", COMP_3}, /* extension */
    {"COMP-4", COMP_4}, /* extension */
    {"COMP-5", COMP_5}, /* extension */
#ifndef __MVS__
    {"COMP-X", COMP_X}, /* extension */
#endif
    {"COMPUTATIONAL", COMP},     /* 2002 */
#ifdef __MVS__                   /* EBCDIC! */
    {"COMPUTATIONAL-X", COMP_X}, /* extension */
#endif
    {"COMPUTATIONAL-1", COMP_1}, /* extension */
    {"COMPUTATIONAL-2", COMP_2}, /* extension */
    {"COMPUTATIONAL-3", COMP_3}, /* extension */
    {"COMPUTATIONAL-4", COMP_4}, /* extension */
    {"COMPUTATIONAL-5", COMP_5}, /* extension */
#ifndef __MVS__
    {"COMPUTATIONAL-X", COMP_X}, /* extension */
#endif
    {"COMPUTE", COMPUTE},                     /* 2002 */
    {"CONDITION", -1},                        /* 2002 */
    {"CONFIGURATION", CONFIGURATION},         /* 2002 */
    {"CONSTANT", CONSTANT},                   /* 2002 */
    {"CONTAINS", CONTAINS},                   /* 2002 */
    {"CONTENT", CONTENT},                     /* 2002 */
    {"CONTINUE", CONTINUE},                   /* 2002 */
    {"CONTROL", CONTROL},                     /* 2002 */
    {"CONTROLS", CONTROL},                    /* 2002 */
    {"CONVERTING", CONVERTING},               /* 2002 */
    {"COPY", 0},                              /* 2002 */
    {"CORE-INDEX", CORE_INDEX},               /* JP */
    {"CORR", CORRESPONDING},                  /* 2002 */
    {"CORRESPONDING", CORRESPONDING},         /* 2002 */
    {"COUNT", COUNT},                         /* 2002 */
    {"CRT", CRT},                             /* 2002 */
    {"CURRENCY", CURRENCY},                   /* 2002 */
    {"CURSOR", CURSOR},                       /* 2002 */
    {"CYCLE", CYCLE},                         /* 2002 (C/S) */
    {"CYL-OVERFLOW", CYL_OVERFLOW},           /* JP */
    {"DATA", DATA},                           /* 2002 */
    {"DATA-POINTER", -1},                     /* 2002 */
    {"DATE", DATE},                           /* 2002 */
    {"DAY", DAY},                             /* 2002 */
    {"DAY-OF-WEEK", DAY_OF_WEEK},             /* 2002 */
    {"DE", DETAIL},                           /* 2002 */
    {"DEBUGGING", DEBUGGING},                 /* 2002 */
    {"DECIMAL-POINT", DECIMAL_POINT},         /* 2002 */
    {"DECLARATIVES", DECLARATIVES},           /* 2002 */
    {"DEFAULT", DEFAULT},                     /* 2002 */
    {"DELETE", DELETE},                       /* 2002 */
    {"DELIMITED", DELIMITED},                 /* 2002 */
    {"DELIMITER", DELIMITER},                 /* 2002 */
    {"DEPENDING", DEPENDING},                 /* 2002 */
    {"DESCENDING", DESCENDING},               /* 2002 */
    {"DESTINATION", -1},                      /* 2002 */
    {"DETAIL", DETAIL},                       /* 2002 */
    {"DISABLE", -1},                          /* 2002 */
    {"DISK", DISK},                           /* extension */
    {"DISPLAY", DISPLAY},                     /* 2002 */
    {"DIVIDE", DIVIDE},                       /* 2002 */
    {"DIVISION", DIVISION},                   /* 2002 */
    {"DOWN", DOWN},                           /* 2002 */
    {"DUPLICATES", DUPLICATES},               /* 2002 */
    {"DYNAMIC", DYNAMIC},                     /* 2002 */
    {"EBCDIC", EBCDIC},                       /* extension */
    {"EC", -1},                               /* 2002 */
    {"EGI", -1},                              /* 2002 */
    {"ELSE", ELSE},                           /* 2002 */
    {"EMI", -1},                              /* 2002 */
    {"ENABLE", -1},                           /* 2002 */
    {"END", END},                             /* 2002 */
    {"END-ACCEPT", END_ACCEPT},               /* 2002 */
    {"END-ADD", END_ADD},                     /* 2002 */
    {"END-CALL", END_CALL},                   /* 2002 */
    {"END-COMPUTE", END_COMPUTE},             /* 2002 */
    {"END-DELETE", END_DELETE},               /* 2002 */
    {"END-DISPLAY", END_DISPLAY},             /* 2002 */
    {"END-DIVIDE", END_DIVIDE},               /* 2002 */
    {"END-EVALUATE", END_EVALUATE},           /* 2002 */
    {"END-IF", END_IF},                       /* 2002 */
    {"END-MULTIPLY", END_MULTIPLY},           /* 2002 */
    {"END-OF-PAGE", EOP},                     /* 2002 */
    {"END-PERFORM", END_PERFORM},             /* 2002 */
    {"END-READ", END_READ},                   /* 2002 */
    {"END-RECEIVE", -1},                      /* 2002 */
    {"END-RETURN", END_RETURN},               /* 2002 */
    {"END-REWRITE", END_REWRITE},             /* 2002 */
    {"END-SEARCH", END_SEARCH},               /* 2002 */
    {"END-START", END_START},                 /* 2002 */
    {"END-STRING", END_STRING},               /* 2002 */
    {"END-SUBTRACT", END_SUBTRACT},           /* 2002 */
    {"END-UNSTRING", END_UNSTRING},           /* 2002 */
    {"END-WRITE", END_WRITE},                 /* 2002 */
    {"ENTRY", ENTRY},                         /* extension */
    {"ENTRY-CONVENTION", -1},                 /* 2002 (C/S) */
    {"ENVIRONMENT", ENVIRONMENT},             /* 2002 */
    {"ENVIRONMENT-NAME", ENVIRONMENT_NAME},   /* extension */
    {"ENVIRONMENT-VALUE", ENVIRONMENT_VALUE}, /* extension */
    {"EO", -1},                               /* 2002 */
    {"EOL", EOL},                             /* 2002 (C/S) */
    {"EOP", EOP},                             /* 2002 */
    {"EOS", EOS},                             /* 2002 (C/S) */
    {"EQUAL", EQUAL},                         /* 2002 */
    {"EQUALS", EQUALS},                       /* extension */
    {"ERASE", ERASE},                         /* 2002 (C/S) */
    {"ERROR", ERROR},                         /* 2002 */
    {"ESCAPE", ESCAPE},                       /* extension */
    {"ESI", -1},                              /* 2002 */
    {"EVALUATE", EVALUATE},                   /* 2002 */
    {"EXCEPTION", EXCEPTION},                 /* 2002 */
    {"EXCEPTION-OBJECT", -1},                 /* 2002 */
    {"EXCLUSIVE", EXCLUSIVE},                 /* extension */
    {"EXIT", EXIT},                           /* 2002 */
    {"EXPANDS", -1},                          /* 2002 (C/S) */
    {"EXTEND", EXTEND},                       /* 2002 */
    {"EXTERNAL", EXTERNAL},                   /* 2002 */
    {"FACTORY", -1},                          /* 2002 */
    {"FALSE", TOK_FALSE},                     /* 2002 */
    {"FD", FD},                               /* 2002 */
    {"FILE", TOK_FILE},                       /* 2002 */
    {"FILE-CONTROL", FILE_CONTROL},           /* 2002 */
    {"FILE-ID", FILE_ID},                     /* extension */
    {"FILLER", FILLER},                       /* 2002 */
    {"FINAL", FINAL},                         /* 2002 */
    {"FIRST", FIRST},                         /* 2002 */
    {"FLOAT-BINARY-16", -1},                  /* 2008 */
    {"FLOAT-BINARY-34", -1},                  /* 2008 */
    {"FLOAT-BINARY-7", -1},                   /* 2008 */
    {"FLOAT-DECIMAL-16", -1},                 /* 2008 */
    {"FLOAT-DECIMAL-34", -1},                 /* 2008 */
    {"FLOAT-EXTENDED", -1},                   /* 2002 */
    {"FLOAT-LONG", COMP_2},                   /* 2002 */
    {"FLOAT-SHORT", COMP_1},                  /* 2002 */
    {"FOOTING", FOOTING},                     /* 2002 */
    {"FOR", FOR},                             /* 2002 */
    {"FOREGROUND-COLOR", FOREGROUND_COLOR},   /* 2002 (C/S) */
    {"FOREVER", FOREVER},                     /* 2002 (C/S) */
    {"FORMAT", -1},                           /* 2002 */
    {"FORMS-OVERLAY", FORMS_OVERLAY},         /* JP */
    {"FREE", FREE},                           /* 2002 */
    {"FROM", FROM},                           /* 2002 */
    {"FULL", FULL},                           /* 2002 (C/S) */
    {"FUNCTION", FUNCTION},                   /* 2002 */
    {"FUNCTION-ID", FUNCTION_ID},             /* 2002 */
    {"FUNCTION-POINTER", -1},                 /* 2008 */
    {"GENERATE", GENERATE},                   /* 2002 */
    {"GET", -1},                              /* 2002 */
    {"GIVING", GIVING},                       /* 2002 */
    {"GLOBAL", GLOBAL},                       /* 2002 */
    {"GO", GO},                               /* 2002 */
    {"GOBACK", GOBACK},                       /* 2002 */
    {"GREATER", GREATER},                     /* 2002 */
    {"GROUP", GROUP},                         /* 2002 */
    {"GROUP-USAGE", -1},                      /* 2002 */
    {"HEADING", HEADING},                     /* 2002 */
    {"HIGH-VALUE", HIGH_VALUE},               /* 2002 */
    {"HIGH-VALUES", HIGH_VALUE},              /* 2002 */
    {"HIGHLIGHT", HIGHLIGHT},                 /* 2002 (C/S) */
    {"I-O", I_O},                             /* 2002 */
    {"I-O-CONTROL", I_O_CONTROL},             /* 2002 */
    {"ID", IDENTIFICATION},                   /* extension */
    {"IDENTIFICATION", IDENTIFICATION},       /* 2002 */
    {"IF", IF},                               /* 2002 */
    {"IGNORE", IGNORE},                       /* extension */
    {"IGNORING", IGNORING},                   /* 2002 (C/S) */
    {"IMPLEMENTS", -1},                       /* 2002 (C/S) */
    {"IN", IN},                               /* 2002 */
    {"INDEX", INDEX},                         /* 2002 */
    {"INDEXED", INDEXED},                     /* 2002 */
    {"INDICATE", INDICATE},                   /* 2002 */
    {"INFINITY", -1},                         /* 2002 */
    {"INHERITS", -1},                         /* 2002 */
    {"INITIAL", TOK_INITIAL},                 /* 2002 */
    {"INITIALIZE", INITIALIZE},               /* 2002 */
    {"INITIALIZED", INITIALIZED},             /* 2002 */
    {"INITIATE", INITIATE},                   /* 2002 */
    {"INPUT", INPUT},                         /* 2002 */
    {"INPUT-OUTPUT", INPUT_OUTPUT},           /* 2002 */
    {"INSPECT", INSPECT},                     /* 2002 */
    {"INTERFACE", -1},                        /* 2002 */
    {"INTERFACE-ID", -1},                     /* 2002 */
    {"INTO", INTO},                           /* 2002 */
    {"INTRINSIC", INTRINSIC},                 /* 2002 (C/S) */
    {"INVALID", INVALID},                     /* 2002 */
    {"INVOKE", -1},                           /* 2002 */
    {"IS", IS},                               /* 2002 */
    {"JUST", JUSTIFIED},                      /* 2002 */
    {"JUSTIFIED", JUSTIFIED},                 /* 2002 */
    {"KEY", KEY},                             /* 2002 */
    {"LABEL", LABEL},                         /* 85 */
    {"LAST", LAST},                           /* 2002 */
    {"LC_ALL", -1},                           /* 2002 (C/S) */
    {"LC_COLLATE", -1},                       /* 2002 (C/S) */
    {"LC_CTYPE", -1},                         /* 2002 (C/S) */
    {"LC_MESSAGES", -1},                      /* 2002 (C/S) */
    {"LC_MONETARY", -1},                      /* 2002 (C/S) */
    {"LC_NUMERIC", -1},                       /* 2002 (C/S) */
    {"LC_TIME", -1},                          /* 2002 (C/S) */
    {"LEADING", LEADING},                     /* 2002 */
    {"LEFT", LEFT},                           /* 2002 */
    {"LENGTH", LENGTH},                       /* 2002 */
    {"LESS", LESS},                           /* 2002 */
    {"LIMIT", LIMIT},                         /* 2002 */
    {"LIMITS", LIMITS},                       /* 2002 */
    {"LINAGE", LINAGE},                       /* 2002 */
    {"LINAGE-COUNTER", LINAGE_COUNTER},       /* 2002 */
    {"LINE", LINE},                           /* 2002 */
    {"LINE-COUNTER", -1},                     /* 2002 */
    {"LINES", LINES},                         /* 2002 */
    {"LINKAGE", LINKAGE},                     /* 2002 */
    {"LOCAL-STORAGE", LOCAL_STORAGE},         /* 2002 */
    {"LOCALE", LOCALE},                       /* 2002 */
    {"LOCK", LOCK},                           /* 2002 */
    {"LOW-VALUE", LOW_VALUE},                 /* 2002 */
    {"LOW-VALUES", LOW_VALUE},                /* 2002 */
    {"LOWLIGHT", LOWLIGHT},                   /* 2002 (C/S) */
    {"MANUAL", MANUAL},                       /* 2002 (C/S) */
    {"MEMORY", MEMORY},                       /* 85 */
    {"MERGE", MERGE},                         /* 2002 */
    {"MESSAGE", -1},                          /* 2002 */
    {"METHOD", -1},                           /* 2002 */
    {"METHOD-ID", -1},                        /* 2002 */
    {"MINUS", MINUS},                         /* 2002 */
    {"MODE", MODE},                           /* 2002 */
    {"MOVE", MOVE},                           /* 2002 */
    {"MULTIPLE", MULTIPLE},                   /* 2002 (C/S) */
    {"MULTIPLY", MULTIPLY},                   /* 2002 */
    {"NATIONAL", NATIONAL},                   /* 2002 */
    {"NATIONAL-EDITED", NATIONAL_EDITED},     /* 2002 */
    {"NATIVE", NATIVE},                       /* 2002 */
    {"NEGATIVE", NEGATIVE},                   /* 2002 */
    {"NESTED", -1},                           /* 2002 */
    {"NEXT", NEXT},                           /* 2002 */
    {"NO", NO},                               /* 2002 */
    {"NOMINAL", NOMINAL},                     /* JP */
    {"NONE", -1},                             /* 2002 (C/S) */
    {"NORMAL", -1},                           /* 2002 (C/S) */
    {"NOT", NOT},                             /* 2002 */
    {"NULL", TOK_NULL},                       /* 2002 */
    {"NULLS", TOK_NULL},                      /* extension */
    {"NUMBER", NUMBER},                       /* 2002 */
    {"NUMBERS", NUMBER},                      /* 2002 (C/S) */
    {"NUMERIC", NUMERIC},                     /* 2002 */
    {"NUMERIC-EDITED", NUMERIC_EDITED},       /* 2002 */
    {"OBJECT", -1},                           /* 2002 */
    {"OBJECT-COMPUTER", OBJECT_COMPUTER},     /* 2002 */
    {"OBJECT-REFERENCE", -1},                 /* 2002 */
    {"OCCURS", OCCURS},                       /* 2002 */
    {"OF", OF},                               /* 2002 */
    {"OFF", OFF},                             /* 2002 */
    {"OMITTED", OMITTED},                     /* 2002 */
    {"ON", ON},                               /* 2002 */
    {"ONLY", ONLY},                           /* 2002 (C/S) */
    {"OPEN", OPEN},                           /* 2002 */
    {"OPTIONAL", OPTIONAL},                   /* 2002 */
    {"OPTIONS", -1},                          /* 2002 */
    {"OR", OR},                               /* 2002 */
    {"ORDER", ORDER},                         /* 2002 */
    {"ORGANIZATION", ORGANIZATION},           /* 2002 */
    {"OTHER", OTHER},                         /* 2002 */
    {"OUTPUT", OUTPUT},                       /* 2002 */
    {"OVERFLOW", OVERFLOW},                   /* 2002 */
    {"OVERLINE", OVERLINE},                   /* extension */
    {"OVERRIDE", -1},                         /* 2002 */
    {"PACKED-DECIMAL", PACKED_DECIMAL},       /* 2002 */
    {"PADDING", PADDING},                     /* 2002 */
    {"PAGE", PAGE},                           /* 2002 */
    {"PAGE-COUNTER", -1},                     /* 2002 */
    {"PARAGRAPH", PARAGRAPH},                 /* 2002 (C/S) */
    {"PERFORM", PERFORM},                     /* 2002 */
    {"PF", PAGE_FOOTING},                     /* 2002 */
    {"PH", PAGE_HEADING},                     /* 2002 */
    {"PIC", 0},                               /* 2002 */
    {"PICTURE", 0},                           /* 2002 */
    {"PLUS", PLUS},                           /* 2002 */
    {"POINTER", POINTER},                     /* 2002 */
    {"POSITION", POSITION},                   /* 85 */
    {"POSITIVE", POSITIVE},                   /* 2002 */
    {"PRESENT", PRESENT},                     /* 2002 */
    {"PREVIOUS", PREVIOUS},                   /* 2002 (C/S) */
    {"PRINTER", PRINTER},                     /* extension */
    {"PRINTING", PRINTING},                   /* 2002 */
    {"PROCEDURE", PROCEDURE},                 /* 2002 */
    {"PROCEDURE-POINTER", PROGRAM_POINTER},   /* extension */
    {"PROCEDURES", PROCEDURES},               /* extension */
    {"PROCEED", PROCEED},                     /* 85 */
    {"PROGRAM", PROGRAM},                     /* 2002 */
    {"PROGRAM-ID", PROGRAM_ID},               /* 2002 */
    {"PROGRAM-POINTER", PROGRAM_POINTER},     /* 2002 */
    {"PROMPT", PROMPT},                       /* extension */
    {"PROPERTY", -1},                         /* 2002 */
    {"PROTOTYPE", -1},                        /* 2002 */
    {"PURGE", -1},                            /* 2002 */
    {"QUEUE", -1},                            /* 2002 */
    {"QUOTE", QUOTE},                         /* 2002 */
    {"QUOTES", QUOTE},                        /* 2002 */
    {"RAISE", -1},                            /* 2002 */
    {"RAISING", -1},                          /* 2002 */
    {"RANDOM", RANDOM},                       /* 2002 */
    {"RD", RD},                               /* 2002 */
    {"READ", READ},                           /* 2002 */
    {"RECEIVE", -1},                          /* 2002 */
    {"RECORD", RECORD},                       /* 2002 */
    {"RECORDING", RECORDING},                 /* extension */
    {"RECORDS", RECORDS},                     /* 2002 */
    {"RECURSIVE", RECURSIVE},                 /* 2002 (C/S) */
    {"REDEFINES", REDEFINES},                 /* 2002 */
    {"REEL", REEL},                           /* 2002 */
    {"REFERENCE", REFERENCE},                 /* 2002 */
    {"RELATION", -1},                         /* 2002 (C/S) */
    {"RELATIVE", RELATIVE},                   /* 2002 */
    {"RELEASE", RELEASE},                     /* 2002 */
    {"REMAINDER", REMAINDER},                 /* 2002 */
    {"REMOVAL", REMOVAL},                     /* 2002 */
    {"RENAMES", RENAMES},                     /* 2002 */
    {"REPLACE", -1},                          /* 2002 */
    {"REPLACING", REPLACING},                 /* 2002 */
    {"REPORT", REPORT},                       /* 2002 */
    {"REPORTING", REPORTING},                 /* 2002 */
    {"REPORTS", REPORTS},                     /* 2002 */
    {"REPOSITORY", REPOSITORY},               /* 2002 */
    {"REPRESENTS-NOT-A-NUMBER", -1},          /* 2008 */
    {"REQUIRED", REQUIRED},                   /* 2002 (C/S) */
    {"RESERVE", RESERVE},                     /* 2002 */
    {"RESET", -1},                            /* 2002 */
    {"RESUME", -1},                           /* 2002 */
    {"RETRY", -1},                            /* 2002 */
    {"RETURN", RETURN},                       /* 2002 */
    {"RETURNING", RETURNING},                 /* 2002 */
    {"REVERSE-VIDEO", REVERSE_VIDEO},         /* 2002 (C/S) */
    {"REWIND", REWIND},                       /* 2002 */
    {"REWRITE", REWRITE},                     /* 2002 */
    {"RF", REPORT_FOOTING},                   /* 2002 */
    {"RH", REPORT_HEADING},                   /* 2002 */
    {"RIGHT", RIGHT},                         /* 2002 */
    {"ROLLBACK", ROLLBACK},                   /* extension */
    {"ROUNDED", ROUNDED},                     /* 2002 */
    {"RUN", RUN},                             /* 2002 */
    {"SAME", SAME},                           /* 2002 */
    {"SCREEN", SCREEN},                       /* 2002 */
    {"SCROLL", SCROLL},                       /* extension */
    {"SD", SD},                               /* 2002 */
    {"SEARCH", SEARCH},                       /* 2002 */
    {"SECONDS", -1},                          /* 2002 (C/S) */
    {"SECTION", SECTION},                     /* 2002 */
    {"SECURE", SECURE},                       /* 2002 (C/S) */
    {"SEGMENT", -1},                          /* 2002 */
    {"SEGMENT-LIMIT", SEGMENT_LIMIT},         /* 85 */
    {"SELECT", SELECT},                       /* 2002 */
    {"SELF", -1},                             /* 2002 */
    {"SEND", -1},                             /* 2002 */
    {"SENTENCE", SENTENCE},                   /* 2002 */
    {"SEPARATE", SEPARATE},                   /* 2002 */
    {"SEQUENCE", SEQUENCE},                   /* 2002 */
    {"SEQUENTIAL", SEQUENTIAL},               /* 2002 */
    {"SET", SET},                             /* 2002 */
    {"SHARING", SHARING},                     /* 2002 */
    {"SIGN", SIGN},                           /* 2002 */
    {"SIGNED", SIGNED},                       /* 2002 (C/S) */
    {"SIGNED-INT", SIGNED_INT},               /* extension */
    {"SIGNED-LONG", SIGNED_LONG},             /* extension */
    {"SIGNED-SHORT", SIGNED_SHORT},           /* extension */
    {"SIZE", SIZE},                           /* 2002 */
    {"SORT", SORT},                           /* 2002 */
    {"SORT-MERGE", SORT_MERGE},               /* 2002 */
    {"SOURCE", SOURCE},                       /* 2002 */
    {"SOURCE-COMPUTER", SOURCE_COMPUTER},     /* 2002 */
    {"SOURCES", -1},                          /* 2002 */
    {"SPACE", SPACE},                         /* 2002 */
    {"SPACES", SPACE},                        /* 2002 */
    {"SPECIAL-NAMES", SPECIAL_NAMES},         /* 2002 */
    {"STANDARD", STANDARD},                   /* 2002 */
    {"STANDARD-1", STANDARD_1},               /* 2002 */
    {"STANDARD-2", STANDARD_2},               /* 2002 */
    {"START", START},                         /* 2002 */
    {"STATEMENT", -1},                        /* 2002 (C/S) */
    {"STATUS", STATUS},                       /* 2002 */
    {"STEP", -1},                             /* 2002 (C/S) */
    {"STOP", STOP},                           /* 2002 */
    {"STRING", STRING},                       /* 2002 */
    {"STRONG", -1},                           /* 2002 (C/S) */
    {"SUB-QUEUE-1", -1},                      /* 2002 */
    {"SUB-QUEUE-2", -1},                      /* 2002 */
    {"SUB-QUEUE-3", -1},                      /* 2002 */
    {"SUBTRACT", SUBTRACT},                   /* 2002 */
    {"SUM", SUM},                             /* 2002 */
    {"SUPER", -1},                            /* 2002 */
    {"SUPPRESS", SUPPRESS},                   /* 2002 */
    {"SYMBOL", -1},                           /* 2002 (C/S) */
    {"SYMBOLIC", SYMBOLIC},                   /* 2002 */
    {"SYNC", SYNCHRONIZED},                   /* 2002 */
    {"SYNCHRONIZED", SYNCHRONIZED},           /* 2002 */
    {"SYSTEM-DEFAULT", -1},                   /* 2002 */
    {"TABLE", -1},                            /* 2002 */
    {"TALLYING", TALLYING},                   /* 2002 */
    {"TAPE", TAPE},                           /* 85 */
    {"TERMINAL", -1},                         /* 2002 */
    {"TERMINATE", TERMINATE},                 /* 2002 */
    {"TEST", TEST},                           /* 2002 */
    {"TEXT", -1},                             /* 2002 */
    {"THAN", THAN},                           /* 2002 */
    {"THEN", THEN},                           /* 2002 */
    {"THROUGH", THRU},                        /* 2002 */
    {"THRU", THRU},                           /* 2002 */
    {"TIME", TIME},                           /* 2002 */
    {"TIMES", TIMES},                         /* 2002 */
    {"TO", TO},                               /* 2002 */
    {"TOP", TOP},                             /* 2002 */
    {"TRACKS", TRACKS},                       /* JP */
    {"TRAILING", TRAILING},                   /* 2002 */
    {"TRANSFORM", TRANSFORM},                 /* OSVS */
    {"TRUE", TOK_TRUE},                       /* 2002 */
    {"TYPE", TYPE},                           /* 2002 */
    {"TYPEDEF", -1},                          /* 2002 */
    {"UCS-4", -1},                            /* 2002 (C/S) */
    {"UNDERLINE", UNDERLINE},                 /* 2002 (C/S) */
    {"UNIT", UNIT},                           /* 2002 */
    {"UNIVERSAL", -1},                        /* 2002 */
    {"UNLOCK", UNLOCK},                       /* 2002 */
    {"UNSIGNED", UNSIGNED},                   /* 2002 (C/S) */
    {"UNSIGNED-INT", UNSIGNED_INT},           /* extension */
    {"UNSIGNED-LONG", UNSIGNED_LONG},         /* extension */
    {"UNSIGNED-SHORT", UNSIGNED_SHORT},       /* extension */
    {"UNSTRING", UNSTRING},                   /* 2002 */
    {"UNTIL", UNTIL},                         /* 2002 */
    {"UP", UP},                               /* 2002 */
    {"UPDATE", UPDATE},                       /* extension */
    {"UPON", UPON},                           /* 2002 */
    {"USAGE", USAGE},                         /* 2002 */
    {"USE", USE},                             /* 2002 */
    {"USER-DEFAULT", -1},                     /* 2002 */
    {"USING", USING},                         /* 2002 */
    {"UTF-16", -1},                           /* 2002 (C/S) */
    {"UTF-8", -1},                            /* 2002 (C/S) */
    {"VAL-STATUS", -1},                       /* 2002 */
    {"VALID", -1},                            /* 2002 */
    {"VALIDATE", -1},                         /* 2002 */
    {"VALIDATE-STATUS", -1},                  /* 2002 */
    {"VALUE", VALUE},                         /* 2002 */
    {"VALUES", VALUE},                        /* 2002 */
    {"VARYING", VARYING},                     /* 2002 */
    {"WAIT", WAIT},                           /* extension */
    {"WHEN", WHEN},                           /* 2002 */
    {"WITH", WITH},                           /* 2002 */
    {"WORDS", WORDS},                         /* 85 */
    {"WORKING-STORAGE", WORKING_STORAGE},     /* 2002 */
    {"WRITE", WRITE},                         /* 2002 */
    {"YYYYDDD", YYYYDDD},                     /* 2002 (C/S) */
    {"YYYYMMDD", YYYYMMDD},                   /* 2002 (C/S) */
    {"ZERO", ZERO},                           /* 2002 */
    {"ZEROES", ZERO},                         /* 2002 */
    {"ZEROS", ZERO},                          /* 2002 */
};

#define NUM_RESERVED_WORDS sizeof(reserved_words) / sizeof(struct reserved)

/* FUNCTION List */
/* Name, Arguments, Implemented, Enum intrinsic, Routine, Category, Can refmod
 */

static const struct cb_intrinsic_table function_list[] = {
    {"ABS", 1, 1, CB_INTR_ABS, "CobolIntrinsic.funcAbs", CB_CATEGORY_NUMERIC,
     0},
    {"ACOS", 1, 1, CB_INTR_ACOS, "CobolIntrinsic.funcAcos", CB_CATEGORY_NUMERIC,
     0},
    {"ANNUITY", 2, 1, CB_INTR_ANNUITY, "CobolIntrinsic.funcAnnuity",
     CB_CATEGORY_NUMERIC, 0},
    {"ASIN", 1, 1, CB_INTR_ASIN, "CobolIntrinsic.funcAsin", CB_CATEGORY_NUMERIC,
     0},
    {"ATAN", 1, 1, CB_INTR_ATAN, "CobolIntrinsic.funcAtan", CB_CATEGORY_NUMERIC,
     0},
    {"BOOLEAN-OF-INTEGER", 2, 0, CB_INTR_BOOLEAN_OF_INTEGER, NULL,
     CB_CATEGORY_NUMERIC, 0},
    {"BYTE-LENGTH", 1, 1, CB_INTR_BYTE_LENGTH, "CobolIntrinsic.funcLength",
     CB_CATEGORY_NUMERIC, 0},
    {"CHAR", 1, 1, CB_INTR_CHAR, "CobolIntrinsic.funcChar",
     CB_CATEGORY_ALPHANUMERIC, 0},
    {"CHAR-NATIONAL", 1, 0, CB_INTR_CHAR_NATIONAL, NULL,
     CB_CATEGORY_ALPHANUMERIC, 0},
    {"COMBINED-DATETIME", 2, 1, CB_INTR_COMBINED_DATETIME,
     "CobolIntrinsic.funcCombinedDatetime", CB_CATEGORY_NUMERIC, 0},
    {"CONCATENATE", -1, 1, CB_INTR_CONCATENATE,
     "CobolIntrinsic.funcConcatenate", CB_CATEGORY_ALPHANUMERIC, 1},
    {"COS", 1, 1, CB_INTR_COS, "CobolIntrinsic.funcCos", CB_CATEGORY_NUMERIC,
     0},
    {"CURRENT-DATE", 0, 1, CB_INTR_CURRENT_DATE,
     "CobolIntrinsic.funcCurrentDate", CB_CATEGORY_ALPHANUMERIC, 1},
    {"DATE-OF-INTEGER", 1, 1, CB_INTR_DATE_OF_INTEGER,
     "CobolIntrinsic.funcDateOfInteger", CB_CATEGORY_NUMERIC, 0},
    {"DATE-TO-YYYYMMDD", -1, 1, CB_INTR_DATE_TO_YYYYMMDD,
     "CobolIntrinsic.funcDateToYyyymmdd", CB_CATEGORY_NUMERIC, 0},
    {"DAY-OF-INTEGER", 1, 1, CB_INTR_DAY_OF_INTEGER,
     "CobolIntrinsic.funcDayOfInteger", CB_CATEGORY_NUMERIC, 0},
    {"DAY-TO-YYYYDDD", -1, 1, CB_INTR_DAY_TO_YYYYDDD,
     "CobolIntrinsic.funcDayToYyyyddd", CB_CATEGORY_NUMERIC, 0},
    {"DISPLAY-OF", -1, 0, CB_INTR_DISPLAY_OF, NULL, CB_CATEGORY_ALPHANUMERIC,
     0},
    {"E", 0, 1, CB_INTR_E, NULL, CB_CATEGORY_NUMERIC, 0},
    {"EXCEPTION-FILE", 0, 1, CB_INTR_EXCEPTION_FILE,
     "CobolIntrinsic.funcExceptionFile", CB_CATEGORY_ALPHANUMERIC, 0},
    {"EXCEPTION-FILE-N", 0, 0, CB_INTR_EXCEPTION_FILE_N, NULL,
     CB_CATEGORY_ALPHANUMERIC, 0},
    {"EXCEPTION-LOCATION", 0, 1, CB_INTR_EXCEPTION_LOCATION,
     "CobolIntrinsic.funcExceptionLocation", CB_CATEGORY_ALPHANUMERIC, 0},
    {"EXCEPTION-LOCATION-N", 0, 0, CB_INTR_EXCEPTION_LOCATION_N, NULL,
     CB_CATEGORY_ALPHANUMERIC, 0},
    {"EXCEPTION-STATEMENT", 0, 1, CB_INTR_EXCEPTION_STATEMENT,
     "CobolIntrinsic.funcExceptionStatement", CB_CATEGORY_ALPHANUMERIC, 0},
    {"EXCEPTION-STATUS", 0, 1, CB_INTR_EXCEPTION_STATUS,
     "CobolIntrinsic.funcExceptionStatus", CB_CATEGORY_ALPHANUMERIC, 0},
    {"EXP", 1, 1, CB_INTR_EXP, "CobolIntrinsic.funcExp", CB_CATEGORY_NUMERIC,
     0},
    {"EXP10", 1, 1, CB_INTR_EXP10, "CobolIntrinsic.funcExp10",
     CB_CATEGORY_NUMERIC, 0},
    {"FACTORIAL", 1, 1, CB_INTR_FACTORIAL, "CobolIntrinsic.funcFactorial",
     CB_CATEGORY_NUMERIC, 0},
    {"FRACTION-PART", 1, 1, CB_INTR_FRACTION_PART,
     "CobolIntrinsic.funcFractionPart", CB_CATEGORY_NUMERIC, 0},
    {"HIGHEST-ALGEBRAIC", 1, 0, CB_INTR_HIGHEST_ALGEBRAIC, NULL,
     CB_CATEGORY_NUMERIC, 0},
    {"INTEGER", 1, 1, CB_INTR_INTEGER, "CobolIntrinsic.funcInteger",
     CB_CATEGORY_NUMERIC, 0},
    {"INTEGER-OF-BOOLEAN", 1, 0, CB_INTR_INTEGER_OF_BOOLEAN, NULL,
     CB_CATEGORY_NUMERIC, 0},
    {"INTEGER-OF-DATE", 1, 1, CB_INTR_INTEGER_OF_DATE,
     "CobolIntrinsic.funcIntegerOfDate", CB_CATEGORY_NUMERIC, 0},
    {"INTEGER-OF-DAY", 1, 1, CB_INTR_INTEGER_OF_DAY,
     "CobolIntrinsic.funcIntegerOfDay", CB_CATEGORY_NUMERIC, 0},
    {"INTEGER-PART", 1, 1, CB_INTR_INTEGER_PART,
     "CobolIntrinsic.funcIntegerPart", CB_CATEGORY_NUMERIC, 0},
    {"LENG", 1, 1, CB_INTR_LENG, "CobolIntrinsic.funcLength",
     CB_CATEGORY_NUMERIC, 0},
    {"LENGTH", 1, 1, CB_INTR_LENGTH, "CobolIntrinsic.funcLength",
     CB_CATEGORY_NUMERIC, 0},
    {"LENGTH-AN", 1, 1, CB_INTR_LENGTH_AN, "CobolIntrinsic.funcLength",
     CB_CATEGORY_NUMERIC, 0},
    {"LOCALE-COMPARE", -1, 0, CB_INTR_LOCALE_COMPARE, NULL,
     CB_CATEGORY_ALPHANUMERIC, 0},
    {"LOCALE-DATE", 2, 1, CB_INTR_LOCALE_DATE, "CobolIntrinsic.funcLocaleDate",
     CB_CATEGORY_ALPHANUMERIC, 1},
    {"LOCALE-TIME", 2, 1, CB_INTR_LOCALE_TIME, "CobolIntrinsic.funcLocaleTime",
     CB_CATEGORY_ALPHANUMERIC, 1},
    {"LOCALE-TIME-FROM-SECONDS", 2, 1, CB_INTR_LOCALE_TIME_FROM_SECS,
     "CobolIntrinsic.funcLocaleTimeFromSeconds", CB_CATEGORY_ALPHANUMERIC, 1},
    {"LOG", 1, 1, CB_INTR_LOG, "CobolIntrinsic.funcLog", CB_CATEGORY_NUMERIC,
     0},
    {"LOG10", 1, 1, CB_INTR_LOG10, "CobolIntrinsic.funcLog10",
     CB_CATEGORY_NUMERIC, 0},
    {"LOWER-CASE", 1, 1, CB_INTR_LOWER_CASE, "CobolIntrinsic.funcLowerCase",
     CB_CATEGORY_ALPHANUMERIC, 1},
    {"LOWEST-ALGEBRAIC", 1, 0, CB_INTR_LOWEST_ALGEBRAIC, NULL,
     CB_CATEGORY_NUMERIC, 0},
    {"MAX", -1, 1, CB_INTR_MAX, "CobolIntrinsic.funcMax", CB_CATEGORY_NUMERIC,
     0},
    {"MEAN", -1, 1, CB_INTR_MEAN, "CobolIntrinsic.funcMean",
     CB_CATEGORY_NUMERIC, 0},
    {"MEDIAN", -1, 1, CB_INTR_MEDIAN, "CobolIntrinsic.funcMedian",
     CB_CATEGORY_NUMERIC, 0},
    {"MIDRANGE", -1, 1, CB_INTR_MIDRANGE, "CobolIntrinsic.funcMidrange",
     CB_CATEGORY_NUMERIC, 0},
    {"MIN", -1, 1, CB_INTR_MIN, "CobolIntrinsic.funcMin", CB_CATEGORY_NUMERIC,
     0},
    {"MOD", 2, 1, CB_INTR_MOD, "CobolIntrinsic.funcMod", CB_CATEGORY_NUMERIC,
     0},
    {"NATIONAL", 1, 1, CB_INTR_NATIONAL, "CobolIntrinsic.funcNational",
     CB_CATEGORY_NATIONAL, 0},
    {"NATIONAL-OF", -1, 0, CB_INTR_NATIONAL_OF, NULL, CB_CATEGORY_ALPHANUMERIC,
     0},
    {"NUMVAL", 1, 1, CB_INTR_NUMVAL, "CobolIntrinsic.funcNumval",
     CB_CATEGORY_NUMERIC, 0},
    {"NUMVAL-C", 2, 1, CB_INTR_NUMVAL_C, "CobolIntrinsic.funcNumvalC",
     CB_CATEGORY_NUMERIC, 0},
    {"NUMVAL-F", 1, 0, CB_INTR_NUMVAL_F, NULL, CB_CATEGORY_NUMERIC, 0},
    {"ORD", 1, 1, CB_INTR_ORD, "CobolIntrinsic.funcOrd", CB_CATEGORY_NUMERIC,
     0},
    {"ORD-MAX", -1, 1, CB_INTR_ORD_MAX, "CobolIntrinsic.funcOrdMax",
     CB_CATEGORY_NUMERIC, 0},
    {"ORD-MIN", -1, 1, CB_INTR_ORD_MIN, "CobolIntrinsic.funcOrdMin",
     CB_CATEGORY_NUMERIC, 0},
    {"PI", 0, 1, CB_INTR_PI, NULL, CB_CATEGORY_NUMERIC, 0},
    {"PRESENT-VALUE", -1, 1, CB_INTR_PRESENT_VALUE,
     "CobolIntrinsic.funcPresentValue", CB_CATEGORY_NUMERIC, 0},
    {"RANDOM", -1, 1, CB_INTR_RANDOM, "CobolIntrinsic.funcRandom",
     CB_CATEGORY_NUMERIC, 0},
    {"RANGE", -1, 1, CB_INTR_RANGE, "CobolIntrinsic.funcRange",
     CB_CATEGORY_NUMERIC, 0},
    {"REM", 2, 1, CB_INTR_REM, "CobolIntrinsic.funcRem", CB_CATEGORY_NUMERIC,
     0},
    {"REVERSE", 1, 1, CB_INTR_REVERSE, "CobolIntrinsic.funcReverse",
     CB_CATEGORY_ALPHANUMERIC, 1},
    {"SECONDS-FROM-FORMATTED-TIME", 2, 1, CB_INTR_SECONDS_PAST_MIDNIGHT,
     "CobolIntrinsic.funcSecondsFromFormattedTime", CB_CATEGORY_NUMERIC, 0},
    {"SECONDS-PAST-MIDNIGHT", 0, 1, CB_INTR_SECONDS_PAST_MIDNIGHT,
     "CobolIntrinsic.funcSecondsPastMidnight", CB_CATEGORY_NUMERIC, 0},
    {"SIGN", 1, 1, CB_INTR_SIGN, "CobolIntrinsic.funcSign", CB_CATEGORY_NUMERIC,
     0},
    {"SIN", 1, 1, CB_INTR_SIN, "CobolIntrinsic.funcSin", CB_CATEGORY_NUMERIC,
     0},
    {"SQRT", 1, 1, CB_INTR_SQRT, "CobolIntrinsic.funcSqrt", CB_CATEGORY_NUMERIC,
     0},
    {"STANDARD-COMPARE", -1, 0, CB_INTR_STANDARD_COMPARE, NULL,
     CB_CATEGORY_ALPHANUMERIC, 0},
    {"STANDARD-DEVIATION", -1, 1, CB_INTR_STANDARD_DEVIATION,
     "CobolIntrinsic.funcStandardDeviation", CB_CATEGORY_NUMERIC, 0},
    {"STORED-CHAR-LENGTH", 1, 1, CB_INTR_STORED_CHAR_LENGTH,
     "CobolIntrinsic.funcStoredCharLength", CB_CATEGORY_NUMERIC, 0},
    {"SUBSTITUTE", -1, 1, CB_INTR_SUBSTITUTE, "CobolIntrinsic.funcSubstitute",
     CB_CATEGORY_ALPHANUMERIC, 1},
    {"SUBSTITUTE-CASE", -1, 1, CB_INTR_SUBSTITUTE_CASE,
     "CobolIntrinsic.funcSubstituteCase", CB_CATEGORY_ALPHANUMERIC, 1},
    {"SUM", -1, 1, CB_INTR_SUM, "CobolIntrinsic.funcSum", CB_CATEGORY_NUMERIC,
     0},
    {"TAN", 1, 1, CB_INTR_TAN, "CobolIntrinsic.funcTan", CB_CATEGORY_NUMERIC,
     0},
    {"TEST-DATE-YYYYMMDD", 1, 1, CB_INTR_TEST_DATE_YYYYMMDD,
     "CobolIntrinsic.funcTestDateYyyymmdd", CB_CATEGORY_NUMERIC, 0},
    {"TEST-DAY-YYYYDDD", 1, 1, CB_INTR_TEST_DAY_YYYYDDD,
     "CobolIntrinsic.funcTestDayYyyyddd", CB_CATEGORY_NUMERIC, 0},
    {"TEST-NUMVAL", 1, 0, CB_INTR_TEST_NUMVAL, NULL, CB_CATEGORY_NUMERIC, 0},
    {"TEST-NUMVAL-C", -1, 0, CB_INTR_TEST_NUMVAL_C, NULL, CB_CATEGORY_NUMERIC,
     0},
    {"TEST-NUMVAL-F", 1, 0, CB_INTR_TEST_NUMVAL_F, NULL, CB_CATEGORY_NUMERIC,
     0},
    {"TRIM", 2, 1, CB_INTR_TRIM, "CobolIntrinsic.funcTrim",
     CB_CATEGORY_ALPHANUMERIC, 1},
    {"UPPER-CASE", 1, 1, CB_INTR_UPPER_CASE, "CobolIntrinsic.funcUpperCase",
     CB_CATEGORY_ALPHANUMERIC, 1},
    {"VARIANCE", -1, 1, CB_INTR_VARIANCE, "CobolIntrinsic.funcVariance",
     CB_CATEGORY_NUMERIC, 0},
    {"WHEN-COMPILED", 0, 1, CB_INTR_WHEN_COMPILED,
     "CobolIntrinsic.funcWhenCompiled", CB_CATEGORY_ALPHANUMERIC, 1},
    {"YEAR-TO-YYYY", -1, 1, CB_INTR_YEAR_TO_YYYY,
     "CobolIntrinsic.funcYearToYyyy", CB_CATEGORY_NUMERIC, 0}};

#define NUM_INTRINSICS sizeof(function_list) / sizeof(struct cb_intrinsic_table)

static int reserve_comp(const void *p1, const void *p2) {
  return strcasecmp(p1, ((struct reserved *)p2)->name);
}

static int intrinsic_comp(const void *p1, const void *p2) {
  return strcasecmp(p1, ((struct cb_intrinsic_table *)p2)->name);
}

cb_tree lookup_system_name(const char *name) {
  int i;

  for (i = 0; system_table[i].name != NULL; ++i) {
    if (strcasecmp(name, system_table[i].name) == 0) {
      return system_table[i].node;
    }
  }
  return cb_error_node;
}

int lookup_reserved_word(const char *name) {
  struct reserved *p;
  struct noreserve *noresptr;

  p = bsearch(name, reserved_words, NUM_RESERVED_WORDS, sizeof(struct reserved),
              reserve_comp);
  if (!p) {
    return 0;
  }
  for (noresptr = norestab; noresptr; noresptr = noresptr->next) {
    if (strcasecmp(name, noresptr->noresword) == 0) {
      return 0;
    }
  }
  if (p->token != -1) {
    return p->token;
  }
  cb_error(_("'%s' reserved word, but not supported yet"), name);
  return 0;
}

struct cb_intrinsic_table *lookup_intrinsic(const char *name,
                                            const int checkres) {
  struct cb_intrinsic_table *cbp;
  struct noreserve *noresptr;

  if (checkres) {
    for (noresptr = norestab; noresptr; noresptr = noresptr->next) {
      if (strcasecmp(name, noresptr->noresword) == 0) {
        return NULL;
      }
    }
  }
  cbp = bsearch(name, function_list, NUM_INTRINSICS,
                sizeof(struct cb_intrinsic_table), intrinsic_comp);
  if (cbp && cbp->implemented) {
    return cbp;
  }
  return NULL;
}

void cb_list_reserved(void) {
  const char *s;
  size_t i;

  printf("Reserved Words (Parsed Y/N)\n\n");
  for (i = 0; i < NUM_RESERVED_WORDS; ++i) {
    size_t n = strlen(reserved_words[i].name);
    switch (n / 8) {
    case 0:
      s = "\t\t\t\t";
      break;
    case 1:
      s = "\t\t\t";
      break;
    case 2:
      s = "\t\t";
      break;
    default:
      s = "\t";
      break;
    }
    printf("%s%s(%s)\n", reserved_words[i].name, s,
           reserved_words[i].token != -1 ? "Y" : "N");
  }
}

void cb_list_intrinsics(void) {
  const char *s;
  size_t i;

  printf("Intrinsic Function (Implemented Y/N)\n\n");
  for (i = 0; i < NUM_INTRINSICS; ++i) {
    size_t n = strlen(function_list[i].name);
    switch (n / 8) {
    case 0:
      s = "\t\t\t\t";
      break;
    case 1:
      s = "\t\t\t";
      break;
    case 2:
      s = "\t\t";
      break;
    default:
      s = "\t";
      break;
    }
    printf("%s%s(%s)\n", function_list[i].name, s,
           function_list[i].implemented ? "Y" : "N");
  }
}

void cb_list_mnemonics(void) {
  size_t i;

  printf("Mnemonic names\n\n");
  for (i = 0; system_table[i].name != NULL; ++i) {
    printf("%s\n", system_table[i].name);
  }
}

void cb_init_reserved(void) {
  int i;

  /* build system-name table */
  for (i = 0; system_table[i].name != NULL; ++i) {
    system_table[i].node =
        cb_build_system_name(system_table[i].category, system_table[i].token);
  }
}
