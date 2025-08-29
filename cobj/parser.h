/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
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
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    TOKEN_EOF = 0,                 /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    ACCEPT = 258,                  /* ACCEPT  */
    ACCESS = 259,                  /* ACCESS  */
    ADD = 260,                     /* ADD  */
    ADDRESS = 261,                 /* ADDRESS  */
    ADVANCING = 262,               /* ADVANCING  */
    AFTER = 263,                   /* AFTER  */
    ALL = 264,                     /* ALL  */
    ALLOCATE = 265,                /* ALLOCATE  */
    ALPHABET = 266,                /* ALPHABET  */
    ALPHABETIC = 267,              /* ALPHABETIC  */
    ALPHABETIC_LOWER = 268,        /* "ALPHABETIC-LOWER"  */
    ALPHABETIC_UPPER = 269,        /* "ALPHABETIC-UPPER"  */
    ALPHANUMERIC = 270,            /* ALPHANUMERIC  */
    ALPHANUMERIC_EDITED = 271,     /* "ALPHANUMERIC-EDITED"  */
    ALSO = 272,                    /* ALSO  */
    ALTER = 273,                   /* ALTER  */
    ALTERNATE = 274,               /* ALTERNATE  */
    AND = 275,                     /* AND  */
    ANY = 276,                     /* ANY  */
    APPLY = 277,                   /* APPLY  */
    ARE = 278,                     /* ARE  */
    AREA = 279,                    /* AREA  */
    ARGUMENT_NUMBER = 280,         /* "ARGUMENT-NUMBER"  */
    ARGUMENT_VALUE = 281,          /* "ARGUMENT-VALUE"  */
    AS = 282,                      /* AS  */
    ASCENDING = 283,               /* ASCENDING  */
    ASSIGN = 284,                  /* ASSIGN  */
    AT = 285,                      /* AT  */
    AUTO = 286,                    /* AUTO  */
    AUTOMATIC = 287,               /* AUTOMATIC  */
    BACKGROUND_COLOR = 288,        /* "BACKGROUND-COLOR"  */
    BASED = 289,                   /* BASED  */
    BEFORE = 290,                  /* BEFORE  */
    BELL = 291,                    /* BELL  */
    BINARY = 292,                  /* BINARY  */
    BINARY_C_LONG = 293,           /* "BINARY-C-LONG"  */
    BINARY_CHAR = 294,             /* "BINARY-CHAR"  */
    BINARY_DOUBLE = 295,           /* "BINARY-DOUBLE"  */
    BINARY_LONG = 296,             /* "BINARY-LONG"  */
    BINARY_SHORT = 297,            /* "BINARY-SHORT"  */
    BLANK = 298,                   /* BLANK  */
    BLANK_LINE = 299,              /* "BLANK-LINE"  */
    BLANK_SCREEN = 300,            /* "BLANK-SCREEN"  */
    BLINK = 301,                   /* BLINK  */
    BLOCK = 302,                   /* BLOCK  */
    BOTTOM = 303,                  /* BOTTOM  */
    BY = 304,                      /* BY  */
    BYTE_LENGTH = 305,             /* "BYTE-LENGTH"  */
    CALL = 306,                    /* CALL  */
    CANCEL = 307,                  /* CANCEL  */
    CH = 308,                      /* CH  */
    CHAINING = 309,                /* CHAINING  */
    CHARACTER = 310,               /* CHARACTER  */
    CHARACTERS = 311,              /* CHARACTERS  */
    CLASS = 312,                   /* CLASS  */
    CLASS_NAME = 313,              /* CLASS_NAME  */
    CLOSE = 314,                   /* CLOSE  */
    CLOSE_NOFEED = 315,            /* "CLOSE-NOFEED"  */
    CODE = 316,                    /* CODE  */
    CODE_SET = 317,                /* "CODE-SET"  */
    COLLATING = 318,               /* COLLATING  */
    COL = 319,                     /* COL  */
    COLS = 320,                    /* COLS  */
    COLUMN = 321,                  /* COLUMN  */
    COLUMNS = 322,                 /* COLUMNS  */
    COMMA = 323,                   /* COMMA  */
    COMMAND_LINE = 324,            /* "COMMAND-LINE"  */
    COMMA_DELIM = 325,             /* "comma delimiter"  */
    COMMIT = 326,                  /* COMMIT  */
    COMMITMENT_CONTROL = 327,      /* "COMMITMENT-CONTROL"  */
    COMMON = 328,                  /* COMMON  */
    COMP = 329,                    /* COMP  */
    COMPUTE = 330,                 /* COMPUTE  */
    COMP_1 = 331,                  /* "COMP-1"  */
    COMP_2 = 332,                  /* "COMP-2"  */
    COMP_3 = 333,                  /* "COMP-3"  */
    COMP_4 = 334,                  /* "COMP-4"  */
    COMP_5 = 335,                  /* "COMP-5"  */
    COMP_X = 336,                  /* "COMP-X"  */
    CONCATENATE_FUNC = 337,        /* "FUNCTION CONCATENATE"  */
    CONFIGURATION = 338,           /* CONFIGURATION  */
    CONSTANT = 339,                /* CONSTANT  */
    CONTAINS = 340,                /* CONTAINS  */
    CONTENT = 341,                 /* CONTENT  */
    CONTINUE = 342,                /* CONTINUE  */
    CONTROL = 343,                 /* CONTROL  */
    CONTROLS = 344,                /* CONTROLS  */
    CONTROL_FOOTING = 345,         /* "CONTROL FOOTING"  */
    CONTROL_HEADING = 346,         /* "CONTROL HEADING"  */
    CONVERTING = 347,              /* CONVERTING  */
    CORE_INDEX = 348,              /* "CORE-INDEX"  */
    CORRESPONDING = 349,           /* CORRESPONDING  */
    COUNT = 350,                   /* COUNT  */
    CRT = 351,                     /* CRT  */
    CURRENCY = 352,                /* CURRENCY  */
    CURRENT_DATE_FUNC = 353,       /* "FUNCTION CURRENT-DATE"  */
    CURSOR = 354,                  /* CURSOR  */
    CYCLE = 355,                   /* CYCLE  */
    CYL_OVERFLOW = 356,            /* "CYL-OVERFLOW"  */
    DATA = 357,                    /* DATA  */
    DATE = 358,                    /* DATE  */
    DAY = 359,                     /* DAY  */
    DAY_OF_WEEK = 360,             /* "DAY-OF-WEEK"  */
    DE = 361,                      /* DE  */
    DEBUGGING = 362,               /* DEBUGGING  */
    DECIMAL_POINT = 363,           /* "DECIMAL-POINT"  */
    DECLARATIVES = 364,            /* DECLARATIVES  */
    DEFAULT = 365,                 /* DEFAULT  */
    DELETE = 366,                  /* DELETE  */
    DELIMITED = 367,               /* DELIMITED  */
    DELIMITER = 368,               /* DELIMITER  */
    DEPENDING = 369,               /* DEPENDING  */
    DESCENDING = 370,              /* DESCENDING  */
    DETAIL = 371,                  /* DETAIL  */
    DISK = 372,                    /* DISK  */
    DISPLAY = 373,                 /* DISPLAY  */
    DIVIDE = 374,                  /* DIVIDE  */
    DIVISION = 375,                /* DIVISION  */
    DOWN = 376,                    /* DOWN  */
    DUPLICATES = 377,              /* DUPLICATES  */
    DYNAMIC = 378,                 /* DYNAMIC  */
    EBCDIC = 379,                  /* EBCDIC  */
    ELSE = 380,                    /* ELSE  */
    END = 381,                     /* END  */
    END_ACCEPT = 382,              /* "END-ACCEPT"  */
    END_ADD = 383,                 /* "END-ADD"  */
    END_CALL = 384,                /* "END-CALL"  */
    END_COMPUTE = 385,             /* "END-COMPUTE"  */
    END_DELETE = 386,              /* "END-DELETE"  */
    END_DISPLAY = 387,             /* "END-DISPLAY"  */
    END_DIVIDE = 388,              /* "END-DIVIDE"  */
    END_EVALUATE = 389,            /* "END-EVALUATE"  */
    END_FUNCTION = 390,            /* "END FUNCTION"  */
    END_IF = 391,                  /* "END-IF"  */
    END_MULTIPLY = 392,            /* "END-MULTIPLY"  */
    END_PERFORM = 393,             /* "END-PERFORM"  */
    END_PROGRAM = 394,             /* "END PROGRAM"  */
    END_READ = 395,                /* "END-READ"  */
    END_RETURN = 396,              /* "END-RETURN"  */
    END_REWRITE = 397,             /* "END-REWRITE"  */
    END_SEARCH = 398,              /* "END-SEARCH"  */
    END_START = 399,               /* "END-START"  */
    END_STRING = 400,              /* "END-STRING"  */
    END_SUBTRACT = 401,            /* "END-SUBTRACT"  */
    END_UNSTRING = 402,            /* "END-UNSTRING"  */
    END_WRITE = 403,               /* "END-WRITE"  */
    ENTRY = 404,                   /* ENTRY  */
    ENVIRONMENT = 405,             /* ENVIRONMENT  */
    ENVIRONMENT_NAME = 406,        /* "ENVIRONMENT-NAME"  */
    ENVIRONMENT_VALUE = 407,       /* "ENVIRONMENT-VALUE"  */
    EOL = 408,                     /* EOL  */
    EOP = 409,                     /* EOP  */
    EOS = 410,                     /* EOS  */
    EQUAL = 411,                   /* EQUAL  */
    EQUALS = 412,                  /* EQUALS  */
    ERASE = 413,                   /* ERASE  */
    ERROR = 414,                   /* ERROR  */
    ESCAPE = 415,                  /* ESCAPE  */
    EVALUATE = 416,                /* EVALUATE  */
    EVENT_STATUS = 417,            /* "EVENT-STATUS"  */
    EXCEPTION = 418,               /* EXCEPTION  */
    EXCLUSIVE = 419,               /* EXCLUSIVE  */
    EXIT = 420,                    /* EXIT  */
    EXTEND = 421,                  /* EXTEND  */
    EXTERNAL = 422,                /* EXTERNAL  */
    FD = 423,                      /* FD  */
    FILE_CONTROL = 424,            /* "FILE-CONTROL"  */
    FILE_ID = 425,                 /* "FILE-ID"  */
    FILLER = 426,                  /* FILLER  */
    FINAL = 427,                   /* FINAL  */
    FIRST = 428,                   /* FIRST  */
    FOOTING = 429,                 /* FOOTING  */
    FOR = 430,                     /* FOR  */
    FOREGROUND_COLOR = 431,        /* "FOREGROUND-COLOR"  */
    FOREVER = 432,                 /* FOREVER  */
    FORMS_OVERLAY = 433,           /* "FORMS-OVERLAY"  */
    FREE = 434,                    /* FREE  */
    FROM = 435,                    /* FROM  */
    FULL = 436,                    /* FULL  */
    FUNCTION = 437,                /* FUNCTION  */
    FUNCTION_ID = 438,             /* "FUNCTION-ID"  */
    FUNCTION_NAME = 439,           /* "FUNCTION"  */
    GE = 440,                      /* GE  */
    GENERATE = 441,                /* GENERATE  */
    GIVING = 442,                  /* GIVING  */
    GLOBAL = 443,                  /* GLOBAL  */
    GO = 444,                      /* GO  */
    GOBACK = 445,                  /* GOBACK  */
    GREATER = 446,                 /* GREATER  */
    GROUP = 447,                   /* GROUP  */
    HEADING = 448,                 /* HEADING  */
    HIGHLIGHT = 449,               /* HIGHLIGHT  */
    HIGH_VALUE = 450,              /* "HIGH-VALUE"  */
    IDENTIFICATION = 451,          /* IDENTIFICATION  */
    IF = 452,                      /* IF  */
    IGNORE = 453,                  /* IGNORE  */
    IGNORING = 454,                /* IGNORING  */
    IN = 455,                      /* IN  */
    INDEX = 456,                   /* INDEX  */
    INDEXED = 457,                 /* INDEXED  */
    INDICATE = 458,                /* INDICATE  */
    INITIALIZE = 459,              /* INITIALIZE  */
    INITIALIZED = 460,             /* INITIALIZED  */
    INITIATE = 461,                /* INITIATE  */
    INPUT = 462,                   /* INPUT  */
    INPUT_OUTPUT = 463,            /* "INPUT-OUTPUT"  */
    INSPECT = 464,                 /* INSPECT  */
    INTO = 465,                    /* INTO  */
    INTRINSIC = 466,               /* INTRINSIC  */
    INVALID = 467,                 /* INVALID  */
    INVALID_KEY = 468,             /* "INVALID KEY"  */
    IS = 469,                      /* IS  */
    I_O = 470,                     /* "I-O"  */
    I_O_CONTROL = 471,             /* "I-O-CONTROL"  */
    JUSTIFIED = 472,               /* JUSTIFIED  */
    KEY = 473,                     /* KEY  */
    LABEL = 474,                   /* LABEL  */
    LAST = 475,                    /* LAST  */
    LAST_DETAIL = 476,             /* "LAST DETAIL"  */
    LE = 477,                      /* LE  */
    LEADING = 478,                 /* LEADING  */
    LEFT = 479,                    /* LEFT  */
    LENGTH = 480,                  /* LENGTH  */
    LESS = 481,                    /* LESS  */
    LEVEL_NUMBER_WORD = 482,       /* LEVEL_NUMBER_WORD  */
    LEVEL88_NUMBER_WORD = 483,     /* LEVEL88_NUMBER_WORD  */
    LIMIT = 484,                   /* LIMIT  */
    LIMITS = 485,                  /* LIMITS  */
    LINAGE = 486,                  /* LINAGE  */
    LINAGE_COUNTER = 487,          /* "LINAGE-COUNTER"  */
    LINE = 488,                    /* LINE  */
    LINES = 489,                   /* LINES  */
    LINKAGE = 490,                 /* LINKAGE  */
    LITERAL = 491,                 /* "Literal"  */
    LOCALE = 492,                  /* LOCALE  */
    LOCALE_DT_FUNC = 493,          /* "FUNCTION LOCALE"  */
    LOCAL_STORAGE = 494,           /* "LOCAL-STORAGE"  */
    LOCK = 495,                    /* LOCK  */
    LOWER_CASE_FUNC = 496,         /* "FUNCTION LOWER-CASE"  */
    LOWLIGHT = 497,                /* LOWLIGHT  */
    LOW_VALUE = 498,               /* "LOW-VALUE"  */
    MANUAL = 499,                  /* MANUAL  */
    MEMORY = 500,                  /* MEMORY  */
    MERGE = 501,                   /* MERGE  */
    MINUS = 502,                   /* MINUS  */
    MNEMONIC_NAME = 503,           /* "MNEMONIC NAME"  */
    MODE = 504,                    /* MODE  */
    MOVE = 505,                    /* MOVE  */
    MULTIPLE = 506,                /* MULTIPLE  */
    MULTIPLY = 507,                /* MULTIPLY  */
    NATIONAL = 508,                /* NATIONAL  */
    NATIONAL_EDITED = 509,         /* "NATIONAL-EDITED"  */
    NATIVE = 510,                  /* NATIVE  */
    NE = 511,                      /* NE  */
    NEGATIVE = 512,                /* NEGATIVE  */
    NEXT = 513,                    /* NEXT  */
    NEXT_SENTENCE = 514,           /* "NEXT SENTENCE"  */
    NO = 515,                      /* NO  */
    NOMINAL = 516,                 /* NOMINAL  */
    NOT = 517,                     /* NOT  */
    NOT_END = 518,                 /* "NOT END"  */
    NOT_EOP = 519,                 /* "NOT EOP"  */
    NOT_EXCEPTION = 520,           /* "NOT EXCEPTION"  */
    NOT_INVALID_KEY = 521,         /* "NOT INVALID KEY"  */
    NOT_OVERFLOW = 522,            /* "NOT OVERFLOW"  */
    NOT_SIZE_ERROR = 523,          /* "NOT SIZE ERROR"  */
    NO_ADVANCING = 524,            /* "NO ADVANCING"  */
    NUMBER = 525,                  /* NUMBER  */
    NUMBERS = 526,                 /* NUMBERS  */
    NUMERIC = 527,                 /* NUMERIC  */
    NUMERIC_EDITED = 528,          /* "NUMERIC-EDITED"  */
    NUMVALC_FUNC = 529,            /* "FUNCTION NUMVALC"  */
    OBJECT_COMPUTER = 530,         /* "OBJECT-COMPUTER"  */
    OCCURS = 531,                  /* OCCURS  */
    OF = 532,                      /* OF  */
    OFF = 533,                     /* OFF  */
    OMITTED = 534,                 /* OMITTED  */
    ON = 535,                      /* ON  */
    ONLY = 536,                    /* ONLY  */
    OPEN = 537,                    /* OPEN  */
    OPTIONAL = 538,                /* OPTIONAL  */
    OR = 539,                      /* OR  */
    ORDER = 540,                   /* ORDER  */
    ORGANIZATION = 541,            /* ORGANIZATION  */
    OTHER = 542,                   /* OTHER  */
    OUTPUT = 543,                  /* OUTPUT  */
    OVERFLOW = 544,                /* OVERFLOW  */
    OVERLINE = 545,                /* OVERLINE  */
    PACKED_DECIMAL = 546,          /* "PACKED-DECIMAL"  */
    PADDING = 547,                 /* PADDING  */
    PAGE = 548,                    /* PAGE  */
    PAGE_FOOTING = 549,            /* "PAGE FOOTING"  */
    PAGE_HEADING = 550,            /* "PAGE HEADING"  */
    PARAGRAPH = 551,               /* PARAGRAPH  */
    PERFORM = 552,                 /* PERFORM  */
    PICTURE = 553,                 /* PICTURE  */
    PLUS = 554,                    /* PLUS  */
    POINTER = 555,                 /* POINTER  */
    POSITION = 556,                /* POSITION  */
    POSITIVE = 557,                /* POSITIVE  */
    PRESENT = 558,                 /* PRESENT  */
    PREVIOUS = 559,                /* PREVIOUS  */
    PRINTER = 560,                 /* PRINTER  */
    PRINTING = 561,                /* PRINTING  */
    PROCEDURE = 562,               /* PROCEDURE  */
    PROCEDURES = 563,              /* PROCEDURES  */
    PROCEED = 564,                 /* PROCEED  */
    PROGRAM = 565,                 /* PROGRAM  */
    PROGRAM_ID = 566,              /* "PROGRAM-ID"  */
    PROGRAM_NAME = 567,            /* "Program name"  */
    PROGRAM_POINTER = 568,         /* "PROGRAM-POINTER"  */
    PROMPT = 569,                  /* PROMPT  */
    QUOTE = 570,                   /* QUOTE  */
    RANDOM = 571,                  /* RANDOM  */
    RD = 572,                      /* RD  */
    READ = 573,                    /* READ  */
    RECORD = 574,                  /* RECORD  */
    RECORDING = 575,               /* RECORDING  */
    RECORDS = 576,                 /* RECORDS  */
    RECURSIVE = 577,               /* RECURSIVE  */
    REDEFINES = 578,               /* REDEFINES  */
    REEL = 579,                    /* REEL  */
    REFERENCE = 580,               /* REFERENCE  */
    RELATIVE = 581,                /* RELATIVE  */
    RELEASE = 582,                 /* RELEASE  */
    REMAINDER = 583,               /* REMAINDER  */
    REMOVAL = 584,                 /* REMOVAL  */
    RENAMES = 585,                 /* RENAMES  */
    REPLACING = 586,               /* REPLACING  */
    REPORT = 587,                  /* REPORT  */
    REPORTING = 588,               /* REPORTING  */
    REPORTS = 589,                 /* REPORTS  */
    REPORT_FOOTING = 590,          /* "REPORT FOOTING"  */
    REPORT_HEADING = 591,          /* "REPORT HEADING"  */
    REPOSITORY = 592,              /* REPOSITORY  */
    REQUIRED = 593,                /* REQUIRED  */
    RESERVE = 594,                 /* RESERVE  */
    RETURN = 595,                  /* RETURN  */
    RETURNING = 596,               /* RETURNING  */
    REVERSE_FUNC = 597,            /* "FUNCTION REVERSE"  */
    REVERSE_VIDEO = 598,           /* "REVERSE-VIDEO"  */
    REWIND = 599,                  /* REWIND  */
    REWRITE = 600,                 /* REWRITE  */
    RIGHT = 601,                   /* RIGHT  */
    ROLLBACK = 602,                /* ROLLBACK  */
    ROUNDED = 603,                 /* ROUNDED  */
    RUN = 604,                     /* RUN  */
    SAME = 605,                    /* SAME  */
    SCREEN = 606,                  /* SCREEN  */
    SCREEN_CONTROL = 607,          /* "SCREEN-CONTROL"  */
    SCROLL = 608,                  /* SCROLL  */
    SD = 609,                      /* SD  */
    SEARCH = 610,                  /* SEARCH  */
    SECTION = 611,                 /* SECTION  */
    SECURE = 612,                  /* SECURE  */
    SEGMENT_LIMIT = 613,           /* "SEGMENT-LIMIT"  */
    SELECT = 614,                  /* SELECT  */
    SEMI_COLON = 615,              /* "semi-colon"  */
    SENTENCE = 616,                /* SENTENCE  */
    SEPARATE = 617,                /* SEPARATE  */
    SEQUENCE = 618,                /* SEQUENCE  */
    SEQUENTIAL = 619,              /* SEQUENTIAL  */
    SET = 620,                     /* SET  */
    SHARING = 621,                 /* SHARING  */
    SIGN = 622,                    /* SIGN  */
    SIGNED = 623,                  /* SIGNED  */
    SIGNED_INT = 624,              /* "SIGNED-INT"  */
    SIGNED_LONG = 625,             /* "SIGNED-LONG"  */
    SIGNED_SHORT = 626,            /* "SIGNED-SHORT"  */
    SIZE = 627,                    /* SIZE  */
    SIZE_ERROR = 628,              /* "SIZE ERROR"  */
    SORT = 629,                    /* SORT  */
    SORT_MERGE = 630,              /* "SORT-MERGE"  */
    SOURCE = 631,                  /* SOURCE  */
    SOURCE_COMPUTER = 632,         /* "SOURCE-COMPUTER"  */
    SPACE = 633,                   /* SPACE  */
    SPECIAL_NAMES = 634,           /* "SPECIAL-NAMES"  */
    STANDARD = 635,                /* STANDARD  */
    STANDARD_1 = 636,              /* "STANDARD-1"  */
    STANDARD_2 = 637,              /* "STANDARD-2"  */
    START = 638,                   /* START  */
    STATUS = 639,                  /* STATUS  */
    STOP = 640,                    /* STOP  */
    STRING = 641,                  /* STRING  */
    SUBSTITUTE_FUNC = 642,         /* "FUNCTION SUBSTITUTE"  */
    SUBSTITUTE_CASE_FUNC = 643,    /* "FUNCTION SUBSTITUTE-CASE"  */
    SUBTRACT = 644,                /* SUBTRACT  */
    SUM = 645,                     /* SUM  */
    SUPPRESS = 646,                /* SUPPRESS  */
    SYMBOLIC = 647,                /* SYMBOLIC  */
    SYNCHRONIZED = 648,            /* SYNCHRONIZED  */
    TALLYING = 649,                /* TALLYING  */
    TAPE = 650,                    /* TAPE  */
    TERMINATE = 651,               /* TERMINATE  */
    TEST = 652,                    /* TEST  */
    THAN = 653,                    /* THAN  */
    THEN = 654,                    /* THEN  */
    THRU = 655,                    /* THRU  */
    TIME = 656,                    /* TIME  */
    TIMES = 657,                   /* TIMES  */
    TO = 658,                      /* TO  */
    TOK_FALSE = 659,               /* "FALSE"  */
    TOK_FILE = 660,                /* "FILE"  */
    TOK_INITIAL = 661,             /* "INITIAL"  */
    TOK_NULL = 662,                /* "NULL"  */
    TOK_TRUE = 663,                /* "TRUE"  */
    TOP = 664,                     /* TOP  */
    TRACKS = 665,                  /* TRACKS  */
    TRAILING = 666,                /* TRAILING  */
    TRANSFORM = 667,               /* TRANSFORM  */
    TRIM_FUNCTION = 668,           /* "FUNCTION TRIM"  */
    TYPE = 669,                    /* TYPE  */
    UNDERLINE = 670,               /* UNDERLINE  */
    UNIT = 671,                    /* UNIT  */
    UNLOCK = 672,                  /* UNLOCK  */
    UNSIGNED = 673,                /* UNSIGNED  */
    UNSIGNED_INT = 674,            /* "UNSIGNED-INT"  */
    UNSIGNED_LONG = 675,           /* "UNSIGNED-LONG"  */
    UNSIGNED_SHORT = 676,          /* "UNSIGNED-SHORT"  */
    UNSTRING = 677,                /* UNSTRING  */
    UNTIL = 678,                   /* UNTIL  */
    UP = 679,                      /* UP  */
    UPDATE = 680,                  /* UPDATE  */
    UPON = 681,                    /* UPON  */
    UPON_ARGUMENT_NUMBER = 682,    /* "UPON ARGUMENT-NUMBER"  */
    UPON_COMMAND_LINE = 683,       /* "UPON COMMAND-LINE"  */
    UPON_ENVIRONMENT_NAME = 684,   /* "UPON ENVIRONMENT-NAME"  */
    UPON_ENVIRONMENT_VALUE = 685,  /* "UPON ENVIRONMENT-VALUE"  */
    UPPER_CASE_FUNC = 686,         /* "FUNCTION UPPER-CASE"  */
    USAGE = 687,                   /* USAGE  */
    USE = 688,                     /* USE  */
    USING = 689,                   /* USING  */
    VALUE = 690,                   /* VALUE  */
    VARYING = 691,                 /* VARYING  */
    WAIT = 692,                    /* WAIT  */
    WHEN = 693,                    /* WHEN  */
    WHEN_COMPILED_FUNC = 694,      /* "FUNCTION WHEN-COMPILED"  */
    WHEN_OTHER = 695,              /* "WHEN OTHER"  */
    WITH = 696,                    /* WITH  */
    WORD = 697,                    /* "Identifier"  */
    WORDS = 698,                   /* WORDS  */
    WORKING_STORAGE = 699,         /* "WORKING-STORAGE"  */
    WRITE = 700,                   /* WRITE  */
    YYYYDDD = 701,                 /* YYYYDDD  */
    YYYYMMDD = 702,                /* YYYYMMDD  */
    ZERO = 703,                    /* ZERO  */
    UNARY_SIGN = 704               /* UNARY_SIGN  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_PARSER_H_INCLUDED  */
