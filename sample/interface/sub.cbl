       identification              division.
       program-id.                 sub.

       data                        division.

       linkage section.
       01 arg-string pic x(5).
       01 arg-num pic 9(3).
       procedure    division using arg-string arg-num.
       main section.
         display arg-string.
         display arg-num.
         add 1 to arg-num.
