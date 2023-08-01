000100 IDENTIFICATION DIVISION.                                         SQ1154.2
000200 PROGRAM-ID. sample.
003100 ENVIRONMENT DIVISION.                                            SQ1154.2
003200 CONFIGURATION SECTION.                                           SQ1154.2
003300 SOURCE-COMPUTER.                                                 SQ1154.2
003400     Linux.                                                       SQ1154.2
003500 OBJECT-COMPUTER.                                                 SQ1154.2
003600     Linux.                                                       SQ1154.2
003700 INPUT-OUTPUT SECTION.                                            SQ1154.2
003800 FILE-CONTROL.                                                    SQ1154.2
005000 DATA DIVISION.       
       working-storage section.
       01  COUNT-OF-RECORDS pic s9(5) computational.
       procedure division.
       main.
           MOVE ZERO TO COUNT-OF-RECORDS.
           perform test-run 10 times.
           stop run.
       test-run.
           ADD 1 TO COUNT-OF-RECORDS.
           display "counter=" COUNT-OF-RECORDS.
       