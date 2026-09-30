       IDENTIFICATION DIVISION.
       PROGRAM-ID. calc.
      *> Called from the Spring Boot controller on a request thread.
      *> Computes a few values from the request and calls a subprogram
      *> whose WORKING-STORAGE counts the calls made on this thread.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CALLS     PIC 9(6) VALUE 0.
       01 NUM       PIC 9(6).
       01 SQUARE    PIC 9(12).
       01 TXT       PIC X(10).
       01 UPP       PIC X(10).
       01 CNT       PIC 9(2).
       01 SUB-CALLS PIC 9(6).
       LINKAGE SECTION.
       01 REQUEST.
          05 REQ-NUM  PIC 9(6).
          05 REQ-TXT  PIC X(10).
       01 RESPONSE   PIC X(100).
       PROCEDURE DIVISION USING REQUEST RESPONSE.
           ADD 1 TO CALLS.
           MOVE REQ-NUM TO NUM.
           COMPUTE SQUARE = NUM * NUM.
           MOVE REQ-TXT TO TXT.
           MOVE FUNCTION UPPER-CASE(TXT) TO UPP.
           MOVE 0 TO CNT.
           INSPECT TXT TALLYING CNT FOR ALL "a".
           CALL "subcount" USING SUB-CALLS.
           MOVE SPACES TO RESPONSE.
           STRING "num=" NUM " square=" SQUARE " upper=" UPP
                  " a=" CNT " calls=" CALLS " subcalls=" SUB-CALLS
                  DELIMITED BY SIZE INTO RESPONSE.
           GOBACK.
