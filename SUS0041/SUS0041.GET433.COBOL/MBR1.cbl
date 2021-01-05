       IDENTIFICATION DIVISION.
       PROGRAM-ID. SU##PGMT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT IN-FILE   ASSIGN TO IFILE.
            SELECT OUT-FILE  ASSIGN TO OFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC.
           05  IN-NAME        PIC X(20).
           05  IN-ADDRESS     PIC X(20).
           05  IN-SSN         PIC 9(9).
           05  IN-CREDITS     PIC 99.
           05  FILLER         PIC X(29).

       FD  OUT-FILE.
       01  OUT-REC.
           05  OUT-NAME       PIC X(20).
           05  OUT-SSN        PIC 9(9).
           05  OUT-CREDITS    PIC 99.

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  IN-CTR         PIC 9(5)  COMP-3  VALUE 0.
           05  OUT-CTR        PIC 9(5)  COMP-3  VALUE 0.
           05  EOF-SW         PIC X             VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM OPEN-FILES-RTN.
           PERFORM READ-INPUT
               UNTIL EOF-SW = 'F'.
           PERFORM CLOSE-FILES-RTN.
           PERFORM DISPLAY-REC-COUNTS.

           STOP RUN.
       OPEN-FILES-RTN.
           OPEN INPUT IN-FILE.

           OPEN OUTPUT OUT-FILE.

           READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
           ADD 1 TO IN-CTR.

       OPEN-FILES-RTN-EXIT. EXIT.
       WRITE-OP-RTN.
           WRITE OUT-REC.
           ADD 1 TO OUT-CTR.
       WRITE-OP-RTN-EXIT. EXIT.
       READ-INPUT.
           IF IN-CREDITS  > 11
               MOVE IN-NAME TO OUT-NAME
               MOVE IN-SSN  TO OUT-SSN
               MOVE IN-CREDITS TO OUT-CREDITS
               WRITE OUT-REC
               ADD 1 TO OUT-CTR
           END-IF.
           READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
           ADD 1 TO IN-CTR.
       READ-INPUT-EXIT. EXIT.
       DISPLAY-REC-COUNTS.
           DISPLAY 'RECS IN  '  IN-CTR.
           DISPLAY 'RECS OUT '  OUT-CTR.
       DISPLAY-REC-COUNTS-EXIT. EXIT.
       CLOSE-FILES-RTN.
           CLOSE IN-FILE OUT-FILE.
       CLOSE-FILES-RTN-EXIT. EXIT.
