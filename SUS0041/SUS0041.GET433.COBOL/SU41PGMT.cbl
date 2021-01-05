       IDENTIFICATION DIVISION.
       PROGRAM-ID. SU41PGMT.
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
           05  IN-YR-GRAD     PIC X(4).
               88  VALID-FOR-EXTRACT VALUES '2017' THRU '9999'.
           05  FILLER         PIC X(25).

       FD  OUT-FILE.
       01  OUT-REC.
           05  OUT-NAME       PIC X(20).
           05  OUT-ADDRESS     PIC X(20).
           05  OUT-SSN         PIC 9(9).
           05  OUT-CREDITS     PIC 99.
           05  OUT-YR-GRAD     PIC X(4).
           05  FILLER         PIC X(25).


       WORKING-STORAGE SECTION.

           COPY PAYREC.

       01  WS-WORK-AREA.
           05  IN-CTR         PIC 9(5)  COMP-3  VALUE 0.
           05  OUT-CTR        PIC 9(5)  COMP-3  VALUE 0.
           05  EOF-SW         PIC X             VALUE SPACES.
           05  SUBA           PIC 9(3)  COMP-3  VALUE 0.
       01  WS-TABLE-AREA.
           05  FILLER OCCURS 100 TIMES.
               10  FLDA       PIC XXX.
               10  FLDB       PIC XXX.
       PROCEDURE DIVISION.
       MAIN-RTN.

           PERFORM OPEN-FILES-RTN.

           PERFORM READ-INPUT
               UNTIL EOF-SW = 'F'.

           PERFORM CLOSE-FILES-RTN.

           PERFORM DISPLAY-COUNTERS.

           STOP RUN.

       OPEN-FILES-RTN.
           OPEN INPUT IN-FILE.

           OPEN OUTPUT OUT-FILE.

           READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
           ADD 1 TO IN-CTR.

       OPEN-FILES-RTN-EXIT. EXIT.
       WRITE-OUTPUT.
           MOVE IN-NAME     TO OUT-NAME.
           MOVE IN-ADDRESS  TO OUT-ADDRESS.
           MOVE IN-SSN      TO OUT-SSN.
           MOVE IN-CREDITS  TO OUT-CREDITS.
           MOVE IN-YR-GRAD TO OUT-YR-GRAD.
           WRITE OUT-REC.
           ADD 1 TO OUT-CTR.
       WRITE-OUTPUT-EXIT. EXIT.
       READ-INPUT.
           IF VALID-FOR-EXTRACT
               PERFORM WRITE-OUTPUT
           END-IF.

           READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.

           ADD 1 TO IN-CTR.
       READ-INPUT-EXIT. EXIT.

       CLOSE-FILES-RTN.
           CLOSE IN-FILE OUT-FILE.
       CLOSE-FILES-RTN-EXIT. EXIT.

       DISPLAY-COUNTERS.
           DISPLAY ' RECORDS INPUT   '   IN-CTR.
           DISPLAY ' RECORDS OUTPUT  '   OUT-CTR.
       DISPLAY-COUNTERS-EXIT. EXIT.
