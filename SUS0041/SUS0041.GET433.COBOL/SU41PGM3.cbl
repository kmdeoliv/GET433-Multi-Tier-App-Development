       IDENTIFICATION DIVISION.
       PROGRAM-ID. SU41PGM3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT IN-FILE     ASSIGN TO IFILE.
            SELECT IN-FILE-2   ASSIGN TO IFILE2.
            SELECT OUT-FILE    ASSIGN TO OFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE
       01  IN-REC.
           05  IN-NAME        PIC X(20).
           05  IN-ADDRESS     PIC X(20).
           05  IN-SSN         PIC 9(9).
           05  IN-CREDITS     PIC 99.
           05  FILLER         PIC X(29).

       FD  IN-FILE-2
       01  IN-REC2.
           05  FILLER         PIC X(40).
           05  IN-SSN-2       PIC 9(9).
           05  IN-CREDITS-2   PIC 99.
           05  IN-YR-GRAD     PIC X(4).
           05  FILLER         PIC X(25).

       FD  OUT-FILE

       01  OUT-REC.
           05  OUT-NAME       PIC X(20).
           05  OUT-ADDRESS    PIC X(20).
           05  OUT-SSN        PIC 9(9).
           05  OUT-CREDITS    PIC 99.
           05  OUT-YR-GRAD    PIC X(4).
           05  MY-FILLER      PIC X(25)
           VALUE   '                        '.

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  IN-CTR         PIC 9(5)  COMP-3  VALUE 0.
           05  IN-CTR-2       PIC 9(5)  COMP-3  VALUE 0.
           05  EOF-SW         PIC X             VALUE SPACES.
           05  EOF-SW-2       PIC X             VALUE SPACES.
           05  MATCH-SWITCH-1 PIC X VALUE SPACES.
           05  MATCH-SWITCH-2 PIC X VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM OPEN-FILES-RTN.
           PERFORM READ-INPUT
              UNTIL EOF-SW = 'F'.
           PERFORM CLOSE-FILES-RTN.
           STOP RUN.

       OPEN-FILES-RTN.
           OPEN INPUT IN-FILE.
           OPEN INPUT IN-FILE-2.
           OPEN OUTPUT OUT-FILE.
           READ IN-FILE
               AT END MOVE 'F' TO EOF-SW.
           READ IN-FILE-2
                AT END MOVE 'F' TO EOF-SW-2.
       OPEN-FILES-RTN-EXIT. EXIT.

       READ-INPUT.
           MOVE SPACES TO MATCH-SWITCH-1.
           PERFORM MATCH-ROUTINE
                UNTIL MATCH-SWITCH-1 > SPACES.
           READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
       READ-INPUT-EXIT. EXIT.

       MATCH-ROUTINE.
            IF IN-SSN = IN-SSN-2
                MOVE IN-YR-GRAD  TO OUT-YR-GRAD
                PERFORM WRITE-OUTPUT
                MOVE 'F' TO MATCH-SWITCH-1.
            IF IN-SSN > IN-SSN-2
                PERFORM  READ-INFILE-2-ROUTINE.
            IF IN-SSN < IN-SSN-2
                MOVE '????' TO  OUT-YR-GRAD
                PERFORM WRITE-OUTPUT
                MOVE 'F' TO MATCH-SWITCH-1.
       MATCH-ROUTINE-EXIT. EXIT.

       READ-INFILE-2-ROUTINE.
            READ IN-FILE-2
                  AT END MOVE 'F' TO EOF-SW-2.
       READ-INFILE-2-ROUTINE-EXIT. EXIT.

       WRITE-OUTPUT.
           MOVE IN-NAME     TO OUT-NAME.
           MOVE IN-ADDRESS  TO OUT-ADDRESS.
           MOVE IN-SSN      TO OUT-SSN.
           MOVE IN-CREDITS  TO OUT-CREDITS.
           MOVE SPACES      TO MY-FILLER.
           WRITE OUT-REC.
       WRITE-OUTPUT-EXIT. EXIT.

       CLOSE-FILES-RTN.
           CLOSE IN-FILE IN-FILE-2 OUT-FILE.
       CLOSE-FILES-RTN-EXIT. EXIT.