       IDENTIFICATION DIVISION.
       PROGRAM-ID. SU41PGM8.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT IN-FILE     ASSIGN TO IFILE.
            SELECT IN-FILE-2   ASSIGN TO IFILE2.
            SELECT OUT-FILE    ASSIGN TO OFILE.
            SELECT EXC-FILE    ASSIGN TO EFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC.
           05  IN-NAME        PIC X(20).
           05  IN-ADDRESS     PIC X(20).
           05  IN-SSN         PIC 9(9).
           05  IN-CREDITS     PIC 99.
           05  FILLER         PIC X(29).

       FD  IN-FILE-2.
       01  IN-REC2.
           05  FILLER         PIC X(40).
           05  IN-SSN-2       PIC 9(9).
           05  IN-CREDITS-2   PIC 99.
           05  FILLER         PIC X(29).

       FD  OUT-FILE.
       01  OUT-REC.
           05  OUT-NAME       PIC X(20).
           05  OUT-ADDRESS    PIC X(20).
           05  OUT-SSN        PIC 9(9).
           05  OUT-CREDITS    PIC 99.
           05  MY-FILLER      PIC X(29).

       FD  EXC-FILE.
       01  EXC-REC.
           05  EXC-FILLER     PIC X(40).
           05  EXC-SSN        PIC 9(9).
           05  EXC-CREDITS    PIC 99.
           05  EXC-FILLER-2   PIC X(29).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  EOF-SW         PIC X         VALUE SPACES.
           05  EOF-SW-2       PIC X         VALUE SPACES.
           05  PURGE-SW       PIC X.

       01 WS-TABLE-AREA VALUE HIGH-VALUES.
           05 WS-TABLE OCCURS 5 TIMES
                   ASCENDING KEY IS TBL-SSN
                   INDEXED BY TBL-INDEX.
             10  TBL-SSN         PIC 9(9).
             10  TBL-CREDITS     PIC 99.
             10  TBL-SW          PIC X.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM OPEN-FILES-RTN.
           PERFORM READ-INFILE-2
              UNTIL EOF-SW-2 = 'F'.
           SET TBL-INDEX TO 1.
           PERFORM READ-INPUT-1
              UNTIL EOF-SW = 'F'.
           SET TBL-INDEX TO 1.
           PERFORM WRITE-EXCEPTION VARYING TBL-INDEX
                FROM 1 BY 1 UNTIL TBL-INDEX >5
           PERFORM CLOSE-FILES-RTN.
           STOP RUN.

       OPEN-FILES-RTN.
           OPEN INPUT IN-FILE.
           READ IN-FILE
                AT END MOVE 'F' TO EOF-SW.
           OPEN INPUT IN-FILE-2.
           READ IN-FILE-2
                AT END MOVE 'F' TO EOF-SW-2.
           OPEN OUTPUT OUT-FILE.
           OPEN OUTPUT EXC-FILE.
       OPEN-FILES-RTN-EXIT. EXIT.

       READ-INFILE-2.
           PERFORM MOVE-DATA-ROUTINE.
           READ IN-FILE-2
             AT END MOVE 'F' TO EOF-SW-2.
       READ-INFILE-2-EXIT. EXIT.

       MOVE-DATA-ROUTINE.
           MOVE IN-SSN-2 TO TBL-SSN(TBL-INDEX).
           DISPLAY 'TABLE: ' TBL-SSN(TBL-INDEX).
           MOVE IN-CREDITS-2 TO TBL-CREDITS(TBL-INDEX).
           DISPLAY 'TABLE CREDITS: ' TBL-CREDITS(TBL-INDEX).
           SET TBL-INDEX UP BY 1.
       MOVE-DATA-ROUTINE-EXIT. EXIT.

       READ-INPUT-1.

           PERFORM MATCH-ROUTINE.
           EVALUATE PURGE-SW
                WHEN 'N'
                    PERFORM WRITE-OUTPUT
                WHEN 'F'
                    DISPLAY "FILE-DELETED"
           END-EVALUATE.
           READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
       READ-INPUT-1-EXIT. EXIT.

       MATCH-ROUTINE.
           SEARCH ALL WS-TABLE
               AT END MOVE 'N' TO PURGE-SW
               WHEN TBL-SSN(TBL-INDEX) = IN-SSN
                   MOVE 'F' TO PURGE-SW
                   MOVE SPACES TO TBL-SW(TBL-INDEX)
                   DISPLAY 'FILE PURGED: ' IN-SSN
           END-SEARCH.
       MATCH-ROUTINE-EXIT. EXIT.

       WRITE-OUTPUT.
           MOVE IN-NAME     TO OUT-NAME.
           MOVE IN-ADDRESS  TO OUT-ADDRESS.
           MOVE IN-SSN      TO OUT-SSN.
           MOVE IN-CREDITS  TO OUT-CREDITS.
           WRITE OUT-REC.
       WRITE-OUTPUT-EXIT. EXIT.

       WRITE-EXCEPTION.
           IF TBL-SW(TBL-INDEX)= HIGH-VALUE
                MOVE SPACES TO EXC-FILLER
                MOVE TBL-SSN(TBL-INDEX) TO EXC-SSN
                MOVE TBL-CREDITS(TBL-INDEX) TO EXC-CREDITS
                MOVE SPACES TO EXC-FILLER-2
                WRITE EXC-REC.
       WRITE-EXCEPTION-EXIT. EXIT.

       CLOSE-FILES-RTN.
           CLOSE IN-FILE IN-FILE-2 OUT-FILE EXC-FILE.
       CLOSE-FILES-RTN-EXIT. EXIT.