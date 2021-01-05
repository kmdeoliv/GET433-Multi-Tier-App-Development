       IDENTIFICATION DIVISION.
       PROGRAM-ID. SU41PGM2.
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
           05  IN-SSN1        PIC 9(3).
           05  IN-SSN2        PIC 9(2).
           05  IN-SSN3        PIC 9(4).
           05  IN-CREDITS     PIC 99.
           05  FILLER         PIC X(29).

       FD  OUT-FILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS OUT-REC.
       01  OUT-REC            PIC X(133).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  IN-CTR         PIC 9(5)  COMP-3  VALUE 0.
           05  EOF-SW         PIC X             VALUE SPACES.


       01  HDG-LINE1.
           05 FILLER          PIC X(8)   VALUE ' BU03999'.
           05 FILLER          PIC X(125) VALUE SPACES.

       01  HDG-LINE2.
           05 FILLER          PIC X(54) VALUE SPACES.
           05 FILLER          PIC X(25)  VALUE
                     'PASS-EM   STATE   COLLEGE'.
           05 FILLER          PIC X(54)  VALUE 'STATE'.

       01  HDG-LINE3.
           05 FILLER          PIC X(20) VALUE SPACES.
           05 FILLER          PIC X(12) VALUE 'STUDENT NAME'.
           05 FILLER          PIC X(20) VALUE SPACES.
           05 FILLER          PIC X(7)  VALUE 'ADDRESS'.
           05 FILLER          PIC X(16) VALUE SPACES.
           05 FILLER          PIC X(7)  VALUE 'STUDENT'.
           05 FILLER          PIC X(7)  VALUE SPACES.
           05 FILLER          PIC X(6)  VALUE 'CREDIT'.
           05 FILLER          PIC X(3)  VALUE SPACES.
           05 FILLER          PIC X(7)  VALUE 'TUITION'.
           05 FILLER          PIC X(28) VALUE SPACES.

       01 HDG-LINE4.
           05 FILLER          PIC X(77) VALUE SPACES.
           05 FILLER          PIC X(2)  VALUE 'ID'.
           05 FILLER          PIC X(10) VALUE SPACES.
           05 FILLER          PIC X(5)  VALUE 'HOURS'.
           05 FILLER          PIC X(6)  VALUE SPACES.
           05 FILLER          PIC X(3)  VALUE 'DUE'.
           05 FILLER          PIC X(30) VALUE SPACES.

       01  DETAIL-LINE.
           05 FILLER          PIC X(14)  VALUE SPACES.
           05 DL-NAME         PIC X(25)  VALUE SPACES.
           05 FILLER          PIC X(4)   VALUE SPACES.
           05 DL-ADDRESS      PIC X(25)  VALUE SPACES.
           05 FILLER          PIC X(5)   VALUE SPACES.
           05 DL-SSN1         PIC X(3)   VALUE SPACES.
           05 FILLER          PIC X(1)   VALUE SPACES.
           05 DL-SSN2         PIC X(2)   VALUE SPACES.
           05 FILLER          PIC X(1)   VALUE SPACES.
           05 DL-SSN3         PIC X(4)   VALUE SPACES.
           05 FILLER          PIC X(6)   VALUE SPACES.
           05 DL-CREDIT-HOURS PIC 999    VALUE 0.
           05 FILLER          PIC X(5)   VALUE SPACES.
           05 DL-TUITION-Z    PIC $$,999.99.
           05 FILLER          PIC X(25)  VALUE SPACES.

       01 TRAILER-LINE.
           05 FILLER          PIC X(22)     VALUE SPACES.
           05 FILLER          PIC X(23)  VALUE
                 '*****  TOTAL STUDENTS  '.
           05 TL-STUDENT-TOTAL PIC ZZ9.
           05 FILLER          PIC X(85)     VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM OPEN-FILES-RTN.
           PERFORM HDG-ROUTINE.
           PERFORM READ-INPUT
              UNTIL EOF-SW = 'F'.
           PERFORM TRAILER-ROUTINE.
           PERFORM CLOSE-FILES-RTN.
           STOP RUN.

       OPEN-FILES-RTN.
           OPEN INPUT IN-FILE.
           OPEN OUTPUT OUT-FILE.
           READ IN-FILE
               AT END MOVE 'F' TO EOF-SW.
           ADD 1 TO IN-CTR.
       OPEN-FILES-RTN-EXIT. EXIT.

       HDG-ROUTINE.
           MOVE  HDG-LINE1 TO OUT-REC.
           WRITE OUT-REC.
           MOVE  HDG-LINE2 TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
           MOVE  HDG-LINE3 TO OUT-REC.
           WRITE OUT-REC.
           MOVE  HDG-LINE4 TO OUT-REC.
           WRITE OUT-REC.
       HDG-ROUTINE-EXIT. EXIT.

       READ-INPUT.
           PERFORM DETAIL-ROUTINE.
           READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
           ADD 1 TO IN-CTR.
       READ-INPUT-EXIT. EXIT.

       DETAIL-ROUTINE.
           MOVE IN-NAME TO DL-NAME.
           MOVE IN-ADDRESS TO DL-ADDRESS.
           MOVE IN-SSN1 TO DL-SSN1.
           MOVE IN-SSN2 TO DL-SSN2.
           MOVE IN-SSN3 TO DL-SSN3.
           MOVE IN-CREDITS TO DL-CREDIT-HOURS.
           MULTIPLY 800 BY IN-CREDITS GIVING DL-TUITION.
           MOVE DETAIL-LINE TO OUT-REC.
           WRITE  OUT-REC.
       DETAIL-ROUTINE-EXIT. EXIT.

       TRAILER-ROUTINE.
           MOVE IN-CTR TO TL-STUDENT-TOTAL.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
           MOVE TRAILER-LINE TO OUT-REC.
           WRITE OUT-REC.
       TRAILER-ROUTINE-EXIT. EXIT.

       CLOSE-FILES-RTN.
           CLOSE IN-FILE OUT-FILE.
       CLOSE-FILES-RTN-EXIT. EXIT.