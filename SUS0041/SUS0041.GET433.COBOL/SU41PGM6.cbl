       IDENTIFICATION DIVISION.
       PROGRAM-ID. SU41PGM2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT IN-FILE   ASSIGN TO IFILE.
            SELECT OUT-FILE  ASSIGN TO OFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE
       01  IN-REC.
           05  IN-NAME        PIC X(20).
           05  IN-ADDRESS     PIC X(20).
           05  IN-SSN         PIC 9(9).
           05  IN-CREDITS     PIC 99.
           05  IN-YR-GRAD     PIC 9(4).
           05  FILLER         PIC X(25).

       FD  OUT-FILE
       01  OUT-REC            PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 IN-CTR           PIC 9(5)          VALUE 0.
           05 EOF-SW           PIC X             VALUE SPACES.
           05 DL-TUITION       PIC 99999999      VALUE 0.
           05 LAST-YR-GRAD     PIC 9(4)          VALUE 0.

       01  HDG-LINE1.
           05 FILLER          PIC X(22) VALUE SPACES.
           05 FILLER          PIC X(37)  VALUE
                     'STUDENT TUITION BY YEAR OF GRADUATION'.
           05 FILLER          PIC X(21)  VALUE SPACES.

       01  DETAIL-LINE.
           05 FILLER            PIC X(4)   VALUE SPACES.
           05 FILLER            PIC X(12)  VALUE 'YEAR OF GRAD'.
           05 FILLER            PIC X(4)   VALUE SPACES.
           05 DL-YR-GRAD        PIC 9(4)   VALUE 0.
           05 FILLER            PIC X(4)   VALUE SPACES.
           05 FILLER            PIC X(12)  VALUE 'NO. STUDENTS'.
           05 FILLER            PIC X(4)   VALUE SPACES.
           05 DL-NO-STUDENTS-Z  PIC ZZ9.
           05 FILLER            PIC X(4)   VALUE SPACES.
           05 FILLER            PIC X(11)  VALUE 'TUITION DUE'.
           05 FILLER            PIC X(5)   VALUE SPACES.
           05 DL-TUITION-Z      PIC $999,999.99.
           05 FILLER            PIC X(2)   VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM OPEN-FILES-RTN.
           PERFORM HDG-ROUTINE.
           PERFORM READ-INPUT
              UNTIL EOF-SW = 'F'.
           PERFORM DETAIL-ROUTINE.
           PERFORM CLOSE-FILES-RTN.
           STOP RUN.

       OPEN-FILES-RTN.
           OPEN INPUT IN-FILE.
           OPEN OUTPUT OUT-FILE.
            READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
           MOVE IN-YR-GRAD TO LAST-YR-GRAD.
       OPEN-FILES-RTN-EXIT. EXIT.

       HDG-ROUTINE.
           MOVE  HDG-LINE1 TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
       HDG-ROUTINE-EXIT. EXIT.

       READ-INPUT.
           PERFORM CALC-BY-YEAR.
           READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.

       READ-INPUT-EXIT. EXIT.

       CALC-BY-YEAR.
           IF IN-YR-GRAD = LAST-YR-GRAD
               ADD 1 TO IN-CTR
               COMPUTE DL-TUITION = (DL-TUITION + (IN-CREDITS * 800))
           END-IF.
           IF IN-YR-GRAD IS NOT = LAST-YR-GRAD
               PERFORM DETAIL-ROUTINE
               MOVE 1 TO IN-CTR
               MOVE IN-YR-GRAD TO LAST-YR-GRAD
               MOVE 0 TO DL-TUITION
               COMPUTE DL-TUITION = (DL-TUITION + (IN-CREDITS * 800))
           END-IF.
       CALC-BY-YEAR-EXIT. EXIT.

       DETAIL-ROUTINE.
           MOVE LAST-YR-GRAD TO DL-YR-GRAD.
           MOVE IN-CTR TO DL-NO-STUDENTS-Z.
           MOVE DL-TUITION TO DL-TUITION-Z.
           MOVE DETAIL-LINE TO OUT-REC.
           WRITE  OUT-REC.
       DETAIL-ROUTINE-EXIT. EXIT.

       CLOSE-FILES-RTN.
           CLOSE IN-FILE OUT-FILE.
       CLOSE-FILES-RTN-EXIT. EXIT.