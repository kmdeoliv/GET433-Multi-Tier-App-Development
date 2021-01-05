       IDENTIFICATION DIVISION.
       PROGRAM-ID. SU41PGM5.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT IN-FILE     ASSIGN TO IFILE.
            COPY VSAMSEL.
            SELECT OUT-FILE    ASSIGN TO OFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC.
           05  IN-NAME        PIC X(20).
           05  IN-ADDRESS     PIC X(20).
           05  IN-SSN         PIC 9(9).
           05  IN-CREDITS     PIC 99.
           05  FILLER         PIC X(29).

       COPY VSAMFD.
      * SHARE.GET.433.COPYLIB

       FD  OUT-FILE.
       01  OUT-REC.
           05  OUT-NAME       PIC X(20).
           05  OUT-ADDRESS    PIC X(20).
           05  OUT-SSN        PIC 9(9).
           05  OUT-CREDITS    PIC 99.
           05  OUT-YR-GRAD    PIC X(4).
           05  MY-FILLER      PIC X(25) VALUE SPACES.

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  EOF-SW         PIC X     VALUE SPACES.
           COPY VSAMRC.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM OPEN-FILES-RTN.
           PERFORM READ-INPUT
              UNTIL EOF-SW = 'F'.
           PERFORM CLOSE-FILES-RTN.
           STOP RUN.

         OPEN-FILES-RTN.
           OPEN INPUT IN-FILE.
           OPEN INPUT VSAM-INFILE.
           OPEN OUTPUT OUT-FILE.
            READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
            MOVE IN-SSN TO VSAM-KEY.
         OPEN-FILES-RTN-EXIT. EXIT.

        READ-INPUT.
           READ VSAM-INFILE.
            IF VSAM-SUCCESSFUL
               MOVE VR-YEAR-OF-GRAD  TO OUT-YR-GRAD.
            IF NOT VSAM-SUCCESSFUL
               MOVE '????' TO  OUT-YR-GRAD.
            PERFORM WRITE-OUTPUT.
            READ IN-FILE
             AT END MOVE 'F' TO EOF-SW.
            MOVE IN-SSN TO VSAM-KEY.
        READ-INPUT-EXIT. EXIT.

         WRITE-OUTPUT.
           MOVE IN-NAME     TO OUT-NAME.
           MOVE IN-ADDRESS  TO OUT-ADDRESS.
           MOVE IN-SSN      TO OUT-SSN.
           MOVE IN-CREDITS  TO OUT-CREDITS.
           WRITE OUT-REC.
        WRITE-OUTPUT-EXIT. EXIT.

       CLOSE-FILES-RTN.
           CLOSE IN-FILE VSAM-INFILE OUT-FILE.
       CLOSE-FILES-RTN-EXIT. EXIT.