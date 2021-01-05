       IDENTIFICATION DIVISION.
       PROGRAM-ID. P49ACCT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *                                                           ******
      *    SAMPLE OF A VERY SIMPLE LOOP PROGRAM                   ******
      *    SAMPLE OF A VERY SIMPLE LOOP PROGRAM                   ******
      *                                                           ******
      *                                                           ******
       WORKING-STORAGE SECTION.
      *                                *********************************
      *                                * GENERAL WORKING STORAGE FIELDS*
      *                                *********************************
       01 WORKING-AREA-FIELDS.
          03 DONE-MSG                  PIC X(30) VALUE 'WELL DONE'.
          03 SCREEN-COUNTER            PIC 9999  VALUE 0.
          03 SCREEN-COUNTERX REDEFINES SCREEN-COUNTER
                                       PIC XXXX.
          03 VSAMFILE                  PIC X(8)  VALUE 'VSAMFILE'.
          03 VSAMAIX                   PIC X(8)  VALUE 'VSAMAIX '.
          03 TS-QUEUE                  PIC X(8)  VALUE '3270BRDG'.
          03 RESP                      PIC 9(4)  COMP.
          03 CONSOLE-MSG.
              05 FILLER                PIC X(10) VALUE 'P49ACCT:  '.
              05 MSG                   PIC X(50).
      *
          03 END-MSG.
              05 FILLER                PIC X(12) VALUE SPACES.
              05 FILLER                PIC X(45)
                 VALUE 'TRANSACTION TERMINATED DUE TO YOUR REQUEST.'.
      *
          03 RESP-MSG.
              05 FILLER                PIC X(37)
                 VALUE '"P49ACCT " INTERNAL ERROR, RESP CODE='.
              05 RESP-NUMBER           PIC 9(5).
      *                                *********************************
      *                                * CICS COMMAREA DEFENITIONS.    *
      *                                *********************************
       01 WS-COMMAREA.
          COPY ACCTREC.
      *                                *********************************
      *                                * TCH MAP STRUCTURE.            *
      *                                *********************************
       COPY M49ACCT.
       COPY DFHAID.
      *                                *********************************
      *                                * LINKAGE SECTION               *
      *                                *********************************
       LINKAGE SECTION.
       01  DFHCOMMAREA                   PIC X.
      *                                *********************************
      *                                * MAIN PROCEDURE                *
      *                                *********************************
       PROCEDURE DIVISION.
      *                                *********************************
      *                                * CLEAR     MAP AREAS AND       *
      *                                * COMMAREA.                     *
      *                                *********************************
       RETRY-AGAIN SECTION.
           MOVE LOW-VALUES TO MENUI.
           MOVE LOW-VALUES TO WS-COMMAREA.
      *                                *********************************
      *                                * DISPLAY MAIN MENU.            *
      *                                *********************************
      *
       SEND-MSG.

            ADD 1 TO SCREEN-COUNTER.
            MOVE SCREEN-COUNTERX TO COUNTERO.

            EXEC CICS SEND MAP('MENU') MAPSET('M49ACCT')
                FREEKB ERASE RESP(RESP)
                END-EXEC.
      *                                *********************************
      *                                * CHECK CICS RESPONSE CODES     *
      *                                *********************************
      *                                *********************************
      *                                * RECEIVE MAIN MENU.            *
      *                                *********************************

            EXEC CICS RECEIVE MAP('MENU') MAPSET('M49ACCT')
                RESP(RESP)
                END-EXEC.
      *                                *********************************
      *                                * CHECK WHICH KEY WAS USED      *
      *                                *********************************
      *                                *********************************
      *                                * CHECK CICS RESPONSE CODES     *
      *                                *********************************
      *                                *********************************
      *                                * EXAMINE REQUEST CODE AND SET  *
      *
            IF EIBAID = DFHPF3
                   EXEC CICS SEND TEXT
                      FROM(DONE-MSG)  ERASE FREEKB
                   END-EXEC
                   EXEC CICS RETURN END-EXEC
            ELSE
                GO TO RETRY-AGAIN.