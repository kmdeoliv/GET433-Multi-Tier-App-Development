       IDENTIFICATION DIVISION.
       PROGRAM-ID. P42ACCT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 WIN-MILLISECS                PIC 9999 VALUE 5000.
       01 WORKING-AREA-FIELDS.
          03 DONE-MSG                  PIC X(30) VALUE 'WELL DONE'.
          03 SCREEN-COUNTER            PIC 9999  VALUE 0.
          03 SCREEN-COUNTERX REDEFINES SCREEN-COUNTER
                                       PIC XXXX.
          03 CHAR                      PIC A.
          03 CHARX REDEFINES CHAR
                                       PIC X.
          03 RESP                      PIC 9(4)  COMP.

       01 WS-COMMAREA.
          COPY ACCTREC.

       COPY M42ACCT.
       COPY DFHAID.

       LINKAGE SECTION.
       01  DFHCOMMAREA                   PIC X.

       PROCEDURE DIVISION.

       RETRY-AGAIN SECTION.
           MOVE LOW-VALUES TO WS-COMMAREA.

       SEND-MSG.

            ADD 1 TO SCREEN-COUNTER.
            MOVE SCREEN-COUNTERX TO COUNTERO.


            EXEC CICS SEND MAP('MENU') MAPSET('M42ACCT')
                FREEKB ERASE RESP(RESP)
                END-EXEC.
             MOVE LOW-VALUES TO SMSGI
                MOVE LOW-VALUES TO RMSGO.


            EXEC CICS RECEIVE MAP('MENU') MAPSET('M42ACCT')
                RESP(RESP)
                END-EXEC.




            IF EIBAID = DFHPF3
                   EXEC CICS SEND TEXT
                      FROM(DONE-MSG)  ERASE FREEKB
                   END-EXEC
                   EXEC CICS RETURN END-EXEC

             ELSE
                 MOVE SMSGI TO CHAR
                 MOVE CHARX TO RMSGO
                 EXEC CICS SEND MAP('MENU') MAPSET('M42ACCT')
                    FREEKB ERASE RESP(RESP)
                 END-EXEC
                GO TO RETRY-AGAIN.



