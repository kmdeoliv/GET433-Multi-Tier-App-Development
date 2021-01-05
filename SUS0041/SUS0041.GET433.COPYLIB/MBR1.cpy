      **** Payroll Record Length 360 characters ***********************
      **** Do not change any names or picture clauses below ***********
       01  PAYROLL-RECORD.
           05  PR-EMPLOYEE-ID         PIC  X(07).
           05  PR-LAST-NAME           PIC  X(26).
           05  PR-FIRST-NAME          PIC  X(15).
           05  PR-DIVISION            PIC  X(10).
           05  PR-GROUP               PIC  X(10).
           05  PR-DEPARTMENT          PIC  X(10).
           05  PR-TITLE               PIC  X(20).
           05  PR-ADDRESS1            PIC  X(30).
           05  PR-ADDRESS2            PIC  X(30).
           05  PR-CITY                PIC  X(30).
           05  PR-STATE               PIC  X(2).
           05  PR-PROVINCE            PIC  X(20).
           05  PR-COUNTRY             PIC  X(20).
           05  PR-ZIP-POSTAL          PIC  X(09).
           05  PR-PAY1-STATUS         PIC  X(01).
               88  PR-STAT-ACTIVE         VALUE 'A'.
               88  PR-STAT-TERMINATED     VALUE 'T'.
               88  PR-STAT-RETIRED        VALUE 'R'.
               88  PR-STAT-DISCHARGED     VALUE 'D'.
           05  PR-START-DT            PIC  9(08).
           05  PR-TERM-DT             PIC  9(08).
           05  PR-TERM-REASON         PIC  X(30).
           05  PR-PAY-RATE            PIC  9(9)V99.
           05  PR-PAY-CYCLE           PIC  X(01).
               88  PR-HOURLY              VALUE 'H'.
               88  PR-BIWEEKLY            VALUE 'B'.
               88  PR-YEARLY              VALUE 'R'.
           05  PR-LAST-INCREASE-DT    PIC  9(08).
           05  PR-LAST-EVAL-DT        PIC  9(08).
           05  PR-LAST-EVAL-RATING    PIC  X(01).
               88  PR-RATE-A              VALUE 'A'.
               88  PR-RATE-B              VALUE 'B'.
               88  PR-RATE-C              VALUE 'C'.
               88  PR-RATE-D              VALUE 'D'.
               88  PR-RATE-N              VALUE 'N'.
           05  PR-BENEFIT-FLAG        PIC  X(01).
               88  PR-BENEFIT-YES         VALUE 'Y'.
               88  PR-BENEFIT-NO          VALUE 'N'.
           05  PR-MEDICAL-FLAG        PIC  X(01).
               88  PR-MEDICAL-YES         VALUE 'Y'.
               88  PR-MEDICAL-NO          VALUE 'N'.
           05  PR-DENTAL-FLAG         PIC  X(01).
               88  PR-DENTAL-YES          VALUE 'Y'.
               88  PR-DENTAL-NO           VALUE 'N'.
           05  PR-VISION-FLAG         PIC  X(01).
               88  PR-VISION-YES          VALUE 'Y'.
               88  PR-VISION-NO           VALUE 'N'.
           05  PR-BENEFIT-COST-YR     PIC  9(9)V99.
           05                         PIC  X(30).
      **** Do not add any data fields below this **********************
      **** End of Payroll Record **************************************