//SUS0041A JOB 1,'KELLY',NOTIFY=SUS0041
//JOBLIB   DD  DSN=SUS0041.GET433.LOADLIB,DISP=SHR
//STEP1    EXEC PGM=SU41PGM2
//DD1      DD DSN=SUS0041.GET433.LAB2.OUTPUT,
//            DISP=(MOD,DELETE),
//            SPACE=(TRK,1),
//            UNIT=SYSDA
//STEP2    EXEC PGM=SU41PGM2
//IFILE    DD DSN=SHARE.GET433.STUDENT.DATA(CBL1),DISP=SHR
//OFILE    DD DSN=SUS0041.GET433.LAB2.OUTPUT,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(BLKSIZE=0,LRECL=133,RECFM=FB),
//            SPACE=(CYL,(1,1),RLSE),
//            UNIT=SYSDA
/*