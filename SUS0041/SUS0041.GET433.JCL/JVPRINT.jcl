//SUS0041A JOB 1,'KELLY',NOTIFY=SUS0041
//STEP1    EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//IFILE    DD DSN=SHARE.GET433.TEST.VSAM,DISP=SHR
//SYSIN    DD DSN=SUS0041.GET433.CARDLIB(VPRINT),DISP=SHR
/*