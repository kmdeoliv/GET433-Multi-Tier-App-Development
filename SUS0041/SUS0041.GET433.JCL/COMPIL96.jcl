//SUS0041C JOB (000000),'KELLY',
//           NOTIFY=&SYSUID,
//           CLASS=A
//********************************************************************/
//* COMPILE PROGRAM
//********************************************************************/
//COBRUN  EXEC IGYWCL,PARM.COBOL='RENT,LIST'
//COBOL.SYSIN  DD DISP=SHR,DSN=&SYSUID..GET433.COBOL(SU41PGM8)
//COBOL.SYSLIB DD DISP=SHR,DSN=SHARE.GET433.COPYLIB
//LKED.SYSLMOD DD DISP=SHR,DSN=&SYSUID..GET433.LOADLIB(SU41PGM8)
//********************************************************************/
//* END COMPILE PROGRAM
//********************************************************************/