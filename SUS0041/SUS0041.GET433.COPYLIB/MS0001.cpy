      * ***************************************************************
      * Created: Apr 24, 2016 5:14:20 PM America/New_York
      * Generated by: IBM Rational Developer for System z
      * ***************************************************************
       01 MENUI.
          02 FILLER                    PIC X(12).
      *
          02 MOPTL                     PIC S9(4) COMP.
          02 MOPTF                     PIC X.
          02 FILLER          REDEFINES MOPTF.
             03 MOPTA                  PIC X.
          02 FILLER                    PIC X(2).
          02 MOPTI                     PIC 9(1).
      *
          02 MMSGL                     PIC S9(4) COMP.
          02 MMSGF                     PIC X.
          02 FILLER          REDEFINES MMSGF.
             03 MMSGA                  PIC X.
          02 FILLER                    PIC X(2).
          02 MMSGI                     PIC X(79).
      * *******************************************
       01 MENUO REDEFINES MENUI.
          02 FILLER                    PIC X(12).
      *
          02 FILLER                    PIC X(3).
          02 MOPTC                     PIC X.
          02 MOPTH                     PIC X.
          02 MOPTO                     PIC 9(1).
      *
          02 FILLER                    PIC X(3).
          02 MMSGC                     PIC X.
          02 MMSGH                     PIC X.
          02 MMSGO                     PIC X(79).
       01 DETAILSI REDEFINES MENUI.
          02 FILLER                    PIC X(12).
      *
          02 HEADL                     PIC S9(4) COMP.
          02 HEADF                     PIC X.
          02 FILLER          REDEFINES HEADF.
             03 HEADA                  PIC X.
          02 FILLER                    PIC X(2).
          02 HEADI                     PIC X(20).
      *
          02 ACCOUNTL                  PIC S9(4) COMP.
          02 ACCOUNTF                  PIC X.
          02 FILLER          REDEFINES ACCOUNTF.
             03 ACCOUNTA               PIC X.
          02 FILLER                    PIC X(2).
          02 ACCOUNTI                  PIC X(8).
      *
          02 BALANCEL                  PIC S9(4) COMP.
          02 BALANCEF                  PIC X.
          02 FILLER          REDEFINES BALANCEF.
             03 BALANCEA               PIC X.
          02 FILLER                    PIC X(2).
          02 BALANCEI                  PIC X(13).
      *
          02 LIMITL                    PIC S9(4) COMP.
          02 LIMITF                    PIC X.
          02 FILLER          REDEFINES LIMITF.
             03 LIMITA                 PIC X.
          02 FILLER                    PIC X(2).
          02 LIMITI                    PIC X(13).
      *
          02 SURNL                     PIC S9(4) COMP.
          02 SURNF                     PIC X.
          02 FILLER          REDEFINES SURNF.
             03 SURNA                  PIC X.
          02 FILLER                    PIC X(2).
          02 SURNI                     PIC X(20).
      *
          02 FIRSTNL                   PIC S9(4) COMP.
          02 FIRSTNF                   PIC X.
          02 FILLER          REDEFINES FIRSTNF.
             03 FIRSTNA                PIC X.
          02 FILLER                    PIC X(2).
          02 FIRSTNI                   PIC X(15).
      *
          02 ADDR1L                    PIC S9(4) COMP.
          02 ADDR1F                    PIC X.
          02 FILLER          REDEFINES ADDR1F.
             03 ADDR1A                 PIC X.
          02 FILLER                    PIC X(2).
          02 ADDR1I                    PIC X(25).
      *
          02 ADDR2L                    PIC S9(4) COMP.
          02 ADDR2F                    PIC X.
          02 FILLER          REDEFINES ADDR2F.
             03 ADDR2A                 PIC X.
          02 FILLER                    PIC X(2).
          02 ADDR2I                    PIC X(20).
      *
          02 ADDR3L                    PIC S9(4) COMP.
          02 ADDR3F                    PIC X.
          02 FILLER          REDEFINES ADDR3F.
             03 ADDR3A                 PIC X.
          02 FILLER                    PIC X(2).
          02 ADDR3I                    PIC X(15).
      *
          02 ACCTCOML                  PIC S9(4) COMP.
          02 ACCTCOMF                  PIC X.
          02 FILLER          REDEFINES ACCTCOMF.
             03 ACCTCOMA               PIC X.
          02 FILLER                    PIC X(2).
          02 ACCTCOMI                  PIC X(50).
      *
          02 DMSGL                     PIC S9(4) COMP.
          02 DMSGF                     PIC X.
          02 FILLER          REDEFINES DMSGF.
             03 DMSGA                  PIC X.
          02 FILLER                    PIC X(2).
          02 DMSGI                     PIC X(79).
      * *******************************************
       01 DETAILSO REDEFINES MENUI.
          02 FILLER                    PIC X(12).
      *
          02 FILLER                    PIC X(3).
          02 HEADC                     PIC X.
          02 HEADH                     PIC X.
          02 HEADO                     PIC X(20).
      *
          02 FILLER                    PIC X(3).
          02 ACCOUNTC                  PIC X.
          02 ACCOUNTH                  PIC X.
          02 ACCOUNTO                  PIC X(8).
      *
          02 FILLER                    PIC X(3).
          02 BALANCEC                  PIC X.
          02 BALANCEH                  PIC X.
          02 BALANCEO                  PIC $$,$$$,$$9.99.
      *
          02 FILLER                    PIC X(3).
          02 LIMITC                    PIC X.
          02 LIMITH                    PIC X.
          02 LIMITO                    PIC $$,$$$,$$9.99.
      *
          02 FILLER                    PIC X(3).
          02 SURNC                     PIC X.
          02 SURNH                     PIC X.
          02 SURNO                     PIC X(20).
      *
          02 FILLER                    PIC X(3).
          02 FIRSTNC                   PIC X.
          02 FIRSTNH                   PIC X.
          02 FIRSTNO                   PIC X(15).
      *
          02 FILLER                    PIC X(3).
          02 ADDR1C                    PIC X.
          02 ADDR1H                    PIC X.
          02 ADDR1O                    PIC X(25).
      *
          02 FILLER                    PIC X(3).
          02 ADDR2C                    PIC X.
          02 ADDR2H                    PIC X.
          02 ADDR2O                    PIC X(20).
      *
          02 FILLER                    PIC X(3).
          02 ADDR3C                    PIC X.
          02 ADDR3H                    PIC X.
          02 ADDR3O                    PIC X(15).
      *
          02 FILLER                    PIC X(3).
          02 ACCTCOMC                  PIC X.
          02 ACCTCOMH                  PIC X.
          02 ACCTCOMO                  PIC X(50).
      *
          02 FILLER                    PIC X(3).
          02 DMSGC                     PIC X.
          02 DMSGH                     PIC X.
          02 DMSGO                     PIC X(79).