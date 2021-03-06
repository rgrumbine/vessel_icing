      SUBROUTINE GRBICE(IFT,F,IDX,IRC,IM,JM,MXBIT,ILPDS,IYR,IMO,IDA,IHR,
     1                  IYC,IMJM,MGRIB,GRIB,LGRIB,IGRIB,JGRIB,KGRIB,
     2                  IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GRBICE      CREATE GRIB MESSAGE
C   PRGMMR: BHAVANI.B          ORG: W/NMC23    DATE: 92-10-31
C
C ABSTRACT: CREATE A GRIB MESSAGE FROM A FULL FIELD.
C   AT PRESENT, ONLY GLOBAL LATLON GRIDS AND GAUSSIAN GRIDS
C   AND REGIONAL POLAR PROJECTIONS ARE ALLOWED.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   94-05-04  JUANG (FOR GSM AND RSM USE)
C   95-06-22  BHAVANI
C   96-02-15  BURROUGHS - GRIB ENCODING OF NH FOG AND VSBY
C   96-06-20  BURROUGHS - GRIB ENCODING OF NH AND AK ICE ACCRETION
C
C USAGE:    CALL GRBICE(IFT,F,IDX,IM,JM,MXBIT,ILPDS,IYR,IMO,IDA,IHR,
C    &                  IMJM,IGRIB,GRIB,LGRIB,IERR)
C
C   INPUT ARGUMENT LIST:
C     IFT      - FILE NAME FOR GRIB OUTPUT FILE
C     F        - REAL (IM*JM) FIELD DATA TO PACK INTO GRIB MESSAGE
C     IDX      - INDEX OF INITIALIZATION/FORECAST 1 - 7
C     IM       - INTEGER LONGITUDINAL DIMENSION
C     JM       - INTEGER LATITUDINAL DIMENSION
C     MXBIT    - INTEGER MAXIMUM NUMBER OF BITS TO USE (0 FOR NO LIMIT)
C     ILPDS    - INTEGER LENGTH OF THE PDS (USUALLY 28)
C     IYR      - INTEGER YEAR
C     IMO      - INTEGER MONTH
C     IDA      - INTEGER DAY
C     IHR      - INTEGER HOUR
C     MGRIB    - MAXIMUM LENGTH OF GRIB MESSAGE ALLOWED
C
C   OUTPUT ARGUMENT LIST:
C     GRIB     - CHARACTER (LGRIB) GRIB MESSAGE
C     LGRIB    - INTEGER LENGTH OF GRIB MESSAGE
C                (NO MORE THAN 100+ILPDS+IM*JM*(MXBIT+1)/8)
C     IERR     - INTEGER ERROR CODE (0 FOR SUCCESS)
C
C   INPUT FILES: NONE
C
C   OUTPUT FILES:
C     fort.06  - FILE FOR PRINTOUT
C     fort.52  - GRIBBED NH 1X1 DEG ICE ACCRETION DATA-ALL HOURS
C
C SUBPROGRAMS CALLED:
C   UNIQUE:    - GICBTS
C
C   LIBRARY:
C     W3LIB    - W3FI72 ISRCHNE
C
C     IMSL     - NONE
C
C     SPECIAL  - MAX    MIN    LOG    NINT   CHAR   MOD
C
C ATTRIBUTES:
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C
C$$$
      SAVE
      PARAMETER (IGDS=18,IDS=25,IBDS=9,ITP=0,IPF=1,ICP=0,IBF=0,
     1           IN=57,IFT06=06)
C
      CHARACTER*4 GRIB(MGRIB)
      CHARACTER*1 KPDS(ILPDS)
C
      INTEGER*4 KGDS(IGDS),KBDS(IBDS),ID(IDS),JPDS(IN)
      INTEGER*4 IBM(IMJM),IFLD(IMJM)
C
      REAL*4 F(IMJM),FR(IMJM)
C
      DATA IBMS/0/,JPDS/0,03,06,09,12,15,18,21,24,27,30,33,36,39,42,45,
     1                 48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,
     2                 96,99,102,105,108,111,114,117,120,123,126,129,
     3                132,135,138,141,144,147,150,153,156,159,162,165,
     4                168/
C
C          DETERMINE GRID PARAMETERS
C
      DO I=1,IMJM
        IFLD(I)=0
      ENDDO

      WRITE(06,5900)IYC
 5900 FORMAT(1X,I5/)
C
C          FILL PARAMETERS FOR NH 1X1 DEG GRID
C
      IF(IFT.EQ.52)THEN
C
C          FILL PDS PARAMETERS
C
         KPDS(1)  = CHAR(0)
         KPDS(2)  = CHAR(0)
         KPDS(3)  = CHAR(28)
         KPDS(4)  = CHAR(2)
         KPDS(5)  = CHAR(7)
         KPDS(6)  = CHAR(96)
         KPDS(7)  = CHAR(3)
         KPDS(8)  = CHAR(0)
         KPDS(9)  = CHAR(097)
         KPDS(10) = CHAR(103)
         KPDS(11) = CHAR(0)
         KPDS(12) = CHAR(10)
         IF(IYC.EQ.2000)THEN
            IYR=100
         ENDIF
         KPDS(13) = CHAR(IYR)
         KPDS(14) = CHAR(IMO)
         KPDS(15) = CHAR(IDA)
         KPDS(16) = CHAR(IHR)
         KPDS(17) = CHAR(0)
         KPDS(18) = CHAR(1)
         KPDS(19) = CHAR(JPDS(IDX))
         KPDS(20) = CHAR(0)
         IF(IDX.EQ.1)THEN
            KPDS(21) = CHAR(1)
         ELSE
            KPDS(21) = CHAR(0)
         END IF
         KPDS(22) = CHAR(0)
         KPDS(23) = CHAR(0)
         KPDS(24) = CHAR(0)
         IF(IYC.GT.2000)THEN
            KPDS(25) = CHAR(21)
         ELSE
            KPDS(25) = CHAR(20)
         END IF
         KPDS(26) = CHAR(4)
         ISCALE=08
         KPDS(27) = CHAR(ISCALE/256)
         KPDS(28) = CHAR(MOD(ISCALE,256))
         IGRID=3
         IGF=0
      END IF
C
C          FILL BDS PARAMETERS
C
      DO I = 1,IBDS
         KBDS(I)=0
      END DO
      DO I=1,IDS
         ID(I) = 0
      END DO
C
C          FILL BITMAP AND COUNT VALID DATA.  RESET BITMAP FLAG IF ALL V
C
      NBM=IMJM
      IF(IBMS.NE.0) THEN
         NBM=0
         DO I=1,IMJM
            IF(F(I).LE.-9999.) THEN
               IBM(I)=1
               NBM=NBM+1
            ELSE
               IBM(I)=0
            END IF
         END DO
      END IF
C
C          ROUND DATA AND DETERMINE NUMBER OF BITS
C
      IF(NBM.EQ.0) THEN
         DO I=1,IMJM
            FR(I)=0.
         END DO
         NBIT=0
      ELSE
         CALL GTBITS(IBMS,ISCALE,IMJM,IBM,F,FR,FMIN,FMAX,NBIT)
         IF(MXBIT.GT.0) NBIT=MIN(NBIT,MXBIT)
      END IF
C
C          CREATE GRIB MESSAGE
CXXXXNB----------------------------------------------------------------E
      CALL W3FI72(ITP,FR,IFLD,NBIT,IPF,ID,KPDS,IGF,IGRID,KGDS,ICP,IBF,
     1            IBM,IMJM,KBDS,NFO,GRIB,LGRIB,IERR)
      IF(IFT.EQ.52)THEN
         CALL BAWRITE(52,JGRIB,LGRIB,JA,GRIB)
         JGRIB=JGRIB+JA
         PRINT *,'IERR2ND',IERR,LGRIB,JA,JGRIB
      ENDIF
C
      RETURN
      END
