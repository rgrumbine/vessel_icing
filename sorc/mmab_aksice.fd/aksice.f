      PROGRAM AKSICE
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                                                                       
C MAIN PROGRAM: AKSICE
C   PRGMMR: BURROUGHS        ORG: W/NP21      DATE: 2002-08-09
C                                                                       
C ABSTRACT: THIS PROGRAM COMPUTES THE RATE OF ICE ACCRETION ON         
C   SHIP SUPERSTRUCTURES IN N HEMIS WATERS. THE COMPUTATION IS         
C   BASED ON ANALYZED SEA SURFACE TEMPERATURE FIELDS AND              
C   ANALYZED AND FORECAST 10 M WIND AND 2 M TEMPERATURE FIELDS
C   FROM THE 00 AND 12 UTC AVIATION RUNS OF THE GLOBAL SPECTRAl
C   MODEL.
C                                                                  
C PROGRAM HISTORY LOG:                                            
C   87-02-02  (ORGINAL AUTHOR)  FEIT                             
C   87-06-16  R.E.JONES      CONVERT FORTXDAM-W3FT19 TO VSAM-W3FT59     
C                            CONVERT FORTXDAM-IW3GET TO VSAM-IW3VGE     
C                            CONVERT FORTXDAM-IW3PUT TO VSAM-IW3VPU     
C                            PUT FILE NAME IN O.N. 85 LABEL             
C   96-06-16  L.D.BURROUGHS  CONVERT FOR USE ON CRAY3
C   98-05-22  L.D.BURROUGHS  Convert for y2k and f90 compliance
C   02-08-09  L.D.BURROUGHS  Extend to 7 days and 4 cycles at 3-h intervals
C                            and year round operation.
C   03-01-10  L.D.BURROUGHS  Corrected an error in a counter which skipped
C                            outputting the 15 and 18-h progs.
C   03-10-02  L.D.BURROUGHS  Change program to make alaska sice only
C                                                                   
C USAGE:                                                                
C   INPUT FILES:                                                        
C      fort.11 - GRIBBED GBL 1X1 DEG ICE ACCRETION DATA AT 3-H INTERVALS OUT
C                TO 168 HOURS
C      fort.12 - GRIB INDEX FILE
C      fort.90 - DATE FILE                                              
C      fort.91 - CYCLE TIME FILE
C                                                                       
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C      fort.06 - PRINTOUT                                               
C      fort.53 - GRIBBED NH AK RGN MERCATOR ICE ACCRETION DATA AT 12-H
C                INTERVALS OUT TO 72-H FOR AKFAX - IF IT STILL EXISTS
C
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - FIND   FINDNH INTLZI ACRE  WRITEI BLINT 
C                  BILINT GRBICE GICBTS
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - W3LOG  GETGB  W3FI72 ISRCHNE BAOPENR BAOPENW BAWRITE
C                  W3TAGB W3TAGE
C                                                                       
C       IMSL     - NONE
C                                                                       
C       SPECIAL  - NONE
C                                                                       
C   EXIT STATES:                                                        
C     COND =   0 - SUCCESSFUL RUN                                       
C     COND =   9 - ERROR: ERROR RUNNING IW3VGE OR IW3PU                 
C                                                                       
C REMARKS: IF ANYTHING GOES WRONG, A DIAGNOSTIC MESSAGE IS WRITTEN TO
C   fort.06 AND THE OUTPUT FIELDS ARE GIVEN VALUES OF -9999 AT EVERY
C   GRID POINT AND SENT TO THE OUTPUT FILE IN GRIB.  THIS PROCEDURE
C   IS FOLLOWED FOR THE WARM SEASON ALSO.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C
C$$$
      SAVE
      PARAMETER (IFT06=06,IMA=71,JMA=34)
      DIMENSION AKSSI(IMA,JMA)
      CALL W3TAGB('AKSICE',2003,0275,0023,'W/NP21') 
      OPEN(90,ACCESS='SEQUENTIAL',FORM='FORMATTED',RECL=8)
      OPEN(91,ACCESS='SEQUENTIAL',FORM='FORMATTED',RECL=2)
C                                                                       
      WRITE(IFT06,1000)
 1000 FORMAT(1H ,'START MAIN PROGRAM')
C                                                                       
      CALL FIND(IFT06,AKSSI,IMA,JMA,IERR,KERR)
 1090 IF(IERR.EQ.9) THEN
         ICODE=9
         WRITE(IFT06,1100)IERR,ICODE
 1100    FORMAT(1H0,'JOB ENDED ABNORMALLY. ERROR IN WRITEF',2I3)
         CALL W3TAGE('AKSICE') 
         CLOSE(90)
         CLOSE(91)
         CLOSE(53)
         STOP 9
      ELSE IF(KERR.EQ.9) THEN
         ICODE=8
         WRITE(IFT06,1105)KERR,ICODE
 1105    FORMAT(1H0,'JOB ENDED ABNORMALLY. ERROR IN GETGB',2I3)
         CALL W3TAGE('AKSICE') 
         CLOSE(90)
         CLOSE(91)
         CLOSE(53)
         STOP 8
      ELSE
         ICODE=0
         WRITE(IFT06,1120)IERR,KERR,ICODE
 1120    FORMAT(1H0,'JOB ENDED NORMALLY',3I3)
         CALL W3TAGE('AKSICE') 
         CLOSE(90)
         CLOSE(91)
         CLOSE(53)
         STOP
      END IF
      END
C                                                                               
      SUBROUTINE FIND(IFT06,AKSSI,IMA,JMA,IERR,KERR)
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    FIND        FIND AVN AND ANL FIELDS                    
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 88-07-29             
C                                                                       
C ABSTRACT: THIS SUBROUTINE FINDS ALL THE INPUT FILES USED TO COMPUTE   
C   SUPERSTRUCTURE ICE ACCRETION ON SHIPS AT SEA IN THE NORTHERN
C   HEMISPHERE AND ALASKA REGION.  IT CALLS THE SUBROUTINES NECESSARY
C   TO MAKE THE VARIOUS FORECAST COMPUTATIONS AND INITIALIZE THE
C   OUTPUT FIELDS.  IT ALSO CALLS THE WRITE ROUTINE.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   88-07-29  L. D. BURROUGHS                                           
C   88-08-31  L. D. BURROUGHS - CHANGED ERROR STOP CODE                 
C   90-03-21  L. D. BURROUGHS - ADDED PROVISION FOR A BACKUP SST        
C   96-02-15  L. D. BURROUGHS - CHANGED TO READ GRIB FILES ON CRAY
C   96-06-17  L. D. BURROUGHS - CHANGED TO BE USED WITH ICE ACCRE PGM
C                                                                       
C USAGE:    CALL FIND(IFT06,IERR,KERR)
C   INPUT ARGUMENT LIST:                                                
C     NONE                                                              
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     IERR     - ERROR CODE IN WRITEI                                   
C                0 - GOOD GRIB WRITES
C                9 - ERROR IN GRIB WRITES
C     KERR     - ERROR CODE - I*4                                       
C                0 - NO FIELDS MISSING.                                 
C                9 - FIELDS MISSING.                                    
C                                                                       
C   INPUT FILES:                                                        
C      see doc block for main
C                                                                       
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C      fort.06 - PRINTOUT                                               
C      fort.53 - GRIBBED NH AK RGN MERCATOR ICE ACCRETION DATA AT 12-H INTV
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - FINDNH INTLZI ACRE   WRITEI
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - NONE                                                 
C                                                                       
C       IMSL     - NONE                                                 
C                                                                       
C       SPECIAL  - NONE                                                 
C                                                                       
C REMARKS: IF ANYTHING GOES WRONG, A DIAGNOSTIC MESSAGE IS WRITTEN TO
C   fort.06 AND THE OUTPUT FIELDS ARE GIVEN VALUES OF -9999. AT EVERY
C   GRID POINT AND SENT TO THE OUTPUT FILE IN GRIB FORMAT.
C                                                                       
C     VARIABLES:                                                        
C                                                                       
C       INPUT    - IDATE      = DATE WORD: YYMMDDHH - I*4
C                  IFT06      = FILE NAME FOR PRINT OUT
C                  AKSSI      = ALASKA MERCATOR MATRIX
C                  IMA        = I INDEX 71
C                  JMA        = J INDEX 34
C                                                                       
C       INTERNAL - IFCST      = FCST TAU AT 12-H INTRVLS FM 00-72 - I*4
C                  CMH2MS     = CONVERSION FACTOR CONVERTING ICE ACCRE
C                               FROM CM/HR TO M/S
C                  IFT90      = FILE NAME FOR DTG
C                  IFT91      = FILE NAME FOR CYCLE TIME
C                  SSI( , )   = ICE ACCRETION MATRIX IN CM/HR
C                  IONA       = 0 - IF FIELD IS FOUND BY FINDNH
C                               9 - IF FIELD IS NOT FOUND OR SOME OTHER
C                                   ERROR OCCURS - I*4
C                  LUGB       = GRIB FILE NAME
C                  LUGI       = INDEX FILE NAME
C                                                                       
C       OUTPUT   - IY         = YEAR - 1900 - I*4                       
C                  IM         = MONTH - I*4                             
C                  ID         = DAY - I*4                               
C                  ICY        = CYCLE (0000/1200 UTC) - I*4             
C                  SSI1X1( , )= ICE ACCRETION MATRIX IN M/S
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C                                                                       
C$$$
      SAVE
      PARAMETER (IFT90=90,IFT91=91)
      REAL*4 SSI(361,91)
      REAL*4 AKSSI(IMA,JMA)
      INTEGER*4 IDATE,ICY,IYC,IY,IM,ID
      CHARACTER*80 FILEB,FILEI,FILEA
      CHARACTER*11 ENVVAR
C                                                                       
      COMMON/FDT/IDATE,ICY,IYC,IY,IM,ID
C
      READ(IFT90,1000)IYC,IM,ID
 1000 FORMAT(I4,I2,I2)
      IYY=IYC/100
      IY=IYC-IYY*100
      READ(IFT91,1005)ICY
 1005 FORMAT(I2)
      WRITE(06,4900)IYC,IM,ID,ICY
 4900 FORMAT(/1X,I4,3I3)
      IDATE=1000000*IY+10000*IM+100*ID+ICY
C                                                                       
      NERR=0
      KERR=0
C                                                                       
C         COMPUTE SUPERSTRUCTURE ICE ACCRETION
C
      LUGA=53
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)')LUGA
      CALL GETENV(ENVVAR,FILEA)
      CALL BAOPENW(LUGA,FILEA,IRET3)
      IF((IRET3).NE.0)THEN
         WRITE(IFT06,FMT='("Error opening GRIB data file for Alaska."
     1)')
      ENDIF
      IGRIB=0
      JGRIB=0
      KGRIB=0
 2002 DO 1250 KK=1,57
C
         IFCST=(KK-1)*3
         IF(IFCST.EQ.0.OR.IFCST.EQ.12.OR.IFCST.EQ.24.OR.IFCST.EQ.36.OR.
     1      IFCST.EQ.48.OR.IFCST.EQ.60.OR.IFCST.EQ.72)THEN
            IF(IFCST.EQ. 0)LL=1
            IF(IFCST.EQ.12)LL=2
            IF(IFCST.EQ.24)LL=3
            IF(IFCST.EQ.36)LL=4
            IF(IFCST.EQ.48)LL=5
            IF(IFCST.EQ.60)LL=6
            IF(IFCST.EQ.72)LL=7
C                                                                       
C          FIND VESSEL ICING FIELDS
C                                                                       
            CALL FINDNH(IFT06,1,01,IFCST,SSI,IONA)
            IF(IONA.NE.0)NERR=9
            IF(NERR.GT.0)GO TO 1030
C
            WRITE(IFT06,1020)
 1020       FORMAT(1H ,' FIELDS HAVE BEEN PROPERLY DECODED.')
            GO TO 1050
C
 1030       WRITE(IFT06,1040)
 1040       FORMAT(1H ,' FIELDS ARE MISSING. 0. WRITTEN')
            CALL INTLZI(AKSSI,IMA,JMA)
            KERR=NERR
            CALL WRITEI(IFT06,SSI,AKSSI,IMA,JMA,LL,IGRIB,JGRIB,KGRIB,
     1                  IERR)
            GO TO 1240
C
C          WRITE ALASKA FAX ICE ACCRETION FORECASTS
C
 1050       CALL WRITEI(IFT06,SSI,AKSSI,IMA,JMA,LL,IGRIB,JGRIB,KGRIB,
     1                  IERR)
 1240       NERR=0
            CALL INTLZI(AKSSI,IMA,JMA)
         ENDIF
         CYCLE
 1250 CONTINUE
      RETURN
      END
C                                                                               
C                                                                       
      SUBROUTINE FINDNH(IFT06,NOFLD,IDENT,IFCST,XFLD,INO)
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    FINDNH      FIND AVN AND ANL FIELDS                    
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 88-07-29             
C                                                                       
C ABSTRACT: GIVEN THE FIELD NUMBER, THIS SUBROUTINE WILL RETURN THE     
C   DESIRED 361X91 NORTHERN HEMISPHERIC AVIATION FORECAST OR ANALYSIS
C   FIELD.                                                              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   88-07-29  L. D. BURROUGHS                                           
C   88-08-31  L. D. BURROUGHS - CHANGED HOW DATE WORD IS CREATED        
C   90-03-21  L. D. BURROUGHS - CHANGED TO INCLUDE BACKUP FOR SST FIELD 
C   96-02-15  L. D. BURROUGHS - CHANGED TO READ GRIB ON CRAY
C   96-06-18  L. D. BURROUGHS - CHANGED TO READ GRIB ON CRAY FOR IC AC
C                                                                       
C USAGE:    CALL FINDNH(NOFLD,IDENT,IFCST,XFLD,INO)
C   INPUT ARGUMENT LIST:                                                
C     NOFLD    - NUMBER OF FIELD TO BE RETRIEVED.  NUMBERS RANGE FROM   
C                01  (I*4)  CODE IS GIVEN BELOW:                  
C                01 = 1 X 1 GBL ICE ACCRETION
C     IFCST    - THE FORECAST TAU AT 12-H INTERVALS FROM 00 - 72 HOURS  
C                (I*4)                                                  
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     XFLD(,)  - OUTPUT MATRIX FOR GIVEN FIELD RETRIEVED (R*4)          
C     INO      - ERROR CODE = 0 FOR SUCCESSFUL RETRIEVE                 
C                           = 9 FOR UNSUCCESSFUL RETRIEVE               
C                                                                       
C   INPUT FILES:                                                        
C      SEE MAIN DOC BLOCK FOR SPECIFIC INFORMATION ABOUT INPUT FILES
C                                                                       
C   OUTPUT FILES:                                                       
C      FORT.06 - PRINTOUT
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - NONE
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - GETGB                                                
C                                                                       
C       IMSL     - NONE                                                 
C                                                                       
C       SPECIAL  - NONE
C                                                                       
C REMARKS: IF ANYTHING GOES WRONG, A DIAGNOSTIC MESSAGE IS WRITTEN TO
C   fort.06 AND THE OUTPUT FIELDS ARE GIVEN VALUES OF -9999. AT EVERY
C   GRID POINT.
C                                                                       
C     VARIABLES:                                                        
C                                                                       
C       INPUT    - IDENT      = FIRST OF 3 WORDS IN IGRD IDENTIFYING
C                               FIELD
C                  IFT06      = FILE NAME FOR PRINTOUT
C                  NOFLD      = NUMBER OF FIELD TO BE RETRIEVED. NOS
C                               RANGE FROM 01 - 06. CODE GIVEN BELOW
C                               01 = GBL ICE ACCRETION
C                  IFCST      = THE FORECAST TAU AT 12-H INTERVALS
C                               FROM 00 - 72 HOURS
C                                                                       
C       INTERNAL - JF         = DIMENSION OF FLD AND LB
C                  JPDS( )    = INPUT PDS MATRIX
C                  JGDS( )    = INPUT GDS MATRIX
C                  IGRD( )    = FIELD IDENTIFIER MATRIX
C                  IRET       = ERROR CODE FROM GETGB
C                  OUTF( , )  = TEMPORARY WORK SPACE
C                                                                       
C       OUTPUT   - XFLD( , )  = OUTPUT MATRIX FOR GIVEN FIELD RETRIEVED
C                  INO        = ERROR CODE = 0 FOR SUCCESSFUL RETRIEVE
C                                          = 9 FOR UNSUCCESSFUL RETRV
C
C ATTRIBUTES:                                                           
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C                                                                       
C$$$
!grib2      USE GRIB_MOD
      IMPLICIT none
      SAVE
      INTEGER JF, IFT06, NOFLD, IDENT, IFCST, INO
      PARAMETER(JF=360*181)
      INTEGER*4 PDS,GDS,IGRD(3)
      INTEGER*4 JPDS(25),JGDS(22)
      INTEGER*4 KPDS(25),KGDS(22)
      REAL*4 FLD(JF),OUT(360,181),OUTF(361,181),XFLD(361,91)
      LOGICAL LB(JF)
      CHARACTER*80 FILEB,FILEI,FILEA
      CHARACTER*11 ENVVAR
C
      INTEGER ICY, ID, IDATE, IDX
      COMMON/FDT/IDATE,ICY,IYC,IY,IM,ID
C
      EQUIVALENCE(OUT(1,1),FLD(1))
C
      INTEGER IM, IOFLD, IRET, IRET1, IRET2, IY, IYC
      INTEGER J, JB, K, KERR, KF, KK, LUGB, LUGI
      REAL SSI1X1
C For grib2
!      INTEGER jpdtn, jgdtn, jdisc
!      INTEGER jids(200), jpdt(200), jgdt(200)
!      LOGICAL unpack
!      TYPE(GRIBFIELD) :: gfld
C
      DATA IGRD/97,103,  10/
C grib2:
!      unpack = .TRUE.
!      jpdtn = -1
!      jgdtn = -1
!      jdisc = -1
C
      LUGB=11
      LUGI=12
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)')LUGB
      CALL GETENV(ENVVAR,FILEB)
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:11),FMT='(I2)')LUGI
      CALL GETENV(ENVVAR,FILEI)
      CALL BAOPENR(LUGB,FILEB,IRET1)
      CALL BAOPENR(LUGI,FILEI,IRET2)
      IF((IRET1+IRET2).NE.0)THEN
         WRITE(IFT06,FMT='("Error opening GRIB data file or index file."
     1)')
         KERR=9
!         CALL INTLZI(SSI1X1)
         SSI1X1 = 0
         GO TO 1100
      END IF
      INO = 0
      DO 1000 PDS=1,25
         JPDS(PDS)=-1
 1000 CONTINUE
      DO 1010 GDS=1,22
         JGDS(GDS)=-1
 1010 CONTINUE
C                                                                       
      WRITE(06,1020)IFCST
 1020 FORMAT(1H0,'IFCST',I4)
C                                                                       
C          DETERMINE ANALYSIS OR MODEL FIELD ID                         
C
      IDX=(IFCST/3)+1
C     JB=0
      IOFLD=NOFLD
C                                                                       
C          SET IDENT FOR CORRECT FORECAST                               
C          GET SELECT FIELDS
CXXXXNS----------------------------------------------------------------E
      IF(IDX.EQ. 1)JPDS(14)= 0
      IF(IDX.EQ. 5)JPDS(14)=12
      IF(IDX.EQ. 9)JPDS(14)=24
      IF(IDX.EQ.13)JPDS(14)=36
      IF(IDX.EQ.17)JPDS(14)=48
      IF(IDX.EQ.21)JPDS(14)=60
      IF(IDX.EQ.25)JPDS(14)=72
C     JPDS(5)=IGRD(IDENT)
C     JPDS(6)=IGRD(IDENT+1)
C     JPDS(7)=IGRD(IDENT+2)
C     PRINT *,'IDENT, JPDS, IGRD ',IDENT,JPDS(5),IGRD(IDENT),JPDS(6),
C    1        IGRD(IDENT+1),JPDS(7),IGRD(IDENT+2)
      CALL GETGB(LUGB,LUGI,JF,JB,JPDS,JGDS,KF,K,KPDS,KGDS,LB,FLD,IRET)
!      PRINT *,'aksice CALL GETGB2'
!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
!     &                  UNPACK,K,GFLD,IRET)
!      PRINT *,'gb2 iret = ',iret
!      PRINT *,'disc = ',GFLD%DISCIPLINE
!      PRINT *,'vers = ',GFLD%VERSION
!      PRINT *,'dlon = ',GFLD%IGDTMPL(17)
!      PRINT *,'dlat = ',GFLD%IGDTMPL(18)
!      PRINT *,'kcat = ',GFLD%IPDTMPL(1)
!      PRINT *,'varb = ',GFLD%IPDTMPL(2)
!      PRINT *,'type = ',GFLD%IPDTMPL(10)
!      PRINT *,'levl = ',GFLD%IPDTMPL(12)
!      IF (iret .EQ. 0) THEN
!        FLD = GFLD%FLD
!        PRINT *,'field max min ',MAXVAL(fld), MINVAL(fld)
!      ENDIF
      IF(IRET.NE.0)THEN
         INO=9
         WRITE(IFT06,1065)IRET,INO
 1065    FORMAT(1X,' IRET =',I5,' INO =',I2)
         GO TO 1100
      END IF
      WRITE(IFT06,1066)KPDS,KF,KGDS
 1066 FORMAT(2(/,2X,'PDS =',13I7),2(/,2X,' GDS =',11I7))
C
C          FLIP GRID
C
      DO 1070 J=1,181
         DO KK=1,360
            OUTF(KK,181-J+1)=OUT(KK,J)
         ENDDO
C
C          DUPLICATE GREENWICH
C
         OUTF(361,181-J+1)=OUTF(1,181-J+1)
 1070 CONTINUE
C
C          SAVE NORTHERN HEMISPHERE GRID
C
      DO 1080 J=91,181
         DO 1085 K=1,361
            XFLD(K,J-90)=OUTF(K,J)
 1085    CONTINUE
 1080 CONTINUE
C                                                                       
 1100 CLOSE(LUGB)
      CLOSE(LUGI)
      RETURN
      END
      SUBROUTINE INTLZI(AKSSI,IMA,JMA)
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    INTLZI      INITIALIZE OUTPUT FILES                  
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 88-07-29             
C                                                                       
C ABSTRACT: THIS SUBROUTINE INITIALIZES THE OUTPUT FILE.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   88-07-29  L. D. BURROUGHS                                           
C   96-02-15  L. D. BURROUGHS - CHANGED TO INITIALIZE OUTPUT FIELDS
C   96-06-19  L. D. BURROUGHS - CHANGED FOR USE WITH ICE ACCRE PGM
C                                                                       
C USAGE:    CALL INTLZI(SSI)
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   INPUT FILES:   NONE
C
C   OUTPUT FILES:  NONE
C
C   SUBPROGRAMS CALLED:
C
C     UNIQUE:    - NONE
C
C     LIBRARY:
C       W3LIB    - NONE
C                                                                       
C       IMSL     - NONE
C
C       SPECIAL  - NONE
C
C REMARKS:  NONE
C
C     VARIABLES:                                                        
C                                                                       
C       INPUT    - NONE
C
C       INTERNAL - NONE
C
C       OUTPUT   - SSI( , )   = 361X91 NORTHERN HEMISPHERIC GRIDDED ICE
C                               ACCRETION OUTPUT MATRIX - REAL
C
C ATTRIBUTES:
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C
C$$$
      SAVE
      REAL*4 AKSSI(IMA,JMA)
C                                                                       
C        INITIALIZE OUTPUT FIELD
C                                                                       
         DO 1410 J=1,JMA
            DO 1400 I=1,IMA
               AKSSI(I,J)=0.
 1400       CONTINUE
 1410    CONTINUE
C
      RETURN
      END
C
      SUBROUTINE WRITEI(IFT06,SSI,AKSSI,IMA,JMA,IDX,IGRIB,JGRIB,KGRIB,
     1                  IERR)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    WRITEI       WRITES OUTPUT TO DISK AND ARCHIVE         
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 88-07-29             
C                                                                       
C ABSTRACT: WRITES OUTPUT TO DISK AND ARCHIVE IN GRIB FORMAT.           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   88-07-29  L. D. BURROUGHS                                           
C   88-08-31  L. D. BURROUGHS - REMOVED SOME UNNECESSARY CODE           
C   96-02-15  L. D. BURROUGHS - CHANGED TO PUT OUTPUT FIELDS IN GRIB
C   96-06-19  L. D. BURROUGHS - CHANGED FOR USE IN ICE ACCRETION PGM
C                                                                       
C USAGE:    CALL WRITEI(IFT06,SSI1X1,IDX,IERR)
C                                                                       
C   INPUT ARGUMENT LIST:
C     IFT06    - FILE NAME FOR PRINT OUT
C     SSI      - ICE ACCRETION MATRIX FOR 1X1 N HEMISPHERIC GRID
C     IDX      - INDEX PASSED TO WRITE TO DETERMINE WHAT THE FIRST      
C                WORD IN THE ON84 FORMAT ID RECORD IS TO BE - I*4       
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     IERR     - ERROR CODE PASSED FROM WRITE TO DETERMINE WHAT IS      
C                WRITTEN TO THE PRINTER. I*4                            
C                = 9 - FIELDS UNABLE TO BE WRITTEN TO OUTPUT FILE       
C                                                                       
C   INPUT FILES:                                                        
C     fort.06  - PRINT OUT FILE
C                                                                       
C   OUTPUT FILES:                                                       
C     fort.53  - GRIBBED NH AK RGN MERCATOR ICE ACCRETION DATA AT 12-H INTERVALS
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE     - BLINT  BILINT GRBICE
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - NONE                                                 
C                                                                       
C       IMSL     - NONE                                                 
C                                                                       
C       SPECIAL  - NONE                                                 
C                                                                       
C REMARKS: NONE                                                         
C                                                                       
C     VARIABLES:                                                        
C       INPUT    - SSI( , )   = N HEMIS 1X1 ICE ACCRETION MATRIX M/S
C                  IFT06      = FILE NAME FOR PRINT OUT
C                  IDATE      = DATE TIME GROUP (YYMMDDCC): YEAR, MONTH,
C                               DAY AND CYCLE TIME - I*4                
C                                                                       
C       INTERNAL - AKSSI( , ) = AK RGN MERCATOR ICE ACCRETION MATRIX M/S
C                                                                       
C       OUTPUT   - AKIG( )    = AK RGN MERCATOR GRIBBED ICE ACCRETION
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C
C$$$
      SAVE
      PARAMETER (IFT53=53,JA=71*34,IGBA=9656,IPDS=28,IMXBIT=0)
C
      REAL*4 SSI(361,91),AKSSI(IMA,JMA),TEMP(9,9)

      REAL*4 XLAT(34),AKIG(JA),AKSI(71,34)
C
      CHARACTER LNDTST(71,34)*1,ZERO*1,GBAK(IGBA)*4
C
      COMMON/FDT/IDATE,ICY,IYC,IY,IM,ID
C
      EQUIVALENCE (AKIG(1),AKSI(1,1))
C
      DATA XLAT/46.40,47.09,47.76,48.43,49.09,49.74,50.38,51.01,51.64,
     1          52.26,52.86,53.46,54.05,54.64,55.21,55.78,56.34,56.89,
     2          57.43,57.96,58.49,59.01,59.52,60.02,60.52,61.01,61.49,
     3          61.96,62.43,62.89,63.34,63.79,64.22,64.66/
      DATA LNDTST/   67*'0', 4*'1',67*'0', 4*'1',67*'0', 4*'1',
     1 65*'0', 6*'1',65*'0', 6*'1',63*'0', 8*'1',64*'0', 7*'1',
     2 64*'0', 7*'1',60*'0', 1*'1', 2*'0', 8*'1', 3*'0', 1*'1',
     3 16*'0', 1*'1',38*'0', 1*'1', 2*'0', 9*'1',22*'0', 1*'1',
     4 35*'0', 2*'1', 1*'0',10*'1',61*'0',10*'1',27*'0', 2*'1',
     5 30*'0', 1*'1', 1*'0',10*'1',28*'0', 2*'1',28*'0',13*'1',
     6 29*'0', 2*'1',26*'0',14*'1',29*'0', 3*'1',24*'0',15*'1',
     7 31*'0', 3*'1', 2*'0', 1*'1',18*'0',16*'1',32*'0', 2*'1',
     8  1*'0', 2*'1',17*'0',17*'1',33*'0', 3*'1', 1*'0', 2*'1',
     9 15*'0',17*'1',33*'0', 4*'1',16*'0',18*'1',33*'0', 8*'1',
     A 11*'0',19*'1',30*'0', 8*'1', 1*'0', 1*'1',11*'0',20*'1',
     B 23*'0', 1*'1', 1*'0',12*'1', 1*'0', 3*'1', 5*'0',25*'1',
     C  1*'1',23*'0',14*'1', 1*'0', 3*'1', 2*'0',27*'1', 2*'1',
     D 23*'0',14*'1', 1*'0',31*'1', 3*'1',22*'0',14*'1', 1*'0',
     E 31*'1', 5*'1',19*'0',47*'1', 6*'1',18*'0',47*'1', 8*'1',
     F  1*'0', 1*'1',15*'0',46*'1',11*'1',14*'0',46*'1',10*'1',
     G  8*'0', 3*'1', 7*'0',43*'1',10*'1', 8*'0', 1*'1',10*'0',
     H 42*'1',10*'1',19*'0',42*'1', 9*'1', 8*'0', 1*'1',11*'0',
     I 42*'1'/
      data ZERO/'0'/
C                                                                       
C          CONVERT 1X1 ICE ACCRETION TO MECATOR ICE ACCRETION
C 
      IERR=0
      XLAT1=0.
      XLON1=0.
      DO J=51,59
         DO I=181,189
            TEMP(I-180,J-50)=425196.85*SSI(I,J)
         ENDDO
      ENDDO
C                                                                       
C          CONVERT 1.0X1.0 ICE ACCRETION TO AK RGN ICE ACCRETION
C                                                                       
      DO J=1,34
         DO I=1,71
            X = I + 170.
            Y = 1. + XLAT(J)
            CALL BILINT(X,Y,SSI,AKSSI(I,J))
         END DO
      END DO
      DO J=1,JMA
         DO I=1,IMA
            IF(LNDTST(I,J).EQ.'1')THEN
               AKSI(I,J)=0.
            ELSE
               IF(AKSSI(I,J).LE.0.)AKSSI(I,J)=0.
               AKSI(I,J)=AKSSI(I,J)
            ENDIF
         ENDDO
      ENDDO
C                                                                       
      WRITE(06,5000)IY,IM,ID,ICY,IYC
 5000 FORMAT(/1X,5I5/)
C                                                                       
C          GRIB AK RGN MERCATOR ICE ACCRETION
C
      IRC=IDX
      CALL GRBICE(IFT53,AKIG,IDX,IRC,IMA,JMA,IMXBIT,IPDS,IY,IM,ID,
     1            ICY,IYC,JA,IGBA,GBAK,LGRIB,IGRIB,JGRIB,KGRIB,KERR)
      WRITE(IFT06,1500)KERR,LGRIB,KGRIB,IDX
 1500 FORMAT(1H ,4I8/)
C
 1005 WRITE(06,1010)IDATE,IERR,JERR,KERR,IDX
 1010 FORMAT(1H0,5I8/)
      RETURN
      END
C
      SUBROUTINE BILINT(X,Y,SSST,SSTI)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C SUBPROGRAM:    BILINT       BILINEAR INTERPOLATION 2.5X2.5 TO MERCATOR
C   PRGMMR: D. M. FEIT       ORG: W/NMC21    DATE: 87-06-26             
C                                                                       
C ABSTRACT: BILINEAR INTERPOLATION ROUTINE FOR 2.5X2.5 TO MERCATOR PROJ
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   87-06-26  D. M. FEIT
C   96-07-23  L. D. BURROUGHS - CONVERTED FOR USE ON CRAY
C                                                                       
C USAGE:    CALL BILINT(X,Y,SSST,SSTI)
C                                                                       
C   INPUT ARGUMENT LIST:
C     X        - LATITUDE: XX.X
C     Y        - LONGITUDE: XXX.X
C     SSST( , )- RATE OF ICE ACCRETION IN METERS/SECOND
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     SSTI     - INTERPOLATED VALUE OF ICE ACCRETION IN M/S
C                                                                       
C   INPUT FILES: NONE
C                                                                       
C   OUTPUT FILES: NONE
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE     - NONE
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - NONE                                                 
C                                                                       
C       IMSL     - NONE                                                 
C                                                                       
C       SPECIAL  - NONE                                                 
C                                                                       
C REMARKS: NONE                                                         
C                                                                       
C     VARIABLES:                                                        
C       INPUT    - X          = LATITUDE: XX.X
C                  Y          = LONGITUDE: XXX.X
C                  SSST( , )  = RATE OF ICE ACCRETION IN M/S
C                                                                       
C       INTERNAL - I          = ROW ASSOCIATED WITH X
C                  J          = COLUMN ASSOCIATED WITH Y
C                  XI         = FLOATING POINT VERSION OF I
C                  XJ         = FLOATING POINT VERSION OF J
C                  S          = VALUE OF SSST AT GRID POINT I,J
C                  SIP        = VALUE OF SSST AT GRID POINT I+1,J
C                  SJP        = VALUE OF SSST AT GRID POINT I,J+1
C                  SIPJP      = VALUE OF SSST AT GRID POINT I+1,J+1
C                  A          = S
C                  B          = SIP-S
C                  C          = SJP-S
C                  D          = S+SIPJP-SIP-SJP
C                                                                       
C       OUTPUT   - SSTI       = INTERPOLATED VALUE OF ICE ACCRETION M/S
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C                                                                       
C$$$
      SAVE
      DIMENSION SSST(361,91)
C
      ID = 361
      JD = 91
      I = X
      J = Y
      YJ = J
      XI = I
C
      SSTI= 0.
      IF(I.LT.1.OR.I.GE.ID.OR.J.LT.1.OR.J.GE.JD) GO TO 10
C
      S =  SSST(I,J)
      SIP = SSST(I+1,J)
      SJP = SSST(I,J+1)
      SIPJP = SSST(I+1,J+1)
C     IF(S.LE.SSTI.OR.SIP.LE.SSTI.OR.SJP.LE.SSTI.OR.SIPJP.LE.SSTI)
C    1   GO TO 10
      IF(S.LE.SSTI)S=SSTI
      IF(SIP.LE.SSTI)SIP=SSTI
      IF(SJP.LE.SSTI)SJP=SSTI
      IF(SIPJP.LE.SSTI)SIPJP=SSTI
      A = S
      B = SIP - S
      C = SJP - S
      D = S + SIPJP - SIP - SJP
      SSTI=A + B*(X-XI) + C*(Y-YJ) +D*(X-XI)*(Y-YJ)
C
   10 CONTINUE
      RETURN
      END
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
C     fort.53  - GRIBBED NH AK RGN MERCATOR ICE ACCRETION DATA-@12-H INT TO 72-H
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
     1           IN=7,IFT06=06)
C
      CHARACTER*4 GRIB(MGRIB)
      CHARACTER*1 KPDS(ILPDS)
C
      INTEGER*4 KGDS(IGDS),KBDS(IBDS),ID(IDS),JPDS(IN)
      INTEGER*4 IBM(IMJM),IFLD(IMJM)
C
      REAL*4 F(IMJM),FR(IMJM)
C
      DATA IBMS/0/,JPDS/0,12,24,36,48,60,72/
C
C          DETERMINE GRID PARAMETERS
C
 1600 DO 4 I=1,IMJM
      IFLD(I)=0
  4   CONTINUE
      WRITE(06,5900)IYC
 5900 FORMAT(1X,I5/)
C
C          FILL PARAMETERS FOR AK RGN MERCATOR GRID
C
      IF(IFT.EQ.53)THEN
C
C          FILL PDS PARAMETERS
C
         KPDS(1)  = CHAR(0)
         KPDS(2)  = CHAR(0)
         KPDS(3)  = CHAR(28)
         KPDS(4)  = CHAR(2)
         KPDS(5)  = CHAR(7)
         KPDS(6)  = CHAR(77)
         KPDS(7)  = CHAR(255)
         KPDS(8)  = CHAR(128)
         KPDS(9)  = CHAR(097)
         KPDS(10) = CHAR(103)
         KPDS(11) = CHAR(0)
         KPDS(12) = CHAR(10)
         IF(IYC.eq.2000)THEN
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
C
C          FILL GDS PARAMETERS
C
         KGDS(01)=0
         KGDS(02)=255
         KGDS(03)=1
         KGDS(04)=71
         KGDS(05)=34
         KGDS(06)=46400
         KGDS(07)=190000
         KGDS(08)=128
         KGDS(09)=64660
         KGDS(10)=120000
         KGDS(11)=0533
         KGDS(12)=1000
         KGDS(13)=20000
         KGDS(14)=64
         KGDS(15)=0
         KGDS(16)=0
         KGDS(17)=0
         KGDS(18)=0
         IGRID=255
         IGF=1
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
      IF(IFT.EQ.53)THEN
         CALL BAWRITE(53,KGRIB,LGRIB,KA,GRIB)
         KGRIB=KGRIB+KA
         PRINT *,'IERR2ND',IERR,LGRIB,KA,KGRIB
      ENDIF
C
      RETURN
      END
C
CXXXXNB----------------------------------------------------------------ENNNNNNNN
