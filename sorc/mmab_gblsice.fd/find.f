C                                                                               
      SUBROUTINE FIND(IFT06,IERR,KERR)
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
C   03-09-08  L. D. BURROUGHS - CHANGED TO USE RTG_SST AND TO BE GLOBAL
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
C      fort.52 - GRIBBED NH 1X1 DEG ICE ACCRETION DATA AT  3-H INTV
C                OUT TO 168 HOURS
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - RDGRBSST FINDNH INTLZI ACRE   WRITEI
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
C                                                                       
C       INTERNAL - IFCST      = FCST TAU AT 3-H INTRVLS FM 00-168 - I*4
C                  CMH2MS     = CONVERSION FACTOR CONVERTING ICE ACCRE
C                               FROM CM/HR TO M/S
C                  IFT90      = FILE NAME FOR DTG
C                  IFT91      = FILE NAME FOR CYCLE TIME
C                  IFT92      = RTG_SST
C                  SSI( , )   = ICE ACCRETION MATRIX IN CM/HR
C                  IONA       = 0 - IF FIELD IS FOUND BY FINDNH
C                               9 - IF FIELD IS NOT FOUND OR SOME OTHER
C                                   ERROR OCCURS - I*4
C                                                                       
C       OUTPUT   - IY         = YEAR - 1900 - I*4                       
C                  IM         = MONTH - I*4                             
C                  ID         = DAY - I*4                               
C                  ICY        = CYCLE (00/06/12/18 UTC) - I*4             
C                  SST( , )   = SEA SURFACE TEMPERATURE MATRIX - R*4    
C                  ATP( , )   = SURFACE AIR TEMPERATURE MATRIX - R*4    
C                  UWD( , )   = 1000 MB U-WIND COMPONENT MATRIX (GRD OR)
C                               R*4                                     
C                  VWD( , )   = 1000 MB V-WIND COMPONENT MATRIX (GRD OR)
C                               R*4                                     
C                  LND( , )   = LAND/SEA TAG MATRIX - R*4
C                  ICE( , )   = SEA ICE TAG MATRIX - R*4
C                  SSI1X1( , )= ICE ACCRETION MATRIX IN M/S
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C                                                                       
C$$$
      SAVE
      PARAMETER (CMH2MS=1./3.6E+05,IFT90=90,IFT91=91,IMM=360,JM=180,
     1           LUPB=92)
      REAL*4 LND(361,181),ICE(361,181),ATP(361,181),SST(361,181),
     1       UWD(361,181),VWD(361,181),SPD(361,181),SSI(361,181),
     2       ANAL(IMM,JM)
      REAL*4 SSI1X1(361,181)
      INTEGER*4 IDATE,ICY,IYC,IY,IM,ID
      CHARACTER*80 FILE1,FILEA,FILE1A
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
      LUG1A=52
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)')LUG1A
      CALL GETENV(ENVVAR,FILE1A)
      CALL BAOPENW(LUG1A,FILE1A,IRET2)
      IF((IRET2).NE.0)THEN
         WRITE(IFT06,FMT='("Error opening GRIB output data file for
     1  1x1 03-h." )')
      ENDIF
      IGRIB=0
      JGRIB=0
      KGRIB=0
      IFST=0

!      CALL FINDNH(IFT06,4,10,IFST,LND,IONA)
!      IF(IONA.NE.0)NERR=9
!      CALL FINDNH(IFT06,5,13,IFST,ICE,IONA)
!      IF(IONA.NE.0)NERR=9
      CALL transl(94, LND)
      CALL transl(94, ICE)

      CALL RDGRBSST(LUPB,IMM,JM,ANAL,IYRSTMP,IMSTMP,IDSTMP,IONA)
      IF(IONA.NE.0)NERR=9
      DO J=1,JM
         IF(J.EQ.JM)CYCLE
         DO I=1,IMM
            IF(I.EQ.IMM)CYCLE
            SST(I+1,180-J+1)=(ANAL(I+1,J+1)+ANAL(I+1,J)+ANAL(I,J+1)+
     1                    ANAL(I,J))/4.
            SST(I+1,1)=0.
            SST(I+1,181)=0.
         ENDDO
         SST(1,180-J+1)=(ANAL(1,J+1)+ANAL(1,J)+ANAL(IMM,J+1)+
     1                   ANAL(IMM,J))/4.
         SST(361,180-J+1)=SST(1,180-J+1)
      ENDDO

! do the diagnostics
      DO KK=1,57
C
!         IFCST=(KK-1)*3
         IFCST=94+KK
         CALL transl(IFCST, ATP)
         CALL transl(IFCST, UWD)
         CALL transl(IFCST, VWD)
C                                                                       
C          FIND AVN FIELDS
C                                                                       
!         CALL FINDNH(IFT06,1,01,IFCST,ATP,IONA)
!         IF(IONA.NE.0)NERR=9
!         CALL FINDNH(IFT06,2,04,IFCST,UWD,IONA)
!         IF(IONA.NE.0)NERR=9
!         CALL FINDNH(IFT06,3,07,IFCST,VWD,IONA)
!         IF(IONA.NE.0)NERR=9
!         IF(NERR.GT.0)GO TO 1030
C
         WRITE(IFT06,1020)
 1020    FORMAT(1H ,' FIELDS HAVE BEEN PROPERLY DECODED.')
         CALL INTLZI(SSI1X1)
         GO TO 1050
C
 1030    WRITE(IFT06,1040)
 1040    FORMAT(1H ,' FIELDS ARE MISSING. 0. WRITTEN')
         CALL INTLZI(SSI1X1)
         KERR=NERR
         CALL WRITEI(IFT06,SSI1X1,KK,IGRIB,JGRIB,KGRIB,IERR)
         GO TO 1240
C
C          COMPUTE N HEMISPHERIC 1X1 ICE ACCRETION FORECASTS
C
 1050    CONTINUE
         DO J=1,181
            DO I=1,361
C
C          LAND/SEA TEST
C
               IF(LND(I,J).GE.0.5)THEN
                  SSI(I,J)=0.
                  CYCLE
               ENDIF
C
C          SEA/ICE TEST
C
               IF(ICE(I,J).GE.0.5)THEN
                  SSI(I,J)=0.
                  CYCLE
               ENDIF
C
C          COMPUTE SUPERSTRUCTURE ICE ACCRETION FORECAST FOR EACH
C          GRIDPOINT
C
C          COMPUTE WIND SPEED
               SPD(I,J)=SQRT(UWD(I,J)**2+VWD(I,J)**2)
C
C          COMPUTE ICE ACCRETION IN CM/HR
C
               IF(SPD(I,J).GT.50.)THEN
                  SSI(I,J)=0.
                  CYCLE
               ENDIF
               AT = ATP(I,J) - 273.16
               IF(AT.GT.0..OR.AT.LT.-40.)THEN
                  SSI(I,J)=0.
                  CYCLE
               ENDIF
               ST = SST(I,J) - 273.16
               IF(ST.LT.-1.7.OR.ST.GT.12.)THEN
                  SSI(I,J)=0.
                  CYCLE
               ENDIF
               CALL ACRE(AT,ST,SPD(I,J),SSI(I,J))
               IF(SSI(I,J).LT.0.)THEN
                  SSI(I,J)=0.
               ENDIF
C
C         CHANGE ICE ACCRETION FROM CM/HR TO M/S
C
               SSI1X1(I,J)=CMH2MS*SSI(I,J)
           ENDDO ! i 
         ENDDO ! j
C
C          WRITE FORECAST FIELD TO GRIB, ARCHIVE AND RE-INITIALIZE
C
         WRITE(IFT06,2900)ATP(190,166),SST(190,166),SPD(190,166),
     1                    SSI(190,166),SSI1X1(190,166)
 2900    FORMAT(1H ,3F9.2,2E14.5)
         CALL WRITEI(IFT06,SSI1X1,KK,IGRIB,JGRIB,KGRIB,IERR)
 1240    NERR=0
         CALL INTLZI(SSI1X1)
      ENDDO ! k

      RETURN
      END

      SUBROUTINE transl(iunit, fld)
      IMPLICIT none
      INTEGER iunit, i, j
      REAL fld(361,181), tmp(360,181)

      READ (iunit) tmp
      
      DO J= 1, 181
         DO i= 1, 360
            fld(i,J)=tmp(i,J)
         ENDDO
C        DUPLICATE GREENWICH
         fld(361,J)=tmp(1,J)
      ENDDO

      RETURN
      END
