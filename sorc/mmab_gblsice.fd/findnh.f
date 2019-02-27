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
C                01 - 06.  (I*4)  CODE IS GIVEN BELOW:                  
C                01 = 2 M AIR TEMPERATURE
C                02 = 10 M EARTH-ORIENTED U-WIND
C                03 = 10 M EARTH-ORIENTED V-WIND
C                04 = LAND/SEA TAG MATRIX
C                05 = SEA ICE TAG MATRIX
C                06 = SEA SURFACE TEMPERATURE
C     IFCST    - THE FORECAST TAU AT 03-H INTERVALS FROM 00 - 168 HOURS  
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
C                               01 = 2 M AIR TEMPERATURE
C                               02 = 10 M EARTH-ORIENTED U-WIND
C                               03 = 10 M EARTH-ORIENTED V-WIND
C                               04 = LAND/SEA TAG MATRIX
C                               05 = SEA ICE TAG MATRIX
C                               06 = SEA SURFACE TEMPERATURE
C                  IFCST      = THE FORECAST TAU AT 03-H INTERVALS
C                               FROM 00 - 168 HOURS
C                                                                       
C       INTERNAL - JF         = DIMENSION OF FLD AND LB
C                  JPDS( )    = INPUT PDS MATRIX
C                  JGDS( )    = INPUT GDS MATRIX
C                  LUGB       = GRIB FILE NAME
C                  LUGI       = INDEX FILE NAME
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
      SAVE
      PARAMETER(JF=360*181)
      INTEGER*4 PDS,GDS
      INTEGER*4 JPDS(25),JGDS(22),IGRD(18)
      INTEGER*4 KPDS(25),KGDS(22)
      REAL*4 FLD(JF),OUT(360,181),XFLD(361,181)
      LOGICAL LB(JF)
      CHARACTER*80 FILEB,FILEI
      CHARACTER*11 ENVVAR
C
      COMMON/FDT/IDATE,ICY,IYC,IY,IM,ID
C
      EQUIVALENCE(OUT(1,1),FLD(1))
C
      DATA IGRD/11,105, 2,33,105,10,34,105,10,81,  1, 0,91,  1, 0,
C     DATA IGRD/11,100,1000,33,100,1000,34,100,1000,81,  1, 0,91,  1, 0,
     1          11,  1, 0/
C For grib2
!      INTEGER jpdtn, jgdtn, jdisc
!      INTEGER jids(200), jpdt(200), jgdt(200)
!      LOGICAL unpack
!      TYPE(GRIBFIELD) :: gfld
C
      INO = 0
      DO PDS=1,25
         JPDS(PDS)=-1
      ENDDO
      DO GDS=1,22
         JGDS(GDS)=-1
      ENDDO
C                                                                       
      WRITE(06,1020)IFCST
 1020 FORMAT(1H0,'IFCST',I4)
C
      unpack = .TRUE.
      jpdtn = -1
      jgdtn = -1
      jdisc = -1
C                                                                       
C          DETERMINE ANALYSIS OR MODEL FIELD ID                         
C
      IDX=(IFCST/3)+1
      LUGB=IDX+94
      LUGI=IDX+194
      JB=0
      IOFLD=NOFLD
      ENVVAR='XLFUNIT_   '
      IF(IDX.LE.5)THEN
         WRITE(ENVVAR(9:10),FMT='(I2)')LUGB
      ELSE
         WRITE(ENVVAR(9:11),FMT='(I3)')LUGB
      ENDIF
      CALL GETENV(ENVVAR,FILEB)
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:11),FMT='(I3)')LUGI
      CALL GETENV(ENVVAR,FILEI)
      CALL BAOPENR(LUGB,FILEB,IRET1)
      CALL BAOPENR(LUGI,FILEI,IRET2)
      IF((IRET1+IRET2).NE.0)THEN
         PRINT *,'iret1, iret2 = ',iret1, iret2
         WRITE(IFT06,FMT='("Error opening GRIB data file or index file."
     1)')
         INO=9
         GO TO 1100
      END IF
C                                                                       
C          SET IDENT FOR CORRECT FORECAST                               
C          GET SELECT FIELDS
CXXXXNS----------------------------------------------------------------E
 1040 JPDS(5)=IGRD(IDENT)
      JPDS(6)=IGRD(IDENT+1)
      JPDS(7)=IGRD(IDENT+2)
      PRINT *,'IDENT, JPDS, IGRD ',IDENT,JPDS(5),IGRD(IDENT),JPDS(6),
     1        IGRD(IDENT+1),JPDS(7),IGRD(IDENT+2)
      PRINT *,'CALL GETGB'
      CALL GETGB(LUGB,LUGI,JF,JB,JPDS,JGDS,KF,K,KPDS,KGDS,LB,FLD,IRET)
!      PRINT *,'Call getgb2'
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
!
!      STOP

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
      DO J=1,181
         DO KK=1,360
C           XFLD(KK,181-J+1)=OUT(KK,J)
            XFLD(KK,J)=OUT(KK,J)
         ENDDO
C
C          DUPLICATE GREENWICH
C
         XFLD(361,J)=XFLD(1,J)
      ENDDO
C
C          SAVE NORTHERN HEMISPHERE GRID
C
C     DO J=1,181
C       DO K=1,361
C           XFLD(K,J)=OUTF(K,J)
C       ENDDO
C     ENDDO

 1100 CONTINUE
      CLOSE(LUGB)
      CLOSE(LUGI)
C                                                                       
      RETURN
      END
