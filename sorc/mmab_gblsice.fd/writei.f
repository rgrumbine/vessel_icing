      SUBROUTINE WRITEI(IFT06,SSI1X1,IDX,IGRIB,JGRIB,KGRIB,IERR)
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
C     SSI1X1   - ICE ACCRETION MATRIX FOR 1X1 N HEMISPHERIC GRID
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
C     fort.52  - GRIBBED NH 1.0x1.0 DEG ICE ACCRETION DATA AT 3-H INTERVALS
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
C       INPUT    - SSI1X1( , )= N HEMIS 1X1 ICE ACCRETION MATRIX M/S
C                  IFT06      = FILE NAME FOR PRINT OUT
C                  IDATE      = DATE TIME GROUP (YYMMDDCC): YEAR, MONTH,
C                               DAY AND CYCLE TIME - I*4                
C                                                                       
C       INTERNAL - NONE
C                                                                       
C       OUTPUT   - SSIG( )    = N HEMIS 1X1 GRIBBED ICE ACCRETION
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C
C$$$
      SAVE
      PARAMETER (JS=360*181,IFT52=52,IGBS=260640,IPDS=28,IMXBIT=0,
     1           IMS=360,JMS=181)
C
      REAL*4 SSI1X1(361,181),TEMP(9,9),TMP(360,181)

      REAL*4 SSIG(JS)
C
      CHARACTER ZERO*1,GBSI(IGBS)*4
C
      COMMON/FDT/IDATE,ICY,IYC,IY,IM,ID
C
      EQUIVALENCE (SSIG(1),TMP(1,1))
C
      data ZERO/'0'/
C 
      IERR=0
C                                                                       
C          GRIB 1X1 GLOBAL ICE ACCRETION
C
      DO J=1,181
         DO I=1,360
            TMP(I,J)=SSI1X1(I,J)
         ENDDO
      ENDDO
      WRITE(06,5000)IY,IM,ID,ICY,IYC
 5000 FORMAT(/1X,5I5/)
      CALL GRBICE(IFT52,SSIG,IDX,IDX,IMS,JMS,IMXBIT,IPDS,IY,IM,ID,ICY,
     1            IYC,JS,IGBS,GBSI,LGRIB,IGRIB,JGRIB,KGRIB,IERR)
      WRITE(IFT06,1500)IERR,LGRIB,JGRIB,IDX
 1500    FORMAT(' IERR FIRST ',4I8)
C
         WRITE(06,1010)IDATE,IERR,IDX
 1010    FORMAT(1H0,3I8/)
      RETURN
      END
