      SUBROUTINE INTLZI(SSI)
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
C       OUTPUT   - SSI( , )   = 361X181 NORTHERN HEMISPHERIC GRIDDED ICE
C                               ACCRETION OUTPUT MATRIX - REAL
C
C ATTRIBUTES:
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C
C$$$
      SAVE
      REAL*4 SSI(361,181)
C                                                                       
C        INITIALIZE OUTPUT FIELD
C                                                                       
         DO 1410 J=1,181
            DO 1400 I=1,361
               SSI(I,J)=0.
 1400       CONTINUE
 1410    CONTINUE
C
      RETURN
      END
