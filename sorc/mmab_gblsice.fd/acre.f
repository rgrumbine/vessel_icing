C
      SUBROUTINE ACRE(X,Y,Z,V)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C SUBPROGRAM:    ACRE         COMPUTES RATE OF ICE ACCRETION            
C   PRGMMR: D. M. FEIT       ORG: W/NMC21    DATE: 87-06-26             
C                                                                       
C ABSTRACT: COMPUTES RATE OF ICE ACCRETION WITH PMEL ALGORITHM.         
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   87-06-26  D. M. FEIT
C   96-07-23  L. D. BURROUGHS - CONVERTED FOR USE ON CRAY
C                                                                       
C USAGE:    CALL ACRE(X,Y,Z,V)
C                                                                       
C   INPUT ARGUMENT LIST:
C     X        - AIR TEMPERATURE IN DEG CELCIUS
C     Y        - SEA SURFACE TEMPERATURE IN DEG CELCIUS
C     Z        - WIND SPEED IN METERS/SEC
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     V        - RATE OF ICE ACCRETION IN CM/HR
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
C       INPUT    - X          = AIR TEMPERATURE IN DEG CELCIUS
C                  Y          = SEA SURFACE TEMPERATURE IN DEG CELCIUS
C                  Z          = WIND SPEED IN METERS/SEC
C                                                                       
C       INTERNAL - A          = 1ST OF THREE CONSTANTS FOR CUBIC FIT
C                  B          = 2ND OF THREE CONSTANTS FOR CUBIC FIT
C                  C          = 3RD OF THREE CONSTANTS FOR CUBIC FIT
C                  PR         = FUNCTIONAL RELATION OF INPUTS X, Y,
C                               AND Z
C                                                                       
C       OUTPUT   - V          = RATE OF ICE ACCRETION IN CM/HR
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: CF77
C   MACHINE:  CRAY3
C                                                                       
C$$$
      SAVE
      A=2.73E-02
      B=2.91E-04
      C=1.84E-06
      PR=Z*(-1.7-X)/(1.+.4*(Y+1.7))
      V=A*PR+B*PR*PR+C*PR**3
      RETURN
      END
