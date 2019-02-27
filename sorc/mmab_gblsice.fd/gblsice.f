      PROGRAM GBLSICE
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                                                                       
C MAIN PROGRAM: GBLSICE
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
C   03-09-05  L.D.BURROUGHS  Extend to global extent and use rtg_sst
C                                                                   
C   03-10-07  L.D.BURROUGHS  Extend to correct GRIB bulletin to n-s instead of
C                            s-n.
C 2016-02-26  R.W.Grumbine   Change to externally-decoded GRIB2 files for input
C                            Add fort.94 for the masks, ignore the grib
C                            index files
C                                                                   
C USAGE:                                                                
C   INPUT FILES:                                                        
C      fort. 94 - GFS Land, ice mask file
C      fort. 95 - AVN INITIALIZATION 00/06/12/18 UTC CYCLES
C      fort.195 - AVN INITIALIZATION GRIB INDEX FILE
C      fort. 96 - AVN 03-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.196 - AVN 03-H FORECASTS GRIB INDEX FILE
C      fort. 97 - AVN 06-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.197 - AVN 06-H FORECASTS GRIB INDEX FILE
C      fort. 98 - AVN 09-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.198 - AVN 09-H FORECASTS GRIB INDEX FILE
C      fort. 99 - AVN 12-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.199 - AVN 12-H FORECASTS GRIB INDEX FILE
C      fort.100 - AVN 15-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.200 - AVN 15-H FORECASTS GRIB INDEX FILE
C      fort.101 - AVN 18-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.201 - AVN 18-H FORECASTS GRIB INDEX FILE
C      fort.102 - AVN 21-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.202 - AVN 21-H FORECASTS GRIB INDEX FILE
C      fort.103 - AVN 24-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.203 - AVN 24-H FORECASTS GRIB INDEX FILE
C      fort.104 - AVN 27-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.204 - AVN 27-H FORECASTS GRIB INDEX FILE
C      fort.105 - AVN 30-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.205 - AVN 30-H FORECASTS GRIB INDEX FILE
C      fort.106 - AVN 33-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.206 - AVN 33-H FORECASTS GRIB INDEX FILE
C      fort.107 - AVN 36-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.207 - AVN 36-H FORECASTS GRIB INDEX FILE
C      fort.108 - AVN 39-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.208 - AVN 39-H FORECASTS GRIB INDEX FILE
C      fort.109 - AVN 42-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.209 - AVN 42-H FORECASTS GRIB INDEX FILE
C      fort.110 - AVN 45-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.210 - AVN 45-H FORECASTS GRIB INDEX FILE
C      fort.111 - AVN 48-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.211 - AVN 48-H FORECASTS GRIB INDEX FILE
C      fort.112 - AVN 51-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.212 - AVN 51-H FORECASTS GRIB INDEX FILE
C      fort.113 - AVN 54-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.213 - AVN 54-H FORECASTS GRIB INDEX FILE
C      fort.114 - AVN 57-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.214 - AVN 57-H FORECASTS GRIB INDEX FILE
C      fort.115 - AVN 60-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.215 - AVN 60-H FORECASTS GRIB INDEX FILE
C      fort.116 - AVN 63-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.216 - AVN 63-H FORECASTS GRIB INDEX FILE
C      fort.117 - AVN 66-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.217 - AVN 66-H FORECASTS GRIB INDEX FILE
C      fort.118 - AVN 69-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.218 - AVN 69-H FORECASTS GRIB INDEX FILE
C      fort.119 - AVN 72-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.219 - AVN 72-H FORECASTS GRIB INDEX FILE
C      fort.120 - AVN 75-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.220 - AVN 75-H FORECASTS GRIB INDEX FILE
C      fort.121 - AVN 78-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.221 - AVN 78-H FORECASTS GRIB INDEX FILE
C      fort.122 - AVN 81-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.222 - AVN 81-H FORECASTS GRIB INDEX FILE
C      fort.123 - AVN 84-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.223 - AVN 84-H FORECASTS GRIB INDEX FILE
C      fort.124 - AVN 87-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.224 - AVN 87-H FORECASTS GRIB INDEX FILE
C      fort.125 - AVN 90-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.225 - AVN 90-H FORECASTS GRIB INDEX FILE
C      fort.126 - AVN 93-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.226 - AVN 93-H FORECASTS GRIB INDEX FILE
C      fort.127 - AVN 96-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.227 - AVN 96-H FORECASTS GRIB INDEX FILE
C      fort.128 - AVN 99-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.228 - AVN 99-H FORECASTS GRIB INDEX FILE
C      fort.129 - AVN 102-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.229 - AVN 102-H FORECASTS GRIB INDEX FILE
C      fort.130 - AVN 105-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.230 - AVN 105-H FORECASTS GRIB INDEX FILE
C      fort.131 - AVN 108-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.231 - AVN 108-H FORECASTS GRIB INDEX FILE
C      fort.132 - AVN 111-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.232 - AVN 111-H FORECASTS GRIB INDEX FILE
C      fort.133 - AVN 114-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.233 - AVN 114-H FORECASTS GRIB INDEX FILE
C      fort.134 - AVN 117-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.234 - AVN 117-H FORECASTS GRIB INDEX FILE
C      fort.135 - AVN 120-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.235 - AVN 120-H FORECASTS GRIB INDEX FILE
C      fort.136 - AVN 123-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.236 - AVN 123-H FORECASTS GRIB INDEX FILE
C      fort.137 - AVN 126-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.237 - AVN 126-H FORECASTS GRIB INDEX FILE
C      fort.138 - AVN 129-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.238 - AVN 129-H FORECASTS GRIB INDEX FILE
C      fort.139 - AVN 132-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.239 - AVN 132-H FORECASTS GRIB INDEX FILE
C      fort.140 - AVN 135-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.240 - AVN 135-H FORECASTS GRIB INDEX FILE
C      fort.141 - AVN 138-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.241 - AVN 138-H FORECASTS GRIB INDEX FILE
C      fort.142 - AVN 141-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.242 - AVN 141-H FORECASTS GRIB INDEX FILE
C      fort.143 - AVN 144-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.243 - AVN 144-H FORECASTS GRIB INDEX FILE
C      fort.144 - AVN 147-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.244 - AVN 147-H FORECASTS GRIB INDEX FILE
C      fort.145 - AVN 150-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.245 - AVN 150-H FORECASTS GRIB INDEX FILE
C      fort.146 - AVN 153-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.246 - AVN 153-H FORECASTS GRIB INDEX FILE
C      fort.147 - AVN 156-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.247 - AVN 156-H FORECASTS GRIB INDEX FILE
C      fort.148 - AVN 159-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.248 - AVN 159-H FORECASTS GRIB INDEX FILE
C      fort.149 - AVN 162-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.249 - AVN 162-H FORECASTS GRIB INDEX FILE
C      fort.150 - AVN 165-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.250 - AVN 165-H FORECASTS GRIB INDEX FILE
C      fort.151 - AVN 168-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.251 - AVN 168-H FORECASTS GRIB INDEX FILE
C      fort.90 - DATE FILE                                              
C      fort.91 - CYCLE TIME FILE
C      fort.92 - rtg_sst
C                                                                       
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C      fort.06 - PRINTOUT                                               
C      fort.52 - GRIBBED NH 1X1 DEG ICE ACCRETION DATA AT 3-H INTERVALS OUT
C                TO 168 HOURS USED FOR NEW WEB PAGE AND NAWIPS
C
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - FIND   FINDNH INTLZI ACRE  WRITEI BLINT 
C                  BILINT GRBICE GICBTS rdgrbsst
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
C   LANGUAGE: FORTRAN90
C   MACHINE:  IBM SP
C
C$$$
      SAVE
      PARAMETER (IFT06=06)
      CALL W3TAGB('GBLSICE',2003,0272,0290,'W/NP21') 
      OPEN(90,ACCESS='SEQUENTIAL',FORM='FORMATTED',RECL=8)
      OPEN(91,ACCESS='SEQUENTIAL',FORM='FORMATTED',RECL=2)
      OPEN(94,FORM="UNFORMATTED", STATUS="OLD")
C                                                                       
      WRITE(IFT06,1000)
 1000 FORMAT(1H ,'START MAIN PROGRAM')
C                                                                       
      CALL FIND(IFT06,IERR,KERR)
      IF(IERR.EQ.9) THEN
         ICODE=9
         WRITE(IFT06,1100)IERR,ICODE
 1100    FORMAT(1H0,'JOB ENDED ABNORMALLY. ERROR IN WRITEF',2I3)
         CALL W3TAGE('GBLSICE') 
         CLOSE(90)
         CLOSE(91)
         CLOSE(52)
         STOP 9
      ELSE IF(KERR.EQ.9) THEN
         ICODE=8
         WRITE(IFT06,1105)KERR,ICODE
 1105    FORMAT(1H0,'JOB ENDED ABNORMALLY. ERROR IN GETGB',2I3)
         CALL W3TAGE('GBLSICE') 
         CLOSE(90)
         CLOSE(91)
         CLOSE(52)
         STOP 8
      ELSE
         ICODE=0
         WRITE(IFT06,1120)IERR,KERR,ICODE
 1120    FORMAT(1H0,'JOB ENDED NORMALLY',3I3)
         CALL W3TAGE('GBLSICE') 
         CLOSE(90)
         CLOSE(91)
         CLOSE(52)
         STOP
      END IF
      END
