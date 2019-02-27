      subroutine rdgrbsst(lupb,im,jm,anal,iyrstmp,imstmp,idstmp,iret)
c
c     to use this routine, the user must set the following 
c     variables in the invoking script:
c
c     XLFRTEOPTS="unit_vars=yes"
c     XLFUNIT_lupb="name of GRIB SST file"
c       where lupb is the FORTRAN unit number of the 
c       GRIB SST file.
c
c     INPUT VARIABLES
c
c     lupb is FORTRAN unit number of GRIB SST file.
c     im is longitudinal dimension of SST.
c     jm is latitudinal dimension of SST.
c
c     OUTPUT VARIABLES
c
c     anal is SST field.
c       anal(1,1)     is at 0.25 deg. E, 89.75 deg. S
c       anal(720,1)   is at 359.75 deg. E, 89.75 deg. N
c       anal(1,360)   is at 0.25 deg. E, 89.75 deg. S
c       anal(720,360) is at 359.75 deg. E, 89.75 deg. N
c     iyrstmp is analysis year.
c     imstmp is analysis month.
c     idstmp is analysis day.
c     iret is return code.
c
c     Subroutine rdgrbsst must be compiled with the NCEP W3 library
c     and the BACIO library.
c
c     Upon return, iret = 0 indicates successful execution.
C
C     25 February 2016 -- add coding for grib2, but do not use as the
C     sst is not yet in grib2.
C     Robert Grumbine
c
!grib2      USE GRIB_MOD
      IMPLICIT none

      INTEGER lupb, im, jm, iyrstmp, imstmp, idstmp, iret
      REAL anal(im,jm)
 
      character ciu*2,ename*10,fname*80
      integer jpds(25),jgds(22),kpds(25),kgds(22)
      logical*1 lb(im*jm)
      real f(im*jm)

      INTEGER I, J, JF, K, KF, NI, NJ, INTRVAL, IOFFSET, MSCAN
      INTEGER KB1, KB2, KB3, IINCDIR, JINCDIR
      REAL LAT1, LON1, LATI, LONI
C  
C     GRIB2
!      INTEGER jpdtn, jgdtn, jdisc
!      INTEGER jids(200), jpdt(200), jgdt(200)
!      LOGICAL unpack
!      TYPE(GRIBFIELD) :: gfld
c                                                                       
!      unpack = .TRUE.
!      jpdtn = -1
!      jgdtn = -1
!      jdisc = -1

c  -  reading in the analysis                                           
c                                                                       
      jf=im*jm
      j=0
      jpds=-1
      jgds=-1
      jpds(5)=11
      jpds(6)=1
      write(ciu,'(i2)')lupb
      ename='XLFUNIT_'//adjustl(ciu)
      call getenv (ename, fname)
      call baopenr(lupb,trim(fname),iret)
      call getgb(lupb,0,jf,j,jpds,jgds,kf,k,kpds,kgds,lb,f,iret)
      print*,' k from getgb:',k
!      CALL GETGB2(lupb,0,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
!     &                  UNPACK,K,GFLD,IRET)
!      PRINT *,'iret from getgb2 ',iret

      if(iret.ne.0)go to 100
      ni=kgds(2)
      nj=kgds(3)
      lat1=kgds(4)
      lon1=kgds(5)
      lati=kgds(9)
      loni=kgds(10)
      if(ni.ne.im.or.nj.ne.jm)then
        print*, '***** unexpected grid dimensions: ',ni,nj
        go to 300
      endif
      intrval = 360000 / im
      ioffset = intrval / 2
      if(lat1.ne.-90000+ioffset.and.lat1.ne.90000-ioffset)then
        print*, '***** unexpected starting lat: ',lat1
        go to 300
      endif
      if(lon1.ne.ioffset.and.lon1.ne.-1*ioffset.and.
     1   lon1.ne.360000-ioffset)then
        print*, '***** unexpected starting lon: ',lon1
        go to 300
      endif
      if(lati.ne.intrval.or.loni.ne.intrval)then
        print*, '***** unexpected inc for lat or lon: ',lati,loni
        go to 300
      endif

      mscan=kgds(11)
      kb1=ibits(mscan,7,1)   ! i scan direction
      kb2=ibits(mscan,6,1)   ! j scan direction
      kb3=ibits(mscan,5,1)   ! (i,j) or (j,i)
c  get i and j scanning directions from kb1 and kb2.
c    0 yields +1, 1 yields -1. +1 is west to east, -1 is east to west.
      iincdir = 1-kb1*2
c    0 yields -1, 1 yields +1. +1 is south to north, -1 is north to 
c      south.
      jincdir = kb2*2 - 1
      do k=1,kf
c   kb3 from scan mode indicates if i points are consecutive
c      or if j points are consecutive
        if(kb3.eq.0)then     !  (i,j)
          i=(ni+1)*kb1+(mod(k-1,ni)+1)*iincdir
          j=(nj+1)*(1-kb2)+jincdir*((k-1)/ni+1)
        else                !  (j,i)
          j=(nj+1)*(1-kb2)+(mod(k-1,nj)+1)*jincdir
          i=(ni+1)*kb1+iincdir*((k-1)/nj+1)
        endif
C       anal(i,j)=f(k)-273.15
        anal(i,j)=f(k)
      enddo

      iyrstmp=kpds(8) + (kpds(21)-1)*100
      imstmp=kpds(9)
      idstmp=kpds(10)
      iret = 0
      return
  100 print 101
  101 format (/'*** no analysis field: guess not written: exit 11')
      iret = 11
      return
  300 print 301
      iret = 12
  301 format (/'*** unexpected panal grid: guess not written: exit 12')
      return
      end
