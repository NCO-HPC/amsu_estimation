      subroutine amsua_ret_metop2(nup,at,bt,ispot,ttype,bl,
     &                                 tret,tpw,clq)
c
c    programmer  Mitch Goldberg  NOAA/NESDIS/ORA  301-763-8136
c                mgoldberg@nesdis.noaa.gov
c
c     07/09/98   Applies antenna correction and limb adjusts AMSU-A  and 
c     compute retrievals
c     04/03/01  Modification for NOAA-16.  Note, I decided not to apply 
c     antenna corrections to NOAA-16, since I found that there is a larger 
c     difference between NOAA-16 and NOAA-15 when antenna corrections 
c     are used.  Better not to use antennacorrections.  I plan to regenerate 
c     coefficients for NOAA-15 w/o corrections.
c
c     input  integer   nup     regression coefficients, based on simulated 
c                              btemps, are used from level 1 to nup.  
c                              Typically nup us set to 10. 
c     input  real*4    at(15)   15 amsua antenna temperatures
c     input  integer   ispot   spot number (1-30)
c     input  integer   ttype   terrain type  0 = sea, 1 = land, 2 - coast, 
ck     input  integer   ttype   terrain type  1 = sea, 0 = land, 2 - coast, 
ck    This convention is changed for the buffer filesc     10 = snow, 11=ice
c     10 = snow, 11=ice
c     output real*4    bl(15)  15 amsua limb adjusted brightness temperatures
c     output real*4    bt(15)   15 amsua brightness temperatures
c     output real*4    tret(40)   40 level temperature retrieval
c     output real*4    tpw    retrieved total precip water
c     output real*4    clq     retrieved cloud liquid water
c
c
      real at(15),bt(15),bl(15)
      real coef(15,30,15), amean(15,30,15),dmean(15)
      real coefy(15,30,15), ameany(15,30,15),dmeany(15)
      integer nchx(15),nchanx(15,15) ,ttype,ispot
      integer nchy(15),nchany(15,15), nchanz(40,15),nchz(40) 
      integer  nchanzr(40,15),nchzr(40)
      real intercr(40),coefregr(40,15) 
      real interc(40),coefreg(40,15) ,tret(40),delta(15)
      data delta/4*0.,-.33,-.72,-1.0,-.63,-.55,-.42,-.05,.33,.34,-.20,0/ 
      save       !   some operating systems requires this
      if(first.ne.1.5) then       ! read files in only once
c
c     read in limb adjustment coefficients - sea
c
      OPEN(56,FILE='amsu_metop2limb_sea.txt')

      do i = 1,15 ! channels
      read(56,*)
      read(56,*) nx  ,nchx(i), dmean(i)
      read(56,*) (nchanx(i,k),k=1,nchx(i))

      do j = 1, 30 !angles
      read(56,*)nn,m,(coef(i,j,nchanx(i,k)),k=1,nchx(i)),
     &   (amean(nchanx(i,k),j,i),k=1,nchx(i)),error 
      enddo
      enddo
      close (56)
c
c          read in limb adjustment coefficients - nonsea
c
      OPEN(66,FILE='amsu_metop2limb_land.txt')

      do i = 1,15 ! channels
      read(66,*)
      read(66,*) ny  ,nchy(i), dmeany(i)
      read(66,*) (nchany(i,k),k=1,nchy(i))

      do j = 1, 30 !angles
      read(66,*)nn,m,(coefy(i,j,nchany(i,k)),k=1,nchy(i)),
     &  (ameany(nchany(i,k),j,i),k=1,nchy(i)),error 
      enddo
      enddo
      close (66)
c
c
c     read in  sea  temperature retrieval coefficients based on raob 
c     matchups for 5 to 1000 mb
c
      OPEN(59,FILE='amsu_metop2regr_sea.txt')
      do i =9,40 ! levels
      read(59,*)
      read(59,*)
      read(59,*) nchz(i), (nchanz(i,k),k=1,nchz(i))
      read(59,*) interc(i),( coefreg(i,nchanz(i,j)),j=1,nchz(i))
      enddo
      close(59)
c          read in  non-sea temperature retrieval coefficients based on raob matchups for 5 to 1000 mb
      OPEN(59,FILE='amsu_metop2regr_land.txt')
      do i =9,40 ! levels
      read(59,*)
      read(59,*)
      read(59,*) nchzr(i), (nchanzr(i,k),k=1,nchzr(i))
      read(59,*) intercr(i),( coefregr(i,nchanzr(i,j)),j=1,nchzr(i))
      enddo
      close(59)
c
c     read in temperature retrieval coefficients based on simulated 
c     rocketsondes 
c
      if(nup.lt.1.or.nup.gt.40) nup=10  ! make sure reasonable value is used.
      OPEN(59,FILE='amsu_metop2amsusimreg.txt')
      do i =1,nup ! levels
      read(59,*)
      read(59,*)
      read(59,*) nchz(i), (nchanz(i,k),k=1,nchz(i))
      read(59,*) interc(i),( coefreg(i,nchanz(i,j)),j=1,nchz(i)),error
      enddo
      close(59)
c
c     read in temperature retrieval coefficients based on simulated 
c     rocketsondes 
c     The same coefficients are read twice to allow for logic of 
c     sea/non-sea coefficients
c
      OPEN(59,FILE='amsu_metop2amsusimreg.txt')
      do i =1,nup ! levels
      read(59,*)
      read(59,*)
      read(59,*) nchzr(i), (nchanzr(i,k),k=1,nchzr(i))
      read(59,*) intercr(i),( coefregr(i,nchanzr(i,j)),j=1,nchzr(i)),error
      enddo
      close(59)
      endif
      first=1.5
c*****************************************************************************8
c
c     apply antenna correction --- NOT!!!  No correction for NOAA-16
c
      do i = 1,15
      bt(i) =at(i) 
      enddo
c       return
c
c     apply limb adjustment  -sea
c
      if(ttype.eq.1) then
      do i=1,15
      sum=0.
      do k = 1,nchx(i)
      sum = sum + coef(i,ispot,nchanx(i,k))*( bt(nchanx(i,k)) 
     &          - amean(nchanx(i,k),ispot,i))
      enddo
      bl(i) =  sum +  dmean(i)
      enddo
      b4save=bl(4)
      endif
c
c     apply limb adjustment - nonsea
c
      if(ttype.ne.1) then
      do i=1,15
      sum=0.
      do k = 1,nchy(i)
      sum = sum + coefy(i,ispot,nchany(i,k))*( bt(nchany(i,k)) 
     &             - ameany(nchany(i,k),ispot,i))
      enddo
      bl(i) = sum +  dmeany(i)
      enddo
      endif
c******************grody tpw and clq algorithm for ocean only*****************
      tpw=-99.
      clq=-99.
      if(ttype.eq.1.and.bl(1).lt.285.and.bl(2).lt.285) then
      tpw = 247.92 - 116.270*alog(285-bl(1)) + 73.409*alog(285-bl(2))
     &          -(69.235-44.177)
      clq = 8.24 +.754*alog(285-bl(1)) - 2.265*alog(285-bl(2))
     &        -(2.622-1.846)
      tpw=-1.234 + .913*tpw   !based on fcst sfc pre  and clq<.1 and dt<1 hour
      if(clq.gt.1./24) then     ! tpw correction based on NOAA-15
      corx = (-1 + 24*clq)
      if(corx.gt.25.)corx=25.
      tpw = tpw - corx 
      endif
      if(clq.lt.0.) clq=0.001
      if(clq.gt.4.) clq=4.
      if(tpw.lt.0.) tpw=0.001
      if(tpw.gt.80.) tpw=80.
      endif 
c*****************************************************************************8
c
c      we set the measured - computed delta for channels 12 - 14 to zero since
c       there is no truth in the upper stratosphere to compute the delta.
c*****************************************************************************8
c
       
       
      do i = 12,14
      delta(i)=0
      enddo

c
c         apply sea-only retrieval coefficients
c
      if(ttype.eq.1) then
      do i=1,40
      sum= interc(i)
c      if(i.eq.37) bl(4) = 31.532 + .096*bl(1) - .104*bl(2) + .875*bl(4)
      do k = 1,nchz(i)
      if(i.lt.nup+1)sum = sum + coefreg(i,nchanz(i,k)) 
     &           * (bl(nchanz(i,k))-delta(nchanz(i,k)))
      if(i.gt.nup)sum = sum + coefreg(i,nchanz(i,k)) 
     &                         * (bl(nchanz(i,k))) 
      enddo
      tret(i) = sum
      enddo
      bl(4)=b4save
      endif
c
c         apply non-sea retrieval coefficients
c
      if(ttype.ne.1) then
      do i=1,40
      sum= intercr(i)
      do k = 1,nchzr(i)
      if(i.lt.nup+1)sum = sum + coefregr(i,nchanzr(i,k)) 
     &                         * (bl(nchanzr(i,k))-delta(nchanzr(i,k)))
      if(i.gt.nup)sum = sum + coefregr(i,nchanzr(i,k)) 
     &                          * (bl(nchanzr(i,k))) 
      enddo
      tret(i) = sum
      enddo
      endif

      return
      end
