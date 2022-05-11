      subroutine tdiff(iy2,im2,id2,it2,iy1,im1,id1,it1,idelt)
c     This routine calculates the number of hours (delt) between
c     two date/times.
c
      dimension nday(12)
c
      data nday /0,31,59,90,120,151,181,212,243,273,304,334/
c
c     Calculate reference year
      iry = iy1-2
      if (iy2 .lt. iry) iry=iy2-2
c
c     Calculate the number of hours from 00 Jan. 1 of the reference year
      ity1 = 0
      do 10 i=iry,iy1-1
         if (mod(i,4) .eq. 0) then
            ity1 = ity1 + 24*366
         else
            ity1 = ity1 + 24*365
         endif
   10 continue
c
      ity2 = 0
      do 15 i=iry,iy2-1
         if (mod(i,4) .eq. 0) then
            ity2 = ity2 + 24*366
         else
            ity2 = ity2 + 24*365
         endif
   15 continue
c
      ity1 = ity1 + 24*nday(im1)
      if ((mod(iy1,4) .eq. 0) .and. im1 .gt. 2) ity1=ity1+24
c
      ity2 = ity2 + 24*nday(im2)
      if ((mod(iy2,4) .eq. 0) .and. im2 .gt. 2) ity2=ity2+24
c
      ity1 = ity1 + 24*id1 + it1
      ity2 = ity2 + 24*id2 + it2
c
      idelt = ity2 - ity1
c
      return
      end
