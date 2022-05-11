      subroutine jdayi(julday,iyear,imon,iday)
c     This routine calculates the month (imon) and day (iday)
c     from the Julian day (julday) and year (iyear).
c     The appropriate correction is made for leap year.
c
      dimension ndmon(12),nsum(13)
c
c     Specify the number of days in each month
      ndmon(1)  = 31
      ndmon(2)  = 28
      ndmon(3)  = 31
      ndmon(4)  = 30
      ndmon(5)  = 31
      ndmon(6)  = 30
      ndmon(7)  = 31
      ndmon(8)  = 31
      ndmon(9)  = 30
      ndmon(10) = 31
      ndmon(11) = 30
      ndmon(12) = 31
c
c     Correct for leap year
      if (mod(iyear,4) .eq. 0) ndmon(2)=29
c
c     Check for illegal input
      if (mod(iyear,4) .eq. 0) then
         mxjul = 366
      else
         mxjul = 365
      endif
c
      if (julday .lt. 1 .or. julday .gt. mxjul) then
         imon = -1
         iday = -1
         return
      endif
c
c     Calculate the month and day
      nsum(1) = 0
      do 10 i=1,12
         nsum(i+1) = nsum(i) + ndmon(i)
   10 continue
c
      do 20 i=2,13
         if (julday .le. nsum(i)) then
            imon = i-1
            go to 1000
         endif
   20 continue
 1000 continue
c
      iday = julday - nsum(imon)
c
      return
      end
