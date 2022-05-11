      subroutine smooth (fld,temp,idim,jdim,idm,jdm,izp)
c -------------------------------------------------------------------
c --- smooths field in zonal and meridional directions 
c --- smoothing function from Shapiro (Shapiro, Ralph,1975: 
c --- Linear Filtering, Mathematics of Computation, Vol 29, No. 132,
c --- p. 1094-1097 
c --- 3 point filter
c --- 01/30/96  ---  Fiona Horsfall
c -------------------------------------------------------------------
c     Modified 2/7/96 by MDM
c
c     This version modified 8/99 to include temp array in argument list
c
c     izp = 1 if field is zonally periodic with no overlapping point
c         = 0 if not periodic
c
c -------------------------------------------------------------------
c
      real temp(idim,jdim),fld(idim,jdim)
c
c --- set weights 
c
      wt1=.5
      wt2=.25
c
c --- apply filter zonally
c
      do 20 j=1,jdm
      do 20 i=1,idm
         im1=i-1
         ip1=i+1 
         if (i.eq.1) then
            if (izp .eq. 1) then
               im1=idm
            else
               im1=1
            endif
         endif
c
         if (i.eq.idm) then
            if (izp .eq. 1) then
               ip1=1
            else
               ip1=idm
            endif
         endif
c
         temp(i,j)=fld(i,j)*wt1+(fld(ip1,j)+fld(im1,j))*wt2
   20 continue
c
c --- apply filter meridionally
c
      do 30 i=1,idm
      do 30 j=1,jdm
         jm1=j-1
         jp1=j+1 
c
         if (j.eq.  1) jm1=1
         if (j.eq.jdm) jp1=jdm 
c
         fld(i,j)=temp(i,j)*wt1+(temp(i,jp1)+temp(i,jm1))*wt2
   30 continue
c
      return 
      end
