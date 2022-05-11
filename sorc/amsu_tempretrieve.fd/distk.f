c
      subroutine distk(rlon1,rlat1,rlon2,rlat2,dx,dy,rad)
c     This routine calculates the distance in km (rad) between the
c     points (rlon1,rlat1) and (rlon2,rlat2) using an approximate
c     formula. The lon and lat are in deg E and N. The east and
c     north components of the distance (dx,dy) are also calculated.
c
      dtk = 111.1
      dtr = 0.0174533
c
      cfac = cos(0.5*dtr*(rlat1+rlat2))
c
      dx  = dtk*(rlon2-rlon1)*cfac
      dy  = dtk*(rlat2-rlat1)
      rad = sqrt(dx*dx + dy*dy)
c
      return
      end
