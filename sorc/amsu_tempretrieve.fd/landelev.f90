SUBROUTINE landelev(xlon,ylat,lfirst,ielev,iland)
  !
  ! This routine 1) reads in a land elevation dataset, if lfirst is .TRUE.,
  ! 2) using this array (either saved or read), linearly interpolates the
  ! elevation to the supplied lat/lon point, and returns the elevation and
  ! land/no-land mask.

  IMPLICIT NONE

  REAL, INTENT (INOUT) :: xlon,ylat
  INTEGER, INTENT (OUT):: ielev, iland
  LOGICAL, INTENT (INOUT) :: lfirst

  INTEGER :: i,j
  INTEGER :: im1,jm1,i1,j1,izp,ierr
  INTEGER :: ii1,iin,jj1,jjn
  INTEGER, ALLOCATABLE ::ied(:,:)
  REAL :: alon1,alonn,alat1,alatn,slon1,slat1,dlon1,dlat1
  REAL,ALLOCATABLE:: fed(:,:)
  REAL:: fsp
  SAVE fed,slon1,slat1,dlon1,dlat1,iin,jjn

  IF (lfirst) THEN

     OPEN (unit=67,file='amsu_lsmask30')

     READ (67,*)
     READ (67,'(10x,4i10)')ii1,iin,jj1,jjn
     READ (67,'(10x,4f10.2)')alon1,alonn,alat1,alatn

     slon1=alon1
     slat1=alat1

     dlon1=(alonn-alon1)/(iin-ii1)
     dlat1=(alatn-alat1)/(jjn-jj1)

     ALLOCATE (fed(iin,jjn))
     ALLOCATE (ied(iin,jjn))

     DO j=jj1,jjn
        READ(67,'(20i5)')(ied(i,j),i=ii1,iin)
     END DO

     fed=float(ied)

     lfirst=.FALSE.

     IF (ALLOCATED (ied)) DEALLOCATE (ied)

     CLOSE(67)

  END IF

  IF (xlon < 0.0) xlon=360+xlon

  IF (xlon > 360.0 .OR. xlon < 0.0) THEN
  
     PRINT*, 'landelev: longitude input (xlon) out of range'

  END IF

  IF (ylat > 90.0 .OR. ylat < -90.0) THEN
     
     PRINT*, 'landelev: latitude input (ylat) out of range'

  END IF

  ! Here we want cyclic zonal conditions
  izp=1
  
  CALL llintsp(fed,slon1,slat1,dlon1,dlat1,iin,jjn,iin,jjn,fsp,xlon,          &
       ylat,izp,ierr)

  IF (xlon > 180.0) xlon=xlon-360

  IF (ierr /= 0) then

     PRINT*, 'landelev: error returned from llintsp, ierr = ',ierr

  END IF

  IF (fsp > 0 ) THEN
     
     iland = 0
     ielev = int(fsp)
     
  ELSE
     iland = 1
     ielev = 0   
 
  END IF

END SUBROUTINE landelev
