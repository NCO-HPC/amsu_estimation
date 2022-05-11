      program temp_ret
c
c     program reads gathers postion information from the COORDINATES file
c     reads raw brightness temperature data aquired via buffer data on the
c     NCEP IBM, creates a TIMES file, and temperature retreivals for each of 
c     the NOAA satellites (15,16,18).
c
c
c     INPUT:    COORDINATES - This file contains a sequential list of
c                             active storms their most current position
c                             and their position 12-h earlier along with
c                             heading, speed, intensity, and adeck storm
c                             number and basin information.
c               BUFFER DATA - Three files will be generated for each storm 
c                             in the COORDINATES file with the following
c                             naming convention:
c                             noaa15.out
c                             noaa16.out
c                             noaa18.out
c               DUMP FILE -   This file contains specific information about
c                             the storm including the date time group,
c                             the geographic location, and the ATCF storm
c                             number.
c               COEFFICIENT FILES - Assorted files containing temperature
c                                   retrieval coefficents.
****************************************************************************
c
c     OUTPUT:       
c    Temperature Retreivals - Three files will be generated for each storm 
c                             in the COORDINATES file with the following
c                             naming convention:
c                             noaa15.ret
c                             noaa16.ret
c                             noaa18.ret
c    Log files for each
c                             noaa15.log
c                             noaa16.log
c                             noaa18.log
c
c
****************************************************************************
*
*
*     Written By   J. Knaff
*                  CIRA
*                  March 2003
*
*     Last Modified: December 13, 2005 
*
*     History:  
*     April 10, 2003,  changed the documentation at the top
*                      made modifications to M. Goldenbergs code
*                      land is now coded as 0  (damn wierd!!!)
*     December 13, 2005, J.Knaff replaced NOAA17 with NOAA18
*
****************************************************************************
      parameter(ip=10000,nup=30)
      integer idate15(ip),itime15(ip),isat15(ip),isen15(ip),
     .     iscan15(ip),ielev15(ip),isatz15(ip),ls15(ip)
      integer idate16(ip),itime16(ip),isat16(ip),isen16(ip),
     .     iscan16(ip),ielev16(ip),isatz16(ip),ls16(ip)
      integer idate18(ip),itime18(ip),isat18(ip),isen18(ip),
     .     iscan18(ip),ielev18(ip),isatz18(ip),ls18(ip)
      integer times(2),dates(2)
      integer inum,iy,im,id,i,dir0,spd0,vtmax,numatcf,vtmax12,irmw
      integer iy2,im2,id2,hh2,mm2,ss2,jd2,iy1,im1,id1,hh1,mm1,ss1
      integer sattime,satdate,satjday,idt,imt,iyt,ijult
c
      real rlat15(ip),rlon15(ip),satzen15(ip),solzen15(ip),tb15(ip,15),
     .     temp15(ip,40),tpw15(ip),clw15(ip)
      real rlat16(ip),rlon16(ip),satzen16(ip),solzen16(ip),tb16(ip,15),
     .     temp16(ip,40),tpw16(ip),clw16(ip)
      real rlat18(ip),rlon18(ip),satzen18(ip),solzen18(ip),tb18(ip,15),
     .     temp18(ip,40),tpw18(ip),clw18(ip)     
      real lats(2),lons(2)
      real closest,strmlatc,strmlonc,strmlat,strmlon,
     .     satclat1,satclon1,satclat2,satclon2,satclat,satclon,
     .     dist2cen,rad,tdiff,tpw,clw
      real bl(15),dummy(15),tret(40),bt(15)
c
      character*6 stnum
      character*8 cdir
      character*10 stname
      character*12 dumpfile
      character*11 coorfile
      character*2 cbasin
      character*10 cdata15, cdata16, cdata18
      character*10 coutp15, coutp16, coutp18
      character*10 clogf15, clogf16, clogf18
c
      logical open15,open16,open18,opendump
c
c     open dump files, one for each entry in the COORDINATES file
c
      do i=1,10
         write(dumpfile,50)i
 50      format('dumpjb.in_',i2.2)
         opendump=.false.
         inquire(file=dumpfile,exist=opendump)
         if (opendump)then
            inumd=i
            open (1,file=dumpfile,status='old')
            read(1,51)idtg
 51         format(i10)
            read(1,*)istuff
            read(1,52)cdir
 52         format(a8)
            close (1)
         endif
      enddo
c     
c
c     open COORDINATE file
c
      coorfile='COORDINATES'
      open (10,file=coorfile,status='old')
c
c     read information from the coordinate files and loop through storms
c 
 1    read(10,30,err=888) inum,iy,im,id,it1,it2,times(2),lats(2),
     .     lons(2),lats(1),lons(1),dir0,spd0,irmw,vtmax,vtmax12,
     .     stnum,stname
 30      format(i2.2,1x,i4.4,2i2.2,1x,i4.4,i3.3,i2.2,4f7.1,1x,
     .     5(i3.3,1x),a6,1x,a10)
         if(inum.eq.inumd)goto 31
         goto 1
 31   continue
      close(10)
c
c     internal read to strip atcf information out of the coordinate files
c     which will be used to access the ascii buffer data.
c
      read(stnum,45)cbasin,numatcf
 45   format(a2,i2)
c
c
c     convert date to julian day
c
      ijul=md2jul(id,im,iy)
c
c     create dates(2)
c
      dates(2)=iy*10000+im*100+id
c
c     determine the t-12h position time and date
c
      times(2)=times(2)         ! nearest synoptic hour in this application
c
      times(1)=times(2)-12
      if (times(1).lt.0)then
         times(1)=24+times(1)
         ijult=ijul-1
         call jdayi(ijult,iyt,imt,idt)
         dates(1)=iyt*10000+imt*100+idt
      else
         dates(1)=iy*10000+im*100+id
      endif
c
c     convert hrs into hhmmss
c
      times(1)=times(1)*10000
      times(2)=times(2)*10000
c
c     construct the datafile names for NOAA - 15, 16, 18 etc..
c
      cdata15='noaa15.out'
      cdata16='noaa16.out'
      cdata18='noaa18.out'
c
c     construct output files for this portion of the loop
c
      coutp15='noaa15.ret'
      coutp16='noaa16.ret'
      coutp18='noaa18.ret'
c
c     construct log file names for this date
c
      clogf15='noaa15.log'
      clogf16='noaa16.log'
      clogf18='noaa18.log'
c
c     open data(old),output(new),and log (new) files
c
      inquire(file=cdata15,exist=open15)
      if(open15)then
         open(15,file=cdata15,status='old')
         open(25,file=coutp15,status='replace')
         open(35,file=clogf15,status='replace')
      endif
      inquire(file=cdata16,exist=open16)
      if(open16)then
         open(16,file=cdata16,status='old')
         open(26,file=coutp16,status='replace')
         open(36,file=clogf16,status='replace')
      endif
      inquire(file=cdata18,exist=open18)
      if(open18)then
         open(18,file=cdata18,status='old')
         open(28,file=coutp18,status='replace')
         open(38,file=clogf18,status='replace')
      endif
c    
c     read data NOAA 15
c
      if(open15)then
         m=1
 115     read(15,75,end=915) idate15(m),itime15(m),rlat15(m),
     &        rlon15(m),isat15(m),isen15(m),iscan15(m),ls15(m),
     &        satzen15(m),solzen15(m),ielev15(m),isatz15(m),
     &        (tb15(m,n),n=1,15)
 75      format(i8,1x,i6,1x,2(f8.3,1x),1x,i2,2x,i3,2x,i2,2x,i1,2x,
     &        f8.3,1x,f8.3,1x,i5,2x,i6,2x,15(f6.2,2x))
         m=m+1
         goto 115
c
 915     continue
         if (m.eq.1)then 
            write(35,835) cdata15,coutp15
 835        format('Error encountered when reading file ',a10,/,
     &           'The file ',a10,' will be contain COORDINATE and ',
     &           'TIMES data, but no data')
         endif
         write(35,935) cdata15,m-1
 935     format('data file ',a10,' read with ',i8,' data points')
         num15=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the storm
c     center.
c
c
         sattime=times(2)
         satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
c
         do i=1,num15
c 
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idate15(i)/10000
            im1=(idate15(i)-iy1*10000)/100
            id1=(idate15(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
            hh1=itime15(i)/10000
            mm1=(itime15(i)-hh1*10000)/100
            ss1=(itime15(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .           (ss2-ss1)/3600.
c
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c
c     calculate distance from strom center to this point at the point relative
c     time.
            call distk(strmlon,strmlat,rlon15(i),rlat15(i),dx,dy,rad)
c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itime15(i)
               satdate=idate15(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
            endif
         enddo
         write(35,945)strmlatc,strmlonc,satdate,sattime,closest
 945     format('storm center located is  at ',2f8.2,' at ',2i10,/,
     .        'The closest point to this center is ',f8.1,' km')
c
c     one the satellite relative storm center is found, find the center of 
c     the amsu swath that corresponds to the time of the observation.
c
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
         do i=1,num15
            if (itime15(i).eq.sattime)then
               if (iscan15(i).eq.15)then
                  satclat1=rlat15(i)
                  satclon1=rlon15(i)
               endif
               if (iscan15(i).eq.16)then
                  satclat2=rlat15(i)
                  satclon2=rlon15(i)
               endif
            endif
         enddo
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(35,955)satclat,satclon,sattime,dist2cen
 955     format('satellite center is located at ',2f8.2,' at ',i6,/,
     .        'This point is ',f8.1,' km from the closest point')
c
c     writing coordinate and times information to output file and log file
c
         write(35,965)
 965     format('writing COORDINATE and TIMES information')
         write(25, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(25,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
 975     format(i2.2,1x,i4.4,i3.3,1x,i6.6,4f9.2)
         write(25,985)num15
 985     format(i6)
c     
c     call temperature retrieval
c    
         if (num15.ne.0) write(35,995)
 995     format('starting NOAA 15 temperature retieval')
c
         do i=1, num15
            iflag=0
            do j=1,15
               dummy(j)=tb15(i,j)
               if (j.ne.11.and.j.ne.14.and.dummy(j).lt.0.0)iflag=1
            enddo
            call amsua_ret_n15(nup,dummy,bt,iscan15(i),ls15(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                  temp15(i,j)=tret(j)
               else
                  temp15(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpw15(i)=tpw
               clw15(i)=clw
            else
               tpw15(i)=-99.00
               clw15(i)=-99.00
            endif
c
c     write NOAA 15 ascii data with temperature retreivals, total 
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(25,1575) idate15(i),itime15(i),rlat15(i),
     &           rlon15(i),isat15(i),isen15(i),iscan15(i),ls15(i),
     &           satzen15(i),solzen15(i),ielev15(i),isatz15(i),
     &           (tb15(i,n),n=1,15),(temp15(i,n),n=1,40),tpw15(i),
     &           clw15(i)
 1575       format(i8,1x,i6,1x,2(f8.3,1x),1x,i2,2x,i3,2x,i2,2x,i1,2x,
     &           f8.3,1x,f8.3,1x,i5,2x,i6,2x,57(f6.2,2x))
         enddo
c
c     close NOAA 15 output file for coordinate file inum 
         if (num15.ne.0)write(35,990)
 990     format('Normal completion of NOAA 15 temperature Retrieval')
 1015    continue
         call flush(15)
         close(15)
         call flush(25)
         close(25)
         call flush(35)
         close(35)
      endif

c    
c     read data NOAA 16
c
      if(open16)then
         m=1
 116     read(16,75,end=916) idate16(m),itime16(m),rlat16(m),
     &        rlon16(m),isat16(m),isen16(m),iscan16(m),ls16(m),
     &        satzen16(m),solzen16(m),ielev16(m),isatz16(m),
     &        (tb16(m,n),n=1,15)
         m=m+1
         goto 116
 916     continue
         if(m.eq.1) then
            write(36,835) cdata16,coutp16
         endif
         write(36,935) cdata16,m-1
         num16=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the storm
c     center.
c
c
         sattime=times(2)
         satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
c
         do i=1,num16
c 
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idate16(i)/10000
            im1=(idate16(i)-iy1*10000)/100
            id1=(idate16(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
            hh1=itime16(i)/10000
            mm1=(itime16(i)-hh1*10000)/100
            ss1=(itime16(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .           (ss2-ss1)/3600.
c
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c
c     calculate distance from strom center to this point at the point relative
c     time.
            call distk(strmlon,strmlat,rlon16(i),rlat16(i),dx,dy,rad)
c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itime16(i)
               satdate=idate16(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
            endif
         enddo
         write(36,945)strmlatc,strmlonc,satdate,sattime,closest
c
c     one the satellite relative storm center is found, find the center of 
c     the amsu swath that corresponds to the time of the observation.
c
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
         do i=1,num16
            if (itime16(i).eq.sattime)then
               if (iscan16(i).eq.16)then
                  satclat1=rlat16(i)
                  satclon1=rlon16(i)
               endif
               if (iscan16(i).eq.16)then
                  satclat2=rlat16(i)
                  satclon2=rlon16(i)
               endif
            endif
         enddo
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(36,955)satclat,satclon,sattime,dist2cen
c
c     writing COORDINATE, and timesinformation to output file and log file
c
         write(36,965)
         write(26, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(26,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
         write(26,985)num16
c
c     call temperature retrieval
c    
         if(num16.ne.0)write(36,996)
 996     format('starting NOAA 16 temperature retieval')
         do i=1, num16
            iflag=0
            do j=1,15
               dummy(j)=tb16(i,j)
               if (dummy(j).lt.0.0)iflag=1
            enddo
            call amsua_ret_n16(nup,dummy,bt,iscan16(i),ls16(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                  temp16(i,j)=tret(j)
               else
                  temp16(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpw16(i)=tpw
               clw16(i)=clw
            else
               tpw16(i)=-99.00
               clw16(i)=-99.00
            endif
c
c     write NOAA 16 ascii data with temperature retreivals, total 
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(26,1575) idate16(i),itime16(i),rlat16(i),
     &           rlon16(i),isat16(i),isen16(i),iscan16(i),ls16(i),
     &           satzen16(i),solzen16(i),ielev16(i),isatz16(i),
     &           (tb16(i,n),n=1,15),(temp16(i,n),n=1,40),tpw16(i),
     &           clw16(i)
         enddo
c
c     close NOAA 16 output file for coordinate file inum 
         if(num16.ne.0)write(36,991)
 991     format('Normal completion of NOAA 16 temperature Retrieval')
 1016    continue
         call flush(16)
         close(16)
         call flush(26)
         close(26)
         call flush(36)
         close(36)
      endif
c    
c     read data NOAA 18
c
      if(open18)then
         m=1
 118     read(18,75,end=918) idate18(m),itime18(m),rlat18(m),
     &        rlon18(m),isat18(m),isen18(m),iscan18(m),ls18(m),
     &        satzen18(m),solzen18(m),ielev18(m),isatz18(m),
     &        (tb18(m,n),n=1,15)
         m=m+1
         goto 118
 918     continue
         if(m.eq.1) then
            write(38,835) cdata18,coutp18
         endif   
         write(38,935) cdata18,m-1
         num18=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the storm
c     center.
c
c
c
         sattime=times(2)
         satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
c
         do i=1,num18
c 
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idate18(i)/10000
            im1=(idate18(i)-iy1*10000)/100
            id1=(idate18(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
            hh1=itime18(i)/10000
            mm1=(itime18(i)-hh1*10000)/100
            ss1=(itime18(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .              (ss2-ss1)/3600.
c
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c     
c     calculate distance from strom center to this point at the point relative
c     time.
            call distk(strmlon,strmlat,rlon18(i),rlat18(i),dx,dy,rad)
c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itime18(i)
               satdate=idate18(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
            endif
         enddo
         write(38,945)strmlatc,strmlonc,satdate,sattime,closest
c
c     one the satellite relative storm center is found, find the center of 
c     the amsu swath that corresponds to the time of the observation.
c     
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
         do i=1,num18
            if (itime18(i).eq.sattime)then
               if (iscan18(i).eq.18)then
                  satclat1=rlat18(i)
                  satclon1=rlon18(i)
               endif
               if (iscan18(i).eq.18)then
                  satclat2=rlat18(i)
                  satclon2=rlon18(i)
               endif
            endif
         enddo
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(38,955)satclat,satclon,sattime,dist2cen
c     
c     writing COORDINATE, TIMES, number of points information to output file
c
         write(38,965)
         write(28, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(28,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
         write(28,985)num18
c
c     call temperature retrieval
c    
         if(num18.ne.0)write(38,997)
 997     format('starting NOAA 18 temperature retieval')
         do i=1, num18
            iflag=0
            do j=1,15
               dummy(j)=tb18(i,j)
               if (j.ne.7.and.dummy(j).lt.0.0)iflag=1
            enddo
            call amsua_ret_n18(nup,dummy,bt,iscan18(i),ls18(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                  temp18(i,j)=tret(j)
               else
                  temp18(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpw18(i)=tpw
               clw18(i)=clw
            else
               tpw18(i)=-99.00
               clw18(i)=-99.00
            endif
c
c     write NOAA 18 ascii data with temperature retreivals, total 
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(28,1575) idate18(i),itime18(i),rlat18(i),
     &           rlon18(i),isat18(i),isen18(i),iscan18(i),ls18(i),
     &           satzen18(i),solzen18(i),ielev18(i),isatz18(i),
     &           (tb18(i,n),n=1,15),(temp18(i,n),n=1,40),tpw18(i),
     &           clw18(i)
         enddo
c
c     close NOAA 18 output file for coordinate file inum 
         if(num18.ne.0)write(38,992)
 992     format('Normal completion of NOAA 18 temperature Retrieval')
 1018    continue
         call flush(18)
         close(18)
         call flush(28)
         close(28)
         call flush(38)
         close(38)
      endif
      goto 999
 888  open (28,file='ret.log')
      write(28,889)coorfile
      close(28)
 889  format('END OF ',a22,' REACHED PREMATURELY EXICUTION STOPPED')
 999  stop
      end



