      integer function md2jul(iday,month,iyear) 
  
c      this routine calculates the julian date from 
c      the year, month, and day.
  
          i n t e g e r 
     + nodays(12) 
  
          d a t a 
     + nodays/31,28,31,30,31,30,31,31,30,31,30,31/
  
      nodays(2)=28
      if(iyear.eq.0)go to 1 
      if(mod(iyear,4) .eq. 0) nodays(2)=29
      md2jul=0
    1 do 2 i=1,month
    2 md2jul = md2jul + nodays(i) 
      idayout = nodays(month)-iday
      md2jul = md2jul - idayout 
  
      return
      end
