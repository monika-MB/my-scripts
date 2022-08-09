sila.zaleznosci<<-function(wartosc,wsp="r"){
   
   r1=abs(wartosc)
   if(wsp=="r")
   {
      if(r1<0.1){magnitude="pomijalnie maly"}
      if(r1>=0.1 & r1<0.2){magnitude="maly"}
      if(r1>=0.2 & r1<0.4){magnitude="sredni"}
      if(r1>=0.4 & r1<0.6){magnitude="stosunkowo duzy"}
      if(r1>=0.6 & r1<0.8){magnitude="duzy"}
      if(r1>=0.8){magnitude="bardzo duzy"}
   }
   if(wsp=="eta_kwadrat")
   {
      if(r1<0.01){magnitude="pomijalnie maly"}
      if(r1>=0.01 & r1<0.06){magnitude="maly"}
      if(r1>=0.06 & r1<0.14){magnitude="sredni"}
      if(r1>=0.14){magnitude="duzy"}
   }


   return(magnitude)
}