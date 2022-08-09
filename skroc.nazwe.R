skroc.nazwe<<-function(wiersze){
library(tidyverse)
   rozbite=strsplit(wiersze,NULL)
   l=0 # licznik, tylko pierwsza litera
   flaga=TRUE
   while(flaga)
   {
      if(length(unique(sapply(rozbite,function(x) x[l+1])))==1) # we wszystkich napisach l+1-sze litery sa takie same
      {l=l+1} else
      { flaga=FALSE }
   }
   # l to teraz numer ostatniego takiego samego znaku we wszystkich napisach

   wiersze_skrocone=sapply(rozbite, function(x) substring(paste(x,collapse=""),first=(l+1)))
   poziomy=wiersze_skrocone
   poczatek=substring(paste(rozbite[[1]],collapse=""),first=(1),last = l)
   wiersze_skrocone[1]=wiersze[1]

   podzial=str_split(poczatek, " ")[[1]]

   nazwa_y=podzial[1]
   nazwa_x=podzial[2]

   wynik=list(wiersze_skrocone=wiersze_skrocone,poziomy=poziomy,nazwa_x=nazwa_x,nazwa_y=nazwa_y)
 }