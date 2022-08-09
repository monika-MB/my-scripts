grupowane.do.ramki.z.macierza<<-function(ramka.danych,zmienne.interwalowe,zmienne.grupujace){
# przekazywana jest ramka, wektor numerow kolumn - zmiennych, ktore nalezy rozbic oraz wektor numerow kolumn - zmiennych grupujacych
 #    a=list(ramka.rozdzielona=ramka.rozdzielona, macierz.po.rozdziale=macierz.po.rozdziale)

   dane=ramka.danych
   library("rowr")

   for (i in zmienne.grupujace)
   {
      ramka.danych[,i]=factor(ramka.danych[,i])
   }

   # 1 Sprawdzenie, czy dla kazdej zmiennej grupujacej jest ta sama liczba poziomow

   ile.grupujacych=length(zmienne.grupujace)
   liczba.poziomow=c(NULL)

   for (g in c(1:ile.grupujacych))
   {
      poziomy=levels(ramka.danych[,zmienne.grupujace[g]])
      liczba.poziomow[g]=length(poziomy)
   } # koniec for (g in c(1:ile.grupujacych))

   if(min(liczba.poziomow)!=max(liczba.poziomow))
   {
      stop("Nie wszystkie zmienne grupujace maja te sama liczbe poziomow!")
   }

   ile.poziomow=liczba.poziomow[1]

   # 2 Jesli tak, to kazda zmienna interwalowa (petla zewnetrzna) zostaje rozbita wedlug kazdej zmiennej grupujacej (petla wewnetrzna)

      ile.interwalowych=length(zmienne.interwalowe)
   nazwy.kolumn.ramki=c(NULL)

   for (i in c(1:ile.interwalowych))
   {
      for (g in c(1:ile.grupujacych))
      {
         poziomy=levels(ramka.danych[,zmienne.grupujace[g]])
         for (p in c(1:ile.poziomow))
         {
            nazwy.kolumn.ramki [(i-1)*ile.grupujacych*ile.poziomow+(g-1)*ile.poziomow+p] = paste(names(dane)[zmienne.interwalowe[i]],names(dane)[zmienne.grupujace[g]],poziomy[p])
            w=data.frame(x=dane[which(dane[,zmienne.grupujace[g]]==poziomy[p]),zmienne.interwalowe[i]])
            # print(summary(w))

            if(i==1 & g==1 & p==1)
            {
             ramka.rozdzielona=w
            } else {
               r1=nrow(ramka.rozdzielona)-nrow(w)
               r2=nrow(w)-nrow(ramka.rozdzielona)
               if(r1>0)
               {
                  w=rbind(w,data.frame(x=rep(NA,r1)))
               }
               if(r2>0)
               {
                  temp=matrix(NA,ncol=ncol(ramka.rozdzielona),nrow=r2,dimnames=list(NULL,names(ramka.rozdzielona)))
                  ramka.rozdzielona=rbind(ramka.rozdzielona,as.data.frame(temp))
               }
               ramka.rozdzielona=cbind(ramka.rozdzielona,w)

            } # koniec else if(i==1 & g==1 & p==1)

         } # koniec for (p in c(1:ile.poziomow))
      } # koniec for (g in c(1:ile.grupujacych))
   } # koniec for (i in c(1:ile.interwalowych))


   # 3 Zadbanie o nazwy kolumn

   names(ramka.rozdzielona)=nazwy.kolumn.ramki
   # print(summary(ramka.rozdzielona))

   # 4 Zrobienie macierzy

   macierz.po.rozdziale=matrix(c(1:(ile.grupujacych*ile.interwalowych*ile.poziomow)),ncol=ile.poziomow,byrow=TRUE)



   # 5 Wypisanie wynikow

   a=list(ramka.rozdzielona=ramka.rozdzielona, macierz.po.rozdziale=macierz.po.rozdziale)

   }
