opis.grupy.badawczej<<-function(ramka,nr.zm.int=NULL,nr.zm.porz=NULL,lista.podzialu, do.nazwy.pliku="",czy.usuwac.outliery=FALSE,czy.rysowac=FALSE,zakres.outlierow=1.5,nr.zm.nom=NULL){
   #nr.zm.int i nr.zm.porz to wektory z numerami kolumn ramki, lista.podzialu to lista, zawierajaca wektory
   #kazdy wektor listy zawiera indeksy podgrupy, a nazwa wektora to nazwa tej podgrupy
  # nr.zm.nom to numery zmiennych nominalnych, dla nich s¹ tabele z licznoœciami i wykresy ko³owe

      if((length(nr.zm.int)+length(nr.zm.porz))==0)
      {
         nr.zm.int=c(1:ncol(ramka))
      }
      if(czy.usuwac.outliery)
      {
         co.z.outlierami="outl us"
         # mainDir=paste(mainDir,"/outliery_usuniete",sep="")
      } else
      {
         # mainDir=paste(mainDir,"/outliery_nieusuniete",sep="")
         co.z.outlierami="outl nieus"
      }
   outl=co.z.outlierami

   mainDir<-getwd()
   subDir<-"histogramy"
   subDir2<-"ko³owe"
   if(czy.rysowac)
   {
      ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
   }
   if(czy.rysowac)
   {
     ifelse(!dir.exists(file.path(mainDir, subDir2)), dir.create(file.path(mainDir, subDir2)), FALSE)
   }
      suppressPackageStartupMessages(library("xlsx"))
      suppressPackageStartupMessages(library("e1071"))
      suppressPackageStartupMessages(library("randomcoloR"))
      suppressPackageStartupMessages(library("RColorBrewer"))

      # -------------------------------------------------------------------------------------
      #                      SELEKCJA ZMIENNYCH
      # -------------------------------------------------------------------------------------
      selekcja=names(lista.podzialu)


      interwalowe <- nr.zm.int
      porzadkowe <- nr.zm.porz

      numeryczne=sort(union(interwalowe,porzadkowe))

      ile.wierszy.w.tab.wynikow <- length(numeryczne)
      ile.wierszy.w.tab.wynikow.interw <- length(interwalowe)
      ile.wierszy.w.tab.wynikow.porz <- length(porzadkowe)
      powt=rep(NA,ile.wierszy.w.tab.wynikow*length(selekcja))
      powt.interw=rep(NA,ile.wierszy.w.tab.wynikow.interw*length(selekcja))
      powt.porz=rep(NA,ile.wierszy.w.tab.wynikow.porz*length(selekcja))
      tabela.wynikow <- data.frame(n=powt,srednia=powt,odch.std=powt,min.Q0=powt,Q1=powt,mediana.Q2=powt,
                                   Q3=powt,max.Q4=powt,rozstep.kwart=powt,wsp.zmiennosci=powt,wsp.asymetrii=powt,kurtoza=powt,p.Shapiro.Wilka=powt)
      tabela.wynikow.interw <- data.frame(n=powt.interw,srednia=powt.interw,odch.std=powt.interw,min.Q0=powt.interw,Q1=powt.interw,mediana.Q2=powt.interw,
                                          Q3=powt.interw,max.Q4=powt.interw,rozstep.kwart=powt.interw,wsp.zmiennosci=powt.interw,wsp.asymetrii=powt.interw,kurtoza=powt.interw,p.Shapiro.Wilka=powt.interw)
      tabela.wynikow.porz <- data.frame(n=powt.porz,min.Q0=powt.porz,Q1=powt.porz,mediana.Q2=powt.porz,
                                        Q3=powt.porz,max.Q4=powt.porz)
      row.names(tabela.wynikow)=paste(rep(names(ramka)[numeryczne],each=length(selekcja)),rep(selekcja,times=length(numeryczne)),sep=" - ")
      row.names(tabela.wynikow.interw)=paste(rep(names(ramka)[interwalowe],each=length(selekcja)),rep(selekcja,times=length(interwalowe)),sep=" - ")
      row.names(tabela.wynikow.porz)=paste(rep(names(ramka)[porzadkowe],each=length(selekcja)),rep(selekcja,times=length(porzadkowe)),sep=" - ")



      # -------------------------------------------------------------------------------------
      #               USTAWIENIA ZMIENNYCH DLA KAZDEJ PODGRUPY
      # -------------------------------------------------------------------------------------
      for (l in seq(lista.podzialu)) # dla kazdej z podgrup / kazdego elementu listy
      {     indeksy=lista.podzialu[[l]]
            przesuniecie.wiersza=l

     # -------------------------- STATYSTYKI OPISOWE ----------------------------

         licznik.interw=1
         licznik.porz=1

            for (i in seq(numeryczne)) # dla kazdej zmiennej interwalowej lub porzadkowej, ale nie po ich indeksach
            {
               zm1=ramka[indeksy,numeryczne[i]]
  

               # ------------- UZUPELNIENIE TABELI --------------------------------------

               if (is.element(numeryczne[i],interwalowe))
               {

                  ile.elementow.na.poczatku1<-length(zm1[!is.na(zm1)])
                  if (ile.elementow.na.poczatku1>0)
                  {
                     if(czy.usuwac.outliery)
                     {
                        indeksy.outlierow1=which(is.element(zm1,boxplot(zm1,range = zakres.outlierow)$out))
                        # print(indeksy.outlierow1)
                        # print(zm1[indeksy.outlierow1])
                        if (length(indeksy.outlierow1)!=0)
                        {
                           zm1=zm1[-indeksy.outlierow1]
                        }

                     }
                  }

                  #n
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,1]=length(zm1[!is.na(zm1)])
                  # mean, sd
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,2]=round(mean(as.numeric(zm1),na.rm=TRUE),2)
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,3]=round(sd(as.numeric(zm1),na.rm=TRUE),2)
                  # kwartyle
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,4]=round(quantile(as.numeric(zm1),na.rm=TRUE)[1],2)
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,5]=round(quantile(as.numeric(zm1),na.rm=TRUE)[2],2)
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,6]=round(quantile(as.numeric(zm1),na.rm=TRUE)[3],2)
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,7]=round(quantile(as.numeric(zm1),na.rm=TRUE)[4],2)
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,8]=round(quantile(as.numeric(zm1),na.rm=TRUE)[5],2)
                  # rozstep kwartylowy
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,9]=round(IQR(as.numeric(zm1),na.rm=TRUE)/2,2)
                  # wsp. zmiennosci, wsp. asymetrii, kurtoza
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,10]=round(sd(as.numeric(zm1),na.rm=TRUE)/mean(as.numeric(zm1),na.rm=TRUE),2)
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,11]=round(e1071::skewness(as.numeric(zm1),na.rm=TRUE),2)
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,12]=round(e1071::kurtosis(as.numeric(zm1),na.rm=TRUE),2)
                  # test normalosci
                  if(length(zm1[!is.na(zm1)])>4 & sd(zm1,na.rm = TRUE)>0)
                  {
                     tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,13]=round(shapiro.test(as.numeric(zm1))$p.value,3)
                  } else
                  {
                     tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,13]=NA
                  }
                  #n
                  tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,1]=length(zm1[!is.na(zm1)])
                  # mean, sd
                  tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,2]=round(mean(as.numeric(zm1),na.rm=TRUE),2)
                  tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,3]=round(sd(as.numeric(zm1),na.rm=TRUE),2)
                  # kwartyle
                  tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,4:8]=round(quantile(as.numeric(zm1),na.rm=TRUE),2)
                  # rozstep kwartylowy
                  tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,9]=round(IQR(as.numeric(zm1),na.rm=TRUE)/2,2)
                  # wsp. zmiennosci, wsp. asymetrii, kurtoza
                  tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,10]=round(sd(as.numeric(zm1),na.rm=TRUE)/mean(as.numeric(zm1),na.rm=TRUE),2)
                  tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,11]=round(e1071::skewness(as.numeric(zm1),na.rm=TRUE),2)
                  tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,12]=round(e1071::kurtosis(as.numeric(zm1),na.rm=TRUE),2)
                  if(length(zm1[!is.na(zm1)])>4 & sd(zm1,na.rm = TRUE)>0)
                  {
                     tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,13]=round(shapiro.test(as.numeric(zm1))$p.value,3)
                  } else
                  {
                     tabela.wynikow.interw[(licznik.interw-1)*length(selekcja)+przesuniecie.wiersza,13]=NA
                  }

                  licznik.interw=licznik.interw+1
               }

               if (is.element(numeryczne[i],porzadkowe))
               {
                  zmienna=zm1[!is.na(zm1)]

                  poziomy=levels(zmienna)

                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,1]=length(zmienna)
                  tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,4:8]=poziomy[round(0.0001+quantile(as.numeric(zmienna),na.rm=TRUE))]
                  # tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,4]=round(quantile(as.numeric(zm1),na.rm=TRUE)[1],2)
                  # tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,5]=round(quantile(as.numeric(zm1),na.rm=TRUE)[2],2)
                  # tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,6]=round(quantile(as.numeric(zm1),na.rm=TRUE)[3],2)
                  # tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,7]=round(quantile(as.numeric(zm1),na.rm=TRUE)[4],2)
                  # tabela.wynikow[(i-1)*length(selekcja)+przesuniecie.wiersza,8]=round(quantile(as.numeric(zm1),na.rm=TRUE)[5],2)
                  tabela.wynikow.porz[(licznik.porz-1)*length(selekcja)+przesuniecie.wiersza,1]=length(zmienna)
                  tabela.wynikow.porz[(licznik.porz-1)*length(selekcja)+przesuniecie.wiersza,2:6]=poziomy[round(0.0001+quantile(as.numeric(zmienna),na.rm=TRUE))]

                  licznik.porz=licznik.porz+1
               }

               # ------------------------------- HISTOGRAMY -------------------------------------------------------------------

               if(czy.rysowac){
                  while (names(dev.cur()) != "null device"){
                     dev.off()
                  }
                  # Draw the plot
                  rozmiar.tytulu=0.5
                  igreki="Liczba wystapien"
                  iksy=names(ramka)[numeryczne[i]]
                  indeksy.outlierow1=which(is.element(zm1,boxplot(zm1,range=zakres.outlierow)$out))
                  podaj.outliery=zm1[indeksy.outlierow1]
                  #print(length(podaj.outliery))
                  tytul=paste("Histogram ",iksy, ",\n grupa ",names(lista.podzialu)[l])
                  # if(length(podaj.outliery)>0)
                  #    tytul=paste(tytul,"\n outliery: ",paste(as.character(podaj.outliery),collapse=", "))

                  nazwa=paste(subDir,"/histogram ",iksy," grupa ",names(lista.podzialu)[l], ".png",sep="")

                  if (file.exists(nazwa)){
                     file.remove(nazwa)
                  }
                  png(filename=nazwa,
                      type="cairo",
                      units="in",
                      width=8,
                      height=5,
                      pointsize=12,
                      res=600)
                  if (is.element(numeryczne[i],interwalowe))
                  {
                     hist(zm1,col=randomColor(),border="black",cex.axis=1,main=tytul,ylab=igreki,xlab=iksy,cex.main=1)
                  }
                  if (is.element(numeryczne[i],porzadkowe))
                  {
                     barplot(table(zm1),col=randomColor(),border="black",cex.axis=1,main=tytul,ylab=igreki,xlab=iksy,cex.main=1)
                  }
                  if (names(dev.cur()) != "null device") dev.off()
               } # koniec  if(czy.rysowac)

            } # koniec petli (i in seq(numeryczne))

      } # koniec petli for(l in seq(lista.podzialu))

      # ------------- ZAPISANIE DO XLSX --------------------------------------

         wb = createWorkbook()

         sheet2 = createSheet(wb, "interwalowe")

         addDataFrame(tabela.wynikow.interw, sheet=sheet2)

         sheet3 = createSheet(wb, "porzadkowe")

         addDataFrame(tabela.wynikow.porz, sheet=sheet3)

         sheet4 = createSheet(wb, "interwalowe i porzadkowe")

         addDataFrame(tabela.wynikow, sheet=sheet4)
        if(do.nazwy.pliku!="")
        {
          do.nazwy.pliku=paste(" ", do.nazwy.pliku,sep="")
        }
         saveWorkbook(wb, paste("opis_grupy_badawczej",do.nazwy.pliku," ",outl,".xlsx",sep=""))

##################         
         # Teraz czêœæ poœwiêcona zmiennych nominalnym
         nominalne <- nr.zm.nom
         if(!is.null(nominalne))
         {
           ile.tabel=length(nominalne) #ile bedzie tabel kontyngencji
           wb = createWorkbook()
           sheet1 = createSheet(wb, "nominalne")
           licznik.wierszy.nom=2
           for (l in seq(lista.podzialu)) # dla kazdej z podgrup / kazdego elementu listy
           {
             indeksy=lista.podzialu[[l]]
             przesuniecie.wiersza=l
             
             # -------------------------- STATYSTYKI OPISOWE ----------------------------
             
             licznik.nom=1
            # sprintf("%1.2f%%", 100*m)
             
             for (i in seq(nominalne)) # dla kazdej zmiennej nominalnej, ale nie po ich indeksach
             {
               zm1=ramka[indeksy,nominalne[i]]
               n.poziom=length(levels(zm1))

               # ------------------------------- WYKRESY KO£OWE  -------------------------------------------------------------------
               
               if(czy.rysowac){
                
                 kolory=brewer.pal(n =n.poziom, name = "Spectral")
                 while (names(dev.cur()) != "null device"){
                   dev.off()
                 }
                 # Draw the plot
                 rozmiar.tytulu=0.5
                 tytul=paste(names(ramka)[nominalne[i]],"\n grupa ",names(lista.podzialu)[l])
                  
                 nazwa=paste(subDir2,"/ ",names(ramka)[nominalne[i]]," grupa ",names(lista.podzialu)[l],".png",sep="")
                 
                 if (file.exists(nazwa)){
                   file.remove(nazwa)
                 }
                 png(filename=nazwa,
                     type="cairo",
                     units="in",
                     width=8,
                     height=5,
                     pointsize=12,
                     res=600)
                x <-summary(zm1)
                procenty <- format(100*x/sum(x),digits=2)
                 pie(x,labels=paste(names(x),'\n',procenty,"%"),
                     col=kolory, main=tytul)
                
                 if (names(dev.cur()) != "null device") dev.off()
               } # koniec  if(czy.rysowac)
               
               
               # -------------- TABELKI DO XLSX -----------------------------------------------
               # 
               # if(i==1) # przy pierwszym ,,obrocie" dodaj nazwy tabel, co czym jest
               # {
                 nazwa.tab.1=paste("LICZNOSC (dla grupy ", names(lista.podzialu)[l],")",sep="")
                 nazwa.tab.2=paste("UDZIAL PROCENTOWY (dla grupy ", names(lista.podzialu)[l],")",sep="")
                 
                 tab=table(ramka[indeksy,nominalne[i]])
                # print(tab)
                 tab.m=matrix(tab,nrow=1)
                 colnames(tab.m)=names(tab)
                 ramka.dodawana=as.data.frame(tab.m)
                 
                 addDataFrame(as.data.frame(nazwa.tab.1),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 2,row.names = F,col.names = F)
                 addDataFrame(as.data.frame(nazwa.tab.2),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 1*(n.poziom+4)+2,row.names = F,col.names = F)

                 licznik.wierszy.nom=licznik.wierszy.nom+2
                 # nazwa czynnika w kolumnach (pierwsza tabela z licznosciami, tabela kontyngencji)
                 addDataFrame(as.data.frame(names(ramka)[nominalne[i]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 2,col.names = F,row.names = F)
                 # 
                 
                 # # nazwa czynnika w kolumnach (druga tabela, z udzialem procentowym)
                 addDataFrame(as.data.frame(names(ramka)[nominalne[i]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = n.poziom+6,col.names = F,row.names = F)
                 # 

                 oo <- as.data.frame(format(tab.m*100/sum(tab.m),digits=1))
                 oo <- sapply(oo,paste,"%")
                 oo <- matrix(oo,nrow=1)
                 colnames(oo)=names(tab)
                 oo=as.data.frame(oo)
                 
                 # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn
                addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn=2, col.names = T,row.names = F)
                 
                 # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn, ale chodzi nam tylko o ,,brzeg'', reszta zostanie nadpisana
                 addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = n.poziom+6,col.names = T,row.names = F)
                 
                 # zawartosc tabeli udzialow procentowych, tylko srodek, bez nazw wierszy i kolumn
                   addDataFrame(oo,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = n.poziom+6,col.names = F,row.names = F)

                 
               # } else # przy kolejnych tabelach lec po staremu
               # {
               #   tab=table(ramka[,nominalne[i]])
               #   tab.m=matrix(tab,nrow=1)
               #   colnames(tab.m)=names(tab)
               #   ramka.dodawana=as.data.frame(tab.m)
               #   
               #   addDataFrame(as.data.frame(nazwa.tab.1),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 2,row.names = F,col.names = F)
               #   addDataFrame(as.data.frame(nazwa.tab.2),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 1*(n.poziom+4)+2,row.names = F,col.names = F)
               #   
               #   licznik.wierszy.nom=licznik.wierszy.nom+2
               #   # nazwa czynnika w kolumnach (pierwsza tabela z licznosciami, tabela kontyngencji)
               #   addDataFrame(as.data.frame(names(ramka)[nominalne[i]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 2,col.names = F,row.names = F)
               #   # 
               #   
               #   # # nazwa czynnika w kolumnach (druga tabela, z udzialem procentowym)
               #   addDataFrame(as.data.frame(names(ramka)[nominalne[i]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = n.poziom+6,col.names = F,row.names = F)
               #   # 
               #   
               #   oo <- as.data.frame(format(tab.m*100/sum(tab.m),digits=1))
               #   oo <- sapply(oo,paste,"%")
               #   oo <- matrix(oo,nrow=1)
               #   colnames(oo)=names(tab)
               #   oo=as.data.frame(oo)
               #   
               #   # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn
               #   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn=2, col.names = T,row.names = F)
               #   
               #   # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn, ale chodzi nam tylko o ,,brzeg'', reszta zostanie nadpisana
               #   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = n.poziom+6,col.names = T,row.names = F)
               #   
               #   # zawartosc tabeli udzialow procentowych, tylko srodek, bez nazw wierszy i kolumn
               #   addDataFrame(oo,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = n.poziom+6,col.names = F,row.names = F)
               #   
               # }
               licznik.wierszy.nom <- licznik.wierszy.nom +nrow(ramka.dodawana)+7
               
               
             } # koniec for (i in seq(nominalne))
           } # koniec petli for(l in seq(lista.podzialu))
           

           saveWorkbook(wb, paste(mainDir,"/opis grupy badawczej tabele",do.nazwy.pliku,".xlsx",sep=""))
           
           
           } # if(!is.null(nominalne))
           
  
 } # koniec deklaracji funkcji





