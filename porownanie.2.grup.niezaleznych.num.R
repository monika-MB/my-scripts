porownanie.2.grup.niezaleznych.num<<-function(zmienne.numeryczne,zmienne.grupujace,nazwy.zm.grupujacych=NULL,czy.usuwac.outliery=FALSE,do.nazwy.pliku="",czy.rysowac.skrzypcowe=TRUE,czy.rysowac.pudelkowe=FALSE){
# zmienne.numeryczne to wektor,ramka lub macierz, ktorej kolumny to poszczegolne zmienne, musi zawierac zmienne numeryczne
  # zmienne.grupujace to wektor lub macierz lub ramka zmiennych grupujacych, grupa musi zawierac zmienne czynnikowe

  liczba.poziomow=2
  opcja1=TRUE

  # nie ruszac, inicjalizacja
  outl=FALSE

  oldw <- getOption("warn")
  options(warn = -1)
  suppressPackageStartupMessages(library("vioplot"))
  suppressPackageStartupMessages(library("xlsx"))
  suppressPackageStartupMessages(library("car"))
  suppressPackageStartupMessages(library("icesTAF"))
  suppressPackageStartupMessages(library("grDevices"))
  suppressPackageStartupMessages(library("randomcoloR"))
  suppressPackageStartupMessages(library("rstatix"))
  suppressPackageStartupMessages(library("rcompanion"))
  mainDir<-getwd()
  subDir<-"wykresy zmienne numeryczne"

  if(czy.rysowac.skrzypcowe | czy.rysowac.pudelkowe)
  {
    ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
  }

  if(is.null(nazwy.zm.grupujacych))
  {
    nazwy.zm.grupujacych=names(zmienne.grupujace)

  }

  if(!is.data.frame(zmienne.grupujace))
  {
    zmienne.grupujace<-as.data.frame(zmienne.grupujace)
  }

  if(!is.data.frame(zmienne.numeryczne))
  {
    zmienne.numeryczne<-as.data.frame(zmienne.numeryczne)
  }

  if(opcja1) #nowa
  {

    wb = createWorkbook()
    paleta=rev(palette(heat.colors(3)))
    # normalna czcionka, wysrodkowany, cienka ramka do srodka/prawa
    cs1 <- CellStyle(wb) + Font(wb) + Border(position = c("BOTTOM","RIGHT"),pen="BORDER_THIN") + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")#
    # bold, wysrodkowany, gruba ramka, kolorowe tlo
    cs2 <- CellStyle(wb) + Fill(foregroundColor = "lightskyblue") +Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
    # bold, wysrodkowany, gruba ramka, tekst obrocony pionowo
    cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER",rotation=90)+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
    # bold, wysrodkowany, gruba ramka
    cs4 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")



    # kolumny0<-c("Grupa")
    kolumny1<-c("N","Srednia","Odch. std","Mediana","Odch. cw", "Min","Q1","Q3","Max")
    kolumny4<-c("p Shapiro-Wilka","test dyspersji","p testu dysp.","test polozenia","p testu polozenia + efekt","p Levene'a","p Manna-Whitneya + efekt")
    #kolumny<-c(kolumny0,kolumny1,kolumny4)
    kolumny=c(kolumny1,kolumny4)

    arkusz = createSheet(wb, "interwalowe")

    for(j in c(1:ncol(zmienne.grupujace))) # wykonaj dla kazdej zmiennej grupujacej
    {
      print(paste("zm grupujaca nr ",j))
      poziomy=levels(zmienne.grupujace[,j])
      liczba.poziomow=length(poziomy)

      if(liczba.poziomow>2)
      {
        stop("liczba poziomow zmiennej grupujacej jest wieksza niz 2")
      }

      if(liczba.poziomow<2)
      {
        stop("liczba poziomow zmiennej grupujacej jest mniejsza niz 2")
      }

      pacjenci<-vector("list",liczba.poziomow)

      for(i in c(1:liczba.poziomow))
      {
        #przypisac do zmiennej pacjenci.i odpowiedni wektor indeksow
        assign(paste("pacjenci",as.character(i),sep=""),which(zmienne.grupujace[,j]==poziomy[i]))

        #przypisac wektor indeksow pacjentow z grupy i do i-tej pozycji na liscie pacjenci
        # pacjenci to lista, podczas gdy pacjenci1, pacjenci2 to wektory
        pacjenci[[i]]=get(paste("pacjenci",as.character(i),sep=""))
      }

      for (k in c(1:ncol(zmienne.numeryczne))) # dla kazdej zmiennej numerycznej

      {
        print(paste("zm num nr ",k))
        liczba.wierszy=liczba.poziomow
        wiersze<-poziomy

        wyniki=matrix(NA,nrow=liczba.wierszy,ncol=length(kolumny),dimnames=list(wiersze,kolumny))
        wyniki<-as.data.frame(wyniki)


        zmienna1=zmienne.numeryczne[,k][pacjenci[[1]]]
        zmienna2=zmienne.numeryczne[,k][pacjenci[[2]]]

        #DALEJ TYLKO JAK JEST NA CZYM PRACOWAC
        if(length(which(!is.na(zmienna1)==TRUE))>2 &length(which(!is.na(zmienna2)==TRUE))>2 )
        {#print(paste("1: ",length(which(!is.na(zmienna1)))))
          # print(paste("2: ",length(which(!is.na(zmienna2)))))

        # USUWANIE OUTLIEROW

        if(czy.usuwac.outliery)
        {
          ekstremalne<-vector("list",liczba.poziomow)
          ekstremalne[[1]]=boxplot(x=zmienna1,plot=FALSE)$out
          ekstremalne[[2]]=boxplot(x=zmienna2,plot=FALSE)$out

          while(length(ekstremalne[[1]])>0)
          {
            zmienna1[is.element(zmienna1,ekstremalne[[1]])]=NA
            outl=TRUE
            ekstremalne[[1]]=boxplot(x=zmienna1,plot=FALSE)$out
          }
          while(length(ekstremalne[[2]])>0)
          {
            zmienna2[is.element(zmienna2,ekstremalne[[2]])]=NA
            outl=TRUE
            ekstremalne[[2]]=boxplot(x=zmienna2,plot=FALSE)$out
          }

        }
          #ZNOWU DALEJ TYLKO JAK JEST NA CZYM PRACOWAC
          if(length(which(!is.na(zmienna1)==TRUE))>2 &length(which(!is.na(zmienna2)==TRUE))>2 )
          {#print(paste("1: ",length(which(!is.na(zmienna1)))))
            # print(paste("2: ",length(which(!is.na(zmienna2)))))

        zmienne=list(zmienna1,zmienna2)
        zmienne.do.wykr=list(zmienna1,zmienna2)

        for(i in c(1:liczba.poziomow))
        {
          wiersz=i
          kolumna=-2

          # ------------- UZUPELNIENIE TABEL STATYSTYKAMI OPISOWYMI   -------
          wyniki[wiersz,kolumna+3]=length(zmienne[[i]][!is.na(as.numeric(zmienne[[i]]))])
          wyniki[wiersz,kolumna+4]=round(mean(as.numeric(zmienne[[i]]),na.rm=TRUE),2)
          wyniki[wiersz,kolumna+5]=round(sd(as.numeric(zmienne[[i]]),na.rm=TRUE),2)
          wyniki[wiersz,kolumna+6]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[3],2)
          wyniki[wiersz,kolumna+7]=round(IQR(zmienne[[i]],na.rm = TRUE)/2,2)

          wyniki[wiersz,kolumna+8]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[1],2)
          wyniki[wiersz,kolumna+9]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[2],2)
          wyniki[wiersz,kolumna+10]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[4],2)
          wyniki[wiersz,kolumna+11]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[5],2)
        } # koniec for i in liczba.poziomow

        n1=length(zmienna1[!is.na(as.numeric(zmienna1))])
        n2=length(zmienna2[!is.na(as.numeric(zmienna2))])

        ramka1=data.frame(zmienna=zmienna1)
        ramka1$nazwa=as.character(1)
        ramka2=data.frame(zmienna=zmienna2)
        ramka2$nazwa=as.character(2)
        ramka.grupujaca<-rbind(ramka1,ramka2) #ramka ma teraz dwie kolumny: 1) zmienna 2) nazwa

        # -------- UZUPELNIENIE TABELI TESTEM I WYNIKIEM TEGO TESTU
        if (n1<3 | n2<3 | var(zmienna1,na.rm=T)==0 | var(zmienna2, na.rm=T)==0)
        {
          sh1=data.frame(p.value=0)
          sh2=data.frame(p.value=0)
        } else
        {
          sh1=shapiro.test(zmienna1)
          sh2=shapiro.test(zmienna2)
        }

        sh=round(c(sh1$p.value,sh2$p.value),4)

        if(sh1$p.value>=0.05 & sh2$p.value>=0.05 ){ # jesli rozklady normalne, to idziemy w testy parametryczne
          

          nazwa.testu.dyspersji="test F"
          p.value.testu.dyspersji=round(var.test(zmienna1,zmienna2)$p.value,4)
          if (var.test(zmienna1,zmienna2)$p.value>=0.05 & chisq.test(c(n1,n2))$p.value>=0.05) #rowne wariancje
          {# do tego musza byc rownoliczne - test zgodnosci chi-kwadrat
            nazwa.testu.polozenia="t-Studenta"
            p.value.testu.polozenia=round(t.test(zmienna1,zmienna2,var.equal = TRUE, paired=FALSE)$p.value,4)
            p.value.testu.polozenia.uzup=p.value.testu.polozenia
            if(p.value.testu.polozenia<=0.05)
            {
              efekt=cohens_d(formula=zmienna~nazwa,data=ramka.grupujaca)
              we=round(efekt$effsize,2)
              magn=efekt$magnitude
              p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", d Cohena = ",we,", wielkosc = ",magn,sep="")
            }
          } else{ # rozne wariancje lub proby o roznej licznosci

            nazwa.testu.polozenia="Welcha"
            p.value.testu.polozenia=round(t.test(zmienna1,zmienna2,var.equal = FALSE, paired=FALSE)$p.value,4)
            p.value.testu.polozenia.uzup=p.value.testu.polozenia
            if(p.value.testu.polozenia<=0.05)
            {
              efekt=cohens_d(formula=zmienna~nazwa,data=ramka.grupujaca)
              we=round(efekt$effsize,2)
              magn=efekt$magnitude
              p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", d Cohena = ",we,", wielkosc = ",magn,sep="")
            }
          }

        } else {  # brak normalnosci rozkladow, jedziemy testy nieparametryczne, tak samo jesli nierownoliczne

          nazwa.testu.dyspersji="Ansari-Bradley"

          p.value.testu.dyspersji=round(ansari.test(zmienna1,zmienna2)$p.value,4)
# 
          # if(ansari.test(zmienna1,zmienna2)$p.value>=0.05) # rowne dyspersje
          # {
            nazwa.testu.polozenia="U Manna-Whitneya"
            p.value.testu.polozenia=round(wilcox.test(zmienna1,zmienna2,paired=FALSE)$p.value,4)
            p.value.testu.polozenia.uzup=p.value.testu.polozenia

            if(is.na(p.value.testu.polozenia)){
              p.value.testu.polozenia=1
            }
            if(p.value.testu.polozenia<=0.05)
            { # rg Rangowy wspolczynnik korelacji dwuseryjnej Glassa
              we=round(wilcoxonRG(x=ramka.grupujaca$zmienna[complete.cases(ramka.grupujaca)],g=ramka.grupujaca$nazwa[complete.cases(ramka.grupujaca)]),2)
              magn=sila.zaleznosci(we)
              p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", r Glassa = ",we,", wielkosc = ",magn,sep="")
            }

          # } else
          # {
          #   nazwa.testu.polozenia="Kolmogorowa-Smirnowa"
          #   p.value.testu.polozenia=round(ks.test(zmienna1,zmienna2)$p.value,4)
          #   p.value.testu.polozenia.uzup=p.value.testu.polozenia
          #   if(p.value.testu.polozenia<=0.05)
          #   {
          #     we=round(ks.test(zmienna1,zmienna2)$statistic,2)
          #     magn=sila.zaleznosci(we)
          #     p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", KS = ",we,", wielkosc = ",magn,sep="")
          #   }
          # }

        }

        LT<-leveneTest(ramka.grupujaca$zmienna,ramka.grupujaca$nazwa,center="mean")
        p.value.testu.Levenea=round(LT[[3]][1],4)
        p.value.testu.UManna=round(wilcox.test(zmienna1,zmienna2,paired=FALSE)$p.value,4)
        if(is.na(p.value.testu.UManna)){
          p.value.testu.UManna=1
        }
        p.value.testu.UManna.uzup=p.value.testu.UManna

        if(p.value.testu.UManna<=0.05)
        { # rg Rangowy wspolczynnik korelacji dwuseryjnej Glassa
           we=round(wilcoxonRG(x=ramka.grupujaca$zmienna[complete.cases(ramka.grupujaca)],g=ramka.grupujaca$nazwa[complete.cases(ramka.grupujaca)]),2)
           magn=sila.zaleznosci(we)
           p.value.testu.UManna.uzup=paste("p = ",p.value.testu.UManna,", r Glassa = ",we,", wielkosc = ",magn,sep="")
        }

        for(i in c(1:liczba.poziomow))
        {
          wiersz=i
        # uzupelnienie czesci z testami statystycznymi
        wyniki[wiersz,length(wyniki[1,])-6]=sh[i] # p.value testu Shapiro-Wilka
        }

        # ------------------------------- ZAPIS DO XLSX -------------------------------------------------------------------

        odstep=2
        dod=(j-1)*(ncol(zmienne.numeryczne)*(liczba.poziomow+2+odstep))
        rows<-createRow(sheet=arkusz, rowIndex = c( (1+ (k-1)*(liczba.poziomow+2+odstep)+dod) : (k*(liczba.poziomow+2+odstep)+  dod ) ))
        setRowHeight(rows,30)
        setColumnWidth(sheet=arkusz, colIndex = c(1:(length(kolumny)+2)), 16)
        setColumnWidth(sheet=arkusz, colIndex = c(7:11,13,14,17), 0)
        setColumnWidth(sheet=arkusz, colIndex = 1, 7)

        lista.kolumn.do.cs1=rep(list(cs1),length(kolumny))
        names(lista.kolumn.do.cs1) =c(1:length(kolumny))

        wiersz.startowy=4+ (k-1)*(liczba.poziomow+2+odstep)+dod
        addDataFrame(wyniki, sheet=arkusz, startColumn=2, row.names=TRUE,rownamesStyle = cs4,colnamesStyle = cs4,startRow = wiersz.startowy,colStyle = lista.kolumn.do.cs1)

        # dodanie naglowkow do nazw wierszy i kolumn tabel
        addMergedRegion(sheet=arkusz,startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),endRow = (k*(liczba.poziomow+2+odstep)+dod),startColumn=1,endColumn = 1)
        addMergedRegion(sheet=arkusz,startRow = (odstep+1+(k-1)*(liczba.poziomow+2+odstep)+dod),endRow = (odstep+1+(k-1)*(liczba.poziomow+2+odstep)+dod),startColumn=3,endColumn = (2+length(kolumny)))
        df.nazwa.cechy.w.wierszach=data.frame(x1=rep(nazwy.zm.grupujacych[j],times=liczba.poziomow))
        df.nazwa.cechy.w.kolumnach=data.frame(x1=rep(names(zmienne.numeryczne)[k],times=length(kolumny)))
        addDataFrame(df.nazwa.cechy.w.wierszach$x1, sheet=arkusz, startColumn = 1, startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs3))
        addDataFrame(df.nazwa.cechy.w.kolumnach$x1, sheet=arkusz, startColumn = 3, startRow = (odstep+1+(k-1)*(liczba.poziomow+2+odstep)+dod),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs4),byrow=TRUE)

        addMergedRegion(sheet=arkusz,startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),endRow = (k*(liczba.poziomow+2+odstep)+dod),startColumn =(length(wyniki[1,])-5+2) ,endColumn = (length(wyniki[1,])-5+2))
        df.nazwa.testu.dyspersji=data.frame(x1=rep(nazwa.testu.dyspersji,times=liczba.poziomow)) # nazwa testu dyspersji
        addDataFrame(df.nazwa.testu.dyspersji$x1, sheet=arkusz, startColumn = (length(wyniki[1,])-5+2), startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

        addMergedRegion(sheet=arkusz,startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),endRow = (k*(liczba.poziomow+2+odstep)+dod),startColumn =(length(wyniki[1,])-4+2) ,endColumn = (length(wyniki[1,])-4+2))
        df.p.testu.dyspersji=data.frame(x1=rep(p.value.testu.dyspersji,times=liczba.poziomow)) # p.value testu dyspersji
        addDataFrame(df.p.testu.dyspersji$x1, sheet=arkusz, startColumn = (length(wyniki[1,])-4+2), startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

        addMergedRegion(sheet=arkusz,startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),endRow = (k*(liczba.poziomow+2+odstep)+dod),startColumn =(length(wyniki[1,])-3+2) ,endColumn = (length(wyniki[1,])-3+2))
        df.nazwa.testu.polozenia=data.frame(x1=rep(nazwa.testu.polozenia,times=liczba.poziomow)) # nazwa testu polozenia
        addDataFrame(df.nazwa.testu.polozenia$x1, sheet=arkusz, startColumn = (length(wyniki[1,])-3+2), startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

        addMergedRegion(sheet=arkusz,startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),endRow = (k*(liczba.poziomow+2+odstep)+dod),startColumn =(length(wyniki[1,])-2+2) ,endColumn = (length(wyniki[1,])-2+2))
        df.p.testu.polozenia=data.frame(x1=rep(p.value.testu.polozenia.uzup,times=liczba.poziomow)) # p.value testu polozenia
        addDataFrame(df.p.testu.polozenia$x1, sheet=arkusz, startColumn = (length(wyniki[1,])-2+2), startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

        addMergedRegion(sheet=arkusz,startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),endRow = (k*(liczba.poziomow+2+odstep)+dod),startColumn =(length(wyniki[1,])-1+2) ,endColumn = (length(wyniki[1,])-1+2))
        df.p.Levenea=data.frame(x1=rep(p.value.testu.Levenea,times=liczba.poziomow)) # p.value testu Levene'a
        addDataFrame(df.p.Levenea$x1, sheet=arkusz, startColumn = (length(wyniki[1,])-1+2), startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

        addMergedRegion(sheet=arkusz,startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),endRow = (k*(liczba.poziomow+2+odstep)+dod),startColumn =(length(wyniki[1,])+2) ,endColumn = (length(wyniki[1,])+2))
        df.p.UManna=data.frame(x1=rep(p.value.testu.UManna.uzup,times=liczba.poziomow)) # p.value testu U Manna-Whitneya
        addDataFrame(df.p.UManna$x1, sheet=arkusz, startColumn = (length(wyniki[1,])+2), startRow = (3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

        # --------------------- KOLOROWANIE P<0,05 ----------------------------------

        rows  <- getRows(arkusz)
        cells <- getCells(rows)
        values <- lapply(cells, getCellValue)

        if(p.value.testu.polozenia<=0.05 & p.value.testu.polozenia>0.01 )
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),".",length(wyniki[1,]),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[1]) +Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(k*(liczba.poziomow+2+odstep)+dod),".", length(wyniki[1,]),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[1])+Font(wb, isBold=TRUE))",sep="")))
          } # koniec if(p.value.testu.polozenia<=0.05 & p.value.testu.polozenia>0.01 )
        if(p.value.testu.polozenia<=0.01 & p.value.testu.polozenia>0.001 )
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),".", length(wyniki[1,]),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[2])+Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(k*(liczba.poziomow+2+odstep)+dod),".",length(wyniki[1,]),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[2])+Font(wb, isBold=TRUE))",sep="")))
        } # koniec if(p.value.testu.polozenia<=0.01 & p.value.testu.polozenia>0.001 )
        if(p.value.testu.polozenia<=0.001)
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),".",length(wyniki[1,]),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[3])+Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(k*(liczba.poziomow+2+odstep)+dod),".", length(wyniki[1,]),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[3])+Font(wb, isBold=TRUE))",sep="")))
        } # koniec if(p.value.testu.polozenia<=0.001)



        if(p.value.testu.UManna<=0.05 & p.value.testu.UManna>0.01 )
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),".",(length(wyniki[1,])+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[1]) +Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(k*(liczba.poziomow+2+odstep)+dod),".", (length(wyniki[1,])+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[1])+Font(wb, isBold=TRUE))",sep="")))
        } # koniec if(p.value.testu.polozenia<=0.05 & p.value.testu.polozenia>0.01 )
        if(p.value.testu.UManna<=0.01 & p.value.testu.UManna>0.001 )
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),".", (length(wyniki[1,])+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[2])+Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(k*(liczba.poziomow+2+odstep)+dod),".",(length(wyniki[1,])+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[2])+Font(wb, isBold=TRUE))",sep="")))
        } # koniec if(p.value.testu.polozenia<=0.01 & p.value.testu.polozenia>0.001 )
        if(p.value.testu.UManna<=0.001)
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(3+odstep+ (k-1)*(liczba.poziomow+2+odstep)+dod),".",(length(wyniki[1,])+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[3])+Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(k*(liczba.poziomow+2+odstep)+dod),".", (length(wyniki[1,])+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[3])+Font(wb, isBold=TRUE))",sep="")))
        } # koniec if(p.value.testu.polozenia<=0.001)

        # ------------------------------- WYKRESY SKRZYPCOWE -------------------------------------------------------------------

        if(czy.rysowac.skrzypcowe){
          if(p.value.testu.polozenia<0.05){
            # Draw the plot
            rozmiar.tytulu=0.5
            iksy=nazwy.zm.grupujacych[j]
            igreki=names(zmienne.numeryczne)[k]
            tytul=paste(igreki, ",\n podzial wzgledem ",iksy,"\n outl us.=",czy.usuwac.outliery )
            
            nazwa=paste(subDir,"/" ,igreki," wzgledem ",iksy," ",do.nazwy.pliku," outl us.=",czy.usuwac.outliery,".png",sep="")
            
            
            if (file.exists(nazwa)){
              file.remove(nazwa)
            }
            png(filename=nazwa,
                type="cairo",
                units="in",
                width=5,
                height=5,
                pointsize=12,
                res=600)
            vioplot( zmienna1[!is.na(zmienna1)] , zmienna2[!is.na(zmienna2)],  col=randomColor() , names=poziomy)
            mtext(paste("Zmienne to: ",paste(poziomy,collapse=", "),sep=""),3,cex = 0.5)
            
            title(main=tytul,ylab=igreki,xlab=iksy,cex.main=rozmiar.tytulu)
            if (names(dev.cur()) != "null device") dev.off()
          }
          
        } # koniec  if(czy.rysowac.skrzypcowe)

        # ------------------------------- WYKRESY PUDELKOWE-------------------------------------------------------------------

        if(czy.rysowac.pudelkowe){
          if(p.value.testu.polozenia<0.05){
            # Draw the plot
            rozmiar.tytulu=0.5
            iksy=nazwy.zm.grupujacych[j]
            igreki=names(zmienne.numeryczne)[k]
            tytul=paste(igreki, ",\n podzial wzgledem ",iksy,"\n outl us.=",czy.usuwac.outliery )
            
            nazwa=paste(subDir,"/" ,igreki," wzgledem ",iksy," ",do.nazwy.pliku," outl us.=",czy.usuwac.outliery," bp.png",sep="")
            
            
            if (file.exists(nazwa)){
              file.remove(nazwa)
            }
            png(filename=nazwa,
                type="cairo",
                units="in",
                width=5,
                height=5,
                pointsize=12,
                res=600)
            
            boundaries<-boxplot(zmienne.do.wykr,plot=FALSE)
            najmn_y=round(min(min(boundaries$stats[1,])*0.9,min(boundaries$stats[1,]-1)))
            najw_y=round(max(max(boundaries$stats[1,])*1.1,max(boundaries$stats[nrow(boundaries$stats),]+1))*1.1)
            boxplot(zmienne.do.wykr,col=randomColor() , names=poziomy,border="black",las=2,cex.axis=0.7,ylim=c(najmn_y,najw_y), par(mar=c(c(6.1, 4.1, 4.1, 2.1))))
            if(najw_y<1)
            {
              text(x=c(1:liczba.poziomow), y=boundaries$stats[nrow(boundaries$stats),]*1.1, paste("n = ",wyniki[,1],sep="") )
            } else
            {
              igreki2=boundaries$stats[nrow(boundaries$stats),]*1.1
              igreki2[igreki2<2]=2
              text(x=c(1:liczba.poziomow), y=igreki2, paste("n = ",wyniki[,1],sep="") )
            }
            
            # boxplot( zmienne.do.wykr,  col=randomColor(),border="black" , names=poziomy,las=1)
            #mtext(paste("Zmienne to: ",paste(poziomy,collapse=", "),sep=""),3,cex = 0.5)
            
            title(main=tytul,ylab=igreki,xlab=iksy,cex.main=rozmiar.tytulu)
            if (names(dev.cur()) != "null device") dev.off()
          }
          
        } # koniec  if(czy.rysowac.pudelkowe)

        }} # 2 x koniec  if(length(zmienna1>2) & length(zmienna2)>2)
      } # koniec: dla kazdej zmiennej numerycznej


    } # koniec: for j dla kazdej zmiennej grupujacej

    addMergedRegion(sheet=arkusz,startRow = 1,endRow = 1,startColumn=1,endColumn =6)
    nazwa.cechy=data.frame(x1=rep("Porownania 2 grup",times=6))
    addDataFrame(nazwa.cechy$x1, sheet=arkusz, startColumn = 1, startRow = 1,row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs2),byrow=TRUE)


    if(czy.usuwac.outliery)
    {
      outl.tekst="us"
    } else
    {
      outl.tekst="nieus"
    }

    saveWorkbook(wb, paste("wyniki por 2 grup niezal ",do.nazwy.pliku," outl ",outl.tekst ,".xlsx",sep=""))

    if(outl)
    {
      print("USUNIETO OUTLIERY! Jesli tego nie chcesz, ustaw zmienna czy.usuwac.outliery na FALSE")
    }

  } # koniec if(opcja1) (nowa)

}


