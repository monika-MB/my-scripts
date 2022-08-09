porownanie.wielu.grup.zaleznych.num<<-function(ramka.danych,macierz.zmiennych.do.porownania,czy.usuwac.outliery=FALSE,do.nazwy.pliku="",czy.rysowac.skrzypcowe=TRUE,czy.rysowac.pudelkowe=FALSE,rysuj.roznice=FALSE){
  # ramka.danych to data.frame, ktorego kolumny to zmienne numeryczne do porownan
  # macierz.zmiennych.do.porownania to matrix o n kolumnach, w kazdym wierszu para oznaczajaca numery kolumn do porownania

   opcja1=TRUE
  # nie ruszac, inicjalizacja zmiennej
  outl=FALSE
  options(contrasts=c("contr.sum","contr.poly"))
  liczba.poziomow=ncol(macierz.zmiennych.do.porownania)
  r=ramka.danych # skrocenie nazwy
  m=macierz.zmiennych.do.porownania # skrocenie notacji

  inform<-Sys.info()  # inform[7] to user
  uzytkownik=inform[7]
  sciezka.skrypty=paste("/Users/",uzytkownik,"/Dysk Google/SkryptyR",sep="")

  # Windows
  if(.Platform$OS.type=="windows")
  {
    if (substr(getwd(),1,1)!="C")
    {
      sciezka.skrypty=paste(substr(getwd(),1,2),"/Dysk Google/SkryptyR",sep="")
    } else
    {
      sciezka.skrypty=paste(substr(getwd(),1,2),sciezka.skrypty,sep="")
    } # koniec if (substr(getwd(),1,1)!="C")
  # } else { # czyli praca na Mac
  #   sciezka.skrypty=paste("/Users/",uzytkownik,"/Mój Dysk/SkryptyR",sep="")
  } # koniec if(.Platform$OS.type=="windows")
  source(paste(sciezka.skrypty,"/friedman.test.with.post.hoc.R",sep=""))

  suppressPackageStartupMessages(library("vioplot"))
  suppressPackageStartupMessages(library("xlsx"))
  suppressPackageStartupMessages(library("icesTAF"))
  suppressPackageStartupMessages(library("car"))
  library("grDevices")
  library("randomcoloR")
  library("rstatix")
  library("rcompanion")
  mainDir<-getwd()
  subDir<-"wykresy zmienne numeryczne"
  if(czy.rysowac.skrzypcowe | czy.rysowac.pudelkowe)
  {
    ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
  }


  if(opcja1) #nowa
  {
    wb = createWorkbook()
    arkusz = createSheet(wb, "Zmienne powiazane")


    paleta=rev(palette(heat.colors(3)))
    # normalna czcionka, wy?rodkowany, cienka ramka do?/prawa
    cs1 <- CellStyle(wb) + Font(wb) + Border(position = c("BOTTOM","RIGHT"),pen="BORDER_THIN") + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")#
    # bold, wy?rodkowany, gruba ramka, kolorowe t?o
    cs2 <- CellStyle(wb) + Fill(foregroundColor = "lightskyblue") +Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
    # bold, wy?rodkowany, gruba ramka, tekst obr?cony pionowo
    cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER",rotation=90)+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
    # bold, wy?rodkowany, gruba ramka
    cs4 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
    # normalna czcionka, wy?rodkowany, cienka ramka d??/prawa, szara czcionka
    cs5 <- CellStyle(wb) + Font(wb,color="gray60") + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("BOTTOM","RIGHT"),pen="BORDER_THIN")


    zbior.grup<-as.character(NULL)
    wsad<-as.character("n1")
    wsad.wykres<-as.character(NULL)

    for (i in c(1:liczba.poziomow))
    {
      if(i==1)
      {
        wsad.wykres<-paste(wsad.wykres,"zmienne[[",as.character(i),"]]",sep="")
      }
      if (i>1)
      {
        wsad<-paste(wsad,",","n",as.character(i),sep="")
        wsad.wykres<-paste(wsad.wykres,",zmienne[[",as.character(i),"]]",sep="")
      }

    }

    kolumny1<-c("N","Srednia","Odch. std","Mediana","Odch. cw", "Min","Q1","Q3","Max")
    kolumny4<-c("p Shapiro-Wilka","Test porownan wielokrotnych","p testu por. wielokrotnych + efekt","Test post-hoc","Zmienna")
    kolumny=c(kolumny1,kolumny4)

    ile.testow.post.hoc=liczba.poziomow*(liczba.poziomow-1)/2  #ile testow post-hoc
    wsad.wykres.roznice=as.character(NULL)
    kolumny5=as.character(NULL)
    for (i in c(1:(liczba.poziomow-1)))
    {
      for (j in c((i+1):liczba.poziomow))
      {

        if(i==1 & j==2)
        {
          wsad.wykres.roznice=paste(wsad.wykres.roznice,"zmienne[[",as.character(i),"]]-zmienne[[",as.character(j),"]]",sep="")

        } else
        {
          wsad.wykres.roznice=paste(wsad.wykres.roznice,",zmienne[[",as.character(i),"]]-zmienne[[",as.character(j),"]]",sep="")

        }
        kolumny5=c(kolumny5,paste("p-value",as.character(i)," vs ", as.character(j)))

      }
    }

    liczba.tabelek=nrow(m)

    odstep=2
    rows<-createRow(sheet=arkusz, rowIndex = c(1:(1+liczba.tabelek*(ncol(m)+1+odstep))))
    setRowHeight(rows,30)
    setColumnWidth(sheet=arkusz, colIndex = c(1:(length(kolumny)+2+liczba.poziomow)), 16)
    setColumnWidth(sheet=arkusz, colIndex = c(7:11), 0)
    setColumnWidth(sheet=arkusz, colIndex = 1, 7)
    setColumnWidth(sheet=arkusz, colIndex = 2, 30)

    lista.kolumn.do.cs1=rep(list(cs1),(length(kolumny)+liczba.poziomow))
    names(lista.kolumn.do.cs1) =c(1:(length(kolumny)+liczba.poziomow))

    for(j in c(1:liczba.tabelek))  # wykonaj dla kazdego zestawu 3 lub wiecej zmiennych
    {
      print(j)
      wiersze=names(r)[m[j,]]
      kolumny=c(kolumny1,kolumny4,wiersze)
      if(czy.rysowac.skrzypcowe | czy.rysowac.pudelkowe)
      {
        wiersze_skrocone=skroc.nazwe(wiersze)$wiersze_skrocone
        poziomy_do_wykresu=skroc.nazwe(wiersze)$poziomy
        nazwa_osi_x=skroc.nazwe(wiersze)$nazwa_x
        nazwa_osi_y=skroc.nazwe(wiersze)$nazwa_y
      }

      wyniki=matrix(NA,nrow=liczba.poziomow,ncol=length(kolumny),dimnames=list(wiersze,kolumny))
      wyniki<-as.data.frame(wyniki)

      nazwy.z.roznicami=as.character(NULL)

      for (i in c(1:(liczba.poziomow-1)))
      {
        for (k in c((i+1):liczba.poziomow))
        {
          nazwy.z.roznicami=c(nazwy.z.roznicami,paste(names(r)[m[j,i]],"-", names(r)[m[j,k]]))
        }
      }

      zmienne<-vector("list",liczba.poziomow)
      for (i in c(1:liczba.poziomow))
      {
        zmienne[[i]]=r[,m[j,i]]
      }

      # USUWANIE OUTLIEROW

      if(czy.usuwac.outliery)
      {
        ekstremalne<-vector("list",liczba.poziomow)
        for (i in c(1:liczba.poziomow))
        {
          ekstremalne[[i]]=boxplot(x=zmienne[[i]],plot=FALSE)$out
          while(length(ekstremalne[[i]])>0)
          {
            zmienne[[i]][is.element(zmienne[[i]],ekstremalne[[i]])]=NA
            outl=TRUE
            ekstremalne[[i]]=boxplot(x=zmienne[[i]],plot=FALSE)$out
          }
        }
      }

      grupowane.pre.pre<-data.frame(ramka.danych[complete.cases(ramka.danych[,macierz.zmiennych.do.porownania[j,]]),macierz.zmiennych.do.porownania[j,]])
      grupowane.pre=as.numeric(as.matrix(grupowane.pre.pre))
      bloki=factor(rep(c(1:nrow(grupowane.pre.pre)),times=liczba.poziomow))
      grupy=factor(rep(c(1:liczba.poziomow),each=nrow(grupowane.pre.pre)))
      grupowane=data.frame(grupowane.pre,grupy,bloki)

      # teraz jest ramka danych ,,grupowane'' z kolumnami jw.

      for(i in c(1:liczba.poziomow))
      {
        wiersz=i
        przesuniecie=0
        kolumna=1


        # ------------- UZUPELNIENIE TABEL STATYSTYKAMI OPISOWYMI   -------

        wyniki[wiersz,przesuniecie+kolumna+1]=round(mean(as.numeric(zmienne[[i]]),na.rm=TRUE),2)
        wyniki[wiersz,przesuniecie+kolumna+2]=round(sd(as.numeric(zmienne[[i]]),na.rm=TRUE),2)
        wyniki[wiersz,przesuniecie+kolumna+3]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[3],2)
        wyniki[wiersz,przesuniecie+kolumna+4]=round(IQR(zmienne[[i]],na.rm = TRUE)/2,2)

        wyniki[wiersz,przesuniecie+kolumna+5]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[1],2)
        wyniki[wiersz,przesuniecie+kolumna+6]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[2],2)
        wyniki[wiersz,przesuniecie+kolumna+7]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[4],2)
        wyniki[wiersz,przesuniecie+kolumna+8]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[5],2)
        assign(paste("n",as.character(i),sep=""),length(zmienne[[i]][!is.na(zmienne[[i]])]))

        eval(parse(text=paste("wyniki[wiersz,1]<-length(which(complete.cases(grupowane.pre.pre)==TRUE))",sep="")))
        wyniki[wiersz,length(kolumny1)+length(kolumny4)]=wiersze[i]
      }


      # -------- UZUPELNIENIE TABELI TESTEM I WYNIKIEM TEGO TESTU

      norm=TRUE


      for(i in c(1:liczba.poziomow))
      {
        if(sd(as.numeric(zmienne[[i]]),na.rm=TRUE)==0 | (length(zmienne[[i]][!is.na(zmienne[[i]])]))<3)
        {
          norm=FALSE
        } else
        {
          wyniki[i,10]=round(shapiro.test(zmienne[[i]])$p.value,4)
          if(shapiro.test(zmienne[[i]])$p.value<0.05)
          {
            norm=FALSE
          }
        }
      }

      if(norm) # jesli rozklady normalne, to idziemy w testy parametryczne
        # zalozenie o rownolicznosci jest spelnione z definicji
      {
        # sprawdzanie sferycznosci - test Mauchly'a
        S=as.matrix(ramka.danych[complete.cases(ramka.danych[,macierz.zmiennych.do.porownania[j,]]),macierz.zmiennych.do.porownania[j,]])
        tM=mauchly.test(lm(S~1),X=~1)
        p.value.testu.sferycznosci=tM$p.value
        if(is.na(p.value.testu.sferycznosci))
        {
          p.value.testu.sferycznosci=1
        }
        if(p.value.testu.sferycznosci>=0.05) # zachowana kulistosc, to test ANOVA dla pomiarow powtarzanych
        {
          nazwa.testu.polozenia="ANOVA pom powt"
          anova.model=aov(grupowane.pre~grupy+Error(bloki/grupy),data=grupowane)
          anova.pom.powt<-summary(anova.model)
          p.value.testu.polozenia=round(anova.pom.powt[[2]][[1]][5][1,1],4)
          p.value.testu.polozenia.uzup=p.value.testu.polozenia
          if(p.value.testu.polozenia<=0.05)
          {
            we=round(eta_squared(anova.model[[3]]),2)
            magn=sila.zaleznosci(we,wsp="eta_kwadrat")
            p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", eta-kwadrat = ",we,", wielkosc = ",magn,sep="")
          }

        } else # brak kulistosci, trzeba jechac ANOVA z poprawka GG Greenhouse-Geisser lub H-F Huynh--Feldt
        {
          nazwa.testu.polozenia="ANOVA pom powt popr H-F"
          anova.pom.powt.poprawka=anova(lm(S~ 1), X= ~ 1, test= "Spherical" )
          p.value.testu.polozenia=round(anova.pom.powt.poprawka[1,7],4)
          p.value.testu.polozenia.uzup=p.value.testu.polozenia
          if(p.value.testu.polozenia<=0.05)
          {
            anova.model=aov(grupowane.pre~grupy+Error(bloki/grupy),data=grupowane)
            we=round(eta_squared(anova.model[[3]]),2)
            magn=sila.zaleznosci(we,wsp="eta_kwadrat")
            p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", eta-kwadrat = ",we,", wielkosc = ",magn,sep="")
          }


        }

        if (p.value.testu.polozenia<=0.05)
        {
          nazwa.testu.post.hoc="Pairwise t-test z popr. Holma"
          post.hoc<-pairwise.t.test(x=grupowane[,1],g=grupowane[,2],paired=TRUE)
          macierz.post.hoc=post.hoc$p.value
          macierz.post.hoc[is.na(macierz.post.hoc)]= 1
          wektor.post.hoc=as.vector(macierz.post.hoc)
          wektor.post.hoc<-wektor.post.hoc[!is.na(wektor.post.hoc)]
          duza.macierz.post.hoc=matrix(NA,nrow=liczba.poziomow,ncol=liczba.poziomow)
          for(wier in c(1:liczba.poziomow))
          {
            for(kol in c(1:liczba.poziomow))
            {
              if(kol<wier)
              {
                duza.macierz.post.hoc[wier,kol]=macierz.post.hoc[(wier-1),kol]
              }
              if(kol>wier)
              {
                duza.macierz.post.hoc[wier,kol]=macierz.post.hoc[(kol-1),wier]
              }
            }
          }
        }

      } else {  # brak normalnosci rozkladow, jedziemy testy nieparametryczne

        # 5 grup lub wiecej - test Friedmana ma wieksza moc
        # dla 3 i 4 grup wieksza moc ma test Quade

        if (liczba.poziomow>=2)
        {
          nazwa.testu.polozenia="Friedmana"

          tF=friedman.test(as.matrix(ramka.danych[,macierz.zmiennych.do.porownania[j,]]))
          p.value.testu.polozenia=round(tF$p.value,4)
          p.value.testu.polozenia.uzup=p.value.testu.polozenia
          if(p.value.testu.polozenia<=0.05)
          {
            efekt=friedman_effsize(formula=grupowane.pre~grupy|bloki,data=grupowane)
            we=round(efekt$effsize,2)
            magn=efekt$magnitude
            p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", W = ",we,", wielkosc = ",magn,sep="")
          }
        } else
        {
          nazwa.testu.polozenia="Quade"
          tQ=quade.test(grupowane.pre~grupy+Error(bloki/grupy),data=grupowane)
          p.value.testu.polozenia=round(tQ$p.value,4)
          p.value.testu.polozenia.uzup=p.value.testu.polozenia
          if(p.value.testu.polozenia<=0.05)
          {
            anova.model=aov(grupowane.pre~grupy+Error(bloki/grupy),data=grupowane)
            # print("Quade")
            # print(anova.model)
            we=round(eta_squared(anova.model[[3]]),2)
            magn=sila.zaleznosci(we,wsp="eta_kwadrat")
            p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", eta-kwadrat = ",we,", wielkosc = ",magn,sep="")
          }

        }

        if (p.value.testu.polozenia<=0.05)
        {
          nazwa.testu.post.hoc="Pairwise Wilcoxon z popr. Holma"
          post.hoc<-pairwise.wilcox.test(x=grupowane[,1],g=grupowane[,2],paired=TRUE)
          macierz.post.hoc=post.hoc$p.value
          wektor.post.hoc=as.vector(macierz.post.hoc)
          wektor.post.hoc<-wektor.post.hoc[!is.na(wektor.post.hoc)]
          duza.macierz.post.hoc=matrix(NA,nrow=liczba.poziomow,ncol=liczba.poziomow)
          for(wier in c(1:liczba.poziomow))
          {
            for(kol in c(1:liczba.poziomow))
            {
              if(kol<wier)
              {
                duza.macierz.post.hoc[wier,kol]=macierz.post.hoc[(wier-1),kol]
              }
              if(kol>wier)
              {
                duza.macierz.post.hoc[wier,kol]=macierz.post.hoc[(kol-1),wier]
              }
            }
          }
        }

      }

      ile.kolumn.bez.post.hoc=length(kolumny1)+length(kolumny4)
      ile.kol1=length(kolumny1)

      if (p.value.testu.polozenia<=0.05){
        wyniki[,c((ile.kolumn.bez.post.hoc+1):(ile.kolumn.bez.post.hoc+liczba.poziomow))]=duza.macierz.post.hoc
      }


      wiersz.startowy=4+ (j-1)*(ncol(m)+1+odstep)
      addDataFrame(wyniki, sheet=arkusz, startColumn=2, row.names=TRUE,rownamesStyle = cs4,colnamesStyle = cs4,startRow = wiersz.startowy,colStyle = lista.kolumn.do.cs1)


      addMergedRegion(sheet=arkusz,startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),endRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+1+liczba.poziomow),startColumn =(2+1+ile.kol1+1) ,endColumn = (2+1+ile.kol1+1))
      df.nazwa.testu.polozenia=data.frame(x1=rep(nazwa.testu.polozenia,times=liczba.poziomow)) # p.value testu Studenta
      addDataFrame(df.nazwa.testu.polozenia$x1, sheet=arkusz, startColumn = (2+1+ile.kol1+1), startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

      p.value.testu.polozenia=round(p.value.testu.polozenia,4)
      addMergedRegion(sheet=arkusz,startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),endRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+1+liczba.poziomow),startColumn =(2+1+ile.kol1+2) ,endColumn = (2+1+ile.kol1+2))
      df.p.polozenia=data.frame(x1=rep(p.value.testu.polozenia.uzup,times=liczba.poziomow)) # p.value testu Wilcoxona
      addDataFrame(df.p.polozenia$x1, sheet=arkusz, startColumn = (2+1+ile.kol1+2), startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

      rows  <- getRows(arkusz)
      cells <- getCells(rows)
      values <- lapply(cells, getCellValue)

      if(p.value.testu.polozenia<=0.05 & p.value.testu.polozenia>0.01 )
      {
        for(poz in c(1:liczba.poziomow))
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+1+poz),".",(2+1+ile.kol1+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[1]) +Font(wb, isBold=TRUE))",sep="")))
        }
      } # koniec if(p.value.testu.polozenia<=0.05 & p.value.testu.polozenia>0.01 )
      if(p.value.testu.polozenia<=0.01 & p.value.testu.polozenia>0.001 )
      {
        for(poz in c(1:liczba.poziomow))
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+1+poz),".",(2+1+ile.kol1+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[2]) +Font(wb, isBold=TRUE))",sep="")))
        }
      } # koniec if(p.value.testu.polozenia<=0.01 & p.value.testu.polozenia>0.001 )
      if(p.value.testu.polozenia<=0.001)
      {
        for(poz in c(1:liczba.poziomow))
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+1+poz),".",(2+1+ile.kol1+2),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[3]) +Font(wb, isBold=TRUE))",sep="")))
        }
      } # koniec if(p.value.testu.polozenia<=0.001)
      
      if(p.value.testu.polozenia <=0.05 )
      {
        duza.macierz.post.hoc[is.nan(duza.macierz.post.hoc)]=1
        for (i.w in c(1:liczba.poziomow))
        {
          for (i.k in c(1:liczba.poziomow))
          {
            if(i.w!=i.k & duza.macierz.post.hoc[i.w,i.k]<=0.05 & duza.macierz.post.hoc[i.w,i.k]>0.01)
            {
              eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+1+i.w),".",(2+ile.kolumn.bez.post.hoc+i.k),"',cellStyle=cs1 + Fill(foregroundColor = paleta[1]) +Font(wb, isBold=TRUE))",sep="")))
            }
            if(i.w!=i.k & duza.macierz.post.hoc[i.w,i.k]<=0.01 & duza.macierz.post.hoc[i.w,i.k]>0.001)
            {
              eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+1+i.w),".",(2+ile.kolumn.bez.post.hoc+i.k),"',cellStyle=cs1 + Fill(foregroundColor = paleta[2]) +Font(wb, isBold=TRUE))",sep="")))
            }
            if(i.w!=i.k & duza.macierz.post.hoc[i.w,i.k]<=0.001)
            {
              eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+1+i.w),".",(2+ile.kolumn.bez.post.hoc+i.k),"',cellStyle=cs1 + Fill(foregroundColor = paleta[3]) +Font(wb, isBold=TRUE))",sep="")))
            }
          } # koniec for (i.k in c(1:liczba.poziomow))
        } # koniec  for (i.w in c(1:liczba.poziomow))
        addMergedRegion(sheet=arkusz,startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),endRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+1+liczba.poziomow),startColumn =(2+1+ile.kol1+3) ,endColumn = (2+1+ile.kol1+3))
        df.nazwa.testu.post.hoc=data.frame(x1=rep(nazwa.testu.post.hoc,times=liczba.poziomow))
        addDataFrame(df.nazwa.testu.post.hoc$x1, sheet=arkusz, startColumn = (2+1+ile.kol1+3), startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))

      } else {# koniec if(p.value.testu.polozenia <=0.05 )
        addMergedRegion(sheet=arkusz,startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),endRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+1+liczba.poziomow),startColumn =(2+1+ile.kol1+3) ,endColumn = (2+1+ile.kol1+3))
        df.nazwa.testu.post.hoc=data.frame(x1=rep("brak",times=liczba.poziomow))
        addDataFrame(df.nazwa.testu.post.hoc$x1, sheet=arkusz, startColumn = (2+1+ile.kol1+3), startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs5))

      }


      if(czy.rysowac.skrzypcowe){

        for(i in c(1:liczba.poziomow))
        {
          zmienne[[i]]<-zmienne[[i]][!is.na(zmienne[[i]])]
        }

        rozmiar.tytulu=0.5

        if(rysuj.roznice)
        {
          # -------------- WYKRESY ROZNIC SKRZYPCOWE ---------------------------
          tytul=paste("Roznice pomiedzy: \n",paste(wiersze_skrocone,collapse=", "),"\n czyszcz outl=",czy.usuwac.outliery )

          nazwa=paste(subDir,"/" ,paste(wiersze_skrocone,collapse=" "),do.nazwy.pliku," roznice czyszcz outl=",czy.usuwac.outliery,".png",sep="")

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

          par(las=1,bty="l")
          eval(parse(text=paste("vioplot(",wsad.wykres.roznice,", col=rgb(0.1,0.4,0.7,0.7) , names=nazwy.z.roznicami)",sep="")))
          mtext(paste("Zmienne to: ",paste(wiersze_skrocone,collapse=" "),sep=""),3,cex = 0.5)
          title(main=tytul,cex.main=rozmiar.tytulu)
          if (names(dev.cur()) != "null device") dev.off()
        } # koniec if(rysuj.roznice)


        # -------------- WYKRESY BEZ ROZNIC SKRZYPCOWE ---------------------------
        tytul=paste(paste(wiersze_skrocone,collapse=", "),"\n czyszcz outl=",czy.usuwac.outliery )

        nazwa=paste(subDir,"/" ,paste(wiersze_skrocone,collapse=" "),do.nazwy.pliku," wartosci czyszcz outl=",czy.usuwac.outliery,".png",sep="")

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
        eval(parse(text=paste("vioplot(",wsad.wykres,", col=randomColor() ,names=names(r)[m[j,]])",sep="")))
        mtext(paste("Zmienne to: ",paste(wiersze_skrocone,collapse=" "),sep=""),3,cex = 0.5)
        title(main=tytul,cex.main=rozmiar.tytulu)
        if (names(dev.cur()) != "null device") dev.off()

      } # koniec if(czy.rysowac.skrzypcowe)

      if(czy.rysowac.pudelkowe){

        for(i in c(1:liczba.poziomow))
        {
          zmienne[[i]]<-zmienne[[i]][!is.na(zmienne[[i]])]
        }
        rozmiar.tytulu=0.5

        # -------------- WYKRESY BEZ ROZNIC PUDELKOWE ---------------------------
        tytul=paste(paste(wiersze_skrocone,collapse=", "),"\n czyszcz outl=",czy.usuwac.outliery )

        nazwa=paste(subDir,"/" ,paste(wiersze_skrocone,collapse=" "),do.nazwy.pliku," wartosci czyszcz outl=",czy.usuwac.outliery," bp.png",sep="")

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
        boundaries<-boxplot(zmienne,plot=FALSE)
        najmn_y=round(min(min(boundaries$stats[1,])*0.9,min(boundaries$stats[1,]-1)))
        najw_y=round(max(max(boundaries$stats[1,])*1.1,max(boundaries$stats[nrow(boundaries$stats),]+1))*1.1)
        boxplot(zmienne,col=randomColor(),xlab=nazwa_osi_x,ylab=nazwa_osi_y, names=poziomy_do_wykresu , border="black",las=2,cex.axis=0.7,ylim=c(najmn_y,najw_y), par(mar=c(c(6.1, 4.1, 4.1, 2.1))))
        if(najw_y<1)
        {
          text(x=c(1:liczba.poziomow), y=boundaries$stats[nrow(boundaries$stats),]*1.1, paste("n = ",wyniki[,1],sep="") )
        } else
        {
          igreki2=boundaries$stats[nrow(boundaries$stats),]*1.1
          igreki2[igreki2<2]=2
          text(x=c(1:liczba.poziomow), y=igreki2, paste("n = ",wyniki[,1],sep="") )
        }

        # mtext(paste("Zmienne to: ",paste(wiersze_skrocone,collapse=" "),sep=""),3,cex = 0.5)
        title(main=tytul,cex.main=rozmiar.tytulu)
        if (names(dev.cur()) != "null device") dev.off()

      } # koniec if(czy.rysowac.pudelkowe)

    } # koniec for(j in c(1:liczba.tabelek))


    if(czy.usuwac.outliery)
    {
      outl.tekst=" outl us"
    } else
    {
      outl.tekst=""
    }

    addMergedRegion(sheet=arkusz,startRow = 1,endRow = 1,startColumn=2,endColumn =6)
    nazwa.cechy=data.frame(x1=rep(paste("Zmienne powiazane",do.nazwy.pliku),times=5))
    addDataFrame(nazwa.cechy$x1, sheet=arkusz, startColumn = 2, startRow = 1,row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs2),byrow=TRUE)

    saveWorkbook(wb, paste("wyniki por ", liczba.poziomow, " grup zal ",do.nazwy.pliku,outl.tekst ,".xlsx",sep=""))


    if(outl)
    {
      print("USUNIETO OUTLIERY! Jesli tego nie chcesz, ustaw zmienna czy.usuwac.outliery na FALSE")
    }
  }
}


