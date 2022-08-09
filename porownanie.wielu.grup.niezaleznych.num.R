porownanie.wielu.grup.niezaleznych.num<<-function(ramka.danych,macierz.zmiennych.do.porownania,czy.usuwac.outliery=FALSE,do.nazwy.pliku="",czy.rysowac.skrzypcowe=TRUE,czy.rysowac.pudelkowe=FALSE){
  # ramka.danych to data.frame, ktorego kolumny to zmienne numeryczne do porownan
  # macierz.zmiennych.do.porownania to matrix o wielu kolumnach, w kazdym wierszu numery kolumn do porownania

  # wyniki to tabela z wynikami
  # w tabeli wyniki jest dla kazdej "nazwa grupy", "N","srednia","odchylenie std","Min","Q1","Q2 (mediana)","Q3","Max" oraz rodzaj testu i jego wartosc p

  #czy.usuwac.outliery=TRUE
  opcja1=TRUE
  # nie ruszac, inicjalizacja zmiennej
  outl=FALSE
  options(contrasts=c("contr.sum","contr.poly"))
  if(is.vector(macierz.zmiennych.do.porownania))
  {
    macierz.zmiennych.do.porownania<-matrix(macierz.zmiennych.do.porownania,nrow=1)
    # print(macierz.zmiennych.do.porownania)

  }
  liczba.poziomow=ncol(macierz.zmiennych.do.porownania)
  r=ramka.danych # skrocenie nazwy
  m=macierz.zmiennych.do.porownania # skrocenie notacji

  suppressPackageStartupMessages(library("vioplot"))
  suppressPackageStartupMessages(library("xlsx"))
  suppressPackageStartupMessages(library("icesTAF"))
  suppressPackageStartupMessages(library("car"))
  suppressPackageStartupMessages(library("agricolae"))
  suppressPackageStartupMessages(library("ggpubr"))
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
      arkusz = createSheet(wb, "Zmienne niepowiazane")

      paleta=rev(palette(heat.colors(3)))
      # normalna czcionka, wysrodkowany, cienka ramka dos/prawa
      cs1 <- CellStyle(wb) + Font(wb) + Border(position = c("BOTTOM","RIGHT"),pen="BORDER_THIN") + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")#
      # bold, wysrodkowany, gruba ramka, kolorowe t?o
      cs2 <- CellStyle(wb) + Fill(foregroundColor = "lightskyblue") +Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
      # bold, wysrodkowany, gruba ramka, tekst obrocony pionowo
      cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER",rotation=90)+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
      # bold, wysrodkowany, gruba ramka
      cs4 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
      # normalna czcionka, wysrodkowany, cienka ramka dos/prawa, szara czcionka
      cs5 <- CellStyle(wb) + Font(wb,color="gray60") + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("BOTTOM","RIGHT"),pen="BORDER_THIN")


    liczba.tabelek=nrow(m)

    zbior.grup<-as.character(NULL)
    wsad<-as.character("n1")
    wsad.wykres<-as.character(NULL)
    for (i in c(1:liczba.poziomow))
    {
      zbior.grup<-c(zbior.grup,as.character(i))
      if (i>1)
      {
        wsad<-paste(wsad,",","n",as.character(i),sep="")
      }
      wsad.wykres<-paste(wsad.wykres,"zmienne[[",as.character(i),"]],",sep="")
    }

    kolumny1<-c("N","Srednia","Odch. std","Mediana","Odch. cw", "Min","Q1","Q3","Max")
    kolumny4<-c("p Shapiro-Wilka","Test porownan wielokrotnych","p testu por. wielokrotnych + efekt","Test post-hoc","Zmienna")

    kolumny5=as.character(NULL)
    ile.testow.post.hoc=liczba.poziomow*(liczba.poziomow-1)/2  #ile testow post-hoc
    for (i in c(1:(liczba.poziomow-1)))
    {
      for (j in c((i+1):liczba.poziomow))
      {
        kolumny5=c(kolumny5,paste("p-value",as.character(i)," vs ", as.character(j)))
      }
    }

    kolumny<-c(kolumny1,kolumny4)

    odstep=2
    rows<-createRow(sheet=arkusz, rowIndex = c(1:(1+liczba.tabelek*(ncol(m)+1+odstep))))
    setRowHeight(rows,30)
    setColumnWidth(sheet=arkusz, colIndex = c(1:(length(kolumny)+2+liczba.poziomow)), 16)
    setColumnWidth(sheet=arkusz, colIndex = c(7:11), 0)
    setColumnWidth(sheet=arkusz, colIndex = 1, 7)
    setColumnWidth(sheet=arkusz, colIndex = 2, 30)

    lista.kolumn.do.cs1=rep(list(cs1),(length(kolumny)+liczba.poziomow))
    names(lista.kolumn.do.cs1) =c(1:(length(kolumny)+liczba.poziomow))

    for(j in c(1:liczba.tabelek)) # wykonaj dla kazdego zestawu 3 lub wiecej zmiennych
    {print(j)
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

      #zmienna i = r[,m[j,i]]

      zmienne<-vector("list",liczba.poziomow)
      # USUWANIE OUTLIEROW

      for(i in c(1:liczba.poziomow))
      {
        zmienne[[i]]=as.numeric(r[,m[j,i]])
      }
      # print(zmienne)

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
      #print(ekstremalne)

      grupowane=data.frame(wartosc=numeric(),nazwa=character())

      for(i in c(1:liczba.poziomow))
      {
        zmienne[[i]]<-zmienne[[i]][!is.na(zmienne[[i]])]
        grupowane=rbind(grupowane,data.frame(wartosc=zmienne[[i]],nazwa=names(r)[m[j,i]]))
      }
      grupowane[,2]=factor(grupowane[,2],ordered=TRUE,levels=unique(grupowane[,2]))



      for(i in c(1:liczba.poziomow))
      {
        wiersz=i
        kolumna=1
        przesuniecie=0

        # ------------- UZUPELNIENIE TABEL STATYSTYKAMI OPISOWYMI   -------

        wyniki[wiersz,przesuniecie+kolumna]=length(zmienne[[i]][!is.na(zmienne[[i]])])
        wyniki[wiersz,przesuniecie+kolumna+1]=round(mean(as.numeric(zmienne[[i]]),na.rm=TRUE),2)
        wyniki[wiersz,przesuniecie+kolumna+2]=round(sd(as.numeric(zmienne[[i]]),na.rm=TRUE),2)
        wyniki[wiersz,przesuniecie+kolumna+3]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[3],2)
        wyniki[wiersz,przesuniecie+kolumna+4]=round(IQR(zmienne[[i]],na.rm = TRUE)/2,2)

        wyniki[wiersz,przesuniecie+kolumna+5]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[1],2)
        wyniki[wiersz,przesuniecie+kolumna+6]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[2],2)
        wyniki[wiersz,przesuniecie+kolumna+7]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[4],2)
        wyniki[wiersz,przesuniecie+kolumna+8]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[5],2)
        assign(paste("n",as.character(i),sep=""),length(zmienne[[i]][!is.na(zmienne[[i]])]))
        wyniki[wiersz,length(kolumny1)+length(kolumny4)]=wiersze[i]

      }

      # -------- UZUPELNIENIE TABELI TESTEM I WYNIKIEM TEGO TESTU


      eval(parse(text=paste("test.zg.chi<-chisq.test(c(",wsad,"))",sep="")))


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


      if(norm & test.zg.chi$p.value>=0.05) # jesli rozklady normalne, to idziemy w testy parametryczne
        # oprocz tego musza byc rownoliczne
      {
        #print("rozklady normalne")


        # test Levene'a
        lt=leveneTest(wartosc~nazwa,data=grupowane ,center=mean)
        lt.p=lt[[3]][1]
        if(lt.p<0.05) #wariancje rozne
        { # ANOVA dla grup niezaleznych z poprawka Welcha F''
          nazwa.testu.polozenia="ANOVA z poprawka Welcha F'' "
          p.value.testu.polozenia=round(stats::oneway.test(formula=wartosc~nazwa,data=grupowane)$p.value,4)
          p.value.testu.polozenia.uzup=p.value.testu.polozenia
          if (p.value.testu.polozenia<=0.05)
          {
            nazwa.testu.post.hoc="Pairwise Welch z popr. Holma"
            post.hoc<-pairwise.t.test(x=grupowane[,1],g=grupowane[,2],p.adjust.method = "bonferroni")

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
            anova.model=aov(wartosc~nazwa,data=grupowane)
            we=round(eta_squared(anova.model),2)
            magn=sila.zaleznosci(we,wsp="eta_kwadrat")
            p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", eta-kwadrat = ",we,", wielkosc = ",magn,sep="")
          }

        } else # rowne wariancje
        { #leci ANOVA
          nazwa.testu.polozenia="ANOVA"
          anova.model=aov(wartosc~nazwa,data=grupowane)
          sum.model=summary(anova.model)
          p.value.testu.polozenia=round(sum.model[[1]][5][1,1],4)
          p.value.testu.polozenia.uzup=p.value.testu.polozenia

          if (p.value.testu.polozenia<=0.05)
          {
            nazwa.testu.post.hoc="Pairwise t-test z popr. Holma"
            post.hoc<-pairwise.t.test(x=grupowane[,1],g=grupowane[,2],paired=FALSE,var.equal=TRUE,p.adjust.method = "bonferroni")

            macierz.post.hoc=post.hoc$p.value
            # print(macierz.post.hoc)
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
            we=round(eta_squared(anova.model),2)
            magn=sila.zaleznosci(we,wsp="eta_kwadrat")
            p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", eta-kwadrat = ",we,", wielkosc = ",magn,sep="")
          }
        }


      } else {  # brak normalnosci rozkladow, jedziemy testy nieparametryczne
        nazwa.testu.polozenia="Kruskala-Wallisa"
        KW=kruskal.test(wartosc~nazwa,data=grupowane)
        p.value.testu.polozenia=round(KW$p.value,4)
        p.value.testu.polozenia.uzup=p.value.testu.polozenia

        if (p.value.testu.polozenia<=0.05)
        {
          nazwa.testu.post.hoc="Pairwise Wilcoxon z popr. Holma"
          post.hoc<-pairwise.wilcox.test(x=grupowane[,1],g=grupowane[,2],paired=FALSE,p.adjust.method ="bonferroni")
          macierz.post.hoc=post.hoc$p.value
          # print(macierz.post.hoc)
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

         we=round(epsilonSquared(x=grupowane$wartosc,g=grupowane$nazwa),2)
         magn=sila.zaleznosci(sqrt(we))
         p.value.testu.polozenia.uzup=paste("p = ",p.value.testu.polozenia,", epsilon-kwadrat = ",we,", wielkosc = ",magn,sep="")


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
        if(p.value.testu.polozenia<0.05){
          rozmiar.tytulu=0.5
          tytul=paste(paste(wiersze_skrocone,collapse=" "),"\n czyszcz outl=",czy.usuwac.outliery )
          nazwa=paste(subDir,"/" ,paste(wiersze_skrocone,collapse=" "),do.nazwy.pliku," czyszcz outl=",czy.usuwac.outliery,".png",sep="")
          
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
          
          eval(parse(text=paste("vioplot(",wsad.wykres,"col=randomColor() , names=names(r)[m[j,]])",sep="")))
          mtext(paste("Zmienne to: ",paste(wiersze_skrocone,collapse=", "),sep=""),3,cex = 0.5)
          title(main=tytul,cex.main=rozmiar.tytulu)
          if (names(dev.cur()) != "null device") dev.off()
        }

      } # koniec if(czy.rysowac.skrzypcowe)

      if(czy.rysowac.pudelkowe){
        if(p.value.testu.polozenia<0.05){
          rozmiar.tytulu=0.5
          tytul=paste(paste(wiersze_skrocone,collapse=", "),"\n czyszcz outl=",czy.usuwac.outliery )
          
          nazwa=paste(subDir,"/" ,paste(wiersze_skrocone,collapse=" "),do.nazwy.pliku," czyszcz outl=",czy.usuwac.outliery," bp.png",sep="")
          
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
          boxplot(zmienne,col=randomColor() ,xlab=nazwa_osi_x,ylab=nazwa_osi_y, names=poziomy_do_wykresu,border="black",las=2,cex.axis=0.7,ylim=c(najmn_y,najw_y),par(mar=c(c(6.1, 4.1, 4.1, 2.1))))
          if(najw_y<1)
          {
            text(x=c(1:liczba.poziomow), y=boundaries$stats[nrow(boundaries$stats),]*1.1, paste("n = ",wyniki[,1],sep="") )
          } else
          {
            igreki2=boundaries$stats[nrow(boundaries$stats),]*1.1
            igreki2[igreki2<2]=2
            text(x=c(1:liczba.poziomow), y=igreki2, paste("n = ",wyniki[,1],sep="") )
          }
          #   mtext(paste("Zmienne to: ",paste(wiersze_skrocone,collapse=", "),sep=""),3,cex = 0.5)
          title(main=tytul,cex.main=rozmiar.tytulu)
          if (names(dev.cur()) != "null device") dev.off()
        }
        
      } # koniec if(czy.rysowac.pudelkowe)

    } # koniec: dla kazdego zestawu 3 lub wiecej porownywanych zmiennych
    if(czy.usuwac.outliery)
    {
      outl.tekst="us"
    } else
    {
      outl.tekst="nieus"
    }
    addMergedRegion(sheet=arkusz,startRow = 1,endRow = 1,startColumn=2,endColumn =6)
    nazwa.cechy=data.frame(x1=rep("Zmienne niepowiazane",times=5))
    addDataFrame(nazwa.cechy$x1, sheet=arkusz, startColumn = 2, startRow = 1,row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs2),byrow=TRUE)

    saveWorkbook(wb, paste("wyniki por ", liczba.poziomow, " grup niezal ",do.nazwy.pliku," outl ",outl.tekst ,".xlsx",sep=""))

    if(outl)
    {
      print("USUNIETO OUTLIERY! Jesli tego nie chcesz, ustaw zmienna czy.usuwac.outliery na FALSE")
    }

  } # koniec if(opcja1)


}


