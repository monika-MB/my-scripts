porownanie.2.grup.zaleznych.num<<-function(ramka.danych,macierz.zmiennych.do.porownania,czy.usuwac.outliery=FALSE,do.nazwy.pliku="",czy.rysowac.skrzypcowe=TRUE,czy.rysowac.pudelkowe=FALSE){
  # ramka.danych to data.frame, ktorego kolumny to zmienne numeryczne do porownan
  # macierz.zmiennych.do.porownania to matrix o dwoch kolumnach, w kazdym wierszu para oznaczajaca numery kolumn do porownania
  
  
  opcja1=TRUE
  
  # nie ruszac, inicjalizacja zmiennej
  outl=FALSE
  r=ramka.danych # skrocenie nazwy
  m=macierz.zmiennych.do.porownania # skrocenie notacji
  
  suppressPackageStartupMessages(library("vioplot"))
  suppressPackageStartupMessages(library("xlsx"))
  suppressPackageStartupMessages(library("icesTAF"))
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
    arkusz = createSheet(wb, "2 zmienne powiazane")
    
    
    paleta=rev(palette(heat.colors(3)))
    # normalna czcionka, wys8rodkowany, cienka ramka do?/prawa
    cs1 <- CellStyle(wb) + Font(wb) + Border(position = c("BOTTOM","RIGHT"),pen="BORDER_THIN") + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")#
    # bold, wysrodkowany, gruba ramka, kolorowe t?o
    cs2 <- CellStyle(wb) + Fill(foregroundColor = "lightskyblue") +Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
    # bold, wysrodkowany, gruba ramka, tekst obr?cony pionowo
    cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER",rotation=90)+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
    # bold, wysrodkowany, gruba ramka
    cs4 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText = TRUE,horizontal = "ALIGN_CENTER", vertical="VERTICAL_CENTER")+Border(position = c("TOP","BOTTOM","LEFT","RIGHT"),pen="BORDER_THICK")
    
    
    kolumny1<-c("N","Srednia","Odch. std","Mediana","Odch. cw", "Min","Q1","Q3","Max")
    kolumny4<-c("p Shapiro-Wilka","p t-Studenta + efekt","p Wilcoxona + efekt")
    kolumny<-c(kolumny1,kolumny4)
    
    liczba.par=nrow(m)
    liczba.poziomow=ncol(m)
    
    odstep=2
    rows<-createRow(sheet=arkusz, rowIndex = c(1:(1+liczba.par*(ncol(m)+1+odstep))))
    setRowHeight(rows,30)
    setColumnWidth(sheet=arkusz, colIndex = c(1:(length(kolumny)+2)), 16)
    setColumnWidth(sheet=arkusz, colIndex = c(7:11), 0)
    setColumnWidth(sheet=arkusz, colIndex = 1, 7)
    
    lista.kolumn.do.cs1=rep(list(cs1),length(kolumny))
    names(lista.kolumn.do.cs1) =c(1:length(kolumny))
    
    for(j in c(1:liczba.par)) # wykonaj dla kazdej pary porownywanych zmiennych
    {
      print(j)
      wiersze<-c(names(r)[m[j,1]],names(r)[m[j,2]])
      
      wyniki=matrix(NA,nrow=ncol(m),ncol=length(kolumny),dimnames=list(wiersze,kolumny))
      
      zmienna1=as.numeric(r[,m[j,1]])
      zmienna2=as.numeric(r[,m[j,2]])
      pelne=complete.cases(data.frame(zmienna1, zmienna2))
      zmienna1=zmienna1[pelne]
      zmienna2=zmienna2[pelne]
      zmienna3=(zmienna1-zmienna2)
      
      # USUWANIE OUTLIEROW
      
      if(czy.usuwac.outliery)
      {
        ekstremalne<-vector("list",3)
        ekstremalne[[1]]=boxplot(x=zmienna1,plot=FALSE)$out
        ekstremalne[[2]]=boxplot(x=zmienna2,plot=FALSE)$out
        ekstremalne[[3]]=boxplot(x=zmienna3,plot=FALSE)$out
        
        if(length(ekstremalne[[1]])>0)
        {
          zmienna1[is.element(zmienna1,ekstremalne[[1]])]=NA
          outl=TRUE
          ekstremalne[[1]]=boxplot(x=zmienna1,plot=FALSE)$out
        }
        if(length(ekstremalne[[2]])>0)
        {
          zmienna2[is.element(zmienna2,ekstremalne[[2]])]=NA
          outl=TRUE
          ekstremalne[[2]]=boxplot(x=zmienna2,plot=FALSE)$out
        }
        if(length(ekstremalne[[3]])>0)
        {
          zmienna3[is.element(zmienna3,ekstremalne[[3]])]=NA
          outl=TRUE
          ekstremalne[[3]]=boxplot(x=zmienna3,plot=FALSE)$out
        }
        na1 = which(is.na(zmienna1))
        na2 = which(is.na(zmienna2))
        do.usun.2 = union(na1,na2)
        if (length(do.usun.2)>0)
        {
          zmienna1=zmienna1[-do.usun.2]
          zmienna2=zmienna2[-do.usun.2]
        }
        
      }
      
      zmienne=list(zmienna1,zmienna2,zmienna3)
      zmienne.do.wykr=list(zmienna1,zmienna2)
      
      for(i in c(1:2))
      {
        wiersz=i
        kolumna=-3
        
        # ------------- UZUPELNIENIE TABEL STATYSTYKAMI OPISOWYMI   -------
        
        
        wyniki[wiersz,kolumna+4]=length(zmienna3[!is.na(zmienna3)])
        
        wyniki[wiersz,kolumna+5]=round(mean(as.numeric(zmienne[[i]]),na.rm=TRUE),2)
        wyniki[wiersz,kolumna+6]=round(sd(as.numeric(zmienne[[i]]),na.rm=TRUE),2)
        wyniki[wiersz,kolumna+7]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[3],2)
        wyniki[wiersz,kolumna+8]=round(IQR(zmienne[[i]],na.rm = TRUE)/2,2)
        
        wyniki[wiersz,kolumna+9]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[1],2)
        wyniki[wiersz,kolumna+10]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[2],2)
        wyniki[wiersz,kolumna+11]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[4],2)
        wyniki[wiersz,kolumna+12]=round(quantile(as.numeric(zmienne[[i]]),na.rm=TRUE)[5],2)
        assign(paste("n",as.character(i),sep=""),length(zmienne[[i]][!is.na(zmienne[[i]])]))
        
        
      }
      
      # -------- UZUPELNIENIE TABELI TESTEM I WYNIKIEM TEGO TESTU
      
      ramka1=data.frame(zmienna=zmienna1)
      ramka1$nazwa=as.character(1)
      ramka2=data.frame(zmienna=zmienna2)
      ramka2$nazwa=as.character(2)
      ramka.grupujaca<-rbind(ramka1,ramka2) #ramka ma teraz dwie kolumny: 1) zmienna 2) nazwa
      ramka.grupujaca=ramka.grupujaca[complete.cases(ramka.grupujaca),]
      
      # brak normalnosci rozkladow, jedziemy testy nieparametryczne, tak samo jesli nierownoliczne
      
      
      
      if (n1<3 | n2<3)
      {
        sh1=0
        sh2=0
      } else
      {
        sh1=shapiro.test.try.catch.p.value(zmienna1)
        sh2=shapiro.test.try.catch.p.value(zmienna2)
      }
      
      sh=round(c(sh1,sh2),4)
      
      if(sh1>=0.05 & sh2>=0.05){ # jesli rozklady normalne, to idziemy w testy parametryczne
        # zalozenie o rownolicznosci jest spelnione z definicji
        
        
        p.value.testu.studenta=round(t.test(zmienna1,zmienna2, paired=TRUE)$p.value,4)
        p.value.testu.studenta.uzup=p.value.testu.studenta
        if(p.value.testu.studenta<=0.05)
        {
          efekt=cohens_d(formula=zmienna~nazwa,data=ramka.grupujaca)
          we=round(efekt$effsize,2)
          magn=efekt$magnitude
          p.value.testu.studenta.uzup=paste("p = ",p.value.testu.studenta,", d Cohena = ",we,", wielkosc = ",magn,sep="")
        }
        
        
        
      } else
      {  # brak normalnosci rozkladow, jedziemy testy nieparametryczne
        p.value.testu.studenta="nie obliczono - zalozenia niespelnione"
        p.value.testu.studenta.uzup=p.value.testu.studenta
      }
      p.value.testu.wilcoxona=round(wilcox.test(zmienna1,zmienna2,paired=TRUE)$p.value,4)
      if(is.na(p.value.testu.wilcoxona))
      {
        p.value.testu.wilcoxona = 1
      }
      p.value.testu.wilcoxona.uzup=p.value.testu.wilcoxona
      if(p.value.testu.wilcoxona<=0.05)
      { # rc Rangowy wspolczynnik korelacji dwuseryjnej dla par dopasowanych
        we=round(wilcoxonPairedRC(x=ramka.grupujaca$zmienna,g=ramka.grupujaca$nazwa),2)
        magn=sila.zaleznosci(we)
        p.value.testu.wilcoxona.uzup=paste("p = ",p.value.testu.wilcoxona,", RC = ",we,", wielkosc = ",magn,sep="")
      }
      
      
      for(i in c(1:liczba.poziomow))
      {
        wiersz=i
        # uzupelnienie czesci z testami statystycznymi
        wyniki[wiersz,length(wyniki[1,])-2]=sh[i] # p.value testu Shapiro-Wilka
      }
      
      wyniki<-as.data.frame(wyniki)
      
      
      wiersz.startowy=4+ (j-1)*(ncol(m)+1+odstep)
      addDataFrame(wyniki, sheet=arkusz, startColumn=2, row.names=TRUE,rownamesStyle = cs4,colnamesStyle = cs4,startRow = wiersz.startowy,colStyle = lista.kolumn.do.cs1)
      
      addMergedRegion(sheet=arkusz,startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),endRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+3),startColumn =(length(wyniki[1,])-1+2) ,endColumn = (length(wyniki[1,])-1+2))
      df.p.Studenta=data.frame(x1=rep(p.value.testu.studenta.uzup,times=liczba.poziomow)) # p.value testu Studenta
      addDataFrame(df.p.Studenta$x1, sheet=arkusz, startColumn = (length(wyniki[1,])-1+2), startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))
      
      addMergedRegion(sheet=arkusz,startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),endRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+3),startColumn =(length(wyniki[1,])+2) ,endColumn = (length(wyniki[1,])+2))
      df.p.Wilcoxona=data.frame(x1=rep(p.value.testu.wilcoxona.uzup,times=liczba.poziomow)) # p.value testu Wilcoxona
      addDataFrame(df.p.Wilcoxona$x1, sheet=arkusz, startColumn = (length(wyniki[1,])+2), startRow = (1+odstep*j+(j-1)*(liczba.poziomow+1)+2),row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs1))
      
      # --------------------- KOLOROWANIE P<0,05 ----------------------------------
      
      rows  <- getRows(arkusz)
      cells <- getCells(rows)
      values <- lapply(cells, getCellValue)
      
      if(!is.character(p.value.testu.studenta))
      {
        if(p.value.testu.studenta<=0.05 & p.value.testu.studenta>0.01 )
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+2),".",(length(wyniki[1,])+1),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[1]) +Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+3),".", (length(wyniki[1,])+1),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[1])+Font(wb, isBold=TRUE))",sep="")))
        } # koniec if(p.value.testu.polozenia<=0.05 & p.value.testu.polozenia>0.01 )
        if(p.value.testu.studenta<=0.01 & p.value.testu.studenta>0.001 )
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+2),".",(length(wyniki[1,])+1),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[2])+Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+3),".",(length(wyniki[1,])+1),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[2])+Font(wb, isBold=TRUE))",sep="")))
        } # koniec if(p.value.testu.polozenia<=0.01 & p.value.testu.polozenia>0.001 )
        if(p.value.testu.studenta<=0.001)
        {
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+2),".",(length(wyniki[1,])+1),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[3])+Font(wb, isBold=TRUE))",sep="")))
          eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+3),".",(length(wyniki[1,])+1),
                                "',cellStyle=cs1 + Fill(foregroundColor = paleta[3])+Font(wb, isBold=TRUE))",sep="")))
        } # koniec if(p.value.testu.polozenia<=0.001)
      }
      
      
      if(p.value.testu.wilcoxona<=0.05 & p.value.testu.wilcoxona>0.01 )
      {
        eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+2),".",(length(wyniki[1,])+2),
                              "',cellStyle=cs1 + Fill(foregroundColor = paleta[1]) +Font(wb, isBold=TRUE))",sep="")))
        eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+3),".", (length(wyniki[1,])+2),
                              "',cellStyle=cs1 + Fill(foregroundColor = paleta[1])+Font(wb, isBold=TRUE))",sep="")))
      } # koniec if(p.value.testu.polozenia<=0.05 & p.value.testu.polozenia>0.01 )
      if(p.value.testu.wilcoxona<=0.01 & p.value.testu.wilcoxona>0.001 )
      {
        eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+2),".", (length(wyniki[1,])+2),
                              "',cellStyle=cs1 + Fill(foregroundColor = paleta[2])+Font(wb, isBold=TRUE))",sep="")))
        eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+3),".",(length(wyniki[1,])+2),
                              "',cellStyle=cs1 + Fill(foregroundColor = paleta[2])+Font(wb, isBold=TRUE))",sep="")))
      } # koniec if(p.value.testu.polozenia<=0.01 & p.value.testu.polozenia>0.001 )
      if(p.value.testu.wilcoxona<=0.001)
      {
        eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+2),".",(length(wyniki[1,])+2),
                              "',cellStyle=cs1 + Fill(foregroundColor = paleta[3])+Font(wb, isBold=TRUE))",sep="")))
        eval(parse(text=paste("setCellStyle(cell=cells$'",(1+odstep*j+(j-1)*(liczba.poziomow+1)+3),".", (length(wyniki[1,])+2),
                              "',cellStyle=cs1 + Fill(foregroundColor = paleta[3])+Font(wb, isBold=TRUE))",sep="")))
      } # koniec if(p.value.testu.polozenia<=0.001)
      
      # ------------------------------- WYKRESY -------------------------------------------------------------------
      
      nazwa1=names(r)[m[j,1]]
      nazwa2=names(r)[m[j,2]]
      nazwa3="roznice"
      
      rozmiar.tytulu=0.5
      
      if(czy.rysowac.skrzypcowe){
        if((!is.character(p.value.testu.studenta) & p.value.testu.studenta<0.05) |
           p.value.testu.wilcoxona<0.05){
          # ----- WYKRESY BEZ ROZNIC -----------------------
          
          tytul=paste(nazwa1, " vs ",nazwa2," \n czyszczenie outlierow=",czy.usuwac.outliery )
          
          nazwa=paste(subDir,"/" ,nazwa1," vs ",nazwa2,do.nazwy.pliku," czyszczenie outlierow=",czy.usuwac.outliery,".png",sep="")
          
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
          vioplot( zmienna1[!is.na(zmienna1)] , zmienna2[!is.na(zmienna2)], col=randomColor() , names=names(r)[m[j,]])
          #  title(main=tytul,ylab=igreki,xlab=iksy)
          mtext(paste("Zmienne to: ",paste(names(r)[m[j,]],collapse=", "),sep=""),3,cex = 0.5)
          
          title(main=tytul,cex.main=rozmiar.tytulu)
          if (names(dev.cur()) != "null device") dev.off()
        }
        
      } # koniec if(czy.rysowac.skrzypcowe)
      
      if(czy.rysowac.pudelkowe){
        if((!is.character(p.value.testu.studenta) & p.value.testu.studenta<0.05) |
           p.value.testu.wilcoxona<0.05){
          # ----- WYKRESY BEZ ROZNIC -----------------------
          
          tytul=paste(nazwa1, " vs ",nazwa2," \n czyszczenie outlierow=",czy.usuwac.outliery )
          
          nazwa=paste(subDir,"/" ,nazwa1," vs ",nazwa2,do.nazwy.pliku," czyszczenie outlierow=",czy.usuwac.outliery," bp.png",sep="")
          
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
          # boxplot( zmienne.do.wykr, col=randomColor() , names=names(r)[m[j,]],las=1)
          #  title(main=tytul,ylab=igreki,xlab=iksy)
          # mtext(paste("Zmienne to: ",paste(names(r)[m[j,]],collapse=", "),sep=""),3,cex = 0.5)
          boundaries<-boxplot(zmienne.do.wykr,plot=FALSE)
          najmn_y=round(min(min(boundaries$stats[1,])*0.9,min(boundaries$stats[1,]-1)))
          najw_y=round(max(max(boundaries$stats[1,])*1.1,max(boundaries$stats[nrow(boundaries$stats),]+1))*1.1)
          boxplot(zmienne.do.wykr,col=randomColor() , names=names(r)[m[j,]],border="black",las=2,cex.axis=0.7,ylim=c(najmn_y,najw_y), par(mar=c(c(6.1, 4.1, 4.1, 2.1))))
          if(najw_y<1)
          {
            text(x=c(1:liczba.poziomow), y=boundaries$stats[nrow(boundaries$stats),]*1.1, paste("n = ",wyniki[,1],sep="") )
          } else
          {
            igreki2=boundaries$stats[nrow(boundaries$stats),]*1.1
            igreki2[igreki2<2]=2
            text(x=c(1:liczba.poziomow), y=igreki2, paste("n = ",wyniki[,1],sep="") )
          }
          title(main=tytul,cex.main=rozmiar.tytulu,ylab=stringr::str_extract(tytul, '\\w*'))
          if (names(dev.cur()) != "null device") dev.off()
        }
        
        
      } # koniec if(czy.rysowac.pudelkowe)
      
    } # koniec: dla kazdej pary porownywanych zmiennych
    
    if(czy.usuwac.outliery)
    {
      outl.tekst="us"
    } else
    {
      outl.tekst="nieus"
    }
    addMergedRegion(sheet=arkusz,startRow = 1,endRow = 1,startColumn=2,endColumn =6)
    nazwa.cechy=data.frame(x1=rep("Zmienne powiazane",times=5))
    addDataFrame(nazwa.cechy$x1, sheet=arkusz, startColumn = 2, startRow = 1,row.names = FALSE, col.names = FALSE,colStyle = list('1'=cs2),byrow=TRUE)
    
    saveWorkbook(wb, paste("wyniki por 2 grup zal ",do.nazwy.pliku," outl ",outl.tekst ,".xlsx",sep=""))
    
    if(outl)
    {
      print("USUNIETO OUTLIERY! Jesli tego nie chcesz, ustaw zmienna czy.usuwac.outliery na FALSE")
    }
    
    
  } # koniec if (opcja1)
  
}


