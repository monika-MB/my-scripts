analiza.nominalnych.stdres<<-function(ramka.zmiennych.nominalnych.lub.porzadkowych,macierz.zmiennych.do.tabel,alfa=0.05,do.nazwy.pliku=""){

r=ramka.zmiennych.nominalnych.lub.porzadkowych
m=macierz.zmiennych.do.tabel
nazwy=names(r)
szerokosc=0.95*getOption("width")

# -------------------------------------------------------------------------------------
#                      SCIEZKI I PACZKI
# -------------------------------------------------------------------------------------

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

mainDir<-getwd()
subDir="wykresy mozaikowe stdres"
ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)


source(paste(sciezka.skrypty,"/porownanie.2.grup.zaleznych.num.R",sep=""))
source(paste(sciezka.skrypty,"/porownanie.2.grup.niezaleznych.num.R",sep=""))
source(paste(sciezka.skrypty,"/porownanie.wielu.grup.niezaleznych.num.R",sep=""))
source(paste(sciezka.skrypty,"/porownanie.wielu.grup.zaleznych.num.R",sep=""))

source(paste(sciezka.skrypty,"/porownanie.2.grup.zaleznych.porz.R",sep=""))
source(paste(sciezka.skrypty,"/porownanie.2.grup.niezaleznych.porz.R",sep=""))
source(paste(sciezka.skrypty,"/porownanie.wielu.grup.niezaleznych.porz.R",sep=""))
source(paste(sciezka.skrypty,"/porownanie.wielu.grup.zaleznych.porz.R",sep=""))

source(paste(sciezka.skrypty,"/korelacje.R",sep=""))

suppressPackageStartupMessages(library("RColorBrewer"))
suppressPackageStartupMessages(library("Cairo"))
suppressPackageStartupMessages(library("xlsx"))
suppressPackageStartupMessages(library("colorspace"))
suppressPackageStartupMessages(library("reshape"))
library('rcompanion')
suppressPackageStartupMessages(library('vcd'))
suppressPackageStartupMessages(library('questionr'))

ile.tabel=nrow(m) #ile bedzie tabel kontyngencji
wb = createWorkbook()
sheet1 = createSheet(wb, "nominalne")
licznik.wierszy.nom=2

for(i in c(1:ile.tabel))
{
  tab=table(r[,m[i,1]],r[,m[i,2]],useNA = "no",dnn=c(nazwy[m[i,1]],nazwy[m[i,2]])) # jest tabela kontyngencji

# -------------- WYKRESY -----------------------------------------------

  cien=TRUE #shade
  legenda=FALSE
  kierunek="v" # v lub h

  tab2 <- tab
   rezydua=round(chisq.test(tab)$stdres,2) #standaryzowane, zeby mozna bylo okreslic istotnosc statystycznaa
   oczekiwane=round(chisq.test(tab)$expected,2)

   p.wart.1=2*(1-pnorm(abs(rezydua)))
   p.wart.2=round(matrix(p.adjust(p.wart.1,method="fdr"),nrow=nrow(tab)),4)

   ile.komorek=ncol(tab)*nrow(tab) # ile komorek jest w calej tabeli
  nowe.alfa=alfa/(ile.komorek*2)
   wart.krytyczna.minus=qnorm(nowe.alfa)
   wart.krytyczna.plus=-wart.krytyczna.minus

  tab2[abs(rezydua)<wart.krytyczna.plus]=""
  tab2[rezydua>=wart.krytyczna.plus]="* (+)"
  tab2[rezydua<=wart.krytyczna.minus]="* (-)"

  paleta=c("Accent","Set1","Paired","Dark2","Set3")
  kolorystyka=brewer.pal(n = 8, name = paleta[5])
  ile.kolorow=ncol(tab)+1
  ile.kolorow.t=nrow(tab)+1
  kolory=kolorystyka[c(ile.kolorow:1)]
  if (ile.kolorow>=3)
  {
    kolory=kolory[-(ile.kolorow-2)]
  }

  kolory.t=kolorystyka[c(ile.kolorow.t:1)]
  if (ile.kolorow.t>=3)
  {
    kolory.t=kolory.t[-(ile.kolorow.t-2)]
  }



  # -------------- W JEDNA STRONE -----------------------------------------------

  tytul=paste(nazwy[m[i,1]]," oraz ",nazwy[m[i,2]],sep="" )
  tytul <- paste(strwrap(tytul,width=szerokosc),collapse="\n")

  nazwa=paste(mainDir, "/wykresy mozaikowe stdres/tab kont ", tytul,".png",sep="")

  if (file.exists(nazwa)){
    file.remove(nazwa)
  }
  png(filename=nazwa,
      type="cairo",
      units="in",
      width=12,
      height=8,
      pointsize=12,
      res=600)

  mosaic(tab,#gp=gpar(fill=kolory),
         direction=kierunek,pop=FALSE,
         labeling_args=list(rot_labels=c(left=0,bottom=90),offset_varnames=c(left=8,bottom=10),
                          just_labels="right"),margins = c(left = 8, bottom = 15),
         tl_labels=c(FALSE,TRUE),main=tytul,shade=TRUE,legend=TRUE)
#  labeling_cells(text=tab2,clip=FALSE)(tab)

  if (names(dev.cur()) != "null device") dev.off()


  # -------------- I W DRUGA STRONE -----------------------------------------------
tytul=paste(nazwy[m[i,2]]," oraz ",nazwy[m[i,1]],sep="" )
tytul <- paste(strwrap(tytul,width=szerokosc),collapse="\n")

nazwa=paste(mainDir, "/wykresy mozaikowe stdres/tab kont ", tytul,".png",sep="")

if (file.exists(nazwa)){
  file.remove(nazwa)
}
png(filename=nazwa,
    type="cairo",
    units="in",
    width=12,
    height=8,
    pointsize=12,
    res=600)

mosaic(t(tab),#gp=gpar(fill=kolory.t),
       direction=kierunek,pop=FALSE,
       labeling_args=list(rot_labels=c(left=0,bottom=90),offset_varnames=c(left=8,bottom=10),
                         just_labels="right"),margins = c(left = 8, bottom = 15),
       tl_labels=c(FALSE,TRUE),main=tytul,shade=TRUE,legend=TRUE)
# labeling_cells(text=t(tab2),clip=FALSE)(t(tab))
if (names(dev.cur()) != "null device") dev.off()

# -------------- TABELKI DO XLSX -----------------------------------------------

if(i==1) # przy pierwszym ,,obrocie" dodaj nazwy tabel, co czym jest
{
   nazwa.tab.1="LICZNOSC"
   nazwa.tab.2="UDZIAL PROCENTOWY"
   nazwa.tab.3="OCZEKIWANE"
   nazwa.tab.4="REZYDUA"
   nazwa.tab.5="P SKORYGOWANE"
   addDataFrame(as.data.frame(nazwa.tab.1),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 2,row.names = F,col.names = F)
   addDataFrame(as.data.frame(nazwa.tab.2),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 1*(ncol(tab)+4)+2,row.names = F,col.names = F)
   addDataFrame(as.data.frame(nazwa.tab.3),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 2*(ncol(tab)+4)+2,row.names = F,col.names = F)
   addDataFrame(as.data.frame(nazwa.tab.4),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 3*(ncol(tab)+4)+2,row.names = F,col.names = F)
   addDataFrame(as.data.frame(nazwa.tab.5),sheet=sheet1,startRow = licznik.wierszy.nom ,startColumn = 4*(ncol(tab)+4)+2,row.names = F,col.names = F)

   licznik.wierszy.nom=licznik.wierszy.nom+2
   # nazwa czynnika w kolumnach (pierwsza tabela z licznosciami, tabela kontyngencji)
   addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 2,col.names = F,row.names = F)

   eval(parse(text=paste("ramka.dodawana=cast(as.data.frame(tab),",nazwy[m[i,1]],"~",nazwy[m[i,2]],")",sep="")))

   # nazwa czynnika w kolumnach (druga tabela, z udzialem procentowym)
   addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = ncol(tab)+6,col.names = F,row.names = F)

   oo <- as.data.frame.matrix(format(tab*100/sum(tab),digits=1))
   oo <- sapply(oo,paste,"%")
   ocz <- as.data.frame.matrix(oczekiwane)
   rez <- as.data.frame.matrix(rezydua)
   p.wart <- as.data.frame.matrix(p.wart.2)

   # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn
   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,col.names = T,row.names = F)

   # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn, ale chodzi nam tylko o ,,brzeg'', reszta zostanie nadpisana
   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = ncol(tab)+5,col.names = T,row.names = F)

   # zawartosc tabeli udzialow procentowych, tylko srodek, bez nazw wierszy i kolumn
   addDataFrame(oo,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = ncol(tab)+6,col.names = F,row.names = F)

   # nazwa czynnika w kolumnach (trzecia tabela, z oczekiwanymi)
   addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 2*(ncol(tab)+5),col.names = F,row.names = F)

   # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn, ale chodzi nam tylko o ,,brzeg'', reszta zostanie nadpisana
   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = 2*(ncol(tab)+5)-1,col.names = T,row.names = F)

   # zawartosc tabeli wartosci oczekiwanych, tylko srodek, bez nazw wierszy i kolumn
   addDataFrame(ocz,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = 2*(ncol(tab)+5),col.names = F,row.names = F)

   # nazwa czynnika w kolumnach (czwarta tabela, z rezyduami standaryzowanymi)
   addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 3*(ncol(tab)+5)-1,col.names = F,row.names = F)

   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = 3*(ncol(tab)+5)-2,col.names = T,row.names = F)
   addDataFrame(rez,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = 3*(ncol(tab)+5)-1,col.names = F,row.names = F)
   nAB=paste("N alfa Bon =",round(wart.krytyczna.plus,2))
   addDataFrame(as.data.frame(nAB),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 3*(ncol(tab)+5)-1,row.names = F,col.names = F)

   addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 4*(ncol(tab)+4)+2,col.names = F,row.names = F)
   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = 4*(ncol(tab)+4)+1,col.names = T,row.names = F)
   addDataFrame(p.wart,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = 4*(ncol(tab)+4)+2,col.names = F,row.names = F)


} else # przy kolejnych tabelach lec po staremu
{
  # nazwa czynnika w kolumnach (pierwsza tabela z licznosciami, tabela kontyngencji)
  addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 2,col.names = F,row.names = F)

  eval(parse(text=paste("ramka.dodawana=cast(as.data.frame(tab),",nazwy[m[i,1]],"~",nazwy[m[i,2]],")",sep="")))

  # nazwa czynnika w kolumnach (druga tabela, z udzialem procentowym)
  addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = ncol(tab)+6,col.names = F,row.names = F)

  oo <- as.data.frame.matrix(format(tab*100/sum(tab),digits=1))
  oo <- sapply(oo,paste,"%")
  ocz <- as.data.frame.matrix(oczekiwane)
  rez <- as.data.frame.matrix(rezydua)
  p.wart <- as.data.frame.matrix(p.wart.2)

  # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn
  addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,col.names = T,row.names = F)

  # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn, ale chodzi nam tylko o ,,brzeg'', reszta zostanie nadpisana
  addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = ncol(tab)+5,col.names = T,row.names = F)

  # zawartosc tabeli udzialow procentowych, tylko srodek, bez nazw wierszy i kolumn
  addDataFrame(oo,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = ncol(tab)+6,col.names = F,row.names = F)

  # nazwa czynnika w kolumnach (trzecia tabela, z oczekiwanymi)
  addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 2*(ncol(tab)+5),col.names = F,row.names = F)

  # zawartosc tabeli kontyngencji, nazwa czynnika w wierszach, nazwy wierszy i kolumn, ale chodzi nam tylko o ,,brzeg'', reszta zostanie nadpisana
  addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = 2*(ncol(tab)+5)-1,col.names = T,row.names = F)

  # zawartosc tabeli wartosci oczekiwanych, tylko srodek, bez nazw wierszy i kolumn
  addDataFrame(ocz,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = 2*(ncol(tab)+5),col.names = F,row.names = F)

  # nazwa czynnika w kolumnach (czwarta tabela, z rezyduami standaryzowanymi)
  addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 3*(ncol(tab)+5)-1,col.names = F,row.names = F)

  addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = 3*(ncol(tab)+5)-2,col.names = T,row.names = F)
  addDataFrame(rez,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = 3*(ncol(tab)+5)-1,col.names = F,row.names = F)
  nAB=paste("N alfa Bon =",round(wart.krytyczna.plus,2))
  addDataFrame(as.data.frame(nAB),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 3*(ncol(tab)+5)-1,row.names = F,col.names = F)

  addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 4*(ncol(tab)+4)+2,col.names = F,row.names = F)
  addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = 4*(ncol(tab)+4)+1,col.names = T,row.names = F)
  addDataFrame(p.wart,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = 4*(ncol(tab)+4)+2,col.names = F,row.names = F)

   }

print(min(oczekiwane))
    if(min(oczekiwane)>=1)
   {print("min(oczekiwane >=1)")

         pChi=paste("p chi-kwadrat =",round(chisq.test(tab)$p.value,4))
         addDataFrame(as.data.frame(pChi),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 6,row.names = F,col.names = F)
          try(fisher.test(tab,workspace = 2000000))->gh
         print(length(grep("too small",gh[1]))) 
         
         if(length(grep("too small",gh[1]))==0)
         {print("nie ma errora,length=0")
           pF=fisher.test(tab,workspace = 2000000)$p.value
              pFish=paste("p Fishera =",round(fisher.test(tab,workspace = 2000000)$p.value,4))
              addDataFrame(as.data.frame(pFish),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 2,row.names = F,col.names = F)
         } else
         {print("jest error,length>0")
           pF=fisher.test(tab,workspace = 2000000,simulate.p.value = TRUE)$p.value
              pFish=paste("p Fishera =",round(fisher.test(tab,workspace = 2000000,simulate.p.value = TRUE)$p.value,4))
              addDataFrame(as.data.frame(pFish),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 2,row.names = F,col.names = F)
         }

   } else
   {print("min(oczekiwane <1)")

      try(fisher.test(tab,workspace = 2000000))->gh
     print(length(grep("too small",gh[1])))
     if(length(grep("too small",gh[1]))==0)
      {print("nie ma errora,length=0")
        pF=fisher.test(tab,workspace = 2000000)$p.value
         pFish=paste("p Fishera =",round(fisher.test(tab,workspace = 2000000)$p.value,4))
         addDataFrame(as.data.frame(pFish),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 2,row.names = F,col.names = F)
      } else{print("jest error,length>0")
        pF=fisher.test(tab,workspace = 2000000,simulate.p.value = TRUE)$p.value
         pFish=paste("p Fishera =",round(fisher.test(tab,workspace = 2000000,simulate.p.value = TRUE)$p.value,4))
         addDataFrame(as.data.frame(pFish),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 2,row.names = F,col.names = F)
      }
   }

if(pF<=0.05 | chisq.test(tab)$p.value <=0.05){

    cV=cramerV(tab)
    vCramera=paste("V Cramera =",cV)
    addDataFrame(as.data.frame(vCramera),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 4,row.names = F,col.names = F)

    prog=min(dim(tab))-1

    if(prog<=5) # jeśli rozmiar mniejszego wymiaru to max 6, to interpretujemy efekt
    {
      if(prog==1)
      {
        sm=0.1
        me=0.3
        la=0.5
      }
      if(prog==2)
      {
        sm=0.071
        me=0.212
        la=0.354
      }
      if(prog==3)
      {
        sm=0.058
        me=0.173
        la=0.289
      }
      if(prog==4)
      {
        sm=0.050
        me=0.150
        la=0.250
      }
      if(prog==5)
      {
        sm=0.045
        me=0.134
        la=0.224
      }
      if(cV>=la)
      {
        interp_ef="efekt duzy"
      } else {
        if(cV>=me)
        {
          interp_ef="efekt sredni"
        } else {
          if(cV>=sm)
          {
            interp_ef="efekt maly"
          } else {
            interp_ef="brak efektu"
          }
        }
      }

      addDataFrame(as.data.frame(interp_ef),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+4,startColumn = 4,row.names = F,col.names = F)

    } # if(prog<=5)
} # koniec if(pFish<=0.05 | pChi <=0.05)
     licznik.wierszy.nom <- licznik.wierszy.nom +nrow(ramka.dodawana)+7





# -------------- ZAKONCZENIE -----------------------------------------------

} # koniec for(i in c(1:ile.tabel))

if(do.nazwy.pliku!="")
{
  do.nazwy.pliku = paste(" ",do.nazwy.pliku,sep="")
}
saveWorkbook(wb, paste(mainDir,"/tabele kontyngencji zm. czynnikowych stdres",do.nazwy.pliku,".xlsx",sep=""))

}
