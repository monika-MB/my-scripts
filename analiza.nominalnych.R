analiza.nominalnych<<-function(ramka.zmiennych.nominalnych.lub.porzadkowych,macierz.zmiennych.do.tabel,do.nazwy.pliku=""){

# analiza.nominalnych<<-function(tabela.kontyngencji,do.nazwy.pliku=""){
  
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
subDir="wykresy mozaikowe"
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
suppressPackageStartupMessages(library('vcd'))
suppressPackageStartupMessages(library('questionr'))
suppressPackageStartupMessages(library("reshape"))

ile.tabel=nrow(m) #ile bedzie tabel kontyngencji
wb = createWorkbook()
sheet1 = createSheet(wb, "nominalne")
licznik.wierszy.nom=2

for(i in c(1:ile.tabel))
{
  tab=table(r[,m[i,1]],r[,m[i,2]],useNA = "no",dnn=c(nazwy[m[i,1]],nazwy[m[i,2]])) # jest tabela kontyngencji
  #print(tab)

# -------------- WYKRESY -----------------------------------------------

  cien=TRUE #shade
  legenda=FALSE
  kierunek="v" # v lub h
  
  tab2 <- tab
   rezydua=chisq.residuals(tab,std=TRUE)
  #rezydua=chisq.residuals(tab)
  
  tab2[abs(rezydua)<2]=""
  tab2[rezydua>=2 & rezydua<3]="(+)*"
  tab2[rezydua>-3 & rezydua<=-2]="(-)*"
  tab2[rezydua>=3 & rezydua<4]="(+)**"
  tab2[rezydua>-4 & rezydua<=-3]="(-)**"
  tab2[rezydua>=4 ]="(+)***"
  tab2[rezydua<=-4 ]="(-)***"
  
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
  
  nazwa=paste(mainDir, "/wykresy mozaikowe/tab kont ", tytul,".png",sep="")
  
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
  
  mosaic(tab,direction=kierunek,
         labeling_args=list(rot_labels=c(left=0,bottom=90),offset_varnames=c(left=8,bottom=10),
                          just_labels="right"),margins = c(left = 8, bottom = 15),
         tl_labels=c(FALSE,TRUE),main=tytul, shade=TRUE,legend=TRUE)

  if (names(dev.cur()) != "null device") dev.off()


  # -------------- I W DRUGA STRONE -----------------------------------------------
tytul=paste(nazwy[m[i,2]]," oraz ",nazwy[m[i,1]],sep="" )
tytul <- paste(strwrap(tytul,width=szerokosc),collapse="\n")

nazwa=paste(mainDir, "/wykresy mozaikowe/tab kont ", tytul,".png",sep="")

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

mosaic(t(tab),#gp=gpar(fill=kolory),
       direction=kierunek,pop=FALSE,
       labeling_args=list(rot_labels=c(left=0,bottom=90),offset_varnames=c(left=8,bottom=10),
                         just_labels="right"),margins = c(left = 8, bottom = 15),
       tl_labels=c(FALSE,TRUE),main=tytul,shade=TRUE,legend=TRUE)
#labeling_cells(text=t(tab2),clip=FALSE)(t(tab))
if (names(dev.cur()) != "null device") dev.off()

# -------------- TABELKI DO XLSX -----------------------------------------------

    addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = 2,col.names = F,row.names = F)
eval(parse(text=paste("ramka.dodawana=cast(as.data.frame(tab),",nazwy[m[i,1]],"~",nazwy[m[i,2]],")",sep="")))
addDataFrame(as.data.frame(nazwy[m[i,2]]),sheet=sheet1,startRow = licznik.wierszy.nom,startColumn = ncol(tab)+5,col.names = F,row.names = F)
oo <- as.data.frame.matrix(format(tab*100/sum(tab),digits=1))
oo <- sapply(oo,paste,"%")
   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,col.names = T,row.names = F)
   addDataFrame(ramka.dodawana,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = ncol(tab)+4,col.names = T,row.names = F)
   addDataFrame(oo,sheet = sheet1,startRow = licznik.wierszy.nom+2,startColumn = ncol(tab)+5,col.names = F,row.names = F)
 #    ramka.dodawana.proc=ramka.dodawana[,-1]
  #   ramka.dodawana.proc=format(100*ramka.dodawana[,-1]/sum(ramka.dodawana[,-1]),digits=1)
   #  print(ramka.dodawana.proc)
    # ramka.dodawana.proc=sapply(ramka.dodawana.proc,paste,"%")
     #print(ramka.dodawana[,1])
     #ramka.dodawana.proc=cbind(ramka.dodawana[,1],ramka.dodawana.proc)
     #print(ramka.dodawana.proc)
     #addDataFrame(ramka.dodawana.proc,sheet=sheet1, startRow=licznik.wierszy.nom+1,startColumn = ncol(tab)+4,col.names = T,row.names = F)
     
#addDataFrame(as.data.frame(tab),sheet=sheet1,startRow = licznik.wierszy.nom+1,startColumn = ncol(tab)+4,col.names = F,row.names = F)


     try(fisher.test(tab,workspace = 2000000))->gh
     if(length(grep("too small",gh[1]))==0)
        {
       pFish=paste("p Fishera =",round(fisher.test(tab,workspace = 2000000)$p.value,4))
       addDataFrame(as.data.frame(pFish),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 2,row.names = F,col.names = F)
       pFishNiez=paste("p niezaokr. =",fisher.test(tab,workspace = 2000000)$p.value)
       addDataFrame(as.data.frame(pFishNiez),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab) +3,startColumn = 4,row.names = F,col.names = F)
     } else
     {
       pFish=paste("p Fishera =",round(fisher.test(tab,workspace = 2000000,simulate.p.value = TRUE)$p.value,4))
       addDataFrame(as.data.frame(pFish),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab)+3,startColumn = 2,row.names = F,col.names = F)
       pFishNiez=paste("p niezaokr. =",fisher.test(tab,workspace = 2000000,simulate.p.value = TRUE)$p.value)
       addDataFrame(as.data.frame(pFishNiez),sheet=sheet1,startRow = licznik.wierszy.nom +nrow(tab) +3,startColumn = 4,row.names = F,col.names = F)
     }
     
     
     
     
     
     
     
     
  #   
     licznik.wierszy.nom <- licznik.wierszy.nom +nrow(ramka.dodawana)+7





# -------------- ZAKONCZENIE -----------------------------------------------

} # koniec for(i in c(1:ile.tabel))

if(do.nazwy.pliku!="")
{
  do.nazwy.pliku = paste(" ",do.nazwy.pliku,sep="")
}

saveWorkbook(wb, paste(mainDir,"/tabele kontyngencji zm. czynnikowych ",do.nazwy.pliku,".xlsx",sep=""))
options(warn = 0)
}
