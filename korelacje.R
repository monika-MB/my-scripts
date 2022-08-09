korelacje<<-function(dane.w.kolumnach,dane.w.wierszach=NULL,wektor.porzadkowych=NULL,do.nazwy.pliku="",nazwa.cech.w.wierszach=NULL,nazwa.cech.w.kolumnach=NULL,rysuj.elipsy=FALSE,alfa.do.pliku=0.05,rysuj.rozrzuty=FALSE){
  # domyslnie wszystkie zmienne sa interwalowe
  # jako drugi parametr mozna przekazac wektor numerow zmiennych porzadkowych
  #dane 2 s¹ w wierszach, dane w kolumnach

  dane=dane.w.kolumnach
  dane2=dane.w.wierszach

  if(is.vector(dane))
  {
    dane=data.frame(x1=dane)
  }
  if(is.vector(dane2))
  {
    dane2=data.frame(x2=dane2)
  }

  indeksy1=NULL
  indeksy2=NULL
  n1=ncol(dane)

  #print(n1)
  dwie.macierze=FALSE
  if(!is.null(dane2))
  {
    n2=ncol(dane2)

    indeksy1=c(1:n1)
    indeksy2=c((n1+1):(n1+n2))
    dane=cbind(dane,dane2)
    dwie.macierze=TRUE
  }

  mainDir<-getwd()
  if (rysuj.elipsy)
  {
    subDirP<-"korelacje Pearsona"
    ifelse(!dir.exists(file.path(mainDir, subDirP)), dir.create(file.path(mainDir, subDirP)), FALSE)
    subDirS<-"korelacje Spearmana"
    ifelse(!dir.exists(file.path(mainDir, subDirS)), dir.create(file.path(mainDir, subDirS)), FALSE)
    sciezki.wykresow<-c(paste(mainDir,"/",subDirP,sep=""),paste(mainDir,"/",subDirS,sep=""))

    if (names(dev.cur()) != "null device") dev.off(dev.list()["RStudioGD"])
  } # koniec if (rysuj)
  if (rysuj.rozrzuty)
  {
    subDirR<-"wykresy rozrzutu"
    ifelse(!dir.exists(file.path(mainDir, subDirR)), dir.create(file.path(mainDir, subDirR)), FALSE)
    if (names(dev.cur()) != "null device") dev.off(dev.list()["RStudioGD"])
  } # koniec if (rysuj.rozrzuty)



  suppressPackageStartupMessages(library("corrplot"))
  suppressPackageStartupMessages(library("Cairo"))
  suppressPackageStartupMessages(library("psych"))
  suppressPackageStartupMessages(library("xlsx"))
  suppressPackageStartupMessages(library("colorspace"))

  if (rysuj.elipsy)
  {
    col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                               "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                               "#4393C3", "#2166AC", "#053061"))
    col5<-colorRampPalette(c("#053061", "#2166AC","#4393C3","#92C5DE","#D1E5F0",
                             "#FFFFFF","#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
    col1<-diverge_hsv(50)
    col3<-diverge_hcl(50)
    col4<-rainbow_hcl(50)
    col6<-diverge_hcl(50,c=100,l=c(50,90),power=1)
    col7<-diverge_hcl(50,h=c(180,70),c=70,l=c(90,95))
    col8<-diverge_hcl(50,h=c(130,43),c=100,l=c(70,90))
  } # koniec if (rysuj.elipsy)


  ile.zmiennych=ncol(dane)
  if(is.vector(dane))
  {
    ile.zmiennych=1
  }
  wszystkie=c(1:ile.zmiennych)

  if(!dwie.macierze)
  {
    n1=ile.zmiennych
  }


  sa.porzadkowe=FALSE # flaga, oznaczajaca czy sa w ogole zmienne porzadkowe

  if(!is.null(wektor.porzadkowych))
  {
    interwalowe=wszystkie[-wektor.porzadkowych]
    # print(interwalowe)
    # print(names(dane))
    # print(names(dane)[wektor.porzadkowych])
    
    sa.porzadkowe=TRUE
  } else
  {
    interwalowe=wszystkie
  } # koniec if(!is.null(wektor.porzadkowych))


    uzywaj="pairwise.complete.obs"
    metoda=c("pearson","spearman")
    ktore=list(interwalowe,wszystkie)  # kolejnosc musi byc zachowana: interwalowe-Pearson, wszystkie-Spearman
    do.tytulow.ktore<-c("Zm. interw.","Wszystkie zm.")
    do.tytulow.metoda <- c("Pearsona","Spearmana")
    wynik=vector("list",2)
    wb = createWorkbook()

  # ------------------- Pearsona robimy tylko dla interwalowych -------------------------------
  # ------------------ zawsze robimy Spearmana dla wszystkich cech ---------------------------

   for (rodz in c(1,2)) # jedziemy po "rodzajach" korelacji, czyli Pearson, Spearman
   {
     # w zmiennej met jest metoda (Pearson/Spearman)
     met=metoda[rodz]
     # w zmiennej kt jest typ zmiennych (interwalowe/wszystkie)
     kt=ktore[[rodz]]

     ile.kt=length(kt)
     lw =lk= ile.kt

     rpn=n=M=matrix(NA,nrow=lw,ncol=lk)
     obliczone.p=matrix(1,nrow=lw,ncol=lk)

     rownames(M)=colnames(M)=names(dane)[kt]
     
     for (i in c(1:lw))
     {  
       for (j in c(1: lk))
       { 

         n[i,j] <- length(which(complete.cases(dane[,kt[i]],dane[,kt[j]])))

         if(n[i,j]>2)
         {
           M[i,j]<-cor(dane[,kt[i]],dane[,kt[j]], use="pairwise.complete.obs", method=met)
           M[i,j] <- round(M[i,j],2)
          
           res1<-cor.mtest(dane[,kt[c(i,j)]], method=met)
           obliczone.p[i,j]<-res1$p[1,2]
           obliczone.p[i,j] <- round(obliczone.p[i,j],4)
    
        
        if(rodz==1)
        {flaga.rys=0}
        czy.dobre.wspolcz=0
        if(!dwie.macierze & i!=j)
        {
          czy.dobre.wspolcz=1
        }
        if(dwie.macierze & ((i<=n1 & j>n1) | (i>n1 & j<=n1) ) )
        {
          czy.dobre.wspolcz=1
        }
            if(is.na(obliczone.p[i,j]))
            {
              flaga2=1
            } else {
              flaga2 = obliczone.p[i,j]
            }
        
           if(rysuj.rozrzuty & (flaga2<=0.05)& flaga.rys==0 & czy.dobre.wspolcz)
             { # korelacja istotna statystycznie i chcemy j¹ rysowaæ
             flaga.rys=1
             x=dane[,kt[i]]
             y=dane[,kt[j]]
             tytul_wykresu=paste(do.tytulow.ktore[rodz]," kor. " , do.tytulow.metoda[rodz],"\n",do.nazwy.pliku)
             nazwa_wykresu <- paste(mainDir,"/",subDirR,"/",names(dane)[kt[i]] , " vs " ,names(dane)[kt[j]] ," ",do.nazwy.pliku,".png",sep="")
             
             if (file.exists(nazwa_wykresu))
             {
               file.remove(nazwa_wykresu)
             }
             
             png(filename=nazwa_wykresu,
                 type="cairo",
                 units="in",
                 width=15,
                 height=12,
                 pointsize=12,
                 res=600)
             plot(x,y,xlab=names(dane)[kt[i]],ylab=names(dane)[kt[j]],pch = 19, frame = FALSE)
             abline(lm(y ~ x), col = "blue")
            
             if (names(dev.cur()) != "null device") dev.off()
             
           } # koniec if(rysuj.rozrzuty & (obliczone.p[i,j]<=0.05))
         rpn[i,j]=paste("r=",M[i,j],",\n ", "n=",n[i,j],", p=", obliczone.p[i,j],sep="")
         } else { # jest <=2 przypadkow
           M[i,j]=0
           obliczone.p[i,j]=0
           n[i,j]=0
           rpn[i,j]="b.d., n<=2"
           
         }
       }
     }

     if (dwie.macierze)
     {indeksy1_temp=indeksy1
     indeksy2_temp=indeksy2
       if(sa.porzadkowe & rodz==1)
       {
         ile.mniej.usunac.wierszy=length(which(wektor.porzadkowych<=n1))
         if(n1>ile.mniej.usunac.wierszy)
           {indeksy1_temp=c(1:(n1-ile.mniej.usunac.wierszy))}
       # } else {
       #     indeksy1_temp=c(NULL)}
         indeksy2_temp=c((n1-ile.mniej.usunac.wierszy+1):(ncol(M)))
       }
nazwy.wierszy=row.names(M)[-indeksy1_temp]
nazwy.kolumn=colnames(M)[-indeksy2_temp]

       M=M[-indeksy1_temp,-indeksy2_temp]
       n=n[-indeksy1_temp,-indeksy2_temp]
       rpn=rpn[-indeksy1_temp,-indeksy2_temp]
       obliczone.p=obliczone.p[-indeksy1_temp,-indeksy2_temp]
       if(is.vector(rpn))
       {
         rpn=matrix(rpn,ncol=n1)
         M=matrix(M,ncol=n1)
         n=matrix(n,ncol=n1)
         obliczone.p=matrix(obliczone.p,ncol=n1)
         row.names(M)=row.names(rpn)=row.names(n)=row.names(obliczone.p)=nazwy.wierszy
         colnames(M)=colnames(rpn)=colnames(n)=colnames(obliczone.p)=nazwy.kolumn
         
       }
     }

     if (rysuj.elipsy) {
       tytul_wykresu=paste(do.tytulow.ktore[rodz]," kor. " , do.tytulow.metoda[rodz],"\n",do.nazwy.pliku)
       nazwa_wykresu <- paste(sciezki.wykresow[rodz],"/", do.tytulow.ktore[rodz], " kor. " , do.tytulow.metoda[rodz]," ",do.nazwy.pliku,".png",sep="")

       if (file.exists(nazwa_wykresu))
       {
         file.remove(nazwa_wykresu)
       }

       png(filename=nazwa_wykresu,
           type="cairo",
           units="in",
           width=15,
           height=12,
           pointsize=12,
           res=600)

       corrplot(M,method="circle",type="upper",tl.col="red",tl.srt = 90,p.mat=obliczone.p,sig.level = c(0.001,0.01,0.05),
                insig="blank",addCoef.col = "black",diag=T,title=tytul_wykresu,tl.pos = "td",mar=c(0,0,4,0),tl.cex=0.4,number.cex = 0.3)

       if (names(dev.cur()) != "null device") dev.off()
     } # koniec if (rysuj.elipsy)


     arkusz = createSheet(wb, do.tytulow.metoda[rodz])
     rownames(rpn)=rownames(n)=rownames(obliczone.p)=rownames(M)
     colnames(rpn)=colnames(n)=colnames(obliczone.p)=colnames(M)

     if(!is.null(nazwa.cech.w.wierszach) & !is.null(nazwa.cech.w.kolumnach))
     {
       zapisz.do.pliku.macierze.korelacji(rpn,obliczone.p,M,arkusz,nazwa.cech.w.wierszach=nazwa.cech.w.wierszach,nazwa.cech.w.kolumnach = nazwa.cech.w.kolumnach,wb=wb,alfa.do.pliku = alfa.do.pliku)
     }
     if(!is.null(nazwa.cech.w.wierszach) & is.null(nazwa.cech.w.kolumnach))
     {
       zapisz.do.pliku.macierze.korelacji(rpn,obliczone.p,M,arkusz,nazwa.cech.w.wierszach=nazwa.cech.w.wierszach,wb=wb,alfa.do.pliku = alfa.do.pliku)
     }
     if(is.null(nazwa.cech.w.wierszach) & !is.null(nazwa.cech.w.kolumnach))
     {
       zapisz.do.pliku.macierze.korelacji(rpn,obliczone.p,M,arkusz,nazwa.cech.w.kolumnach = nazwa.cech.w.kolumnach,wb=wb,alfa.do.pliku = alfa.do.pliku)
     }
     if(is.null(nazwa.cech.w.wierszach) & is.null(nazwa.cech.w.kolumnach))
     {
       zapisz.do.pliku.macierze.korelacji(rpn,obliczone.p,M,arkusz,wb=wb,alfa.do.pliku = alfa.do.pliku)
     }

     wynik[[rodz]]=list(do.tytulow.metoda[[rodz]],rpn,M,obliczone.p,n)

  }

  # -------------------- KONIEC PETLI DLA 2 TYPOW KORELACJI ------------------------

    saveWorkbook(wb, paste("Wyn. kor. ",do.nazwy.pliku,".xlsx",sep=""))
 # return(wynik)

}
# return() nie musi byc return, ale musi to byc jedna wartosc
# jesli w return ktos chce zwrocic wiecej niz jedna wartosc, trzeba z nich zrobic liste i return(lista)

