rysuj.wykres<<-function(df, nazwa.osi.x, nazwa.osi.y,etykiety.osi.x,nazwa.wykresu,kolory,typ="v",tytul=NULL){
  rozmiar.tytulu = 1.5
  suppressPackageStartupMessages(library("vioplot"))
  suppressPackageStartupMessages(library("randomcoloR"))

  liczba.poziomow=ncol(df)
  
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
  
  zmienne<-vector("list",liczba.poziomow)
  for(i in c(1:liczba.poziomow))
  {
    zmienne[[i]]=as.numeric(df[,i])
  }
  

  while (names(dev.cur()) != "null device"){
  dev.off()}
  if(typ=="v")
  {
    nazwa.wykresu = paste(nazwa.wykresu,".png",sep="")
  } else {
    nazwa.wykresu = paste(nazwa.wykresu," bp.png",sep="")
  }
  png(filename=nazwa.wykresu,type="cairo", units="in",width=6,height=6, pointsize=12, res=600)
if(typ=="v")
{
  eval(parse(text=paste("vioplot(",wsad.wykres,"col=kolory , names=etykiety.osi.x)",sep="")))
  } else {
  
    boundaries<-boxplot(zmienne,plot=FALSE)
    najmn_y=round(min(min(boundaries$stats[1,])*0.9,min(boundaries$stats[1,]-1)))
    najw_y=round(max(max(boundaries$stats[1,])*1.1,max(boundaries$stats[nrow(boundaries$stats),]+1))*1.1)
    boxplot(zmienne,col=kolory, names=etykiety.osi.x,border="black",las=1,cex.axis=0.7,ylim=c(najmn_y,najw_y),par(mar=c(c(6.1, 4.1, 4.1, 2.1))))
    n=matrix(0,nrow=1,ncol=liczba.poziomow)
    for (i in 1:liczba.poziomow){
      n[1,i] = length(zmienne[[i]][!is.na(zmienne[[i]])])
    }
    if(najw_y<1)
    {
      text(x=c(1:liczba.poziomow), y=boundaries$stats[nrow(boundaries$stats),]*1.1, paste("n = ",n,sep="") )
    } else
    {
      igreki2=boundaries$stats[nrow(boundaries$stats),]*1.1
      igreki2[igreki2<2]=2
      text(x=c(1:liczba.poziomow), y=igreki2, paste("n = ",n,sep="") )
    }
    
}
if(is.null(tytul)){
  title(ylab=nazwa.osi.y,xlab=nazwa.osi.x,cex.main=rozmiar.tytulu)
} else {
  title(main=tytul,ylab=nazwa.osi.y,xlab=nazwa.osi.x,cex.main=rozmiar.tytulu)
  }
  if (names(dev.cur()) != "null device") dev.off()
}  
  