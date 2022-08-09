ocena<<-function(macierz.bledow,nazwa.klasyfikatora="brak nazwy klasyfikatora",rysuj=FALSE){
  # macierz postaci |TP  FP |
  #                 | FN TN |
  # 
  tp=macierz.bledow[1,1]
  fp=macierz.bledow[1,2]
  fn=macierz.bledow[2,1]
  tn=macierz.bledow[2,2]
  
  acc=(tp+tn)/(tp+tn+fp+fn)
  sen=tp/(tp+fn)
  spe=tn/(tn+fp)
  pre=tp/(tp+fp)
  f1=2*pre*sen/(pre+sen)
  
  if(rysuj)
  {
    jakosc=c(acc,sen,spe,pre,f1)
    nazwy=c("dokladnosc","czulosc","specyficznosc","precyzja","F1")
    kol=c("slategray2","seashell2","lightpink2","wheat2",'cornflowerblue')
    while(names(dev.cur()) != "null device") dev.off()  
    png(paste(nazwa.klasyfikatora,".png",sep=""),width=1000,height=800)
    barplot(jakosc,col=kol,main=nazwa.klasyfikatora,names=nazwy,ylim=c(0,1))
    dev.off()
  }
  jakosc.ramka=round(data.frame(acc,sen,spe,pre,f1),2)
  return(jakosc.ramka)
}