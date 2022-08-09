setwd("C:/Users/Monika/Desktop")

library("xlsx")

tab.rpn=matrix(c("ja","ty","on","ona","2 koty","3 psy","1 krowa","3 konie","dom","mieszkanie","namiot","dom"),ncol=3)
tab.r=matrix(c(0.8,-0.27,0.1, -0.45, 0.62,0.03, -0.2,-0.93,0.06,0.57,0.31,-0.76),ncol=3)
tab.p=matrix(c(0.02,0.05,0.7,0.04,0.01,0.8,0.6,0.02,0.9,0.03,0.04,0.001),ncol=3)

rownames(tab.rpn)=rownames(tab.r)=rownames(tab.p)=c("1 osoba","2 osoba","meski","zenski")
colnames(tab.rpn)=colnames(tab.r)=colnames(tab.p)=c("kto","zwierze","zamieszkanie")

macierz.rpn=tab.rpn
macierz.r=tab.r
macierz.p=tab.p

wb=createWorkbook()
sheet=createSheet(wb,"arkusz")

nazwa.cech.w.kolumnach="kolumny"
nazwa.cech.w.wierszach="wiersze"

saveWorkbook(wb,"test.xlsx")
