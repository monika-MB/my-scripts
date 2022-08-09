wybor.wygladzania<<-function(x,y,czy.rysowac=TRUE,...) 
{
suppressPackageStartupMessages(require(drc))
suppressPackageStartupMessages(require(aomisc))
          
df=data.frame(n=x,ig=y)

ktory=c("wykladniczy","asymptotyczny","potegowy","logarytmiczny","krzywa rentownosci","krzywa plonow i chwastow", "logistyczny 3",
        "logistyczny 4", "Gompertza 2", "Gompertza 3", "Gompertza 4","log-logistyczny 2", "log-logistyczny 3", "log-logistyczny 4",
        "Weibulla typ 1 2", "Weibulla typ1 3", "Weibulla typ 1 4", "Weibulla typ 2 2","Weibulla typ 2 3", "Weibulla typ 2 4")
funkcje=list(DRC.expoDecay(),DRC.asymReg(),DRC.powerCurve(),DRC.logCurve(),DRC.YL(),DRC.cousens85(),L.3(),L.4(),G.2(),G.3(),G.4(),
             LL.2(),LL.3(),LL.4(),W1.2(),W1.3(),W1.4(),W2.2(),W2.3(),W2.4())

ile.modeli=length(funkcje)
rse=999999
model=NA

for (i in c(1:ile.modeli)) {

     wynik=tryCatch({drm(ig~n,fct=funkcje[[i]],data=df,...)},
                    error=function(cond){
                            message("Pojawi? si? error")
                        return(NA)    
                    })
     if(!is.na(wynik))
     {
             rse.temp=summary(wynik)$rseMat[1]
             if(rse.temp<rse)
             {
                  model=wynik
                  rse=rse.temp
                  typ=ktory[i]
             }
     } # koniec if(!is.na(wynik))
} # koniec for (i in c(1:ile.modeli))


list(model=model,typ=typ,rse=rse)

}