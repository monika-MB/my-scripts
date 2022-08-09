loess.sd.MNB<<-function (x, y = NULL, nsigma = 1 ,czy.usuwac.outliery=TRUE,span1=0.75,span.sd=0.75,...) 
{
   xy <- xy.coords(x, y)
   x <- xy$x
   y <- xy$y
   nsigma <- as.numeric(nsigma)
   mod <- loess(y ~ x,span=span1,...) # wszystkie wyniki w kolejnosci oryginalnej
   r <- residuals(mod) # kolejnosc oryginalna
   if(czy.usuwac.outliery)
   {
      do.usun=which(r %in% boxplot(r,plot=F,range=qnorm(0.995))$out)
      if(length(do.usun)>0)
      {
      r=r[-do.usun]
      x=x[-do.usun]
      y=y[-do.usun] 
      } # koniec if(length(do.usun)>0)

   }
   
   r.abs=abs(r)
   modr <- loess(r.abs ~ x,span= span.sd,...) # wszystkie wyniki w kolejnosci oryginalnej
   x0 <- sort(x)
   yfit <- predict(mod, data.frame(x = x0)) # posortowane wg x, zeby ladnie rysowalo lines()
   
  sd <-pmax(0,predict(modr, data.frame(x = x0)))
  sd.usr=0.5*sd+0.5*mean(sd,na.rm = T)

     list(model = mod, x = x0, y = yfit, sd = sd, upper = yfit + 
           nsigma * sd, lower = yfit - nsigma * sd, sd.usr=sd.usr)
}