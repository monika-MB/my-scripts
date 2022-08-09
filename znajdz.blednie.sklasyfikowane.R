znajdz.blednie.sklasyfikowane<<-function(split,referencja,wynik.klasyfikacji,wypisz=TRUE){
  # macierz postaci |TP  FP |
  #                 | FN TN |
  # 

  wiersze.testowane=which(split==FALSE)
  n=length(wynik.klasyfikacji)
  
  bledna.klasyfikacja=data.frame(wiersz=c(NULL),rzeczywistosc=c(NULL),sklasyfikowane.jako=c(NULL))
  licznik=1
  for(i in 1:n)
  {
    if(referencja[i]!=wynik.klasyfikacji[i])
    {
      wiersz=wiersze.testowane[i]
      rzeczywistosc=referencja[i]
      sklasyfikowane.jako=wynik.klasyfikacji[i]
      bledna.klasyfikacja.dodaj=data.frame(wiersz,rzeczywistosc,sklasyfikowane.jako)
      bledna.klasyfikacja=rbind(bledna.klasyfikacja,bledna.klasyfikacja.dodaj)
      licznik=licznik+1
    }
  }
  if(wypisz)
  {
    print(bledna.klasyfikacja)
  }
  return(bledna.klasyfikacja)
}