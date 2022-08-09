zaladuj<<-function(paczka){
  if(!(suppressWarnings(suppressMessages(require(paczka,character.only = TRUE))))){
    install.packages(as.character(paczka))
    suppressWarnings(suppressMessages(library(paczka,character.only = TRUE)))
  } 
}