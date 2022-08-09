shapiro.test.try.catch.p.value <<- function(x){

  tryCatch(
    expr = {
      sh = shapiro.test(x)$p.value
      return(sh)
    },
    error = function(e){
      sh=0
      return(sh)
    },
    warning = function(w){
      sh = shapiro.test(x)$p.value
      return(sh)
    }
  )    
}