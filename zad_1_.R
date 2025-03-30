library(genalg)

evaluate <- function(X=c()){
  returnVal = NA;
  uslugi1 <- array(c(2, 0, 0, 4, 8, 1, 0))
  uslugi2 <- array(c(0, 2, 1, 0, 3, 4, 5))
  uslugi3 <- array(c(1, 0, 1, 0, 3, 0, 1))
  uslugi4 <- array(c(7, 2, 0, 5, 0, 0, 9))
  uslugi5 <- array(c(0, 4, 0, 1, 0, 11, 0))
  uslugi6 <- array(c(3, 6, 10, 0, 0, 0, 0))
  uslugi7 <- array(c(1, 0, 0, 8, 0, 1, 3))
  uslugi8 <- array(c(0, 9, 0, 0, 1, 4, 0))
  uslugi <- list(uslugi1, uslugi2, uslugi3, uslugi4, uslugi5, uslugi6, uslugi7, uslugi8)
  wyniki <- numeric(length(uslugi))
  
  for (i in seq_along(uslugi)) {
    wyniki[i] <- sum(uslugi[[i]] * X)
  }
  granica <- array(c(148, 87, 234, 176, 23, 845, 234, 111))
  if( any(wyniki>granica)){
    returnVal = 100000000000000000
  } else{
    cena <- array(c(-50, -237, -128, -34, -27, -239, -78, -99))
    returnVal = sum(cena * wyniki)
  }
  returnVal
}

rbga.results = rbga(c(0,0,0,0,0,0,0), c(25,5,84,23,18,2,17), popSize=300, evalFunc=evaluate, iters=500, verbose=TRUE, mutationChance=0.01)

genalg:::summary.rbga(rbga.results,echo=TRUE)

plot(rbga.results)
