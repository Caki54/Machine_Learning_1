library(genalg)

evaluate <- function(X=c()) {
  returnVal = NA;
  tab1 <- c(3400, 2400, 2400, 1400, 1200, 3400, 1300, 2500)
  tab2 <- c(5600, 4600, 5100, 2400, 2200, 3400, 1300, 2500)
  wagi <- tab1
  sprx <- which(X<3)
  if(length(sprx)>0){
    wagi[sprx] <- tab2[sprx]
  }
  a = sum(wagi * X);
  if (a<150000) {
    returnVal = -200000*X[1]-130000*X[2]-145000*X[3]-75000*X[4]-55000*X[5]-210000*X[6]-150000*X[7]-20000*X[8];
  } else {
    returnVal =   1000000000000000000000000
  }
  returnVal
}

rbga.results = rbga(c(0,0,0,0,0,0,0,0),c(44,63,63,107,125,44,115,60),popSize=1000, evalFunc=evaluate,iters=500, verbose=TRUE, mutationChance=0.01)
genalg:::summary.rbga(rbga.results,echo=TRUE)

plot(rbga.results)

