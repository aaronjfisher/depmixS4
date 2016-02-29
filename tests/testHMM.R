library(doMC)
library(depmixS4)
library(caret)
data(speed)

mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,family=list(gaussian(),multinomial()),ntimes=c(168,134,137))
set.seed(3)
fmod1 <- fit(mod1)