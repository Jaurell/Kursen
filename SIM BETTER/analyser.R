#change to your local path
path <- "C:/Users/JOAUR001/Desktop/Kursen/R-script/SIM BETTER"
setwd(path)

require(lavaan)
require(semTools)
options(max.print=1000000)
source("functions.R")
datapath <- "nsfs_agreement_sim.csv"
#datapath <- "nsfs_extent_sim.csv"
rdata <- read.csv(datapath)

reversed_items <- c("a2", "a4", "a5", "s2", "s4", "s5", "c1", "c3", "c6")
rdata[reversed_items] <- 6 - rdata[reversed_items]

fitm <- c("chisq", "df", "rmsea", "cfi", "tli", "srmr", "aic", "bic")

#3 correlated factors
model1 <- "
          autonomy   =~ a1 + a2 + a3 + a4 + a5 + a6
          social     =~ s1 + s2 + s3 + s4 + s5 + s6
          competence =~ c1 + c2 + c3 + c4 + c5 + c6
"
fit1 <- cfa(model = model1, data = rdata, mimic = "Mplus")
write.output(fit1, "output/3_factors_cfa", model1)

#3 UNcorrelated factors
model2 <- "
#factor definition
          autonomy   =~ a1 + a2 + a3 + a4 + a5 + a6
          social     =~ s1 + s2 + s3 + s4 + s5 + s6
          competence =~ c1 + c2 + c3 + c4 + c5 + c6
#set correlation to 0
          autonomy ~~ 0*social
          autonomy ~~ 0*competence
          social ~~ 0*competence
"

fit2 <- cfa(model = model2, data = rdata, mimic = "Mplus")
write.output(fit2, "output/3_factors_cfa_orthogonal", model2)

#6 factors
model3 <- "
          as =~ a1 + a3 + a6
          af =~ a2 + a4 + a5
          ss =~ s1 + s3 + s6
          sf =~ s2 + s4 + s5
          cs =~ c2 + c4 + c5
          cf =~ c1 + c3 + c6
"
fit3 <- cfa(model = model3, data = rdata, mimic = "Mplus")
write.output(fit3, "output/6_factors_cfa", model3)

#sem models
model4 <- "
          autonomy   =~ a1 + a2 + a3 + a4 + a5 + a6
          social     =~ s1 + s2 + s3 + s4 + s5 + s6
          competence =~ c1 + c2 + c3 + c4 + c5 + c6
          swls ~ autonomy + social + competence
"
fit4 <- sem(model = model4, data = rdata, mimic = "Mplus")
write.output(fit4, "output/3_factors_sem_swls", model4, cfa = F)

model5 <- "
          as =~ a1 + a3 + a6
          af =~ a2 + a4 + a5
          ss =~ s1 + s3 + s6
          sf =~ s2 + s4 + s5
          cs =~ c2 + c4 + c5
          cf =~ c1 + c3 + c6
          swls ~ as + af + ss + sf + cs + cf
"
fit5 <- sem(model = model5, data = rdata, mimic = "Mplus")
write.output(fit5, "output/6_factors_sem_swls", model5, cfa = F)






