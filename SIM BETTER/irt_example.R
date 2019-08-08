rm(list = ls())
setwd("~/Dropbox/Kurs_2019//R-script/SIM BETTER")
source("functions.R")
require(mirt)
require(ggplot2)
#rdata <- read.csv("nsfs/nsfs_agreement_sim.csv")
rdata <- read.csv("nsfs_extent_sim.csv")

reversed_items <- c("a2", "a4", "a5", "s2", "s4", "s5", "c1", "c3", "c6")
rdata[reversed_items] <- 6 - rdata[reversed_items]

###################
#gpcmIRT estimates difficulty (location) and discrimination (slope) for each item
#grsmIRT estimates difficulty (location) and assumes the same discrimination (slope) for all items
itemtype.choice <- "gpcmIRT"
##################

autonomy <- c( "a1",  "a2",  "a3",  "a4", "a5", "a6")
autonomy <- autonomy[-c(2,4,5)]
A.mod <- mirt(data = rdata[autonomy], model = 1, itemtype = itemtype.choice)
M2(A.mod, type = "C2")
summary(A.mod)
itemfit(A.mod)
coef(A.mod, IRTpars =  T, simplify = T)
plot(A.mod, type = "trace")
plot(A.mod, type = 'itemscore')
plot(A.mod, type = "info")

rdata["autonomy_fscore"] <- fscores(A.mod)[,1]
rdata["autonomy_personfit"] <- personfit(A.mod)
rdata["autonomy_expected"] <- expected.test(A.mod, as.matrix(rdata["autonomy_fscore"]), lenght(autonomy))
rdata["autonomy_true"] <- rowSums(rdata[autonomy])

aplot <- plot(A.mod)
ggplot() +
  geom_line(aes(x = aplot$panel.args[[1]]$x, y = aplot$panel.args[[1]]$y)) + 
  geom_point(data = rdata, aes(x = autonomy_fscore, y= autonomy_true)) 

plot_item_vs_expected(A.mod, rdata, autonomy, "autonomy_fscore")

cor(rdata["autonomy_fscore"],rdata["swls"])
cor(rowSums(rdata[autonomy]), rdata["swls"])
cor(rowSums(rdata[autonomy]), rdata["autonomy_fscore"])

relatedness <- c("s1",  "s2", "s3",  "s4",  "s5",  "s6")
#relatedness <- relatedness[-c(1,3,6)]
R.mod <- mirt(data = rdata[relatedness], model = 1, itemtype = itemtype.choice)
M2(R.mod, type = "C2")
summary(R.mod)
coef(R.mod, IRTpars =  T, simplify = T)
plot(R.mod, type = "trace")
plot(R.mod, type = 'itemscore')
plot(R.mod, type = "info")
itemfit(R.mod)
rdata["relatedness_fscore"] <- fscores(R.mod)[,1]
rdata["relatedness_personfit"] <- personfit(R.mod)
rdata["relatedness_true"] <- rowSums(rdata[relatedness])

rplot <- plot(R.mod)
ggplot() +
  geom_line(aes(x = rplot$panel.args[[1]]$x, y = rplot$panel.args[[1]]$y)) + 
  geom_point(data = rdata, aes(x = relatedness_fscore, y= relatedness_true)) 

plot_item_vs_expected(R.mod, rdata, relatedness, "relatedness_fscore")

cor(rdata["relatedness_fscore"],rdata["swls"])
cor(rowSums(rdata[7:12]), rdata["swls"])

competence <- c("c1", "c2", "c3", "c4", "c5", "c6")
#competence <- competence[-c(3)]
C.mod <- mirt(data = rdata[competence], model = 1, itemtype = itemtype.choice)
M2(C.mod, type = "C2")
summary(C.mod)
coef(C.mod, IRTpars =  T, simplify = T)
plot(C.mod, type = "trace")
plot(C.mod, type = 'itemscore')
plot(C.mod, type = "info")
itemfit(C.mod)
rdata["competence_fscore"] <- fscores(C.mod)[,1]
rdata["competence_personfit"] <- personfit(C.mod)
rdata["competence_true"] <- rowSums(rdata[competence])

cplot <- plot(C.mod)
ggplot() +
  geom_line(aes(x = cplot$panel.args[[1]]$x, y = cplot$panel.args[[1]]$y)) + 
  geom_point(data = rdata, aes(x = competence_fscore, y= competence_true)) 

plot_item_vs_expected(C.mod, rdata, competence, "competence_fscore")

cor(rdata["competence_fscore"],rdata["swls"])
cor(rowSums(rdata[13:18]), rdata["swls"])

