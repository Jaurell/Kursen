rm(list = ls())
setwd("~/Dropbox/Kurs_2019//R-script/SIM BETTER")
source("functions.R")
require(DAKS)
require(mirt)
require(ggplot2)
#rdata <- read.csv("nsfs/nsfs_agreement_sim.csv")
rdata <- pisa

#reversed_items <- c("a2", "a4", "a5", "s2", "s4", "s5", "c1", "c3", "c6")
#rdata[reversed_items] <- 6 - rdata[reversed_items]

###################
#gpcmIRT estimates difficulty (location) and discrimination (slope) for each item
#grsmIRT estimates difficulty (location) and assumes the same discrimination (slope) for all items
itemtype.choice <- "Rasch"
##################


A.mod <- mirt(data = rdata, model = 1, itemtype = itemtype.choice)
M2(A.mod, type = "C2")
summary(A.mod)
itemfit(A.mod)
coef(A.mod, IRTpars =  T, simplify = T)
plot(A.mod, type = "trace")
plot(A.mod, type = 'itemscore')
plot(A.mod, type = "info")

personfit(A.mod)

rdata["pisa_fscore"] <- fscores(A.mod)[,1]
rdata["pisa_personfit"] <- personfit(A.mod)
rdata["pisa_expected"] <- expected.test(A.mod, as.matrix(rdata[1:5]), 5)
rdata["pisa_true"] <- rowSums(rdata[1:5])

aplot <- plot(A.mod)
ggplot() +
  geom_line(aes(x = aplot$panel.args[[1]]$x, y = aplot$panel.args[[1]]$y)) + 
  geom_point(data = rdata, aes(x = pisa_fscore, y= pisa_true)) 

summary(rdata$pisa_fscore)

plot_item_vs_expected(A.mod, rdata, 1:5, "pisa_fscore")

plot_item_vs_expected(A.mod, rdata, autonomy, "autonomy_fscore")

cor(rdata["autonomy_fscore"],rdata["swls"])
cor(rowSums(rdata[autonomy]), rdata["swls"])
cor(rowSums(rdata[autonomy]), rdata["autonomy_fscore"])