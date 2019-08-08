rm(list = ls())
path <- "~/github/Kursen/"
setwd(path)
source("bicop.R")
df <- getBiCop(200, .7)
df <- df+3
g <- c(-0,6)


plot(df, xlim = g, ylim = g)
dfreg <- lm(formula = df[,1] ~ df[,2])
abline(dfreg, col = "red")


bias_select_df <- df[df[,1 ]> 2,]
plot(bias_select_df, xlim = g, ylim = g)
xdfreg <- lm(formula = bias_select_df[,1] ~ bias_select_df[,2])
abline(xdfreg, col = "blue")
abline(dfreg, col = "red")


bias_floor_df <- df
bias_floor_df[bias_floor_df[,1 ] < 2,] <- 2
plot(bias_floor_df, xlim = g, ylim = g)
ydfreg <- lm(formula = bias_floor_df[,1] ~ bias_floor_df[,2])
abline(ydfreg, col = "green")
abline(dfreg, col = "red")


listanbo <- c()
for (n in 1:1000){
  df <- getBiCop(100, .7)
  xdf <- df[df[,1 ]> -1,]
  ydf <- df[df[,1 ]> -1,]
  bias_floor_df <- df
  bias_floor_df[bias_floor_df[,1 ] < -1,] <- -1
  dfreg <- coef(lm(formula = df[,1] ~ df[,2]))[2]
  ydfreg <- coef(lm(formula = bias_floor_df[,1] ~ ydf[,2]))[2]
  xdfreg <- coef(lm(formula = xdf[,1] ~ xdf[,2]))[2]
  x <- dfreg > ydfreg
  listanbo <- c(as.numeric(listanbo),as.numeric(x))
}

length(listanbo[listanbo == 0])/length(listanbo)
