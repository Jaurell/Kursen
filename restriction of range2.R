rm(list = ls())
n <- 100
rho <- .7
C <- matrix(rho, nrow = 2, ncol = 2)
diag(C) <- 1

x1 <- rep(seq(1,5),20)


C <- chol(C)

x2 <- rnorm(n, mean = -2)
X <- cbind(x1,x2)

# induce correlation (does not change X1)
df <- as.data.frame(X %*% C)
names(df) <- c("class", "aptitude")



require(ggplot2)
#require(lm.beta)

df.restricted <- df
df.restricted2 <- df[ which(df$class < 4),]

df.restricted["aptitude"][ which(df.restricted$aptitude < 0),] <- 0
df.restricted["aptitude"][ which(df.restricted$aptitude > 2),] <- 2

cor.text <- as.character(round(cor(df)[1,2],3))
cor.text2 <- as.character(round(cor(df.restricted)[1,2],3))
cor.text3 <- as.character(round(cor(df.restricted2)[1,2],3))




qplot(x = class, y = aptitude, data = df.restricted2) + geom_smooth(method="lm", se = F) +
  scale_y_continuous(limits = c(-3, +5)) + annotate("text", x = 2, y = -2, label = cor.text3) 

qplot(x = class, y = aptitude, data = df.restricted) + geom_smooth(method="lm", se = F) +
  scale_y_continuous(limits = c(-3, +5)) + annotate("text", x = 3, y = -2, label = cor.text2) 

qplot(x = class, y = aptitude, data = df) + geom_smooth(method="lm", se = F) +
  scale_y_continuous(limits = c(-3, +5)) + annotate("text", x = 3, y = -2, label = cor.text)
