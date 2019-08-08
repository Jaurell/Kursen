lim <- c(-1,1)
items <- c(paste0("s", 1:6),paste0("c", 1:6))

fit <- fit_oblique
fit_no_rotation$model
plot(fit_no_rotation$scores, xlim = lim, ylim = lim)
plot(fit_orthogonal$scores, xlim = lim, ylim = lim)
plot(fit_oblique$scores, xlim = lim, ylim = lim)
?lines()
colnames(fit_orthogonal$scores)
plot(fit$scores)
fit_lm <- lm(PA1~PA2, as.data.frame(fit$scores))
abline(fit_lm)
summary(fit_lm)
?biplot
biplot(fit_no_rotation)

fit <- principal(rdata[items], nfactors = 2, rotate = "oblimin")
biplot(fit)
?biplot
