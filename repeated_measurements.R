rm(list = ls())
path <- "~/github/Kursen/"
setwd(path)

ci_tol <- 1.96
measure <- c()
medel  <- c()
standardavvikelse <- c()
standarderror <-c()
error_rate <- 0
iter <- 20
samplesize <- 200
breaks <- seq(-20,20,1)

for (x in 1:iter){
  sample <- rnorm(samplesize, mean = 0, sd=5)
  sample_sem <- sd(sample)/sqrt(length(sample))
  sample_ci <- c(mean(sample)-ci_tol*sample_sem,mean(sample)+ci_tol*sample_sem)
  if (sample_ci[1] > 0 || sample_ci[2] < 0){
    error_rate <- error_rate + 1   }
  measure <- c(measure, sample)
  m <- round(mean(measure),3)
  sdt <- round(sd(measure), 3)
  sem<- round(sd(measure)/sqrt(length(measure)),3)
  ci <- c(mean(measure)-ci_tol*sem,mean(measure)+ci_tol*sem)
  hist(measure, xlim = c(-20,20), ylim = c(0,iter*samplesize/5), main = paste0("n = ",length(measure),"\nmean = ",m, " \nsd= ",sdt, "\nsem = ",sem), breaks = breaks)
  if (ci[1] > 0){
    abline(v = ci[1], col = "red")
  }else{
    abline(v = ci[1], col = "green")
  }
  if (ci[2] < 0){
    abline(v = ci[2], col = "red")
  }else{
    abline(v = ci[2], col = "green")
  }
  medel <- c(medel ,m)
  standardavvikelse <- c(standardavvikelse, sdt)
  standarderror <- c(standarderror, sem)

  Sys.sleep(.1)
}

error_rate/(iter)
plot(medel, ylim = c(-.5,.5), ... = abline(c(.5,0)))
plot(standardavvikelse, ylim = c(4.5,5.5), ... = abline(.5,0))
plot(standarderror, ylim = c(0,1))

?plot
?hist
