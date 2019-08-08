rm(list = ls())
path <- "~/github/Kursen/"
setwd(path)

require(psych)
require(GPArotation)

datapath <- "data/nsfs_agreement_sim.csv"
#datapath <- "data/nsfs_extent_sim.csv"
rdata <- read.csv(datapath)

items <- c("a1","a3","a6","c1","c3","c6")


fit_no_rotation <- fa(rdata[items], nfactors = 2, fm = "pa", rotate = "none")
fit_orthogonal <- fa(rdata[items], nfactors = 2, fm = "pa", rotate = "varimax")
fit_oblique <- fa(rdata[items], nfactors = 2, fm = "pa", rotate = "oblimin")




pfa.eigen<-eigen(cor(rdata[items]))
# Print and note that eigen values are those produced by SPSS.
# Also note that SPSS will extract 2 components as eigen values > 1 = 2
plot(pfa.eigen$values, type = "o")
# set a value for the number of factors (for clarity)
factors<-2
# Extract and transform two components.
pfa <- pfa.eigen$vectors [ , 1:factors ]  %*% 
  + diag ( sqrt (pfa.eigen$values [ 1:factors ] ),factors,factors )

require(psych)
test <- principal(rdata[items], nfactors = 2, rotate = "none")
test$loadings[] + pfa
pfa
smc(rdata[items])
length(fit_no_rotation$communality.iterations)

factors<-2
cormat_org <- cor(rdata[items])
comm <- smc(rdata[items])
cormat <- cormat_org

for (x in 1:3){
  diag(cormat) <- comm
  pfa.eigen <- eigen(cormat)
  print(pfa.eigen$values)
  pfa <- pfa.eigen$vectors [ , 1:factors ]  %*% 
    + diag ( sqrt (pfa.eigen$values [ 1:factors ] ),factors,factors )
  comm <- rowSums(pfa**2)
}
pfa
fit_no_rotation$loadings[]
pfa.eigen$vectors

