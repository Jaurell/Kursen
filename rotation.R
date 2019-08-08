rm(list = ls())

#change to your local path
path <- "~/github/Kursen//R-Script"
setwd(path)

require(lavaan)
require(semTools)
require(psych)
require(GPArotation)
options(max.print=1000000)
source("rotation_functions.R")
datapath <- "data/nsfs_agreement_sim.csv"
#datapath <- "data/nsfs_extent_sim.csv"
rdata <- read.csv(datapath)

items <- c("a1","a3","a6","c1","c3","c6")
factor_col <- items[1:3]

fit_no_rotation <- fa(rdata[items], nfactors = 2, fm = "pa", rotate = "none")
fit_orthogonal <- fa(rdata[items], nfactors = 2, fm = "pa", rotate = "varimax", normalize = F)
fit_oblique <- fa(rdata[items], nfactors = 2, fm = "pa", rotate = "oblimin")

steps <- 30
lim <- c(-1,1)

plot_fit(fit_no_rotation, factor_col)


#rotate_axes(fit_orthogonal, steps, factor_col)
#rotate_data(fit_orthogonal, steps, factor_col)
plot_fit(fit_orthogonal, factor_col)
rotate_all(fit_orthogonal, steps, factor_col)



#rotate_axes(fit_oblique, steps, factor_col)
#rotate_data(fit_oblique, steps, factor_col)
plot_fit(fit_oblique, factor_col)
rotate_all(fit_oblique, steps, factor_col)




