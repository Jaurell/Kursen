rm(list = ls())

#change to your local path
path <- "C:/Users/JOAUR001/Dropbox/Kurs_2019/R-Script"
setwd(path)

require(haven)
require(lavaan)
require(semTools)
require(psych)
require(GPArotation)
options(max.print=1000000)
source("rotation_functions.R")
datapath <- "data/hads_data5.sav"
#datapath <- "data/nsfs_extent_sim.csv"
Bechtoldtd_spss(datapath)
items <- paste0("hads_",letters[seq(1,14,by = 1)])
Bechtoldta(Bechtoldt)
Bechtoldt
#items <- c("a1","a3","a6","c1","c3","c6")
#items <- c(paste0("hads", 1:6),paste0("a", 1:6))
scree(Reise)
fit_no_rotation <- fa(Reise, nfactors = 2, fm = "pa", rotate = "none")
fit_orthogonal <- fa(Bechtoldt, nfactors = 2, fm = "pa", rotate = "varimax")
fit_oblique <- fa(Bechtoldt, nfactors = 2, fm = "pa", rotate = "oblimin")
fit_no_rotation
steps <- 20
lim <- c(-1,1)
factor_col <- paste0("hads_",letters[seq(1,14,by = 2)])
plot_fit(fit_no_rotation, factor_col)
items <- colnames(Bechtoldt)

rotate_axes(fit_orthogonal, steps)
rotate_data(fit_orthogonal, steps, factor_col)
plot_fit(fit_orthogonal)

rotate_axes(fit_oblique, steps)
rotate_data(fit_oblique, steps, factor_col)
plot_fit(fit_oblique)

plot_fit(fit_no_rotation)


data(Thurstone)
