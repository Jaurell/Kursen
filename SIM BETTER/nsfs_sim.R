path_to_data <- "nsfs_agreement_sim.csv"
nvars <- 25
nobs <- 585

org_data <- read.csv(file = path_to_data, sep = ",")
missing_values <- -99
org_data[org_data == -99] <- NA
varnames <- c(paste0("a",seq(1,6)), paste0("s",seq(1,6)), paste0("c",seq(1,6)), paste0("scas", seq(1,6)), "swls")
names(org_data) <- varnames

reversed_items <- c("a2", "a4", "a5", "s2", "s4", "s5", "c1", "c3", "c6")
org_data[reversed_items] <- 6 - org_data[reversed_items]
org_data[1:18] <- 6 - org_data[1:18]

covmat <- cor(org_data, use = "pairwise.complete.obs")
write.csv(covmat, "flik1.csv")
L <- chol(covmat) 
write.csv(L, "flik2.csv")
r <- t(L) %*% matrix(rnorm(nvars*nobs), nrow=nvars, ncol=nobs)
write.csv(r, "flik3.csv")
r <- t(r)
write.csv(r, "flik4.csv")

rdata = as.data.frame(r)

nsfs_cutvals <- c()
start <- 1
end <- 18
data <- org_data
def_cut_vals <- function(data, start, end){
  cut_vals <- c()
  for (v in start:end){
    freq <- cumsum(table(data[v])/sum(table(data[v])))
    cumfreq <- freq[-length(freq)]
    cut_vals <- c(cut_vals,cumfreq)
  }
  cut_vals <- as.data.frame(matrix(cut_vals, ncol = end - start + 1))
  names(cut_vals) <- names(data[start:end])
  return(cut_vals)
}


nsfs_cutvals <- def_cut_vals(org_data,1,18)
scas_cutvals <- def_cut_vals(org_data,19,24)
swls_cutvals <- def_cut_vals(org_data,25,25)
  
#rdata
source("functions.R")
nsfs <- make_int_data(rdata[1:18], nsfs_cutvals)
scas <- make_int_data(rdata[19:24], scas_cutvals)
swls <- make_int_data(rdata[25], swls_cutvals)

new_data <- cbind(nsfs,scas,swls)

utils::View(new_data)

write.csv(new_data, 'nsfs/nsfs_extent_sim.csv', row.names=FALSE)

