
#Generates matrix from factorloadings. 
#Returns The Choleski Decomposition version of the correlationmatrix.
generate_matrix <- function(factorloadings = fload, chold = TRUE){
  fload <- factorloadings
  uniq <- 1-rowSums(fload^2)
  fact <- sim.structure(fload, items = TRUE, uniq = uniq)
  M <- fact$model
  if (chold == TRUE){
    L= chol(M)
    return(L)}
  else{return(M)}
}


#The Choleski Decomposition matrix is multiplied with the randomly created variables to induce correlation
#returns correlated data
simulate_data <- function(chol_matrix, ncol = nobs){
  rdata <- generate_matrix(chol_matrix)
  nvars <- dim(chol_matrix)[1]
  r = t(chol_matrix) %*% chol_matrix(rnorm(nvars*nobs), nrow=nvars, ncol=nobs)
  r = t(r)
  rdata = as.data.frame(r)
  return(rdata)
}

#used by function make_int_data
cutfunc <- function(var, list_of_cut, data, rows=nobs){
  df <- data.frame(matrix(ncol = 1, nrow = rows))
  new_var = paste('int',var, sep = "_")
  df[new_var] <- NA
  cut <- quantile(unlist(data[var]), list_of_cut)
  df[new_var][data[var] <= cut[1]] <- 1
  for (n in 2:length(list_of_cut)-1){
    df[new_var][(data[var] > cut[n]) & (data[var] <= cut[n+1])] <- n+1
  }
  df[new_var][(data[var] > cut[n+1])] <- n+2
  return(df[new_var])
}


#cutlist is a matrix of proportional cumulative frequencies of response categories (omit the 100% entery)
#takes data and cutlist and return data with integer responses
make_int_data <- function(data = rdata, cutlist = df_cut){
  varnames <- colnames(data)
  len <- length(data) + 1
  flag <- typeof(cutlist) == "double"
  if (flag == 1){avector <- cutlist}
  for (var in varnames){
    if (flag != 1){avector <- unlist(cutlist[var])}
    data[paste('int',var,sep = '_')] <- cutfunc(var = var, list_of_cut = avector, data = data)
  }
  int_data <- data[len:length(data)]
  names(int_data) <- varnames
  return(int_data)
}


#writes output from sem and cfa analysises
write.output <- function(model, outputname, mod.spec, cfa=T){
  write(paste0("Model specification:", mod.spec), file = sprintf("%s.txt", outputname))
  capture.output(summary(model, standardized=TRUE), file = sprintf("%s.txt", outputname), append = T)
  write("\nFit Measures:\n", file = sprintf("%s.txt", outputname), append = T)
  capture.output(round(fitmeasures(model)[fitm],3), file = sprintf("%s.txt", outputname), append = T)
  if (cfa == T){
  write("\nReliability:\n", file = sprintf("%s.txt", outputname), append = T)
  capture.output(reliability(model), file = sprintf("%s.txt", outputname), append = T)
  }
  write("\nModification Indices:\n", file = sprintf("%s.txt", outputname), append = T)
  capture.output(modificationindices(model, sort. = T), file = sprintf("%s.txt", outputname), append = T)
  
  write.table(round(resid(model)$cov,3), file = sprintf("%s_resid.csv", outputname), sep = ";")
}

plot_item_vs_expected <- function(mod, rdata, vars, fscore){
  for (v in vars){
    se_plot <- itemplot(mod, v, type = "score")
    the_plot <- ggplot() +
      geom_line(aes(x = se_plot$panel.args[[1]]$x, y = se_plot$panel.args[[1]]$y)) + 
      geom_point(data = rdata, aes(x = rdata[[fscore]], y= rdata[[v]])) + 
      annotate("text", label = v, x = -6, y = 3, color = "black")
    show(the_plot)
  }
}