# angle from x-axis
get_angle <- function(vec) {
  Arg(complex(real = vec[1], imaginary = vec[2]))
}

# unit vector
get_vector <- function(ang) {
  c(cos(ang), sin(ang))
}

get_intermediate_vectors <- function(first, last, steps) {
  first_angle <- get_angle(first)
  last_angle <- get_angle(last)
  delta_angle <- (first_angle - last_angle) / steps
  
  ret_val = c()
  for (ang in seq(first_angle, last_angle, -delta_angle)[2:steps+1]) {
    ret_val = c(ret_val, list(get_vector(ang)))
  }
  return(ret_val)
  #vector_from_angle <- get_vector(ang)
  #modifier <- (last/vector_from_angle)
  
  #lapply(ret_val, "*", modifier)
}

rotate_axes <- function(fit, steps, factor_col = FALSE){
  items <- colnames(fit$model)
  rotation_matrix <- fit$rot.mat
  v1 <- get_intermediate_vectors(c(1,0), rotation_matrix[,1],steps)
  v2 <- get_intermediate_vectors(c(0,1), rotation_matrix[,2],steps)
  
  plot(fit$loadings %*% solve(fit$rot.mat), xlim = c(-1,1), ylim = c(-1,1),
       main = fit$rotation, xlab = "Factor 1", ylab = "Factor 2", type = "n")
  
  if (length(factor_col) > 1){
    text(fit$loadings[factor_col,] %*% solve(fit$rot.mat), labels = factor_col, col = "blue")
    text(fit$loadings[items[!items %in% factor_col],] %*% solve(fit$rot.mat), labels = items[!items %in% factor_col], col = "red")
  }else{
    text(fit$loadings%*% solve(fit$rot.mat), labels = items)
  }
  
  abline(v =0, h= 0)
  Sys.sleep(.1)
  
  original_loadings <- fit$loadings %*% solve(fit$rot.mat)
  
  for (x in 1:(steps-1)){
    plot(original_loadings, xlim = c(-1,1), ylim = c(-1,1),
         main = fit$rotation, xlab = "Factor 1", ylab = "Factor 2", type = "n")
    if (length(factor_col) > 1){
      text(original_loadings[factor_col,], labels = factor_col, col = "blue")
      text(original_loadings[items[!items %in% factor_col],], labels = items[!items %in% factor_col], col = "red")
    }else{
      text(fit$loadings %*% solve(fit$rot.mat), labels = items)
    }
    
    
    r1 <- v1[[x]]
    r2 <- v2[[x]]
    abline(a =0, b= -r1[1]/r1[2])
    abline(a =0, b= -r2[1]/r2[2])
    Sys.sleep(.1)
  }
}

rotate_data <- function(fit, steps, factor_col = FALSE){
  items <- colnames(fit$model)
  rotation_matrix <- fit$rot.mat
  v1 <- get_intermediate_vectors(c(1,0), rotation_matrix[,1],steps)
  v2 <- get_intermediate_vectors(c(0,1), rotation_matrix[,2],steps)
  
  
  
  plot(fit$loadings %*% solve(fit$rot.mat), xlim = c(-1,1), ylim = c(-1,1),
       main = fit$rotation, xlab = "Factor 1", ylab = "Factor 2", type = "n")
  if (length(factor_col) > 1){
    text(fit$loadings[factor_col,] %*% solve(fit$rot.mat), labels = factor_col, col = "blue")
    text(fit$loadings[items[!items %in% factor_col],] %*% solve(fit$rot.mat), labels = items[!items %in% factor_col], col = "red")
  }else{
    text(fit$loadings %*% solve(fit$rot.mat), labels = items)
  }
  
  abline(v =0, h= 0)
  Sys.sleep(.1)
  
  original_loadings <- fit$loadings %*% solve(fit$rot.mat)
  
  for (x in 1:(steps-1)){
    loadmat <- matrix(c(v1[[x]],v2[[x]]),2)
    loadmat <- det(solve(loadmat)) * loadmat
    new_loadings <- original_loadings %*% loadmat
    plot(new_loadings, xlim = c(-1,1), ylim = c(-1,1), type = "n", 
         main = fit$rotation, xlab = "Factor 1", ylab = "Factor 2")
    if (length(factor_col) > 1){
      text(new_loadings[factor_col,], labels = factor_col, col = "blue")
      text(new_loadings[items[!items %in% factor_col],], labels = items[!items %in% factor_col], col = "red")
    }else{
      text(new_loadings, labels = items)
    }
    abline(v =0, h= 0)
    Sys.sleep(.1)
  }
  
}


rotate_all <- function(fit, steps, factor_col = FALSE){
  items <- colnames(fit$model)
  rotation_matrix <- fit$rot.mat
  v1 <- get_intermediate_vectors(c(1,0), rotation_matrix[,1],steps)
  v2 <- get_intermediate_vectors(c(0,1), rotation_matrix[,2],steps)
  # v1_back <- get_intermediate_vectors(rotation_matrix[,1], c(1,0), steps)
  # v2_back <- get_intermediate_vectors(rotation_matrix[,1], c(0,1), steps)
  
  
  plot(fit$loadings %*% solve(fit$rot.mat), xlim = c(-1,1), ylim = c(-1,1),
       main = fit$rotation, xlab = "Factor 1", ylab = "Factor 2", type = "n")
  if (length(factor_col) > 1){
    text(fit$loadings[factor_col,] %*% solve(fit$rot.mat), labels = factor_col, col = "blue")
    text(fit$loadings[items[!items %in% factor_col],] %*% solve(fit$rot.mat), labels = items[!items %in% factor_col], col = "red")
  }else{
    text(fit$loadings %*% solve(fit$rot.mat), labels = items)
  }
  
  abline(v =0, h= 0)
  Sys.sleep(.1)
  
  original_loadings <- fit$loadings %*% solve(fit$rot.mat)
  ##rotate axis
  for (x in 1:(steps-1)){
    plot(original_loadings, xlim = c(-1,1), ylim = c(-1,1),
         main = fit$rotation, xlab = "Factor 1", ylab = "Factor 2", type = "n")
    if (length(factor_col) > 1){
      text(original_loadings[factor_col,], labels = factor_col, col = "blue")
      text(original_loadings[items[!items %in% factor_col],], labels = items[!items %in% factor_col], col = "red")
    }else{
      text(fit$loadings %*% solve(fit$rot.mat), labels = items)
    }
    loadmat <- matrix(c(v1[[x]],v2[[x]]),2)
    loadmat <- det(solve(loadmat)) * loadmat
    r1 <- loadmat[,1]
    r2 <- loadmat[,2]
    abline(a =0, b= -r1[1]/r1[2])
    abline(a =0, b= -r2[1]/r2[2])
    Sys.sleep(.1)
  }
  #rotate axis and data back
  v1 <- c(list(c(1,0)),v1)
  v2 <- c(list(c(0,1)),v2)
  for (x in 1:(steps)){
    loadmat <- matrix(c(v1[[x]],v2[[x]]),2)
    loadmat <- det(solve(loadmat)) * loadmat
    
    new_loadings <- original_loadings %*% loadmat
    
    
    plot(new_loadings, xlim = c(-1,1), ylim = c(-1,1), type = "n",
         main = fit$rotation, xlab = "Factor 1", ylab = "Factor 2")
    if (length(factor_col) > 1){
      text(new_loadings[factor_col,], labels = factor_col, col = "blue")
      text(new_loadings[items[!items %in% factor_col],], labels = items[!items %in% factor_col], col = "red")
    }else{
      text(new_loadings, labels = items)
    }
    if (x == (steps)){
      abline(v = 0, h = 0)
    }else{
      loadmat_back <- matrix(c(v1[[steps - (x-1)]],v2[[steps - (x-1)]]),2)
      # loadmat_back <- matrix(c(v1_back[[x]],v2_back[[x]]),2)
      loadmat_back <- det(solve(loadmat_back)) * loadmat_back
      r1 <- loadmat_back[,1]
      r2 <- loadmat_back[,2]
      abline(a =0, b= -r1[1]/r1[2])
      abline(a =0, b= -r2[1]/r2[2])
    }
    Sys.sleep(.1)
  }
}


v1_back

plot_fit <- function(fit, factor_col = FALSE){
  items <- colnames(fit$model)
  plot(fit$loadings, ylim = c(-1,1), xlim = c(-1,1), type = "n",
       main = fit$rotation, xlab = "Factor 1", ylab = "Factor 2")
  if (length(factor_col) > 1){
    text(fit$loadings[factor_col,], labels = factor_col, col = "blue")
    text(fit$loadings[items[!items %in% factor_col],], labels = items[!items %in% factor_col], col = "red")
  }else{
    text(fit$loadings, labels = items)
  }
  abline(v = 0, h = 0)
}
