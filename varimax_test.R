colSums((fit_no_rotation$loadings[] / rowSums(fit_no_rotation$loadings**2))**2)

colSums(fit_orthogonal$loadings**2)

indata <- fit_no_rotation$loadings[]

indata

ang <- 38.9080
data_rot <- data.frame()
for (ang in seq(38.9,39,0.001)){
  radian <- (ang*pi)/180
  rot_mat <- matrix(c(cos(radian), sin(radian), -sin(radian), cos(radian)),2)
  new_mat <- indata %*% rot_mat
  plot(new_mat, ylim = lim, xlim = lim)
  abline(v = 0, h = 0)
  kvot <- (c(colSums((new_mat / rowSums(new_mat)**2))**2,ang))
  kvot <- kvot[1] - kvot[2]
  print(c(kvot, ang))
  for_bind <- as.data.frame(matrix(c(kvot, ang),ncol = 2))
  data_rot <- rbind(data_rot, for_bind)
  #Sys.sleep(0.1)
}
min(abs(data_rot[,1]))
sort(round(data_rot[,1],3))

finddd
ang
(matrix(c(cos(radian), sin(radian), -sin(radian), cos(radian)),2) *180)/pi


plot(fit_orthogonal$loadings[], ylim = lim, xlim = lim)
abline(v = 0, h = 0)
new_mat
fit_orthogonal$loadings[]
rot_mat


(get_angle(fit_orthogonal$rot.mat[,1])*180)/pi


pca_iris        <- fit_no_rotation
rawLoadings     <- pca_iris$rotation[,1:2] %*% diag(pca_iris$sdev, 2, 2)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(irisX) %*% invLoadings

varimax(fit_no_rotation$loadings[],normalize = T)

new_mat
c(kvot, ang)
