require(haven)
test <- read_spss("C:/Users/JOAUR001/Downloads/Watson (1988).sav")

require(psych)
hest <- as.data.frame(test)[1:10,c('PA1','PA2','PA3')]
a <- alpha(hest)
a$total$raw_alpha


cor(rowSums(hest[1:2],hest[3]))
raw.r <- cor(hest[1]+hest[2]+hest[3], hest[3])
pre.r <- cor(hest[1]+hest[2], hest[3])
raw.r/sqrt((1-a$total$raw_alpha/1.8)+a$total$raw_alpha)

a$item.stats$r.cor
spf(pre.r,1.3)
pre.r
citc(raw.r, a$total$raw_alpha, .5)
spf(citc(raw.r, a$total$raw_alpha, .3),3)
spf <- function(r, n){
  return(n*r/(1+(n-1)*r))
}

citc <- function(r, alpha, k){
  r/(sqrt((1-alpha)/k)+r)
}
