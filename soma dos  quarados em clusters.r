totss <- function(x) sum(scale(x, scale = FALSE)^2)

cat("-------------------------------------------------------------- \n")
cat("Soma dos quadrados entre clusters  \n")
cat("-------------------------------------------------------------- \n")

stats.hclust <- function(data, hclust, plot.it = FALSE, ...) {
   if(class(data) != "data.frame") {
     stop(" 'data' devem ser um quadro de dados!")
   }

   if(class(hclust) != "hclust") {
      stop(" 'hclust' deve ser um objeto hclust!")
   }

   if(length(hclust$order) != nrow(data)) {
      stop(" 'data' e 'hclust' são incompativeis")
   }

   X <- data
   n <- nrow(data)
   p <- ncol(data)
   cen <- apply(X, 2, mean)
   mc <- matrix(0, nr=n, nc=n-1)
   mSS <- matrix(0, nr=n-1, nc=n-1)

   for(j in 1:(n-1)) {
      mc[,j] <- cutree(hclust, k = j)

      for(i in 1:max(mc[,j])) {
         mSS[i,j] <- apply(X[mc[,j] == i, ], 2, length)[1] * crossprod(apply(X[mc[,j] == i, ], 2, mean) - cen, apply(X[mc[,j] == i, ], 2, mean) - cen)
      }
   }

   mSS <- mSS[, (n-1):1]
   SSb <- apply(mSS, 2, sum)
   Rsq <- SSb / totss(X)
   nclust <- seq(n-1, 1)
   pseudoF <- Rsq/(1 - Rsq) * (n - nclust)/(nclust - 1)


   out <- cbind(step = 1:(n-1), nclust, merge1 = hclust$merge[,1], merge2 = hclust$merge[,2], height = hclust$height, SSb, Rsq, pseudoF)
   
   if(plot.it) {
      list(plot(nclust, Rsq, type="b", ...),
      devAskNewPage(ask = TRUE),
      plot(nclust, pseudoF, type="b", ...))
   }

   return(out)
}
