cat("------------------------------------------------------------------------ \n")
cat("gower: função para calcular o coeficiente de similaridade de Gower \n")
cat("------------------------------------------------------------------------ \n")

gower <- function(data) {
   stopifnot(inherits(data, "data.frame"))

   n <- nrow(data)
   p <- ncol(data)
   l1 <- rep(1:n, each = n)
   l2 <- rep(1:n, times = n)

   logi <- function(x) {
      k1 <- rep(x, each = n)
      k2 <- rep(x, times = n)
      aux <- rbind(k1[l1 < l2], k2[l1 < l2])
      lo <- NULL

      for(i in 1:ncol(aux)) {
         lo[i] <- (aux[1, i] == aux[2, i]) * 1
      }

      return(lo)
   }

   sim <- function(x) {
      k1 <- rep(x, each = n)
      k2 <- rep(x, times = n)
      aux <- rbind(k1[l1 < l2], k2[l1 < l2])
      si <- NULL

      for(i in 1:ncol(aux)) {
         si[i] <- 1 - abs(aux[1, i] - aux[2, i]) / (max(x) - min(x))
      }
      return(si)
   }

   delta <- matrix(NA, choose(n, 2), p)
   colnames(data) <- colnames(data)

   for(j in 1:p) {
      if (is.numeric(data[, j])) {
         delta[, j] <- sim(data[, j])
      }

      else { 
         delta[, j] <- logi(data[, j]) 
      }
   }

   m <- matrix(NA, n, n)
   dimnames(m) <- list(rownames(data), rownames(data))
   dis <- as.dist(m)
   dis[] <- rowMeans(delta)
   
   return(dis)
}