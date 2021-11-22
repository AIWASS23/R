cat("-------------------------------------------------------- \n")
cat("tau: calcula as correlações de Kendall, tau-a e tau-b. \n")
cat("-------------------------------------------------------- \n")

tau <- function(x, y) {
   stopifnot(is.numeric(x))
   stopifnot(is.numeric(y))

   if(length(x) != length(y)) {
	   stop("dimensões incompativeis!")
   }

   if(sum(is.na(c(x, y))) > 0) {
	   stop("você deve remover NA!")
   }

   x <- as.vector(x)
   y <- as.vector(y)
   n <- length(y)
   ntx <- sum(choose(table(x)[table(x)!=1], 2))
   nty <- sum(choose(table(y)[table(y)!=1], 2))

   s = matrix(NA, n, n)

   for(i in 1:n) {
	   for(j in 1:n) {
	      if(j > i) {
	         s[i,j] <- sign(x[j] - x[i]) * sign(y[j] - y[i])
         }
	   }
	}

   S <- sum(s, na.rm = TRUE)
   tauA <- S / choose(n, 2)
   tauB <- S / sqrt((choose(n, 2) - ntx) * (choose(n, 2) - nty))
   
   return(list(tauA = tauA, tauB = tauB))
}