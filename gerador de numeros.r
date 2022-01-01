simvnorm <- function(n, Mean, Cov = diag(length(Mean))){
   stopifnot(n > 0)
   stopifnot(isSymmetric(Cov))
   stopifnot(nrow(Cov) == length(Mean))

   p <- length(Mean)

   m1 <- matrix(rnorm(n*p), ncol=p)
   colnames(m1) <- colnames(Cov)

   S <- chol(Cov, pivot = TRUE)
   o <- order(attr(S, 'pivot'))

   m2 <- m1 %*% S[,o]
   m2 <- sweep(m2, 2, Mean, "+")
   
   return(m2)
}